defmodule NativeElixirPdfUtilities.HtmlToPdf.FontLimitsTest do
  use ExUnit.Case, async: false

  alias NativeElixirPdfUtilities.{FileReader, HtmlToPdf, Limits}
  alias NativeElixirPdfUtilities.HtmlToPdf.{AssetLoader, Font, FontCache, SystemFontCache}
  alias NativeElixirPdfUtilities.Validators.{FileValidator, FontValidator, LimitsValidator}

  setup do
    original = Limits.effective()
    path = Path.join(System.tmp_dir!(), "font-limit-#{System.unique_integer([:positive])}.ttf")

    on_exit(fn ->
      Limits.install(original)
      File.rm(path)
    end)

    {:ok, path: path}
  end

  test "bounded regular file reads handle exact limits, empty files and invalid inputs", %{
    path: path
  } do
    File.write!(path, "1234")
    assert {:ok, "1234"} = FileReader.read(path, 4)
    assert {:error, {:resource_limit_exceeded, diagnostic}} = FileReader.read(path, 3)
    assert diagnostic.source == path
    assert diagnostic.stage == :file
    File.write!(path, "")
    assert {:ok, ""} = FileReader.read(path, 1)
    assert {:error, :enoent} = FileReader.read(path <> "missing", 1)
    assert {:error, {:invalid_options, _}} = FileReader.read(nil, 1)
    assert {:error, {:invalid_options, _}} = FileReader.read(path, 0)

    assert {:error, {:invalid_document, _}} =
             FileValidator.validate_info(%File.Stat{type: :directory}, 10, path)
  end

  test "per-font limits apply before parsing configured bytes and file snapshots", %{path: path} do
    Limits.install(%{Limits.effective() | max_font_source_bytes: 3})
    File.write!(path, "1234")

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             FontCache.fetch(path, fn _ -> flunk("oversized file must not load") end)

    assert diagnostic.stage == :font
    assert diagnostic.message =~ "max_font_source_bytes"

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             Font.load_registry(fonts: [%{family: "Oversized", data: "1234"}])

    assert diagnostic.message =~ "max_font_source_bytes"
    assert Process.get({FontValidator, :budget}) == nil
  end

  test "asset maps and callbacks obey font byte budgets independently from image budgets" do
    Limits.install(%{Limits.effective() | max_font_source_bytes: 3})

    for opts <- [
          [assets: %{"font.ttf" => {:bytes, "1234"}}],
          [asset_resolver: fn _ -> {:ok, "1234"} end]
        ] do
      assert {:error, {:resource_limit_exceeded, diagnostic}} =
               AssetLoader.resolve("font.ttf", :font, opts)

      assert diagnostic.message =~ "max_font_source_bytes"
    end
  end

  test "font asset files use bounded reads and aggregate remaining capacity", %{path: path} do
    File.write!(path, "1234")
    Limits.install(%{Limits.effective() | max_font_source_bytes: 3})

    assert {:error, {:resource_limit_exceeded, _}} =
             AssetLoader.resolve("font.ttf", :font, assets: %{"font.ttf" => {:file, path}})

    Limits.install(%{
      Limits.effective()
      | max_font_source_bytes: 10,
        max_aggregate_font_source_bytes: 5
    })

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             FontValidator.with_budget(fn ->
               assert {:ok, "ab"} =
                        AssetLoader.resolve("a", :font, assets: %{"a" => {:bytes, "ab"}})

               AssetLoader.resolve("b", :font, assets: %{"b" => {:file, path}})
             end)

    assert diagnostic.message =~ "max_aggregate_font_source_bytes"
    Limits.install(Limits.defaults())

    assert {:ok, "1234"} =
             AssetLoader.resolve("font.ttf", :font, assets: %{"font.ttf" => {:file, path}})
  end

  test "one rendering resolves duplicate sources once and charges unique byte payloads once" do
    {:ok, calls} = Agent.start_link(fn -> 0 end)

    opts = [
      asset_resolver: fn _ ->
        Agent.update(calls, &(&1 + 1))
        {:ok, "abc"}
      end
    ]

    Limits.install(%{Limits.effective() | max_aggregate_font_source_bytes: 3})

    assert :ok =
             FontValidator.with_budget(fn ->
               assert {:ok, "abc"} = AssetLoader.resolve("same", :font, opts)
               assert {:ok, "abc"} = AssetLoader.resolve("same", :font, opts)
               assert {:ok, "abc"} = AssetLoader.resolve("alias", :font, opts)
               assert :ok = FontValidator.reserve_source("abc")
             end)

    assert Agent.get(calls, & &1) == 2

    assert {:error, {:resource_limit_exceeded, _}} =
             FontValidator.with_budget(fn ->
               FontValidator.reserve_source("abc")
               FontValidator.reserve_source("different")
             end)

    assert {:error, {:resource_limit_exceeded, _}} =
             FontValidator.with_budget(fn ->
               FontValidator.reserve_source("abc")
               FontValidator.read_limit()
             end)
  end

  test "configured faces and source candidates stop before unbounded lookup work" do
    Limits.install(%{Limits.effective() | max_font_count: 1, max_font_candidates: 1})

    assert {:error, {:resource_limit_exceeded, _}} =
             Font.normalize_options(fonts: [%{family: "A", data: "a"}, %{family: "B", data: "b"}])

    assert {:error, {:resource_limit_exceeded, _}} =
             Font.load_registry(fonts: [%{family: "A", data: ["a", "b"]}])

    assert {:error, {:resource_limit_exceeded, _}} =
             Font.load_registry(fonts: [%{family: "A", path: ["a", "b"]}])

    assert {:error, {:resource_limit_exceeded, _}} =
             FontValidator.with_budget(fn ->
               FontValidator.reserve_face(%{family: "A", weight: 400, style: :normal})
               FontValidator.reserve_face(%{family: "B", weight: 400, style: :normal})
             end)
  end

  test "discovery charges cache hits and returned data rather than swallowing limits" do
    assert {:ok, registry} = Font.load_registry([])
    Limits.install(%{Limits.effective() | max_font_discoveries: 1})

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             Font.requested_faces(
               ["DefinitelyMissingFontOne", "DefinitelyMissingFontTwo"],
               400,
               :normal,
               registry
             )

    assert diagnostic.message =~ "max_font_discoveries"
    Limits.install(%{Limits.defaults() | max_font_source_bytes: 1})

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             Font.resolve("sans-serif", 400, :normal, registry)

    assert diagnostic.message =~ "max_font_source_bytes"
    Limits.install(Limits.defaults())
    assert {:ok, _, _} = Font.resolve("sans-serif", 400, :normal, registry)
  end

  test "system discovery propagates native font parser resource failures" do
    assert {:ok, registry} = Font.load_registry([])
    Limits.install(%{Limits.effective() | max_font_cmap_work: 1})

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             Font.resolve("serif", 751, :normal, registry)

    assert diagnostic.message =~ "character-map work"
  end

  test "both caches evict by retained bytes and never retain resource failures", %{path: path} do
    Limits.install(%{
      Limits.effective()
      | max_font_cache_bytes: 1,
        max_system_font_cache_bytes: 1
    })

    File.write!(path, "font")
    {:ok, calls} = Agent.start_link(fn -> 0 end)

    loader = fn data ->
      Agent.update(calls, &(&1 + 1))
      {:ok, data}
    end

    assert {:ok, "font"} = FontCache.fetch(path, loader)
    assert {:ok, "font"} = FontCache.fetch(path, loader)
    assert Agent.get(calls, & &1) == 2
    key = {:font_byte_limit, make_ref()}
    assert :first = SystemFontCache.fetch(key, fn -> :first end)
    assert :second = SystemFontCache.fetch(key, fn -> :second end)
    Limits.install(Limits.defaults())
    failure = {:error, {:resource_limit_exceeded, %{message: "test"}}}
    assert ^failure = SystemFontCache.fetch(key, fn -> failure end)
    assert :recovered = SystemFontCache.fetch(key, fn -> :recovered end)

    assert {:error, {:resource_limit_exceeded, _}} =
             FontValidator.with_budget(fn -> FontValidator.discovery_result(failure) end)

    assert :error = FontValidator.source_result(:error)
  end

  test "ordinary document fonts remain supported and share budgets with page furniture" do
    data = File.read!(Path.expand("../../priv/fonts/dejavu/DejaVuSans.ttf", __DIR__))

    html =
      "<style>@font-face {font-family:Fixture;src:url(font.ttf)} p {font-family:Fixture}</style><p>Hello</p>"

    assert {:ok, _} =
             HtmlToPdf.render(html,
               assets: %{"font.ttf" => {:bytes, data}},
               page_furniture: [header: "<p>header</p>"],
               margin: 50
             )

    Limits.install(%{Limits.effective() | max_font_source_bytes: 10})

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             HtmlToPdf.render(html, assets: %{"font.ttf" => {:bytes, data}})

    assert diagnostic.message =~ "max_font_source_bytes"
    assert diagnostic.operation == :load_registry
  end

  test "font aggregate configurations must accommodate one source" do
    assert {:error, message} =
             LimitsValidator.validate(
               max_font_source_bytes: 10,
               max_aggregate_font_source_bytes: 9
             )

    assert message =~ "max_aggregate_font_source_bytes"
  end
end
