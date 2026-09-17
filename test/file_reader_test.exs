defmodule NativeElixirPdfUtilities.FileReaderTest do
  use ExUnit.Case, async: false

  alias NativeElixirPdfUtilities.{FileReader, HtmlToPdf, Limits, Text}
  alias NativeElixirPdfUtilities.HtmlToPdf.{AssetLoader, Style}
  alias NativeElixirPdfUtilities.Validators.{FileValidator, HtmlValidator}

  setup do
    root = Path.join(System.tmp_dir!(), "bounded-#{System.unique_integer([:positive])}")
    File.mkdir_p!(root)
    limits = Limits.effective()

    on_exit(fn ->
      File.rm_rf!(root)
      Limits.install(limits)
    end)

    {:ok, root: root}
  end

  test "portable reads enforce byte limits and validate regular files", %{root: root} do
    path = Path.join(root, "file")
    File.write!(path, "1234")
    assert {:ok, "1234"} = FileReader.read(path, 4)

    assert {:error, {:resource_limit_exceeded, %{stage: :file, source: ^path}}} =
             FileReader.read(path, 3)

    File.write!(path, "")
    assert {:ok, ""} = FileReader.read(path, 1)
    assert {:error, :enoent} = FileReader.read(Path.join(root, "missing"), 1)
    assert {:error, {:invalid_options, %{stage: :file}}} = FileReader.read(path, 0)

    assert {:error, {:invalid_document, %{stage: :file, source: ^root}}} =
             FileValidator.validate_info(%File.Stat{type: :directory, size: 0}, 1, root)
  end

  test "base_url image and font reads require no external executable", %{root: root} do
    path = System.get_env("PATH")
    System.put_env("PATH", root)

    try do
      File.write!(Path.join(root, "asset"), "approved")

      for kind <- [:image, :font] do
        assert {:ok, "approved"} = AssetLoader.resolve("asset", kind, base_url: root)
      end

      assert {:ok, "resolved"} =
               AssetLoader.resolve("missing", :image,
                 base_url: root,
                 asset_resolver: fn _ -> {:ok, "resolved"} end
               )
    after
      if path, do: System.put_env("PATH", path), else: System.delete_env("PATH")
    end
  end

  test "unreadable local assets use the resolver or return a diagnostic", %{root: root} do
    assert {:error, {:invalid_document, %{stage: :asset, source: "."}}} =
             AssetLoader.resolve(".", :image, base_url: root)

    assert {:ok, "approved"} =
             AssetLoader.resolve(".", :image,
               base_url: root,
               asset_resolver: fn _ -> {:ok, "approved"} end
             )
  end

  test "HTML CSS and PDF entry points reject oversized files before parsing", %{root: root} do
    path = Path.join(root, "large")
    File.write!(path, "12345")

    Limits.install(%{
      Limits.effective()
      | max_html_source_bytes: 4,
        max_css_source_bytes: 4,
        max_pdf_input_bytes: 4
    })

    assert {:error, {:resource_limit_exceeded, %{operation: :render_file, source: ^path}}} =
             HtmlToPdf.render_file(path, Path.join(root, "out.pdf"))

    assert {:error, {:resource_limit_exceeded, %{source: ^path}}} =
             Style.load_stylesheets(%{type: :document, children: []},
               stylesheets: [{:file, path}]
             )

    assert {:error, {:resource_limit_exceeded, %{operation: :extract_file, source: ^path}}} =
             Text.extract_file(path)

    assert {:error, {:resource_limit_exceeded, %{operation: :extract_file_spans, source: ^path}}} =
             Text.extract_file_spans(path)
  end

  test "local image reads use remaining aggregate capacity and stop at count limits", %{
    root: root
  } do
    File.write!(Path.join(root, "file"), "1234")

    Limits.install(%{
      Limits.effective()
      | max_image_source_bytes: 4,
        max_aggregate_image_source_bytes: 5,
        max_image_count: 2
    })

    budget = HtmlValidator.new_image_budget()
    assert {:ok, "1234"} = AssetLoader.resolve("file", :image, [base_url: root], budget)

    assert {:error, {:resource_limit_exceeded, _}} =
             AssetLoader.resolve("file", :image, [base_url: root], budget)

    assert :ok = HtmlValidator.reserve_image_source(budget, 1)
    assert {:error, {:resource_limit_exceeded, _}} = HtmlValidator.image_source_read_limit(budget)
    Limits.install(%{Limits.effective() | max_image_count: 3})
    assert {:error, {:resource_limit_exceeded, _}} = HtmlValidator.image_source_read_limit(budget)
  end
end
