defmodule NativeElixirPdfUtilities.HtmlToPdf.Woff2Test do
  use ExUnit.Case, async: true
  alias NativeElixirPdfUtilities.TestSupport.FontCorpus
  alias NativeElixirPdfUtilities.{HtmlToPdf, Text}
  alias NativeElixirPdfUtilities.HtmlToPdf.Font
  alias NativeElixirPdfUtilities.HtmlToPdf.Font.Sfnt
  alias NativeElixirPdfUtilities.TestSupport.Paths

  alias NativeElixirPdfUtilities.Validators.{
    FontContainerValidator,
    FontValidator,
    TrueTypeSubsetValidator,
    Woff2Validator
  }

  test "transformed TrueType, literal TrueType and CFF agree with independent decoded tables, points and metrics" do
    manifest = Paths.fixture("fonts/woff2-fonts.json") |> File.read!() |> :json.decode()

    for {filename, expected} <- manifest["files"] do
      input = FontCorpus.path(filename) |> File.read!()
      assert {:ok, data} = FontContainerValidator.prepare(input)
      assert Sfnt.checksum(data) == 0xB1B0_AFBA
      assert {:ok, _, directory} = FontValidator.prepare_table_directory(data)

      for {tag, hash} <- expected["tables"] do
        {offset, length} = directory[tag]

        assert Base.encode16(:crypto.hash(:sha256, binary_part(data, offset, length)),
                 case: :lower
               ) == hash
      end

      if map_size(expected["glyphs"]) > 0 do
        assert {:ok, outlines} = TrueTypeSubsetValidator.prepare_outlines(data)

        for {id, glyph} <- expected["glyphs"] do
          actual = elem(outlines.glyphs, String.to_integer(id))
          assert Enum.map(actual.points, fn {x, y, _} -> [x, y] end) == glyph["points"]
          assert Enum.map(actual.points, &elem(&1, 2)) == glyph["flags"]
          assert actual.ends == glyph["ends"]
          assert Tuple.to_list(actual.bbox) == glyph["bounds"]
        end
      end

      assert {:ok, [font]} = Font.load_configured_faces([%{family: "WOFF2", data: input}])
      assert font.data == data

      assert {:ok, pdf} =
               HtmlToPdf.render("<p style='font-family:WOFF2'>Hx0</p>",
                 fonts: [%{family: "WOFF2", data: input}],
                 system_font_discovery: false
               )

      assert {:ok, "Hx0"} = Text.extract(pdf, layout: false)
    end
  end

  test "truncated headers and directories, invalid flavor, flags and indices retain decoder source context" do
    original = FontCorpus.path("selected-transformed.woff2") |> File.read!()

    cases = [
      {nil, 0},
      {<<>>, 0},
      {original, -1},
      {original, 1},
      {binary_part(original, 0, 47), 0},
      {replace(original, 4, <<0::32>>), 0},
      {replace(original, 14, <<1::16>>), 0},
      {replace(original, 48, <<0x9A>>), 0},
      {replace(original, 49, <<128, 0>>), 0},
      {replace(original, 20, <<0::32>>), 0}
    ]

    for {input, index} <- cases do
      assert {:error, {:invalid_font, diagnostic}} = Woff2Validator.prepare(input, index)
      assert diagnostic.stage == :font
      assert diagnostic.operation == :decode_woff2
      assert diagnostic.source =~ "WOFF2"
      assert diagnostic.message != ""
    end
  end

  test "re-encoded malformed directories and transformed table streams fail at their owning boundary" do
    manifest = Paths.fixture("fonts/woff2-invalid-vectors.json") |> File.read!() |> :json.decode()

    for vector <- manifest["vectors"] do
      assert {:error, {reason, diagnostic}} =
               Woff2Validator.prepare(Base.decode64!(vector["compressed"]), 0),
             vector["name"]

      assert reason in [:invalid_font, :resource_limit_exceeded]
      assert diagnostic.stage == :font
      assert diagnostic.source != ""
      assert diagnostic.message != ""
    end

    source = FontCorpus.path(manifest["source"]) |> File.read!()
    assert {:ok, expected} = FontContainerValidator.prepare(source)

    assert {:ok, ^expected} =
             FontContainerValidator.prepare(Base.decode64!(manifest["valid_optional_blocks"]))
  end

  test "collections share complete outline pairs and decode every 255UInt16 form" do
    manifest = Paths.fixture("fonts/woff2-fonts.json") |> File.read!() |> :json.decode()
    collection = manifest["collection"]
    source = FontCorpus.path(collection["file"]) |> File.read!()

    for {tables, index} <- Enum.with_index(collection["tables"]) do
      assert {:ok, data} = FontContainerValidator.prepare(source, index)
      assert {:ok, _, directory} = FontValidator.prepare_table_directory(data)

      for {tag, hash} <- tables do
        {offset, length} = directory[tag]

        assert Base.encode16(:crypto.hash(:sha256, binary_part(data, offset, length)),
                 case: :lower
               ) == hash
      end

      config = %{family: "Collection", data: source, collection_index: index}
      assert {:ok, [font]} = Font.load_configured_faces([config])
      assert font.data == data

      assert {:ok, pdf} =
               HtmlToPdf.render("<p style='font-family:Collection'>Hx0</p>",
                 fonts: [config],
                 system_font_discovery: false
               )

      assert pdf =~ "/BaseFont /#{font.pdf_name} "
      assert Text.extract(pdf, layout: false) == {:ok, "Hx0"}
    end

    manifest = Paths.fixture("fonts/woff2-edge-fonts.json") |> File.read!() |> :json.decode()

    assert {:ok, expected} =
             FontCorpus.path("selected-transformed.woff2")
             |> File.read!()
             |> FontContainerValidator.prepare()

    for collection <- manifest["collections"] do
      assert {:ok, ^expected} =
               FontCorpus.path(collection["file"])
               |> File.read!()
               |> FontContainerValidator.prepare()
    end
  end

  test "an unselected face must reference its glyf and loca in global outline order" do
    source = FontCorpus.path("selected-collection.woff2") |> File.read!()
    [_, {glyf, _}] = :binary.matches(source, <<255, "glyf">>)
    [_, {loca, _}] = :binary.matches(source, <<255, "loca">>)
    reversed = source |> replace(glyf + 1, "loca") |> replace(loca + 1, "glyf")

    assert {:error, {:invalid_font, diagnostic}} = Woff2Validator.prepare(reversed, 0)
    assert diagnostic.stage == :font
    assert diagnostic.message =~ "directory pairing"
  end

  test "all triplet sizes, composite programs and asymmetric hmtx transforms agree with FontTools" do
    manifest = Paths.fixture("fonts/woff2-edge-fonts.json") |> File.read!() |> :json.decode()

    for {file, expected} <- manifest["files"] do
      assert {:ok, data} =
               FontCorpus.path(file) |> File.read!() |> FontContainerValidator.prepare()

      assert {:ok, outlines} = TrueTypeSubsetValidator.prepare_outlines(data)
      assert {:ok, _, directory} = FontValidator.prepare_table_directory(data)

      for {tag, hash} <- expected["tables"] do
        {offset, length} = directory[tag]

        assert Base.encode16(:crypto.hash(:sha256, binary_part(data, offset, length)),
                 case: :lower
               ) == hash
      end

      for {id, glyph} <- expected["glyphs"] do
        {actual, _} =
          FontValidator.with_budget(fn ->
            NativeElixirPdfUtilities.Validators.TrueTypeGlyphValidator.resolve(
              String.to_integer(id),
              outlines.glyphs,
              %{},
              MapSet.new(),
              0,
              false
            )
          end)

        assert actual.ends == glyph["ends"]
        assert length(actual.points) == length(glyph["points"])

        for {{x, y, _}, [rx, ry]} <- Enum.zip(actual.points, glyph["points"]) do
          assert_in_delta x, rx, 1.0e-9
          assert_in_delta y, ry, 1.0e-9
        end
      end
    end
  end

  defp replace(data, offset, bytes) do
    <<before::binary-size(^offset), _old::binary-size(byte_size(^bytes)), after_bytes::binary>> =
      data

    before <> bytes <> after_bytes
  end
end

defmodule NativeElixirPdfUtilities.HtmlToPdf.Woff2GlobalStateTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.TestSupport.FontCorpus
  alias NativeElixirPdfUtilities.Limits

  alias NativeElixirPdfUtilities.Validators.{
    FontContainerValidator,
    ResourceBudgetValidator,
    Woff2Validator
  }

  setup do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    :ok
  end

  test "shared collection faces charge for the selected tables within the operation's work limit" do
    source = FontCorpus.path("selected-collection.woff2") |> File.read!()

    # The third face repeats the first. All three use the same literal Brotli payload.
    [{first, first_work}, {second, second_work}, {repeat, repeat_work}] =
      for index <- 0..2 do
        ResourceBudgetValidator.with_budget(fn ->
          assert {:ok, _, tables} = Woff2Validator.prepare(source, index)
          {tables, ResourceBudgetValidator.snapshot().max_work}
        end)
      end

    assert {repeat, repeat_work} == {first, first_work}
    assert length(first) > length(second)
    assert first_work - second_work == length(first) - length(second)

    limits = Limits.effective()
    Limits.install(%{limits | max_work: first_work - 1})
    assert {:error, {:resource_limit_exceeded, diagnostic}} = Woff2Validator.prepare(source, 2)
    assert diagnostic.stage == :font and diagnostic.message =~ "max_work"
    assert {:ok, _, ^second} = Woff2Validator.prepare(source, 1)

    Limits.install(%{limits | max_work: first_work})
    assert {:ok, _, ^first} = Woff2Validator.prepare(source, 2)
  end

  test "decompression and reconstruction obey shared size and work limits" do
    input = FontCorpus.path("selected-transformed.woff2") |> File.read!()

    for limit <- [:max_decoded_bytes, :max_work] do
      limits = Limits.effective()
      Limits.install(Map.put(limits, limit, 1))

      assert {:error, {:resource_limit_exceeded, diagnostic}} =
               FontContainerValidator.prepare(input)

      assert diagnostic.message =~ Atom.to_string(limit)
      Limits.install(limits)
    end
  end
end
