defmodule NativeElixirPdfUtilities.HtmlToPdf.FontTest do
  use ExUnit.Case

  alias NativeElixirPdfUtilities.HtmlToPdf.Font

  test "load_registry loads explicit TTF fonts and resolves fallback families" do
    font_path = ttf_font_path!()

    assert {:ok, registry} =
             Font.load_registry(
               fonts: [%{family: "Fixture Sans", path: font_path, weight: 400, style: :normal}]
             )

    assert {:ok, ["Missing", "Fixture Sans", "Helvetica"], font} =
             Font.resolve("Missing, 'Fixture Sans', Helvetica", 400, :normal, registry)

    assert font.type == :embedded
    assert font.family == "Fixture Sans"
    assert is_binary(font.data)
    assert Font.pdf_name(font) == "Embedded-" <> font.id
  end

  test "load_registry accepts supported config shapes and normalizes metadata" do
    font_path = ttf_font_path!()

    assert {:ok, registry} =
             Font.load_registry(
               fonts: [
                 [family: "Fixture Keyword", path: font_path, weight: 400.0, style: "normal"],
                 %{
                   "family" => "Fixture String Keys",
                   "path" => font_path,
                   "weight" => "bold",
                   "style" => "italic"
                 },
                 %{
                   family: "Fixture Atom Italic",
                   path: font_path,
                   weight: "normal",
                   style: :italic
                 },
                 %{family: "!!!", path: font_path, weight: "500", style: :normal}
               ]
             )

    assert {:ok, _families, keyword_font} =
             Font.resolve("Fixture Keyword", 400, :normal, registry)

    assert keyword_font.weight == 400.0
    assert keyword_font.style == :normal

    assert {:ok, _families, string_key_font} =
             Font.resolve("Fixture String Keys", 700, :italic, registry)

    assert string_key_font.weight == 700
    assert string_key_font.style == :italic

    assert {:ok, _families, atom_italic_font} =
             Font.resolve("Fixture Atom Italic", 400, :italic, registry)

    assert atom_italic_font.weight == 400
    assert atom_italic_font.style == :italic

    assert {:ok, _families, safe_name_font} = Font.resolve("!!!", 500, :normal, registry)
    assert safe_name_font.pdf_name =~ "EmbeddedFont-"
  end

  test "resolve handles built-in and generic font families" do
    assert {:ok, ["Helvetica"], sans} = Font.resolve("sans-serif", 400, :normal, %{embedded: []})
    assert sans.pdf_name == "Helvetica"

    assert {:ok, ["Helvetica"], helvetica_bold_italic} =
             Font.resolve("Helvetica", 700, :italic, %{embedded: []})

    assert helvetica_bold_italic.pdf_name == "Helvetica-BoldOblique"

    assert {:ok, ["Helvetica"], helvetica_italic} =
             Font.resolve("Helvetica", 400, :italic, %{embedded: []})

    assert helvetica_italic.pdf_name == "Helvetica-Oblique"

    assert {:ok, ["Times-Roman"], times_bold_italic} =
             Font.resolve("serif", 700, :italic, %{embedded: []})

    assert times_bold_italic.pdf_name == "Times-BoldItalic"

    assert {:ok, ["Times-Roman"], times_bold} =
             Font.resolve("Times-Roman", 700, :normal, %{embedded: []})

    assert times_bold.pdf_name == "Times-Bold"

    assert {:ok, ["Times-Roman"], times_italic} =
             Font.resolve("Times-Roman", 400, :italic, %{embedded: []})

    assert times_italic.pdf_name == "Times-Italic"

    assert {:ok, ["Courier"], courier_bold_italic} =
             Font.resolve("monospace", 700, :italic, %{embedded: []})

    assert courier_bold_italic.pdf_name == "Courier-BoldOblique"

    assert {:ok, ["Courier"], courier_bold} =
             Font.resolve("Courier", 700, :normal, %{embedded: []})

    assert courier_bold.pdf_name == "Courier-Bold"

    assert {:ok, ["Courier"], courier_italic} =
             Font.resolve("Courier", 400, :italic, %{embedded: []})

    assert courier_italic.pdf_name == "Courier-Oblique"

    assert {:ok, ["Courier"], courier} = Font.resolve("Courier", 400, :normal, %{embedded: []})
    assert courier.pdf_name == "Courier"

    assert {:ok, ["Times-Roman"], times} =
             Font.resolve("Times-Roman", 400, :normal, %{embedded: []})

    assert times.pdf_name == "Times-Roman"
  end

  test "text_width and embedded text encoding use TTF cmap and hmtx data" do
    font_path = ttf_font_path!()

    assert {:ok, registry} = Font.load_registry(fonts: [{"Fixture Sans", font_path}])
    assert {:ok, _families, font} = Font.resolve("Fixture Sans", 400, :normal, registry)

    assert Font.text_width("iiii", font, 12.0) < Font.text_width("WWWW", font, 12.0)
    assert Font.encode_embedded_text("é", font) =~ ~r/^[0-9A-F]{4}$/u
    assert Font.unicode_mappings(["é", <<0>>], font) |> Map.values() == [?é]
  end

  test "load_registry rejects unsupported font config" do
    invalid_path = Path.join(System.tmp_dir!(), "native-elixir-pdf-invalid-font.ttf")
    short_path = Path.join(System.tmp_dir!(), "native-elixir-pdf-short-font.ttf")

    short_directory_path =
      Path.join(System.tmp_dir!(), "native-elixir-pdf-short-directory-font.ttf")

    missing_table_path = Path.join(System.tmp_dir!(), "native-elixir-pdf-missing-table-font.ttf")
    short_head_path = Path.join(System.tmp_dir!(), "native-elixir-pdf-short-head-font.ttf")
    bad_cmap_path = Path.join(System.tmp_dir!(), "native-elixir-pdf-bad-cmap-font.ttf")
    bad_range_path = Path.join(System.tmp_dir!(), "native-elixir-pdf-bad-range-font.ttf")

    File.write!(invalid_path, "not a font")
    File.write!(short_path, <<0, 1, 0>>)
    File.write!(short_directory_path, <<0, 1, 0, 0, 0>>)

    File.write!(missing_table_path, ttf_fixture(%{"head" => head_table()}))

    File.write!(
      short_head_path,
      ttf_fixture(valid_tables() |> Map.put("head", binary_part(head_table(), 0, 42)))
    )

    File.write!(bad_cmap_path, ttf_fixture(valid_tables() |> Map.put("cmap", bad_cmap_table())))

    File.write!(
      bad_range_path,
      ttf_fixture(valid_tables() |> Map.put("cmap", bad_range_cmap_table()))
    )

    assert Font.load_registry(fonts: [%{family: "Bad"}]) == :error
    assert Font.load_registry(fonts: :not_a_list) == :error
    assert Font.load_registry(fonts: ["bad"]) == :error

    assert Font.load_registry(fonts: [%{family: "Bad", path: __ENV__.file, weight: "950"}]) ==
             :error

    assert Font.load_registry(fonts: [%{family: "Bad", path: __ENV__.file, weight: "boldish"}]) ==
             :error

    assert Font.load_registry(fonts: [%{family: "Bad", path: __ENV__.file, style: "oblique"}]) ==
             :error

    assert Font.load_registry(fonts: [%{family: "Bad", path: invalid_path}]) == :error
    assert Font.load_registry(fonts: [%{family: "Bad", path: short_path}]) == :error
    assert Font.load_registry(fonts: [%{family: "Bad", path: short_directory_path}]) == :error
    assert Font.load_registry(fonts: [%{family: "Bad", path: missing_table_path}]) == :error
    assert Font.load_registry(fonts: [%{family: "Bad", path: short_head_path}]) == :error
    assert Font.load_registry(fonts: [%{family: "Bad", path: bad_cmap_path}]) == :error
    assert Font.load_registry(fonts: [%{family: "Bad", path: bad_range_path}]) == :error

    assert Font.load_registry(fonts: [%{family: "Bad", path: __ENV__.file, weight: 1_000}]) ==
             :error

    assert Font.resolve(:not_a_family, 400, :normal, %{embedded: []}) == :error
    assert Font.resolve(["Missing", 123], 400, :normal, %{embedded: []}) == :error
  after
    invalid_path = Path.join(System.tmp_dir!(), "native-elixir-pdf-invalid-font.ttf")
    short_path = Path.join(System.tmp_dir!(), "native-elixir-pdf-short-font.ttf")

    short_directory_path =
      Path.join(System.tmp_dir!(), "native-elixir-pdf-short-directory-font.ttf")

    missing_table_path = Path.join(System.tmp_dir!(), "native-elixir-pdf-missing-table-font.ttf")
    short_head_path = Path.join(System.tmp_dir!(), "native-elixir-pdf-short-head-font.ttf")
    bad_cmap_path = Path.join(System.tmp_dir!(), "native-elixir-pdf-bad-cmap-font.ttf")
    bad_range_path = Path.join(System.tmp_dir!(), "native-elixir-pdf-bad-range-font.ttf")

    File.rm(invalid_path)
    File.rm(short_path)
    File.rm(short_directory_path)
    File.rm(missing_table_path)
    File.rm(short_head_path)
    File.rm(bad_cmap_path)
    File.rm(bad_range_path)
  end

  defp ttf_font_path! do
    [
      "/usr/share/fonts/truetype/dejavu/DejaVuSans.ttf",
      "/usr/share/fonts/truetype/liberation/LiberationSans-Regular.ttf",
      "/usr/share/fonts/truetype/noto/NotoSans-Regular.ttf"
    ]
    |> Enum.find(&File.exists?/1)
    |> case do
      nil -> flunk("No local TTF font fixture found")
      path -> path
    end
  end

  defp valid_tables do
    %{
      "head" => head_table(),
      "hhea" => hhea_table(),
      "maxp" => <<0::32, 2::16>>,
      "hmtx" => <<600::16, 0::16>>,
      "cmap" => valid_cmap_table()
    }
  end

  defp ttf_fixture(tables) do
    table_count = map_size(tables)
    table_start = 12 + table_count * 16

    {_offset, records, data} =
      tables
      |> Enum.sort_by(fn {tag, _data} -> tag end)
      |> Enum.reduce({table_start, [], ""}, fn {tag, table_data}, {offset, records, data} ->
        record = <<tag::binary-size(4), 0::32, offset::32, byte_size(table_data)::32>>
        {offset + byte_size(table_data), records ++ [record], data <> table_data}
      end)

    <<0x0001_0000::32, table_count::16, 0::16, 0::16, 0::16>> <> Enum.join(records, "") <> data
  end

  defp head_table do
    <<0::size(18 * 8), 1000::16, 0::size(16 * 8), 0::signed-16, 0::signed-16, 1000::signed-16,
      1000::signed-16>>
  end

  defp hhea_table do
    <<0::32, 800::signed-16, -200::signed-16, 0::size(26 * 8), 1::16>>
  end

  defp valid_cmap_table do
    format4 = cmap_format4_table(65, 65, 1, 0)
    <<0::16, 1::16, 3::16, 1::16, 12::32, format4::binary>>
  end

  defp bad_cmap_table do
    <<0::16, 1::16, 3::16, 1::16, 12::32, 0::16>>
  end

  defp bad_range_cmap_table do
    format4 = cmap_format4_table(65, 65, 0, 1000)
    <<0::16, 1::16, 3::16, 1::16, 12::32, format4::binary>>
  end

  defp cmap_format4_table(start_code, end_code, id_delta, range_offset) do
    seg_count = 1
    length = 24

    <<4::16, length::16, 0::16, seg_count * 2::16, 0::16, 0::16, 0::16, end_code::16, 0::16,
      start_code::16, id_delta::signed-16, range_offset::16>>
  end
end
