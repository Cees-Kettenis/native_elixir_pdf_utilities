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

  test "text_width and embedded text encoding use TTF cmap and hmtx data" do
    font_path = ttf_font_path!()

    assert {:ok, registry} = Font.load_registry(fonts: [{"Fixture Sans", font_path}])
    assert {:ok, _families, font} = Font.resolve("Fixture Sans", 400, :normal, registry)

    assert Font.text_width("iiii", font, 12.0) < Font.text_width("WWWW", font, 12.0)
    assert Font.encode_embedded_text("é", font) =~ ~r/^[0-9A-F]{4}$/u
    assert Font.unicode_mappings(["é"], font) |> Map.values() == [?é]
  end

  test "load_registry rejects unsupported font config" do
    assert Font.load_registry(fonts: [%{family: "Bad"}]) == :error

    assert Font.load_registry(fonts: [%{family: "Bad", path: __ENV__.file, weight: 1_000}]) ==
             :error
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
end
