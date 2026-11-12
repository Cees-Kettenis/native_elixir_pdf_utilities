defmodule NativeElixirPdfUtilities.HtmlToPdf.ColorPaintGraphTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.TestSupport.FontCorpus
  alias NativeElixirPdfUtilities.HtmlToPdf.{Font, PdfWriter}
  alias NativeElixirPdfUtilities.Validators.{ColrFontValidator, ColrPaintValidator}
  alias NativeElixirPdfUtilities.TestSupport.Paths

  test "all COLR v1 paint formats and compact transforms agree with independently decoded FontTools matrices" do
    manifest = Paths.fixture("fonts/colr-paints.json") |> File.read!() |> :json.decode()
    data = FontCorpus.path(manifest["file"]) |> File.read!()

    assert {:ok, [font]} =
             Font.load_configured_faces([
               %{family: "Paints", data: data, variation_settings: %{"wght" => 900}}
             ])

    for {name, expected} <- manifest["transforms"] do
      assert {:ok, %{graph: {:transform, matrix, _}}} =
               ColrFontValidator.prepare(font, expected["glyph"], {0, 0, 0, 1})

      for {actual, independent} <- Enum.zip(matrix, expected["matrix"]),
          do:
            assert_in_delta(
              actual,
              independent,
              1.0e-9,
              "#{name}: #{inspect(matrix)} vs #{inspect(expected["matrix"])}"
            )
    end

    for {_name, id} <- manifest["glyphs"] do
      assert {:ok, paint} = ColrFontValidator.prepare(font, id, {0, 0, 0, 1})
      assert {:ok, svg, _, _context} = ColrPaintValidator.prepare(font, paint)
      assert is_binary(svg)
    end

    text =
      manifest["cmap"]
      |> Map.keys()
      |> Enum.map(fn "0x" <> hex -> String.to_integer(hex, 16) end)
      |> Enum.sort()
      |> List.to_string()

    box = %{
      type: :text,
      text: text,
      x: 10,
      y: 20,
      font: Font.pdf_name(font),
      font_face: font,
      font_size: 12,
      color: {0, 0, 0}
    }

    assert {:ok, pdf} = PdfWriter.render([%{size: {800, 100}, boxes: [box]}])
    assert {:ok, ^text} = NativeElixirPdfUtilities.Text.extract(pdf, layout: false)
  end
end
