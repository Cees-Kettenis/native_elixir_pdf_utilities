defmodule NativeElixirPdfUtilities.HtmlToPdf.SvgFontBoundsTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.HtmlToPdf.SvgRasterizer
  alias NativeElixirPdfUtilities.Validators.SvgConverterValidator

  test "glyph bounds use admitted SVG geometry and reject text, invalid IDs and renderer failures" do
    source =
      "<svg xmlns='http://www.w3.org/2000/svg' width='100' height='100'><rect id='ink' x='10' y='20' width='30' height='40'/></svg>"

    assert {:ok, %{id: "ink"}} = SvgConverterValidator.prepare_bounds(source, "ink")
    assert {:ok, {10.0, 20.0, 40.0, 60.0}} == SvgRasterizer.measure_bounds(source, "ink")
    assert {:ok, nil} = SvgConverterValidator.bounds_result([], "missing")

    for result <- [:failed, [%{id: "ink", x: 0, y: 0, width: -1, height: 1}]] do
      assert {:error, {:invalid_document, %{message: message}}} =
               SvgConverterValidator.bounds_result(result, "ink")

      assert message =~ "bounds"
    end

    assert {:error, {:invalid_document, _}} = SvgConverterValidator.prepare_bounds(source, "")

    assert {:error, {:invalid_document, _}} =
             SvgConverterValidator.prepare_bounds(
               String.replace(source, "<rect", "<text>text</text><rect"),
               "ink"
             )
  end
end
