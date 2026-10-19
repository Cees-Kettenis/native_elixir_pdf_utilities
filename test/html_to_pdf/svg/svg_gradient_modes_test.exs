alias NativeElixirPdfUtilities.TestSupport.Paths

Code.require_file(Paths.support("svg_public_workflow.ex"))
Code.require_file(Paths.support("png_fixture.ex"))

defmodule NativeElixirPdfUtilities.HtmlToPdf.SvgGradientModesTest do
  use ExUnit.Case, async: false

  test "print direct gradients preserve independent alpha while filter input retains declared interpolation" do
    source =
      "<svg width='100' height='40'><defs><linearGradient id='g' color-interpolation='linearRGB'><stop offset='0' stop-color='black' stop-opacity='.2'/><stop offset='1' stop-color='white' stop-opacity='.8'/></linearGradient></defs><rect width='100' height='40' fill='url(#g)'/></svg>"

    assert {:ok, png} =
             NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.png(source, [], %{
               mode: :inline
             })

    assert {:ok, image} =
             NativeElixirPdfUtilities.HtmlToPdf.PngDecoder.decode(
               png,
               NativeElixirPdfUtilities.Validators.HtmlValidator.new_image_budget(),
               false
             )

    offset = 20 * image.width_px + 50
    <<r, g, b>> = binary_part(image.data, 3 * offset, 3)
    alpha = :binary.at(image.alpha_data, offset)
    assert r in 127..129 and g == r and b == r
    assert alpha in 126..129

    composite =
      String.replace(
        source,
        "<rect width='100'",
        "<rect width='100' height='40' fill='red'/><rect width='100'"
      )

    assert {:ok, png} =
             NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.png(composite, [], %{
               mode: :inline
             })

    assert {:ok, image} =
             NativeElixirPdfUtilities.HtmlToPdf.PngDecoder.decode(
               png,
               NativeElixirPdfUtilities.Validators.HtmlValidator.new_image_budget(),
               false
             )

    <<r, g, b>> = binary_part(image.data, 3 * offset, 3)
    assert r in 190..193 and g in 63..65 and b == g

    html =
      "<svg width='100' height='40'><defs><linearGradient id='g' color-interpolation='linearRGB'><stop offset='0'/><stop offset='1' stop-color='white'/></linearGradient><filter id='f'><feGaussianBlur stdDeviation='1'/></filter></defs><rect width='100' height='40' fill='url(#g)' filter='url(#f)'/></svg>"

    assert {:ok, png_pdf} = NativeElixirPdfUtilities.HtmlToPdf.render(html)
    refute png_pdf =~ "/Subtype /Link"

    assert :ok =
             NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(
               png_pdf,
               false
             )
  end
end
