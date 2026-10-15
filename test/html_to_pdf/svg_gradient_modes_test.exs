Code.require_file("../support/svg_public_workflow.ex", __DIR__)
Code.require_file("../support/png_fixture.ex", __DIR__)

defmodule NativeElixirPdfUtilities.HtmlToPdf.SvgGradientModesTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.Pdf.Reader

  test "negative repeat and reflect periods cover the actual transformed shape extent" do
    for spread <- ~w(repeat reflect) do
      source =
        "<svg width='180' height='70'><defs><linearGradient id='g' gradientUnits='userSpaceOnUse' x1='60' y1='0' x2='90' y2='0' spreadMethod='#{spread}'><stop offset='0' stop-color='red'/><stop offset='.3' stop-color='green'/><stop offset='1' stop-color='blue'/></linearGradient></defs><rect width='180' height='70' fill='url(#g)'/></svg>"

      assert {:ok, pdf} =
               NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.pdf(source, [], %{
                 mode: :inline
               })

      refute pdf =~ "/Subtype /Link"
      assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf, false)
      assert {:ok, _doc} = Reader.read(pdf)
    end
  end

  test "radial spread extends both color and varying alpha along the same focal circles" do
    source =
      "<svg width='180' height='70'><defs><radialGradient id='g' gradientUnits='userSpaceOnUse' cx='80' cy='35' fx='75' fy='30' r='15' spreadMethod='reflect' gradientTransform='skewX(10)' color-interpolation='linearRGB'><stop offset='0' stop-color='red' stop-opacity='.2'/><stop offset='1' stop-color='blue' stop-opacity='.8'/></radialGradient></defs><rect width='180' height='70' fill='url(#g)'/></svg>"

    assert {:ok, pdf} =
             NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.pdf(source, [], %{
               mode: :inline
             })

    refute pdf =~ "/Subtype /Link"
    assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf, false)
    assert {:ok, _doc} = Reader.read(pdf)
  end

  test "linearRGB and spread paint genuine selected CID text alongside its original Type3 stroke" do
    data = File.read!("priv/fonts/dejavu/DejaVuSans.ttf")

    source =
      "<svg width='320' height='90'><defs><linearGradient id='g' x2='.2' spreadMethod='repeat' color-interpolation='linearRGB' gradientTransform='rotate(8 .5 .5)'><stop offset='0' stop-color='black' stop-opacity='.2'/><stop offset='.4' stop-color='red' stop-opacity='.9'/><stop offset='1' stop-color='white' stop-opacity='.4'/></linearGradient></defs><g transform='translate(8 4)' opacity='.7'><text x='10' y='55' font-family='NativeSvgFont0' font-size='32' fill='url(#g)' stroke='black' stroke-width='.8'>Modes ffi café Ω</text></g></svg>"

    assert {:ok, pdf} =
             NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.pdf(source, [], %{
               mode: :inline,
               fonts: [%{data: data}]
             })

    refute pdf =~ "/Subtype /Link"
    assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf, false)
    assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf)

    assert {:error, {:no_extractable_text, _}} =
             NativeElixirPdfUtilities.Text.extract(pdf, layout: false)

    assert {:ok, _doc} = Reader.read(pdf)
    refute pdf =~ "0.4166666666666667 exp"
  end

  test "host CSS interpolation and referenced object bounds preserve the canonical gradient mode" do
    html =
      "<style>defs{color-interpolation:linearRGB}</style><svg width='150' height='50'><defs><linearGradient id='g' spreadMethod='reflect' x2='.3'><stop offset='0' stop-color='black'/><stop offset='1' stop-color='white'/></linearGradient><rect id='r' width='50' height='40' fill='url(#g)'/></defs><use href='#r'/><use href='#r' x='60' transform='scale(1.2 1)'/></svg>"

    assert {:ok, pdf} = NativeElixirPdfUtilities.HtmlToPdf.render(html)
    refute pdf =~ "/Subtype /Link"
    assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf, false)
    refute pdf =~ "0.4166666666666667 exp"
  end

  test "print direct gradients preserve independent alpha while actual filter input retains declared interpolation" do
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
