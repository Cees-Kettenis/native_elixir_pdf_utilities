Code.require_file("../support/svg_converted_image.ex", __DIR__)
Code.require_file("../support/svg_public_workflow.ex", __DIR__)
Code.require_file("../support/png_fixture.ex", __DIR__)

defmodule NativeElixirPdfUtilities.HtmlToPdf.SvgTextLengthTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.TestSupport.SvgConvertedImage
  alias NativeElixirPdfUtilities.{Limits}

  setup do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    %{data: File.read!("priv/fonts/dejavu/DejaVuSans.ttf"), limits: limits}
  end

  test "parent length and positioned chunks paint through the existing converter" do
    source =
      "<svg width=200 height=80><text y='40' textLength='120'>A<tspan>B</tspan></text></svg>"

    assert SvgConvertedImage.coverage(SvgConvertedImage.render(source)) > 20
    source = "<svg width=200 height=80><text y='40' x='0 30' textLength='120'>AB</text></svg>"
    assert SvgConvertedImage.coverage(SvgConvertedImage.render(source)) > 20
  end

  test("spacing and glyph scaling paint real selected characters across the authored length", %{
    data: data
  }) do
    for mode <- ["spacing", "spacingAndGlyphs"] do
      source =
        "<svg width='240' height='80'><text x='20' y='40' font-size='20' font-family='NativeSvgFont0' textLength='120' lengthAdjust='#{mode}'>ABCD</text></svg>"

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

      assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf)

      assert {:error, {:no_extractable_text, _}} =
               NativeElixirPdfUtilities.Text.extract(pdf, layout: false)

      assert {:ok, _doc} = NativeElixirPdfUtilities.Pdf.Reader.read(pdf)
    end
  end

  test "length units and anchors retain actual host font and viewport contexts" do
    html =
      "<svg width=280 height=150 font-size=20><text x=140 y=30 text-anchor=middle textLength='6em' lengthAdjust=spacing>ABCD</text><text x=140 y=80 text-anchor=end textLength='50%' lengthAdjust=spacingAndGlyphs>ABCD</text></svg>"

    assert {:ok, pdf} = NativeElixirPdfUtilities.HtmlToPdf.render(html)
    refute pdf =~ "/Subtype /Link"
    assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf, false)
    assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf)

    assert {:error, {:no_extractable_text, _}} =
             NativeElixirPdfUtilities.Text.extract(pdf, layout: false)

    assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf)

    assert {:error, {:no_extractable_text, _}} =
             NativeElixirPdfUtilities.Text.extract(pdf, layout: false)
  end

  test "parent length distributes across descendants while absolute chunk distributions stay diagnosed" do
    assert {:ok, pdf} =
             NativeElixirPdfUtilities.HtmlToPdf.render(
               "<svg width=200 height=80><text y='40' textLength='120'>A<tspan>B</tspan></text></svg>"
             )

    refute pdf =~ "/Subtype /Link"
    assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf, false)
    assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf)

    assert {:error, {:no_extractable_text, _}} =
             NativeElixirPdfUtilities.Text.extract(pdf, layout: false)

    assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf)

    assert {:error, {:no_extractable_text, _}} =
             NativeElixirPdfUtilities.Text.extract(pdf, layout: false)

    assert {:ok, pdf} =
             NativeElixirPdfUtilities.HtmlToPdf.render(
               "<svg width=200 height=80><text y='40' x='0 30' textLength='120'>AB</text></svg>"
             )

    assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf, false)

    assert {:error, {:no_extractable_text, _}} =
             NativeElixirPdfUtilities.Text.extract(pdf, layout: false)

    refute pdf =~ "/Subtype /Link"
  end

  test(
    "source preparation preserves original Unicode and IDs and normalizes before both paints",
    %{data: data}
  ) do
    source =
      "<svg width='240' height='120'><g id='NativeTextLength1'/><text x='120' y='40' text-anchor='middle' textLength='50%' font-family='NativeSvgFont0'>AΩB</text></svg>"

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

    assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf)

    assert {:error, {:no_extractable_text, _}} =
             NativeElixirPdfUtilities.Text.extract(pdf, layout: false)

    assert {:ok, png} =
             NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.png(source, [], %{
               mode: :inline,
               fonts: [%{data: data}]
             })

    assert png =~ <<137, 80, 78, 71>>
  end

  test(
    "independent adjusted spans preserve original UTF-8 ranges and the scaled following cursor",
    %{data: data}
  ) do
    source =
      "<svg width='240' height='120'><text x='20' y='40' font-family='NativeSvgFont0'>A<tspan font-family='NativeSvgFont0' textLength='120' lengthAdjust='spacing'>ΩBéD</tspan>F</text></svg>"

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

    assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf)

    assert {:error, {:no_extractable_text, _}} =
             NativeElixirPdfUtilities.Text.extract(pdf, layout: false)

    scaled = String.replace(source, "lengthAdjust='spacing'", "lengthAdjust='spacingAndGlyphs'")

    assert {:ok, scaled_pdf} =
             NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.pdf(scaled, [], %{
               mode: :inline,
               fonts: [%{data: data}]
             })

    refute scaled_pdf =~ "/Subtype /Link"

    assert :ok =
             NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(
               scaled_pdf,
               false
             )

    assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(scaled_pdf)

    assert {:error, {:no_extractable_text, _}} =
             NativeElixirPdfUtilities.Text.extract(scaled_pdf, layout: false)

    assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(scaled_pdf)

    assert {:error, {:no_extractable_text, _}} =
             NativeElixirPdfUtilities.Text.extract(scaled_pdf, layout: false)

    assert {:ok, _png} =
             NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.png(scaled, [], %{
               mode: :inline,
               fonts: [%{data: data}]
             })
  end
end
