Code.require_file("../support/svg_public_workflow.ex", __DIR__)
Code.require_file("../support/png_fixture.ex", __DIR__)

defmodule NativeElixirPdfUtilities.HtmlToPdf.SvgNestedScaledLengthTest do
  use ExUnit.Case, async: false

  test "composite scaling stretches actual selected glyphs across original descendant spans" do
    data = File.read!("priv/fonts/dejavu/DejaVuSans.ttf")

    for {outer, inner} <- [
          {"spacingAndGlyphs", ""},
          {"spacing", "textLength='40' lengthAdjust='spacingAndGlyphs'"},
          {"spacingAndGlyphs", "textLength='40'"}
        ] do
      source =
        "<svg width='240' height='80'><text x='20' y='40' font-size='20' font-family='NativeSvgFont0' textLength='120' lengthAdjust='#{outer}'>AB<tspan font-family='NativeSvgFont0' #{inner}>CD</tspan>EF</text></svg>"

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

      assert {:ok, _png} =
               NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.png(source, [], %{
                 mode: :inline,
                 fonts: [%{data: data}]
               })
    end
  end

  test "scaled independent span retains multiple prefixes and the following real text cursor" do
    data = File.read!("priv/fonts/dejavu/DejaVuSans.ttf")

    source =
      "<svg width='240' height='80'><text x='20' y='40' font-size='20' font-family='NativeSvgFont0'>A<tspan font-family='NativeSvgFont0'>B</tspan><tspan font-family='NativeSvgFont0' textLength='40' lengthAdjust='spacingAndGlyphs'>CD</tspan>EF</text></svg>"

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
  end
end
