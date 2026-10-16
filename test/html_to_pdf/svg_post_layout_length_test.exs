Code.require_file("../support/svg_public_workflow.ex", __DIR__)
Code.require_file("../support/png_fixture.ex", __DIR__)

defmodule NativeElixirPdfUtilities.HtmlToPdf.SvgPostLayoutLengthTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.{Limits}

  setup do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    %{data: File.read!("priv/fonts/dejavu/DejaVuSans.ttf"), limits: limits}
  end

  test("post-layout scaling preserves source attributes and paints selected original fonts", %{
    data: data
  }) do
    source =
      "<svg width='240' height='80'><text x='20' y='40' font-size='20' font-family='NativeSvgFont0' textLength='120' lengthAdjust='spacingAndGlyphs'>AΩ<tspan font-family='NativeSvgFont0'>éD</tspan>EF</text></svg>"

    context = %{mode: :inline, fonts: [%{data: data}]}

    assert {:ok, pdf} =
             NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.pdf(source, [], context)

    refute pdf =~ "/Subtype /Link"
    assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf, false)
    assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf)

    assert {:error, {:no_extractable_text, _}} =
             NativeElixirPdfUtilities.Text.extract(pdf, layout: false)

    assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf)

    assert {:error, {:no_extractable_text, _}} =
             NativeElixirPdfUtilities.Text.extract(pdf, layout: false)

    assert {:ok, _doc} = NativeElixirPdfUtilities.Pdf.Reader.read(pdf)

    assert {:ok, adjusted} =
             NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.png(source, [], context)

    natural = String.replace(source, "textLength='120'", "")

    assert {:ok, unadjusted} =
             NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.png(natural, [], context)

    assert adjusted != unadjusted
  end
end
