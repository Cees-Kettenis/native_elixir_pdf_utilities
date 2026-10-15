Code.require_file("../support/svg_public_workflow.ex", __DIR__)
Code.require_file("../support/png_fixture.ex", __DIR__)

defmodule NativeElixirPdfUtilities.HtmlToPdf.SvgFollowingLengthChunkTest do
  use ExUnit.Case, async: false

  test "adjusted descendant preserves a following actual absolute-position chunk" do
    data = File.read!("priv/fonts/dejavu/DejaVuSans.ttf")

    source =
      "<svg width='240' height='80'><text x='20' y='40' font-size='20' font-family='NativeSvgFont0'>A<tspan font-family='NativeSvgFont0' textLength='40'>BC</tspan><tspan font-family='NativeSvgFont0' x='110'>DE</tspan></text></svg>"

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
