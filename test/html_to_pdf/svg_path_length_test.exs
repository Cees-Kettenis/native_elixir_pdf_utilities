Code.require_file("../support/svg_public_workflow.ex", __DIR__)
Code.require_file("../support/png_fixture.ex", __DIR__)

defmodule NativeElixirPdfUtilities.HtmlToPdf.SvgPathLengthTest do
  use ExUnit.Case, async: false

  test "zero author length keeps zero intervals and both painters' dash representation finite" do
    for pattern <- ["5 5", "0 1", "1 0", "0 0", "0 1 1 1"] do
      for offset <- ["0", "1", "-1"] do
        source =
          "<svg width='240' height='120'><path d='M10 40 H210' pathLength='0' fill='none' stroke='#174a86' stroke-width='3' stroke-linecap='round' stroke-dasharray='#{pattern}' stroke-dashoffset='#{offset}'/></svg>"

        assert {:ok, pdf} =
                 NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.pdf(source, [], %{
                   mode: :inline
                 })

        refute pdf =~ "/Subtype /Link"

        assert :ok =
                 NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf, false)

        assert {:ok, _document} = NativeElixirPdfUtilities.Pdf.Reader.read(pdf)
        refute pdf =~ "inf"
        refute pdf =~ "NaN"

        assert {:ok, _png} =
                 NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.png(source, [], %{
                   mode: :inline
                 })
      end
    end
  end
end
