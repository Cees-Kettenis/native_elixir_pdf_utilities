alias NativeElixirPdfUtilities.TestSupport.Paths

Code.require_file(Paths.support("svg_public_workflow.ex"))
Code.require_file(Paths.support("png_fixture.ex"))

defmodule NativeElixirPdfUtilities.HtmlToPdf.SvgEmbeddedFormSourceTransportTest do
  use ExUnit.Case, async: true
  alias NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow

  test "slot probe transports its source Form and preserves public failure diagnostics" do
    source = "<svg width='4' height='2'><rect width='4' height='2' fill='red'/></svg>"
    assert {:ok, pdf} = SvgPublicWorkflow.pdf(source, [], %{mode: :inline})
    refute pdf =~ "/Subtype /Link"
    assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf, false)
    assert pdf =~ "%PDF-"
    assert {:ok, document} = NativeElixirPdfUtilities.Pdf.Reader.read(pdf)
    assert length(document.pages) == 1
  end
end
