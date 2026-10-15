Code.require_file("../support/svg_public_workflow.ex", __DIR__)
Code.require_file("../support/png_fixture.ex", __DIR__)

defmodule NativeElixirPdfUtilities.HtmlToPdf.SvgPublicProducerRestorationTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow

  test "slot probe transports its genuine source Form and preserves public failure diagnostics" do
    source = "<svg width='4' height='2'><rect width='4' height='2' fill='red'/></svg>"
    assert {:ok, pdf} = SvgPublicWorkflow.pdf(source, [], %{mode: :inline})
    refute pdf =~ "/Subtype /Link"
    assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf, false)
    assert pdf =~ "%PDF-"
    assert {:ok, document} = NativeElixirPdfUtilities.Pdf.Reader.read(pdf)
    assert length(document.pages) == 1
  end

  test "inserted issued HTML Form leaves keep physical operand and original DOM ownership through affine placement" do
    for {transform, clipped} <- [
          {"", false},
          {"rotate(10 40 15)", false},
          {"skewX(15)", false},
          {"", true}
        ] do
      defs =
        if clipped do
          "<defs><clipPath id='clip'><rect width='20' height='80'/></clipPath></defs><g clip-path='url(#clip)'>"
        else
          ""
        end

      ending =
        if clipped do
          "</g>"
        else
          ""
        end

      html =
        "<svg width='220' height='80'>#{defs}<foreignObject x='10' y='8' width='190' height='50' transform='#{transform}'><div style='margin:0;font-family:DejaVu Sans;font-size:16px'>Alpha ffi<b>Beta</b></div></foreignObject>#{ending}</svg>"

      assert {:error, {:unsupported_html, diagnostic}} =
               NativeElixirPdfUtilities.HtmlToPdf.render(html)

      assert diagnostic.reason == :unsupported_html
      assert diagnostic.message =~ "foreignObject"

      assert diagnostic.stage == :layout and
               diagnostic.module == NativeElixirPdfUtilities.Validators.SvgConverterValidator and
               diagnostic.operation == :convert
    end
  end

  test "internal input rasterizes the actual issued HTML painting PDF and selected Unicode" do
    html =
      "<svg width='200' height='80'><foreignObject x='10' y='10' width='180' height='50'><div style='font-family:DejaVu Sans;font-size:14px;color:#234a88;background:#e3c369'>Native input café Ω</div></foreignObject></svg>"

    assert {:error, {:unsupported_html, diagnostic}} =
             NativeElixirPdfUtilities.HtmlToPdf.render(html)

    assert diagnostic.reason == :unsupported_html
    assert diagnostic.message =~ "foreignObject"

    assert diagnostic.stage == :layout and
             diagnostic.module == NativeElixirPdfUtilities.Validators.SvgConverterValidator and
             diagnostic.operation == :convert
  end
end
