defmodule NativeElixirPdfUtilities.HtmlToPdf.SvgMasksTest do
  Code.require_file("../support/svg_public_workflow.ex", __DIR__)
  Code.require_file("../support/png_fixture.ex", __DIR__)
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.{HtmlToPdf, Limits}

  setup do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    %{limits: limits}
  end

  test "original luminance and alpha mask artwork uses the converter image transport" do
    html =
      File.read!(
        Path.expand("../fixtures/html_to_pdf/browser_parity/inline_svg_masks.html", __DIR__)
      )

    assert {:ok, pdf} = HtmlToPdf.render(html)
    assert pdf =~ "/Subtype /Image"
    assert pdf =~ "/SMask"
    assert {:ok, _} = NativeElixirPdfUtilities.Pdf.Reader.read(pdf)
  end

  alias NativeElixirPdfUtilities.{Limits}

  test "luminance and alpha masks become PDF transparency masks over native vector artwork" do
    html =
      File.read!(
        Path.expand(
          "../fixtures/html_to_pdf/browser_parity/inline_svg_masks.html",
          __DIR__
        )
      )

    assert {:ok, pdf} = NativeElixirPdfUtilities.HtmlToPdf.render(html)
    refute pdf =~ "/Subtype /Link"
    assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf)

    (
      assert {:ok, text} = NativeElixirPdfUtilities.Text.extract(pdf, layout: false)
      assert text =~ "Native vector masks Ω café"
      assert text =~ "Masks retained genuine selected glyphs and paint order."
    )
  end
end
