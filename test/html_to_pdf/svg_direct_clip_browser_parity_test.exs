defmodule NativeElixirPdfUtilities.HtmlToPdf.SvgDirectClipBrowserParityTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.TestSupport.PdfVisualCompare
  @moduletag :browser_parity
  @moduletag :svg_converter_contract
  @moduletag :svg_direct_clip

  test "direct SVG image links stay inert while retaining Chromium paint" do
    fixture =
      Path.expand(
        "../fixtures/html_to_pdf/browser_parity/inline_svg_direct_clip_links.html",
        __DIR__
      )

    stats =
      PdfVisualCompare.assert_browser_match!(fixture,
        max_changed_ratio: 0.01,
        max_average_delta: 0.01
      )

    assert stats.page_count == 1
    pdf = File.read!(Path.join(stats.artifact_dir, "native.pdf"))
    assert {:ok, doc} = NativeElixirPdfUtilities.Pdf.Reader.read(pdf)

    refute Enum.any?(doc.objects, fn {_ref, %{value: value}} ->
             is_map(value) and value["Subtype"] == {:name, "Link"}
           end)

    assert {:ok, "Native shape and selected-font links follow exact clips"} =
             NativeElixirPdfUtilities.Text.extract(pdf, layout: false)

    assert {:ok, chromium} =
             NativeElixirPdfUtilities.Text.extract(
               File.read!(Path.join(stats.artifact_dir, "chromium.pdf")),
               layout: false
             )

    assert chromium =~ "Native shape and selected-font links follow exact clips"
    assert chromium =~ "café Ω"

    assert :ok = PdfVisualCompare.assert_svg_image_artwork!(pdf)
  end
end
