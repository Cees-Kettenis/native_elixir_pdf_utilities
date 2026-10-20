defmodule NativeElixirPdfUtilities.HtmlToPdf.SvgXmlTransportBrowserParityTest do
  use ExUnit.Case, async: true
  alias NativeElixirPdfUtilities.{HtmlToPdf, Text}
  alias NativeElixirPdfUtilities.TestSupport.{Paths, PdfVisualCompare}
  @moduletag :browser_parity

  test "original XML keeps host geometry across image, selected document and multipage composition" do
    fixture = Paths.fixture("html_to_pdf/browser_parity/svg_xml_entry_paths.html")

    stats =
      PdfVisualCompare.assert_browser_match!(fixture,
        native_renderer: fn html, opts ->
          PdfVisualCompare.render_embedded_font_bytes(html, opts, fixture)
        end,
        svg_reference: :png,
        max_changed_ratio: 0.01,
        max_average_delta: 0.01
      )

    assert stats.page_count == 2
    assert {:ok, pdf} = HtmlToPdf.render(File.read!(fixture), base_url: Path.dirname(fixture))
    assert {:ok, text} = Text.extract(pdf, layout: false)
    assert text =~ "SVG XML through images" and text =~ "SVG XML on the second page"
    assert pdf =~ "/Subtype /Image" and pdf =~ "/Subtype /Link" and pdf =~ "/AcroForm"
  end
end
