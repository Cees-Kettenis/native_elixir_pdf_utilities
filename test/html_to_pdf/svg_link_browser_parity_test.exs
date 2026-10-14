defmodule NativeElixirPdfUtilities.HtmlToPdf.SvgLinkBrowserParityTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.TestSupport.PdfVisualCompare
  @moduletag :browser_parity
  @moduletag :svg_glyph_shape_links

  test "original selected glyph and shape hyperlinks retain Chromium paint" do
    fixture =
      Path.expand(
        "../fixtures/html_to_pdf/browser_parity/inline_svg_glyph_shape_links_instances.html",
        __DIR__
      )

    stats =
      PdfVisualCompare.assert_browser_match!(fixture,
        max_changed_ratio: 0.01,
        max_average_delta: 0.01
      )

    assert stats.page_count == 1

    native = File.read!(Path.join(stats.artifact_dir, "native.pdf"))
    assert :ok = PdfVisualCompare.assert_svg_image_without_text!(native)

    assert {:ok, text} =
             NativeElixirPdfUtilities.Text.extract(
               File.read!(Path.join(stats.artifact_dir, "chromium.pdf")),
               layout: false
             )

    assert {:ok, document} = NativeElixirPdfUtilities.Pdf.Reader.read(native)

    refute Enum.any?(document.objects, fn {_ref, %{value: value}} ->
             is_map(value) and value["Subtype"] == {:name, "Link"}
           end)

    assert text =~ "ffi Ωé selected ABC"
    assert text =~ "Curved destination Ωé"
  end
end
