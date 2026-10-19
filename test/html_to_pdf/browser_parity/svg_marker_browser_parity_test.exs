alias NativeElixirPdfUtilities.TestSupport.Paths

defmodule NativeElixirPdfUtilities.HtmlToPdf.SvgMarkerBrowserParityTest do
  use ExUnit.Case, async: true
  alias NativeElixirPdfUtilities.TestSupport.PdfVisualCompare
  @moduletag :browser_parity
  @moduletag :svg_marker_geometry
  test "marker viewport clipping follows meet and slice on the unchanged source" do
    path =
      Paths.fixture("html_to_pdf/browser_parity/svg_marker_viewport_geometry.html")

    stats =
      PdfVisualCompare.assert_browser_match!(path,
        svg_reference: :png,
        max_changed_ratio: 0.01,
        max_average_delta: 0.01
      )

    assert stats.page_count == 1
  end

  @tag :svg_marker_image_dependency
  test "marker source retains image, object, embed and iframe child contexts" do
    path =
      Paths.fixture("html_to_pdf/browser_parity/svg_marker_embedded_contexts.html")

    stats =
      PdfVisualCompare.assert_browser_match!(path,
        render_opts: [fonts: []],
        svg_reference: :png,
        max_changed_ratio: 0.01,
        max_average_delta: 0.01
      )

    assert stats.page_count == 1
    refute File.exists?(Path.join(stats.artifact_dir, "chromium-input.html"))
  end

  test "marker artwork retains object, embed and iframe document owner geometry" do
    path =
      Paths.fixture("html_to_pdf/browser_parity/svg_marker_document_contexts.html")

    stats =
      PdfVisualCompare.assert_browser_match!(path,
        render_opts: [fonts: []],
        svg_reference: :png,
        max_changed_ratio: 0.01,
        max_average_delta: 0.01
      )

    assert stats.page_count == 1
    refute File.exists?(Path.join(stats.artifact_dir, "chromium-input.html"))
  end
end
