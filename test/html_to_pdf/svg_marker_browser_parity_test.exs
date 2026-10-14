defmodule NativeElixirPdfUtilities.HtmlToPdf.SvgMarkerBrowserParityTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.TestSupport.PdfVisualCompare
  @moduletag :browser_parity
  @moduletag :svg_marker_geometry
  test "actual marker viewport clipping follows meet and slice on the unchanged source" do
    path =
      Path.expand(
        "../fixtures/html_to_pdf/browser_parity/svg_marker_viewport_geometry.html",
        __DIR__
      )

    stats =
      PdfVisualCompare.assert_browser_match!(path,
        max_changed_ratio: 0.01,
        max_average_delta: 0.01
      )

    assert stats.page_count == 1
  end

  test "marker shorthand, context paint, curve directions, subpaths and nested transforms" do
    path =
      Path.expand(
        "../fixtures/html_to_pdf/browser_parity/svg_marker_context_orientation.html",
        __DIR__
      )

    stats =
      PdfVisualCompare.assert_browser_match!(path,
        max_changed_ratio: 0.01,
        max_average_delta: 0.01
      )

    assert stats.page_count == 1
  end

  @tag :svg_marker_image_dependency
  test "marker source retains actual image, object, embed and iframe child contexts" do
    path =
      Path.expand(
        "../fixtures/html_to_pdf/browser_parity/svg_marker_embedded_contexts.html",
        __DIR__
      )

    stats =
      PdfVisualCompare.assert_browser_match!(path,
        render_opts: [fonts: []],
        max_changed_ratio: 0.01,
        max_average_delta: 0.01
      )

    assert stats.page_count == 1
    refute File.exists?(Path.join(stats.artifact_dir, "chromium-input.html"))
  end

  test "marker artwork retains actual object, embed and iframe document owner geometry" do
    path =
      Path.expand(
        "../fixtures/html_to_pdf/browser_parity/svg_marker_document_contexts.html",
        __DIR__
      )

    stats =
      PdfVisualCompare.assert_browser_match!(path,
        render_opts: [fonts: []],
        max_changed_ratio: 0.01,
        max_average_delta: 0.01
      )

    assert stats.page_count == 1
    refute File.exists?(Path.join(stats.artifact_dir, "chromium-input.html"))
  end
end
