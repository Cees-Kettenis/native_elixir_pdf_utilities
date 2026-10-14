defmodule NativeElixirPdfUtilities.HtmlToPdf.Wave20OriginalSvgBrowserTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.TestSupport.PdfVisualCompare
  @moduletag :browser_parity

  test "original zero-size foreignObject input reports unsupported conversion with browser reference" do
    fixture =
      Path.expand(
        "../fixtures/html_to_pdf/browser_parity/svg_original_zero_foreign_object.html",
        __DIR__
      )

    stats =
      PdfVisualCompare.assert_browser_unsupported!(fixture,
        render_opts: [fonts: []],
        artifact_dir: "tmp/browser_parity/svg_original_zero_foreign_object",
        max_changed_ratio: 0.01,
        max_average_delta: 0.01
      )

    assert stats.status == :unsupported_feature
    assert stats.visual_comparison == :not_performed
    assert stats.chromium_page_count == 1
    assert stats.font_sources == []

    assert File.read!(Path.join(stats.artifact_dir, "chromium-input.html")) ==
             File.read!(fixture) <>
               "\n<style>\n  @page { size: 612pt 792pt; margin: 0; }\n  html, body { margin: 0; }\n</style>\n"
  end

  for fixture <- [
        "svg_original_stroke_profile.html",
        "html_svg_document_grid_initial_canvas.html"
      ] do
    @fixture fixture
    test "original physical paint and grid canvas match Chromium for #{@fixture}" do
      fixture = Path.expand("../fixtures/html_to_pdf/browser_parity/#{@fixture}", __DIR__)

      stats =
        PdfVisualCompare.assert_browser_match!(fixture,
          render_opts: [fonts: []],
          artifact_dir: "tmp/browser_parity/#{Path.rootname(@fixture)}",
          max_changed_ratio: 0.01,
          max_average_delta: 0.01
        )

      assert stats.page_count == 1
      assert stats.font_sources == []

      case @fixture do
        "html_svg_document_grid_initial_canvas.html" ->
          refute File.exists?(Path.join(stats.artifact_dir, "chromium-input.html"))

        _ ->
          # Unpaged original fragments use the helper's established Letter
          # reference geometry. Their complete source remains the exact prefix.
          assert File.read!(Path.join(stats.artifact_dir, "chromium-input.html")) ==
                   File.read!(fixture) <>
                     "\n<style>\n  @page { size: 612pt 792pt; margin: 0; }\n  html, body { margin: 0; }\n</style>\n"
      end
    end
  end
end
