defmodule NativeElixirPdfUtilities.HtmlToPdf.CssCompositingBrowserParityTest do
  use ExUnit.Case, async: true
  @moduletag :browser_parity
  alias NativeElixirPdfUtilities.TestSupport.{Paths, PdfVisualCompare}

  test "HTML compositing applies group alpha, nested blending and isolation" do
    # The existing painting sheet has no room for these overlapping scenes.
    # All forty effect/transform scenes share one new reference instead of
    # adding an independent browser capture for each property.
    # Filter rasters use the shared SVG converter and its existing comparison
    # tolerance. Vector compositing keeps the stricter 1% regions above.
    stats =
      PdfVisualCompare.assert_browser_match!(
        Paths.fixture("html_to_pdf/browser_parity/css_compositing.html"),
        artifact_dir: Paths.project("tmp/browser_parity/css_compositing"),
        render_opts: [forms: :static],
        # Chromium's print painter loses CSS backface, plane intersections and
        # mask exclusion. Capture only the nine marked source viewports from
        # its print-media compositor, using the existing SVG capture adapter.
        # Every native input, page bound and original vector threshold stays
        # unchanged. Compositor evidence and the original printed PDF are kept.
        compositor_reference: :png,
        regions: [
          overlap: {10, 30, 100, 70},
          nested: {150, 30, 100, 70},
          multiply: {290, 30, 100, 70},
          isolated: {10, 160, 100, 70},
          transformed: {150, 160, 100, 70},
          inline: {290, 160, 100, 70},
          background: {12, 282, 102, 72},
          layers: {150, 282, 102, 72},
          field: {288, 282, 102, 72},
          blur: {10, 406, 112, 82},
          color_filter: {150, 408, 102, 72},
          drop_shadow: {286, 406, 112, 82},
          circle_clip: {12, 528, 102, 72},
          polygon_clip: {150, 528, 102, 72},
          path_hole: {288, 528, 102, 72},
          alpha_mask: {12, 648, 102, 72},
          luminance_mask: {150, 648, 102, 72},
          mask_exclusion: {288, 648, 102, 72},
          individual: {10, 766, 112, 82},
          content_reference: {148, 766, 112, 82},
          static_inline_offsets: {288, 768, 102, 72},
          perspective_function: {10, 886, 112, 82},
          perspective_ancestor: {148, 886, 112, 82},
          backface: {286, 886, 112, 82},
          intersections: {10, 1006, 112, 82},
          flattened: {148, 1006, 112, 82},
          opacity_flattening: {286, 1006, 112, 82},
          path_start: {10, 1126, 112, 82},
          path_middle: {148, 1126, 112, 82},
          path_end: {286, 1126, 112, 82},
          path_tangent: {10, 1246, 112, 82},
          local_path: {148, 1246, 112, 82},
          ray: {286, 1246, 112, 82},
          closed_path: {10, 1366, 112, 82},
          offset_order: {148, 1366, 112, 82},
          backdrop: {286, 1366, 112, 82},
          circle_path: {10, 1486, 112, 82},
          inset_path: {148, 1486, 112, 82},
          referenced_filter: {286, 1486, 112, 82},
          colored_control_opacity: {12, 1608, 102, 72}
        ],
        region_thresholds:
          Map.new(
            ~w(overlap nested multiply isolated transformed inline background layers field individual content_reference static_inline_offsets)a,
            &{&1, %{max_changed_ratio: 0.01, max_average_delta: 0.01}}
          ),
        max_changed_ratio: 0.12,
        max_average_delta: 0.03
      )

    # Compositor text uses screen glyph AA rather than PDF glyph hinting. Its
    # new region uses the established raster tolerance; this independent channel
    # check distinguishes group alpha from text alpha over the faded green fill.
    for source <- ["native", "chromium"] do
      picture =
        PdfVisualCompare.read_ppm!(
          Paths.project("tmp/browser_parity/css_compositing/#{source}-1.ppm")
        )

      blue =
        for y <- 1608..1631,
            x <- 12..95,
            <<r, g, b>> = binary_part(picture.pixels, (y * picture.width + x) * 3, 3),
            b > 240 and r < 180 and g < 180,
            do: {r, g, b}

      assert blue != []
      {r, g, b} = Enum.min_by(blue, fn {r, g, _} -> r + g end)
      assert abs(r - 128) <= 3 and abs(g + b / 2 - 255) <= 4 and b > 240
    end

    assert stats.page_count == 1
    assert Enum.all?(hd(stats.pages).regions, &(&1.native_ink_pixels > 100))
  end
end
