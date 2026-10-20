defmodule NativeElixirPdfUtilities.HtmlToPdf.SvgConverterDiscrepancyBrowserParityTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.HtmlToPdf.SvgRasterizer
  alias NativeElixirPdfUtilities.TestSupport.{Paths, PdfVisualCompare}
  @moduletag :browser_parity

  # Expected mismatches are discrepancy reproductions, not accepted parity results.
  # filter_input_region follows the published Filter Effects 9.4 input clip rule;
  # Chromium differs there. Corrected Resvg cases use strict passing comparisons.
  # https://www.w3.org/TR/filter-effects-1/#FilterPrimitiveSubRegion
  # Historical reports that no longer reproduce retain ordinary strict comparisons.
  setup do
    limits = NativeElixirPdfUtilities.Limits.effective()
    on_exit(fn -> NativeElixirPdfUtilities.Limits.install(limits) end)
    :ok
  end

  for {name, width, height, scale, mismatch} <- [
        {"fractional_pattern", 160, 48, 2, false},
        {"linear_gradient", 80, 40, 2, false},
        {"linear_mask", 80, 40, 2, false},
        {"filter_input_region", 80, 40, 2, true},
        {"marker_degenerate_tangent", 60, 60, 2, false},
        {"marker_closed_join", 70, 70, 2, false},
        {"marker_child_percentages", 60, 40, 2, false},
        {"small_canvas_stroke", 64, 1, 1, false},
        {"small_canvas_stroke", 64, 1, 2, false}
      ] do
    @name name
    @width width
    @height height
    @mismatch mismatch
    @scale scale
    test "classifies original #{@name} at density #{@scale} through standalone Resvg and repeated Chromium captures" do
      NativeElixirPdfUtilities.Limits.install(
        Map.put(NativeElixirPdfUtilities.Limits.effective(), :svg_raster_scale, @scale)
      )

      source = File.read!(Paths.fixture("html_to_pdf/svg/#{@name}.svg"))
      options = [width: @width * @scale, height: @height * @scale]
      assert {:ok, actual} = SvgRasterizer.rasterize(source, options, nil)

      assert {:ok, standalone} =
               Resvg.svg_string_to_png_binary(
                 source,
                 options ++ [skip_system_fonts: true, resources_dir: System.tmp_dir!()]
               )

      assert actual == standalone

      root =
        Paths.project("tmp/browser_parity/svg_converter_discrepancies/#{@name}/density-#{@scale}")

      File.mkdir_p!(root)
      fixture = Path.join(root, "original.html")

      File.write!(fixture, """
      <!doctype html><html><head><style>
      @page{size:#{@width * 0.75}pt #{max(@height, 4) * 0.75}pt;margin:0}
      html,body{margin:0;padding:0}svg{display:block}
      </style></head><body>#{source}</body></html>
      """)

      for attempt <- 1..2 do
        compare = fn ->
          PdfVisualCompare.assert_browser_match!(fixture,
            artifact_dir: Path.join(root, "capture-#{attempt}"),
            render_opts: [fonts: []],
            svg_reference: :png,
            max_changed_ratio: 0.01,
            max_average_delta: 0.01
          )
        end

        if @mismatch do
          assert_raise ExUnit.AssertionError, ~r/changed-pixel threshold exceeded/, compare
        else
          assert compare.().max_changed_ratio < 0.01
        end
      end

      assert File.read!(Path.join(root, "capture-1/chromium-svg-0.png")) ==
               File.read!(Path.join(root, "capture-2/chromium-svg-0.png"))
    end
  end
end
