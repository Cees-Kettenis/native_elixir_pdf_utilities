defmodule NativeElixirPdfUtilities.HtmlToPdf.SvgConverterDiscrepancyBrowserParityTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.HtmlToPdf.SvgRasterizer
  alias NativeElixirPdfUtilities.TestSupport.{Paths, PdfVisualCompare}
  @moduletag :browser_parity

  # Expected mismatches are discrepancy reproductions, not accepted parity results.
  # filter_input_region follows the published Filter Effects 9.4 input clip rule;
  # Chromium differs there. Known Resvg rendering differences have case-specific
  # changed-pixel and average-color bounds; the other controls stay strict.
  # https://www.w3.org/TR/filter-effects-1/#FilterPrimitiveSubRegion
  # Historical reports that no longer reproduce retain ordinary strict comparisons.
  setup do
    limits = NativeElixirPdfUtilities.Limits.effective()
    on_exit(fn -> NativeElixirPdfUtilities.Limits.install(limits) end)
    :ok
  end

  for {name, width, height, scale, mismatch, changed_ratio, average_delta} <- [
        # awaiting upstream fixes for resvg
        # https://github.com/linebender/resvg/pull/1171
        {"fractional_pattern", 160, 48, 2, false, 0.26, 0.15},
        # awaiting upstream fixes for resvg
        # https://github.com/linebender/resvg/pull/1172
        {"linear_gradient", 80, 40, 2, false, 0.92, 0.20},
        # awaiting upstream fixes for resvg
        # Every pixel differs in this mask; average color error remains bounded.
        # https://github.com/linebender/resvg/pull/1172
        {"linear_mask", 80, 40, 2, false, 1.0, 0.20},
        {"filter_input_region", 80, 40, 2, true, 0.01, 0.01},
        # awaiting upstream fixes for resvg
        # https://github.com/linebender/resvg/pull/1173
        {"marker_degenerate_tangent", 60, 60, 2, false, 0.05, 0.03},
        # awaiting upstream fixes for resvg
        # https://github.com/linebender/resvg/pull/1173
        {"marker_closed_join", 70, 70, 2, false, 0.05, 0.025},
        {"marker_child_percentages", 60, 40, 2, false, 0.01, 0.01},
        # awaiting upstream fixes for resvg
        # https://github.com/linebender/resvg/pull/1174
        {"small_canvas_stroke", 64, 1, 1, false, 0.26, 0.17},
        {"small_canvas_stroke", 64, 1, 2, false, 0.01, 0.01}
      ] do
    @name name
    @width width
    @height height
    @mismatch mismatch
    @scale scale
    @changed_ratio changed_ratio
    @average_delta average_delta
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
            max_changed_ratio: @changed_ratio,
            max_average_delta: @average_delta
          )
        end

        if @mismatch do
          assert_raise ExUnit.AssertionError, ~r/changed-pixel threshold exceeded/, compare
        else
          assert compare.().max_changed_ratio <= @changed_ratio
        end
      end

      assert File.read!(Path.join(root, "capture-1/chromium-svg-0.png")) ==
               File.read!(Path.join(root, "capture-2/chromium-svg-0.png"))
    end
  end
end
