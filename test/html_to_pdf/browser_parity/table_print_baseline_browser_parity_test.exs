alias NativeElixirPdfUtilities.TestSupport.Paths

defmodule NativeElixirPdfUtilities.HtmlToPdf.TablePrintBaselineBrowserParityTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.TestSupport.PdfVisualCompare
  @moduletag :browser_parity
  @moduletag :table_print_baseline_context
  @fixtures Paths.fixture("html_to_pdf/browser_parity")
  for fixture <- ["html_table_print_baselines.html", "html_table_print_baseline_headers.html"] do
    @fixture fixture
    test "original paginated table baseline geometry matches Chromium for #{@fixture}" do
      stats =
        PdfVisualCompare.assert_browser_match!(Path.join(@fixtures, @fixture),
          max_changed_ratio: 0.01,
          max_average_delta: 0.01
        )

      assert stats.page_count >= 1
    end
  end

  test "original child viewport retains unfragmented row baseline alignment" do
    path = Path.join(@fixtures, "html_table_child_viewport_baselines.html")

    stats =
      PdfVisualCompare.with_embedded_font_configuration(path, fn ->
        PdfVisualCompare.assert_browser_match!(path,
          render_opts: [fonts: []],
          native_renderer: fn html, opts ->
            PdfVisualCompare.render_embedded_font_bytes(html, opts, path)
          end,
          max_changed_ratio: 0.01,
          max_average_delta: 0.01
        )
      end)

    assert stats.page_count == 1
    assert stats.embedded_fonts.chromium =~ "DejaVuSans"
    assert stats.embedded_fonts.native =~ "DejaVuSans"
    refute File.exists?(Path.join(stats.artifact_dir, "chromium-input.html"))
  end
end
