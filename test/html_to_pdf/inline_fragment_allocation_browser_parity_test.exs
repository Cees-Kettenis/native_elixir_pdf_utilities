defmodule NativeElixirPdfUtilities.HtmlToPdf.InlineFragmentAllocationBrowserParityTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.TestSupport.PdfVisualCompare
  @moduletag :browser_parity
  @moduletag :inline_fragment_allocation
  @fixtures Path.expand("../fixtures/html_to_pdf/browser_parity", __DIR__)

  test "final styled fragments retain Chromium's occupied width and line alignment" do
    stats =
      PdfVisualCompare.assert_browser_match!(
        Path.join(@fixtures, "html_inline_fragment_allocation.html"),
        render_opts: [
          fonts: [
            %{
              family: "Fixture Sans",
              path: Path.expand("../../priv/fonts/dejavu/DejaVuSans.ttf", __DIR__)
            }
          ],
          system_font_discovery: false
        ],
        max_changed_ratio: 0.01,
        max_average_delta: 0.01
      )

    assert stats.page_count == 1
  end

  for fixture <-
        ~w(css_inline_block_baselines.html html_inline_word_boundaries.html html_decorated_list_fragments.html) do
    @fixture fixture
    test "original #{fixture} retains its source glyph geometry" do
      stats =
        PdfVisualCompare.assert_browser_match!(Path.join(@fixtures, @fixture),
          max_changed_ratio: 0.01,
          max_average_delta: 0.01
        )

      assert stats.page_count >= 1
    end
  end
end
