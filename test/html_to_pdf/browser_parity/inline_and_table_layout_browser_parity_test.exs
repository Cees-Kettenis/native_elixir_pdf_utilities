alias NativeElixirPdfUtilities.TestSupport.Paths

defmodule NativeElixirPdfUtilities.HtmlToPdf.InlineAndTableLayoutBrowserParityTest do
  use ExUnit.Case, async: true
  alias NativeElixirPdfUtilities.TestSupport.PdfVisualCompare
  @moduletag :browser_parity
  @moduletag :inline_and_table_layout
  @fixtures Paths.fixture("html_to_pdf/browser_parity")

  for fixture <-
        ~w(atomic_table_visible_overflow.html atomic_table_hidden_overflow.html interrupted_inline_nested_positioning.html atomic_table_oversized_pagination.html interrupted_inline_decoration.html anonymous_table_cells_and_borders.html table_percentage_baselines.html table_empty_percentage_baselines.html clipped_form_controls.html list_ruby_table_pagination.html atomic_table_nested_pagination.html interrupted_inline_pagination.html interrupted_inline_vertical_alignment.html interrupted_inline_empty_fragments.html interrupted_inline_images_and_backgrounds.html) do
    @fixture fixture
    test "matches Chromium for #{@fixture}" do
      stats =
        PdfVisualCompare.assert_browser_match!(Path.join(@fixtures, @fixture),
          artifact_dir:
            Path.join("tmp/browser_parity/inline_and_table_layout", Path.rootname(@fixture)),
          max_changed_ratio: 0.01,
          max_average_delta: 0.01
        )

      assert stats.page_count >= 1
    end
  end
end
