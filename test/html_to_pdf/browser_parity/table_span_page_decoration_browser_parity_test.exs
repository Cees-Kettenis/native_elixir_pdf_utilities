alias NativeElixirPdfUtilities.TestSupport.Paths

defmodule NativeElixirPdfUtilities.HtmlToPdf.TableSpanPageDecorationBrowserParityTest do
  use ExUnit.Case, async: true
  alias NativeElixirPdfUtilities.TestSupport.PdfVisualCompare
  @moduletag :browser_parity
  @moduletag :table_span_page_decoration
  @fixture Paths.fixture("html_to_pdf/browser_parity/html_table_span_page_decorations.html")
  test "original spanning surface consumes fragmentainer capacity and retains final cell extent" do
    stats =
      PdfVisualCompare.assert_browser_match!(@fixture,
        max_changed_ratio: 0.01,
        max_average_delta: 0.01
      )

    assert stats.page_count == 2
  end

  test "original translucent span respects row-group and row background layers" do
    fixture =
      Paths.fixture("html_to_pdf/browser_parity/html_table_span_layered_backgrounds.html")

    stats =
      PdfVisualCompare.assert_browser_match!(fixture,
        max_changed_ratio: 0.01,
        max_average_delta: 0.01
      )

    assert stats.page_count == 2
  end

  test "original spanning row and group fragment destinations retain clipped source coordinates" do
    fixture =
      Paths.fixture("html_to_pdf/browser_parity/html_table_span_fragment_targets.html")

    stats =
      PdfVisualCompare.assert_browser_match!(fixture,
        max_changed_ratio: 0.01,
        max_average_delta: 0.01
      )

    assert stats.page_count == 2
  end
end
