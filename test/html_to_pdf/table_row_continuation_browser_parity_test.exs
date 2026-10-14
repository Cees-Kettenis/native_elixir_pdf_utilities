defmodule NativeElixirPdfUtilities.HtmlToPdf.TableRowContinuationBrowserParityTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.TestSupport.PdfVisualCompare
  @moduletag :browser_parity
  @moduletag :table_row_continuation
  @fixture Path.expand(
             "../fixtures/html_to_pdf/browser_parity/html_table_row_whole_line_continuation.html",
             __DIR__
           )

  test "whole ordinary row lines consume remaining page capacity and repeat real headers" do
    stats =
      PdfVisualCompare.assert_browser_match!(@fixture,
        max_changed_ratio: 0.01,
        max_average_delta: 0.01
      )

    assert stats.page_count == 3
  end

  test "spanning text keeps every whole source line across the actual page clip" do
    fixture =
      Path.expand(
        "../fixtures/html_to_pdf/browser_parity/html_table_span_whole_line_continuation.html",
        __DIR__
      )

    stats =
      PdfVisualCompare.assert_browser_match!(fixture,
        max_changed_ratio: 0.01,
        max_average_delta: 0.01
      )

    assert stats.page_count == 2
  end

  test "a zero-height authored target follows its real deferred source line" do
    fixture =
      Path.expand(
        "../fixtures/html_to_pdf/browser_parity/html_table_zero_target_continuation.html",
        __DIR__
      )

    stats =
      PdfVisualCompare.assert_browser_match!(fixture,
        max_changed_ratio: 0.01,
        max_average_delta: 0.01
      )

    assert stats.page_count == 2
  end
end
