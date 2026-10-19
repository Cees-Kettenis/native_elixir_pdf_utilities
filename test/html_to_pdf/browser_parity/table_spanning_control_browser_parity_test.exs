alias NativeElixirPdfUtilities.TestSupport.Paths

defmodule NativeElixirPdfUtilities.HtmlToPdf.TableSpanningControlBrowserParityTest do
  use ExUnit.Case, async: true
  alias NativeElixirPdfUtilities.TestSupport.PdfVisualCompare
  @moduletag :browser_parity
  @moduletag :table_spanning_controls

  test "spanning controls retain their initial appearance after a whole atomic page cut" do
    fixture =
      Paths.fixture("html_to_pdf/browser_parity/html_table_spanning_controls.html")

    stats =
      PdfVisualCompare.assert_browser_match!(fixture,
        max_changed_ratio: 0.01,
        max_average_delta: 0.01
      )

    assert stats.page_count == 2
  end

  test "unpaid spanning control source continues after measured rows without losing the final lines" do
    fixture =
      Paths.fixture("html_to_pdf/browser_parity/html_table_spanning_controls_long.html")

    stats =
      PdfVisualCompare.assert_browser_match!(fixture,
        max_changed_ratio: 0.01,
        max_average_delta: 0.01
      )

    assert stats.page_count == 3

    assert {:ok, %{pages: [_, second, third]}} =
             NativeElixirPdfUtilities.Text.extract_spans(
               File.read!(Path.join(stats.artifact_dir, "native.pdf"))
             )

    refute Enum.any?(second.spans, &(&1.text in ["Line19", "Line20"]))

    assert Enum.filter(third.spans, &(&1.text in ["Line19", "Line20"])) |> Enum.map(& &1.text) ==
             ["Line19", "Line20"]

    assert {:ok, text} =
             NativeElixirPdfUtilities.Text.extract(
               File.read!(Path.join(stats.artifact_dir, "native.pdf")),
               layout: false
             )

    for index <- 1..20 do
      assert length(Regex.scan(Regex.compile!("Line#{index}(?![0-9])"), text)) == 1
    end
  end
end
