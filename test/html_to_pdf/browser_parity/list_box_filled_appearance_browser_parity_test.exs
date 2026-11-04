defmodule NativeElixirPdfUtilities.HtmlToPdf.ListBoxFilledAppearanceBrowserParityTest do
  use ExUnit.Case, async: true
  alias NativeElixirPdfUtilities.{Forms, HtmlToPdf}
  alias NativeElixirPdfUtilities.TestSupport.{Paths, PdfVisualCompare}
  @moduletag :browser_parity

  test "filled and flattened source list boxes match Chromium's visible labels highlights and scrolling" do
    stats =
      PdfVisualCompare.assert_browser_match!(
        Paths.fixture("html_to_pdf/browser_parity/list_box_filled_appearances.html"),
        artifact_dir: "tmp/browser_parity/list_box_filled_appearances",
        render_opts: [forms: :interactive],
        native_renderer: fn html, opts ->
          initial =
            html
            |> String.replace(" selected", "")
            |> String.replace("<option value=a>", "<option value=a selected>")

          with {:ok, pdf} <- HtmlToPdf.render(initial, opts),
               {:ok, filled} <-
                 Forms.fill(
                   pdf,
                   %{
                     "changed" => "c",
                     "cleared" => [],
                     "multiple" => ["a", "c"],
                     "scrolled" => "e",
                     "empty" => "c"
                   },
                   appearance: :source
                 ),
               {:ok, flat} <- Forms.flatten(filled),
               do: {:ok, flat}
        end,
        max_changed_ratio: 0.01,
        max_average_delta: 0.01
      )

    assert stats.scripting == :disabled and stats.page_count == 1
  end
end
