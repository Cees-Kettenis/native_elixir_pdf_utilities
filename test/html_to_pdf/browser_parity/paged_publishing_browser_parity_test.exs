defmodule NativeElixirPdfUtilities.HtmlToPdf.PagedPublishingBrowserParityTest do
  use ExUnit.Case, async: true
  @moduletag :browser_parity
  alias NativeElixirPdfUtilities.TestSupport.{Paths, PdfVisualCompare}

  # Page-dependent documents stay isolated: composing them into a single sheet
  # changes page selection, assignments and note allocation. Local generated
  # counters/markers reuse generated_content_counters.html's existing capture.
  test "named page geometry and margin counters match Chromium" do
    stats =
      PdfVisualCompare.assert_browser_match!(
        Paths.fixture("html_to_pdf/browser_parity/publishing_pages.html"),
        # The ordinary helper supplies one explicit API page size. This case
        # deliberately exercises CSS mixed geometry without an API override.
        native_renderer: fn html, opts ->
          NativeElixirPdfUtilities.HtmlToPdf.render(
            html,
            Keyword.drop(opts, [:page_size, :margin])
          )
        end,
        artifact_dir: "tmp/browser_parity/publishing_pages",
        max_changed_ratio: 0.01,
        max_average_delta: 0.01
      )

    assert stats.page_count == 2
  end

  for {name, count} <- [{"running", 2}, {"notes", 1}, {"references", 2}] do
    @name name
    @count count
    test "publishing #{@name} matches independently positioned reference artwork" do
      # CSS Page WD 2023-09-14 §5 (margin-area centering) and GCPM WD
      # 2024-01-25 §§1–2 (source assignments and footnote placement). Chromium
      # lacks strings/running elements/footnotes, so the authored reference uses
      # explicit 240×180pt sheets, 30pt insets and 12pt line boxes. No native
      # output or renderer measurement was used to build the reference.
      # The reference row uses 29 glyphs at 10pt from the bundled monospace
      # face's 1233/2048 em advance. The target is on the second sheet.
      root = "html_to_pdf/browser_parity/publishing_#{@name}"

      stats =
        PdfVisualCompare.assert_browser_match!(Paths.fixture(root <> ".html"),
          chromium_transform: fn _html -> File.read!(Paths.fixture(root <> "_reference.html")) end,
          artifact_dir: "tmp/browser_parity/publishing_#{@name}",
          max_changed_ratio: 0.01,
          max_average_delta: 0.01
        )

      assert stats.page_count == @count
    end
  end
end
