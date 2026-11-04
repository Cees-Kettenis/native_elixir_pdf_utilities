defmodule NativeElixirPdfUtilities.HtmlToPdf.HtmlSubSupBrowserParityTest do
  use ExUnit.Case, async: true

  alias NativeElixirPdfUtilities.TestSupport.{Paths, PdfVisualCompare}
  @moduletag :browser_parity

  for scenario <- ~w(defaults styles pagination empty) do
    @scenario scenario
    test "HTML subscript and superscript #{@scenario} match Chromium" do
      stats =
        PdfVisualCompare.assert_browser_match!(
          Paths.fixture("html_to_pdf/browser_parity/html_sub_sup_#{@scenario}.html"),
          artifact_dir: "tmp/browser_parity/html_sub_sup/#{@scenario}",
          render_opts: [system_font_discovery: false],
          max_changed_ratio: 0.01,
          max_average_delta: 0.01
        )

      assert stats.page_count >= 1
      assert stats.scripting == :disabled
    end
  end
end
