alias NativeElixirPdfUtilities.TestSupport.Paths

defmodule NativeElixirPdfUtilities.HtmlToPdf.RubyDecorationBrowserParityTest do
  use ExUnit.Case
  alias NativeElixirPdfUtilities.TestSupport.PdfVisualCompare
  @moduletag :browser_parity
  test "Ruby decorations propagate to bases and preserve annotation-owned scopes" do
    fixture =
      Paths.fixture("html_to_pdf/browser_parity/ruby_decoration_scopes.html")

    stats =
      PdfVisualCompare.assert_browser_match!(fixture,
        artifact_dir: "tmp/test_artifacts/ruby_decoration_browser_parity/browser",
        render_opts: [system_font_discovery: false],
        max_changed_ratio: 0.01,
        max_average_delta: 0.01
      )

    assert stats.page_count == 1
  end
end
