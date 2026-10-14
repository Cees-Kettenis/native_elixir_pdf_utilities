defmodule NativeElixirPdfUtilities.HtmlToPdf.RubyDecorationBrowserTest do
  use ExUnit.Case
  alias NativeElixirPdfUtilities.TestSupport.PdfVisualCompare
  @moduletag :browser_parity
  test "Ruby decorations propagate to bases and preserve annotation-owned scopes" do
    fixture =
      Path.expand("../fixtures/html_to_pdf/browser_parity/ruby_decoration_scopes.html", __DIR__)

    stats =
      PdfVisualCompare.assert_browser_match!(fixture,
        artifact_dir: ".quality/orchestration/ruby-decoration-work/browser",
        render_opts: [system_font_discovery: false],
        max_changed_ratio: 0.01,
        max_average_delta: 0.01
      )

    assert stats.page_count == 1
  end
end
