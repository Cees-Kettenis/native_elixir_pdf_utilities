defmodule NativeElixirPdfUtilities.HtmlToPdf.NumericCharacterReferenceBrowserParityTest do
  use ExUnit.Case, async: true

  alias NativeElixirPdfUtilities.TestSupport.{Paths, PdfVisualCompare}
  @moduletag :browser_parity

  test "large numeric references paint replacement and zero-prefixed values like Chromium" do
    directory = Paths.project("tmp/browser_parity/numeric_character_reference")
    File.mkdir_p!(directory)
    fixture = Path.join(directory, "numeric_character_reference.html")

    source =
      """
      <!doctype html><html><head><meta charset="utf-8"><style>
      @page {size:240pt 80pt;margin:0}
      body {margin:12pt;font-size:18pt;font-family:'DejaVu Sans'}
      </style></head><body>
      """ <>
        "&#" <>
        String.duplicate("9", 1_800_000) <>
        "; &#x" <>
        String.duplicate("0", 4_096) <> "41; &#128;</body></html>"

    File.write!(fixture, source)

    stats =
      PdfVisualCompare.assert_browser_match!(fixture,
        artifact_dir: Path.join(directory, "comparison"),
        render_opts: [system_font_discovery: false, page_size: {240, 80}, margin: 0],
        max_changed_ratio: 0.01,
        max_average_delta: 0.01
      )

    assert stats.page_count == 1
  end
end
