defmodule NativeElixirPdfUtilities.HtmlToPdf.AdvancedLayoutBrowserParityTest do
  use ExUnit.Case, async: true

  alias NativeElixirPdfUtilities.TestSupport.{Paths, PdfVisualCompare}

  @moduletag :browser_parity

  test "anchor fallback and finite draft artwork match one Chromium sheet" do
    stats =
      PdfVisualCompare.assert_browser_match!(
        Paths.fixture("html_to_pdf/browser_parity/css_advanced_layout.html"),
        chromium_transform: &reference_layout/1,
        render_opts: [fonts: []],
        max_changed_ratio: 0.001,
        max_average_delta: 0.001
      )

    assert stats.page_count == 1
  end

  # The Grid 3 placement algorithm at csswg-drafts commit 2da1cb17a433 gives lane starts 0, 56 and 56 CSS
  # pixels, with stack starts 0, 0 and 24px. A beveled 12px corner connects
  # the two adjacent edge endpoints. Those scenes share the actual anchor test
  # capture so the oracle does not launch a browser per draft.
  defp reference_layout(document) do
    String.replace(document, "</style>", """
    .packing { display:block }
    .packing > div {position:absolute; width:48px}
    .packing .a {left:0; top:0}
    .packing .b {left:56px; top:0}
    .packing .c {left:56px; top:24px}
    .corner {border-radius:0; clip-path:polygon(12px 0,44px 0,56px 12px,56px 20px,
      44px 32px,12px 32px,0 20px,0 12px)}
    </style>
    """)
  end
end
