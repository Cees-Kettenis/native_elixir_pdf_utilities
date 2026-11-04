defmodule NativeElixirPdfUtilities.HtmlToPdf.HtmlTextProfileBrowserParityTest do
  use ExUnit.Case, async: true

  alias NativeElixirPdfUtilities.TestSupport.{Paths, PdfVisualCompare}
  alias NativeElixirPdfUtilities.Pdf.Reader

  @moduletag :browser_parity

  test "Unicode separator spacing, repetition and wrapping match Chromium" do
    modes = ~w(normal nowrap pre pre-line pre-wrap break-spaces)

    cases =
      for(mode <- modes, kind <- ~w(LF LS PS NEL), do: "#{mode}-#{kind}") ++
        for(mode <- modes, kind <- ~w(LSLS PSPS SP-LS-SP LSPS), do: "#{mode}-#{kind}") ++
        for(mode <- ~w(normal break-spaces), kind <- ~w(LS PS), do: "narrow-#{mode}-#{kind}")

    # Whole CSS-pixel tracks avoid unrelated fractional grid-origin raster shifts.
    regions =
      for {name, index} <- Enum.with_index(cases) do
        {name, {12 + rem(index, 4) * 99, 24 + div(index, 4) * 51, 96, 36}}
      end

    stats =
      PdfVisualCompare.assert_browser_match!(
        Paths.fixture("html_to_pdf/browser_parity/css_unicode_separators.html"),
        max_changed_ratio: 0.01,
        max_average_delta: 0.01,
        regions: regions
      )

    assert stats.page_count == 1
  end

  test "accepted static container nesting matches Chromium and its PDF remains readable" do
    stats =
      PdfVisualCompare.assert_browser_match!(
        Paths.fixture("html_to_pdf/browser_parity/html_container_nesting.html"),
        max_changed_ratio: 0.01,
        max_average_delta: 0.01
      )

    assert stats.page_count == 1

    chromium_pdf = File.read!(Path.join(stats.artifact_dir, "chromium.pdf"))
    assert {:ok, document} = Reader.read(chromium_pdf)
    assert length(document.pages) == 1
  end
end
