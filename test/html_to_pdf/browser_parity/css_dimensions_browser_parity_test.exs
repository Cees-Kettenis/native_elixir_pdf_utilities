alias NativeElixirPdfUtilities.TestSupport.Paths

defmodule NativeElixirPdfUtilities.HtmlToPdf.CssDimensionsBrowserParityTest do
  use ExUnit.Case, async: true
  alias NativeElixirPdfUtilities.TestSupport.PdfVisualCompare
  @moduletag :browser_parity
  @moduletag :css_computed_lengths

  test "calculated box dimensions and ordinary inline padding match the original Chromium source" do
    fixture =
      Paths.fixture("html_to_pdf/browser_parity/css_calculated_dimensions_inline_padding.html")

    stats =
      PdfVisualCompare.assert_browser_match!(fixture,
        artifact_dir: "tmp/browser_parity/css_calculated_dimensions_inline_padding",
        max_changed_ratio: 0.01,
        max_average_delta: 0.01,
        regions: [
          {"selected metrics", {10, 158, 50, 25}},
          {"typed attributes and percentage edges", {104, 158, 65, 20}},
          {"intrinsic arithmetic", {210, 158, 50, 20}},
          {"computed text metrics", {104, 188, 50, 24}},
          {"allocated percentage basis", {10, 188, 85, 24}}
        ],
        region_thresholds:
          Map.new(
            [
              "selected metrics",
              "typed attributes and percentage edges",
              "intrinsic arithmetic",
              "computed text metrics",
              "allocated percentage basis"
            ],
            &{&1, %{max_changed_ratio: 0.03, max_average_delta: 0.02}}
          )
      )

    assert stats.page_count == 1
    assert stats.scripting == :disabled

    for engine <- ["native", "chromium"] do
      assert {:ok, text} =
               NativeElixirPdfUtilities.Text.extract_file(
                 Path.join(stats.artifact_dir, engine <> ".pdf"),
                 layout: false
               )

      for label <- ["Units", "Attr", "Size", "Text", "Basis"],
          do: assert(String.replace(text, ~r/\s/u, "") =~ String.replace(label, ~r/\s/u, ""))
    end
  end
end
