defmodule NativeElixirPdfUtilities.HtmlToPdf.CssTransformBrowserParityTest do
  use ExUnit.Case, async: true
  alias NativeElixirPdfUtilities.TestSupport.PdfVisualCompare
  @moduletag :browser_parity
  @moduletag :css_affine_styles
  @fixtures_dir Path.expand("../fixtures/html_to_pdf/browser_parity", __DIR__)
  for name <-
        ~w(html_css_affine_boxes html_css_affine_nested html_css_affine_fragments html_css_affine_fragment_origins html_css_affine_table) do
    @fixture name
    test "original authored CSS affine source #{name}" do
      stats =
        PdfVisualCompare.assert_browser_match!(Path.join(@fixtures_dir, @fixture <> ".html"),
          artifact_dir: Path.join("tmp/browser_parity/css-affine-second", @fixture),
          max_changed_ratio: 0.01,
          max_average_delta: 0.01
        )

      assert stats.page_count ==
               if(@fixture in ["html_css_affine_fragments", "html_css_affine_fragment_origins"],
                 do: 2,
                 else: 1
               )

      assert {:ok, []} =
               NativeElixirPdfUtilities.Forms.fields(
                 File.read!(Path.join(stats.artifact_dir, "native.pdf"))
               )
    end
  end
end
