defmodule NativeElixirPdfUtilities.HtmlToPdf.ExplicitBlockSpaceBrowserParityTest do
  use ExUnit.Case, async: true
  alias NativeElixirPdfUtilities.TestSupport.{Paths, PdfVisualCompare}
  alias NativeElixirPdfUtilities.Text
  @moduletag :browser_parity

  for {fixture, minimum_y, maximum_y} <- [
        {"explicit_hidden_block_space", 6, 12},
        {"explicit_visible_text_space", 6, 12},
        {"explicit_hidden_padded_space", 13.5, 19.5}
      ] do
    @fixture fixture
    @minimum_y minimum_y
    @maximum_y maximum_y
    test "authored empty height and following text match Chromium for #{@fixture}" do
      artifact_dir = Paths.project("tmp/browser_parity/#{@fixture}")

      stats =
        PdfVisualCompare.assert_browser_match!(
          Paths.fixture("html_to_pdf/browser_parity/#{@fixture}.html"),
          artifact_dir: artifact_dir,
          max_changed_ratio: 0.001,
          max_average_delta: 0.001
        )

      assert stats.page_count == 5

      assert {:ok, %{pages: pages}} =
               Text.extract_spans(File.read!(Path.join(artifact_dir, "native.pdf")))

      assert Enum.map_join(List.last(pages).spans, & &1.text) == "Following"
      y = hd(List.last(pages).spans).y
      assert y >= @minimum_y and y <= @maximum_y
    end
  end
end
