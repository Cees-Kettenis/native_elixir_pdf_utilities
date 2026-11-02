defmodule NativeElixirPdfUtilities.HtmlToPdf.AutomaticTablePageOverflowTest do
  use ExUnit.Case, async: true

  alias NativeElixirPdfUtilities.{HtmlToPdf, Text}
  alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, Layout, Style}
  alias NativeElixirPdfUtilities.TestSupport.{Paths, PdfVisualCompare}

  for scenario <- ~w(word_width inline_block_width) do
    @scenario scenario

    test "automatic table #{@scenario} retains content minimums and text size on narrow pages" do
      html =
        File.read!(
          Paths.fixture("html_to_pdf/browser_parity/auto_table_minimum_#{@scenario}.html")
        )

      assert {:ok, dom} = HtmlParser.parse_document_detailed(html)
      assert {:ok, styled} = Style.compute_detailed(dom, system_font_discovery: false)
      assert {:ok, layout} = Layout.layout(styled, page_size: {140, 100}, margin: 10)

      [first_cell, second_cell] =
        Enum.filter(layout.boxes, &(Map.get(&1, :role) == :table_cell_background))

      assert second_cell.x + second_cell.width > 140
      assert second_cell.x >= first_cell.x + first_cell.width

      case @scenario do
        "word_width" ->
          assert_in_delta first_cell.width, 92.818359375, 0.01
          assert_in_delta second_cell.width, first_cell.width, 0.01

        "inline_block_width" ->
          assert_in_delta first_cell.width, 31.095703125, 0.01
          assert_in_delta second_cell.width, 200, 0.01
      end

      assert {:ok, narrow_pdf} =
               HtmlToPdf.render(html,
                 page_size: {140, 100},
                 margin: 10,
                 system_font_discovery: false
               )

      assert {:ok, wide_pdf} =
               HtmlToPdf.render(html,
                 page_size: {500, 100},
                 margin: 10,
                 system_font_discovery: false
               )

      assert {:ok, %{pages: [narrow]}} = Text.extract_spans(narrow_pdf)
      assert {:ok, %{pages: [wide]}} = Text.extract_spans(wide_pdf)
      assert narrow.media_box == [0, 0, 140, 100]
      assert wide.media_box == [0, 0, 500, 100]
      assert narrow.spans == wide.spans

      for span <- narrow.spans do
        [a, b, _c, _d, _x, _y] = span.ctm
        assert_in_delta span.font_size * :math.sqrt(a * a + b * b), 12, 0.0001
      end

      case @scenario do
        "word_width" ->
          assert Enum.any?(narrow.spans, &(&1.end_x > 140))
          assert {:ok, text} = Text.extract(narrow_pdf, layout: false)
          assert text =~ "COLUMN-TWO"

        "inline_block_width" ->
          [auto] = Enum.filter(narrow.spans, &(&1.text == "Auto"))
          fixed = Enum.reject(narrow.spans, &(&1.text == "Auto"))
          assert Enum.map_join(fixed, & &1.text) == "Fixed"
          assert Enum.max_by(fixed, & &1.end_x).end_x <= auto.x
          assert auto.end_x < 140
      end
    end

    @tag :browser_parity
    test "automatic table #{@scenario} matches Chromium on a page wide enough for its contents" do
      stats =
        PdfVisualCompare.assert_browser_match!(
          Paths.fixture("html_to_pdf/browser_parity/auto_table_minimum_#{@scenario}.html"),
          artifact_dir: "tmp/browser_parity/auto_table_minimum_#{@scenario}",
          render_opts: [system_font_discovery: false],
          max_changed_ratio: 0.01,
          max_average_delta: 0.01
        )

      assert stats.page_count == 1
      assert stats.scripting == :disabled
    end
  end
end
