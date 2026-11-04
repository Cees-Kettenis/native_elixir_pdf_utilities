defmodule NativeElixirPdfUtilities.HtmlToPdf.HtmlSubSupTest do
  use ExUnit.Case, async: true

  alias NativeElixirPdfUtilities.{HtmlToPdf, Text}
  alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, Layout, Pagination, Style}
  alias NativeElixirPdfUtilities.Validators.HtmlValidator

  @css "<style>html,body{margin:0}body{font-family:'DejaVu Sans';font-size:12pt;line-height:normal}p{margin:0}</style>"

  test "subscript has smaller text below the surrounding baseline" do
    [page] = layout_pages("<p>H<sub>2</sub>O</p>")
    [hydrogen, subscript, oxygen] = Enum.filter(page.boxes, &(&1.type == :text))
    assert hydrogen.text == "H" and subscript.text == "2" and oxygen.text == "O"
    assert subscript.font_size == 10.0
    assert hydrogen.y == oxygen.y
    assert_in_delta hydrogen.y - subscript.y, 3.140625, 0.00001
    assert subscript.x > hydrogen.x and oxygen.x > subscript.x
  end

  test "superscript has smaller text above the surrounding baseline" do
    [page] = layout_pages("<p>m<sup>2</sup>Z</p>")
    [metres, superscript, after_text] = Enum.filter(page.boxes, &(&1.type == :text))
    assert metres.text == "m" and superscript.text == "2" and after_text.text == "Z"
    assert superscript.font_size == 10.0
    assert metres.y == after_text.y
    assert_in_delta superscript.y - metres.y, 4.74609375, 0.00001
  end

  test "nested scripts accumulate shifts and ordinary descendants retain their ancestor baseline" do
    [page] = layout_pages("<p>X<sup>A<span>B</span><sup>C</sup><sub>D</sub></sup>Y</p>")

    [base, outer_a, outer_b, inner_sup, inner_sub, after_text] =
      Enum.filter(page.boxes, &(&1.type == :text))

    assert base.y == after_text.y
    assert outer_a.y == outer_b.y
    assert outer_a.y > base.y and inner_sup.y > outer_a.y
    assert inner_sub.y < outer_a.y
    assert_in_delta inner_sup.font_size, 12 / 1.2 / 1.2, 0.00001
    assert_in_delta inner_sub.font_size, inner_sup.font_size, 0.00001
  end

  test "author CSS overrides default sizing and alignment without shifting following text" do
    [page] =
      layout_pages(
        "<p>X<sub style='font-size:inherit;vertical-align:baseline'>A</sub>" <>
          "<sup style='font-size:18pt;vertical-align:-2pt'>B</sup>Y</p>"
      )

    [base, flat, lowered, after_text] = Enum.filter(page.boxes, &(&1.type == :text))
    assert flat.font_size == 12.0 and flat.y == base.y
    assert lowered.font_size == 18.0
    assert_in_delta base.y - lowered.y, 2.0, 0.00001
    assert after_text.y == base.y
  end

  test "script keywords validate through the shared source and computed CSS grammar" do
    for {value, expected} <- [{" SUB ", :sub}, {"SuPeR", :super}] do
      assert HtmlValidator.parse_vertical_align(value, :source) == {:ok, expected}
      assert HtmlValidator.parse_vertical_align(value, 12) == {:ok, expected}
      [page] = layout_pages("<p>X<span style='vertical-align:#{value}'>A</span>Y</p>")
      [base, shifted, _after] = Enum.filter(page.boxes, &(&1.type == :text))
      assert shifted.font_size == base.font_size
      assert if(expected == :sub, do: shifted.y < base.y, else: shifted.y > base.y)
    end
  end

  test "zero line height keeps script ink while allowing authors to control line spacing" do
    [normal] = layout_pages("<p>X<sup>A</sup><sub>B</sub><br>Next</p>")

    [compact] =
      layout_pages(
        "<p>X<sup style='line-height:0'>A</sup><sub style='line-height:0'>B</sub><br>Next</p>"
      )

    normal_text = Enum.filter(normal.boxes, &(&1.type == :text))
    compact_text = Enum.filter(compact.boxes, &(&1.type == :text))
    assert Enum.map(normal_text, & &1.text) == Enum.map(compact_text, & &1.text)
    assert hd(normal_text).line_box_height > hd(compact_text).line_box_height
    assert Enum.at(compact_text, 1).y > hd(compact_text).y
    assert Enum.at(compact_text, 2).y < hd(compact_text).y
  end

  test "scripts wrap and paginate with their line extents and remain extractable PDF text" do
    html =
      "<div style='width:100pt'>" <>
        String.duplicate("Alpha<sub>low</sub> Beta<sup>high</sup> Gamma Delta ", 12) <>
        "</div>"

    pages = layout_pages(html, page_size: {144, 108})
    assert length(pages) > 2
    boxes = Enum.flat_map(pages, & &1.boxes)
    assert Enum.count(boxes, &(Map.get(&1, :text) == "low")) == 12
    assert Enum.count(boxes, &(Map.get(&1, :text) == "high")) == 12

    for page <- pages, box <- page.boxes, box.type == :text do
      assert box.y > 0 and box.y < 108
      assert box.line_box_height > 0
    end

    assert {:ok, pdf} = HtmlToPdf.render(@css <> html, system_font_discovery: false)
    assert {:ok, text} = Text.extract(pdf)
    assert text =~ "Alpha" and text =~ "low" and text =~ "high"
  end

  test "atomic script contents reserve the shifted height before the following line" do
    box = "<span style='display:inline-block;width:12pt;height:18pt;background:blue'></span>"
    [ordinary] = layout_pages("<p>X#{box}Y</p><p>Next</p>")
    [raised] = layout_pages("<p>X<sup>#{box}</sup>Y</p><p>Next</p>")
    normal_text = Enum.filter(ordinary.boxes, &(&1.type == :text))
    raised_text = Enum.filter(raised.boxes, &(&1.type == :text))

    assert_in_delta hd(raised_text).line_box_height - hd(normal_text).line_box_height,
                    4.74609375,
                    0.00001

    assert List.last(raised_text).y < List.last(normal_text).y

    assert {:ok, _pdf} =
             HtmlToPdf.render(@css <> "<p>X<sup>#{box}</sup>Y</p>", system_font_discovery: false)
  end

  test "an inline script keeps its own font strut around atomic and nested script contents" do
    [wrapped] =
      layout_pages(
        "<p>X<sup><span style='display:inline-block;width:12pt;height:18pt'></span></sup>Y<sub><span style='display:inline-block;width:10pt;height:8pt'></span></sub>Z</p>"
      )

    [atomic] =
      layout_pages(
        "<p>X<sup style='display:inline-block;width:12pt;height:18pt'></sup>Y<sub style='display:inline-block;width:10pt;height:8pt'></sub>Z</p>"
      )

    wrapped_text = Enum.filter(wrapped.boxes, &(&1.type == :text))
    atomic_text = Enum.filter(atomic.boxes, &(&1.type == :text))

    assert_in_delta hd(wrapped_text).line_box_height - hd(atomic_text).line_box_height,
                    2.25,
                    0.00001

    [nested] = layout_pages("<p>X<sup><sub>A</sub></sup>Y</p>")
    [base, script, after_text] = Enum.filter(nested.boxes, &(&1.type == :text))
    assert base.y == after_text.y
    assert script.y > base.y
    assert_in_delta script.font_size, 12 / 1.2 / 1.2, 0.00001
    assert_in_delta base.line_baseline_depth, 13.74609375, 0.00001
  end

  test "script alignment follows author-selected inline blocks and image baselines" do
    png =
      NativeElixirPdfUtilities.TestSupport.PngFixture.build(2, 2, 2, 8, 0, fn _, _ ->
        [0, 0, 255]
      end)

    source = "data:image/png;base64," <> Base.encode64(png)

    for {tag, shift} <- [{"sub", -3.140625}, {"sup", 4.74609375}] do
      [block_page] =
        layout_pages(
          "<p>X<#{tag} style='display:inline-block;width:12pt;height:18pt;background:blue'></#{tag}>Y</p>"
        )

      [base, after_text] = Enum.filter(block_page.boxes, &(&1.type == :text))
      block = Enum.find(block_page.boxes, &(Map.get(&1, :fill_color) == {0.0, 0.0, 1.0}))
      assert_in_delta block.y - base.y, shift, 0.00001
      assert base.y == after_text.y

      alignment = if tag == "sub", do: "sub", else: "super"

      [image_page] =
        layout_pages(
          "<p>X<img src='#{source}' width=8 height=16 style='vertical-align:#{alignment}'>Y</p>"
        )

      [base, after_text] = Enum.filter(image_page.boxes, &(&1.type == :text))
      image = Enum.find(image_page.boxes, &(&1.type == :image))
      assert_in_delta image.y - base.y, shift, 0.00001
      assert base.y == after_text.y
    end
  end

  test "line-edge alignment preserves nested script shifts and inline-box geometry" do
    [page] =
      layout_pages(
        "<p>X<span style='vertical-align:top'><span style='display:inline-block;width:10pt;height:11.25pt;background:blue'></span><sup style='font-size:8pt;vertical-align:1.5pt'>A</sup></span>Y</p>"
      )

    [base, raised, after_text] = Enum.filter(page.boxes, &(&1.type == :text))
    assert base.y == after_text.y
    assert_in_delta raised.y - base.y, 1.5, 0.00001
    box = Enum.find(page.boxes, &(Map.get(&1, :fill_color) == {0.0, 0.0, 1.0}))
    assert_in_delta box.y, base.y, 0.00001

    for html <- [
          "<p>X<sup><span style='vertical-align:top;font-size:8pt'>A</span></sup>Y</p>",
          "<p>X<span style='vertical-align:top'><span style='vertical-align:bottom;font-size:8pt'>A</span></span>Y</p>",
          "<p>X<sup><span style='font-size:24pt'>A</span></sup>Y</p>"
        ] do
      [page] = layout_pages(html)
      [base, script, after_text] = Enum.filter(page.boxes, &(&1.type == :text))
      assert base.y == after_text.y
      assert script.text == "A"
      assert script.y != base.y
    end
  end

  test "empty scripts retain their font struts in occupied lines without creating an empty flow line" do
    [page] = layout_pages("<p>Before</p><p>X<sub></sub>Y<sup></sup>Z</p><p>After</p>")
    text = Enum.filter(page.boxes, &(&1.type == :text))
    middle = Enum.find(text, &(&1.text == "X"))
    assert_in_delta middle.line_box_height, 19.13671875, 0.00001
    assert_in_delta middle.line_baseline_depth, 13.74609375, 0.00001
    assert Enum.map_join(text, & &1.text) == "BeforeXYZAfter"

    [empty] = layout_pages("<sup></sup><div>Next</div>")
    [ordinary] = layout_pages("<div>Next</div>")
    [empty_text] = Enum.filter(empty.boxes, &(&1.type == :text))
    [ordinary_text] = Enum.filter(ordinary.boxes, &(&1.type == :text))
    assert empty_text.y == ordinary_text.y

    [spaced] = layout_pages("<p>Words <sup></sup> between<sub></sub> words</p>")
    words = Enum.filter(spaced.boxes, &(&1.type == :text))
    assert Enum.map_join(words, & &1.text) == "Words between words"
  end

  test "empty and hidden scripts do not paint text or leak their size into siblings" do
    [page] = layout_pages("<p>A<sub></sub><sup hidden>Hidden</sup>B</p>")
    text = Enum.filter(page.boxes, &(&1.type == :text))
    assert Enum.map_join(text, & &1.text) == "AB"
    assert Enum.all?(text, &(&1.font_size == 12.0))
  end

  defp layout_pages(html, opts \\ []) do
    assert {:ok, dom} = HtmlParser.parse_document_detailed(@css <> html)
    assert {:ok, styled} = Style.compute_detailed(dom, system_font_discovery: false)

    assert {:ok, layout} =
             Layout.layout(styled, Keyword.merge([page_size: {240, 180}, margin: 12], opts))

    assert {:ok, pages} = Pagination.paginate(layout)
    pages
  end
end
