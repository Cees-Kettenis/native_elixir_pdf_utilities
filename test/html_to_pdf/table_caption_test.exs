defmodule NativeElixirPdfUtilities.HtmlToPdf.TableCaptionTest do
  use ExUnit.Case, async: true

  alias NativeElixirPdfUtilities.HtmlToPdf
  alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, Layout, Style}

  test "caption block flow uses ordinary block geometry while preserving caption grouping" do
    content =
      "Before<div style='padding:3pt;border:1pt solid black'><p style='margin:0 0 4pt'>Block one</p><p style='margin:0'>Block two</p></div>After"

    caption_style = "padding:2pt;border:.75pt solid black;width:140pt;height:60pt;text-align:left"

    assert {:ok, dom} =
             HtmlParser.parse(
               "<table style='width:180pt;border-collapse:collapse'><caption style='#{caption_style}'>#{content}</caption><tr><td>Cell</td></tr></table>"
             )

    assert {:ok, styled} = Style.compute_detailed(dom)
    [table] = styled.children
    [caption, _row] = table.children
    assert {:ok, %{boxes: boxes}} = Layout.layout(styled)
    caption_boxes = Enum.filter(boxes, &(Map.get(&1, :table_caption_side) == :top))
    [table_id] = caption_boxes |> Enum.map(& &1.table_id) |> Enum.uniq()
    assert match?({:table, _, _}, table_id)
    assert Enum.any?(caption_boxes, &(Map.get(&1, :flow_id) == {:table_caption, table_id}))
    refute Enum.any?(caption_boxes, &(Map.get(&1, :text) == "Cell"))
    assert Enum.any?(boxes, &(Map.get(&1, :text) == "Cell" and &1.table_id == table_id))

    assert Enum.any?(
             caption_boxes,
             &(&1.type == :rect and &1.width == 145.5 and &1.height == 65.5)
           )

    assert Enum.map(Enum.filter(caption_boxes, &(&1.type == :text)), & &1.text) == [
             "Before",
             "Block one",
             "Block two",
             "After"
           ]

    refute Enum.any?(caption_boxes, fn box ->
             Map.get(box, :flow_id) == {:table_caption, table_id} and
               Map.has_key?(box, :fragment_id)
           end)

    assert Enum.any?(
             Enum.filter(caption_boxes, &(&1.type == :text)),
             &Map.has_key?(&1, :fragment_id)
           )

    assert {:ok, %{boxes: ordinary}} =
             Layout.layout(%{
               type: :document,
               children: [%{caption | style: Map.put(caption.style, :display, :block)}]
             })

    for {actual, expected} <- Enum.zip(caption_boxes, ordinary) do
      for key <- [
            :type,
            :x,
            :y,
            :width,
            :height,
            :text,
            :color,
            :fill_color,
            :stroke_color,
            :fragment_id,
            :fragment_lines
          ] do
        assert Map.get(actual, key) == Map.get(expected, key)
      end
    end

    assert length(caption_boxes) == length(ordinary)

    assert {:ok, pdf} =
             HtmlToPdf.render(
               "<table><caption>#{content}</caption><tr><td>Body</td></tr></table>"
             )

    assert {:ok, "Before Block one Block two After Body"} =
             NativeElixirPdfUtilities.Text.extract(pdf, layout: false)
  end

  test "empty and zero-height captions retain authored box geometry and visible overflowing content" do
    assert {:ok, dom} =
             HtmlParser.parse(
               "<table style='width:180pt;border-collapse:collapse'><caption style='height:0;padding:0;margin:0'>Overflow</caption><tr><td style='padding:0'>Body</td></tr></table>"
             )

    assert {:ok, styled} = Style.compute_detailed(dom)
    assert {:ok, %{boxes: boxes}} = Layout.layout(styled)
    overflow = Enum.find(boxes, &(&1.type == :text and &1.text == "Overflow"))
    body = Enum.find(boxes, &(&1.type == :text and &1.text == "Body"))
    assert overflow.y == body.y

    assert {:ok, _pdf} =
             HtmlToPdf.render(
               "<table><caption style='height:12pt;background:red'></caption><tr><td>After empty caption</td></tr></table>"
             )
  end

  test "caption table descendants fail through validators with actionable public diagnostics" do
    html =
      "<table><caption><div><table><tr><td>Nested</td></tr></table></div></caption><tr><td>Outer</td></tr></table>"

    assert {:error, {:unsupported_html, diagnostic}} = HtmlToPdf.render(html)
    assert diagnostic.reason == :unsupported_html and diagnostic.stage == :html
    assert diagnostic.source != "" and diagnostic.operation == :render
    nested = %{type: :element, tag: "table", attributes: %{}, children: []}

    caption = %{
      type: :element,
      tag: "caption",
      attributes: %{},
      children: [%{type: :element, tag: "div", attributes: %{}, children: [nested]}]
    }

    assert {:error, {:invalid_document, diagnostic}} =
             Style.compute_detailed(%{type: :document, children: [caption]})

    assert diagnostic.source == "caption" and diagnostic.stage == :style
    assert diagnostic.message =~ "move the nested table outside"
  end
end
