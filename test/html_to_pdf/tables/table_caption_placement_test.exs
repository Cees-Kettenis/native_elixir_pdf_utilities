defmodule NativeElixirPdfUtilities.HtmlToPdf.TableCaptionPlacementTest do
  use ExUnit.Case, async: true

  alias NativeElixirPdfUtilities.HtmlToPdf
  alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, Layout, Style}

  test "inherited bottom caption side and legacy align retain block flow and CSS precedence" do
    for {table_css, caption_attrs, side} <- [
          {"caption-side:bottom", "", :bottom},
          {"", "align=bottom", :bottom},
          {"caption-side:bottom", "align=bottom style='caption-side:top'", :top},
          {"", "style='caption-side:bottom'", :bottom},
          {"", "align=bottom style='caption-side:inherit'", :top},
          {"caption-side:bottom", "style='caption-side:initial'", :top}
        ],
        collapse <- ["separate", "collapse"] do
      html =
        "<table style='#{table_css};font-size:9pt;line-height:12pt;width:120pt;background:red;border:0;border-spacing:0;border-collapse:#{collapse}'><caption #{caption_attrs}><div style='height:18pt;background:blue'><a href='https://example.com/caption'>Caption</a></div></caption><tr><td style='height:12pt;padding:0'>Body</td></tr></table><div style='height:6pt;background:green'></div>"

      assert {:ok, dom} = HtmlParser.parse_detailed(html)
      assert {:ok, styled} = Style.compute_detailed(dom)
      [table, _] = styled.children
      [caption_node, _] = table.children
      assert caption_node.style.caption_side == side
      assert {:ok, %{boxes: boxes}} = Layout.layout(styled, page_size: {240, 240}, margin: 0)
      table_box = Enum.find(boxes, &(&1.type == :rect and &1.fill_color == {1, 0, 0}))
      caption_box = Enum.find(boxes, &(&1.type == :rect and &1.fill_color == {0, 0, 1}))
      assert table_box.height == 12.0 and caption_box.height == 18.0
      assert table_box.y == if(side == :bottom, do: 228.0, else: 210.0)
      assert caption_box.y == if(side == :bottom, do: 210.0, else: 222.0)
      link = Enum.find(boxes, &(&1.type == :text and &1.text == "Caption"))
      assert link.link_url == "https://example.com/caption"
      assert match?({:table_caption, _}, link.flow_id)
      following = List.last(boxes)
      assert following.y + following.height == 210.0
      assert {:ok, _pdf} = HtmlToPdf.render(html)
    end
  end

  test "empty tables and hidden captions retain their independent paint and flow" do
    for collapse <- ["separate", "collapse"] do
      html =
        "<table style='width:120pt;caption-side:bottom;background:red;border:0;border-collapse:#{collapse}'><caption style='height:12pt;background:blue'></caption></table><div style='height:6pt;background:green'></div>"

      assert {:ok, dom} = HtmlParser.parse_detailed(html)
      assert {:ok, styled} = Style.compute_detailed(dom)

      assert {:ok, %{boxes: [caption, caption_space, following, space]}} =
               Layout.layout(styled, page_size: {240, 240}, margin: 0)

      assert %{type: :flow_marker, role: :explicit_block_space, height: 12.0} = caption_space
      assert caption_space.width == 0.0 and space.width == 0.0
      assert caption_space.y == caption.y
      assert caption.fieldset_paint_id in caption_space.fieldset_ancestors
      assert %{type: :flow_marker, role: :explicit_block_space, height: 6.0} = space
      assert space.y == following.y
      assert following.fieldset_paint_id in space.fieldset_ancestors
      assert caption.fill_color == {0, 0, 1} and caption.height == 12.0
      assert following.y + following.height == 228.0
    end

    assert {:ok, dom} =
             HtmlParser.parse_detailed(
               "<table style='width:120pt;caption-side:bottom'><caption hidden>Hidden caption</caption><tr><td style='height:12pt;padding:0'>Body</td></tr></table>"
             )

    assert {:ok, styled} = Style.compute_detailed(dom)
    assert {:ok, %{boxes: boxes}} = Layout.layout(styled)
    refute Enum.any?(boxes, &match?({:table_caption, _}, Map.get(&1, :flow_id)))
  end

  test "unsupported caption placement and malformed prepared sides return shared diagnostics" do
    assert {:error, {:invalid_css, diagnostic}} =
             HtmlToPdf.render(
               "<table><caption style='caption-side:left'>Caption</caption></table>"
             )

    assert diagnostic.stage == :css and diagnostic.reason == :invalid_css
    assert diagnostic.source =~ "caption-side" and diagnostic.message != ""

    assert {:ok, dom} = HtmlParser.parse_detailed("<table><caption>Caption</caption></table>")
    assert {:ok, styled} = Style.compute_detailed(dom)
    [table] = styled.children
    [caption] = table.children

    malformed = %{
      styled
      | children: [
          %{table | children: [%{caption | style: Map.put(caption.style, :caption_side, :left)}]}
        ]
    }

    assert {:error, {:invalid_layout, diagnostic}} = Layout.layout(malformed)
    assert diagnostic.stage == :layout and diagnostic.reason == :invalid_layout
    assert diagnostic.message != ""
  end
end
