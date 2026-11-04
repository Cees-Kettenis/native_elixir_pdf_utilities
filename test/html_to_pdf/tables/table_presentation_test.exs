defmodule NativeElixirPdfUtilities.HtmlToPdf.TablePresentationTest do
  use ExUnit.Case, async: true

  alias NativeElixirPdfUtilities.HtmlToPdf
  alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, Layout, Style}
  alias NativeElixirPdfUtilities.Validators.HtmlTableValidator

  test "legacy dimensions colors spacing and cell padding enter the cascade below authored CSS" do
    html = """
    <style>.override{width:120pt;height:30pt;border-spacing:3pt;background:#abc}.cell{width:40pt;height:12pt;padding:2pt;background:white;text-align:left;vertical-align:top;white-space:normal}</style>
    <table width='240' height='80' bgcolor='#eef' cellspacing='4' cellpadding='3'>
    <colgroup><col width='40%'></colgroup><tbody bgcolor='#def' align='RIGHT' valign='bottom'><tr height='32' bgcolor='#fde'>
    <th id='heading' scope='col' width='60' height='16' bgcolor='chucknorris' nowrap>Heading</th><td headers='heading' class='cell' width='120' height='32' bgcolor='red' align='center' valign='bottom' nowrap>Data</td>
    </tr></tbody></table><table class='override' width='240' height='80' bgcolor='red' cellspacing='8' cellpadding='9'><tr><td class='cell'>Override</td></tr></table>
    """

    assert {:ok, dom} = HtmlParser.parse_detailed(html)
    assert {:ok, styled} = Style.compute_detailed(dom)
    [table, override] = styled.children
    [colgroup, group] = table.children
    [col] = colgroup.children
    [row] = group.children
    [heading, data] = row.children
    assert table.style.width == 180.0 and table.style.height == 60.0
    assert table.style.border_spacing == {3.0, 3.0}
    assert table.style.background_color == {238 / 255, 238 / 255, 1}
    assert col.style.width == {:percent, 0.4}
    assert group.style.text_align == :right and group.style.vertical_align == :bottom
    assert row.style.height == 24.0 and row.style.vertical_align == :bottom
    assert heading.style.width == 45.0 and heading.style.height == 12.0
    assert heading.style.padding == %{top: 2.25, right: 2.25, bottom: 2.25, left: 2.25}
    assert heading.style.background_color == {192 / 255, 0, 0}
    assert heading.style.white_space == :nowrap and heading.style.text_align == :right
    assert heading.style.vertical_align == :bottom
    assert data.style._table_cell.headers == [heading.style._table_cell.source]
    assert data.style.width == 40.0 and data.style.height == 12.0
    assert data.style.padding == %{top: 2.0, right: 2.0, bottom: 2.0, left: 2.0}
    assert data.style.background_color == {1, 1, 1}
    assert data.style.text_align == :left and data.style.vertical_align == :top
    assert data.style.white_space == :normal
    assert override.style.width == 120.0 and override.style.height == 30.0
    assert override.style.border_spacing == {3.0, 3.0}
    assert override.style.background_color == {170 / 255, 187 / 255, 204 / 255}
    assert {:ok, _pdf} = HtmlToPdf.render(html)
  end

  test "zero and invalid legacy dimensions preserve the specified HTML fallback behavior" do
    assert {:ok, hints} =
             HtmlTableValidator.presentation_hints(
               %{tag: "table", attributes: %{"width" => "0", "height" => "0"}},
               [],
               %{}
             )

    assert hints == %{height: 0.0}

    assert {:ok, %{vertical_align: :middle}} =
             HtmlTableValidator.presentation_hints(
               %{
                 tag: "td",
                 attributes: %{"width" => "0", "height" => "0", "bgcolor" => "transparent"}
               },
               [],
               %{}
             )

    assert {:ok, hints} =
             HtmlTableValidator.presentation_hints(
               %{tag: "table", attributes: %{"cellspacing" => "0"}},
               [],
               %{}
             )

    assert hints == %{border_spacing: {0.0, 0.0}}

    assert {:ok, %{}} =
             HtmlTableValidator.presentation_hints(
               %{tag: "table", attributes: %{"width" => "invalid", "cellspacing" => "-1"}},
               [],
               %{}
             )

    assert {:ok, %{}} =
             HtmlTableValidator.presentation_hints(
               %{namespace: :svg, tag: "table", attributes: %{"width" => "120"}},
               [],
               %{}
             )
  end

  test "nearest table owns cell padding and nested tables do not borrow an outer hint" do
    outer = %{tag: "table", attributes: %{"cellpadding" => "8"}}
    inner = %{tag: "table", attributes: %{}}
    cell = %{tag: "td", attributes: %{}}
    assert {:ok, %{padding: padding}} = HtmlTableValidator.presentation_hints(cell, [outer], %{})
    assert padding.top == 6.0
    assert {:ok, hints} = HtmlTableValidator.presentation_hints(cell, [inner, outer], %{})
    refute Map.has_key?(hints, :padding)
  end

  test "table auto margins center definite tables and author margins override align hints" do
    for {extra, expected_x} <- [{"", 60.0}, {"style='margin-left:9pt;margin-right:0'", 9.0}] do
      html =
        "<table width=160 align=center bgcolor=red cellspacing=0 cellpadding=0 #{extra}><tr height=16><td>Cell</td></tr></table>"

      assert {:ok, dom} = HtmlParser.parse_detailed(html)
      assert {:ok, styled} = Style.compute_detailed(dom)
      assert {:ok, %{boxes: boxes}} = Layout.layout(styled, page_size: {240, 240}, margin: 0)
      background = Enum.find(boxes, &(&1.type == :rect and &1.fill_color == {1, 0, 0}))
      assert background.x == expected_x and background.width == 120.0
    end
  end

  test "row and group backgrounds clip separated gaps while keeping colspan paint continuous" do
    html =
      "<table style='width:100pt;border-spacing:6pt;table-layout:fixed'><colgroup><col style='width:50%'><col style='width:50%'></colgroup><tbody style='background:blue'><tr style='background:red'><td colspan=2 style='height:12pt;padding:0'></td></tr><tr><td style='height:12pt;padding:0'></td><td></td></tr></tbody></table>"

    assert {:ok, dom} = HtmlParser.parse_detailed(html)
    assert {:ok, styled} = Style.compute_detailed(dom)
    assert {:ok, %{boxes: boxes}} = Layout.layout(styled, page_size: {240, 240}, margin: 0)
    row = Enum.find(boxes, &(Map.get(&1, :role) == :table_row_background))
    assert row.fill_color == {1, 0, 0} and row.clip.x == 6.0 and row.clip.width == 88.0
    groups = Enum.filter(boxes, &(Map.get(&1, :role) == :table_row_group_background))
    assert length(groups) == 3
    assert Enum.map(groups, & &1.clip.width) == [88.0, 41.0, 41.0]
    assert Enum.all?(groups, &(&1.fill_color == {0, 0, 1}))
  end

  test "group height is a minimum distributed across its rows before table sizing" do
    html =
      "<table style='width:100pt;border-spacing:0;background:red'><tbody height=64><tr height=16><td style='padding:0'></td></tr><tr height=16><td style='padding:0'></td></tr></tbody></table>"

    assert {:ok, dom} = HtmlParser.parse_detailed(html)
    assert {:ok, styled} = Style.compute_detailed(dom)
    assert {:ok, %{boxes: [background]}} = Layout.layout(styled, page_size: {240, 240}, margin: 0)
    assert background.height == 48.0
  end

  test "unavailable floating table alignment and oversized hints use shared actionable diagnostics" do
    for align <- ["LEFT", "right"] do
      assert {:error, {:unsupported_html, diagnostic}} =
               HtmlToPdf.render("<table align=#{align}><tr><td>Cell</td></tr></table>")

      assert diagnostic.stage == :style and diagnostic.reason == :unsupported_html
      assert diagnostic.message =~ "float layout" and diagnostic.source =~ "table align="
      assert diagnostic.module == HtmlTableValidator
      assert diagnostic.line == 1 and diagnostic.column == 1
    end

    assert {:error, {:unsupported_html, diagnostic}} =
             HtmlToPdf.render("<table><tr><td align=justify>Justified</td></tr></table>")

    assert diagnostic.stage == :style and diagnostic.message =~ "text justification"
    assert diagnostic.source == "td align=justify" and diagnostic.line == 1

    assert {:ok, _pdf} =
             HtmlToPdf.render(
               "<table><tr><td align=justify style='text-align:left'>CSS override</td></tr></table>"
             )

    # Retain the original admitted input and prove its actual PDF placement.
    assert {:ok, pdf} =
             HtmlToPdf.render(
               "<div style='display:flex'><table width=160 align=center><tr><td>Cell</td></tr></table></div>"
             )

    assert {:ok, %{page_count: 1, pages: [%{media_box: [0, 0, 595.28, 841.89], spans: [cell]}]}} =
             NativeElixirPdfUtilities.Text.extract_spans(pdf)

    assert cell.text == "Cell" and cell.paints_text?
    assert_in_delta cell.x, 240.0, 0.0001
    assert_in_delta cell.y, 13.4999, 0.0001
    assert cell.end_x > cell.x and cell.render_mode == 0

    for {attribute, value} <- [
          {"cellspacing", "999999999999999999999999999"},
          {"width", "999999999999999999999999999"}
        ] do
      assert {:error, {:resource_limit_exceeded, diagnostic}} =
               HtmlToPdf.render("<table #{attribute}=#{value}><tr><td>Cell</td></tr></table>")

      assert diagnostic.stage == :style and diagnostic.reason == :resource_limit_exceeded
      assert diagnostic.source == attribute <> "=" <> value
      assert diagnostic.message =~ "max_css_numeric_magnitude"
    end
  end
end
