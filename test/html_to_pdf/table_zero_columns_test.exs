defmodule NativeElixirPdfUtilities.HtmlToPdf.TableZeroColumnsTest do
  use ExUnit.Case, async: true

  alias NativeElixirPdfUtilities.HtmlToPdf
  alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, Layout, Style}
  alias NativeElixirPdfUtilities.Validators.HtmlTableValidator

  test "empty visible rows retain their height without creating columns or spacing" do
    for {collapse, expected_height} <- [{"collapse", 30.0}, {"separate", 31.5}] do
      html =
        "<table style='width:100pt;background:red;border:.75pt solid black;border-collapse:#{collapse};border-spacing:9pt 6pt'><tr style='height:18pt'></tr><tr style='height:12pt'></tr></table><div style='height:10pt;background:blue'></div>"

      assert {:ok, dom} = HtmlParser.parse_detailed(html)
      assert {:ok, styled} = Style.compute_detailed(dom)
      [table, _following] = styled.children
      assert {table.style._table_model.width, table.style._table_model.height} == {0, 2}
      assert table.style._table_model.cells == []

      assert {:ok, %{rows: [first, second], columns: []}} =
               HtmlTableValidator.form(table.children)

      assert first.cells == [] and second.cells == []

      assert {:ok, %{boxes: [background, following, space]}} =
               Layout.layout(styled, page_size: {240, 240}, margin: 0)

      assert_flow_space(space, following)
      assert_in_delta background.height, expected_height, 0.0001
      assert background.width == 100.0
      assert_in_delta following.y + following.height, 240 - expected_height, 0.0001
      assert {:ok, _pdf} = HtmlToPdf.render(html)
    end
  end

  test "grouped rows and rows with hidden cells retain visible row sizing independently of the logical model" do
    html = """
    <table style='width:100pt;background:red;border:3pt solid black;padding:3pt;border-collapse:collapse'>
      <thead style='background:green'><tr style='height:12pt;background:blue;border:3pt solid'></tr></thead>
      <tbody style='background:green'><tr style='height:9pt'><td style='display:none' id='hidden'>Hidden</td></tr><tr style='display:none;height:99pt'><td>Excluded</td></tr></tbody>
      <tfoot style='background:green'><tr style='height:6pt'></tr></tfoot>
    </table><div style='height:10pt;background:blue'></div>
    """

    assert {:ok, dom} = HtmlParser.parse_detailed(html)
    assert {:ok, styled} = Style.compute_detailed(dom)
    [table, _following] = styled.children
    model = table.style._table_model
    assert {model.width, model.height} == {1, 4}
    assert Enum.any?(model.cells, &(&1.id == "hidden"))
    assert {:ok, %{rows: rows, columns: []}} = HtmlTableValidator.form(table.children)
    assert length(rows) == 3 and Enum.all?(rows, &(&1.cells == []))

    assert {:ok, %{boxes: [background, following, space]}} =
             Layout.layout(styled, page_size: {240, 240}, margin: 0)

    assert_flow_space(space, following)
    assert background.height == 27.0 and background.fill_color == {1, 0, 0}
    assert background.stroke_width == 0.0
    assert following.y + following.height == 213.0
    assert {:ok, _pdf} = HtmlToPdf.render(html)
  end

  test "zero-column table height follows ordinary row minimum and declared table height sizing" do
    for collapse <- ["collapse", "separate"],
        {height, expected} <- [{"auto", 18.0}, {"10pt", 18.0}, {"48pt", 48.0}] do
      html =
        "<table style='width:100pt;height:#{height};background:red;border:0;border-spacing:12pt;border-collapse:#{collapse}'><tr style='height:6pt'></tr><tr style='height:12pt'></tr></table><p style='margin:0'>After</p>"

      reference =
        "<div style='width:100pt;height:#{expected}pt;background:red'></div><p style='margin:0'>After</p>"

      assert {:ok, dom} = HtmlParser.parse(html)
      assert {:ok, styled} = Style.compute_detailed(dom)
      assert {:ok, %{boxes: boxes}} = Layout.layout(styled, page_size: {240, 240}, margin: 0)
      assert {:ok, reference_dom} = HtmlParser.parse(reference)
      assert {:ok, reference_styled} = Style.compute_detailed(reference_dom)

      assert {:ok, %{boxes: reference_boxes}} =
               Layout.layout(reference_styled, page_size: {240, 240}, margin: 0)

      background = Enum.find(boxes, &(&1.type == :rect))
      assert background.height == expected

      assert Enum.find(boxes, &(&1.type == :text)).y ==
               Enum.find(reference_boxes, &(&1.type == :text)).y
    end
  end

  test "zero-height rows and tables without rows retain separate decorations and omit collapsed decorations" do
    for {rows, row_height} <- [
          {"", 0.0},
          {"<tr></tr><tr></tr>", 0.0},
          {"<tr style='height:12pt'></tr>", 12.0}
        ],
        collapse <- ["separate", "collapse"] do
      html =
        "<table style='width:100pt;background:red;border:3pt solid black;padding:3pt;border-spacing:9pt;border-collapse:#{collapse}'>#{rows}</table><div style='height:10pt;background:blue'></div>"

      assert {:ok, dom} = HtmlParser.parse(html)
      assert {:ok, styled} = Style.compute_detailed(dom)
      assert {:ok, %{boxes: boxes}} = Layout.layout(styled, page_size: {240, 240}, margin: 0)
      following = Enum.find(Enum.reverse(boxes), &(Map.get(&1, :type) == :rect))
      expected = if collapse == "separate", do: row_height + 12, else: row_height
      assert following.y + following.height == 240 - expected

      cond do
        collapse == "collapse" and expected == 0 ->
          assert [^following, space] = boxes
          assert_flow_space(space, following)

        collapse == "collapse" ->
          assert hd(boxes).stroke_width == 0.0

        true ->
          assert hd(boxes).stroke_width == 3.0
      end

      assert {:ok, _pdf} = HtmlToPdf.render(html)
    end
  end

  test "table default border box and authored sizing include decorations in definite heights" do
    for {sizing, expected_height, expected_sizing} <- [
          {"", 36.0, :border_box},
          {"box-sizing:border-box;", 36.0, :border_box},
          {"box-sizing:content-box;", 48.0, :content_box}
        ],
        rows <- [
          "",
          "<tr style='height:12pt'></tr>",
          "<tr><td style='height:12pt;padding:0;border:0'></td></tr>"
        ] do
      html =
        "<table style='#{sizing}width:100pt;height:36pt;padding:3pt;border:3pt solid black;background:red;border-spacing:6pt'>#{rows}</table><div style='height:10pt;background:blue'></div>"

      assert {:ok, dom} = HtmlParser.parse_detailed(html)
      assert {:ok, styled} = Style.compute_detailed(dom)
      [table, _following] = styled.children
      assert table.style.box_sizing == expected_sizing
      assert table.style._table_model.width == if(String.contains?(rows, "<td"), do: 1, else: 0)

      assert {:ok, %{boxes: [background, following, space]}} =
               Layout.layout(styled, page_size: {240, 240}, margin: 0)

      assert_flow_space(space, following)
      assert background.height == expected_height
      assert background.width == if(expected_sizing == :content_box, do: 112.0, else: 100.0)
      assert following.y + following.height == 240 - expected_height
    end

    assert {:ok, dom} =
             HtmlParser.parse_detailed(
               "<table style='width:100pt;height:10pt;padding:3pt;border:3pt solid black;background:red'><tr style='height:18pt'></tr></table>"
             )

    assert {:ok, styled} = Style.compute_detailed(dom)
    assert {:ok, %{boxes: [background]}} = Layout.layout(styled, page_size: {240, 240}, margin: 0)
    assert background.height == 30.0
  end

  test "caption flow and linked content precede the zero-column row height" do
    html =
      "<table style='width:100pt;background:red;border:3pt solid;border-collapse:collapse'><caption style='height:12pt;padding:0;text-align:left'><a href='https://example.com/empty-table'>Caption</a></caption><tr style='height:9pt'></tr></table><div style='height:10pt;background:blue'></div>"

    assert {:ok, dom} = HtmlParser.parse(html)
    assert {:ok, styled} = Style.compute_detailed(dom)
    assert {:ok, %{boxes: boxes}} = Layout.layout(styled, page_size: {240, 240}, margin: 0)

    caption =
      Enum.find(boxes, &(&1.type == :text and match?({:table_caption, _}, Map.get(&1, :flow_id))))

    background = Enum.find(boxes, &(&1.type == :rect and &1.fill_color == {1, 0, 0}))
    assert caption.text == "Caption" and caption.link_url == "https://example.com/empty-table"
    assert {background.y, background.height} == {219.0, 9.0}
    following = Enum.find(Enum.reverse(boxes), &(Map.get(&1, :type) == :rect))
    assert following.y + following.height == 219.0
    assert {:ok, _pdf} = HtmlToPdf.render(html)
  end

  defp assert_flow_space(space, following) do
    assert %{type: :flow_marker, role: :explicit_block_space} = space
    assert space.width == 0.0
    assert space.y == following.y and space.height == following.height
    assert following.fieldset_paint_id in space.fieldset_ancestors
    assert match?({:explicit_block_space, reference} when is_reference(reference), space.flow_id)
  end
end
