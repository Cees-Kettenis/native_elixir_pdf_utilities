defmodule NativeElixirPdfUtilities.HtmlToPdf.AnonymousTableTest do
  use ExUnit.Case, async: true

  alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, Layout, Style}
  alias NativeElixirPdfUtilities.Validators.HtmlTableValidator

  test "generated rows and cells retain authored boxes and selector identity" do
    html = """
    <style>table{width:120pt}script{display:block}script:nth-child(2){color:red}</style>
    <table><script type='application/json'>First</script><script class=second type='application/json'>Second</script><tr><script type='text/plain'>Third</script><script type='text/plain'>Fourth</script><td>Cell</td></tr></table>
    """

    assert {:ok, dom} = HtmlParser.parse_detailed(html)
    assert {:ok, styled} = Style.compute_detailed(dom, system_font_discovery: false)
    table = Enum.find(styled.children, &match?(%{style: %{display: :table}}, &1))
    assert {:ok, %{rows: [first, second]}} = HtmlTableValidator.form(table.children, table.style)
    assert first.row.anonymous
    assert hd(first.cells).cell.anonymous
    [script1, script2] = hd(first.cells).cell.children
    assert script1.tag == "script" and script2.tag == "script"
    assert script2.style.color == {1.0, 0.0, 0.0}
    assert ["script", "script"] == Enum.map(hd(second.cells).cell.children, & &1.tag)
    refute Map.has_key?(List.last(second.cells).cell, :anonymous)
    assert {:ok, layout} = Layout.layout(styled, page_size: {180, 180}, margin: 0)

    for text <- ~w(First Second Third Fourth Cell) do
      assert Enum.count(layout.boxes, &(Map.get(&1, :text) == text)) == 1
    end
  end

  test "row groups generate rows while column groups discard irrelevant children" do
    html = """
    <table><colgroup><col style='width:60pt'></colgroup><tbody><script type='text/plain' style='display:block'>Visible</script></tbody></table>
    """

    assert {:ok, dom} = HtmlParser.parse_detailed(html)
    assert {:ok, styled} = Style.compute_detailed(dom, system_font_discovery: false)
    [table] = styled.children
    [group, body] = table.children
    [script] = body.children
    group = %{group | children: [script | group.children]}
    table = %{table | children: [group, body]}
    styled = %{styled | children: [table]}

    assert {:ok, %{rows: [row], columns: [%{width: 60.0}]}} =
             HtmlTableValidator.form(table.children, table.style)

    assert row.row.anonymous and hd(row.cells).cell.anonymous
    assert {:ok, layout} = Layout.layout(styled)
    assert Enum.any?(layout.boxes, &(Map.get(&1, :text) == "Visible"))
    assert Enum.count(layout.boxes, &(Map.get(&1, :text) == "Visible")) == 1
  end

  test "anonymous cells retain whitespace between visible non-cell siblings" do
    html =
      "<table><tr><script type='text/plain' style='display:inline'>Left</script> <script type='text/plain' style='display:inline'>Right</script></tr></table>"

    assert {:ok, pdf} =
             NativeElixirPdfUtilities.HtmlToPdf.render(html, system_font_discovery: false)

    assert {:ok, "Left Right"} = NativeElixirPdfUtilities.Text.extract(pdf, layout: false)
  end

  test "formatting formation preserves styled text and suppresses only contextual whitespace" do
    assert {:ok, dom} = HtmlParser.parse_detailed("<table><tr><td>Cell</td></tr></table>")
    assert {:ok, %{children: [table]}} = Style.compute_detailed(dom, system_font_discovery: false)
    [row] = table.children
    [cell] = row.children
    [text] = cell.children
    # Formatting-box formation also accepts styled text supplied by a caller.
    # HTML source parsing retains its separate table content-model rules.
    text = %{text | style: Map.delete(text.style, :display)}
    whitespace = %{text | text: " "}

    assert {:ok, %{rows: [first, second]}} =
             HtmlTableValidator.form([whitespace, row, whitespace, row, whitespace], table.style)

    refute Map.has_key?(first.row, :anonymous)
    refute Map.has_key?(second.row, :anonymous)

    assert {:ok, %{rows: [anonymous, authored]}} =
             HtmlTableValidator.form([text, whitespace, row], table.style)

    assert hd(anonymous.cells).cell.children == [text, whitespace]
    refute Map.has_key?(authored.row, :anonymous)

    assert {:ok, %{rows: [anonymous]}} =
             HtmlTableValidator.form([whitespace, text], table.style)

    assert hd(anonymous.cells).cell.children == [whitespace, text]
  end

  test "standalone columns form a formatting group and ragged slots respect the shared work budget" do
    html =
      "<table style='border-collapse:collapse'><colgroup><col><col></colgroup><tr><td>One</td></tr><tr><td>Two</td><td>Three</td></tr></table>"

    assert {:ok, dom} = HtmlParser.parse_detailed(html)
    assert {:ok, %{children: [table]}} = Style.compute_detailed(dom, system_font_discovery: false)
    [group | rows] = table.children

    assert {:ok, %{columns: [_, _], rows: [first, _]}} =
             HtmlTableValidator.form(group.children ++ rows, table.style)

    assert List.last(first.cells).cell.anonymous
    alias NativeElixirPdfUtilities.Validators.HtmlValidator

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             HtmlValidator.with_render_budget(fn ->
               HtmlValidator.reserve_render_resource(
                 :max_table_grid_work,
                 NativeElixirPdfUtilities.Limits.get(:max_table_grid_work),
                 :layout
               )

               HtmlTableValidator.form(table.children, table.style)
             end)

    assert diagnostic.stage == :layout and diagnostic.reason == :resource_limit_exceeded
    assert diagnostic.message =~ "max_table_grid_work"
  end
end
