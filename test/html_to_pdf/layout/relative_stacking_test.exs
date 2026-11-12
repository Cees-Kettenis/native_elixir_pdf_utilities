defmodule NativeElixirPdfUtilities.HtmlToPdf.RelativeStackingTest do
  use ExUnit.Case, async: true
  alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, Layout, Pagination, Style}

  test "relative siblings paint by z-index and source order without moving flow" do
    for {first, second, expected} <- [
          {2, 1, [:blue, :red]},
          {1, 2, [:red, :blue]},
          {2, 2, [:red, :blue]},
          {:auto, :auto, [:red, :blue]}
        ] do
      pages =
        pages("""
        <div style='position:relative;z-index:#{first};width:50pt;height:50pt;background:red'></div>
        <div style='position:relative;z-index:#{second};top:-50pt;width:50pt;height:50pt;background:blue'></div>
        <div style='height:10pt;background:lime'></div>
        """)

      assert colors(pages) == [:lime | expected]
      green = hd(pages).boxes |> Enum.find(&(Map.get(&1, :fill_color) == {0, 1, 0}))
      assert_in_delta green.y, 190, 0.001
    end
  end

  test "relative and absolute siblings share one source-ordered stacking context" do
    for position <- ["absolute", "relative"], indices <- [{2, 1}, {1, 2}, {0, 0}] do
      {first, second} = indices

      source = """
      <div style='position:relative;height:100pt'>
        <div style='position:#{position};z-index:#{first};top:0;left:0;width:50pt;height:50pt;background:red'></div>
        <div style='position:relative;z-index:#{second};width:50pt;height:50pt;background:blue'></div>
      </div>
      """

      assert colors(pages(source)) ==
               if(indices == {2, 1}, do: [:blue, :red], else: [:red, :blue])
    end
  end

  test "nested contexts isolate children while auto ancestors let them participate outside" do
    for parent <- [0, :auto] do
      source = """
      <div style='position:relative;z-index:#{parent};height:50pt;background:lime'>
        <div style='position:relative;z-index:100;height:50pt;background:red'></div>
      </div>
      <div style='position:relative;z-index:1;top:-50pt;height:50pt;background:blue'></div>
      """

      expected = if parent == 0, do: [:lime, :red, :blue], else: [:lime, :blue, :red]
      assert colors(pages(source)) == expected
    end
  end

  test "negative descendants paint above context backgrounds and below its normal content" do
    for parent <- [0, :auto] do
      source = """
      <div style='position:relative;z-index:#{parent};height:50pt;background:lime'>
        <div style='position:relative;z-index:-1;height:50pt;background:red'></div>
        <div style='height:10pt;background:blue'></div>
      </div>
      """

      assert colors(pages(source)) ==
               if(parent == 0, do: [:lime, :red, :blue], else: [:red, :lime, :blue])
    end
  end

  test "inline relative contexts participate in sibling stacking" do
    source = """
    <div><span style='position:relative;z-index:2;background:red'>AAA</span><span
      style='position:relative;z-index:1;left:-20pt;background:blue'>BBB</span></div>
    """

    assert colors(pages(source)) == [:blue, :red]
  end

  test "transformed contexts isolate positioned descendants" do
    source = """
    <div style='transform:translateX(0);height:50pt;background:lime'>
      <div style='position:relative;z-index:100;height:50pt;background:red'></div>
    </div>
    <div style='position:relative;z-index:1;top:-50pt;height:50pt;background:blue'></div>
    """

    [page] = pages(source)
    group = Enum.find(page.boxes, &(Map.get(&1, :type) == :drawing_group))
    assert colors([%{boxes: group.boxes}]) == [:lime, :red]
    assert colors([page]) == [:lime, :red, :blue]
    assert {:ok, [finalized_again]} = Pagination.finalize([page], %{}, [])
    assert finalized_again == page

    assert Enum.find_index(page.boxes, &(&1 == group)) <
             Enum.find_index(page.boxes, &(Map.get(&1, :fill_color) == {0, 0, 1}))
  end

  test "nested inline backgrounds retain their owning context" do
    source = """
    <div><span style='position:relative;z-index:0;background:lime'>A<span
      style='position:relative;z-index:-1;background:red'>B</span>C</span></div>
    """

    assert colors(pages(source)) == [:lime, :lime, :lime, :red]
  end

  test "stacking never changes page assignment" do
    source = fn z ->
      "<div style='position:relative;z-index:#{z};height:250pt;background:red'></div>" <>
        "<div style='position:relative;height:100pt;background:blue'></div>"
    end

    assert Enum.map(pages(source.(2)), &Enum.sort(colors([&1]))) ==
             Enum.map(pages(source.(:auto)), &Enum.sort(colors([&1])))
  end

  test "advanced layout inputs reject malformed stacking metadata" do
    [page] = pages("<div style='height:10pt;background:red'></div>")
    [box | _] = page.boxes

    for metadata <- [
          %{_stacking_path: :bad},
          %{_stacking_path: [:bad]},
          %{_stacking_path: [{:bad, 0}]},
          %{_stacking_path: [{[-1], 0}]},
          %{_stacking_path: [{[0], :auto}]},
          %{
            _stacking_path:
              List.duplicate({[0], 0}, NativeElixirPdfUtilities.Limits.get(:max_depth) + 1)
          },
          %{_stacking_background_order: :bad},
          %{_stacking_root_background: :bad}
        ] do
      bad = %{page | boxes: [Map.merge(box, metadata)]}
      assert {:error, {:invalid_layout, diagnostic}} = Pagination.finalize([bad], %{}, [])
      assert diagnostic.stage == :pagination
      assert diagnostic.source == "CSS stacking"
      assert diagnostic.message =~ "recompute layout"
    end

    group = %{type: :drawing_group, boxes: [Map.put(box, :_stacking_path, :bad)]}

    assert {:error, {:invalid_layout, _}} =
             Pagination.finalize([%{page | boxes: [group]}], %{}, [])

    assert {:ok, dom} = HtmlParser.parse_detailed("<div>Text</div>")
    assert {:ok, styled} = Style.compute_detailed(dom, system_font_discovery: false)
    [child | rest] = styled.children

    for order <- [
          :bad,
          [-1],
          List.duplicate(0, NativeElixirPdfUtilities.Limits.get(:max_depth) + 1)
        ] do
      node = %{child | style: Map.put(child.style, :_stacking_order, order)}

      assert {:error, {:invalid_layout, diagnostic}} =
               Layout.layout(%{styled | children: [node | rest]})

      assert diagnostic.reason == :invalid_layout
    end
  end

  defp colors(pages) do
    pages
    |> Enum.flat_map(& &1.boxes)
    |> Enum.flat_map(fn box ->
      case box do
        %{boxes: children} -> colors([%{boxes: children}])
        %{fill_color: {1, 0, 0}} -> [:red]
        %{fill_color: {0, 1, 0}} -> [:lime]
        %{fill_color: {0, 0, 1}} -> [:blue]
        _ -> []
      end
    end)
  end

  defp pages(source) do
    source =
      "<style>body{margin:0;font-family:'DejaVu Sans';font-size:12pt;line-height:20pt}</style>" <>
        source

    assert {:ok, dom} = HtmlParser.parse_detailed(source)
    assert {:ok, styled} = Style.compute_detailed(dom, system_font_discovery: false)
    assert {:ok, layout} = Layout.layout(styled, page_size: {300, 300}, margin: 0)
    assert {:ok, pages} = Pagination.paginate(layout, page_size: {300, 300}, margin: 0)
    pages
  end
end
