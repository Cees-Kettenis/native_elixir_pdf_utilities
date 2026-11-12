defmodule NativeElixirPdfUtilities.HtmlToPdf.StaticItemStackingTest do
  use ExUnit.Case, async: true
  alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, Layout, Pagination, Style}
  alias NativeElixirPdfUtilities.Validators.CssStackingValidator

  test "integer z-index orders ordinary grid and flex items while auto preserves source order" do
    for display <- ["grid", "flex"],
        {first, second, expected} <- [
          {2, 1, [:blue, :red]},
          {-1, 0, [:red, :blue]},
          {0, 0, [:red, :blue]},
          {:auto, :auto, [:red, :blue]}
        ] do
      overlap = if display == "flex", do: "margin-left:-50pt", else: "grid-column:1;grid-row:1"

      pages =
        pages("""
        <div style='display:#{display};width:100pt;grid-template-columns:100pt;grid-template-rows:50pt'>
          <div style='z-index:#{first};flex-shrink:0;grid-column:1;grid-row:1;width:50pt;height:50pt;background:red'></div>
          <div style='z-index:#{second};flex-shrink:0;#{overlap};width:50pt;height:50pt;background:blue'></div>
        </div><div style='height:10pt;background:lime'></div>
        """)

      assert Enum.filter(colors(pages), &(&1 in [:red, :blue])) == expected
      green = hd(pages).boxes |> Enum.find(&(Map.get(&1, :fill_color) == {0, 1, 0}))
      assert_in_delta green.y, 240, 0.001
    end
  end

  test "integer static item contexts isolate relative descendants while auto lets them escape" do
    for display <- ["grid", "flex"], parent <- [0, :auto] do
      source = """
      <div style='display:#{display};width:100pt;grid-template-columns:100pt'>
        <div style='z-index:#{parent};width:100pt;height:50pt;background:lime'>
          <div style='position:relative;z-index:100;width:50pt;height:50pt;background:red'></div>
        </div>
      </div><div style='position:relative;top:-50pt;z-index:1;width:50pt;height:50pt;background:blue'></div>
      """

      assert colors(pages(source)) ==
               if(parent == 0, do: [:lime, :red, :blue], else: [:lime, :blue, :red])
    end
  end

  test "hoisted absolute descendants retain item contexts without changing their containing block" do
    for display <- ["grid", "flex"] do
      [page] =
        pages("""
        <div style='position:relative;width:150pt;height:100pt'>
          <div style='display:#{display};margin-left:30pt;padding-top:20pt;width:100pt;grid-template-columns:100pt'>
            <div style='z-index:0;width:100pt;height:50pt;background:lime'>
              <div style='position:absolute;left:0;top:0;z-index:100;width:50pt;height:50pt;background:red'></div>
            </div>
          </div><div style='position:absolute;left:0;top:0;z-index:1;width:50pt;height:50pt;background:blue'></div>
        </div>
        """)

      assert colors([page]) == [:lime, :red, :blue]
      red = Enum.find(page.boxes, &(Map.get(&1, :fill_color) == {1, 0, 0}))
      assert_in_delta red.x, 0, 0.001
      assert_in_delta red.y + red.height, 300, 0.001
    end
  end

  test "nested static item contexts retain every ancestor around an escaping absolute child" do
    [page] =
      pages("""
      <div style='display:grid;grid-template-columns:100pt'>
        <div style='z-index:0;display:flex;height:50pt;background:lime'>
          <div style='z-index:10;width:100pt;height:50pt'>
            <div style='position:absolute;left:0;top:0;z-index:100;width:50pt;height:50pt;background:red'></div>
          </div>
        </div>
      </div><div style='position:absolute;left:0;top:0;z-index:1;width:50pt;height:50pt;background:blue'></div>
      """)

    assert colors([page]) == [:lime, :red, :blue]
    assert {:ok, [^page]} = Pagination.finalize([page], %{}, [])
  end

  test "internally fixed descendants retain static item ancestry when hoisted" do
    fixed = %{
      type: :element,
      tag: "div",
      style: %{display: :block, position: :fixed, z_index: 100},
      children: []
    }

    item = %{type: :element, tag: "div", style: %{display: :block, z_index: 0}, children: [fixed]}
    grid = %{type: :element, tag: "div", style: %{display: :grid}, children: [item]}
    assert {[_grid], [fixed]} = Layout.collect_positioned_children([grid])
    assert fixed.style._stacking_contexts == [{[0, 0], 0}]
  end

  test "advanced trees reject malformed retained stacking ancestry with actionable diagnostics" do
    assert CssStackingValidator.valid_context_path?([{[0, 1], 3}])
    assert {:ok, dom} = HtmlParser.parse("<div></div>")
    assert {:ok, %{children: [child]} = styled} = Style.compute(dom)

    for contexts <- [
          :bad,
          [:bad],
          [{:bad, 0}],
          [{[0], :auto}],
          List.duplicate({[0], 0}, NativeElixirPdfUtilities.Limits.get(:max_depth) + 1)
        ] do
      bad_child = put_in(child.style[:_stacking_contexts], contexts)

      assert {:error, {:invalid_layout, diagnostic}} =
               Layout.layout(%{styled | children: [bad_child]})

      assert diagnostic.stage == :layout
      assert diagnostic.source == "CSS stacking"
    end

    valid_child = put_in(child.style[:_stacking_contexts], [{[0], 0}])
    assert {:ok, _} = Layout.layout(%{styled | children: [valid_child]})
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
    assert {:ok, dom} = HtmlParser.parse(source)
    assert {:ok, styled} = Style.compute_detailed(dom, system_font_discovery: false)
    assert {:ok, layout} = Layout.layout(styled, page_size: {300, 300}, margin: 0)
    assert {:ok, pages} = Pagination.paginate(layout, page_size: {300, 300}, margin: 0)
    pages
  end
end
