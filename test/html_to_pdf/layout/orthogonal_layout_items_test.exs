defmodule NativeElixirPdfUtilities.HtmlToPdf.OrthogonalLayoutItemsTest do
  use ExUnit.Case, async: true
  alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, Layout, Pagination, PdfWriter, Style}

  # CSS Writing Modes 3 (10 December 2019), orthogonal flow sizing and
  # CSS Grid 2 (26 March 2025), subgrid axis mapping.
  test "orthogonal flex and grid items paint in their own writing plane" do
    for display <- [
          "flex;align-items:baseline",
          "flex;flex-direction:column",
          "grid;grid-template-columns:40pt"
        ] do
      tree =
        layout(
          "<div style='display:#{display};width:100pt'><div style='writing-mode:vertical-rl;width:40pt;height:50pt;background:red'>Vertical</div></div>"
        )

      [group] = Enum.filter(tree.boxes, &(Map.get(&1, :role) == :writing_mode))
      assert_in_delta group.writing_extent.width, 40, 0.02
      assert_in_delta group.writing_extent.height, 50, 0.02
      assert Enum.any?(group.boxes, &(Map.get(&1, :text) == "Vertical"))
      assert {:ok, pages} = Pagination.paginate(tree)
      assert {:ok, _} = PdfWriter.render(pages)
    end
  end

  test "intrinsic orthogonal flex and grid items keep a finite size without a preferred inline size" do
    for display <- ["flex", "flex;flex-direction:column", "grid"] do
      tree =
        layout(
          "<div style='display:#{display};width:100pt'><div style='writing-mode:vertical-lr'>Word</div></div>"
        )

      [group] = Enum.filter(tree.boxes, &(Map.get(&1, :role) == :writing_mode))
      assert group.writing_extent.width > 0 and group.writing_extent.width <= 100
      assert group.writing_extent.height > 0 and group.writing_extent.height < 100
    end
  end

  test "orthogonal subgrid axes inherit the corresponding parent tracks and names" do
    tree =
      layout(
        "<div style='display:grid;width:40pt;grid-template-columns:40pt;grid-template-rows:[a] 20pt [b] 30pt [c]'><div style='grid-row:1 / 3;writing-mode:vertical-rl;display:grid;grid-template-columns:subgrid;grid-template-rows:subgrid'><div style='grid-column:a / b;background:red'></div><div style='grid-column:b / c;background:blue'></div></div></div>"
      )

    [group] = Enum.filter(tree.boxes, &(Map.get(&1, :role) == :writing_mode))
    red = Enum.find(group.boxes, &(Map.get(&1, :fill_color) == {1, 0, 0}))
    blue = Enum.find(group.boxes, &(Map.get(&1, :fill_color) == {0, 0, 1}))
    assert_in_delta red.width, 20, 0.02
    assert_in_delta blue.width, 30, 0.02
    assert_in_delta red.height, 40, 0.02
    assert_in_delta blue.height, 40, 0.02
    assert_in_delta blue.x - red.x, 20, 0.02
  end

  test "reversed orthogonal subgrid lines retain physical parent track boundaries" do
    tree =
      layout(
        "<div style='display:grid;width:40pt;grid-template-columns:40pt;grid-template-rows:20pt 30pt'><div style='grid-row:1 / 3;writing-mode:vertical-rl;direction:rtl;display:grid;grid-template-columns:subgrid;grid-template-rows:subgrid'><div style='background:red'></div><div style='background:blue'></div></div></div>"
      )

    [group] = Enum.filter(tree.boxes, &(Map.get(&1, :role) == :writing_mode))
    red = Enum.find(group.boxes, &(Map.get(&1, :fill_color) == {1, 0, 0}))
    blue = Enum.find(group.boxes, &(Map.get(&1, :fill_color) == {0, 0, 1}))
    assert_in_delta red.width, 30, 0.02
    assert_in_delta blue.width, 20, 0.02
    assert_in_delta red.x - blue.x, 20, 0.02
  end

  test "orthogonal descendants contribute physical extents to automatic parent tracks" do
    tree =
      layout(
        "<div style='display:grid;width:100pt;justify-content:start;grid-template-columns:max-content;grid-template-rows:auto auto'><div style='grid-row:1 / 3;writing-mode:vertical-rl;display:grid;grid-template-columns:subgrid;grid-template-rows:subgrid'><div style='width:40pt;height:20pt;background:red'></div><div style='width:40pt;height:20pt;background:blue'></div></div></div>"
      )

    [group] = Enum.filter(tree.boxes, &(Map.get(&1, :role) == :writing_mode))
    assert_in_delta group.writing_extent.width, 40, 0.02
    assert_in_delta group.writing_extent.height, 40, 0.02
  end

  test "horizontal subgrids inside a vertical parent preserve both parent axes" do
    for mode <- ["vertical-rl", "vertical-lr"] do
      tree =
        layout(
          "<div style='writing-mode:#{mode};display:grid;width:50pt;height:40pt;grid-template-columns:20pt 20pt;grid-template-rows:50pt'><div style='writing-mode:horizontal-tb;display:grid;grid-column:1 / 3;grid-template-columns:subgrid;grid-template-rows:subgrid'><div style='background:red'></div><div style='background:blue'></div></div></div>"
        )

      [outer] = Enum.filter(tree.boxes, &(Map.get(&1, :role) == :writing_mode))
      [inner] = Enum.filter(outer.boxes, &(Map.get(&1, :role) == :writing_mode))
      rects = Enum.filter(inner.boxes, &(Map.get(&1, :fill_color) in [{1, 0, 0}, {0, 0, 1}]))
      assert length(rects) == 2
      assert Enum.all?(rects, &(abs(&1.width - 50) < 0.02 and abs(&1.height - 20) < 0.02))
    end
  end

  test "orthogonal item margins leave the shared interior subgrid line fixed" do
    tree =
      layout(
        "<div style='display:grid;width:40pt;grid-template-columns:40pt;grid-template-rows:20pt 30pt'><div style='grid-row:1 / 3;writing-mode:vertical-rl;display:grid;grid-template-columns:subgrid;grid-template-rows:subgrid;margin:2pt'><div style='background:red'></div><div style='background:blue'></div></div></div>"
      )

    [group] = Enum.filter(tree.boxes, &(Map.get(&1, :role) == :writing_mode))
    red = Enum.find(group.boxes, &(Map.get(&1, :fill_color) == {1, 0, 0}))
    blue = Enum.find(group.boxes, &(Map.get(&1, :fill_color) == {0, 0, 1}))
    assert_in_delta red.width, 18, 0.02
    assert_in_delta blue.width, 28, 0.02
    assert_in_delta red.height, 36, 0.02
  end

  test "orthogonal flex and grid item roots suppress floats and retain internal links" do
    for display <- ["flex", "grid;grid-template-columns:40pt"] do
      tree =
        layout(
          "<div style='display:#{display};width:100pt'><div id='rotated' style='float:left;writing-mode:vertical-rl;width:40pt;height:50pt'>Rotated</div></div><a href='#rotated'>Jump</a>"
        )

      assert {:ok, pages} = Pagination.paginate(tree)
      assert {:ok, _} = PdfWriter.render(pages)

      links =
        Enum.flat_map(pages, & &1.boxes) |> Enum.filter(&Map.has_key?(&1, :link_destination))

      assert [%{link_destination: {:xyz, 1, _, _}}] = links
    end
  end

  defp layout(html) do
    {:ok, dom} = HtmlParser.parse("<div>" <> html <> "</div>")
    {:ok, styled} = Style.compute(dom, system_font_discovery: false)
    {:ok, tree} = Layout.layout(styled, page_size: {300, 300}, margin: 0)
    tree
  end
end
