defmodule NativeElixirPdfUtilities.HtmlToPdf.StackingContextGeometryTest do
  use ExUnit.Case, async: true
  alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, Style, Layout, Pagination}
  alias NativeElixirPdfUtilities.Validators.CssStackingValidator

  # CSS Positioned Layout 3 (2025-10-07), and CSS Containment 2 geometry.
  test "isolation separates paint ownership from absolute and fixed containing blocks" do
    for {style, expected} <- [
          {%{}, {false, false, false}},
          {%{position: :relative}, {false, true, false}},
          {%{position: :absolute, z_index: 1}, {true, true, false}},
          {%{position: :sticky}, {true, true, false}},
          {%{position: :fixed}, {true, true, false}},
          {%{isolation: :isolate}, {true, false, false}},
          {%{contain: [:layout]}, {true, true, true}},
          {%{contain: [:paint]}, {true, true, true}},
          {%{_stacking_item: true, z_index: -1}, {true, false, false}}
        ] do
      assert CssStackingValidator.geometry(style) == expected
      assert CssStackingValidator.context?(style) == elem(expected, 0)
      assert CssStackingValidator.containing_block?(style, :absolute) == elem(expected, 1)
      assert CssStackingValidator.containing_block?(style, :fixed) == elem(expected, 2)
    end

    for {property, value, key, expected} <- [
          {"isolation", "auto", :isolation, :auto},
          {"isolation", "isolate", :isolation, :isolate},
          {"contain", "none", :contain, []},
          {"contain", "paint layout", :contain, [:layout, :paint]}
        ] do
      assert {:ok, ^key, ^expected} = CssStackingValidator.prepare(property, value)
    end

    for {property, value} <- [
          {"isolation", "42"},
          {"isolation", "unknown"},
          {"contain", "layout layout"},
          {"contain", "size"},
          {"unknown", "auto"}
        ] do
      assert :error = CssStackingValidator.prepare(property, value)
    end
  end

  test "isolated descendants cannot paint above a higher sibling context" do
    for {isolation, last} <- [{"isolate", {0, 0, 1}}, {"auto", {1, 0, 0}}] do
      html =
        "<div style='isolation:#{isolation};width:60pt;height:40pt;background:green'><div style='position:absolute;z-index:999;width:20pt;height:20pt;top:0;left:0;background:red'></div></div><div style='position:absolute;z-index:1;top:0;left:0;width:20pt;height:20pt;background:blue'></div>"

      assert {:ok, dom} = HtmlParser.parse_detailed(html)
      assert {:ok, tree} = Style.compute_detailed(dom, system_font_discovery: false)
      assert {:ok, result} = Layout.layout(tree, page_size: {200, 200}, margin: 0)
      assert {:ok, [page]} = Pagination.paginate(result)

      colors =
        Enum.flat_map(page.boxes, fn box ->
          case Map.get(box, :fill_color) do
            nil -> []
            color -> [color]
          end
        end)

      assert List.last(colors) == last
    end
  end
end
