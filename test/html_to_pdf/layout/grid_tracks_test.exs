defmodule NativeElixirPdfUtilities.HtmlToPdf.GridTracksTest do
  use ExUnit.Case, async: true
  alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, Layout, Style}
  alias NativeElixirPdfUtilities.Validators.CssGridValidator

  # CSS Grid 2, 26 March 2025, sections 7 through 12. Track/line
  # expectations are independently calculated in points, including gutters.
  test "named lines, repeated names and negative explicit lines resolve without losing case" do
    for {tracks, placement, x, width} <- [
          {"[Start] 20pt [Middle] 30pt [End]", "Middle / End", 20, 30},
          {"20pt 30pt", "-2 / -1", 20, 30},
          {"repeat(3,[a] 20pt) [end]", "a 2 / a 3", 20, 20},
          {"20pt 30pt", "-4 / -3", 0, 10}
        ] do
      [item] =
        boxes("""
        <div style='display:grid;width:100pt;grid-template-columns:#{tracks};grid-auto-columns:10pt'>
          <div style='grid-column:#{placement};height:10pt;background:red'></div>
        </div>
        """)

      assert_in_delta item.x, x, 0.001
      assert_in_delta item.width, width, 0.001
    end
  end

  test "auto-fill retains empty tracks while auto-fit collapses tracks and their gutters" do
    for {repeat, expected} <- [{"auto-fill", 30}, {"auto-fit", 50}] do
      [first, second] =
        boxes("""
        <div style='display:grid;width:110pt;gap:10pt;grid-template-columns:repeat(#{repeat},minmax(30pt,1fr))'>
          <div style='height:10pt;background:red'></div><div style='height:10pt;background:blue'></div>
        </div>
        """)

      assert_in_delta first.width, expected, 0.001
      assert_in_delta second.x, expected + 10, 0.001
      assert_in_delta second.width, expected, 0.001
    end
  end

  test "dense placement fills earlier holes while sparse flow preserves its cursor" do
    for {flow, x, top} <- [
          {"row", 40, 280},
          {"row dense", 40, 300},
          {"column", 0, 260},
          {"column dense", 0, 260}
        ] do
      items =
        boxes("""
        <div style='display:grid;grid-template-columns:repeat(3,20pt);grid-template-rows:repeat(3,20pt);grid-auto-flow:#{flow}'>
          <div style='grid-column:span 2;height:20pt;background:red'></div>
          <div style='grid-column:span 2;height:20pt;background:blue'></div>
          <div style='height:20pt;background:lime'></div>
        </div>
        """)

      third = Enum.find(items, &(&1.fill_color == {0, 1, 0}))
      assert_in_delta third.x, x, 0.001
      assert_in_delta third.y + third.height, top, 0.001
    end
  end

  test "template areas create implicit named edges and template shorthand assigns row sizes" do
    [first, second] =
      boxes("""
      <div style='display:grid;grid-template:"title title" 20pt "left right" 30pt / 40pt 60pt'>
        <div style='grid-area:title;background:red'></div><div style='grid-area:right;background:blue'></div>
      </div>
      """)

    assert {first.x, first.width, first.height} == {0.0, 100.0, 20.0}
    assert {second.x, second.width, second.height} == {40.0, 60.0, 30.0}
    assert second.y + second.height == 280
    assert :error = CssGridValidator.areas("\"a a\" \"a b\"")
  end

  test "subgrid inherits parent track positions and named lines" do
    items =
      boxes("""
      <div style='display:grid;width:120pt;grid-template-columns:[Start] 30pt [Middle] 80pt [End];gap:10pt'>
        <div style='display:grid;grid-column:1 / -1;grid-template-columns:subgrid'>
          <div style='grid-column:Start / Middle;height:10pt;background:red'></div>
          <div style='grid-column:Middle / End;height:10pt;background:blue'></div>
        </div>
      </div>
      """)

    assert Enum.map(items, &{&1.x, &1.width}) == [{0.0, 30.0}, {40.0, 80.0}]
  end

  test "implicit sizes cycle and grid shorthand clears the previous template" do
    items =
      boxes("""
      <div style='display:grid;grid-template-columns:99pt;grid:auto-flow 10pt 20pt / 30pt'>
        <div style='background:red'></div><div style='background:blue'></div><div style='background:lime'></div>
      </div>
      """)

    assert Enum.map(items, & &1.width) == [30.0, 30.0, 30.0]
    assert Enum.map(items, & &1.height) == [10.0, 20.0, 10.0]
  end

  test "explicit zero subgrid gutters share the parent gap center rather than inheriting normal" do
    for {gap, first_width, second_x, second_width} <- [{"normal", 30, 40, 60}, {"0", 35, 35, 65}] do
      [first, second] =
        boxes("""
        <div style='display:grid;width:100pt;grid-template-columns:30pt 60pt;gap:10pt'>
          <div style='grid-column:span 2;display:grid;grid-template-columns:subgrid;column-gap:#{gap}'>
            <div style='height:10pt;background:red'></div><div style='height:10pt;background:blue'></div>
          </div>
        </div>
        """)

      assert_in_delta first.width, first_width, 0.001
      assert_in_delta second.x, second_x, 0.001
      assert_in_delta second.width, second_width, 0.001
    end
  end

  test "percentage gutters use content axes and cyclic row percentages overflow the intrinsic allocation" do
    for {height, second_top} <- [{"height:100pt", 270}, {"", 276}] do
      [first, second] =
        boxes("""
        <div style='display:grid;width:100pt;#{height};grid-template-columns:20pt 20pt;grid-template-rows:20pt 20pt;gap:10%;align-content:start'>
          <div style='background:red'></div><div style='grid-row:2;grid-column:2;background:blue'></div>
        </div>
        """)

      assert first.x == 0
      assert_in_delta second.x, 30, 0.001
      assert_in_delta second.y + second.height, second_top, 0.001
    end
  end

  test "right-to-left named subgrids preserve parent lines across an opposite child direction" do
    [first, second] =
      boxes("""
      <div style='display:grid;direction:rtl;width:100pt;grid-template-columns:[a] 30pt [b] 60pt [c];gap:10pt'>
        <div style='grid-column:a / c;direction:ltr;display:grid;grid-template-columns:subgrid'>
          <div style='grid-column:c / b;height:10pt;background:red'></div><div style='grid-column:b / a;height:10pt;background:blue'></div>
        </div>
      </div>
      """)

    assert_in_delta first.x, 0, 0.001
    assert_in_delta first.width, 60, 0.001
    assert_in_delta second.x, 70, 0.001
    assert_in_delta second.width, 30, 0.001
  end

  test "named span occurrences resolve in both directions and automatic named spans retain their count" do
    alias NativeElixirPdfUtilities.HtmlToPdf.GridTracks

    style = %{
      grid_template_columns: List.duplicate({:length, 10}, 4),
      grid_column_names: %{"a" => [1, 2, 3, 4]}
    }

    for {start, finish, expected} <- [
          {1, {:named_span, "a", 2}, {1, 3}},
          {{:named_span, "a", 2}, 4, {2, 4}},
          {{:named_span, "a", 2}, :auto, {{:span, 2}, :auto}},
          {:auto, {:named_span, "a", 2}, {:auto, {:span, 2}}},
          {{:named_line, "a", -2}, 4, {3, 4}}
        ] do
      item =
        GridTracks.place_lines(
          %{column_start: start, column_end: finish, row_start: :auto, row_end: :auto},
          style
        )

      assert {item.column_start, item.column_end} == expected
    end
  end

  test "implicit prefixes and collapsed auto-fit tracks retain remapped names" do
    alias NativeElixirPdfUtilities.HtmlToPdf.GridTracks
    item = %{column_start: 0, column_end: 1, row_start: 1, row_end: 2}

    style = %{
      grid_template_columns: [{:length, 10}],
      grid_auto_columns: {:track_cycle, [{:length, 10}, {:length, 20}]},
      grid_column_names: %{"a" => [1]}
    }

    {[placed], expanded} = GridTracks.prepend_implicit([item], style)
    assert placed.column_start == 1
    assert hd(expanded.grid_template_columns) == {:length, 20}
    assert expanded.grid_column_names == %{"a" => [2]}

    style = %{
      grid_template_columns: List.duplicate({:length, 10}, 3),
      grid_column_auto_fit: [0, 1, 2],
      grid_column_names: %{"a" => [1, 2, 3, 4]}
    }

    {[placed], collapsed} =
      GridTracks.collapse_auto_fit([%{item | column_start: 2, column_end: 3}], style)

    assert {placed.column_start, placed.column_end} == {1, 2}
    assert collapsed.grid_column_names == %{"a" => [1, 2]}
  end

  test "repeat selection and percentage tracks preserve indefinite-axis behavior" do
    alias NativeElixirPdfUtilities.HtmlToPdf.GridTracks

    style = %{
      grid_template_columns: [{:repeat, :auto_fill, [{:minmax, {:length, 10}, {:length, 20}}]}]
    }

    assert length(GridTracks.expand(style, nil, nil).grid_template_columns) == 1
    assert length(GridTracks.expand(style, 100, nil).grid_template_columns) == 5
    assert GridTracks.resolve({:length, {:percent, 0.2}}, nil, %{}) == :auto
    assert GridTracks.resolve({:length, {:percent, 0.2}}, 100, %{}) == {:length, 20.0}
    assert GridTracks.resolve({:fit_content, 20}, 100, %{}) == {:fit_content, 20}

    assert {:error, {:resource_limit_exceeded, _}} =
             NativeElixirPdfUtilities.Validators.HtmlValidator.with_render_budget(fn ->
               GridTracks.expand(
                 %{grid_template_columns: [{:repeat, :auto_fill, [{:length, 1}]}]},
                 NativeElixirPdfUtilities.Limits.get(:max_entries) + 1,
                 nil
               )
             end)
  end

  test "subgrid fixed and auto-fill name repeats bind to the actual parent span" do
    for {tracks, names, placement, expected} <- [
          {"20pt 40pt", "repeat(2,[x]) [end]", "x 2 / end", 20},
          {"repeat(3,20pt)", "repeat(auto-fill,[x]) [end]", "x 3 / end", 40}
        ] do
      [box] =
        boxes("""
        <div style='display:grid;width:100pt;grid-template-columns:#{tracks}'>
          <div style='grid-column:1 / -1;display:grid;grid-template-columns:subgrid #{names}'>
            <div style='grid-column:#{placement};height:10pt;background:red'></div>
          </div>
        </div>
        """)

      assert_in_delta box.x, expected, 0.001
    end
  end

  test "area line names merge with explicit names and indefinite subgrids retain intrinsic tracks" do
    alias NativeElixirPdfUtilities.HtmlToPdf.GridTracks

    expanded =
      GridTracks.expand(
        %{
          grid_template_columns: [
            {:line_names, ["a-start"]},
            {:length, 20},
            {:line_names, ["a-end"]}
          ],
          grid_template_areas: [["a"]]
        },
        100,
        nil
      )

    assert expanded.grid_column_names == %{"a-start" => [1], "a-end" => [2]}
    item = %{style: %{grid_template_rows: [{:subgrid, []}]}, row_start: 1, row_end: 3}
    inherited = GridTracks.inherit(item, %{grid_template_rows: [:auto, :max_content]}, nil, nil)

    assert Enum.reject(inherited.style.grid_template_rows, &match?({:line_names, _}, &1)) == [
             :auto,
             :max_content
           ]
  end

  test "subgrid padding consumes only its outside tracks and preserves shared interior lines" do
    for direction <- ["ltr", "rtl"] do
      [first, second] =
        boxes(
          "<div style='display:grid;width:100pt;direction:#{direction};grid-template-columns:40pt 50pt;gap:10pt'><div style='grid-column:1 / 3;display:grid;grid-template-columns:subgrid;padding:0 5pt;border:0.75pt solid black'><div style='height:10pt;background:red'></div><div style='height:10pt;background:blue'></div></div></div>"
        )

      assert_in_delta first.width, 34.25, 0.02
      assert_in_delta second.width, 44.25, 0.02

      assert_in_delta abs(second.x - first.x),
                      if(direction == "ltr", do: 44.25, else: 54.25),
                      0.02
    end
  end

  test "subgrid edge insets contribute to their own intrinsic parent tracks" do
    [first, second] =
      boxes(
        "<div style='display:inline-grid;grid-template-columns:max-content max-content;gap:10pt'><div style='grid-column:1 / 3;display:grid;grid-template-columns:subgrid;padding:0 5pt'><div style='width:20pt;height:10pt;background:red'></div><div style='width:30pt;height:10pt;background:blue'></div></div></div>"
      )

    assert_in_delta first.x, 5, 0.02
    assert_in_delta first.width, 20, 0.02
    assert_in_delta second.x, 35, 0.02
    assert_in_delta second.width, 30, 0.02
  end

  defp boxes(html) do
    assert {:ok, dom} = HtmlParser.parse(html)
    assert {:ok, styled} = Style.compute(dom, system_font_discovery: false)
    assert {:ok, layout} = Layout.layout(styled, page_size: {300, 300}, margin: 0)

    Enum.filter(
      layout.boxes,
      &(&1.type == :rect and Map.get(&1, :fill_color) in [{1, 0, 0}, {0, 0, 1}, {0, 1, 0}])
    )
  end
end
