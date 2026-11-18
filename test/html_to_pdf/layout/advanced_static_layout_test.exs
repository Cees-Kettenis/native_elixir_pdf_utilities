defmodule NativeElixirPdfUtilities.HtmlToPdf.AdvancedStaticLayoutTest do
  use ExUnit.Case, async: true

  alias NativeElixirPdfUtilities.HtmlToPdf
  alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, Layout, Style}

  @opts [page_size: {240, 240}, margin: 0, system_font_discovery: false]
  @red {1, 0, 0}
  @green {0, 0.5019607843, 0}
  @blue {0, 0, 1}

  test "grid lanes place fixed cards by running position and preserve explicit lane spans" do
    html = """
    <div style="display:grid-lanes;grid-template-columns:40pt 40pt;gap:5pt;fit-tolerance:0">
      <div style="height:30pt;background:red"></div>
      <div style="height:10pt;background:green"></div>
      <div style="height:15pt;background:blue"></div>
      <div style="height:10pt;background:black;grid-column:span 2"></div>
    </div>
    """

    boxes = boxes(html)
    assert %{x: +0.0, y: 210.0, width: 40.0} = fill(boxes, @red)
    assert %{x: 45.0, y: 230.0, width: 40.0} = fill(boxes, @green)
    assert %{x: 45.0, y: 210.0, width: 40.0} = fill(boxes, @blue)
    assert %{x: +0.0, y: 195.0, width: 85.0} = fill(boxes, {0, 0, 0})
  end

  test "fit tolerance breaks near-ties by cursor and definite rows support horizontal lanes" do
    html = """
    <section style="display:grid-lanes;grid-template-columns:40pt 40pt;fit-tolerance:10pt">
      <div style="height:19pt"></div>
      <div style="height:12pt"></div>
      <div style="height:8pt;background:red"></div>
    </section>
    <section style="display:grid-lanes;grid-template-rows:20pt 20pt;gap:5pt;fit-tolerance:0">
      <div style="width:30pt"></div>
      <div style="width:10pt"></div>
      <div style="width:12pt;background:blue"></div>
    </section>
    """

    boxes = boxes(html)
    assert %{x: +0.0, y: 213.0} = fill(boxes, @red)
    assert %{x: 15.0, y: 168.0, width: 12.0, height: 20.0} = fill(boxes, @blue)
  end

  test "grid rules stop at spanning cells and inset the remaining segments" do
    html = """
    <div style="display:grid;grid-template-columns:40pt 40pt;grid-template-rows:20pt 20pt;
                gap:8pt;rule:2pt solid red;column-rule-inset:2pt">
      <div style="grid-column:span 2">Span</div><div>One</div><div>Two</div>
    </div>
    """

    rules = Enum.filter(boxes(html), &match?(%{role: :gap_rule}, &1))

    assert [
             %{x: 43.0, y: 194.0, width: 2.0, height: 20.0},
             %{x: +0.0, y: 215.0, width: 88.0, height: 2.0}
           ] = rules
  end

  test "named flows replace frame children and continue to the next page in source order" do
    html = """
    <style>.frame{flow-from:article;width:90pt;height:26pt}</style>
    <section style="flow-into:article">
      <div style="line-height:12pt;orphans:1;widows:1;color:red">First<br>Second<br>Third<br>Fourth</div>
    </section>
    <div class=frame>Fallback</div>
    <div class=frame style="break-before:page">Fallback too</div>
    """

    assert {:ok, dom} = HtmlParser.parse_detailed(html)
    assert {:ok, tree} = Style.compute_detailed(dom, @opts)
    assert {:ok, layout} = Layout.layout(tree, @opts)

    assert {:ok, [first, second]} =
             NativeElixirPdfUtilities.HtmlToPdf.Pagination.paginate(layout, @opts)

    assert ["First", "Second"] = text(first.boxes)
    assert ["Third", "Fourth"] = text(second.boxes)

    assert Enum.all?(first.boxes ++ second.boxes, fn box ->
             box.type != :text or box.color == @red
           end)
  end

  test "named flows diagnose mismatched explicit frame dimensions and preserve last-frame overflow" do
    present = """
    <div style="flow-into:story;line-height:12pt;orphans:1;widows:1">One<br>Two<br>Three</div>
    <div style="flow-from:story;width:90pt;height:18pt"></div>
    """

    assert ["One", "Two", "Three"] = text(boxes(present))

    assert ["One"] =
             text(
               boxes(String.replace(present, "height:18pt", "height:18pt;region-fragment:break"))
             )

    unequal = present <> ~s(<div style="flow-from:story;width:80pt;height:18pt"></div>)
    assert {:error, {:not_implemented, %{source: "flow-from"}}} = HtmlToPdf.render(unequal, @opts)
  end

  test "definite exclusions alter line bands and wrap-through suppresses that reservation" do
    html = """
    <div style="position:relative;width:120pt;height:72pt;line-height:12pt">
      <div style="position:absolute;left:0;top:0;width:42pt;height:27pt;wrap-flow:end"></div>
      <div>Wrapped words</div><div style="wrap-through:none">Unwrapped words</div>
    </div>
    """

    boxes = boxes(html)
    assert %{x: 42.0} = Enum.find(boxes, &match?(%{text: "Wrapped"}, &1))
    assert %{x: +0.0} = Enum.find(boxes, &match?(%{text: "Unwrapped words"}, &1))
  end

  test "shape-padding reduces the interior line interval" do
    html =
      ~s|<div style="width:90pt;height:60pt;font-size:10pt;line-height:12pt;shape-inside:inset(0);shape-padding:5pt">Inset</div>|

    assert %{x: 5.0} = Enum.find(boxes(html), &match?(%{text: "Inset"}, &1))
  end

  test "shared baseline phase and independent step sizing use the selected line metrics" do
    html = """
    <div style="line-grid:create;line-height:24pt;line-snap:baseline">
      <div style="line-height:12pt">One<br>Two<br>Three</div>
    </div>
    <div style="height:12pt;block-step-size:10pt;block-step-insert:padding-box;block-step-align:start;background:red"></div>
    <div style="height:4pt;background:blue"></div>
    """

    boxes = boxes(html)
    [a, b, c] = for %{type: :text, y: y} <- boxes, do: y
    assert_in_delta a - b, 24.0, 1.0e-5
    assert_in_delta b - c, 24.0, 1.0e-5
    assert %{height: 20.0} = fill(boxes, @red)
    assert %{height: 4.0} = fill(boxes, @blue)

    stepped = ~s(<div style="line-height:15pt;line-height-step:12pt">First<br>Second</div>)
    [a, b] = for %{type: :text, y: y} <- boxes(stepped), do: y
    assert_in_delta a - b, 24.0, 1.0e-5
  end

  test "border contours, cuts and shadow lists remain native paint on the same box" do
    html = """
    <div style="width:60pt;height:30pt;background:red;border:3pt solid blue;
                border-radius:9pt;corner-shape:bevel scoop notch squircle"></div>
    <div style="width:60pt;height:30pt;border:3pt solid blue;
                border-shape:circle(40%);box-shadow-offset:4pt 2pt, none, 2pt;
                box-shadow-spread:1pt;box-shadow-color:rgb(100 0 0);
                box-shadow-blur:1pt, 3pt;box-shadow-position:outset, inset"></div>
    <div style="width:60pt;height:24pt;border:2pt solid green;
                border-top-clip:25% 50% 1fr;border-limit:sides"></div>
    """

    boxes = boxes(html)
    assert %{clip: %{path_commands: [_ | _]}} = fill(boxes, @red)
    assert Enum.count(boxes, &match?(%{css_paint_role: :shaped_border}, &1)) == 2
    assert Enum.count(boxes, &match?(%{css_paint_role: :box_shadow}, &1)) == 2

    top =
      Enum.filter(
        boxes,
        &match?(%{css_paint_role: :clipped_border, border_styles: %{top: :solid}}, &1)
      )

    assert [%{clip: %{x: +0.0, width: 15.75}}, %{clip: %{x: 47.25, width: 15.75}}] = top
    assert {:ok, "%PDF-" <> _} = HtmlToPdf.render(html, @opts)
  end

  test "short regions advance named content to a taller frame at authored boundaries" do
    html = """
    <div style="display:none;flow-into:story">Hidden</div>
    <div style="flow-into:story content">
      <div style="line-height:12pt;color:red">One</div>
      <div style="line-height:12pt;break-before:region;break-inside:avoid-region">Two</div>
      <div style="line-height:12pt">Three</div>
    </div>
    <section>
      <div style="flow-from:story;width:80pt;height:18pt;margin-left:10pt"></div>
      <div style="flow-from:story;width:80pt;height:42pt;margin-left:20pt"></div>
      <div style="flow-from:empty">No source</div>
    </section>
    """

    assert [
             %{text: "One", x: 10.0, color: @red},
             %{text: "Two", x: 20.0},
             %{text: "Three", x: 20.0}
           ] =
             Enum.filter(boxes(html), &match?(%{type: :text}, &1))

    assert {:error, {:not_implemented, %{source: "flow-from"}}} =
             HtmlToPdf.render(String.replace(html, "height:18pt", "height:auto"), @opts)

    assert {:error, {:not_implemented, %{source: "flow-into"}}} =
             HtmlToPdf.render(
               String.replace(
                 html,
                 "flow-into:story content",
                 "flow-into:story element;position:absolute;top:2pt"
               ),
               @opts
             )
  end

  test "exclusions choose explicit start, narrow and clear intervals around a definite obstacle" do
    for {flow, expected} <- [
          {"start", 0},
          {"minimum", 0},
          {"maximum", 55},
          {"both", 55},
          {"clear", 0}
        ] do
      html = """
      <div style="position:relative;width:120pt;height:80pt;line-height:12pt">
        <div style="position:absolute;right:65pt;top:0;width:20pt;height:30pt;wrap-flow:#{flow}"></div>
        <p style="margin:0">Text</p>
      </div>
      """

      assert %{x: x, y: y} = Enum.find(boxes(html), &match?(%{text: "Text"}, &1))
      assert x == expected
      if flow == "clear", do: assert(y < 210)
    end

    html = """
    <div style="position:relative;width:100pt;height:80pt;direction:rtl;line-height:12pt">
      <div style="position:absolute;left:25%;bottom:40pt;width:20pt;height:40pt;wrap-flow:start"></div>
      <div style="direction:ltr">Text</div>
    </div>
    """

    assert %{x: 45.0} = Enum.find(boxes(html), &match?(%{text: "Text"}, &1))

    assert %{x: +0.0} =
             Enum.find(
               boxes(String.replace(html, "wrap-flow:start", "wrap-flow:end")),
               &match?(%{text: "Text"}, &1)
             )

    unsupported = String.replace(html, "bottom:40pt", "bottom:auto")

    assert {:error, {:not_implemented, %{source: "wrap-flow"}}} =
             HtmlToPdf.render(unsupported, @opts)
  end

  test "gap rules follow occupied cells and follow lanes through a spanning card" do
    html = """
    <div style="display:grid-lanes;grid-template-columns:40pt 40pt;gap:8pt;
                fit-tolerance:infinite;rule-style:solid;rule-width:2pt;color:red">
      <div style="grid-column:2;height:12pt">One</div>
      <div style="grid-column:2;height:12pt">Two</div>
      <div style="grid-column:span 2;height:10pt">Three</div>
      <div style="height:12pt">Four</div>
    </div>
    """

    rules = Enum.filter(boxes(html), &match?(%{role: :gap_rule}, &1))
    assert Enum.all?(rules, fn box -> @red in Map.values(box.border_colors) end)
    assert Enum.any?(rules, &match?(%{x: 43.0, width: 2.0}, &1))
    assert Enum.any?(rules, &match?(%{x: 48.0, width: 40.0}, &1))

    html = """
    <div style="display:grid;grid-template-columns:40pt 40pt 40pt;grid-template-rows:20pt 20pt;
      gap:8pt;column-rule:2pt solid;column-rule-break:intersection;column-rule-inset-start:10%;
      column-rule-visibility-items:around">
      <div style="grid-column:1">One</div><div style="grid-column:2">Two</div>
    </div>
    """

    rules = Enum.filter(boxes(html), &match?(%{role: :gap_rule}, &1))
    assert length(rules) == 2
    Enum.each(rules, &assert_in_delta(&1.height, 21.6, 1.0e-6))

    assert [] =
             html
             |> String.replace("around", "between")
             |> String.replace("grid-column:2", "grid-column:3")
             |> boxes()
             |> Enum.filter(&match?(%{role: :gap_rule}, &1))
  end

  test "line-grid alignment covers block edges and line fit placeholders" do
    alias NativeElixirPdfUtilities.HtmlToPdf.LineRhythm

    assert LineRhythm.box_shift(:baseline, 120.0, 110.0, []) == 0.0
    assert LineRhythm.line_shift(:contain, 120.0, 8.0, 6.0) == 0.0
    assert LineRhythm.reset_origin(fn -> :unmodified end) == :unmodified

    line = %{font_size: 10.0, line_height: 20.0, font_face: %{}}

    LineRhythm.with_grid(line, 120.0, fn ->
      assert LineRhythm.box_shift(:block_start, 115.0, 105.0, []) == 15.0
      assert LineRhythm.box_shift(:block_end, 115.0, 105.0, []) == 5.0
      assert LineRhythm.box_shift(:center, 115.0, 105.0, []) == 0.0
      assert LineRhythm.box_shift(:baseline, 115.0, 105.0, []) == 0.0

      assert LineRhythm.box_shift(:last_baseline, 115.0, 105.0, [
               %{type: :text, y: 109.0},
               %{type: :rect}
             ]) == 19.0

      assert LineRhythm.line_shift(:contain, 120.0, 8.0, 6.0) == 6.0

      assert LineRhythm.reset_origin(fn ->
               LineRhythm.box_shift(:block_start, -5.0, -10.0, [])
             end) == 15.0
    end)

    assert LineRhythm.block_step(%{block_step_size: 10.0}, 15.0, 0.0) == {0.0, 2.5, 5.0}

    assert LineRhythm.block_step(
             %{block_step_size: 10.0, block_step_round: :down, block_step_align: :end},
             15.0,
             0.0
           ) == {0.0, -5.0, -5.0}

    assert LineRhythm.block_step(
             %{
               block_step_size: 10.0,
               block_step_round: :nearest,
               block_step_insert: :content_box
             },
             15.0,
             0.0
           ) == {5.0, 0.0, 0.0}

    html = """
    <div style="line-grid:create;line-snap:contain;line-height:18pt;columns:2;column-gap:8pt;
                width:120pt;height:42pt">
      <span style="line-height:10pt">One</span><br>Two<br>Three<br>Four
    </div>
    <div style="line-grid:create;line-height:20pt">
      <div style="height:5pt"></div>
      <div style="height:10pt;box-snap:block-start;background:red"></div>
    </div>
    """

    boxes = boxes(html)
    assert ["One", "Two", "Three", "Four"] == text(boxes)
    assert %{height: 10.0} = fill(boxes, @red)
  end

  test "edge clips and a two-shape border preserve independent path geometry" do
    html = """
    <div style="width:60pt;height:40pt;border:3pt solid blue;border-shape:ellipse(50% 45%) ellipse(40% 30%);background:red"></div>
    <div style="width:60pt;height:40pt;border:3pt solid red;border-radius:8pt;corner-shape:square"></div>
    <div style="width:60pt;height:40pt;border:3pt solid green;border-radius:8pt;border-limit:corners 4pt"></div>
    <div style="width:60pt;height:40pt;border:3pt solid blue;border-radius:8pt;border-limit:top 20%;border-top-clip:10pt"></div>
    """

    paint = boxes(html)
    assert Enum.count(paint, &match?(%{css_paint_role: :shaped_border}, &1)) == 2
    assert Enum.count(paint, &match?(%{css_paint_role: :clipped_border}, &1)) == 11
    assert {:ok, "%PDF-" <> _} = HtmlToPdf.render(html, @opts)
  end

  test "row lanes support percentage tolerance and paint rules along the stacking axis" do
    for tolerance <- ["normal", "15%", "calc(10% + 1pt)"] do
      html = """
      <div style="display:grid-lanes;direction:rtl;grid-template-rows:20pt 20pt;
        width:100pt;gap:5pt;fit-tolerance:#{tolerance};column-rule:2pt solid red;
        rule-overlap:row-over-column">
        <div style="grid-row:1;width:30pt;background:blue"></div>
        <div style="grid-row:1;width:20pt;background:red"></div>
      </div>
      """

      boxes = boxes(html)
      assert %{x: 70.0, y: 220.0} = fill(boxes, @blue)
      assert %{x: 45.0, y: 220.0} = fill(boxes, @red)
      assert [%{x: x, width: 2.0}] = Enum.filter(boxes, &match?(%{role: :gap_rule}, &1))
      assert_in_delta x, 66.5, 1.0e-5
    end
  end

  test "overflow uses the shaped inner contour and a shaped shadow uses the same boundary" do
    html = """
    <div style="width:60pt;height:40pt;overflow:hidden;border:3pt solid blue;
      border-shape:circle(40%);box-shadow-offset:4pt 3pt;box-shadow-color:red">
      <div style="width:90pt;height:50pt;background:black"></div>
    </div>
    """

    paint = boxes(html)
    assert Enum.any?(paint, &match?(%{css_paint_role: :box_shadow}, &1))

    assert Enum.any?(
             paint,
             &match?(%{clip: %{path_commands: [_ | _]}, boxes: [%{fill_color: {0, 0, 0}}]}, &1)
           )

    assert {:ok, "%PDF-" <> _} = HtmlToPdf.render(html, @opts)

    ordinary = """
    <div style="width:60pt;height:40pt;border:3pt solid blue;
      border-radius:12pt;border-limit:corners"></div>
    <div style="width:60pt;height:40pt;border:3pt solid blue;
      border-radius:12pt;border-top-clip:8pt 10pt"></div>
    <div style="width:60pt;height:40pt;border:3pt solid blue;corner-shape:round"></div>
    """

    assert Enum.count(boxes(ordinary), &match?(%{css_paint_role: :clipped_border}, &1)) == 13
  end

  test "vertical multicol applies the same line grid across columns after a font change" do
    html = """
    <div style="writing-mode:vertical-rl;line-grid:create;line-snap:baseline;line-height:18pt;
      font-size:10pt;columns:2;column-gap:8pt;width:48pt;height:110pt;orphans:1;widows:1">
      <div style="font-size:8pt;line-height:9pt">A<br>B<br>C<br>D</div>
    </div>
    """

    assert [%{role: :writing_mode, boxes: vertical}] = boxes(html)
    runs = Enum.filter(vertical, &match?(%{type: :text}, &1))
    assert Enum.map(runs, & &1.text) == ["A", "B", "C", "D"]
    [a, b, c, d] = runs
    assert_in_delta a.y - b.y, 18.0, 1.0e-5
    assert_in_delta c.y - d.y, 18.0, 1.0e-5
    assert_in_delta a.y, c.y, 1.0e-5
    assert c.x > a.x
  end

  test "a contour keeps its image and paint crop and asymmetric radii shape the overflow clip" do
    alias NativeElixirPdfUtilities.HtmlToPdf.{BorderShape, DrawingGeometry, LineRhythm}
    assert [marker] = LineRhythm.align_column([%{type: :flow_marker}], 40.0, :baseline)
    assert marker.type == :flow_marker

    html = """
    <div style="width:80pt;height:50pt;overflow:hidden;border-radius:16pt 8pt;border-top-left-radius:10pt 4pt;
      border:2pt solid blue;corner-shape:bevel;background:linear-gradient(red, blue)">
      <div style="width:100pt;height:80pt;background:black"></div>
    </div>
    """

    boxes = boxes(html)
    assert Enum.any?(boxes, &match?(%{clip: %{path_commands: _}}, &1))
    assert {:ok, "%PDF-" <> _} = HtmlToPdf.render(html, @opts)
    layer = Enum.find(boxes, &match?(%{clip: %{path_commands: _}, type: :rect}, &1))

    assert [%{type: :drawing_group, boxes: [%{clip: %{path_commands: _}}]}] =
             DrawingGeometry.clip_boxes([layer], %{x: 0.0, y: 0.0, width: 200.0, height: 200.0})

    base = %{x: 0.0, y: 0.0, width: 40.0, height: 32.0, type: :image}

    rect =
      Map.merge(base, %{
        border_styles: %{top: :none},
        border_widths: %{top: 0.0, right: 0.0, bottom: 0.0, left: 0.0},
        border_colors: %{top: nil},
        border_radius: 6.0,
        stroke_width: 0.0
      })

    assert [%{type: :image, clip: %{path_commands: [_ | _]}}] =
             BorderShape.apply(%{corner_top_left_shape: :square}, rect, [base])
  end

  test "a column-only rule paints along grid lanes with no stack rule" do
    html = """
    <div style="display:grid-lanes;grid-template-columns:40pt 40pt;gap:8pt;column-rule:solid 2pt blue">
      <div style="height:12pt"></div><div style="height:20pt"></div>
    </div>
    """

    assert [%{x: 43.0, width: 2.0, height: 20.0}] =
             Enum.filter(boxes(html), &match?(%{role: :gap_rule}, &1))
  end

  defp boxes(html) do
    assert {:ok, dom} = HtmlParser.parse_detailed(html)
    assert {:ok, tree} = Style.compute_detailed(dom, @opts)
    assert {:ok, layout} = Layout.layout(tree, @opts)
    layout.boxes
  end

  defp fill(boxes, color), do: Enum.find(boxes, &(Map.get(&1, :fill_color) == color))
  defp text(boxes), do: for(%{type: :text, text: text} <- boxes, do: text)
end
