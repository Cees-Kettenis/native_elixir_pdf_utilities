defmodule NativeElixirPdfUtilities.HtmlToPdf.AnchorLayoutTest do
  use ExUnit.Case, async: true

  alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, Layout, Style}

  @options [page_size: {200, 200}, margin: 0, system_font_discovery: false]

  test "anchors resolve against the preceding eligible border box" do
    html = """
    <div style="position:relative;width:120pt;height:100pt">
      <div style="anchor-name:--ref;width:30pt;height:20pt;margin-left:10pt"></div>
      <div style="position:absolute;position-anchor:--ref;left:anchor(right);
                  top:anchor(bottom);width:anchor-size(width);height:10pt;background:red"></div>
      <div style="anchor-name:--ref;width:80pt;height:30pt"></div>
    </div>
    """

    assert %{x: 40.0, y: 170.0, width: 30.0, height: 10.0} = rect(html)
  end

  test "position area resolves finite placement and selects a fitting fallback" do
    html = """
    <style>
      @position-try --above {position-area: top center}
    </style>
    <div style="position:relative;width:100pt;height:100pt;padding-top:70pt;box-sizing:border-box">
      <div style="anchor-name:--ref;width:30pt;height:22pt;margin-left:35pt"></div>
      <div style="position:absolute;position-anchor:--ref;position-area:bottom center;
                  position-try:--above; width:20pt;height:15pt;background:red"></div>
    </div>
    """

    assert %{x: 40.0, y: 130.0, width: 20.0, height: 15.0} = rect(html)
  end

  test "missing anchors use the supplied fallback unless visibility requires one" do
    for {visibility, expected} <- [{"always", true}, {"anchors-valid", false}] do
      html = """
      <div style="position:relative;width:60pt;height:40pt">
        <div style="position:absolute;left:anchor(--missing right, 7pt);top:2pt;
                    width:10pt;height:10pt;background:red;position-visibility:#{visibility}"></div>
      </div>
      """

      result = rect(html)
      assert result != nil == expected
      if expected, do: assert(%{x: 7.0, y: 188.0} = result)
    end
  end

  test "scope boundaries hide duplicate names from other subtrees" do
    html = """
    <div style="position:relative;width:100pt;height:100pt">
      <section style="anchor-scope:all">
        <div style="anchor-name:--ref;width:30pt;height:20pt"></div>
        <div style="position:absolute;left:anchor(--ref right);top:0;
                    width:10pt;height:10pt;background:red"></div>
      </section>
      <div style="position:absolute;left:anchor(--ref right, 5pt);top:15pt;
                  width:10pt;height:10pt;background:blue"></div>
    </div>
    """

    shapes = rects(html)
    assert %{x: 30.0} = Enum.find(shapes, &match?(%{fill_color: {1, 0, 0}}, &1))
    assert %{x: 5.0} = Enum.find(shapes, &match?(%{fill_color: {0, 0, 1}}, &1))
  end

  test "position-area fallbacks try finite alternatives and no-overflow hides a failed option" do
    html = """
    <div style="position:relative;width:100pt;height:50pt">
      <div style="width:20pt;height:15pt;margin-left:25pt;anchor-name:--ref"></div>
      <div style="position:absolute;position-anchor:--ref;position-area:top center;
        position-try-fallbacks:flip-block;position-try-order:most-height;
        width:10pt;height:10pt;background:red"></div>
      <div style="position:absolute;position-anchor:--ref;position-area:top center;
        position-try-fallbacks:--undefined, span-bottom;position-visibility:no-overflow;
        width:200pt;height:10pt;background:blue"></div>
    </div>
    """

    assert [%{x: 30.0, y: 175.0, fill_color: {1, 0, 0}}] = rects(html)
  end

  test "hidden geometry still resolves but anchors-visible suppresses the callout" do
    html = """
    <div style="position:relative;width:100pt;height:80pt">
      <div style="visibility:hidden;anchor-name:--hidden;width:20pt;height:15pt"></div>
      <div style="position:absolute;position-anchor:--hidden;left:anchor(right);
                  top:0;width:10pt;height:10pt;background:red;
                  position-visibility:anchors-valid"></div>
    </div>
    """

    assert %{x: 20.0} = rect(html)
    assert nil == rect(String.replace(html, "anchors-valid", "anchors-visible"))
  end

  test "side coordinates and anchor sizes use each owner's axes" do
    alias NativeElixirPdfUtilities.HtmlToPdf.AnchorLayout

    anchor = %{
      x: 20.0,
      top: 80.0,
      width: 30.0,
      height: 20.0,
      names: ["--ref"],
      order: [0],
      writing_mode: :vertical_rl,
      direction: :ltr
    }

    block = %{
      x: 0.0,
      top: 100.0,
      width: 100.0,
      height: 100.0,
      writing_mode: :horizontal_tb,
      direction: :ltr
    }

    style = %{position_anchor: "--ref", _anchor_source_order: [1]}

    place = fn prepared, _block ->
      {:ok, %{bounds: %{x: 2.0, top: 98.0, width: 10.0, height: 10.0}, boxes: [prepared.style]}}
    end

    for {side, keyword, value} <- [
          {:left, "center", 35.0},
          {:left, "inside", 20.0},
          {:left, "right", 50.0},
          {:right, "left", 80.0},
          {:left, "self-end", 50.0},
          {:right, "start", 80.0},
          {:top, "start", 20.0},
          {:top, "end", 40.0},
          {:bottom, "top", 80.0},
          {:top, "outside", 40.0},
          {:left, {:percent, 0.25}, 27.5},
          {:top, "left", 3.0}
        ] do
      css = {:css_anchor, "anchor", nil, keyword, 3.0}
      child = %{style: Map.put(style, :offsets, %{side => css})}

      assert {:ok, %{boxes: [%{offsets: %{^side => ^value}}]}} =
               AnchorLayout.place(child, block, {[anchor], []}, place)
    end

    for {size, key, value} <- [
          {:width, "height", 20.0},
          {:height, "inline", 20.0},
          {:max_height, "block", 30.0},
          {:width, "self-inline", 30.0},
          {:height, "self-block", 20.0},
          {:width, :property, 30.0}
        ] do
      child = %{style: Map.put(style, size, {:css_anchor, "anchor-size", nil, key, :auto})}

      assert {:ok, %{boxes: [%{^size => ^value}]}} =
               AnchorLayout.place(child, block, {[anchor], []}, place)
    end
  end

  test "an identically named anchor in a different tree scope is ineligible" do
    alias NativeElixirPdfUtilities.HtmlToPdf.AnchorLayout

    host = make_ref()
    shadow = make_ref()
    base = %{x: 20.0, top: 80.0, width: 30.0, height: 20.0, names: ["--ref"]}

    anchors = [
      Map.merge(base, %{order: [0], tree_scope: host}),
      Map.merge(base, %{x: 50.0, order: [1], tree_scope: shadow})
    ]

    block = %{x: 0.0, top: 100.0, width: 100.0, height: 100.0}

    child = %{
      style: %{
        _anchor_source_order: [2],
        _anchor_tree_scope: host,
        offsets: %{left: {:css_anchor, "anchor", "--ref", "right", :auto}}
      }
    }

    assert {:ok, %{boxes: [%{offsets: %{left: 50.0}}]}} =
             AnchorLayout.place(child, block, {anchors, []}, fn node, _ ->
               {:ok,
                %{bounds: %{x: 50.0, top: 90.0, width: 5.0, height: 8.0}, boxes: [node.style]}}
             end)
  end

  test "area keywords map to the selected physical region in both writing modes" do
    alias NativeElixirPdfUtilities.HtmlToPdf.AnchorLayout

    anchor = %{x: 20.0, top: 80.0, width: 30.0, height: 20.0, names: ["--ref"], order: [0]}
    containing = %{x: 0.0, top: 100.0, width: 100.0, height: 100.0}
    style = %{position_anchor: "--ref", _anchor_source_order: [1]}

    place = fn _candidate, block ->
      {:ok, %{bounds: %{x: block.x, top: block.top, width: 1.0, height: 1.0}, boxes: [block]}}
    end

    for {area, writing_mode, width, height} <- [
          {~w(left), :horizontal_tb, 20.0, 100.0},
          {~w(right), :horizontal_tb, 50.0, 100.0},
          {~w(top), :horizontal_tb, 100.0, 20.0},
          {~w(center), :horizontal_tb, 30.0, 20.0},
          {~w(span-all), :horizontal_tb, 100.0, 100.0},
          {~w(x-start y-end), :horizontal_tb, 20.0, 60.0},
          {~w(span-left span-bottom), :horizontal_tb, 50.0, 80.0},
          {~w(center top), :horizontal_tb, 30.0, 20.0},
          {~w(center span-all), :horizontal_tb, 30.0, 100.0},
          {~w(start inline-end), :vertical_rl, 50.0, 60.0},
          {~w(self-block-end inline-start), :vertical_rl, 20.0, 20.0}
        ] do
      child = %{style: Map.merge(style, %{position_area: area, writing_mode: writing_mode})}

      assert {:ok, %{boxes: [%{width: ^width, height: ^height}]}} =
               AnchorLayout.place(child, containing, {[anchor], []}, place)
    end

    invalid = %{style: Map.put(style, :position_area, ~w(top bottom))}

    assert {:error, {:invalid_layout, %{source: "position-area"}}} =
             AnchorLayout.place(invalid, containing, {[anchor], []}, place)
  end

  test "absolute anchors register in source order and each flip operates in its selected axes" do
    html = """
    <div style="position:relative;width:80pt;height:50pt">
      <div style="position:absolute;top:1pt;left:25pt;width:20pt;height:12pt;anchor-name:--ref"></div>
      <div style="position:absolute;position-anchor:--ref;top:anchor(top);
                  width:10pt;height:8pt;background:red"></div>
    </div>
    """

    assert %{x: +0.0, y: 191.0} = rect(html)

    alias NativeElixirPdfUtilities.HtmlToPdf.AnchorLayout
    anchor = %{x: 20.0, top: 80.0, width: 30.0, height: 20.0, names: ["--anchor"], order: [0]}
    block = %{x: 0.0, top: 100.0, width: 100.0, height: 100.0}

    for {flip, mode, from, to, side} <- [
          {:x, :horizontal_tb, :left, :right, "left"},
          {:y, :horizontal_tb, :top, :bottom, "top"},
          {:inline, :horizontal_tb, :right, :left, "right"},
          {:inline, :vertical_rl, :top, :bottom, "top"},
          {:block, :vertical_lr, :left, :right, "left"},
          {:diagonal, :horizontal_tb, :left, :top, "left"}
        ] do
      style = %{
        writing_mode: mode,
        position_anchor: "--anchor",
        _anchor_source_order: [1],
        position_try_fallbacks: [{:rule, nil, [flip]}],
        offsets: %{from => {:css_anchor, "anchor", nil, side, :auto}}
      }

      original = fn node, _ ->
        offset = Map.get(node.style.offsets, to)
        x = if offset != nil, do: 15.0, else: 200.0
        {:ok, %{boxes: [node.style], bounds: %{x: x, top: 90.0, width: 10.0, height: 10.0}}}
      end

      assert {:ok, %{boxes: [%{offsets: %{^to => offset}}]}} =
               AnchorLayout.place(%{style: style}, block, {[anchor], []}, original)

      assert is_number(offset)
    end

    style = %{position_area: ~w(top), _anchor_source_order: [1], position_anchor: "--anchor"}
    failure = {:error, {:invalid_layout, %{source: "placement"}}}

    assert ^failure =
             AnchorLayout.place(%{style: style}, block, {[anchor], []}, fn _, _ -> failure end)

    assert AnchorLayout.track?(%{position_area: ~w(top)})
    assert AnchorLayout.track?(%{position_try_fallbacks: [{:area, ~w(top)}]})
  end

  test "a named fallback ignores invalid style values and keeps its other declarations" do
    html = """
    <style>@position-try --usable {left:bad;top:2pt}</style>
    <div style="position:relative;width:70pt;height:40pt">
      <div style="position:absolute;position-try-fallbacks:--usable;
        left:1pt;top:60pt;background:red;width:10pt;height:8pt"></div>
    </div>
    """

    assert %{x: 1.0, y: 190.0} = rect(html)
  end

  test "empty markers leave existing boxes alone and absent fallback values use auto" do
    alias NativeElixirPdfUtilities.HtmlToPdf.AnchorLayout
    block = %{x: 0.0, top: 100.0, width: 100.0, height: 100.0}
    box = %{type: :flow_marker, x: 0.0, y: 0.0, width: 0.0, height: 0.0}
    assert [^box] = AnchorLayout.mark(%{}, [box], block)
    assert [^box] = AnchorLayout.mark_scope(%{anchor_scope: []}, [box], 0.0, 100.0)

    style = %{
      _anchor_source_order: [0],
      offsets: %{left: {:css_anchor, "anchor", "--missing", "right", :auto}}
    }

    render = fn child, _ -> {:ok, %{boxes: [child.style], bounds: block}} end

    assert {:ok, %{boxes: [%{offsets: %{left: :auto}}]}} =
             AnchorLayout.place(%{style: style}, block, {[], []}, render)

    invalid = %{
      style: %{position_area: ~w(left right), position_try_fallbacks: [{:rule, nil, [:x]}]}
    }

    assert {:error, {:invalid_layout, %{source: "position-area"}}} =
             AnchorLayout.place(invalid, block, {[], []}, render)
  end

  defp rect(html), do: Enum.find(rects(html), &match?(%{fill_color: {1, 0, 0}}, &1))

  defp rects(html) do
    assert {:ok, dom} = HtmlParser.parse(html)
    assert {:ok, tree} = Style.compute_detailed(dom, @options)
    assert {:ok, layout} = Layout.layout(tree, @options)

    Enum.filter(
      layout.boxes,
      &match?(%{type: :rect, fill_color: color} when not is_nil(color), &1)
    )
  end
end
