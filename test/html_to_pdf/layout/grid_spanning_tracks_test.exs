defmodule NativeElixirPdfUtilities.HtmlToPdf.GridSpanningTracksTest do
  use ExUnit.Case, async: true

  alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, Layout, Style}

  test "a row span contributes its height to the grid and following flow" do
    boxes =
      layout("""
      <div style='display:grid;width:100pt;grid-template-columns:100pt;grid-template-rows:auto auto'>
        <div style='grid-row:1 / 3;height:40pt;background:red'></div>
      </div>
      <div style='height:10pt;background:blue'></div>
      """)

    assert_in_delta color(boxes, {0, 0, 1}).y + 10, 260, 0.001
  end

  test "column spans reserve intrinsic width in ordinary and inline grids" do
    for display <- ["grid", "inline-grid"] do
      boxes =
        layout("""
        <div style='display:#{display};grid-template-columns:auto auto;justify-content:start'>
          <div style='grid-column:1 / 3;width:80pt;height:20pt;background:red'></div>
          <div style='grid-row:2;grid-column:1;height:10pt;background:blue'></div>
          <div style='grid-row:2;grid-column:2;height:10pt;background:lime'></div>
        </div>
        """)

      assert_in_delta color(boxes, {0, 0, 1}).width, 40, 0.001
      assert_in_delta color(boxes, {0, 1, 0}).x - color(boxes, {0, 0, 1}).x, 40, 0.001
    end
  end

  test "spanning contributions include margins and decorations but count gutters once" do
    boxes =
      layout("""
      <div style='display:grid;width:100pt;grid-template-columns:100pt;grid-template-rows:auto auto;row-gap:10pt'>
        <div style='grid-row:1 / 3;height:40pt;margin:2pt;padding:3pt;background:red'></div>
      </div><div style='height:10pt;background:blue'></div>
      """)

    assert_in_delta color(boxes, {0, 0, 1}).y + 10, 250, 0.001
  end

  test "fixed tracks retain their size and intrinsic maximums absorb the remaining span" do
    for {tracks, expected_height} <- [
          {"10pt auto", 40},
          {"10pt 10pt", 25},
          {"minmax(auto, 15pt) auto", 40},
          {"minmax(auto, 15pt) minmax(auto, 15pt)", 40},
          {"minmax(10pt, auto) auto", 40},
          {"1fr 2fr", 40},
          {"0.25fr 0.25fr", 40},
          {"0fr 0fr", 40}
        ] do
      boxes =
        layout("""
        <div style='display:grid;width:100pt;grid-template-columns:100pt;grid-template-rows:#{tracks};row-gap:5pt'>
          <div style='grid-row:1 / 3;height:40pt;background:red'></div>
        </div><div style='height:10pt;background:blue'></div>
        """)

      assert_in_delta color(boxes, {0, 0, 1}).y + 10, 300 - expected_height, 0.001
    end
  end

  test "equal spans plan growth independently of source order and larger spans see it" do
    first = "<div style='grid-column:1 / 3;grid-row:1;width:60pt;height:10pt'></div>"
    second = "<div style='grid-column:2 / 4;grid-row:2;width:60pt;height:10pt'></div>"

    for items <- [first <> second, second <> first] do
      boxes =
        layout("""
        <div style='display:grid;grid-template-columns:auto auto auto;justify-content:start'>
          #{items}
          <div style='grid-column:1 / 4;grid-row:3;width:120pt;height:10pt'></div>
          <div style='grid-column:1;grid-row:4;height:10pt;background:red'></div>
          <div style='grid-column:2;grid-row:4;height:10pt;background:blue'></div>
          <div style='grid-column:3;grid-row:4;height:10pt;background:lime'></div>
        </div>
        """)

      for rgb <- [{1, 0, 0}, {0, 0, 1}, {0, 1, 0}] do
        assert_in_delta color(boxes, rgb).width, 40, 0.001
      end
    end
  end

  test "flexible spans of different lengths share one intrinsic sizing round" do
    boxes =
      layout("""
      <div style='display:grid;width:100pt;grid-template-columns:100pt;grid-template-rows:1fr 1fr 1fr'>
        <div style='grid-row:1 / 3;height:60pt'></div>
        <div style='grid-row:1 / 4;height:90pt'></div>
        <div style='grid-row:1;grid-column:1;background:red'></div>
        <div style='grid-row:2;grid-column:1;background:blue'></div>
        <div style='grid-row:3;grid-column:1;background:lime'></div>
      </div>
      """)

    for rgb <- [{1, 0, 0}, {0, 0, 1}, {0, 1, 0}],
        do: assert_in_delta(color(boxes, rgb).height, 30, 0.001)
  end

  test "an indefinite flexible axis preserves a common fraction after minimum constraints" do
    for spanning <- ["", "<div style='grid-row:1 / 3;height:150pt'></div>"] do
      boxes =
        layout("""
        <div style='display:grid;width:100pt;grid-template-columns:100pt;grid-template-rows:minmax(100pt,1fr) 1fr'>
          #{spanning}
          <div style='grid-row:1;grid-column:1;background:red'></div>
          <div style='grid-row:2;grid-column:1;background:blue'></div>
        </div>
        """)

      for rgb <- [{1, 0, 0}, {0, 0, 1}], do: assert_in_delta(color(boxes, rgb).height, 100, 0.001)
    end
  end

  defp color(boxes, rgb), do: Enum.find(boxes, &(Map.get(&1, :fill_color) == rgb))

  defp layout(source) do
    source =
      "<style>html,body{margin:0;font-family:'DejaVu Sans';font-size:12pt;line-height:12pt}</style>" <>
        source

    assert {:ok, dom} = HtmlParser.parse(source)
    assert {:ok, styled} = Style.compute(dom, system_font_discovery: false)
    assert {:ok, layout} = Layout.layout(styled, page_size: {300, 300}, margin: 0)
    layout.boxes
  end
end
