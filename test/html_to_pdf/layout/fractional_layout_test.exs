defmodule NativeElixirPdfUtilities.HtmlToPdf.FractionalLayoutTest do
  use ExUnit.Case, async: true
  alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, Layout, Style}

  test "grid factors below one leave unused space in either definite axis" do
    for {tracks, expected} <- [
          {"0.25fr 0.25fr", [50, 50]},
          {"0.5fr 0.5fr", [100, 100]},
          {"1fr 3fr", [50, 150]},
          {"minmax(80pt,0.25fr) 0.25fr", [80, 30]},
          {"0fr 0.25fr", [0, 50]}
        ],
        axis <- [:row, :column] do
      property = if axis == :column, do: "columns", else: "rows"

      boxes =
        layout("""
        <div style='display:grid;width:200pt;height:200pt;grid-template-#{property}:#{tracks}'>
          <div style='grid-#{axis}:1;background:red'></div>
          <div style='grid-#{axis}:2;background:blue'></div>
        </div>
        """)

      dimension = if axis == :column, do: :width, else: :height
      # A zero-sized background has no paint box.
      actual = Enum.map(boxes, &Map.fetch!(&1, dimension))
      assert length(actual) == Enum.count(expected, &(&1 > 0))

      for {size, expected} <- Enum.zip(actual, Enum.filter(expected, &(&1 > 0))),
          do: assert_in_delta(size, expected, 0.001)
    end
  end

  test "flex growth caps partial factors against initial space through min and max freezing" do
    cases = [
      {"flex:0.25 1 0", "flex:0.25 1 0", [50, 50]},
      {"flex:0.5 1 0", "flex:0.5 1 0", [100, 100]},
      {"flex:1 1 0", "flex:3 1 0", [50, 150]},
      {"flex:0.25 1 0;max-width:20pt", "flex:0.25 1 0", [20, 50]},
      {"flex:0.25 1 0;min-width:80pt", "flex:0.25 1 0", [80, 50]},
      {"flex:0.25 1 0;min-width:80pt", "flex:0.25 1 0;max-width:20pt", [80, 20]},
      {"flex:0.25 1 0;min-width:120pt", "flex:1 1 0;max-width:20pt", [120, 20]},
      {"flex:0.25 1 100pt;max-width:50pt", "flex:0.25 1 0", [50, 37.5]},
      {"flex:0 1 40pt", "flex:0 1 40pt", [40, 40]}
    ]

    for {first, second, expected} <- cases, direction <- ["row", "column"] do
      {first, second} =
        if direction == "column",
          do:
            {String.replace(first, "width", "height"), String.replace(second, "width", "height")},
          else: {first, second}

      assert_flex(first, second, expected, 200, direction)
    end
  end

  test "partial shrink uses unscaled factors for the cap and inner bases for distribution" do
    for {first, second, width, expected} <- [
          {"flex:0 0.25 100pt", "flex:0 0.25 100pt", 100, [75, 75]},
          {"flex:0 0.25 50pt", "flex:0 0.25 150pt", 100, [37.5, 112.5]},
          {"flex:0 0.25 100pt;min-width:90pt", "flex:0 0.25 100pt", 100, [90, 75]},
          {"flex:0 0.25 0;min-width:100pt", "flex:0 0.25 200pt", 200, [100, 175]},
          {"flex:0 0.25 40pt;padding:0 10pt", "flex:0 0.25 80pt", 100, [160 / 3, 200 / 3]},
          {"flex:0 1 0;padding:0 10pt", "flex:0 1 0;padding:0 10pt", 20, [20, 20]}
        ] do
      assert_flex(first, second, expected, width, "row")
    end
  end

  defp assert_flex(first, second, expected, extent, direction) do
    boxes =
      layout("""
      <div style='display:flex;flex-direction:#{direction};width:#{extent}pt;height:#{extent}pt;align-items:stretch'>
        <div style='#{first};background:red'></div><div style='#{second};background:blue'></div>
      </div>
      """)

    dimension = if direction == "row", do: :width, else: :height
    assert length(boxes) == 2

    for {box, width} <- Enum.zip(boxes, expected),
        do: assert_in_delta(Map.fetch!(box, dimension), width, 0.001)
  end

  defp layout(source) do
    assert {:ok, dom} = HtmlParser.parse(source)
    assert {:ok, styled} = Style.compute(dom)
    assert {:ok, layout} = Layout.layout(styled, page_size: {400, 400}, margin: 0)
    Enum.filter(layout.boxes, &(&1.type == :rect))
  end
end
