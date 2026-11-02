defmodule NativeElixirPdfUtilities.HtmlToPdf.GridAutoStretchTest do
  use ExUnit.Case, async: true
  alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, Layout, Style}

  test "default normal and explicit stretch fill the available column space" do
    for alignment <- ["", "justify-content:normal", "justify-content:stretch"] do
      [first, second] = boxes("auto auto", alignment)
      assert_in_delta first.width, 100, 0.001
      assert_in_delta second.width, 100, 0.001
      assert_in_delta second.x, 100, 0.001
    end
  end

  test "nonstretch alignment retains intrinsic columns and distributes their positions" do
    for {alignment, first_x, second_x} <- [
          {"start", 0, 20},
          {"center", 80, 100},
          {"end", 160, 180},
          {"space-between", 0, 180}
        ] do
      [first, second] = boxes("auto auto", "justify-content:#{alignment}")
      assert_in_delta first.width, 20, 0.001
      assert_in_delta second.width, 20, 0.001
      assert_in_delta first.x, first_x, 0.001
      assert_in_delta second.x, second_x, 0.001
    end
  end

  test "stretch reserves gutters and only expands tracks with an automatic maximum" do
    for {tracks, css, expected} <- [
          {"auto auto", "column-gap:20pt", [90, 90]},
          {"40pt auto", "", [40, 160]},
          {"minmax(50pt, auto) auto", "", [115, 85]},
          {"minmax(auto, 40pt) auto", "", [40, 160]},
          {"40pt 1fr", "", [40, 160]},
          {"40pt 40pt", "", [40, 40]},
          {"auto auto", "width:30pt", [20, 20]}
        ] do
      actual = boxes(tracks, css)
      for {box, width} <- Enum.zip(actual, expected), do: assert_in_delta(box.width, width, 0.001)
    end
  end

  test "normal content alignment uses stretch in either grid axis without changing flex defaults" do
    for {display, expected} <- [{"grid", :stretch}, {"flex", :flex_start}] do
      assert {:ok, dom} = HtmlParser.parse("<div style='display:#{display}'></div>")
      assert {:ok, %{children: [%{style: style}]}} = Style.compute(dom)
      assert style.justify_content == expected
    end

    assert {:ok, dom} =
             HtmlParser.parse("""
             <div style='display:grid;width:100pt;height:100pt;grid-template-rows:minmax(20pt,auto) auto;align-content:normal'>
               <div style='height:auto;min-height:10pt;background:red'></div>
               <div style='height:auto;min-height:10pt;background:blue'></div>
             </div>
             """)

    assert {:ok, styled} = Style.compute(dom)
    assert {:ok, layout} = Layout.layout(styled, page_size: {300, 300}, margin: 0)
    [first, second] = Enum.filter(layout.boxes, &(&1.type == :rect))
    assert_in_delta first.height + second.height, 100, 0.001
  end

  test "a definite minimum height provides the free space for automatic row stretch" do
    for {alignment, expected} <- [{"normal", 50}, {"start", 0}] do
      assert {:ok, dom} =
               HtmlParser.parse("""
               <div style='display:grid;width:100pt;min-height:100pt;grid-template-rows:auto auto;align-content:#{alignment}'>
                 <div style='background:red'></div><div style='background:blue'></div>
               </div><div style='height:10pt;background:lime'></div>
               """)

      assert {:ok, styled} = Style.compute(dom)
      assert {:ok, layout} = Layout.layout(styled, page_size: {300, 300}, margin: 0)
      reds = Enum.filter(layout.boxes, &(Map.get(&1, :fill_color) == {1, 0, 0}))
      assert Enum.sum(Enum.map(reds, & &1.height)) == expected
      following = Enum.find(layout.boxes, &(Map.get(&1, :fill_color) == {0, 1, 0}))
      assert_in_delta following.y + following.height, 200, 0.001
    end
  end

  defp boxes(tracks, extra) do
    source = """
    <div style='display:grid;width:200pt;grid-template-columns:#{tracks};#{extra}'>
      <div style='min-width:20pt;height:10pt;background:red'></div>
      <div style='min-width:20pt;height:10pt;background:blue'></div>
    </div>
    """

    assert {:ok, dom} = HtmlParser.parse(source)
    assert {:ok, styled} = Style.compute(dom)
    assert {:ok, layout} = Layout.layout(styled, page_size: {300, 300}, margin: 0)
    Enum.filter(layout.boxes, &(&1.type == :rect))
  end
end
