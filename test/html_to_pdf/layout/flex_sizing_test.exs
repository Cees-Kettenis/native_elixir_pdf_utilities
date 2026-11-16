defmodule NativeElixirPdfUtilities.HtmlToPdf.FlexSizingTest do
  use ExUnit.Case, async: true

  alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, Layout, Style}

  # CSS Flexbox 1, 14 October 2025, sections 4.5 and 9.2 through 9.7.
  # Expectations below follow the specified size and free-space equations.
  test "automatic minimum retains an unbreakable child's width unless explicitly reset or scrollable" do
    for {minimum, expected} <- [{"", 80}, {"min-width:0", 50}, {"overflow:hidden", 50}] do
      [first, second] =
        boxes("""
        <div style='display:flex;width:100pt'>
          <div style='flex:1 1 80pt;#{minimum};background:red'><span style='display:inline-block;width:80pt;height:10pt'></span></div>
          <div style='flex:1 1 80pt;#{minimum};background:blue'><span style='display:inline-block;width:80pt;height:10pt'></span></div>
        </div>
        """)

      assert_in_delta first.width, expected, 0.75 / 64
      assert_in_delta second.width, expected, 0.75 / 64
    end
  end

  test "line collection uses clamped hypothetical sizes while growth uses unclamped bases" do
    [first, second] =
      boxes("""
      <div style='display:flex;flex-wrap:wrap;width:100pt'>
        <div style='flex:1 1 80pt;max-width:40pt;height:10pt;background:red'></div>
        <div style='flex:1 1 80pt;max-width:40pt;height:10pt;background:blue'></div>
      </div>
      """)

    assert first.y == second.y
    assert_in_delta first.width, 40, 0.75 / 64
    assert_in_delta second.x, 40, 0.75 / 64
    assert_in_delta second.width, 40, 0.75 / 64
  end

  test "percentage and calculated bases resolve on the main axis with border box sizing" do
    for {basis, width} <- [{"25%", 50}, {"calc(25% + 10pt)", 60}, {"60pt", 60}] do
      [item] =
        boxes("""
        <div style='display:flex;width:200pt'>
          <div style='flex:0 0 #{basis};box-sizing:border-box;padding:5pt;height:20pt;background:red'></div>
        </div>
        """)

      assert_in_delta item.width, width, 0.75 / 64
    end
  end

  test "content basis ignores preferred width and intrinsic bases preserve word opportunities" do
    for basis <- ["content", "min-content", "max-content", "fit-content(100pt)"] do
      [item] =
        boxes("""
        <div style='display:flex;width:200pt'>
          <div style='flex:0 0 #{basis};width:1pt;min-width:0;background:red'>word word</div>
        </div>
        """)

      assert item.width > 20
      assert item.width < 100
    end
  end

  test "reverse wrapping reverses the cross axis for every flex direction" do
    for direction <- ["row", "row-reverse", "column", "column-reverse"] do
      row? = String.starts_with?(direction, "row")
      {width, height} = if row?, do: {60, 20}, else: {20, 60}

      [first, second] =
        boxes("""
        <div style='display:flex;flex-direction:#{direction};flex-wrap:wrap-reverse;width:100pt;height:100pt;align-content:flex-start;align-items:flex-start;gap:10pt'>
          <div style='flex:none;width:#{width}pt;height:#{height}pt;background:red'></div>
          <div style='flex:none;width:#{width}pt;height:#{height}pt;background:blue'></div>
        </div>
        """)

      assert_in_delta if(row?, do: 300 - first.y - first.height, else: first.x), 80, 0.75 / 64
      assert_in_delta if(row?, do: 300 - second.y - second.height, else: second.x), 50, 0.75 / 64
    end
  end

  test "automatic minimum is capped by an explicit maximum rather than defeating it" do
    [item] =
      boxes("""
      <div style='display:flex;width:100pt'>
        <div style='max-width:40pt;background:red'><span style='display:inline-block;width:80pt;height:10pt'></span></div>
      </div>
      """)

    assert_in_delta item.width, 40, 0.75 / 64
  end

  test "adjacent text nodes form one anonymous flex or grid item" do
    for {display, expected} <- [{"flex", 22.046875}, {"grid", 30}] do
      {:ok, dom} =
        HtmlParser.parse(
          "<div style='display:#{display};font-family:FixtureMono;font-size:10pt;width:100pt;gap:10pt;grid-template-columns:20pt 20pt'>AB<div style='width:20pt;height:10pt;background:red'></div></div>"
        )

      {:ok, %{children: [container]} = styled} =
        Style.compute(dom,
          system_font_discovery: false,
          fonts: [
            %{
              family: "FixtureMono",
              path:
                NativeElixirPdfUtilities.TestSupport.Paths.project(
                  "priv/fonts/dejavu/DejaVuSansMono.ttf"
                )
            }
          ]
        )

      [text, block] = container.children
      container = %{container | children: [%{text | text: "A"}, %{text | text: "B"}, block]}

      {:ok, tree} =
        Layout.layout(%{styled | children: [container]}, page_size: {300, 300}, margin: 0)

      box = Enum.find(tree.boxes, &(Map.get(&1, :fill_color) == {1, 0, 0}))
      assert_in_delta box.x, expected, 0.001
    end
  end

  test "preferred aspect ratios transfer definite cross sizes into the automatic minimum" do
    for {extra, expected} <- [{"", 40}, {"width:30pt", 30}, {"min-width:0", 10}] do
      [item] =
        boxes(
          "<div style='display:flex;width:10pt'><div style='height:20pt;aspect-ratio:2;flex:1 1 auto;#{extra};background:red'>a</div></div>"
        )

      assert_in_delta item.width, expected, 0.02
    end
  end

  test "replaced flex items honor an explicit basis while retaining their automatic minimum" do
    png =
      NativeElixirPdfUtilities.TestSupport.PngFixture.build(1, 1, 6, 8, 0, fn _, _ ->
        [0, 0, 0, 255]
      end)

    uri = "data:image/png;base64," <> Base.encode64(png)

    for {minimum, expected} <- [{"", 20}, {"min-width:0", 10}] do
      {:ok, dom} =
        HtmlParser.parse(
          "<div style='display:flex;width:100pt;align-items:baseline'><img src='#{uri}' style='height:20pt;flex:0 0 10pt;#{minimum}'><span>Peer</span></div>"
        )

      {:ok, styled} = Style.compute(dom, system_font_discovery: false)
      {:ok, tree} = Layout.layout(styled, page_size: {300, 300}, margin: 0)
      image = Enum.find(tree.boxes, &(&1.type == :image))
      assert_in_delta image.width, expected, 0.02
      peer = Enum.find(tree.boxes, &(Map.get(&1, :text) == "Peer"))
      assert_in_delta image.y, peer.y, 0.02
    end
  end

  test "an image's automatic cross size follows its final flexed main size" do
    png =
      NativeElixirPdfUtilities.TestSupport.PngFixture.build(1, 1, 6, 8, 0, fn _, _ ->
        [0, 0, 0, 255]
      end)

    uri = "data:image/png;base64," <> Base.encode64(png)

    for direction <- ["row", "column"] do
      {:ok, dom} =
        HtmlParser.parse(
          "<div style='display:flex;flex-direction:#{direction};width:100pt;height:100pt;align-items:flex-start'><img src='#{uri}' style='flex:0 0 40pt;min-width:0;min-height:0'></div>"
        )

      {:ok, styled} = Style.compute(dom, system_font_discovery: false)
      {:ok, tree} = Layout.layout(styled, page_size: {300, 300}, margin: 0)
      image = Enum.find(tree.boxes, &(&1.type == :image))
      assert_in_delta image.width, 40, 0.02
      assert_in_delta image.height, 40, 0.02
    end
  end

  test "column automatic minima use text height independently of a zero flex basis" do
    for {minimum, expected} <- [{"", 12}, {"min-height:0", 5}] do
      items =
        boxes(
          "<div style='display:flex;flex-direction:column;width:100pt;height:10pt;font-size:10pt;line-height:12pt'><div style='flex:1 1 0;#{minimum};background:red'>A</div><div style='flex:1 1 0;#{minimum};background:blue'>B</div></div>"
        )

      assert length(items) == 2
      for item <- items, do: assert_in_delta(item.height, expected, 0.02)
    end
  end

  defp boxes(html) do
    assert {:ok, dom} = HtmlParser.parse(html)
    assert {:ok, styled} = Style.compute(dom, system_font_discovery: false)
    assert {:ok, layout} = Layout.layout(styled, page_size: {300, 300}, margin: 0)

    Enum.filter(
      layout.boxes,
      &(&1.type == :rect and Map.get(&1, :fill_color) in [{1, 0, 0}, {0, 0, 1}])
    )
  end
end
