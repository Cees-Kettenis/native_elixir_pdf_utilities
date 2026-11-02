defmodule NativeElixirPdfUtilities.HtmlToPdf.RelativePercentageOffsetsTest do
  use ExUnit.Case, async: true

  alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, Layout, Style}

  test "block offsets use the containing content height without changing sibling flow" do
    for {parent, offset, expected_top, flow_top} <- [
          {"height:200pt", "top:50%", 100, 20},
          {"height:200pt", "bottom:-25%", 50, 20},
          {"height:auto", "top:50%", 0, 20},
          {"height:auto", "top:50%;bottom:7pt", -7, 20},
          {"height:200pt;box-sizing:border-box;padding:18pt;border:6pt solid", "top:50%", 100, 44}
        ] do
      boxes =
        layout("""
        <div style='#{parent};width:200pt'>
          <div style='position:relative;#{offset};width:20pt;height:20pt;background:red'></div>
          <div style='width:20pt;height:20pt;background:blue'></div>
        </div>
        """)

      red = Enum.find(boxes, &(Map.get(&1, :fill_color) == {1, 0, 0}))
      blue = Enum.find(boxes, &(Map.get(&1, :fill_color) == {0, 0, 1}))
      assert_in_delta 300 - red.y - red.height, expected_top, 0.001
      assert_in_delta 300 - blue.y - blue.height, flow_top, 0.001
    end
  end

  test "an auto-height intermediary does not inherit its ancestor's percentage basis" do
    boxes =
      layout("""
      <div style='height:200pt'><div>
        <div style='position:relative;top:50%;width:20pt;height:20pt;background:red'></div>
      </div></div>
      """)

    red = Enum.find(boxes, &(Map.get(&1, :fill_color) == {1, 0, 0}))
    assert_in_delta 300 - red.y - red.height, 0, 0.001
  end

  test "inline atomic and interrupted inline offsets share the containing height" do
    for {tag, display, content} <- [
          {"span", "inline", "Inside"},
          {"span", "inline-block", "Inside"},
          {"span", "inline", "Start<span style='display:block'>Block</span>End"},
          {"ruby", "inline", "Base<rt>Reading</rt>"}
        ] do
      source = fn top ->
        "<div style='height:200pt'>" <>
          "<#{tag} style='display:#{display};position:relative;top:#{top};color:red'>#{content}</#{tag}>" <>
          "<span style='color:blue'>Following</span></div>"
      end

      before = layout(source.("auto")) |> Enum.filter(&(&1.type == :text))
      after_boxes = layout(source.("25%")) |> Enum.filter(&(&1.type == :text))
      assert Enum.map(before, & &1.text) == Enum.map(after_boxes, & &1.text)
      assert Enum.any?(before, &(&1.color == {1, 0, 0}))

      for {original, moved} <- Enum.zip(before, after_boxes) do
        shift = if original.color == {1, 0, 0}, do: 50, else: 0

        assert_in_delta moved.y,
                        original.y - shift,
                        0.001,
                        "#{display}: #{content}: #{original.text}"

        assert_in_delta moved.x, original.x, 0.001
      end
    end
  end

  test "inline image percentage offsets retain the parent height" do
    png =
      NativeElixirPdfUtilities.TestSupport.PngFixture.build(1, 1, 2, 8, 0, fn _, _ ->
        [255, 0, 0]
      end)

    image = "data:image/png;base64," <> Base.encode64(png)

    source = fn top ->
      "<div style='height:200pt'><img src='#{image}' style='position:relative;top:#{top};width:20pt;height:20pt'>Following</div>"
    end

    before = layout(source.("auto"))
    after_boxes = layout(source.("25%"))
    original = Enum.find(before, &(&1.type == :image))
    moved = Enum.find(after_boxes, &(&1.type == :image))
    assert_in_delta moved.y, original.y - 50, 0.001

    assert Enum.filter(before, &(&1.type == :text)) ==
             Enum.filter(after_boxes, &(&1.type == :text))
  end

  defp layout(source) do
    source =
      "<style>body{margin:0;font-size:12pt;font-family:'DejaVu Sans';line-height:15pt}</style>" <>
        source

    assert {:ok, dom} = HtmlParser.parse_detailed(source)
    assert {:ok, styled} = Style.compute_detailed(dom, system_font_discovery: false)
    assert {:ok, layout} = Layout.layout(styled, page_size: {300, 300}, margin: 0)
    layout.boxes
  end
end
