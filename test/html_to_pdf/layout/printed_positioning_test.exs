defmodule NativeElixirPdfUtilities.HtmlToPdf.PrintedPositioningTest do
  use ExUnit.Case, async: true
  alias NativeElixirPdfUtilities.{HtmlToPdf, Text}
  alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, Layout, Style, StickyGeometry}

  # CSS Positioned Layout 3, 7 October 2025, sections 3.4 and 3.5.
  test "printed sticky placement uses the initial scrollport and preserves subsequent flow" do
    html = """
    <style>@page{size:200pt 200pt;margin:0}html,body{margin:0;font:10pt 'DejaVu Sans';line-height:12pt}.sticky{position:sticky;top:20pt;background:red;height:24pt}</style><body><div style='height:600pt'><div class=sticky>First</div><div style='height:190pt'>Flow</div><div class=sticky>Middle</div><div style='height:190pt'></div><div class=sticky>Last</div></div><div style='overflow:hidden;height:100pt'><div class=sticky>Nested</div></div></body>
    """

    assert {:ok, pdf} = HtmlToPdf.render(html, system_font_discovery: false)
    assert {:ok, %{pages: pages}} = Text.extract_spans(pdf)
    assert length(pages) == 4

    for {label, page_index, top} <- [
          {"First", 0, 20},
          {"Middle", 1, 14},
          {"Last", 2, 28},
          {"Nested", 3, 20}
        ] do
      words = Enum.flat_map(pages, &words/1)
      assert Enum.count(words, &(&1.text == label)) == 1
      span = Enum.find(words(Enum.at(pages, page_index)), &(&1.text == label))
      first = Enum.find(words(hd(pages)), &(&1.text == "First"))
      assert_in_delta span.y - first.y, top - 20, 0.75
    end
  end

  test "sticky view rectangles constrain both axes and reduce the end inset for oversized boxes" do
    port = %{x: 0, y: 0, width: 100, height: 100}
    cb = %{x: 0, y: -100, width: 200, height: 300}
    box = %{x: 0, y: 80, width: 20, height: 20}
    style = %{offsets: %{left: 10, right: :auto, top: 15, bottom: :auto}}
    assert StickyGeometry.offset(style, box, cb, port) == {10, -15}

    assert StickyGeometry.offset(
             style,
             %{box | height: 200, y: -100},
             %{cb | y: -200, height: 400},
             port
           ) == {10, -15}

    assert StickyGeometry.offset(style, box, %{cb | width: 20}, port) == {0, -15}
    style = %{offsets: %{left: :auto, right: 10, top: :auto, bottom: 15}}
    assert StickyGeometry.offset(style, %{box | x: 90, y: 0}, cb, port) == {-20, 15}
  end

  test "explicit auto opposing insets stretch while over-constrained RTL geometry uses its owning direction" do
    for {direction, position, expected} <- [
          {"ltr", "absolute", 10},
          {"rtl", "absolute", 60},
          {"ltr", "relative", 10},
          {"rtl", "relative", 60}
        ] do
      html =
        "<div style='position:relative;direction:#{direction};width:100pt;height:80pt'><div style='position:#{position};left:10pt;right:20pt;width:20pt;height:10pt;background:red'></div></div>"

      red = layout(html) |> Enum.find(&(Map.get(&1, :fill_color) == {1, 0, 0}))
      assert red.x == expected
    end

    html =
      "<div style='position:relative;width:100pt;height:80pt'><div style='position:absolute;inset:10pt;width:auto;height:auto;background:red'></div></div>"

    red = layout(html) |> Enum.find(&(Map.get(&1, :fill_color) == {1, 0, 0}))
    assert {red.x, red.width, red.height} == {10.0, 80.0, 60.0}
  end

  test "ordinary fixed artwork repeats on first middle and last pages without reserving normal flow" do
    html =
      "<style>@page{size:200pt 100pt;margin:0}body{margin:0;font:10pt 'DejaVu Sans';line-height:12pt}</style><div style='position:fixed;top:5pt;left:10pt'>Fixed</div><div style='height:250pt'>Flow</div>"

    assert {:ok, pdf} = HtmlToPdf.render(html, system_font_discovery: false)
    assert {:ok, %{pages: pages}} = Text.extract_spans(pdf)
    assert length(pages) == 3

    for page <- pages do
      assert [%{x: x, y: y}] = Enum.filter(words(page), &(&1.text == "Fixed"))
      assert_in_delta x, 10, 0.1
      first = Enum.find(words(hd(pages)), &(&1.text == "Fixed"))
      assert_in_delta y, first.y, 0.1
    end
  end

  test "a transformed or contained ancestor owns fixed artwork without page repetition" do
    for owner <- ["transform:translateX(10pt)", "contain:layout", "contain:paint"] do
      html =
        "<style>@page{size:200pt 100pt;margin:0}body{margin:0;font:10pt 'DejaVu Sans';line-height:12pt}</style><div style='height:100pt'>First</div><div style='#{owner};height:100pt'><div style='position:fixed;top:5pt;left:10pt'>Local</div>Middle</div><div style='height:100pt'>Last</div>"

      assert {:ok, pdf} = HtmlToPdf.render(html, system_font_discovery: false)
      assert {:ok, %{pages: pages}} = Text.extract_spans(pdf)
      assert length(pages) == 3
      assert Enum.count(Enum.flat_map(pages, &words/1), &(&1.text == "Local")) == 1
      assert Enum.any?(words(Enum.at(pages, 1)), &(&1.text == "Local"))
    end
  end

  test "nested sticky translations carry their containing blocks and authored transforms" do
    for inner <- [
          "position:sticky;top:10pt",
          "position:sticky;top:10pt;transform:translateX(5pt)"
        ] do
      html =
        "<div style='height:300pt'><div style='position:sticky;top:20pt;height:80pt'><div style='#{inner};height:15pt'>Nested</div><div style='position:sticky;top:25pt'>Sibling</div></div></div>"

      assert {:ok, pdf} =
               HtmlToPdf.render(html,
                 page_size: {200, 200},
                 margin: 0,
                 system_font_discovery: false
               )

      assert {:ok, text} = Text.extract(pdf)
      assert text =~ "Nested" and text =~ "Sibling"
    end
  end

  test "inline sticky placement moves artwork while its following text retains normal flow" do
    html =
      "<body style='margin:0;font:10pt sans-serif;line-height:12pt'><div style='height:100pt'><span style='position:sticky;top:20pt'>Sticky</span> Following</div></body>"

    assert {:ok, pdf} =
             HtmlToPdf.render(html,
               page_size: {200, 200},
               margin: 0,
               system_font_discovery: false
             )

    assert {:ok, %{pages: [page]}} = Text.extract_spans(pdf)
    sticky = Enum.find(words(page), &(&1.text == "Sticky"))
    following = Enum.find(words(page), &(String.trim(&1.text) == "Following"))
    assert_in_delta sticky.y - following.y, 20, 0.75
  end

  test "table and list containing blocks export their actual used padding edges" do
    child =
      "<div style='position:absolute;left:5pt;top:5pt;width:10pt;height:10pt;background:red'></div>"

    for container <- [
          "<table style='position:relative;width:80pt;border-spacing:0'><tr><td style='padding:0;height:30pt'>#{child}</td></tr></table>",
          "<div style='position:relative;display:list-item;width:80pt;height:30pt'>#{child}</div>",
          "<ul style='margin:0;padding:0'><li style='position:relative;width:80pt;height:30pt'>#{child}</li></ul>"
        ] do
      red = layout(container) |> Enum.find(&(Map.get(&1, :fill_color) == {1, 0, 0}))
      assert_in_delta red.x, 5, 0.75
      assert_in_delta red.y + red.height, 195, 0.75
    end

    assert {:ok, _pdf} =
             HtmlToPdf.render(
               "<table style='position:absolute;display:inline-table;width:80pt;left:10pt;top:10pt'><tr><td>Cell</td></tr></table>",
               system_font_discovery: false
             )
  end

  defp words(page) do
    Enum.reduce(page.spans, [], fn span, words ->
      case words do
        [previous | rest] when span.joins_previous? ->
          [%{previous | text: previous.text <> span.text} | rest]

        _ ->
          [span | words]
      end
    end)
    |> Enum.reverse()
  end

  defp layout(html) do
    assert {:ok, dom} = HtmlParser.parse_detailed(html)
    assert {:ok, styled} = Style.compute_detailed(dom, system_font_discovery: false)
    assert {:ok, result} = Layout.layout(styled, page_size: {200, 200}, margin: 0)
    result.boxes
  end

  defp physical_rectangles(boxes, composition \\ nil) do
    alias NativeElixirPdfUtilities.HtmlToPdf.{DrawingGeometry, ImageMapGeometry}

    Enum.flat_map(boxes, fn box ->
      case box do
        %{type: :drawing_group, boxes: children} ->
          physical_rectangles(children, DrawingGeometry.compose_group(box, composition))

        %{type: :rect} ->
          if composition == nil do
            [box]
          else
            {matrix, _, _} = DrawingGeometry.composition_parts(composition)

            {:polygon, points} =
              DrawingGeometry.transform_clip(
                Map.take(box, [:x, :y, :width, :height]),
                DrawingGeometry.affine_matrix(matrix)
              )

            {left, bottom, right, top} = ImageMapGeometry.bounds([points])
            [%{box | x: left, y: bottom, width: right - left, height: top - bottom}]
          end

        _ ->
          []
      end
    end)
  end

  test "fixed controls stay on their first printed page and child viewports keep their own fixed origin" do
    alias NativeElixirPdfUtilities.Forms

    html =
      "<input name='fixed' value='Field' style='position:fixed;top:10pt;left:10pt'><div style='height:190pt'>Flow</div><p>Next page</p>"

    assert {:ok, pdf} =
             HtmlToPdf.render(html,
               page_size: {200, 200},
               margin: 0,
               system_font_discovery: false
             )

    assert {:ok, 2} = NativeElixirPdfUtilities.Info.page_count(pdf)
    assert {:ok, [field]} = Forms.fields(pdf)
    assert field.name == "fixed"
    assert length(:binary.matches(pdf, "/Subtype /Widget")) == 1

    assert {:ok, child} =
             HtmlToPdf.render(
               "<iframe style='width:100pt;height:40pt' srcdoc=\"<input name='child' style='position:fixed;top:0'><p>Child</p>\"></iframe>",
               system_font_discovery: false
             )

    assert {:ok, [field]} = Forms.fields(child)
    assert field.name == "CHILD_1_child"
    assert StickyGeometry.attach([], %{position: :static}, %{}) == []
  end

  test "unsupported positioned descendants return the ordinary layout failure" do
    assert {:ok, dom} =
             HtmlParser.parse_detailed(
               "<div style='position:relative'><div style='position:absolute'>Child</div></div>"
             )

    assert {:ok, tree} = Style.compute_detailed(dom, system_font_discovery: false)
    [parent] = tree.children
    [child] = parent.children
    child = %{child | style: Map.put(child.style, :display, :list)}
    tree = %{tree | children: [%{parent | children: [child]}]}
    assert {:error, :invalid_layout} = Layout.layout(tree)
  end

  test "relative over-constraints use the containing block axes through child direction overrides" do
    for {axes, expected_x, expected_y} <- [
          {"direction:rtl", -20, -10},
          {"writing-mode:vertical-rl", -20, -10},
          {"writing-mode:vertical-rl;direction:rtl", -20, 30},
          {"writing-mode:sideways-lr", 10, 30}
        ] do
      boxes =
        layout(
          "<div style='#{axes}'><div style='position:relative;direction:ltr;writing-mode:horizontal-tb;left:10pt;right:20pt;top:10pt;bottom:30pt;width:20pt;height:20pt;background:red'></div></div>"
        )
        |> physical_rectangles()

      red = Enum.find(boxes, &(Map.get(&1, :fill_color) == {1, 0, 0}))
      # Horizontal RTL fills the parent's width. An orthogonal vertical auto
      # block instead has the child's 20pt physical width, as Chromium confirms.
      natural_x = if axes == "direction:rtl", do: 180, else: 0

      assert_in_delta red.x - natural_x, expected_x, 0.01
      assert_in_delta red.y + red.height - 200, expected_y, 0.01
    end
  end

  test "absolute over-constraints select the containing block start side in both physical axes" do
    for {axes, x, top} <- [
          {"direction:rtl", 60, 10},
          {"writing-mode:vertical-rl", 60, 10},
          {"writing-mode:vertical-rl;direction:rtl", 60, 50},
          {"writing-mode:sideways-lr", 10, 50}
        ] do
      red =
        layout(
          "<div style='position:relative;width:100pt;height:100pt;#{axes}'><div style='position:absolute;direction:ltr;writing-mode:horizontal-tb;left:10pt;right:20pt;top:10pt;bottom:30pt;width:20pt;height:20pt;background:red'></div></div>"
        )
        |> physical_rectangles()
        |> Enum.find(&(Map.get(&1, :fill_color) == {1, 0, 0}))

      assert_in_delta red.x, x, 0.01
      assert_in_delta 200 - red.y - red.height, top, 0.01
    end
  end

  test "relative inline descendants retain the containing block direction across inline overrides" do
    x = fn offsets ->
      boxes =
        layout(
          "<div style='direction:rtl'><span style='direction:ltr'><span style='position:relative;#{offsets}'>Moved</span></span></div>"
        )

      Enum.find(boxes, &(Map.get(&1, :text) == "Moved")).x
    end

    assert_in_delta x.("left:10pt;right:20pt") - x.("left:0;right:auto"), -20, 0.01
  end
end
