defmodule NativeElixirPdfUtilities.HtmlToPdf.ColumnsTest do
  use ExUnit.Case, async: true
  alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, Layout, Pagination, Style}
  alias NativeElixirPdfUtilities.Validators.CssColumnsValidator

  # CSS Multicolumn Layout 2, 19 December 2024, sections 3–8.
  # Expected coordinates are calculated from the content width and source order.
  test "count and preferred width select equal columns and retain logical order" do
    for {columns, expected_width, second_x} <- [
          {"2", 45, 55},
          {"25pt", 80 / 3, 110 / 3},
          {"30pt 2", 45, 55}
        ] do
      boxes =
        layout("<div style='columns:#{columns};column-gap:10pt'>#{blocks(6)}</div>").boxes
        |> painted()

      assert length(boxes) == 6
      assert_in_delta hd(boxes).width, expected_width, 0.02
      assert_in_delta Enum.at(boxes, if(columns == "25pt", do: 2, else: 3)).x, second_x, 0.02
      assert Enum.map(boxes, & &1.fill_color) == colors(6)
    end
  end

  test "automatic fill uses definite height and overflows in additional columns" do
    boxes =
      layout(
        "<div style='columns:2;column-gap:10pt;height:40pt;column-fill:auto'>#{blocks(6)}</div>"
      ).boxes
      |> painted()

    assert Enum.map(boxes, & &1.x) == [0, 0, 55, 55, 110, 110]
    assert Enum.map(boxes, & &1.y) == [280, 260, 280, 260, 280, 260]
  end

  test "spanners balance the preceding section and restart the column set" do
    boxes =
      layout("""
      <div style='columns:2;column-gap:10pt'>
        #{blocks(2)}
        <div style='column-span:all;height:10pt;background:black'></div>
        #{blocks(2)}
      </div>
      """).boxes
      |> painted()

    assert Enum.map(boxes, & &1.width) == [45, 45, 100, 45, 45]
    assert_in_delta Enum.at(boxes, 2).y, 270, 0.02
    assert_in_delta Enum.at(boxes, 3).y, 250, 0.03
  end

  test "explicit column breaks and right-to-left columns preserve source order" do
    boxes =
      layout("""
      <div style='columns:2;column-gap:10pt;direction:rtl'>
        <div style='height:20pt;background:red'></div>
        <div style='height:20pt;break-before:column;background:blue'></div>
      </div>
      """).boxes
      |> painted()

    assert Enum.map(boxes, & &1.x) == [55, 0]
    assert_in_delta hd(boxes).y, 280, 0.02
    assert_in_delta List.last(boxes).y, 280, 0.02
  end

  test "column rules occupy the gutter and use the shared border painter" do
    boxes =
      layout(
        "<div style='columns:2;column-gap:10pt;column-rule:2pt solid black'>#{blocks(2)}</div>"
      ).boxes

    assert Enum.any?(boxes, fn box ->
             Map.get(box, :stroke_color) == {0, 0, 0} and abs(box.x + box.width / 2 - 50) < 0.02
           end)
  end

  test "normal gaps use one em while explicit zero remains zero" do
    for {gap, x} <- [{"normal", 56}, {"0", 50}] do
      boxes =
        layout("<div style='columns:2;font-size:12pt;column-gap:#{gap}'>#{blocks(2)}</div>").boxes
        |> painted()

      assert_in_delta List.last(boxes).x, x, 0.02
    end
  end

  test "column syntax and typed failures retain the diagnostic contract" do
    assert {:ok, %{column_count: 3, column_width: 20.0}} =
             CssColumnsValidator.prepare("columns", "20pt 3")

    for value <- ["-1", "2 3", "20pt 30pt", "", "auto auto auto", "20%"] do
      refute match?({:ok, _}, CssColumnsValidator.prepare("columns", value))
    end

    assert {:error, {:invalid_layout, %{stage: :layout, message: message}}} =
             CssColumnsValidator.validate_style(%{column_count: 0}, "test")

    assert message =~ "positive"
    assert {:error, {:invalid_layout, _}} = Pagination.fragment_columns([], 0, 20)
  end

  test "font-relative column widths and percentage gutters resolve at their declaring box" do
    boxes =
      layout("<div style='columns:2em;font-size:10pt;column-gap:10%'>#{blocks(6)}</div>").boxes
      |> painted()

    assert_in_delta hd(boxes).width, 80 / 3, 0.02
    assert_in_delta Enum.at(boxes, 2).x, 110 / 3, 0.02
    assert {:ok, %{column_width: zero}} = CssColumnsValidator.prepare("column-width", "0")
    assert zero == 0
  end

  test "balancing retains the minimum height of an indivisible inline item" do
    tree =
      layout(
        "<div style='columns:2;column-gap:10pt;font-size:10pt;line-height:12pt'>One</div><div style='height:10pt;background:red'></div>"
      )

    following = Enum.find(tree.boxes, &(Map.get(&1, :fill_color) == {1, 0, 0}))
    assert_in_delta following.y + following.height, 288, 0.02
  end

  test "empty columns consume no height and max-height constrains sequential columns" do
    assert layout("<div style='columns:2'></div>").boxes == []
    empty = layout("<div style='columns:2'><span style='display:none'>Hidden</span></div>")
    refute Enum.any?(empty.boxes, &(Map.get(&1, :text) == "Hidden"))

    boxes =
      layout(
        "<div style='columns:2;max-height:40pt;column-fill:auto;column-gap:10pt'>#{blocks(6)}</div>"
      ).boxes
      |> painted()

    assert Enum.map(boxes, & &1.x) == [0, 0, 55, 55, 110, 110]
  end

  test "column content failures retain the original diagnostic, including nested reflow" do
    alias NativeElixirPdfUtilities.HtmlToPdf.{ColumnLayout, Hyphenator}

    failure =
      NativeElixirPdfUtilities.Diagnostics.error(
        :layout,
        :invalid_layout,
        "column content failed"
      )

    rule = fn _, _, _, _ -> [] end

    assert ^failure =
             ColumnLayout.layout(
               [%{style: %{}}],
               %{column_count: 2},
               0,
               100,
               100,
               fn _, _, _, _, _ -> failure end,
               rule
             )

    key = make_ref()
    source = [%{type: :rect, x: 0, y: -10, width: 10, height: 10}]

    render = fn _, _, _, _, _ ->
      attempt = Process.get(key, 0)
      Process.put(key, attempt + 1)
      if attempt == 0, do: {:ok, source, 10}, else: failure
    end

    assert ^failure =
             NativeElixirPdfUtilities.Validators.HtmlValidator.with_render_budget(fn ->
               Hyphenator.with_fragmentainer(100, fn ->
                 ColumnLayout.layout(
                   [%{style: %{column_count: 2}}],
                   %{column_count: 2},
                   0,
                   100,
                   100,
                   render,
                   rule
                 )
               end)
             end)

    Process.delete(key)
  end

  test "a forced column break advances a sequential set even with ample remaining height" do
    boxes =
      layout(
        "<div style='columns:2;height:100pt;column-fill:auto;column-gap:10pt'><div style='height:10pt;background:red'></div><div style='break-before:column;height:10pt;background:blue'></div></div>"
      ).boxes
      |> painted()

    assert Enum.map(boxes, & &1.x) == [0, 55]
  end

  test "nested spanners escape normal ancestors and preserve their surrounding source order" do
    for decoration <- [
          "",
          "padding:2pt;border:0.75pt solid black"
        ] do
      boxes =
        layout(
          "<div style='columns:2;column-gap:10pt'><section style='#{decoration}'><section>#{blocks(2)}<div style='column-span:all;height:10pt;background:lime'></div>#{blocks(2)}</section></section></div>"
        ).boxes
        |> painted()

      assert Enum.map(boxes, & &1.fill_color) == [
               {1, 0, 0},
               {0, 0, 1},
               {0, 1, 0},
               {1, 0, 0},
               {0, 0, 1}
             ]

      assert Enum.at(boxes, 2).width == 100
    end
  end

  test "a formatting context between a spanner and its columns keeps the span local" do
    tree =
      layout(
        "<div style='columns:2;column-gap:10pt'><section style='overflow:hidden'><div style='column-span:all;height:10pt;background:lime'></div></section></div>"
      )

    boxes = painted(tree.boxes)
    assert Enum.all?(boxes, &(&1.width == 45))
    assert_in_delta Enum.sum(Enum.map(boxes, & &1.height)), 10, 0.02
  end

  test "spanners contain their own floats before the next column set" do
    boxes =
      layout(
        "<div style='columns:2;column-gap:10pt'><div style='column-span:all'><div style='float:left;height:20pt;width:20pt;background:red'></div></div><div style='height:10pt;background:blue'></div></div>"
      ).boxes
      |> painted()

    float = Enum.find(boxes, &(&1.fill_color == {1, 0, 0}))

    for after_span <- Enum.filter(boxes, &(&1.fill_color == {0, 0, 1})),
        do: assert_in_delta(after_span.y + after_span.height, float.y, 0.02)
  end

  test "leading and trailing nested spanners retain empty ancestor edge fragments and one link target" do
    for position <- [:first, :last] do
      span = "<div style='column-span:all;height:10pt;background:lime'>Span</div>"
      normal = "<div style='height:10pt;background:red'>Normal</div>"
      content = if position == :first, do: span <> normal, else: normal <> span

      tree =
        layout(
          "<div style='columns:2'><section id='section' style='padding:2pt'>#{content}</section></div><a href='#section'>Jump</a>"
        )

      assert Enum.any?(tree.boxes, &(Map.get(&1, :fill_color) == {0, 1, 0} and &1.width == 100))
      assert {:ok, pages} = Pagination.paginate(tree)
      assert {:ok, _} = NativeElixirPdfUtilities.HtmlToPdf.PdfWriter.render(pages)
    end
  end

  test "border-box maximum height leaves only its content height available for columns" do
    boxes =
      layout(
        "<div style='columns:2;column-fill:auto;column-gap:10pt;box-sizing:border-box;padding:5pt;max-height:40pt'>#{blocks(2)}</div>"
      ).boxes
      |> painted()

    assert Enum.map(boxes, & &1.x) == [5, 55]
    assert_in_delta hd(boxes).y, List.last(boxes).y, 0.02
  end

  defp layout(html) do
    {:ok, dom} = HtmlParser.parse("<div style='width:100pt'>" <> html <> "</div>")
    {:ok, styled} = Style.compute(dom, system_font_discovery: false)
    {:ok, tree} = Layout.layout(styled, page_size: {300, 300}, margin: 0)
    tree
  end

  defp blocks(count) do
    Enum.map_join(Enum.take(Stream.cycle(["red", "blue", "lime"]), count), fn color ->
      "<div style='height:20pt;break-inside:avoid;background:#{color}'></div>"
    end)
  end

  defp colors(count), do: Enum.take(Stream.cycle([{1, 0, 0}, {0, 0, 1}, {0, 1, 0}]), count)

  defp painted(boxes),
    do: Enum.filter(boxes, &(Map.get(&1, :fill_color) in (colors(3) ++ [{0, 0, 0}])))
end
