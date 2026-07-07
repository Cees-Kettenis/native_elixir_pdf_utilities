defmodule NativeElixirPdfUtilities.HtmlToPdf.PaginationTest do
  use ExUnit.Case

  alias NativeElixirPdfUtilities.HtmlToPdf.HtmlParser
  alias NativeElixirPdfUtilities.HtmlToPdf.Layout
  alias NativeElixirPdfUtilities.HtmlToPdf.Pagination
  alias NativeElixirPdfUtilities.HtmlToPdf.Style

  test "paginate keeps a fitting layout tree on one page" do
    layout_tree = %{
      type: :layout,
      page_size: {200.0, 100.0},
      margin: 10.0,
      boxes: [text_box("Hello", 78.0, {:block, 1})]
    }

    assert Pagination.paginate(layout_tree, []) ==
             {:ok, [%{size: {200.0, 100.0}, boxes: [text_box("Hello", 78.0, {:block, 1})]}]}
  end

  test "paginate creates automatic page breaks at the bottom margin" do
    boxes =
      [
        text_box("One", 78.0, {:block, 1}),
        text_box("Two", 51.6, {:block, 2}),
        text_box("Three", 25.2, {:block, 3}),
        text_box("Four", -1.2, {:block, 4})
      ]

    layout_tree = %{type: :layout, page_size: {200.0, 100.0}, margin: 10.0, boxes: boxes}

    assert {:ok, [first_page, second_page]} = Pagination.paginate(layout_tree, [])
    assert Enum.map(first_page.boxes, & &1.text) == ["One", "Two", "Three"]
    assert Enum.map(second_page.boxes, & &1.text) == ["Four"]
    [four] = second_page.boxes
    assert_in_delta four.y, 78.0, 0.0001
  end

  test "paginate honors manual page breaks" do
    boxes = [
      text_box("Before", 78.0, {:block, 1}),
      text_box("After", 51.6, {:block, 2}, %{break_before: :page})
    ]

    layout_tree = %{type: :layout, page_size: {200.0, 100.0}, margin: 10.0, boxes: boxes}

    assert {:ok, [first_page, second_page]} = Pagination.paginate(layout_tree, [])
    assert Enum.map(first_page.boxes, & &1.text) == ["Before"]
    assert Enum.map(second_page.boxes, & &1.text) == ["After"]
    [after_break] = second_page.boxes
    assert_in_delta after_break.y, 78.0, 0.0001
  end

  test "paginate repeats table headers when body rows continue on a new page" do
    rows =
      1..3
      |> Enum.map(fn index ->
        "<tr><td>Alpha #{index}</td><td>#{index}</td></tr>"
      end)
      |> Enum.join()

    html =
      "<table><thead><tr><th>Name</th><th>Count</th></tr></thead><tbody>" <>
        rows <> "</tbody></table>"

    assert {:ok, dom} = HtmlParser.parse(html)
    assert {:ok, styled_tree} = Style.compute(dom, [])
    assert {:ok, layout_tree} = Layout.layout(styled_tree, page_size: {200, 100}, margin: 10)
    assert {:ok, [first_page, second_page]} = Pagination.paginate(layout_tree, [])

    assert Enum.count(first_page.boxes, &(&1.type == :text and &1.text == "Name")) == 1
    assert Enum.count(second_page.boxes, &(&1.type == :text and &1.text == "Name")) == 1
    assert Enum.any?(second_page.boxes, &(&1.type == :text and &1.text == "Alpha 3"))

    header_text = Enum.find(second_page.boxes, &(&1.type == :text and &1.text == "Name"))
    body_text = Enum.find(second_page.boxes, &(&1.type == :text and &1.text == "Alpha 3"))

    assert header_text.y > body_text.y
  end

  test "paginate rejects invalid layout trees" do
    assert Pagination.paginate(%{boxes: []}, []) == {:error, :invalid_layout}
  end

  defp text_box(text, y, flow_id, extra \\ %{}) do
    Map.merge(
      %{
        type: :text,
        text: text,
        x: 10.0,
        y: y,
        width: 180.0,
        annotation_width: 36.0,
        font: "Helvetica",
        font_size: 12.0,
        line_height: 14.4,
        color: {0, 0, 0},
        flow_id: flow_id,
        break_before: :auto,
        break_after: :auto
      },
      extra
    )
  end
end
