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

  test "paginate supports default opts and empty pages" do
    assert Pagination.paginate(%{
             type: :layout,
             page_size: {200.0, 100.0},
             margin: 10.0,
             boxes: []
           }) ==
             {:ok, [%{size: {200.0, 100.0}, boxes: []}]}
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

  test "paginate honors break-after and ignores empty page breaks" do
    boxes = [
      text_box("First", 78.0, {:block, 1}, %{break_after: :page}),
      text_box("Second", 78.0, {:block, 2})
    ]

    layout_tree = %{type: :layout, page_size: {200.0, 100.0}, margin: 10.0, boxes: boxes}

    assert {:ok, [first_page, second_page]} = Pagination.paginate(layout_tree, [])
    assert Enum.map(first_page.boxes, & &1.text) == ["First"]
    assert Enum.map(second_page.boxes, & &1.text) == ["Second"]

    leading_break_tree = %{
      type: :layout,
      page_size: {200.0, 100.0},
      margin: 10.0,
      boxes: [text_box("Only", 78.0, {:block, 1}, %{break_before: :page})]
    }

    assert {:ok, [%{boxes: [only]}]} = Pagination.paginate(leading_break_tree, [])
    assert only.text == "Only"
  end

  test "paginate handles non-text bounds and boxes without y coordinates" do
    boxes = [
      %{
        type: :rect,
        x: 10.0,
        y: 50.0,
        width: 20.0,
        height: 10.0,
        flow_id: {:shape, 1},
        break_before: :auto,
        break_after: :auto
      },
      %{
        type: :text,
        text: "Line",
        x: 10.0,
        y: 20.0,
        width: 20.0,
        annotation_width: 20.0,
        font: "Helvetica",
        line_height: 12.0,
        color: {0, 0, 0},
        flow_id: {:text, 1},
        break_before: :auto,
        break_after: :auto
      },
      %{type: :metadata, flow_id: {:metadata, 1}, break_before: :auto, break_after: :auto}
    ]

    layout_tree = %{type: :layout, page_size: {200.0, 100.0}, margin: 10.0, boxes: boxes}

    assert {:ok, [page]} = Pagination.paginate(layout_tree, [])
    assert length(page.boxes) == 3
    assert Enum.at(page.boxes, 2).type == :metadata
  end

  test "paginate overflows table body groups without a repeated header" do
    boxes = [
      text_box("One", 78.0, {:table_row, :table_without_head, 1}, %{
        table_id: :table_without_head,
        table_section: :body
      }),
      text_box("Two", -20.0, {:table_row, :table_without_head, 2}, %{
        table_id: :table_without_head,
        table_section: :body
      })
    ]

    layout_tree = %{type: :layout, page_size: {200.0, 100.0}, margin: 10.0, boxes: boxes}

    assert {:ok, [first_page, second_page]} = Pagination.paginate(layout_tree, [])
    assert Enum.map(first_page.boxes, & &1.text) == ["One"]
    assert Enum.map(second_page.boxes, & &1.text) == ["Two"]
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

    assert Pagination.paginate(%{type: :layout, page_size: {-1, 100}, margin: 10, boxes: []}, []) ==
             {:error, :invalid_layout}

    assert Pagination.paginate(%{type: :layout, page_size: {100, 100}, margin: -1, boxes: []}, []) ==
             {:error, :invalid_layout}
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
