defmodule NativeElixirPdfUtilities.HtmlToPdf.PaginationTest do
  use ExUnit.Case

  alias NativeElixirPdfUtilities.HtmlToPdf.HtmlParser
  alias NativeElixirPdfUtilities.HtmlToPdf.Layout
  alias NativeElixirPdfUtilities.HtmlToPdf.PageFurniture
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

  test "paginate fragments default auto paragraphs across the remaining page space" do
    paragraph_boxes =
      [63.6, 49.2, 34.8, 20.4, 6.0, -8.4]
      |> Enum.with_index(1)
      |> Enum.map(fn {y, index} ->
        text_box("Line #{index}", y, {:block, :paragraph}, %{
          break_inside: :auto,
          fragment_id: {:line, :paragraph, index}
        })
      end)

    layout_tree = %{
      type: :layout,
      page_size: {200.0, 100.0},
      margin: 10.0,
      boxes: [text_box("Lead", 78.0, {:block, :lead})] ++ paragraph_boxes
    }

    assert {:ok, [first_page, second_page]} = Pagination.paginate(layout_tree, [])

    assert Enum.map(first_page.boxes, & &1.text) ==
             ["Lead", "Line 1", "Line 2", "Line 3", "Line 4"]

    assert Enum.map(second_page.boxes, & &1.text) == ["Line 5", "Line 6"]
  end

  test "paginate moves a fitting break-inside avoid paragraph to a fresh page" do
    lead = %{
      type: :rect,
      x: 10.0,
      y: 30.0,
      width: 180.0,
      height: 60.0,
      flow_id: {:block, :lead},
      break_before: :auto,
      break_after: :auto
    }

    paragraph_boxes =
      [13.0, -1.4]
      |> Enum.with_index(1)
      |> Enum.map(fn {y, index} ->
        text_box("Avoid #{index}", y, {:block, :avoid}, %{
          break_inside: :avoid,
          fragment_id: {:line, :avoid, index}
        })
      end)

    layout_tree = %{
      type: :layout,
      page_size: {200.0, 100.0},
      margin: 10.0,
      boxes: [lead] ++ paragraph_boxes
    }

    assert {:ok, [first_page, second_page]} = Pagination.paginate(layout_tree, [])
    assert first_page.boxes == [lead]
    assert Enum.map(second_page.boxes, & &1.text) == ["Avoid 1", "Avoid 2"]
  end

  test "paginate fragments an oversized break-inside avoid paragraph" do
    paragraph_boxes =
      [78.0, 63.6, 49.2, 34.8, 20.4, 6.0, -8.4]
      |> Enum.with_index(1)
      |> Enum.map(fn {y, index} ->
        text_box("Oversized #{index}", y, {:block, :oversized}, %{
          break_inside: :avoid,
          fragment_id: {:line, :oversized, index}
        })
      end)

    layout_tree = %{
      type: :layout,
      page_size: {200.0, 100.0},
      margin: 10.0,
      boxes: paragraph_boxes
    }

    assert {:ok, [first_page, second_page]} = Pagination.paginate(layout_tree, [])

    assert Enum.map(first_page.boxes, & &1.text) ==
             ["Oversized 1", "Oversized 2", "Oversized 3", "Oversized 4", "Oversized 5"]

    assert Enum.map(second_page.boxes, & &1.text) == ["Oversized 6", "Oversized 7"]
  end

  test "paginated page count supplies current and total page furniture tokens" do
    boxes = [
      text_box("One", 68.0, {:block, 1}),
      text_box("Two", 41.6, {:block, 2}),
      text_box("Three", 15.2, {:block, 3})
    ]

    layout_tree = %{
      type: :layout,
      page_size: {200.0, 100.0},
      margin: 20.0,
      boxes: boxes
    }

    assert {:ok, pages} = Pagination.paginate(layout_tree, [])
    assert length(pages) == 2

    assert {:ok, [first_page, second_page]} =
             PageFurniture.decorate(pages, layout_tree,
               page_furniture: [
                 header: "<div style=\"font-size: 8pt\">Page {{page}}/{{pages}}</div>"
               ]
             )

    assert Enum.any?(first_page.boxes, &(&1.type == :text and &1.text == "Page 1/2"))
    assert Enum.any?(second_page.boxes, &(&1.type == :text and &1.text == "Page 2/2"))
  end

  test "paginate preserves first page top offset from parent padding" do
    layout_tree = %{
      type: :layout,
      page_size: {200.0, 100.0},
      margin: 0.0,
      boxes: [text_box("Padded", 58.0, {:block, 1})]
    }

    assert {:ok, [page]} = Pagination.paginate(layout_tree, [])
    [padded] = page.boxes

    assert_in_delta padded.y, 58.0, 0.0001
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

  test "paginate keeps child groups inside an already placed parent background" do
    boxes = [
      %{
        type: :rect,
        x: 0.0,
        y: 0.0,
        width: 200.0,
        height: 100.0,
        flow_id: {:block, :parent},
        break_before: :auto,
        break_after: :auto
      },
      text_box("Child", 78.0, {:block, :child})
    ]

    layout_tree = %{type: :layout, page_size: {200.0, 100.0}, margin: 0.0, boxes: boxes}

    assert {:ok, [page]} = Pagination.paginate(layout_tree, [])
    assert Enum.map(page.boxes, &Map.get(&1, :text, :rect)) == [:rect, "Child"]

    child = Enum.find(page.boxes, &(&1.type == :text))
    assert_in_delta child.y, 78.0, 0.0001
  end

  test "paginate consumes empty manual page-break markers" do
    boxes = [
      text_box("First", 78.0, {:block, 1}),
      %{
        type: :page_break,
        x: 10.0,
        y: 63.6,
        width: 180.0,
        height: 0.0,
        flow_id: {:block, 2},
        break_before: :auto,
        break_after: :page
      },
      text_box("Second", 63.6, {:block, 3})
    ]

    layout_tree = %{type: :layout, page_size: {200.0, 100.0}, margin: 10.0, boxes: boxes}

    assert {:ok, [first_page, second_page]} = Pagination.paginate(layout_tree, [])
    assert Enum.map(first_page.boxes, & &1.text) == ["First"]
    assert Enum.map(second_page.boxes, & &1.text) == ["Second"]
    refute Enum.any?(first_page.boxes ++ second_page.boxes, &match?(%{type: :page_break}, &1))
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
      %{
        type: :text,
        text: "Font size bounds",
        x: 10.0,
        y: 12.0,
        width: 20.0,
        annotation_width: 20.0,
        font: "Helvetica",
        font_size: 8.0,
        color: {0, 0, 0},
        flow_id: {:text, 2},
        break_before: :auto,
        break_after: :auto
      },
      %{type: :metadata, flow_id: {:metadata, 1}, break_before: :auto, break_after: :auto}
    ]

    layout_tree = %{type: :layout, page_size: {200.0, 100.0}, margin: 10.0, boxes: boxes}

    assert {:ok, [page]} = Pagination.paginate(layout_tree, [])
    assert length(page.boxes) == 4
    assert Enum.at(page.boxes, 3).type == :metadata
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
      "<table><thead><tr><th colspan=\"2\">Inventory</th></tr>" <>
        "<tr><th>Name</th><th>Count</th></tr></thead><tbody>" <>
        rows <> "</tbody></table>"

    assert {:ok, dom} = HtmlParser.parse(html)
    assert {:ok, styled_tree} = Style.compute(dom, [])
    assert {:ok, layout_tree} = Layout.layout(styled_tree, page_size: {200, 100}, margin: 10)
    assert {:ok, pages} = Pagination.paginate(layout_tree, [])
    assert length(pages) > 1

    for page <- pages do
      assert Enum.count(page.boxes, &(&1.type == :text and &1.text == "Inventory")) == 1
      assert Enum.count(page.boxes, &(&1.type == :text and &1.text == "Name")) == 1
    end

    last_page = List.last(pages)
    assert Enum.any?(last_page.boxes, &(&1.type == :text and &1.text == "Alpha 3"))

    title = Enum.find(last_page.boxes, &(&1.type == :text and &1.text == "Inventory"))
    column_header = Enum.find(last_page.boxes, &(&1.type == :text and &1.text == "Name"))
    body_text = Enum.find(last_page.boxes, &(&1.type == :text and &1.text == "Alpha 3"))

    assert title.y > column_header.y
    assert column_header.y > body_text.y
  end

  test "paginate keeps a near-page-height table row inside the page bounds" do
    table_id = :near_page_height

    header =
      text_box("Header", 78.0, {:table_row, table_id, :header}, %{
        table_id: table_id,
        table_section: :head,
        repeat_table_header: true
      })

    first_row =
      text_box("First", 60.0, {:table_row, table_id, :first}, %{
        table_id: table_id,
        table_section: :body
      })

    tall_flow_id = {:table_row, table_id, :tall}

    tall_row = [
      %{
        type: :rect,
        x: 10.0,
        y: -15.0,
        width: 180.0,
        height: 70.0,
        flow_id: tall_flow_id,
        table_id: table_id,
        table_section: :body,
        break_before: :auto,
        break_after: :auto
      },
      text_box("Tall", 35.0, tall_flow_id, %{
        table_id: table_id,
        table_section: :body
      })
    ]

    layout_tree = %{
      type: :layout,
      page_size: {200.0, 100.0},
      margin: 10.0,
      boxes: [header, first_row | tall_row]
    }

    assert {:ok, [first_page, second_page]} = Pagination.paginate(layout_tree, [])
    assert Enum.map(first_page.boxes, &Map.get(&1, :text, :rect)) == ["Header", "First"]
    assert Enum.map(second_page.boxes, &Map.get(&1, :text, :rect)) == [:rect, "Tall"]

    tall_background = hd(second_page.boxes)
    assert_in_delta tall_background.y, 20.0, 0.0001
    assert tall_background.y >= 10.0
  end

  test "paginate rejects invalid layout trees" do
    assert {:error,
            {:invalid_layout,
             %{
               stage: :pagination,
               reason: :invalid_layout,
               operation: :paginate,
               module: NativeElixirPdfUtilities.HtmlToPdf.Pagination
             }}} = Pagination.paginate(%{boxes: []}, [])

    assert {:error,
            {:invalid_layout,
             %{stage: :pagination, reason: :invalid_layout, operation: :paginate}}} =
             Pagination.paginate(
               %{type: :layout, page_size: {-1, 100}, margin: 10, boxes: []},
               []
             )

    assert {:error,
            {:invalid_layout,
             %{stage: :pagination, reason: :invalid_layout, operation: :paginate}}} =
             Pagination.paginate(
               %{type: :layout, page_size: {100, 100}, margin: -1, boxes: []},
               []
             )

    assert {:error,
            {:invalid_layout,
             %{stage: :pagination, reason: :invalid_layout, operation: :paginate}}} =
             Pagination.paginate(
               %{type: :layout, page_size: :a4, margin: 10, boxes: []},
               []
             )

    for margin <- [50, 60] do
      assert {:error,
              {:invalid_layout,
               %{stage: :pagination, reason: :invalid_layout, operation: :paginate}}} =
               Pagination.paginate(
                 %{type: :layout, page_size: {100, 100}, margin: margin, boxes: []},
                 []
               )
    end

    assert {:ok, [%{size: {100, 100}, boxes: []}]} =
             Pagination.paginate(
               %{type: :layout, page_size: {100, 100}, margin: 49, boxes: []},
               []
             )
  end

  test "paginate uses asymmetric top and bottom page margins" do
    margins = %{top: 10.0, right: 5.0, bottom: 30.0, left: 15.0}

    layout_tree = %{
      type: :layout,
      page_size: {100.0, 100.0},
      margin: margins,
      margins: margins,
      boxes: [
        text_box("First", 78.0, {:block, 1}),
        text_box("Second", 50.0, {:block, 2}),
        text_box("Third", 20.0, {:block, 3})
      ]
    }

    assert {:ok, [first_page, second_page]} = Pagination.paginate(layout_tree, [])
    assert Enum.map(first_page.boxes, & &1.text) == ["First", "Second"]
    assert Enum.map(second_page.boxes, & &1.text) == ["Third"]
    assert_in_delta hd(second_page.boxes).y, 78.0, 0.0001

    assert {:error, {:invalid_layout, %{stage: :pagination}}} =
             Pagination.paginate(
               %{layout_tree | margins: %{margins | left: 50.0, right: 50.0}},
               []
             )
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
