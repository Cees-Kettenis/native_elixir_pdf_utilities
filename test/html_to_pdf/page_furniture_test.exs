defmodule NativeElixirPdfUtilities.HtmlToPdf.PageFurnitureTest do
  use ExUnit.Case

  alias NativeElixirPdfUtilities.HtmlToPdf.PageFurniture

  @page_size {200.0, 100.0}
  @margin 20.0

  test "decorate leaves pages unchanged when page furniture is disabled" do
    pages = pages(1)
    layout_tree = layout_tree()

    assert PageFurniture.decorate(pages, layout_tree, []) == {:ok, pages}

    assert PageFurniture.decorate(pages, layout_tree, page_furniture: nil) ==
             {:ok, pages}

    assert PageFurniture.decorate(pages, layout_tree, page_furniture: false) ==
             {:ok, pages}
  end

  test "decorate substitutes page tokens and applies first odd and even variants" do
    assert {:ok, decorated} =
             PageFurniture.decorate(pages(4), layout_tree(),
               page_furniture: [
                 header: [
                   default: furniture_html("Default"),
                   first: furniture_html("First"),
                   odd: furniture_html("Odd"),
                   even: furniture_html("Even")
                 ],
                 footer: furniture_html("Page {{page}} of {{pages}}")
               ]
             )

    assert Enum.map(decorated, &furniture_texts/1) == [
             ["Body 1", "First", "Page 1 of 4"],
             ["Body 2", "Even", "Page 2 of 4"],
             ["Body 3", "Odd", "Page 3 of 4"],
             ["Body 4", "Even", "Page 4 of 4"]
           ]

    Enum.each(decorated, fn page ->
      [body, header, footer] = Enum.filter(page.boxes, &(&1.type == :text))
      assert header.y > body.y
      assert footer.y < body.y
      assert footer.y >= 0
    end)
  end

  test "decorate supports first-page-only and except-first-page furniture" do
    assert {:ok, first_only} =
             PageFurniture.decorate(pages(3), layout_tree(),
               page_furniture: [
                 header: [default: false, first: furniture_html("First only")]
               ]
             )

    assert Enum.map(first_only, &furniture_texts/1) == [
             ["Body 1", "First only"],
             ["Body 2"],
             ["Body 3"]
           ]

    assert {:ok, except_first} =
             PageFurniture.decorate(pages(3), layout_tree(),
               page_furniture: %{
                 footer: %{default: furniture_html("Later"), first: nil}
               }
             )

    assert Enum.map(except_first, &furniture_texts/1) == [
             ["Body 1"],
             ["Body 2", "Later"],
             ["Body 3", "Later"]
           ]
  end

  test "decorate accepts plain-text templates and configured stylesheet rules" do
    assert {:ok, [page]} =
             PageFurniture.decorate(pages(1), layout_tree(),
               stylesheets: [".accent { color: red; font-size: 8pt; }"],
               page_furniture: [
                 header: "<span class=\"accent\">Plain {{page}}</span>"
               ]
             )

    header = Enum.find(page.boxes, &(&1.type == :text and &1.text == "Plain 1"))
    assert header.color == {1.0, 0.0, 0.0}
    assert header.font_size == 8.0
  end

  test "decorate returns actionable diagnostics for invalid options and oversized furniture" do
    invalid_options = [
      :invalid,
      [:not_a_keyword],
      [unknown: "value"],
      [header: 123],
      [header: [[:not_a_keyword]]],
      [header: [unknown: "value"]],
      [footer: [default: 123]]
    ]

    Enum.each(invalid_options, fn furniture ->
      assert {:error,
              {:invalid_options,
               %{
                 stage: :options,
                 reason: :invalid_options,
                 operation: :decorate_pages,
                 module: NativeElixirPdfUtilities.HtmlToPdf.PageFurniture,
                 message: message
               }}} =
               PageFurniture.decorate(pages(1), layout_tree(), page_furniture: furniture)

      assert is_binary(message)
    end)

    assert {:error,
            {:invalid_layout,
             %{
               stage: :layout,
               reason: :invalid_layout,
               operation: :decorate_pages,
               message: "header page furniture height 30.0pt exceeds the 20.0pt page margin"
             }}} =
             PageFurniture.decorate(pages(1), layout_tree(),
               page_furniture: [
                 header:
                   "<div style=\"height: 30pt; background: #eeeeee; font-size: 8pt\">Too tall</div>"
               ]
             )
  end

  test "decorate validates its public layout inputs and template failures" do
    assert {:error,
            {:invalid_layout,
             %{
               stage: :layout,
               reason: :invalid_layout,
               operation: :decorate_pages,
               module: NativeElixirPdfUtilities.HtmlToPdf.PageFurniture
             }}} = PageFurniture.decorate([%{}], layout_tree(), [])

    assert {:error, {:invalid_layout, %{stage: :layout}}} =
             PageFurniture.decorate(:not_pages, layout_tree(), [])

    invalid_page_size_layout = %{layout_tree() | page_size: :a4}

    assert {:error, {:invalid_layout, %{stage: :layout}}} =
             PageFurniture.decorate(
               [%{size: :a4, boxes: []}],
               invalid_page_size_layout,
               []
             )

    assert {:error, {:invalid_options, %{stage: :options}}} =
             PageFurniture.decorate(pages(1), layout_tree(), [:not_options])

    assert {:error,
            {:unsupported_html,
             %{
               stage: :html,
               reason: :unsupported_html,
               source: "<canvas>"
             }}} =
             PageFurniture.decorate(pages(1), layout_tree(),
               page_furniture: [header: "<canvas></canvas>"]
             )
  end

  test "decorate supports full HTML documents and empty visual templates" do
    full_document = """
    <!doctype html>
    <html>
      <body><div style="font-size: 8pt">Full document</div></body>
    </html>
    """

    assert {:ok, [full_page]} =
             PageFurniture.decorate(pages(1), layout_tree(),
               page_furniture: [header: full_document]
             )

    assert furniture_texts(full_page) == ["Body 1", "Full document"]

    assert {:ok, [empty_page]} =
             PageFurniture.decorate(pages(1), layout_tree(),
               page_furniture: [header: "<div style=\"display: none\">Hidden</div>"]
             )

    assert furniture_texts(empty_page) == ["Body 1"]
  end

  defp pages(count) do
    Enum.map(1..count, fn page_number ->
      %{
        size: @page_size,
        boxes: [text_box("Body #{page_number}", 44.0)]
      }
    end)
  end

  defp layout_tree do
    %{
      type: :layout,
      page_size: @page_size,
      margin: @margin,
      boxes: [],
      content_width: 160.0,
      content_height: 60.0
    }
  end

  defp furniture_html(text, height \\ 12) do
    "<div style=\"height: #{height}pt; font-size: 8pt; line-height: 10pt\">#{text}</div>"
  end

  defp furniture_texts(page) do
    page.boxes
    |> Enum.filter(&(&1.type == :text))
    |> Enum.map(& &1.text)
  end

  defp text_box(text, y) do
    %{
      type: :text,
      text: text,
      x: 20.0,
      y: y,
      width: 160.0,
      annotation_width: 40.0,
      font: "Helvetica",
      font_size: 10.0,
      line_height: 12.0,
      color: {0, 0, 0}
    }
  end
end
