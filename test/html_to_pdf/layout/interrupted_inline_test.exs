defmodule NativeElixirPdfUtilities.HtmlToPdf.InterruptedInlineTest do
  use ExUnit.Case, async: true
  alias NativeElixirPdfUtilities.Text
  alias NativeElixirPdfUtilities.HtmlToPdf
  alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, Style, Layout, Pagination}

  test "interrupted inline fragments retain their links and one positioned child across pages" do
    html =
      File.read!("test/fixtures/html_to_pdf/browser_parity/interrupted_inline_pagination.html")

    assert {:ok, dom} = HtmlParser.parse_document_detailed(html)
    assert {:ok, styled} = Style.compute_detailed(dom, system_font_discovery: false)
    assert {:ok, layout} = Layout.layout(styled, page_size: {240, 180}, margin: 12)
    assert {:ok, pages} = Pagination.paginate(layout)
    assert length(pages) == 2
    boxes = Enum.flat_map(pages, & &1.boxes)

    for label <- [
          "Before",
          "Leading ",
          "nested",
          "Block content",
          "Trailing text",
          "Positioned",
          "After"
        ] do
      assert Enum.count(boxes, &(Map.get(&1, :text) == label)) == 1
    end

    refute Enum.any?(
             boxes,
             &(Map.get(&1, :text) in ["Before", "After"] and Map.has_key?(&1, :link_url))
           )

    assert Enum.all?(
             Enum.filter(
               boxes,
               &(Map.get(&1, :text) in ["Leading ", "Block content", "Trailing text"])
             ),
             &(&1.link_url == "https://example.com/split")
           )

    assert {:ok, pdf} = HtmlToPdf.render(html, system_font_discovery: false)
    assert pdf =~ "/Subtype /Link"
  end

  test "nested split inline ancestors anchor each positioned child once" do
    html =
      File.read!(
        "test/fixtures/html_to_pdf/browser_parity/interrupted_inline_nested_positioning.html"
      )

    assert {:ok, dom} = HtmlParser.parse_document_detailed(html)
    assert {:ok, styled} = Style.compute_detailed(dom, system_font_discovery: false)
    assert {:ok, layout} = Layout.layout(styled, page_size: {240, 180}, margin: 12)
    assert {:ok, [page]} = Pagination.paginate(layout)

    for label <- ["Inner child", "Outer child", "Block content"] do
      assert Enum.count(page.boxes, &(Map.get(&1, :text) == label)) == 1
    end

    assert {:ok, pdf} = HtmlToPdf.render(html, system_font_discovery: false)
    assert {:ok, text} = Text.extract(pdf)
    assert text =~ "Inner child" and text =~ "Outer child"
  end

  test "split inline alignment keeps struts and source fragments through the page break" do
    html =
      File.read!(
        "test/fixtures/html_to_pdf/browser_parity/interrupted_inline_vertical_alignment.html"
      )

    assert {:ok, pdf} = HtmlToPdf.render(html, system_font_discovery: false)
    assert {:ok, [first, second]} = NativeElixirPdfUtilities.Split.by_page(pdf)
    assert {:ok, before} = Text.extract(first)
    assert {:ok, after_text} = Text.extract(second)
    assert before =~ "Middle" and not (before =~ "Raised")
    assert after_text =~ "Raised" and after_text =~ "Block four"
  end

  test "continuous inline image backgrounds retain one positioned child on its source page" do
    html =
      File.read!(
        "test/fixtures/html_to_pdf/browser_parity/interrupted_inline_images_and_backgrounds.html"
      )

    assert {:ok, dom} = HtmlParser.parse_document_detailed(html)
    assert {:ok, styled} = Style.compute_detailed(dom, system_font_discovery: false)
    assert {:ok, layout} = Layout.layout(styled, page_size: {240, 180}, margin: 12)
    refute Enum.any?(layout.boxes, &Map.has_key?(&1, :inline_background_image))
    assert Enum.count(layout.boxes, &(&1.type == :image)) > 2
    assert {:ok, [first, second]} = Pagination.paginate(layout)
    [positioned] = Enum.filter(first.boxes, &(Map.get(&1, :text) == "Positioned"))
    assert_in_delta positioned.x, 115.75, 0.001
    refute Enum.any?(second.boxes, &(Map.get(&1, :text) == "Positioned"))

    source = Enum.find(layout.boxes, &Map.has_key?(&1, :inline_positioned_anchor))

    for invalid <- [
          :bad,
          %{id: make_ref(), source_y: 0},
          Map.put(source.inline_positioned_anchor, :source_y, :bad)
        ] do
      altered = %{
        layout
        | boxes:
            Enum.map(layout.boxes, fn box ->
              if box == source, do: Map.put(box, :inline_positioned_anchor, invalid), else: box
            end)
      }

      assert {:error, {:invalid_layout, diagnostic}} = Pagination.paginate(altered)
      assert diagnostic.stage == :pagination and diagnostic.reason == :invalid_layout
      assert diagnostic.source == "inline_positioned_anchor" and diagnostic.operation == :paginate
      assert diagnostic.message =~ "matching reference anchors"
    end

    altered = %{
      layout
      | boxes:
          Enum.map(layout.boxes, fn box ->
            if Map.has_key?(box, :inline_fragment_anchors),
              do: Map.put(box, :inline_fragment_anchors, :bad),
              else: box
          end)
    }

    assert {:error, {:invalid_layout, %{source: "inline_positioned_anchor"}}} =
             Pagination.paginate(altered)
  end

  test "split inline source placement coexists with child drawing groups and glyph groups" do
    html =
      "<body style='margin:0;font-family:DejaVu Sans;font-size:10pt;line-height:16pt'><p>soft&shy;word</p><iframe srcdoc='&lt;p&gt;Inside&lt;/p&gt;' style='width:60pt;height:24pt'></iframe><span style='position:relative'>Start<span style='display:block'>Block</span>End<b style='position:absolute;left:40pt;top:0'>Positioned</b></span></body>"

    assert {:ok, pdf} = HtmlToPdf.render(html, system_font_discovery: false)
    assert {:ok, text} = Text.extract(pdf)
    assert text =~ "Inside" and text =~ "Positioned"
    assert length(Regex.scan(~r/Positioned/, text)) == 1

    NativeElixirPdfUtilities.HtmlToPdf.ResourceContext.run(
      [],
      NativeElixirPdfUtilities.Validators.HtmlValidator.new_image_budget(),
      fn context ->
        assert {:ok, dom} = HtmlParser.parse_document_detailed(html)

        assert {:ok, styled} =
                 Style.compute_detailed(dom, [system_font_discovery: false], context.image_budget)

        assert {:ok, layout} = Layout.layout(styled)

        altered = %{
          layout
          | boxes:
              Enum.map(layout.boxes, fn box ->
                case box do
                  %{type: :drawing_group, boxes: [first | rest]} ->
                    %{box | boxes: [Map.put(first, :inline_fragment_anchors, :bad) | rest]}

                  _ ->
                    box
                end
              end)
        }

        assert {:error, {:invalid_layout, diagnostic}} = Pagination.paginate(altered)
        assert diagnostic.source == "inline_positioned_anchor" and diagnostic.stage == :pagination
      end
    )
  end

  test "clipped and nonrepeating inline backgrounds use the continuous source geometry in measurement" do
    alias NativeElixirPdfUtilities.HtmlToPdf.ResourceContext
    alias NativeElixirPdfUtilities.Validators.HtmlValidator

    html =
      File.read!(
        "test/fixtures/html_to_pdf/browser_parity/interrupted_inline_images_and_backgrounds.html"
      )

    for source <- [
          html,
          String.replace(
            html,
            "background-size:12pt 6pt;",
            "background-size:12pt 6pt;background-repeat:no-repeat;"
          ),
          String.replace(
            html,
            "body{width:180pt;",
            "body{width:180pt;height:140pt;overflow:hidden;"
          )
        ] do
      assert {:ok, dom} = HtmlParser.parse_document_detailed(source)
      assert {:ok, styled} = Style.compute_detailed(dom, system_font_discovery: false)
      assert {:ok, layout} = Layout.layout(styled, page_size: {240, 180}, margin: 12)
      refute Enum.any?(layout.boxes, &Map.has_key?(&1, :inline_background_image))

      ResourceContext.run([], HtmlValidator.new_image_budget(), fn context ->
        assert {:ok, measured} =
                 Layout.measure_child(hd(styled.children), context, %{available_width: 180.0})

        assert measured.width > 0
        refute Enum.any?(measured.boxes, &Map.has_key?(&1, :inline_background_image))
      end)

      assert {:ok, _pdf} = HtmlToPdf.render(source, system_font_discovery: false)
    end
  end
end
