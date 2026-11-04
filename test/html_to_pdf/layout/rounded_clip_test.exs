defmodule NativeElixirPdfUtilities.HtmlToPdf.RoundedClipTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.{HtmlToPdf, Limits}

  alias NativeElixirPdfUtilities.HtmlToPdf.{
    HtmlParser,
    Style,
    Layout,
    Pagination,
    PdfWriter,
    PageGeometry
  }

  alias NativeElixirPdfUtilities.Validators.{WriterValidator, HtmlFormValidator}
  alias NativeElixirPdfUtilities.Pdf.Reader

  setup do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    :ok
  end

  test "public writer prepares bounded intersected rounded clips and preserves ordinary clip bytes" do
    box = %{
      type: :rect,
      x: 1,
      y: 1,
      width: 20,
      height: 20,
      fill_color: {1, 0, 0},
      stroke_color: nil,
      stroke_width: 0,
      border_radius: 0
    }

    outer = %{
      x: 2,
      y: 2,
      width: 10,
      height: 12,
      border_radius: 100,
      inner_clip: %{x: 3, y: 3, width: 8, height: 8, border_radius: 50}
    }

    pages = [%{size: {30, 30}, boxes: [Map.put(box, :clip, outer)]}]
    assert {:ok, prepared} = WriterValidator.prepare(pages, [])
    [prepared] = hd(prepared.pages).boxes
    assert prepared.clip.border_radius == 5
    assert prepared.clip.inner_clip.border_radius == 4
    assert {:ok, pdf} = PdfWriter.render(pages)
    assert length(Regex.scan(~r/W n/, pdf)) == 2
    assert pdf =~ " c "

    text = %{
      type: :text,
      text: "A",
      x: 2,
      y: 10,
      font: "Helvetica",
      font_size: 8,
      color: {0, 0, 0},
      snap_to_css_pixel_grid: true,
      clip: outer
    }

    assert {:ok, _} =
             PdfWriter.render([
               %{size: {30, 30}, boxes: [%{type: :glyph_group, actual_text: "A", boxes: [text]}]}
             ])

    # Glyph children without a snap flag keep authored clip coordinates.
    unsnapped = Map.delete(text, :snap_to_css_pixel_grid)

    assert {:ok, prepared} =
             WriterValidator.prepare(
               [
                 %{
                   size: {30, 30},
                   boxes: [%{type: :glyph_group, actual_text: "A", boxes: [unsnapped]}]
                 }
               ],
               []
             )

    child = Enum.find(hd(prepared.pages).boxes, &(&1.type == :text))
    assert child.clip.x == outer.x and child.clip.y == outer.y
    assert child.clip.inner_clip.x == outer.inner_clip.x

    ordinary = %{x: 2, y: 2, width: 10, height: 12}

    assert {:ok, pdf} =
             PdfWriter.render([%{size: {30, 30}, boxes: [Map.put(box, :clip, ordinary)]}])

    assert pdf =~ "2 2 10 12 re W n"

    for clip <- [
          ordinary |> Map.put(:width, 0) |> Map.put(:border_radius, 0),
          Map.put(outer, :height, 0),
          put_in(outer.inner_clip.width, 0)
        ] do
      assert {:ok, prepared} =
               WriterValidator.prepare(
                 [%{size: {30, 30}, boxes: [Map.put(box, :clip, clip)]}],
                 []
               )

      assert {:ok, _} = PdfWriter.render(prepared.pages)
    end

    for corrupt <- [
          Map.put(outer, :border_radius, -1),
          Map.put(outer, :border_radius, "round"),
          Map.put(outer, :width, -1),
          Map.put(outer, :x, 1_000_000_001),
          Map.put(outer, :x, 1_000_000_000),
          Map.put(outer, :unrecognized, true),
          Map.put(outer, :inner_clip, nil),
          put_in(outer.inner_clip.border_radius, -1),
          put_in(outer.inner_clip.x, 1_000_000_000),
          Map.update!(outer, :inner_clip, &Map.put(&1, :inner_clip, ordinary))
        ] do
      assert {:error, {:invalid_pdf_input, diagnostic}} =
               PdfWriter.render([%{size: {30, 30}, boxes: [Map.put(box, :clip, corrupt)]}])

      assert diagnostic.stage == :pdf and diagnostic.operation == :write_pdf
      assert diagnostic.message =~ "inner_clip"
    end
  end

  test "nested snapping keeps exact ties and validates rounded far edges after preparation" do
    for {value, expected} <- [{-1.125, -0.75}, {-0.375, 0.0}, {0.375, 0.75}, {1.125, 1.5}] do
      assert PageGeometry.snap_css_pixel(value) == expected
    end

    box = %{
      type: :rect,
      x: 0,
      y: 0,
      width: 1,
      height: 1,
      fill_color: {1, 0, 0},
      stroke_color: nil,
      stroke_width: 0,
      border_radius: 0,
      snap_to_css_pixel_grid: true,
      clip: %{
        x: 0.375,
        y: 0.375,
        width: 1.5,
        height: 1.5,
        border_radius: 100,
        inner_clip: %{x: 0.375, y: 0.375, width: 0, height: 1.5, border_radius: 2}
      }
    }

    assert {:ok, prepared} = WriterValidator.prepare([%{size: {3, 3}, boxes: [box]}], [])
    clip = hd(hd(prepared.pages).boxes).clip
    assert clip.x == 0.75 and clip.y == 0.0 and clip.width == 1.5 and clip.height == 1.5
    assert clip.border_radius == 0.75 and clip.inner_clip.border_radius == 0

    Limits.install(%{Limits.effective() | max_css_numeric_magnitude: 1.2})

    box = %{
      box
      | clip: %{
          x: 0,
          y: 0,
          width: 1.2,
          height: 1.2,
          inner_clip: %{x: 0, y: 0, width: 1.2, height: 1.2}
        }
    }

    assert {:error, {:invalid_pdf_input, _}} = PdfWriter.render([%{size: {3, 3}, boxes: [box]}])
  end

  test "listbox client clips include authored padding and rounded borders fill contour rings" do
    html = """
    <style>body{margin:0}select{display:block;width:100pt;height:30pt;padding:2pt 3pt;box-sizing:content-box}</style>
    <select multiple><option selected>First</option><option>Second</option><option>Third</option></select>
    """

    assert {:ok, dom} = HtmlParser.parse_detailed(html)
    assert {:ok, styled} = Style.compute_detailed(dom)
    assert {:ok, layout} = Layout.layout(styled)
    text = Enum.find(layout.boxes, &(&1.type == :text and &1.text == "First"))
    assert text.clip.width == 100 + 6 - 11.25
    assert text.clip.height == 30 + 4
    assert text.clip.inner_clip.width == text.clip.width + 11.25
    assert text.clip.inner_clip.height == text.clip.height
    assert {:ok, pdf} = HtmlToPdf.render(html)
    assert pdf =~ "f*"
  end

  test "menu arrows use validated open strokes and reject missing paint frames" do
    assert {:ok, dom} = HtmlParser.parse_detailed("<select><option>A</option></select>")
    assert {:ok, styled} = Style.compute_detailed(dom)
    assert {:ok, layout} = Layout.layout(styled)
    arrow = Enum.find(layout.boxes, &(&1[:role] == :select_arrow))
    assert arrow.shape == {:chevron, :down} and arrow.fill_color == nil
    assert arrow.stroke_width == 1.5

    assert {:ok, pdf} =
             PdfWriter.render([%{size: {200, 200}, boxes: [arrow]}],
               compress_streams: false,
               forms: :static
             )

    assert pdf =~ "2 w" and pdf =~ " l S"

    for corrupt <- [Map.delete(arrow, :clip), Map.put(arrow, :shape, {:chevron, :up})] do
      assert {:error, {:invalid_pdf_input, diagnostic}} =
               PdfWriter.render([%{size: {200, 200}, boxes: [corrupt]}])

      assert diagnostic.stage == :pdf and diagnostic.operation == :write_pdf
      assert diagnostic.message =~ "chevron"
    end
  end

  test "appearance frames preserve page snapping and reject malformed page origins" do
    box = %{
      type: :rect,
      x: 12.2,
      y: 31.2,
      width: 20.3,
      height: 10.3,
      fill_color: {1, 0, 0},
      stroke_color: nil,
      stroke_width: 0,
      border_radius: 0,
      snap_to_css_pixel_grid: true,
      clip: %{
        x: 12.2,
        y: 31.2,
        width: 20.3,
        height: 10.3,
        inner_clip: %{x: 12.2, y: 31.2, width: 20.3, height: 10.3, border_radius: 2}
      }
    }

    origin = %{x: 12, y: 31, width: 100, height: 100}
    frame = %{size: {21, 11}, boxes: [box], paint_frame: origin}
    assert {:ok, prepared} = WriterValidator.prepare_appearances([frame], 1)
    [painted] = hd(prepared.pages).boxes
    assert painted.clip.y == 31 and painted.clip.height == 10.5
    assert painted.clip.inner_clip.y == painted.clip.y
    assert {:ok, objects, _, _} = PdfWriter.appearance_objects([frame], 1)

    assert Enum.any?(objects, fn {_id, _generation, body} ->
             match?({:stream, _, _}, body) and elem(body, 2) =~ "q 1 0 0 1 -12 -31 cm"
           end)

    for invalid <- [
          nil,
          %{},
          Map.put(origin, :height, 0),
          Map.put(origin, :x, "12"),
          Map.put(origin, :x, 1_000_000_000),
          Map.put(origin, :inner_clip, origin),
          Map.put(origin, :border_radius, 0)
        ] do
      assert {:error, {:invalid_pdf_input, diagnostic}} =
               PdfWriter.appearance_objects([%{frame | paint_frame: invalid}], 1)

      assert diagnostic.stage == :pdf and diagnostic.operation == :write_pdf
      assert diagnostic.message =~ "paint_frame"
    end

    assert {:error, {:invalid_pdf_input, _}} = PdfWriter.render([frame])
  end

  test "widget frames use the same rounded border contours as absolute page painting" do
    html = """
    <style>@page{size:200pt 100pt;margin:0}body{margin:0}select{display:block;width:100.2pt;border:1.5pt solid #767676;border-radius:3pt}</style>
    <select multiple size=3 name=pick><option>A</option><option selected>B</option><option>C</option></select>
    """

    assert {:ok, dom} = HtmlParser.parse_detailed(html)
    assert {:ok, styled} = Style.compute_detailed(dom)
    assert {:ok, layout} = Layout.layout(styled, page_size: {200, 100}, margin: 0)
    assert {:ok, pages} = Pagination.paginate(layout)
    assert {:ok, prepared} = WriterValidator.prepare(pages, [])
    assert {:ok, _, [field]} = HtmlFormValidator.prepare(prepared.pages, forms: :interactive)
    box = Enum.find(hd(prepared.pages).boxes, &(&1[:role] == :native_listbox))
    snapped = PageGeometry.snap_box_to_css_pixel_grid(%{box | y: 100 - box.y - box.height})
    snapped = %{snapped | y: 100 - snapped.y - snapped.height}
    [outer, _inner] = PageGeometry.rounded_border_contours(snapped, 100)
    assert field.rect == [outer.x, outer.y, outer.x + outer.width, outer.y + outer.height]
    assert field.appearance.paint_frame.x == outer.x
    assert field.appearance.paint_frame.y == outer.y
    assert field.appearance.paint_frame.height == 100
    assert field.appearance.size == {outer.width, outer.height}

    assert field.appearance.boxes ==
             Enum.filter(hd(prepared.pages).boxes, &(&1[:form_owner] == box.form_owner))

    assert field.top_index == 0 and field.selected_indices == [1]
  end

  test "nested prepared clips constrain link annotations and suppress zero-area links" do
    clip = %{
      x: 0,
      y: 0,
      width: 30,
      height: 30,
      inner_clip: %{x: 3, y: 4, width: 7, height: 8, border_radius: 2}
    }

    box = %{
      type: :text,
      text: "Linked",
      x: 0,
      y: 0,
      width: 30,
      font: "Helvetica",
      font_size: 20,
      line_height: 30,
      color: {0, 0, 0},
      link_url: "https://example.com/clipped",
      clip: clip
    }

    assert {:ok, pdf} = PdfWriter.render([%{size: {50, 50}, boxes: [box]}])
    assert pdf =~ "/Rect [3 4 10 12]"
    assert {:ok, document} = Reader.read_validated(pdf)
    assert length(hd(document.pages).dictionary["Annots"]) == 1
    box = put_in(box.clip.inner_clip.width, 0)
    assert {:ok, pdf} = PdfWriter.render([%{size: {50, 50}, boxes: [box]}])
    refute pdf =~ "/Subtype /Link"
  end

  test "ordinary text appearances translate both prepared clip rectangles once" do
    assert {:ok, dom} =
             HtmlParser.parse_detailed(
               "<textarea name=note style='display:block;width:50pt;height:30pt'>First\nSecond</textarea>"
             )

    assert {:ok, styled} = Style.compute_detailed(dom)
    assert {:ok, layout} = Layout.layout(styled, page_size: {200, 100}, margin: 10)
    assert {:ok, pages} = Pagination.paginate(layout)
    [page] = pages

    page = %{
      page
      | boxes:
          Enum.map(page.boxes, fn box ->
            case Map.get(box, :clip) do
              nil ->
                box

              clip ->
                Map.put(box, :clip, Map.put(clip, :inner_clip, Map.put(clip, :border_radius, 2)))
            end
          end)
    }

    assert {:ok, context} = WriterValidator.prepare([page], [])
    assert {:ok, _, [field]} = HtmlFormValidator.prepare(context.pages, forms: :interactive)
    [left, bottom, _, _] = field.rect
    before = Enum.find(hd(context.pages).boxes, &(&1.type == :text))
    after_box = Enum.find(field.appearance.boxes, &(&1.type == :text))
    assert after_box.clip.x == before.clip.x - left
    assert after_box.clip.y == before.clip.y - bottom
    assert after_box.clip.inner_clip.x == before.clip.inner_clip.x - left
    assert after_box.clip.inner_clip.y == before.clip.inner_clip.y - bottom
    assert after_box.clip.inner_clip.border_radius == before.clip.inner_clip.border_radius
    assert {:ok, _} = PdfWriter.render([page], forms: :interactive)
    refute Map.has_key?(field.appearance, :paint_frame)
  end

  test "relative placement and pagination translate each absolute clip once" do
    render = fn relative ->
      html = """
      <style>@page{size:200pt 100pt;margin:0}body{margin:0;font-size:10pt}select{display:inline-block;width:90pt;height:35pt}p{margin:0}</style>
      <body><p>Before</p><p style="break-before:page"><select multiple size=3 style="#{relative}"><option selected>A</option><option>B</option><option>C</option></select></p></body>
      """

      assert {:ok, dom} = HtmlParser.parse_detailed(html)
      assert {:ok, styled} = Style.compute_detailed(dom)
      assert {:ok, layout} = Layout.layout(styled, page_size: {200, 100}, margin: 0)
      assert {:ok, pages} = Pagination.paginate(layout)
      {layout, pages}
    end

    {normal, normal_pages} = render.("")
    {relative, relative_pages} = render.("position:relative;left:3pt;top:2pt")
    clipped = fn boxes -> Enum.filter(boxes, &is_map(get_in(&1, [:clip, :inner_clip]))) end
    normal_boxes = clipped.(normal.boxes)
    relative_boxes = clipped.(relative.boxes)
    assert normal_boxes != [] and length(normal_boxes) == length(relative_boxes)

    for {before, after_box} <- Enum.zip(normal_boxes, relative_boxes), key <- [:clip] do
      assert after_box[key].x == before[key].x + 3
      assert after_box[key].y == before[key].y - 2
      assert after_box[key].inner_clip.x == before[key].inner_clip.x + 3
      assert after_box[key].inner_clip.y == before[key].inner_clip.y - 2
    end

    assert length(normal_pages) == 2 and length(relative_pages) == 2

    for pages <- [normal_pages, relative_pages] do
      boxes = clipped.(List.last(pages).boxes)
      assert boxes != []

      for box <- boxes do
        assert box.clip.inner_clip.x == box.clip.x
        assert box.clip.inner_clip.y == box.clip.y
        assert box.clip.y >= 0 and box.clip.y + box.clip.height <= 100
      end

      assert {:ok, _} = PdfWriter.render(pages)
    end
  end

  test "authored row geometry and content-box intrinsic dimensions retain ordinary layout validation" do
    for select <- [
          "<select multiple style='box-sizing:content-box'><option style='height:20pt;min-height:10%'>A</option></select>",
          "<select style='box-sizing:content-box'><option>A</option></select>",
          "<select multiple style='display:inline-block;font-size:8pt'><option style='min-height:3pt'>A</option></select>"
        ] do
      assert {:ok, _} = HtmlToPdf.render("<p style='font-size:20pt'>Prefix #{select} suffix</p>")
    end

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             HtmlToPdf.render(
               "<select size=1000000000 style='font-size:100pt'><option>A</option></select>"
             )

    assert diagnostic.message =~ "max_css_numeric_magnitude"
    Limits.install(%{Limits.effective() | max_css_numeric_magnitude: 100})
    rows = String.duplicate("<option>A</option>", 20)

    assert {:error, {:invalid_layout, diagnostic}} =
             HtmlToPdf.render("<select multiple style='width:90pt;height:30pt'>#{rows}</select>",
               page_size: {60, 60},
               margin: 0
             )

    assert diagnostic.operation == :prepare_scrollbar
  end
end
