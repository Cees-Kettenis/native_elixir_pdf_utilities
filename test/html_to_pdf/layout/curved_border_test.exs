alias NativeElixirPdfUtilities.TestSupport.Paths

defmodule NativeElixirPdfUtilities.HtmlToPdf.CurvedBorderTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.{Forms, HtmlToPdf, Limits, Text}
  alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, Layout, PdfWriter, Style}
  alias NativeElixirPdfUtilities.Pdf.Reader
  alias NativeElixirPdfUtilities.Validators.WriterValidator

  @fixture Paths.fixture("html_to_pdf/browser_parity/html_curved_border_sides.html")

  test "curved backgrounds and distinct edges preserve source boxes fields and searchable captions" do
    html = File.read!(@fixture)
    assert {:ok, dom} = HtmlParser.parse_document_detailed(html)
    assert {:ok, tree} = Style.compute_detailed(dom)
    assert {:ok, layout} = Layout.layout(tree, page_size: {480, 360}, margin: 0)

    source_boxes =
      layout.boxes
      |> Enum.reject(&(&1.type == :flow_marker))
      |> Enum.map(&Map.take(&1, [:x, :y, :width, :height, :form_owner]))

    pages = [%{size: {480, 360}, boxes: layout.boxes}]
    assert {:ok, context} = WriterValidator.prepare(pages, [])

    assert Enum.map(
             hd(context.pages).boxes,
             &Map.take(&1, [:x, :y, :width, :height, :form_owner])
           ) == source_boxes

    for mode <- [:static, :interactive] do
      assert {:ok, pdf} = HtmlToPdf.render(html, forms: mode, compress_streams: false)
      assert {:ok, context} = Reader.read_validated(pdf)

      groups =
        Enum.count(context.document.objects, fn {_ref, obj} ->
          is_map(obj.value) and is_map(obj.value["Group"]) and
            obj.value["Group"]["S"] == {:name, "Transparency"}
        end)

      assert groups >= 7
      assert {:ok, fields} = Forms.fields(pdf)

      if mode == :static do
        assert fields == []
      else
        assert [field] = fields
        assert field.name == "pick" and field.value == "a" and not field.read_only
        assert Enum.map(field.choices, & &1.value) == ["a", "b"]
        [widget] = field.widgets
        [left, bottom, right, top] = widget.rect

        assert right > left and top > bottom and left >= 0 and bottom >= 0 and right <= 480 and
                 top <= 360

        assert {:ok, filled} = Forms.fill(pdf, %{"pick" => "b"})
        assert {:ok, flat} = Forms.flatten(filled)
        assert {:ok, []} = Forms.fields(flat)
        assert {:ok, text} = Text.extract(flat)
        assert text =~ "Beta" and text =~ "Caption" and text =~ "Following unchanged source boxes"
      end

      assert {:ok, text} = Text.extract(pdf)

      for caption <- [
            "Thin dashed",
            "Thick dashed",
            "Thin dotted",
            "Thick dotted",
            "Unequal widths and colors",
            "Different edge styles",
            "Actual curved clipping and fields"
          ] do
        assert text =~ caption
      end
    end
  end

  test "source contour piece work uses shared limits and invalid surface metadata has a diagnostic" do
    previous = Limits.effective()

    failure =
      try do
        Limits.install(Map.put(previous, :max_clip_projection_work, 1))

        HtmlToPdf.render(
          "<div style='width:100pt;height:30pt;background:#eef;border:2px dashed red;border-radius:8px'>Real</div>",
          forms: :static
        )
      after
        Limits.install(previous)
      end

    assert {:error, {:resource_limit_exceeded, diagnostic}} = failure

    assert diagnostic.reason == :resource_limit_exceeded and
             diagnostic.message =~ "max_clip_projection_work"

    assert is_atom(diagnostic.stage) and is_binary(diagnostic.message)

    invalid = %{
      type: :rect,
      css_border_surface: %{
        alpha: 0.3,
        clip: %{x: 0, y: 0, width: 1, height: 1, border_radius: 0.5}
      }
    }

    assert {:error, {_reason, diagnostic}} = WriterValidator.validate_native_boxes([invalid])
    assert is_atom(diagnostic.stage) and is_binary(diagnostic.message)
  end

  test "short clipped native border hosts remain valid without zero-length PDF dash arrays" do
    for style <- [
          "border:8px dotted #345;border-radius:20px;width:1px;height:1px",
          "border:12px dashed #345;border-radius:20px;width:1px;height:1px"
        ] do
      assert {:ok, pdf} =
               HtmlToPdf.render("<div style='#{style}'></div>",
                 forms: :static,
                 compress_streams: false
               )

      assert {:ok, _} = Reader.read_validated(pdf)
      refute pdf =~ "[0 0] 0 d"
    end
  end

  test "unclipped translated artwork retains its negative local border and link bounds" do
    border = %{
      type: :rect,
      x: -12,
      y: -8,
      width: 30,
      height: 20,
      border_radius: 4,
      fill_color: {0.8, 0.9, 1.0},
      stroke_color: {0.3, 0.2, 0.1},
      stroke_width: 1.5,
      border_widths: %{top: 1.5, right: 1.5, bottom: 1.5, left: 1.5},
      border_styles: %{top: :dashed, right: :dashed, bottom: :dashed, left: :dashed},
      snap_to_css_pixel_grid: true
    }

    link = %{
      type: :text,
      text: "Actual",
      x: -12,
      y: -8,
      width: 30,
      font: "Helvetica",
      font_size: 10,
      color: {0, 0, 0},
      link_url: "https://example.com/actual-negative-artwork"
    }

    group = %{
      type: :drawing_group,
      width: 20,
      height: 20,
      transform: {1, 0, 0, 1, 25, 30},
      boxes: [border, link]
    }

    assert {:ok, pdf} =
             PdfWriter.render([%{size: {100, 100}, boxes: [group]}], compress_streams: false)

    assert {:ok, context} = Reader.read_validated(pdf)

    groups =
      Enum.flat_map(context.document.objects, fn {_ref, obj} ->
        if is_map(obj.value) and is_map(obj.value["Group"]),
          do: [obj.value["BBox"]],
          else: []
      end)

    assert Enum.any?(groups, fn [left, bottom, right, top] ->
             left <= -12 and bottom <= -8 and right >= 18 and top >= 12
           end)

    assert {:ok, ordinary} =
             PdfWriter.render([
               %{
                 size: {100, 100},
                 boxes: [%{group | boxes: [%{border | border_radius: 0}, link]}]
               }
             ])

    assert {:ok, ordinary_context} = Reader.read_validated(ordinary)

    rectangles = fn document ->
      Enum.flat_map(document.objects, fn {_ref, obj} ->
        if is_map(obj.value) and obj.value["Subtype"] == {:name, "Link"},
          do: [obj.value["Rect"]],
          else: []
      end)
    end

    assert [rect] = rectangles.(context.document)
    assert [rect] == rectangles.(ordinary_context.document)
    assert [left, bottom, right, top] = rect
    assert left >= 0 and bottom >= 0 and right > left and top > bottom
    assert pdf =~ "https://example.com/actual-negative-artwork"
  end
end
