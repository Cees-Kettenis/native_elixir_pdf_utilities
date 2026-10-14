defmodule NativeElixirPdfUtilities.HtmlToPdf.AffineDrawingGeometryTest do
  use ExUnit.Case, async: false

  alias NativeElixirPdfUtilities.{Limits, Text}
  alias NativeElixirPdfUtilities.HtmlToPdf.{DrawingGeometry, Font, PdfWriter}
  alias NativeElixirPdfUtilities.HtmlToPdf.{EmbeddedViewport, ResourceContext}
  alias NativeElixirPdfUtilities.Pdf.Reader
  alias NativeElixirPdfUtilities.Validators.{HtmlLinkValidator, HtmlValidator}

  setup do
    image = %{
      type: :image,
      x: 0,
      y: 0,
      width: 20,
      height: 20,
      image: %{
        format: :png,
        data: <<255, 0, 0>>,
        width_px: 1,
        height_px: 1,
        color_space: :device_rgb,
        bits_per_component: 8
      },
      link_url: "https://example.com/affine"
    }

    group = %{
      type: :drawing_group,
      width: 20,
      height: 20,
      transform: {1, 0, 0, 1, 0, 0},
      clip: %{x: 0, y: 0, width: 20, height: 20},
      boxes: [image]
    }

    {:ok, image: image, group: group}
  end

  test "rotation and reflection preserve native image paint and exact link corners", %{
    group: group
  } do
    for matrix <- [{0, 1, -1, 0, 50, 20}, {-1, 0, 0, 1, 50, 20}] do
      assert {:ok, pdf} =
               PdfWriter.render([%{size: {100, 100}, boxes: [%{group | transform: matrix}]}],
                 compress_streams: false
               )

      assert pdf =~ "q #{Enum.join(Tuple.to_list(matrix), " ")} cm 0 0 20 20 re W n"
      assert pdf =~ "20 0 0 20 0 0 cm /Im1 Do"
      assert {:ok, context} = Reader.read_validated(pdf)

      [%{value: annotation}] =
        Enum.filter(
          Map.values(context.document.objects),
          &match?(%{value: %{"Subtype" => {:name, "Link"}}}, &1)
        )

      assert annotation["Rect"] == [30, 20, 50, 40]
      assert length(annotation["QuadPoints"]) == 8
    end
  end

  test "nested rotated clips restrict a sheared image link to its actual parallelogram", %{
    group: group
  } do
    inner = %{group | transform: {0, 1, -1, 0, 20, 0}, clip: %{x: 0, y: 0, width: 10, height: 20}}
    outer = %{group | transform: {1, 0, 0.5, 1, 10, 20}, boxes: [inner]}
    assert {:ok, pdf} = PdfWriter.render([%{size: {100, 100}, boxes: [outer]}])
    assert {:ok, context} = Reader.read_validated(pdf)

    [annotation] =
      for %{value: %{"Subtype" => {:name, "Link"}} = value} <-
            Map.values(context.document.objects),
          do: value

    assert annotation["Rect"] == [10, 20, 35, 30]

    quads =
      annotation["QuadPoints"]
      |> Enum.chunk_every(8)
      |> Enum.map(fn [x1, y1, x2, y2, x3, y3, x4, y4] ->
        [{x1, y1}, {x2, y2}, {x4, y4}, {x3, y3}]
      end)

    refute Enum.any?(quads, &DrawingGeometry.clip_contains?({:polygon, &1}, 11, 29))
    assert Enum.any?(quads, &DrawingGeometry.clip_contains?({:polygon, &1}, 25, 25))

    assert Enum.all?(List.flatten(quads), fn {x, y} ->
             x >= 10 + (y - 20) / 2 and x <= 30 + (y - 20) / 2 and y >= 20 and y <= 30
           end)
  end

  test "sheared CSS selected-font paint retains its source canvas and logical link box", %{
    group: group
  } do
    assert {:ok, registry} = Font.load_registry([])
    assert {:ok, _, font} = Font.resolve("DejaVu Sans", 400, :normal, registry)

    text = %{
      type: :text,
      text: "Native N",
      x: 0,
      y: 3,
      width: 40,
      line_height: 9,
      font: Font.pdf_name(font),
      font_face: font,
      font_size: 6,
      color: {0, 0, 0},
      snap_to_css_pixel_grid: true,
      link_url: "https://example.com/text"
    }

    group = %{
      group
      | width: 50,
        height: 20,
        transform: {1, 0.25, 0.5, 1, 20, 20},
        clip: nil,
        boxes: [text]
    }

    assert {:ok, pdf} =
             PdfWriter.render([%{size: {100, 100}, boxes: [group]}], compress_streams: false)

    assert pdf =~ "q 1 0.25 0.5 1 20 20 cm"
    assert pdf =~ "q 0.23999999 0 0 -0.23999999 0 20 cm 3.125 0 0 3.125 0 0 cm BT"
    assert pdf =~ "/QuadPoints ["
    assert {:ok, extracted} = Text.extract(pdf)
    assert extracted =~ "Native N"
  end

  test "authored image-map quadrilaterals are transformed before actual ancestor clipping", %{
    group: group
  } do
    region = %{
      type: :link_region,
      x: 0,
      y: 0,
      width: 20,
      height: 20,
      link_quad_points: [0, 20, 20, 20, 0, 0, 20, 0],
      link_url: "https://example.com/map"
    }

    group = %{group | transform: {1, 1, 0, 1, 20, 10}, boxes: [region]}
    assert {:ok, pdf} = PdfWriter.render([%{size: {100, 100}, boxes: [group]}])
    assert {:ok, context} = Reader.read_validated(pdf)

    [annotation] =
      for %{value: %{"Subtype" => {:name, "Link"}} = value} <-
            Map.values(context.document.objects),
          do: value

    assert annotation["Rect"] == [20, 10, 40, 50]

    quads =
      annotation["QuadPoints"]
      |> Enum.chunk_every(8)
      |> Enum.map(fn [x1, y1, x2, y2, x3, y3, x4, y4] ->
        [{x1, y1}, {x2, y2}, {x4, y4}, {x3, y3}]
      end)

    refute Enum.any?(quads, &DrawingGeometry.clip_contains?({:polygon, &1}, 21, 49))
    assert Enum.any?(quads, &DrawingGeometry.clip_contains?({:polygon, &1}, 30, 30))
  end

  test "singular and serialized-singular affine matrices return canonical diagnostics", %{
    group: group
  } do
    for matrix <- [{1, 1, 1, 1, 20, 20}, {0.00001, 0, 0, 1, 20, 20}] do
      assert {:error, {:invalid_pdf_input, diagnostic}} =
               PdfWriter.render([%{size: {100, 100}, boxes: [%{group | transform: matrix}]}])

      assert diagnostic.stage == :pdf
      assert diagnostic.operation == :write_pdf
      assert diagnostic.message =~ "nondegenerate"
    end
  end

  test "affine clipping work uses shared Limits and cannot accept caller-prepared quads", %{
    group: group,
    image: image
  } do
    original = Limits.effective()
    on_exit(fn -> Limits.install(original) end)
    Limits.install(%{original | max_clip_projection_work: 1})

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             PdfWriter.render([
               %{size: {100, 100}, boxes: [%{group | transform: {1, 0.5, 0, 1, 20, 20}}]}
             ])

    assert diagnostic.message =~ "max_clip_projection_work"
    Limits.install(original)
    forged = Map.put(image, :drawing_affine_link_quads, [[{0, 0}, {20, 0}, {20, 20}, {0, 20}]])

    assert {:error, {:invalid_pdf_input, _}} =
             PdfWriter.render([%{size: {100, 100}, boxes: [forged]}])
  end

  test "scoped child fragment destinations follow the first actually visible affine placement" do
    child =
      "<style>html,body{margin:0;font-size:10pt;line-height:12pt}p{margin:0}</style><p><a href='#target'>Jump</a></p><p id=target>Selected target</p>"

    ResourceContext.run([], HtmlValidator.new_image_budget(), fn context ->
      assert {:ok, surface} = EmbeddedViewport.prepare_html({:srcdoc, child}, context)
      assert {:ok, group} = EmbeddedViewport.layout(surface, 90, 60)
      group = %{group | transform: {1, 1, 0, 1, 20, 20}}
      second = DrawingGeometry.translate_box(group, 5, 3)

      rounded = %{
        group
        | boxes:
            Enum.map(group.boxes, fn box ->
              if Map.get(box, :html_targets, []) == [] do
                box
              else
                Map.put(box, :clip, %{
                  x: box.x,
                  y: box.y - 20,
                  width: 20,
                  height: 20,
                  border_radius: 10
                })
              end
            end)
      }

      for first <- [%{group | clip: %{x: 0, y: 0, width: 90, height: 1}}, rounded] do
        pages = [%{size: {200, 200}, boxes: [first]}, %{size: {200, 200}, boxes: [second]}]
        assert {:ok, [first, second]} = HtmlLinkValidator.resolve_pages(pages, nil)

        assert Enum.find(hd(first.boxes).boxes, &(Map.get(&1, :text) == "Jump")).link_destination ==
                 {:xyz, 2, 25.0, 71.0}

        assert Enum.find(hd(second.boxes).boxes, &(Map.get(&1, :text) == "Jump")).link_destination ==
                 {:xyz, 2, 25.0, 71.0}

        assert {:ok, pdf} = PdfWriter.render([first, second])
        assert pdf =~ "/QuadPoints ["
      end
    end)
  end

  test "zero-area affine viewports retain source geometry and emit no link", %{
    group: group,
    image: image
  } do
    group = %{
      group
      | transform: {1, 1, 0.5, 1, 20, 20},
        clip: %{x: 0, y: 0, width: 0, height: 20},
        boxes: [image]
    }

    assert {:ok, pdf} = PdfWriter.render([%{size: {100, 100}, boxes: [group]}])
    refute pdf =~ "/Subtype /Link"
    assert {:ok, _} = Reader.read_validated(pdf)
  end

  test "rounded outer and inner affine clips project the actual painted cubic curves", %{
    group: group,
    image: image
  } do
    clip = %{
      x: 0,
      y: 0,
      width: 20,
      height: 20,
      border_radius: 10,
      inner_clip: %{x: 2, y: 2, width: 16, height: 16, border_radius: 8}
    }

    image =
      image |> Map.put(:clip, clip) |> Map.put(:link_rect, %{x: 0, y: 0, width: 20, height: 20})

    group = %{group | transform: {1, 0, 0.5, 1, 20, 20}, boxes: [image]}

    assert {:ok, pdf} =
             PdfWriter.render([%{size: {100, 100}, boxes: [group]}], compress_streams: false)

    assert pdf =~ " c " and pdf =~ " W n "
    assert {:ok, context} = Reader.read_validated(pdf)

    [annotation] =
      for %{value: %{"Subtype" => {:name, "Link"}} = value} <-
            Map.values(context.document.objects),
          do: value

    quads =
      annotation["QuadPoints"]
      |> Enum.chunk_every(8)
      |> Enum.map(fn [x1, y1, x2, y2, x3, y3, x4, y4] ->
        [{x1, y1}, {x2, y2}, {x4, y4}, {x3, y3}]
      end)

    refute Enum.any?(quads, &DrawingGeometry.clip_contains?({:polygon, &1}, 23, 22))
    assert Enum.any?(quads, &DrawingGeometry.clip_contains?({:polygon, &1}, 35, 30))
    assert length(quads) > 4
  end

  test "real issued child widgets reuse canonical affine Form projection without axis coercion" do
    alias NativeElixirPdfUtilities.Validators.{EmbeddedFormValidator, HtmlFormValidator}

    ResourceContext.run([], HtmlValidator.new_image_budget(), fn parent ->
      assert {:ok, prepared} =
               EmbeddedViewport.prepare_iframe_document(
                 {:srcdoc,
                  "<body style='margin:0'><input name='rotated' value='Native' style='width:40px;height:20px'></body>"},
                 parent
               )

      assert {:ok, surface} = EmbeddedViewport.style_viewport(prepared, 90, 60)

      assert {:ok, layout} =
               NativeElixirPdfUtilities.HtmlToPdf.Layout.layout_viewport(surface.tree, 90, 60)

      assert {:ok, [_], fields} =
               HtmlFormValidator.prepare_scoped(
                 [%{size: {90, 60}, boxes: layout.boxes}],
                 [],
                 surface.context
               )

      assert {:ok, descriptor} = EmbeddedFormValidator.issue(surface.context, fields)

      group = %{
        type: :drawing_group,
        width: 90,
        height: 60,
        transform: {0, 1, -1, 0, 100, 20},
        clip: %{x: 0, y: 0, width: 90, height: 60},
        boxes: [],
        embedded_widgets: descriptor
      }

      assert {:ok, pdf} = PdfWriter.render([%{size: {160, 160}, boxes: [group]}])
      assert {:ok, [field]} = NativeElixirPdfUtilities.Forms.fields(pdf)
      assert field.value == "Native"
      [left, bottom, right, top] = hd(fields).rect
      assert [%{rect: [widget_left, widget_bottom, widget_right, widget_top]}] = field.widgets
      assert_in_delta widget_left, 100 - top, 0.0001
      assert_in_delta widget_right, 100 - bottom, 0.0001
      assert_in_delta widget_bottom, 20 + left, 0.0001
      assert_in_delta widget_top, 20 + right, 0.0001
      assert {:ok, flat} = NativeElixirPdfUtilities.Forms.flatten(pdf)
      assert {:ok, _} = Reader.read_validated(flat)
    end)
  end
end
