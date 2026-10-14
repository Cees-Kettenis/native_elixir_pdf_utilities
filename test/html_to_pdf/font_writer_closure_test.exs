defmodule NativeElixirPdfUtilities.HtmlToPdf.FontWriterClosureTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.{Forms, Limits, Text}
  alias NativeElixirPdfUtilities.HtmlToPdf.{Font, FontFallback, HtmlParser, PdfWriter, Style}
  alias NativeElixirPdfUtilities.Validators.{NativeGlyphSourceValidator, WriterValidator}

  setup do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)

    {:ok, [face]} =
      Font.load_configured_faces([
        %{family: "Selected", data: File.read!("priv/fonts/dejavu/DejaVuSans.ttf")}
      ])

    text = %{
      type: :text,
      text: "A",
      x: 10,
      y: 20,
      width: 10,
      font_size: 12,
      font: Font.pdf_name(face),
      font_face: face,
      color: {0, 0, 0}
    }

    rect = %{
      type: :rect,
      x: 10,
      y: 20,
      width: 10,
      height: 10,
      fill_color: {1, 0, 0},
      stroke_color: {0, 0, 0},
      stroke_width: 1,
      border_radius: 0
    }

    group = %{
      type: :drawing_group,
      width: 60,
      height: 60,
      transform: {1, 0, 0, 1, 0, 0},
      boxes: []
    }

    {:ok, text: text, rect: rect, group: group}
  end

  test "unissued registry owners fail at the canonical validator and ordinary font loads stay unbound",
       %{text: text} do
    alias NativeElixirPdfUtilities.Validators.FontValidator

    assert {:error, {:invalid_document, diagnostic}} =
             FontValidator.with_source_owner(make_ref(), fn ->
               flunk("unissued registry owner ran")
             end)

    assert diagnostic.stage == :font
    assert diagnostic.operation == :template_renderer_context
    assert diagnostic.message =~ "live normal preparation"

    config = %{
      family: text.font_face.family,
      weight: text.font_face.weight,
      style: text.font_face.style,
      source: :configured
    }

    assert FontValidator.observe_loaded_font(config, 0, 0, text.font_face) == text.font_face
    refute Map.has_key?(text.font_face, :__template_font_outcome__)
    assert Process.get({FontValidator, :source_owner}) == nil
    assert Process.get({FontValidator, :registry_loads}) == nil
  end

  test "replacement of actual unsupported DOM scalars retains their original fragments" do
    source = "A\u{10FFFF}B"
    assert {:ok, dom} = HtmlParser.parse_document_detailed("<p>#{source}</p>")
    assert {:ok, styled} = Style.compute_detailed(dom, system_font_discovery: false)
    prepared = NativeGlyphSourceValidator.prepare_tree(styled)
    assert {:ok, resolved} = FontFallback.resolve(prepared, :replace)

    nodes = get_in(resolved, [:children, Access.at(0), :children, Access.at(0), :children])
    assert Enum.map_join(nodes, & &1.text) == "A�B"
    characters = Enum.flat_map(nodes, & &1.style._source_glyph_characters)
    replacement = Enum.find(characters, &(&1.displayed == "�"))
    assert Enum.map_join(replacement.fragments, & &1.text) == "\u{10FFFF}"
    assert Enum.all?(replacement.fragments, &is_reference(&1.owner.node_id))
  end

  test "source position and private writer metadata require genuine compatible primitives", %{
    text: text,
    rect: rect
  } do
    for box <- [
          Map.put(rect, :css_source_positioned, true),
          text |> Map.put(:css_source_positioned, true) |> Map.put(:snap_to_css_pixel_grid, true),
          Map.put(text, :css_source_positioned, :yes),
          Map.put(rect, :initial_select_opacity, %{alpha: 0.7}),
          Map.put(rect, :drawing_link_quad_points, [0, 0, 1, 0, 0, 1, 1, 1]),
          Map.put(rect, :drawing_annotation_rect, {0, 0, 10, 10}),
          Map.put(rect, :drawing_page_height, 100),
          Map.put(rect, :drawing_affine_link_quads, [[{0, 0}, {10, 0}, {10, 10}, {0, 10}]]),
          Map.put(rect, :css_border_surface, %{
            alpha: 0.5,
            clip: %{x: 0, y: 0, width: 10, height: 10}
          }),
          Map.put(rect, :range_frame, {0, 0, 10, 10, 0, 0}),
          Map.put(rect, :range_frame, :invalid)
        ] do
      assert {:error, {:invalid_pdf_input, diagnostic}} =
               WriterValidator.validate_native_boxes([box])

      assert diagnostic.stage == :pdf
      assert diagnostic.message != ""
    end

    assert :ok =
             WriterValidator.validate_native_boxes([Map.put(text, :css_source_positioned, true)])
  end

  test "border opacity surfaces preserve canonical clips and short patterned edges", %{rect: rect} do
    clip = Map.take(rect, [:x, :y, :width, :height])
    surface = Map.put(rect, :css_border_surface, %{alpha: 1.0, clip: clip})
    assert {:ok, context} = WriterValidator.prepare([%{size: {100, 100}, boxes: [surface]}], [])
    assert hd(hd(context.pages).boxes).css_border_surface.clip.border_radius == 0

    for style <- [:dotted, :dashed] do
      box = %{rect | width: 1, height: 1, stroke_width: 2}
      box = Map.put(box, :border_styles, Map.new([:top, :right, :bottom, :left], &{&1, style}))

      assert {:ok, pdf} =
               PdfWriter.render([%{size: {100, 100}, boxes: [box]}], compress_streams: false)

      assert pdf =~ if(style == :dotted, do: "[0 5.3333] 0 d", else: "[5.3333 2.6667] 0 d")
    end
  end

  test "paint bounds include genuine negative artwork and reject derived overflow", %{rect: rect} do
    assert {:ok, {-10, -20, 60, 60}} =
             WriterValidator.prepare_paint_group_bounds(%{width: 60, height: 60}, [
               %{rect | x: -10, y: -20},
               %{type: :flow_marker}
             ])

    assert {:error, {:invalid_pdf_input, diagnostic}} =
             WriterValidator.prepare_paint_group_bounds(%{width: 60, height: 60}, [
               %{rect | x: Limits.get(:max_pdf_numeric_magnitude)}
             ])

    assert diagnostic.message =~ "max_pdf_numeric_magnitude"
  end

  test "invalid group viewport roles and unissued embedded descriptors fail", %{
    group: group,
    rect: rect
  } do
    for box <- [
          %{group | boxes: [Map.put(rect, :snap_to_css_pixel_grid, :yes)]},
          %{group | boxes: [%{type: :actual_text_start, text: "unissued replacement"}]},
          Map.put(group, :viewport_paint_role, :unknown),
          Map.put(group, :embedded_widgets, %{token: :unissued}),
          %{group | transform: {1, 0.1, 0.2, 1, 0, 0}}
          |> Map.put(:embedded_widgets, %{token: :unissued})
        ] do
      assert {:error, {reason, diagnostic}} =
               PdfWriter.render([%{size: {100, 100}, boxes: [box]}])

      assert diagnostic.reason == reason
      assert diagnostic.message != ""
    end
  end

  test "real issued child widgets project without an explicit group clip and respect projection work limits" do
    alias NativeElixirPdfUtilities.HtmlToPdf.{EmbeddedViewport, Layout, ResourceContext}

    alias NativeElixirPdfUtilities.Validators.{
      EmbeddedFormValidator,
      HtmlFormValidator,
      HtmlValidator
    }

    ResourceContext.run([], HtmlValidator.new_image_budget(), fn parent ->
      assert {:ok, prepared} =
               EmbeddedViewport.prepare_iframe_document(
                 {:srcdoc,
                  "<body style='margin:0'><input name='actual' value='A' style='width:40px;height:20px'></body>"},
                 parent
               )

      assert {:ok, surface} = EmbeddedViewport.style_viewport(prepared, 90, 60)
      assert {:ok, layout} = Layout.layout_viewport(surface.tree, 90, 60)

      assert {:ok, _, fields} =
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
        transform: {1, 0.1, 0.2, 1, 10, 20},
        boxes: [],
        embedded_widgets: descriptor
      }

      assert {:ok, pdf} = PdfWriter.render([%{size: {240, 240}, boxes: [group]}])
      assert {:ok, [%{value: "A"}]} = Forms.fields(pdf)
      Limits.install(%{Limits.effective() | max_clip_projection_work: 0})

      assert {:error, {:resource_limit_exceeded, diagnostic}} =
               PdfWriter.render([%{size: {240, 240}, boxes: [group]}])

      assert diagnostic.message =~ "max_clip_projection_work"
    end)
  end

  test "ordinary painted PDF Form image supports declared replacement text without hidden glyphs",
       %{text: text} do
    assert {:ok, source} =
             PdfWriter.render([%{size: {60, 60}, boxes: [text]}], compress_streams: false)

    image = %{format: :pdf_form, data: source, width_px: 1, height_px: 1, logical_text: "A"}
    box = %{type: :image, x: 0, y: 0, width: 60, height: 60, image: image}

    assert {:ok, pdf} =
             PdfWriter.render([%{size: {60, 60}, boxes: [box]}], compress_streams: false)

    assert pdf =~ "/ActualText"
    assert {:ok, "A"} = Text.extract(pdf, layout: false)
    artifacts = ".quality/orchestration/font-writer-closure/evidence/actual-form-image"
    File.mkdir_p!(artifacts)
    File.write!(Path.join(artifacts, "source.pdf"), source)
    File.write!(Path.join(artifacts, "wrapped.pdf"), pdf)

    for value <- [123, <<255>>] do
      assert {:error, {_, diagnostic}} =
               PdfWriter.render([
                 %{size: {60, 60}, boxes: [%{box | image: %{image | logical_text: value}}]}
               ])

      assert diagnostic.message != ""
    end

    Limits.install(%{Limits.effective() | max_rendered_text_bytes: 0})

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             PdfWriter.render([%{size: {60, 60}, boxes: [box]}])

    assert diagnostic.message =~ "max_rendered_text_bytes"
  end

  test "real authored link corners remain clipped to the page and nested source plane", %{
    group: group
  } do
    link = %{
      type: :link_region,
      x: 5,
      y: 5,
      width: 20,
      height: 20,
      link_url: "https://example.test/paint",
      link_quads: [[{5, 5}, {25, 5}, {25, 25}, {5, 25}]]
    }

    assert {:ok, pdf} =
             PdfWriter.render([%{size: {100, 100}, boxes: [%{group | boxes: [link]}]}],
               compress_streams: false
             )

    assert pdf =~ "/Subtype /Link"
    clipped = %{group | boxes: [link]} |> Map.put(:clip, %{x: 40, y: 40, width: 10, height: 10})
    assert {:ok, pdf} = PdfWriter.render([%{size: {100, 100}, boxes: [clipped]}])
    refute pdf =~ "/Subtype /Link"
    affine = %{group | transform: {1, 0.2, 0.1, 1, 0, 0}, boxes: [link]}

    assert {:ok, pdf} =
             PdfWriter.render([%{size: {100, 100}, boxes: [affine]}], compress_streams: false)

    assert pdf =~ "/QuadPoints"
  end

  test "actual repeated affine link projection shares its bounded work meter", %{group: group} do
    alias NativeElixirPdfUtilities.Validators.HtmlValidator

    link = %{
      type: :link_region,
      x: 5,
      y: 5,
      width: 20,
      height: 20,
      link_url: "https://example.test/corners",
      link_quad_points: [5, 5, 25, 5, 5, 25, 25, 25]
    }

    for transform <- [{2, 0, 0, 2, 10, 10}, {1, 0.2, 0.1, 1, 0, 0}] do
      pages = [%{size: {240, 240}, boxes: [%{group | transform: transform, boxes: [link]}]}]

      work =
        HtmlValidator.with_render_budget(fn ->
          assert {:ok, _} = WriterValidator.prepare(pages, [])
          Process.get({HtmlValidator, :render_budget}).max_clip_projection_work
        end)

      assert work > 0
      original = Limits.effective()
      Limits.install(%{original | max_clip_projection_work: work - 1})
      assert {:error, {:resource_limit_exceeded, diagnostic}} = PdfWriter.render(pages)
      assert diagnostic.message =~ "max_clip_projection_work"
      Limits.install(original)
    end
  end

  test "page clipping drops off-page authored corners and preserves border-box image hit areas",
       %{group: group, text: text} do
    link = %{
      type: :link_region,
      x: 110,
      y: 10,
      width: 20,
      height: 20,
      link_url: "https://example.test/outside",
      link_quad_points: [110, 10, 130, 10, 110, 30, 130, 30]
    }

    assert {:ok, pdf} = PdfWriter.render([%{size: {100, 100}, boxes: [link]}])
    refute pdf =~ "/Subtype /Link"
    assert {:ok, source} = PdfWriter.render([%{size: {60, 60}, boxes: [text]}])
    image = %{format: :pdf_form, data: source, width_px: 1, height_px: 1}

    box = %{
      type: :image,
      x: 5,
      y: 5,
      width: 20,
      height: 20,
      image: image,
      link_rect: %{x: 0, y: 0, width: 40, height: 40},
      link_url: "https://example.test/area"
    }

    for transform <- [{1, 0, 0, 1, 0, 0}, {1, 0.1, 0.1, 1, 0, 0}],
        rounded? <- [false, true] do
      clip = %{x: 5, y: 5, width: 20, height: 20}
      clip = if rounded?, do: Map.put(clip, :border_radius, 2), else: clip
      child = Map.put(box, :clip, clip)

      assert {:ok, pdf} =
               PdfWriter.render([
                 %{size: {100, 100}, boxes: [%{group | transform: transform, boxes: [child]}]}
               ])

      assert pdf =~ "/Subtype /Link"
    end
  end

  test "opacity paint allocation rejects a derived Form extent beyond the PDF numeric ceiling", %{
    rect: rect
  } do
    box =
      %{rect | x: Limits.get(:max_pdf_numeric_magnitude)}
      |> Map.put(:css_border_surface, %{alpha: 1.0, clip: %{x: 0, y: 0, width: 60, height: 60}})

    assert {:error, {:invalid_pdf_input, diagnostic}} =
             PdfWriter.render([%{size: {100, 100}, boxes: [box]}])

    assert diagnostic.message =~ "paint group bounds"
  end
end
