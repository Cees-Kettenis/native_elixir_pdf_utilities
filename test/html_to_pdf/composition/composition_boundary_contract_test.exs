defmodule NativeElixirPdfUtilities.HtmlToPdf.CompositionBoundaryContractTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.Limits
  alias NativeElixirPdfUtilities.HtmlToPdf.{DrawingGeometry, EmbeddedViewport, ResourceContext}

  alias NativeElixirPdfUtilities.Validators.{
    CssTransformValidator,
    EmbeddedViewportValidator,
    FramesetValidator,
    HtmlValidator
  }

  setup do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    :ok
  end

  test "moving an marked source box retains its transform origin and authored quads" do
    style = %{css_transform: [{"translatex", [{:length_x, "8px"}]}]}

    box = %{
      type: :rect,
      x: 10,
      y: 20,
      width: 30,
      height: 15,
      link_rect: %{x: 10, y: 20, width: 30, height: 15},
      link_quads: [[{10, 20}, {40, 20}, {40, 35}, {10, 35}]],
      link_quad_points: [10, 35, 40, 35, 10, 20, 40, 20]
    }

    [marked] =
      CssTransformValidator.mark_border_box(style, [box], %{x: 10, top: 35, width: 30, height: 15})

    moved = DrawingGeometry.translate_box(marked, 7, -11)
    assert moved.css_transform_border_box == %{x: 17, top: 24, width: 30, height: 15}
    assert moved.link_rect == %{x: 17, y: 9, width: 30, height: 15}
    assert moved.link_quad_points == [17, 24, 47, 24, 17, 9, 47, 9]
    assert moved.link_quads == [[{17, 9}, {47, 9}, {47, 24}, {17, 24}]]
  end

  test "a tighter containing clip retains the rounded inner boundary" do
    rounded = %{x: 0, y: 0, width: 20, height: 20, border_radius: 10}
    box = %{type: :rect, x: 0, y: 0, width: 20, height: 20, clip: rounded}

    for clip <- [rounded, %{x: -2, y: -2, width: 24, height: 24, inner_clip: rounded}] do
      [clipped] =
        DrawingGeometry.clip_boxes([%{box | clip: clip}], %{x: 0, y: 0, width: 10, height: 20})

      assert clipped.clip.inner_clip == rounded
      expression = DrawingGeometry.box_clip_expression(clipped.clip, {1, 0, 0, 1, 0, 0})

      assert DrawingGeometry.clip_contains?(
               %{bounds: {0, 0, 10, 20}, expression: expression},
               5,
               10
             )

      refute DrawingGeometry.clip_contains?(
               %{bounds: {0, 0, 10, 20}, expression: expression},
               0,
               0
             )
    end

    assert DrawingGeometry.box_clip_expression(nil, {1, 0, 0, 1, 0, 0}) == nil
    assert DrawingGeometry.clip_contains?(nil, -100, 100)
  end

  test "paint-only surfaces accept static artwork but reject interactive fields" do
    ResourceContext.run([], HtmlValidator.new_image_budget(), fn context ->
      assert {:ok, surface} =
               EmbeddedViewport.prepare_html(
                 {:srcdoc, "<body style='margin:0'><p>Actual child</p></body>"},
                 context
               )

      assert {:ok, layout} =
               NativeElixirPdfUtilities.HtmlToPdf.Layout.layout_viewport(surface.tree, 90, 60)

      assert {:ok, boxes} = EmbeddedViewportValidator.prepare_paint(layout, surface, 90, 60)
      assert Enum.any?(boxes, &(Map.get(&1, :text) == "Actual child"))

      assert {:ok, field_surface} =
               EmbeddedViewport.prepare_html(
                 {:srcdoc, "<input name=actual value=Source>"},
                 context
               )

      assert {:ok, layout} =
               NativeElixirPdfUtilities.HtmlToPdf.Layout.layout_viewport(
                 field_surface.tree,
                 90,
                 60
               )

      assert {:error, {:unsupported_form, diagnostic}} =
               EmbeddedViewportValidator.prepare_paint(layout, field_surface, 90, 60)

      assert diagnostic.stage == :layout
      assert diagnostic.message =~ "prepare_composition"
      assert diagnostic.operation == :layout_embedded_viewport
    end)
  end

  test "frameset color and edge inheritance rejects malformed callers before layout" do
    for {attributes, inherited} <- [{:invalid, nil}, {%{}, %{border_color: :invalid}}] do
      assert {:error, {:invalid_document, diagnostic}} =
               FramesetValidator.prepare_color_hints(attributes, inherited)

      assert diagnostic.stage == :layout
      assert diagnostic.message =~ "border-color"
    end

    assert {:ok, plan} = FramesetValidator.prepare(%{"rows" => "*", "cols" => "*"})

    for children <- [
          [
            %{
              tag: "frameset",
              style: %{_frameset: %{outer_edges: {true, :invalid, false, false}}}
            }
          ],
          [%{tag: "div", style: %{}}]
        ] do
      assert {:error, {:invalid_document, diagnostic}} =
               FramesetValidator.prepare_children(plan, children)

      assert diagnostic.message =~ "edges" or diagnostic.message =~ "edge flags"
    end

    Limits.install(%{Limits.effective() | max_html_nodes: 1})

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             FramesetValidator.prepare_children(plan, [%{}, %{}])

    assert diagnostic.message =~ "max_html_nodes"
  end

  test "invalid scoped viewport dimensions retain the shared actionable failure" do
    ResourceContext.run([], HtmlValidator.new_image_budget(), fn context ->
      assert {:ok, surface} = EmbeddedViewport.prepare_html({:srcdoc, "<p>Actual</p>"}, context)
      assert {:error, {:invalid_layout, diagnostic}} = EmbeddedViewport.layout(surface, -1, 30)
      assert diagnostic.message =~ "viewport"
      assert diagnostic.stage == :layout

      assert {:error, {:invalid_layout, diagnostic}} =
               EmbeddedViewport.layout(Map.put(surface, :initial_document_kind, :invalid), 90, 60)

      assert diagnostic.message =~ "document kind"
    end)
  end

  test "nested rounded box clipping controls first visible child destinations" do
    alias NativeElixirPdfUtilities.Validators.HtmlLinkValidator

    ResourceContext.run([], HtmlValidator.new_image_budget(), fn context ->
      assert {:ok, surface} =
               EmbeddedViewport.prepare_html(
                 {:srcdoc,
                  "<style>html,body{margin:0;font-size:10pt;line-height:12pt}p{margin:0}</style><p><a href='#target'>Jump</a></p><p id=target>Selected target</p>"},
                 context
               )

      assert {:ok, group} = EmbeddedViewport.layout(surface, 90, 60)
      group = %{group | transform: {1, 1, 0, 1, 20, 20}}

      first = %{
        group
        | boxes:
            Enum.map(group.boxes, fn box ->
              if Map.get(box, :html_targets, []) == [],
                do: box,
                else:
                  Map.put(box, :clip, %{
                    x: box.x,
                    y: box.y - 20,
                    width: 20,
                    height: 20,
                    inner_clip: %{
                      x: box.x,
                      y: box.y - 20,
                      width: 20,
                      height: 20,
                      border_radius: 10
                    }
                  })
            end)
      }

      second = DrawingGeometry.translate_box(group, 5, 3)

      assert {:ok, [first, _second]} =
               HtmlLinkValidator.resolve_pages(
                 [
                   %{size: {200, 200}, boxes: [first]},
                   %{size: {200, 200}, boxes: [second]}
                 ],
                 nil
               )

      assert Enum.find(hd(first.boxes).boxes, &(Map.get(&1, :text) == "Jump")).link_destination ==
               {:xyz, 2, 25.0, 71.0}
    end)
  end

  test "furniture layout budgets retain the library diagnostic contract" do
    Limits.install(%{Limits.effective() | max_layout_boxes: 1})

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             NativeElixirPdfUtilities.HtmlToPdf.PageFurniture.decorate(
               [%{size: {200, 100}, boxes: []}],
               %{
                 type: :layout,
                 page_size: {200, 100},
                 margin: 20,
                 boxes: [],
                 content_width: 160,
                 content_height: 60
               },
               page_furniture: [header: "<p>One<br>Two</p>"]
             )

    assert diagnostic.stage == :layout
    assert diagnostic.message =~ "max_layout_boxes"
  end

  test "multirow legacy frames paint horizontal dividers and isolated children" do
    assert {:ok, pdf} =
             NativeElixirPdfUtilities.HtmlToPdf.render(
               "<frameset rows='*,*'><frame src='about:blank'><frame src='about:blank'></frameset>",
               page_size: {120, 90},
               margin: 0,
               compress_streams: false
             )

    assert pdf =~ " re f"
    assert {:ok, context} = NativeElixirPdfUtilities.Pdf.Reader.read_validated(pdf)
    assert length(context.document.pages) == 1
  end

  test "initial resource defaults and frame canvases preserve source ownership" do
    ResourceContext.run([], HtmlValidator.new_image_budget(), fn context ->
      alias NativeElixirPdfUtilities.HtmlToPdf.EmbeddedResource
      assert {:ok, initial} = EmbeddedResource.load_initial({:srcdoc, "<p>Source</p>"}, context)
      assert initial.kind == :html
      assert initial.bytes == "<p>Source</p>"
      assert initial.context.document_id != context.document_id

      assert {:ok, prepared} =
               EmbeddedViewport.prepare_iframe_document(
                 {:srcdoc, "<html style='background:red'></html>"},
                 context
               )

      assert {:ok, group} = EmbeddedViewport.layout(%{prepared | host: :frame}, 90, 60)
      assert Enum.any?(group.boxes, &(Map.get(&1, :role) == :frameset_canvas))
    end)
  end

  test "frame source and scene work limits diagnose before child paint" do
    Limits.install(%{Limits.effective() | max_html_numeric_token_bytes: 1})

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             FramesetValidator.prepare_frame(%{"marginwidth" => "12"}, nil)

    assert diagnostic.source == "12"
    assert diagnostic.message =~ "max_html_numeric_token_bytes"

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             FramesetValidator.prepare_frame(%{"frameborder" => "12"}, nil)

    assert diagnostic.source == "12"
    assert diagnostic.message =~ "max_html_numeric_token_bytes"
    Limits.install(Limits.defaults())

    ResourceContext.run([], HtmlValidator.new_image_budget(), fn context ->
      assert {:ok, dom} =
               NativeElixirPdfUtilities.HtmlToPdf.HtmlParser.parse_document_detailed(
                 "<frameset rows='*,*'><frame src='about:blank'><frame src='about:blank'></frameset>"
               )

      assert {:ok, tree} =
               NativeElixirPdfUtilities.HtmlToPdf.Style.compute_detailed(
                 dom,
                 [],
                 context.image_budget
               )

      [node] = tree.children
      budget_key = {HtmlValidator, :render_budget}
      before = Map.get(Process.get(budget_key), :max_html_tree_work, 0)
      assert {:ok, _grid} = FramesetValidator.prepare_layout(node.style._frameset, 120, 90)
      allocated = Map.get(Process.get(budget_key), :max_html_tree_work, 0)
      measured_allocation = allocated - before

      Limits.install(%{
        Limits.effective()
        | max_html_tree_work: allocated + measured_allocation + 1
      })

      assert {:error, {:resource_limit_exceeded, diagnostic}} =
               FramesetValidator.prepare_scene(node, 120, 90, 1)

      assert diagnostic.message =~ "max_html_tree_work"
    end)
  end

  test "disabled child borders suppress the horizontal legacy divider" do
    assert {:ok, pdf} =
             NativeElixirPdfUtilities.HtmlToPdf.render(
               "<frameset rows='*,*'><frame frameborder='0' src='about:blank'><frame frameborder='0' src='about:blank'></frameset>",
               page_size: {120, 90},
               margin: 0
             )

    assert {:ok, document} = NativeElixirPdfUtilities.Pdf.Reader.read_validated(pdf)
    assert length(document.document.pages) == 1
    assert {:error, {:invalid_document, diagnostic}} = FramesetValidator.prepare_children(nil, [])
    assert diagnostic.message =~ "prepared tracks"
  end

  test "legacy styled iframe surfaces reflow the fixed overflow tree" do
    ResourceContext.run([], HtmlValidator.new_image_budget(), fn context ->
      assert {:ok, surface} =
               EmbeddedViewport.prepare_iframe(
                 {:srcdoc,
                  "<body style='margin:0'><div style='width:200pt;height:150pt;background:red'></div></body>"},
                 context
               )

      refute Map.has_key?(surface, :prepared_dom)
      assert {:ok, group} = EmbeddedViewport.layout(surface, 90, 60)
      [client, _scrollbars] = group.boxes
      assert client.client_rect == %{x: 0.0, y: 0.0, width: 78.75, height: 48.75}
    end)
  end

  test "initial SVG documents retain caller fonts and their child context through conversion" do
    root =
      Path.join(System.tmp_dir!(), "native-child-svg-font-#{System.unique_integer([:positive])}")

    File.mkdir_p!(root)
    on_exit(fn -> File.rm_rf!(root) end)

    File.write!(
      Path.join(root, "source.svg"),
      "<svg xmlns='http://www.w3.org/2000/svg' width='80' height='20'><text x='2' y='15' font-family='DejaVu Sans' font-size='12'>Actual</text></svg>"
    )

    font = %{family: "DejaVu Sans", data: File.read!("priv/fonts/dejavu/DejaVuSans.ttf")}

    ResourceContext.run(
      [authorization_root: root],
      HtmlValidator.new_image_budget(),
      fn context ->
        assert {:ok, prepared} =
                 EmbeddedViewport.prepare_iframe_document("source.svg", context,
                   fonts: [font],
                   system_font_discovery: false
                 )

        assert prepared.initial_document_kind == :svg
        assert prepared.source == {:file, Path.join(root, "source.svg")}
        assert prepared.context.document_id != context.document_id
        assert {:ok, group} = EmbeddedViewport.layout(prepared, 90, 60)
        assert [%{type: :image, image: %{format: :png}}] = group.boxes
      end
    )
  end
end
