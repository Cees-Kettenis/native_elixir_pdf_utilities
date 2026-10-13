defmodule NativeElixirPdfUtilities.HtmlToPdf.ScrollbarViewportTest do
  use ExUnit.Case, async: false

  alias NativeElixirPdfUtilities.HtmlToPdf.{
    DrawingGeometry,
    EmbeddedViewport,
    Layout,
    ResourceContext,
    ScrollbarPainter
  }

  alias NativeElixirPdfUtilities.Validators.{EmbeddedViewportValidator, HtmlValidator}

  test "client geometry reflows each axis, rounds logical pixels and keeps the authored clip" do
    ResourceContext.run([], HtmlValidator.new_image_budget(), fn parent ->
      for {content, width, height, cw, ch} <- [
            {"<div style='width:20px;height:20px'></div>", 90, 45, 90, 45},
            {"<div style='height:100px'></div>", 90, 45, 78.75, 45},
            {"<div style='width:160px;height:20px'></div>", 90, 45, 90, 33.75},
            {"<div style='width:110px;height:100px'></div>", 90, 45, 78.75, 33.75},
            {"<div style='height:100px'></div>", 90.5, 45.5, 79.5, 45.75},
            {"<div style='width:200px;height:200px'></div>", 7.5, 7.5, 0, 0},
            {"<div style='height:100px'></div>", 7.5, 45, 0, 33.75},
            {"", 30, 3.75, 18.75, 3.75},
            {"<div style='height:100px'></div>", 30, 0.75, 18.75, 0.75},
            {"<div style='height:100px'></div>", 30, 1.5, 18.75, 1.5}
          ] do
        html = if content == "", do: "", else: "<body style='margin:0'>#{content}</body>"
        assert {:ok, prepared} = EmbeddedViewport.prepare_iframe_document({:srcdoc, html}, parent)
        refute Map.has_key?(prepared, :tree)
        assert {:ok, outer} = EmbeddedViewport.layout(prepared, width, height)
        assert outer.clip == %{x: 0.0, y: 0.0, width: width, height: height}
        client = if Map.has_key?(outer, :client_rect), do: outer, else: hd(outer.boxes)
        assert client.client_rect == %{x: 0.0, y: 0.0, width: cw, height: ch}
        assert client.scroll_offset == %{x: 0.0, y: 0.0}
        assert client.transform == {1, 0, 0, 1, 0, height - ch}
        assert ResourceContext.current() == parent
      end
    end)
  end

  test "deferred preparation does not fetch selected images before the explicit client viewport" do
    ResourceContext.run([], HtmlValidator.new_image_budget(), fn parent ->
      assert {:ok, prepared} =
               EmbeddedViewport.prepare_iframe_document(
                 {:srcdoc, "<img src='missing.png'>"},
                 parent
               )

      assert prepared.context.document_id != parent.document_id
      assert prepared.context.image_budget == parent.image_budget
      assert prepared.options[:system_font_discovery] == false

      assert {:error, {:resource_denied, diagnostic}} =
               EmbeddedViewport.style_viewport(prepared, 90, 45)

      assert diagnostic.stage == :resource
      assert ResourceContext.current() == parent

      assert {:ok, prepared} =
               EmbeddedViewport.prepare_iframe_document({:srcdoc, "<p>Client</p>"}, parent)

      assert {:ok, styled} = EmbeddedViewport.style_viewport(prepared, 78.75, 45)
      assert styled.context == prepared.context
      assert styled.prepared_dom == prepared.prepared_dom
      assert styled.document_url == prepared.document_url
      assert styled.tree.type == :document
      assert styled.client_css_viewport == %{width: 105.0, height: 60.0}
      assert {:ok, restyled} = EmbeddedViewport.style_viewport(prepared, 90, 45)
      assert restyled.context == styled.context
      assert restyled.prepared_dom == styled.prepared_dom
      assert restyled.client_css_viewport == %{width: 120.0, height: 60.0}
      assert {:ok, _group} = EmbeddedViewport.layout(styled, 90, 45)
    end)
  end

  test "scrollbar artwork is a sibling outside document targets and scoped client origin" do
    ResourceContext.run([], HtmlValidator.new_image_budget(), fn parent ->
      html =
        "<body style='margin:0'><a href='#target'>Jump</a><div id='target' style='height:100px'>Target</div></body>"

      assert {:ok, prepared} = EmbeddedViewport.prepare_iframe_document({:srcdoc, html}, parent)
      assert {:ok, %{boxes: [client, ui]}} = EmbeddedViewport.layout(prepared, 90, 45)
      assert client.link_scope == {prepared.context.document_id, prepared.context.tree_scope_id}
      assert client.link_document_origin == %{x: 0, y: 45}
      refute Map.has_key?(ui, :link_scope)
      refute Map.has_key?(ui, :html_targets)
      assert Enum.all?(ui.boxes, &(&1.role == :scrollbar and not Map.has_key?(&1, :html_targets)))
      assert Enum.any?(client.boxes, &(Map.get(&1, :html_targets, []) != []))
    end)
  end

  test "host origins use containing top coordinates including negative half pixels" do
    for {x, top, expected_x, expected_top} <- [
          {-0.375, -0.375, 0.0, 0.0},
          {0.375, 0.375, 0.75, 0.75},
          {1.2, 2.2, 1.5, 2.25}
        ] do
      host = %{
        role: :embedded_viewport_host,
        width: 20,
        height: 10.25,
        transform: {1, 0, 0, 1, x, 100 - top - 10.25}
      }

      assert DrawingGeometry.local_group_transform(host, 100) ==
               {1.0, 0.0, 0.0, 1.0, expected_x, 100 - expected_top - 10.25}

      assert DrawingGeometry.local_group_transform(Map.delete(host, :role), 100) ==
               {1.0, 0.0, 0.0, 1.0, x, Float.round((100 - top - 10.25) * 1.0, 4)}
    end
  end

  test "zero viewports and invalid dimensions retain diagnostic contracts" do
    ResourceContext.run([], HtmlValidator.new_image_budget(), fn parent ->
      assert {:ok, surface} = EmbeddedViewport.prepare_html({:srcdoc, ""}, parent)
      assert {:ok, layout} = Layout.layout_viewport(surface.tree, 0, 0)
      assert layout.page_size == {0.0, 0}

      for {width, height} <- [{-1, 10}, {10, -1}, {:bad, 2}] do
        assert {:error,
                {:invalid_layout,
                 %{stage: :layout, operation: :layout_viewport, message: message}}} =
                 Layout.layout_viewport(surface.tree, width, height)

        assert message =~ "nonnegative"
      end

      assert {:error, {:invalid_layout, %{operation: :layout_viewport}}} =
               Layout.layout_viewport(%{}, 10, 10)
    end)
  end

  test "viewport preparation rejects malformed descriptors and percentage sizing cycles" do
    assert {:error, {:invalid_layout, %{operation: :style_viewport, message: message}}} =
             EmbeddedViewport.style_viewport(%{}, 90, 45)

    assert message =~ "prepared DOM"

    ResourceContext.run([], HtmlValidator.new_image_budget(), fn parent ->
      assert {:ok, prepared} =
               EmbeddedViewport.prepare_iframe_document(
                 {:srcdoc,
                  "<body style='margin:0'><div style='width:100%;aspect-ratio:1.9'></div></body>"},
                 parent
               )

      assert {:error,
              {:not_implemented, %{operation: :layout_embedded_viewport, message: message}}} =
               EmbeddedViewport.layout(prepared, 90, 45)

      assert message =~ "cycles"
      assert ResourceContext.current() == parent
    end)
  end

  test "native arrow paths paint triangles and reject unsupported shapes before writing" do
    alias NativeElixirPdfUtilities.HtmlToPdf.PdfWriter
    alias NativeElixirPdfUtilities.Validators.WriterValidator

    for {direction, path} <- [
          {:up, "0 0 m 6 0 l 3 3 l h"},
          {:down, "0 3 m 6 3 l 3 0 l h"},
          {:left, "6 0 m 6 3 l 0 1.5 l h"},
          {:right, "0 0 m 0 3 l 6 1.5 l h"}
        ] do
      [box] =
        ScrollbarPainter.paint([
          %{kind: :arrow, direction: direction, x: 0, y: 0, width: 6, height: 3, border_radius: 0}
        ])

      assert {:ok, pdf} = PdfWriter.render([%{size: {30, 30}, boxes: [box]}])
      assert pdf =~ path

      assert {:error, {:invalid_pdf_input, %{stage: :pdf, message: message}}} =
               WriterValidator.prepare(
                 [
                   %{
                     size: {30, 30},
                     boxes: [Map.put(box, :shape, {:triangle, :invalid})]
                   }
                 ],
                 []
               )

      assert message != ""
    end
  end

  test "inline iframe top and bottom alignment preserve their respective shared edges" do
    alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, Style}

    for alignment <- ["top", "bottom"] do
      ResourceContext.run([], HtmlValidator.new_image_budget(), fn _parent ->
        html =
          "<body style='margin:0'><iframe style='border:0;width:30px;height:60px;vertical-align:#{alignment}'></iframe><iframe style='border:0;width:30px;height:10px;vertical-align:#{alignment}'></iframe></body>"

        assert {:ok, dom} = HtmlParser.parse_document_detailed(html)
        assert {:ok, tree} = Style.compute_detailed(dom)
        assert {:ok, layout} = Layout.layout(tree, page_size: {180, 120}, margin: 0)

        [first, second] =
          Enum.filter(layout.boxes, &(Map.get(&1, :role) == :embedded_viewport_host))

        if alignment == "top",
          do: assert_in_delta(first.y + first.height, second.y + second.height, 0.0001),
          else: assert_in_delta(first.y, second.y, 0.0001)
      end)
    end
  end

  test "scrollbar cycles diagnose and empty artwork stays empty" do
    surface = %{host: :iframe, tree: %{children: []}}
    initial = EmbeddedViewportValidator.initial_viewport(surface, 90, 45)

    layout = %{
      page_size: {90, 45},
      content_width: 90,
      content_height: 45,
      boxes: [%{type: :rect, x: 0, y: -10, width: 90, height: 55}]
    }

    previous = %{initial | seen: MapSet.new([{false, false}, {false, true}])}

    assert {:error, {:not_implemented, diagnostic}} =
             EmbeddedViewportValidator.prepare_scroll_geometry(layout, surface, 90, 45, previous)

    assert diagnostic.operation == :layout_embedded_viewport
    assert diagnostic.source == "iframe" and diagnostic.message =~ "cycles"
    assert EmbeddedViewportValidator.prepare_scrollbar_parts(initial) == {:ok, []}
    assert ScrollbarPainter.paint([]) == []
  end
end
