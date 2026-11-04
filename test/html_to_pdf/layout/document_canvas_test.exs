defmodule NativeElixirPdfUtilities.HtmlToPdf.DocumentCanvasTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.Limits

  alias NativeElixirPdfUtilities.HtmlToPdf.{
    EmbeddedViewport,
    HtmlParser,
    Layout,
    ResourceContext,
    Style
  }

  alias NativeElixirPdfUtilities.Validators.{HtmlValidator, ResourceValidator}

  setup do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    :ok
  end

  test "root background wins the canvas while the body paints its box" do
    ResourceContext.run([], HtmlValidator.new_image_budget(), fn parent ->
      source =
        "<!doctype html><style>html{background:red}body{background:blue;margin:10px;width:20px;height:10px}</style><body></body>"

      assert {:ok, dom} = HtmlParser.parse_document_detailed(source)
      assert {:ok, tree} = Style.compute_detailed(dom, [], parent.image_budget)
      assert tree.document_canvas.style.background_color == {1.0, 0.0, 0.0}
      refute tree.document_canvas.body_background_propagated
      [body] = tree.children
      assert body.style.background_color == {0.0, 0.0, 1.0}
      assert {:ok, layout} = Layout.layout_viewport(tree, 90, 60)
      canvas = Enum.find(layout.boxes, &(Map.get(&1, :position_anchor) == :canvas))

      assert {canvas.x, canvas.y, canvas.width, canvas.height, canvas.fill_color} ==
               {0, 0, 90, 60, {1.0, 0.0, 0.0}}

      body_box =
        Enum.find(
          layout.boxes,
          &(&1.type == :rect and Map.get(&1, :fill_color) == {0.0, 0.0, 1.0})
        )

      assert {body_box.x, body_box.width, body_box.height} == {7.5, 15.0, 7.5}
      assert ResourceContext.current() == parent
    end)
  end

  test "transparent roots propagate body background and hidden bodies retain root canvas" do
    ResourceContext.run([], HtmlValidator.new_image_budget(), fn parent ->
      for {root, body, color, propagated} <- [
            {"background:transparent", "background:blue", {0.0, 0.0, 1.0}, true},
            {"background:rgba(255,0,0,0)", "background:blue", {0.0, 0.0, 1.0}, true},
            {"background:red", "display:none;background:blue", {1.0, 0.0, 0.0}, false}
          ] do
        assert {:ok, dom} =
                 HtmlParser.parse_document_detailed(
                   "<!doctype html><html style='#{root}'><body style='#{body}'></body></html>"
                 )

        assert {:ok, tree} = Style.compute_detailed(dom, [], parent.image_budget)
        assert tree.document_canvas.style.background_color == color
        assert tree.document_canvas.body_background_propagated == propagated
        assert {:ok, layout} = Layout.layout_viewport(tree, 90, 60)

        assert Enum.count(layout.boxes, &(&1.type == :rect and Map.get(&1, :fill_color) == color)) ==
                 1
      end
    end)
  end

  test "per-styling viewport media choices preserve source scope and cannot reuse a previous canvas" do
    ResourceContext.run([], HtmlValidator.new_image_budget(), fn parent ->
      source =
        "<!doctype html><style>html{background:red}@media (min-width:100px){html{background:blue}}</style><body></body>"

      assert {:ok, prepared} = EmbeddedViewport.prepare_iframe_document({:srcdoc, source}, parent)
      assert {:ok, small} = EmbeddedViewport.style_viewport(prepared, 60, 45)
      assert {:ok, large} = EmbeddedViewport.style_viewport(prepared, 90, 45)
      assert small.tree.document_canvas.style.background_color == {1.0, 0.0, 0.0}
      assert large.tree.document_canvas.style.background_color == {0.0, 0.0, 1.0}
      assert small.context == large.context and small.context.image_budget == parent.image_budget
      assert small.source == {:srcdoc, source}
      assert :ok = ResourceValidator.validate_context(small.context)
      assert ResourceContext.current() == parent
    end)
  end

  test "malformed public prepared canvas and canvas-box limits return diagnostics" do
    assert {:ok, dom} = HtmlParser.parse_document_detailed("<div></div>")
    assert {:ok, tree} = Style.compute_detailed(dom)

    for plan <- [
          :invalid,
          %{style: %{}, body_background_propagated: 12},
          %{style: %{background_color: :invalid}, body_background_propagated: false}
        ] do
      assert {:error, {:invalid_document, diagnostic}} =
               Layout.layout_viewport(Map.put(tree, :document_canvas, plan), 90, 60)

      assert diagnostic.stage == :layout and diagnostic.operation == :layout_document_canvas
      assert diagnostic.reason == :invalid_document and diagnostic.module == HtmlValidator
    end

    ResourceContext.run([], HtmlValidator.new_image_budget(), fn parent ->
      assert {:ok, dom} =
               HtmlParser.parse_document_detailed(
                 "<html style='background:red'><body style='display:none'></body></html>"
               )

      assert {:ok, tree} = Style.compute_detailed(dom, [], parent.image_budget)
      Limits.install(Map.put(Limits.effective(), :max_layout_boxes, 0))

      assert {:error, {:resource_limit_exceeded, diagnostic}} =
               Layout.layout_viewport(tree, 90, 60)

      assert diagnostic.message =~ "max_layout_boxes" and ResourceContext.current() == parent
      :callback_continued
    end)
    |> then(&assert(&1 == :callback_continued))
  end
end
