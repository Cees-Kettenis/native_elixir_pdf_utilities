defmodule NativeElixirPdfUtilities.HtmlToPdf.TransformedOverflowClipTest do
  use ExUnit.Case, async: true

  alias NativeElixirPdfUtilities.HtmlToPdf.{
    DrawingGeometry,
    HtmlParser,
    PdfWriter,
    ResourceContext,
    Style
  }

  alias NativeElixirPdfUtilities.Pdf.Reader
  alias NativeElixirPdfUtilities.Validators.{CssTransformValidator, HtmlValidator}

  test "containing clips bound scaled link paint in the containing coordinate space" do
    for {scale, width, expected_right} <- [{0.01, 1600, 16}, {2, 80, 80}] do
      scope = scope("scaleX(#{scale})", width, 16)

      image = image(width) |> Map.put(:css_transform_scopes, [scope])
      boxes = DrawingGeometry.clip_boxes([image], %{x: 0, y: 0, width: 80, height: 20})

      pdf = render(boxes)
      assert annotation(pdf)["Rect"] == [0, 0, expected_right, 16]
    end
  end

  test "outer transforms move the clip once while descendants retain their own transforms" do
    child = scope("scaleX(.5)", 80, 16)

    parent = scope("translate(10pt,-20pt) scale(2)", 30, 20)

    image = image(80) |> Map.put(:css_transform_scopes, [child])
    [wrapper] = DrawingGeometry.clip_boxes([image], %{x: 0, y: 0, width: 30, height: 20})
    wrapper = Map.update!(wrapper, :css_transform_scopes, &[parent | &1])

    pdf = render([wrapper])
    assert annotation(pdf)["Rect"] == [10, 20, 70, 52]
  end

  test "clipping an existing scoped group preserves its stacking ancestry without duplicating scopes" do
    scope = scope("scaleX(.5)", 80, 20)

    group = %{
      type: :drawing_group,
      width: 80,
      height: 20,
      transform: {1, 0, 0, 1, 0, 0},
      boxes: [image(80)],
      css_transform_scopes: [scope],
      _stacking_path: [{[1], 2}]
    }

    [wrapper] = DrawingGeometry.clip_boxes([group], %{x: 0, y: 0, width: 30, height: 20})
    assert wrapper._stacking_path == group._stacking_path
    assert wrapper.css_transform_scopes == [scope]

    pdf = render([wrapper])
    assert annotation(pdf)["Rect"] == [0, 0, 30, 16]
  end

  test "source fragment clips stay inside the transform while containing clips stay outside" do
    scope = scope("scaleX(.5)", 80, 20)

    for box <- [
          image(80),
          %{
            type: :drawing_group,
            width: 80,
            height: 20,
            transform: {1, 0, 0, 1, 0, 0},
            boxes: [image(80)]
          }
        ] do
      boxes =
        [Map.put(box, :css_transform_scopes, [scope])]
        |> DrawingGeometry.clip_boxes(%{x: 0, y: 0, width: 30, height: 20})
        |> DrawingGeometry.clip_boxes(%{x: 0, y: 0, width: 20, height: 20}, :source)
        |> DrawingGeometry.translate_boxes(6, 0)

      assert annotation(render(boxes))["Rect"] == [6, 0, 16, 16]
    end
  end

  test "clips on separate members of a shared transform retain distinct containing viewports" do
    scope = scope("scaleX(.5)", 80, 20)
    box = image(80) |> Map.put(:css_transform_scopes, [scope])

    boxes =
      DrawingGeometry.clip_boxes([box], %{x: 0, y: 0, width: 20, height: 20}) ++
        DrawingGeometry.clip_boxes([box], %{x: 0, y: 0, width: 30, height: 20})

    assert {:ok, context} = Reader.read_validated(render(boxes))

    rectangles =
      for %{value: %{"Subtype" => {:name, "Link"}, "Rect" => rect}} <-
            Map.values(context.document.objects),
          do: rect

    assert Enum.sort(rectangles) == [[0, 0, 20, 16], [0, 0, 30, 16]]
  end

  test "translated rounded containing clips and empty intersections retain their geometry" do
    scope = scope("scaleX(.5)", 80, 20)
    box = image(80) |> Map.put(:css_transform_scopes, [scope])

    clip = %{
      x: 0,
      y: 0,
      width: 30,
      height: 20,
      inner_clip: %{x: 2, y: 2, width: 26, height: 16, border_radius: 2}
    }

    [translated] =
      DrawingGeometry.clip_boxes([box], clip) |> DrawingGeometry.translate_boxes(5, 10)

    assert :ok = CssTransformValidator.validate_box_scopes([translated])
    [{_, moved}] = translated.css_parent_clips
    assert moved.x == 5
    assert moved.y == 10
    assert moved.inner_clip.x == 7
    assert moved.inner_clip.y == 12

    assert NativeElixirPdfUtilities.HtmlToPdf.PageGeometry.box_vertical_bounds(translated) ==
             {26, 10}

    [outside] = DrawingGeometry.clip_boxes([box], %{x: 0, y: 40, width: 30, height: 20})

    assert NativeElixirPdfUtilities.HtmlToPdf.PageGeometry.box_vertical_bounds(outside) ==
             {16, 16}
  end

  test "clipped tall transformed descendants do not create overflow sheets" do
    for parent <- ["", "transform:translateX(0);", "transform:scale(0);"],
        child <- ["scaleY(2)", "translateY(500px)", "scaleY(0)"] do
      html = """
      <style>@page{size:200px 100px;margin:0}html,body{margin:0}</style>
      <div style="#{parent}width:80px;height:40px;overflow:hidden">
        <div style="width:80px;height:400px;transform:#{child};transform-origin:0 0;background:red"></div>
      </div>
      """

      assert {:ok, pdf} =
               NativeElixirPdfUtilities.HtmlToPdf.render(html, system_font_discovery: false)

      assert {:ok, 1} = NativeElixirPdfUtilities.Info.page_count(pdf)
    end
  end

  test "containing overflow preserves all physical percentage-transform fragments" do
    html = """
    <style>@page{size:200px 100px;margin:0}html,body{margin:0}</style>
    <div style="width:80px;height:400px;overflow:hidden">
      <div style="width:80px;height:400px;transform:translateY(20%);transform-origin:0 0;background:red"></div>
    </div>
    """

    assert {:ok, pdf} =
             NativeElixirPdfUtilities.HtmlToPdf.render(html, system_font_discovery: false)

    assert {:ok, 4} = NativeElixirPdfUtilities.Info.page_count(pdf)
  end

  test "pagination and direct materialization diagnose malformed containing clip metadata" do
    scope = scope("scaleX(.5)", 80, 20)
    clip = %{x: 0, y: 0, width: 30, height: 20}
    box = image(80) |> Map.put(:css_transform_scopes, [scope])

    for clips <- [
          nil,
          :bad,
          [:bad],
          [{scope.id, nil}],
          [{:bad, clip}],
          [{make_ref(), clip}],
          [{scope.id, %{clip | y: "bad"}}],
          [{scope.id, %{clip | width: -1}}],
          [{scope.id, Map.put(clip, :inner_clip, %{clip | height: "bad"})}]
        ] do
      malformed = Map.put(box, :css_parent_clips, clips)
      layout = %{type: :layout, page_size: {200, 100}, margin: 0, boxes: [malformed]}

      assert {:error, {:invalid_layout, diagnostic}} =
               NativeElixirPdfUtilities.HtmlToPdf.Pagination.paginate(layout)

      assert diagnostic.stage == :layout
      assert diagnostic.reason == :invalid_layout
      assert diagnostic.operation == :resolve_css_transform
      assert diagnostic.source == "css_parent_clips"
      assert diagnostic.message =~ "retained transform scope"

      assert {:error, {:invalid_layout, diagnostic}} =
               CssTransformValidator.materialize([%{size: {200, 100}, boxes: [malformed]}], [])

      assert diagnostic.stage == :layout
      assert diagnostic.operation == :resolve_css_transform
      assert diagnostic.source == "css_parent_clips"
    end

    orphan = image(80) |> Map.put(:css_parent_clips, [{scope.id, clip}])

    group = %{
      type: :drawing_group,
      width: 200,
      height: 100,
      transform: {1, 0, 0, 1, 0, 0},
      boxes: [orphan]
    }

    assert {:error, {:invalid_layout, %{source: "css_parent_clips"}}} =
             CssTransformValidator.materialize([%{size: {200, 100}, boxes: [group]}], [])

    assert {:error,
            {:invalid_layout, %{operation: :resolve_css_transform, source: "css_parent_clips"}}} =
             NativeElixirPdfUtilities.HtmlToPdf.Pagination.paginate(%{
               type: :layout,
               page_size: {200, 100},
               margin: 0,
               boxes: [group]
             })
  end

  defp scope(transform, width, height) do
    assert {:ok, dom} =
             HtmlParser.parse("<div style='transform:#{transform};transform-origin:0 0'></div>")

    assert {:ok, %{children: [%{style: style}]}} =
             Style.compute(dom, system_font_discovery: false)

    assert {:ok, scope} =
             CssTransformValidator.prepare_scope(style, %{
               x: 0,
               top: 0,
               width: width,
               height: height
             })

    scope
  end

  defp render(boxes) do
    ResourceContext.run([], HtmlValidator.new_image_budget(), fn context ->
      assert {:ok, pages} =
               CssTransformValidator.materialize([%{size: {200, 100}, boxes: boxes}], [], context)

      assert {:ok, pdf} = PdfWriter.render(pages)
      pdf
    end)
  end

  defp image(width) do
    %{
      type: :image,
      x: 0,
      y: 0,
      width: width,
      height: 16,
      image: %{
        format: :png,
        data: <<255, 0, 0>>,
        width_px: 1,
        height_px: 1,
        color_space: :device_rgb,
        bits_per_component: 8
      },
      link_url: "https://example.com/scaled"
    }
  end

  defp annotation(pdf) do
    assert {:ok, context} = Reader.read_validated(pdf)

    [annotation] =
      for %{value: %{"Subtype" => {:name, "Link"}} = value} <-
            Map.values(context.document.objects),
          do: value

    annotation
  end
end
