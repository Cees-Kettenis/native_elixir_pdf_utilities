defmodule NativeElixirPdfUtilities.HtmlToPdf.ClipGeometryTest do
  use ExUnit.Case, async: false

  alias NativeElixirPdfUtilities.Limits
  alias NativeElixirPdfUtilities.HtmlToPdf.{AffineGeometry, ClipGeometry, DrawingGeometry}
  alias NativeElixirPdfUtilities.Validators.{ClipGeometryValidator, HtmlValidator}

  setup do
    previous = Limits.effective()
    on_exit(fn -> Limits.install(previous) end)
    :ok
  end

  test "rotated and reflected box matrices invert without parsing SVG source" do
    for matrix <- [{0, 2, -2, 0, 13, 5}, {-1, 0, 0, 1, 25, -7}, {1, 0.5, 2, 3, 4, 5}] do
      assert {:ok, inverse} = AffineGeometry.inverse(matrix)
      product = AffineGeometry.multiply(matrix, inverse)

      assert Enum.zip(Tuple.to_list(product), [1, 0, 0, 1, 0, 0])
             |> Enum.all?(fn {actual, expected} -> abs(actual - expected) < 1.0e-12 end)
    end

    for invalid <- [nil, {0, 0, 0, 1, 0, 0}, {:invalid, 0, 0, 1, 0, 0}, {1.0e-300, 0, 0, 1, 0, 0}] do
      assert :error = AffineGeometry.inverse(invalid)
    end
  end

  test "nested HTML box clips retain boundary points and exclude clipped links" do
    outer = DrawingGeometry.clip_expression({0, 0, 20, 20})
    inner = DrawingGeometry.clip_expression({5, 5, 15, 15})
    expression = {:intersection, [outer, inner]}

    assert {:contour, _} = outer
    assert ClipGeometry.contains_point?(expression, 5, 10)
    assert ClipGeometry.contains_point?(expression, 10, 10)
    refute ClipGeometry.contains_point?(expression, 3, 10)
    refute ClipGeometry.contains_point?(expression, 21, 10)
    polygon = [{0, 0}, {20, 0}, {20, 20}, {0, 20}]

    assert {:ok, [[5.0, 15.0, 15.0, 15.0, 5.0, 5.0, 15.0, 5.0]]} =
             ClipGeometryValidator.project(expression, polygon, {0, 0, 20, 20})

    assert {:ok, []} = ClipGeometryValidator.project(inner, polygon, {30, 30, 40, 40})
  end

  test "ordinary rounded HTML clips use the same cubic box commands as PDF paint" do
    HtmlValidator.with_render_budget(fn ->
      clip = %{x: 0, y: 0, width: 20, height: 20, border_radius: 5}
      expression = DrawingGeometry.box_clip_expression(clip, {1, 0, 0, 1, 0, 0})
      assert ClipGeometry.contains_point?(expression, 10, 10)
      refute ClipGeometry.contains_point?(expression, 0, 0)
      assert ClipGeometry.contains_point?(expression, 0, 10)

      assert {:ok, quads} =
               ClipGeometryValidator.project(
                 expression,
                 [{0, 0}, {20, 0}, {20, 20}, {0, 20}],
                 {0, 0, 20, 20}
               )

      assert length(quads) > 1

      assert Enum.all?(quads, fn [x1, y1, x2, y2, x3, y3, x4, y4] ->
               ClipGeometry.convex?([{x1, y1}, {x2, y2}, {x4, y4}, {x3, y3}])
             end)
    end)
  end

  test "invalid PDF link geometry returns canonical actionable diagnostics" do
    for polygon <- [
          nil,
          [],
          [{0, 0}, {1, 0}, {0, 0}, {0, 1}],
          [{0, 0}, {1, 0}, {1, 1}, {0, :invalid}],
          [{0, 0}, {1, 0}, {1, 1}, :invalid]
        ] do
      assert {:error, {:invalid_document, diagnostic}} =
               ClipGeometryValidator.prepare_polygon(polygon)

      assert diagnostic.stage == :layout
      assert diagnostic.reason == :invalid_document
      assert diagnostic.module == ClipGeometryValidator
      assert diagnostic.operation == :project
      assert diagnostic.source == "PDF link polygon"
      assert diagnostic.message =~ "four finite convex corners"
    end

    assert {:error, {:invalid_document, diagnostic}} =
             ClipGeometryValidator.project(
               nil,
               [{0, 0}, {10, 0}, {10, 10}, {0, 10}],
               {:invalid, 0, 10, 10}
             )

    assert diagnostic.source == "PDF clip geometry"
    assert diagnostic.message =~ "finite viewport"
  end

  test "PDF quadrilaterals must stay inside their actual annotation rectangle" do
    box = %{type: :link_region, x: 0, y: 0, width: 10, height: 10}
    assert ClipGeometryValidator.valid_quad?(nil, box)
    assert ClipGeometryValidator.valid_quad?([0, 10, 10, 10, 0, 0, 10, 0], box)
    refute ClipGeometryValidator.valid_quad?([0, 10, 11, 10, 0, 0, 11, 0], box)
    refute ClipGeometryValidator.valid_quad?([0, 0, 0, 0, 0, 0, 0, 0], box)
    refute ClipGeometryValidator.valid_quad?([], box)

    refute ClipGeometryValidator.valid_quad?(
             [0, 10, 10, 10, 0, 0, 10, 0],
             %{box | type: :image}
           )
  end

  test "zero-scale rounded HTML boxes have no visible link projection" do
    HtmlValidator.with_render_budget(fn ->
      clip = %{x: 0, y: 0, width: 20, height: 20, border_radius: 5}
      expression = DrawingGeometry.box_clip_expression(clip, {0, 0, 0, 0, 12, 14})
      refute ClipGeometry.contains_point?(expression, 12, 14)

      assert {:ok, []} =
               ClipGeometryValidator.project(
                 expression,
                 [{0, 0}, {20, 0}, {20, 20}, {0, 20}],
                 {0, 0, 20, 20}
               )
    end)
  end

  test "inverse rejects arithmetic overflow under a deliberately expanded numeric policy" do
    Limits.install(Map.put(Limits.effective(), :max_css_numeric_magnitude, Integer.pow(10, 309)))
    assert :error = AffineGeometry.inverse({1.0e308, 0, 0, 1.0e308, 0, 0})
  end

  test "malformed caller clip expressions return shared diagnostics before geometric projection" do
    for expression <- [
          :invalid,
          {:contour, :invalid},
          {:contour, [{{0, 0}, :invalid}]},
          {:contour, [{{0, 0}, {1, :invalid}}]},
          {:intersection, [:invalid]},
          {:intersection, :invalid}
        ] do
      assert {:error, {:invalid_document, diagnostic}} =
               ClipGeometryValidator.project(
                 expression,
                 [{0, 0}, {20, 0}, {20, 20}, {0, 20}],
                 {0, 0, 20, 20}
               )

      assert diagnostic.stage == :layout
      assert diagnostic.source == "PDF clip geometry"
      assert diagnostic.module == ClipGeometryValidator
      assert diagnostic.operation == :project
      assert diagnostic.message =~ "PDF clip"
    end
  end

  test "clip expressions have their own depth limit, independent of drawing groups" do
    assert Limits.defaults().max_clip_expression_depth == 256

    Limits.install(
      Limits.effective()
      |> Map.put(:max_clip_expression_depth, 2)
      |> Map.put(:max_drawing_group_depth, 1)
    )

    contour = DrawingGeometry.clip_expression({0, 0, 20, 20})
    at_limit = {:intersection, [contour]}
    polygon = [{0, 0}, {20, 0}, {20, 20}, {0, 20}]

    assert {:ok, quads} = ClipGeometryValidator.project(at_limit, polygon, {0, 0, 20, 20})
    assert quads == [[0.0, 20.0, 20.0, 20.0, 0.0, 0.0, 20.0, 0.0]]

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             ClipGeometryValidator.project({:intersection, [at_limit]}, polygon, {0, 0, 20, 20})

    assert diagnostic.stage == :layout
    assert diagnostic.reason == :resource_limit_exceeded
    assert diagnostic.message =~ "max_clip_expression_depth"
    refute diagnostic.message =~ "max_drawing_group_depth"
  end

  test "real projection work consumes the shared generic resource limit" do
    Limits.install(Map.put(Limits.effective(), :max_clip_projection_work, 1))

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             ClipGeometryValidator.project(
               nil,
               [{0, 0}, {10, 0}, {10, 10}, {0, 10}],
               {0, 0, 10, 10}
             )

    assert diagnostic.stage == :layout
    assert diagnostic.reason == :resource_limit_exceeded
    assert diagnostic.message =~ "max_clip_projection_work"
  end
end
