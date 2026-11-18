defmodule NativeElixirPdfUtilities.HtmlToPdf.AdvancedLayoutLimitsTest do
  use ExUnit.Case, async: false

  alias NativeElixirPdfUtilities.HtmlToPdf.{
    AnchorLayout,
    BorderShape,
    GridGapPaint,
    OverflowGeometry
  }

  alias NativeElixirPdfUtilities.Limits

  test "named fallbacks propagate a shared CSS substitution budget failure" do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    Limits.install(%{limits | max_work: 25})

    block = %{x: 0.0, top: 80.0, width: 100.0, height: 80.0}

    style = %{
      position_try_fallbacks: [{:rule, "--try", []}],
      _position_tries: %{"--try" => [{"width", "var(--x)"}]},
      _custom_properties: %{"--x" => "bananas"}
    }

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             AnchorLayout.place(%{style: style}, block, {[], []}, fn _, _ ->
               flunk("The fallback declarations must respect the shared budget before layout")
             end)

    assert diagnostic.message =~ "max_work"
  end

  test "percentage geometry remains bounded when it meets a used border or gap" do
    alias NativeElixirPdfUtilities.Validators.{
      CssBorderRadiusValidator,
      CssBorderShapeValidator,
      CssGapRuleValidator
    }

    style = %{color: {1, 0, 0}}
    box = %{x: 0.0, y: 0.0, width: 1000.0, height: 1000.0}

    rect =
      Map.merge(box, %{
        border_widths: %{top: 2.0, right: 2.0, bottom: 2.0, left: 2.0},
        border_styles: %{top: :solid, right: :solid, bottom: :solid, left: :solid},
        border_colors: %{top: {1, 0, 0}, right: {1, 0, 0}, bottom: {1, 0, 0}, left: {1, 0, 0}},
        border_radius: 0.0,
        stroke_width: 2.0,
        type: :rect,
        fill_color: nil,
        stroke_color: {1, 0, 0}
      })

    assert {:ok, shape} =
             CssBorderShapeValidator.prepare(style, "border-shape", "circle(1000000000%)")

    assert {:render_resource_limit, {:error, {_reason, _}}} =
             catch_throw(BorderShape.contours(shape, rect))

    assert {:ok, shape} = CssBorderShapeValidator.prepare(style, "border-shape", "circle(45%)")
    far_edge = %{rect | x: Limits.get(:max_numeric_magnitude) - 10.0}

    assert {:render_resource_limit, {:error, {:invalid_layout, diagnostic}}} =
             catch_throw(BorderShape.contours(shape, far_edge))

    assert diagnostic.message =~ "clip"

    assert {:ok, clip} =
             CssBorderShapeValidator.prepare(style, "border-top-clip", "calc(100% / 0)")

    assert {:render_resource_limit, {:error, {:resource_limit_exceeded, _}}} =
             catch_throw(BorderShape.apply(clip, rect, [rect]))

    assert {:ok, fields} = CssGapRuleValidator.prepare(style, "rule-inset", "calc(100% / 0)")
    grid = Map.merge(fields, %{column_rule_style: :solid, color: {1, 0, 0}})
    area = %{x: 0.0, top: 1000.0, width: 1000.0, rtl?: false}

    assert {:render_resource_limit, {:error, {:resource_limit_exceeded, _}}} =
             catch_throw(
               GridGapPaint.grid(grid, [], area, {[490.0, 490.0], 20.0}, {[1000.0], 0.0})
             )

    assert {:ok, fields} =
             CssBorderRadiusValidator.prepare(
               style,
               "border-top-left-radius",
               "calc(100% / 0) 6pt"
             )

    style = Map.merge(fields, %{overflow: :hidden, corner_top_left_shape: :square})

    assert {:render_resource_limit, {:error, {:resource_limit_exceeded, _}}} =
             catch_throw(OverflowGeometry.clip([], style, box))
  end
end
