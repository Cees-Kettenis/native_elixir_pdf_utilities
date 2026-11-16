defmodule NativeElixirPdfUtilities.Validators.CssShapeValidatorTest do
  use ExUnit.Case, async: true
  alias NativeElixirPdfUtilities.Validators.CssShapeValidator

  test "basic shape grammar preserves the reference box and computed alpha policy" do
    for source <- [
          "none",
          "circle()",
          "ellipse()",
          "circle(farthest-side at left top)",
          "ellipse(closest-side farthest-side)",
          "border-box",
          "padding-box",
          "content-box",
          "inset(2pt)",
          "inset(1pt 2pt round 3pt)",
          "inset(1pt round 3pt / 4pt)",
          "polygon(evenodd,0 0,20pt 0,0 20pt)",
          "polygon(nonzero,0 0,20pt 0,0 20pt)"
        ] do
      assert {:ok, fields} = CssShapeValidator.prepare("shape-outside", source)
      assert :ok = CssShapeValidator.validate_style(fields, "shape")
    end

    assert {:ok, %{shape_inside: :auto}} = CssShapeValidator.prepare("shape-inside", "auto")

    assert {:ok, %{shape_image_threshold: threshold}} =
             CssShapeValidator.prepare("shape-image-threshold", "-0.5")

    assert threshold == 0
    assert {:ok, %{shape_margin: 2.0}} = CssShapeValidator.prepare("shape-margin", "2pt")

    for source <- [
          "border-box padding-box",
          "circle() circle()",
          "inset(1pt round 2pt / 3pt / 4pt)",
          "polygon(foo,0 0,20pt 0,0 20pt)",
          "path('M0 0')"
        ] do
      assert :error = CssShapeValidator.prepare("shape-outside", source)
    end

    assert :error = CssShapeValidator.prepare("unknown", "auto")
  end

  test "forged typed geometry and decoded images fail before flow execution" do
    for shape <- [
          %{type: :unknown, reference: :border_box},
          %{type: :box, reference: :unknown},
          %{type: :ellipse, reference: :margin_box, radii: [1], position: {0, 0}},
          %{type: :circle, reference: :margin_box, radii: [-1], position: {0, 0}},
          %{
            type: :polygon,
            reference: :margin_box,
            rule: :evenodd,
            points: [{0, 0}, {10, 0}, :bad]
          },
          %{type: :inset, reference: :margin_box, offsets: %{top: 0}, rx: %{}, ry: %{}}
        ] do
      assert {:error, {:invalid_layout, %{stage: :layout, source: "shape", message: message}}} =
               CssShapeValidator.validate_style(%{shape_outside: shape}, "shape")

      assert message != ""
    end

    assert {:error, {:invalid_layout, _}} =
             CssShapeValidator.validate_style(%{shape_image: %{format: :png}}, "image")

    assert :ok =
             CssShapeValidator.validate_style(%{shape_outside: :image, display: :none}, "hidden")

    assert {:ok, %{shape_margin: 20}} =
             CssShapeValidator.compute_style(%{shape_margin: {:rem, 2}, _root_font_size: 10})
  end
end
