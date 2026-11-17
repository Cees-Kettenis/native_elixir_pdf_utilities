defmodule NativeElixirPdfUtilities.HtmlToPdf.CssMotionTest do
  use ExUnit.Case, async: true
  alias NativeElixirPdfUtilities.HtmlToPdf.{Font, HtmlParser, Style}
  alias NativeElixirPdfUtilities.Validators.{CssMotionValidator, CssTransformValidator}

  setup do
    {:ok, registry} = Font.load_registry(system_font_discovery: false)
    {:ok, _, font} = Font.resolve("DejaVu Sans", 400, :normal, registry)

    {:ok,
     style: %{
       font_face: font,
       font_size: 12.0,
       line_height: 15.0,
       _root_font_face: font,
       _root_font_size: 12.0,
       _root_line_height: 15.0,
       _offset_reference: %{x: 10.0, top: 100.0, width: 100.0, height: 80.0}
     }}
  end

  test "path endpoints, percentages, anchors and clamping have independent positions", %{
    style: style
  } do
    assert {:ok, style} = CssMotionValidator.prepare(style, "offset-path", "path('M0 0L80 0')")
    assert {:ok, style} = CssMotionValidator.prepare(style, "offset-anchor", "left top")
    assert {:ok, style} = CssMotionValidator.prepare(style, "offset-rotate", "0deg")

    for {distance, x} <- [
          {"0%", 10.0},
          {"50%", 40.0},
          {"100%", 70.0},
          {"150%", 70.0},
          {"-50%", 10.0}
        ] do
      assert {:ok, used} = CssMotionValidator.prepare(style, "offset-distance", distance)

      assert {:ok, {_, _, _, _, tx, ty}} =
               CssMotionValidator.resolve(used, %{x: 20.0, top: 90.0, width: 12.0, height: 9.0})

      assert_in_delta tx, x - 20.0, 0.00001
      assert_in_delta ty, 10.0, 0.00001
      assert CssMotionValidator.valid_style?(used)

      assert {:ok, _} =
               CssTransformValidator.resolve(used, %{x: 20.0, top: 90.0, width: 12.0, height: 9.0})
    end
  end

  test "ray bearings, contain and offset starting positions share the containing reference", %{
    style: style
  } do
    for source <- [
          "ray(90deg)",
          "ray(90deg farthest-side contain)",
          "ray(45deg closest-corner at 25% 75%)",
          "ray(0deg sides)",
          "ray(180deg farthest-corner)"
        ] do
      assert {:ok, used} = CssMotionValidator.prepare(style, "offset-path", source)
      assert CssMotionValidator.valid_style?(used)

      assert {:ok, _} =
               CssMotionValidator.resolve(used, %{x: 20.0, top: 90.0, width: 12.0, height: 9.0})
    end

    for {property, value} <- [
          {"offset-path", "ray(foo)"},
          {"offset-distance", "auto"},
          {"offset-rotate", "auto reverse"},
          {"offset-anchor", "normal"}
        ] do
      assert :error = CssMotionValidator.prepare(style, property, value)
    end
  end

  test "closed shape paths begin and proceed in the specified direction", %{style: style} do
    bounds = %{x: 20.0, top: 90.0, width: 12.0, height: 9.0}

    for {path, distance, target} <- [
          {"circle(20px at 50% 50%)", "0%", {75.0, 60.0}},
          {"circle(20px at 50% 50%)", "25%", {60.0, 45.0}},
          {"inset(0)", "0%", {10.0, 100.0}},
          {"border-box", "50%", {110.0, 20.0}},
          {"polygon(0 0,100% 0,100% 100%,0 100%)", "25%", {100.0, 100.0}}
        ] do
      assert {:ok, used} = CssMotionValidator.prepare(style, "offset-path", path)
      assert {:ok, used} = CssMotionValidator.prepare(used, "offset-distance", distance)
      assert {:ok, used} = CssMotionValidator.prepare(used, "offset-anchor", "left top")
      assert {:ok, used} = CssMotionValidator.prepare(used, "offset-rotate", "0deg")
      assert {:ok, {_, _, _, _, x, y}} = CssMotionValidator.resolve(used, bounds)
      assert_in_delta x + bounds.x, elem(target, 0), 0.01
      assert_in_delta y + bounds.top, elem(target, 1), 0.01
    end
  end

  test "offset shorthand resets omitted values and composes with independent longhands", %{
    style: style
  } do
    assert {:ok, used} =
             CssMotionValidator.prepare(
               style,
               "offset",
               "auto path('M0 0H80') 50% reverse 20deg / left top"
             )

    assert used.css_offset_position == :auto
    assert used.css_offset_anchor == {{:percent, 0.0}, {:percent, 0.0}}
    assert CssMotionValidator.valid_style?(used)
    assert {:ok, used} = CssMotionValidator.prepare(used, "offset", "none")
    refute CssMotionValidator.active?(used)
    assert used.css_offset_rotate == {true, 0.0}
  end

  test "normal-flow starting positions and implicit circle centers retain independent coordinates",
       %{style: style} do
    bounds = %{x: 20.0, top: 90.0, width: 12.0, height: 9.0}
    assert {:ok, used} = CssMotionValidator.prepare(style, "offset-path", "circle(20px)")
    assert {:ok, used} = CssMotionValidator.prepare(used, "offset-position", "25% 25%")
    assert {:ok, used} = CssMotionValidator.prepare(used, "offset-anchor", "left top")
    assert {:ok, used} = CssMotionValidator.prepare(used, "offset-rotate", "0deg")
    assert {:ok, {_, _, _, _, x, y}} = CssMotionValidator.resolve(used, bounds)
    assert_in_delta x + bounds.x, 50.0, 0.001
    assert_in_delta y + bounds.top, 80.0, 0.001
    assert {:ok, _} = CssMotionValidator.prepare(style, "offset", "25% 25% / center")
    assert :error = CssMotionValidator.prepare(style, "offset-rotate", "auto 1deg 2deg")

    for invalid <- [
          %{css_offset_rotate: :unknown},
          %{css_offset_anchor: :unknown},
          %{css_offset_position: {:unknown, 0}},
          %{
            css_offset_path: %{
              type: :ray,
              angle: 0.0,
              size: "sides",
              contain: false,
              position: :bad
            }
          }
        ] do
      refute CssMotionValidator.valid_style?(invalid)
    end

    assert {:error, {:invalid_layout, diagnostic}} =
             CssMotionValidator.compute_style(%{
               css_offset_distance: {:css_position_math, "foo("}
             })

    assert diagnostic.source =~ "foo("

    broken = Map.put(used, :css_offset_distance, {:percent, 1.0e308})
    assert {:error, {:invalid_layout, _}} = CssMotionValidator.resolve(broken, bounds)

    assert {:ok, _} =
             NativeElixirPdfUtilities.HtmlToPdf.render(
               "<div style='width:100pt;height:60pt'><div style=\"width:12pt;height:9pt;offset-path:path('M0 0H80');offset-position:25% 25%;offset-anchor:center;offset-distance:50%;background:red\">Flow</div><p>Ordinary sibling</p></div>",
               system_font_discovery: false
             )
  end

  test "path and position boxes use containing edges rather than the moving element's edges", %{
    style: style
  } do
    reference =
      Map.merge(style._offset_reference, %{
        offset_box_style: %{
          border_widths: %{top: 2, right: 2, bottom: 2, left: 2},
          padding: %{top: 3, right: 3, bottom: 3, left: 3}
        },
        offset_position_box: :padding_box
      })

    style =
      Map.merge(style, %{
        _offset_reference: reference,
        border_widths: %{top: 10, right: 10, bottom: 10, left: 10}
      })

    bounds = %{x: 20.0, top: 90.0, width: 12.0, height: 9.0}

    for {path, position, {px, py}} <- [
          {"content-box", "normal", {15, 95}},
          {"path('M0 0')", "25% 25%", {36, 79}}
        ] do
      assert {:ok, used} = CssMotionValidator.prepare(style, "offset-path", path)
      assert {:ok, used} = CssMotionValidator.prepare(used, "offset-position", position)
      assert {:ok, used} = CssMotionValidator.prepare(used, "offset-anchor", "left top")
      assert {:ok, used} = CssMotionValidator.prepare(used, "offset-rotate", "0deg")
      assert {:ok, {_, _, _, _, x, y}} = CssMotionValidator.resolve(used, bounds)
      assert_in_delta x + bounds.x, px, 0.001
      assert_in_delta y + bounds.top, py, 0.001
    end

    assert {:ok, ray} = CssMotionValidator.prepare(style, "offset-path", "ray(45deg at 25% 75%)")
    broken = put_in(ray.css_offset_path.position, {{:css_position_math, "foo("}, {:percent, 0.5}})
    assert {:error, {:invalid_layout, _}} = CssMotionValidator.resolve(broken, bounds)
    refute CssMotionValidator.valid_style?(put_in(ray.css_offset_path.angle, :bad))
  end

  test "offset properties cascade independently and do not inherit" do
    assert {:ok, dom} =
             HtmlParser.parse_detailed(
               "<div style=\"offset-path:path('M0 0H80');offset-distance:50%;offset-rotate:reverse 20deg\"><p>Plain</p><p style='offset-distance:inherit'>Inherited distance</p></div>"
             )

    assert {:ok, %{children: [parent]}} =
             Style.compute_detailed(dom, system_font_discovery: false)

    [plain, inherited] = parent.children
    assert CssMotionValidator.active?(parent.style)
    refute CssMotionValidator.active?(plain.style)
    refute CssMotionValidator.active?(inherited.style)
    assert inherited.style.css_offset_distance == parent.style.css_offset_distance
  end
end
