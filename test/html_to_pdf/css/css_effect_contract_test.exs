defmodule NativeElixirPdfUtilities.HtmlToPdf.CssEffectContractTest do
  use ExUnit.Case, async: true

  alias NativeElixirPdfUtilities.HtmlToPdf.{
    Font,
    PdfWriter,
    CssEffectScene,
    DrawingGeometry,
    PathGeometry,
    ProjectionGeometry,
    Compositing
  }

  alias NativeElixirPdfUtilities.Validators.{
    CssClipValidator,
    CssCompositingValidator,
    CssEffectsValidator,
    CssMaskValidator,
    CssMotionValidator,
    CssProjectionValidator,
    CssTransformValidator,
    SvgPathValidator
  }

  alias NativeElixirPdfUtilities.TestSupport.PngFixture

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
       _root_line_height: 15.0
     }}
  end

  test "static geometry preparation shares declaring metrics and keeps actionable calculation failures",
       %{style: style} do
    assert {:ok, used} = CssProjectionValidator.prepare(style, "perspective", "2em")
    assert {:ok, used} = CssMotionValidator.prepare(used, "offset-distance", "3em")
    assert {:ok, used} = CssTransformValidator.individual(used, "translate", "1em 0")
    assert {:ok, computed} = CssTransformValidator.compute_style(used)
    assert computed.css_perspective == 24.0
    assert computed.css_offset_distance == 36.0
    assert computed.css_translate_metric_style.font_size == 12.0

    for invalid <- [
          %{css_perspective: {:css_length_math, {"foo(", false, :length}}},
          %{css_offset_distance: {:css_position_math, "foo("}}
        ] do
      assert {:error, {:invalid_layout, diagnostic}} =
               CssTransformValidator.compute_style(Map.merge(style, invalid))

      assert diagnostic.stage == :layout
      assert diagnostic.source =~ "foo("
      assert is_binary(diagnostic.message)
    end
  end

  test "styled-tree admission rejects malformed compositing facts through their owning validator" do
    for invalid <- [%{opacity: :bad}, %{mix_blend_mode: :Unknown}, %{background_blend_modes: []}] do
      tree = %{
        type: :document,
        children: [
          %{
            type: :element,
            tag: "div",
            children: [],
            style: Map.merge(%{display: :block}, invalid)
          }
        ]
      }

      assert {:error, {:invalid_layout, diagnostic}} =
               NativeElixirPdfUtilities.Validators.HtmlValidator.prepare_layout_input(tree, [])

      assert diagnostic.stage == :layout
      assert diagnostic.operation == :layout
      assert diagnostic.source == "div"
      assert diagnostic.message =~ "recompute styles"
    end
  end

  test "optional scene dispatch still rejects each malformed prepared feature" do
    for invalid <- [
          %{css_filter: [{"invert", 2.0}]},
          %{css_clip_path: :bad},
          %{css_mask: :bad},
          %{css_transform: :bad},
          %{_css_3d_context: :bad},
          %{css_offset_rotate: :bad}
        ] do
      tree = %{
        type: :document,
        children: [
          %{
            type: :element,
            tag: "div",
            children: [],
            style: Map.merge(%{display: :block}, invalid)
          }
        ]
      }

      assert {:error, :invalid_layout} =
               NativeElixirPdfUtilities.Validators.HtmlValidator.prepare_layout_input(tree, [])
    end
  end

  test "homogeneous operations preserve independently specified three-dimensional points" do
    half = :math.pi() / 2

    for {name, args, expected} <- [
          {"matrix", [1, 0, 0, 1, 3, 4], {4, 6, 3}},
          {"translate", [3], {4, 2, 3}},
          {"translate", [3, 4], {4, 6, 3}},
          {"translatex", [3], {4, 2, 3}},
          {"translatey", [4], {1, 6, 3}},
          {"translatez", [5], {1, 2, 8}},
          {"scale", [2], {2, 4, 3}},
          {"scale", [2, 3], {2, 6, 3}},
          {"scalex", [2], {2, 2, 3}},
          {"scaley", [3], {1, 6, 3}},
          {"scalez", [4], {1, 2, 12}},
          {"rotate", [half], {-2, 1, 3}},
          {"rotatez", [half], {-2, 1, 3}},
          {"rotatex", [half], {1, -3, 2}},
          {"rotatey", [half], {3, 2, -1}},
          {"rotate3d", [0, 0, 0, half], {1, 2, 3}},
          {"skew", [:math.atan(1)], {3, 2, 3}},
          {"skew", [:math.atan(1), :math.atan(1)], {3, 3, 3}},
          {"skewx", [:math.atan(1)], {3, 2, 3}},
          {"skewy", [:math.atan(1)], {1, 3, 3}}
        ] do
      point = ProjectionGeometry.point(ProjectionGeometry.operation(name, args), 1, 2, 3)
      for index <- 0..2, do: assert_in_delta(elem(point, index), elem(expected, index), 0.000001)
    end
  end

  test "3D and individual CSS grammar reaches measured matrices with a nonzero origin", %{
    style: style
  } do
    bounds = %{x: 10.0, top: 90.0, width: 80.0, height: 60.0}

    for source <- [
          "matrix3d(1,0,0,0,0,1,0,0,0,0,1,0,10,20,30,1)",
          "translate3d(10px,20%,3px)",
          "translateZ(10px)",
          "scaleZ(2)",
          "rotate3d(1,2,3,30deg)",
          "rotateX(20deg) skewX(5deg) translateY(2px)"
        ] do
      assert {:ok, used} = CssTransformValidator.apply(style, "transform", source)
      assert {:ok, used} = CssTransformValidator.apply(used, "transform-origin", "25% 75% 5px")
      assert CssTransformValidator.valid_style?(used)
      assert {:ok, _} = CssTransformValidator.resolve(used, bounds)
    end

    for {property, value} <- [
          {"translate", "1px 2px 3px"},
          {"scale", "2 3 4"},
          {"rotate", "x 20deg"},
          {"rotate", "1 0 0 20deg"},
          {"translate", "none"}
        ] do
      assert {:ok, used} = CssTransformValidator.individual(style, property, value)
      assert CssTransformValidator.valid_style?(used)
      assert {:ok, _} = CssTransformValidator.used_operations(used, 80, 60)
    end

    for transform <- ["rotateY(180deg)", "scale3d(0,1,1)", "translateZ(10px)"] do
      assert {:ok, used} =
               CssTransformValidator.apply(
                 Map.put(style, :css_backface_visibility, :hidden),
                 "transform",
                 transform
               )

      assert {:ok, _} = CssProjectionValidator.resolve(used, bounds)
    end

    assert {:ok, used} = CssTransformValidator.individual(style, "transform-box", "fill-box")
    assert {:ok, used} = CssTransformValidator.apply(used, "transform", "translateX(20%)")
    assert {:ok, _} = CssTransformValidator.resolve(used, bounds)
  end

  test "projection descriptors reject malformed and excessive geometry with actionable diagnostics",
       %{style: style} do
    bounds = %{x: 0, top: 100, width: 100, height: 100}

    descriptor = %{
      matrix: ProjectionGeometry.identity(),
      perspective: ProjectionGeometry.identity(),
      preserve: false,
      backface: :visible,
      bounds: bounds
    }

    for invalid <- [
          %{},
          %{descriptor | bounds: %{bounds | width: -1}},
          %{descriptor | matrix: {1, 2}},
          %{descriptor | backface: :unknown}
        ] do
      assert {:error, {:invalid_layout, diagnostic}} =
               CssProjectionValidator.validate_descriptor(invalid)

      assert diagnostic.stage == :layout and diagnostic.operation == :resolve_css_projection
      assert is_binary(diagnostic.message)
    end

    for {property, value} <- [{"transform-style", "flat"}, {"backface-visibility", "visible"}] do
      assert {:ok, _} = CssProjectionValidator.prepare(style, property, value)
      assert :error = CssProjectionValidator.prepare(style, property, "unknown")
    end

    for invalid <- [
          %{css_transform: :broken},
          %{css_transform: [:broken]},
          %{css_perspective_origin: :unknown}
        ] do
      if Map.has_key?(invalid, :css_transform),
        do: refute(CssTransformValidator.valid_style?(invalid)),
        else: refute(CssProjectionValidator.valid_style?(invalid))

      assert is_boolean(CssProjectionValidator.needs_group?(invalid))
    end

    large = ProjectionGeometry.operation("scale3d", [1.0e8, 1.0e8, 1.0e8])

    group = %{
      transform: {1.0e8, 0, 0, 1.0e8, 0, 0},
      width: 100,
      height: 100,
      css_projection: descriptor
    }

    assert {:error, {:invalid_layout, _}} =
             CssProjectionValidator.compose_group(large, group, 100)

    assert {:ok, used} =
             CssTransformValidator.apply(
               style,
               "transform",
               "scale3d(1e8,1e8,1e8) scale3d(1e8,1e8,1e8)"
             )

    assert {:error, {:invalid_layout, _}} = CssProjectionValidator.resolve(used, bounds)
  end

  test "clip metadata failures keep canonical path and coordinate diagnostics" do
    for source <- ["path(evenodd,'M0 0H10V10H0Z')", "path(nonzero,'M0 0H10V10H0Z')"] do
      assert {:ok, style} = CssClipValidator.prepare(%{}, source)
      assert {:ok, _} = CssClipValidator.resolve(style, %{x: 0, top: 10, width: 10, height: 10})
    end

    for source <- ["path()", "path(foo,'M0 0')", "path('M0 0',foo)", "path('M0 0') unknown-box"] do
      assert :error = CssClipValidator.prepare(%{}, source)
    end

    for style <- [
          %{css_clip_path: :unknown},
          %{
            css_clip_path: %{
              type: :path,
              operations: [:bad],
              rule: :nonzero,
              reference: :border_box
            }
          }
        ] do
      refute CssClipValidator.valid_style?(style)

      assert {:error, {:invalid_layout, diagnostic}} =
               CssClipValidator.resolve(style, %{x: 0, top: 10, width: 10, height: 10})

      assert diagnostic.source == "clip-path"
    end

    for clip <- [
          %{path_commands: [{1, [0, 0]}], fill_rule: :nonzero},
          %{path_commands: [:invalid], fill_rule: :evenodd},
          %{path_commands: [], fill_rule: :unknown}
        ] do
      assert {:error, {:invalid_layout, _}} = CssClipValidator.validate_clip(clip)
    end

    style = %{
      border_widths: %{top: 1, right: 2, bottom: 3, left: 4},
      margin: %{top: 1, right: :auto, bottom: 3, left: 4}
    }

    bounds = %{x: 10, top: 90, width: 80, height: 60}

    assert CssClipValidator.reference_box(bounds, style, :padding_box) == %{
             x: 14,
             top: 89,
             width: 74,
             height: 56
           }

    assert CssClipValidator.reference_box(bounds, style, :margin_box) == %{
             x: 6,
             top: 91,
             width: 84,
             height: 64
           }

    assert {:ok, empty} = CssClipValidator.prepare(%{}, "inset(100%)")
    assert {:ok, _} = CssClipValidator.resolve(empty, bounds)
  end

  test "filter paint rejects unknown instructions and unsupported source combinations" do
    assert :error = CssEffectsValidator.prepare(%{}, "filter", "")

    for style <- [%{css_filter: :bad}, %{css_filter: [:bad]}, %{css_filter: [{"unknown", 2}]}] do
      refute CssEffectsValidator.valid_style?(style)
    end

    for effect <- [
          %{},
          %{
            filters: [],
            backdrop: [],
            bounds: %{x: 0, top: 10, width: -1, height: 10},
            definitions: "",
            color: {0, 0, 0}
          }
        ] do
      assert {:error, {:unsupported_html, diagnostic}} =
               CssEffectsValidator.validate_group(effect)

      assert diagnostic.stage == :layout and is_binary(diagnostic.message)
    end

    rectangle = %{
      type: :rect,
      x: 0.0,
      y: 0.0,
      width: 9.0,
      height: 9.0,
      fill_color: {1.0, 0.0, 0.0},
      stroke_color: nil,
      stroke_width: 0.0,
      border_radius: 0.0
    }

    assert {:error, {:unsupported_html, _}} =
             CssEffectsValidator.validate_artwork(
               [Map.put(rectangle, :border_styles, %{top: :dashed})],
               []
             )

    region = Map.take(rectangle, [:x, :y, :width, :height])
    effect = %{definitions: "", color: {0, 0, 0}}

    assert {:error, {:unsupported_html, _}} =
             CssEffectScene.render([%{type: :unknown}], region, [], effect, {30, 30})

    assert {:error, {:unsupported_html, _}} =
             CssEffectScene.render([rectangle], region, [{"url", "absent"}], effect, {30, 30})

    for invalid <- [nil, [%{}], [%{size: {30, 30}, boxes: [%{}]}]] do
      assert {:error, _} = PdfWriter.render(invalid)
    end

    assert {:error, _} = PdfWriter.render([%{size: {30, 30}, boxes: [rectangle]}], :invalid)
  end

  test "raster source preserves sixteen-bit gray alpha, orientation and glyph stroke", %{
    style: style
  } do
    alias NativeElixirPdfUtilities.HtmlToPdf.PngDecoder
    budget = NativeElixirPdfUtilities.Validators.HtmlValidator.new_image_budget()

    for {type, depth, pixel} <- [
          {0, 8, [128]},
          {4, 16, [32768, 32768]},
          {2, 16, [65535, 0, 0]},
          {6, 8, [255, 0, 0, 128]}
        ] do
      png = PngFixture.build(2, 2, type, depth, 0, fn _, _ -> pixel end)
      assert {:ok, image} = PngDecoder.decode(png, budget, true)

      box = %{
        type: :image,
        x: 0.0,
        y: 0.0,
        width: 9.0,
        height: 9.0,
        image: Map.put(image, :image_orientation, 2)
      }

      assert {:ok, %{image: painted}} =
               CssEffectScene.render(
                 [box],
                 %{x: 0.0, y: 0.0, width: 9.0, height: 9.0},
                 [],
                 %{definitions: "", color: {0, 0, 0}},
                 {30, 30}
               )

      assert painted.width_px == 24
      assert byte_size(painted.data) == painted.width_px * painted.height_px * 3
    end

    text =
      Map.merge(style, %{
        type: :text,
        x: 1.0,
        y: 10.0,
        width: 18.0,
        text: "A",
        font: "DejaVu Sans",
        color: {1.0, 0.0, 0.0},
        text_stroke_width: 0.5,
        text_stroke_color: {0.0, 0.0, 1.0}
      })

    NativeElixirPdfUtilities.Validators.FontValidator.with_budget(fn ->
      assert {:ok, _} =
               CssEffectScene.render(
                 [text],
                 %{x: 0.0, y: 0.0, width: 30.0, height: 30.0},
                 [],
                 %{definitions: "", color: {0, 0, 0}},
                 {30, 30}
               )

      builtin = %{text | font_face: %{type: :built_in, pdf_name: "Helvetica"}, font: "Helvetica"}

      assert {:error, {:unsupported_html, _}} =
               CssEffectScene.render(
                 [builtin],
                 %{x: 0.0, y: 0.0, width: 30.0, height: 30.0},
                 [],
                 %{definitions: "", color: {0, 0, 0}},
                 {30, 30}
               )

      assert {:error, _} = DrawingGeometry.placed_ink_rectangles([builtin])
    end)
  end

  test "filtered marker, wave and nested rounded clips keep nonempty paint" do
    rectangle = %{
      type: :rect,
      x: 3.0,
      y: 3.0,
      width: 12.0,
      height: 9.0,
      fill_color: {1.0, 0.0, 0.0},
      stroke_color: {0.0, 0.0, 1.0},
      stroke_width: 0.75,
      border_radius: 0.0
    }

    clip = %{
      x: 0.0,
      y: 0.0,
      width: 24.0,
      height: 24.0,
      border_radii:
        Map.new([:top_left, :top_right, :bottom_right, :bottom_left], &{&1, {2.0, 3.0}}),
      inner_clip: %{x: 2.0, y: 2.0, width: 18.0, height: 18.0}
    }

    scope = %{id: [0], opacity: 0.7, mix_blend_mode: :Multiply}

    for shape <- [
          {:triangle, :up},
          {:triangle, :down},
          {:triangle, :left},
          {:triangle, :right},
          {:chevron, :down},
          {:diagonal, :up},
          :wave,
          :rectangle
        ] do
      box =
        rectangle
        |> Map.put(:shape, shape)
        |> Map.put(:clip, clip)
        |> Map.put(:css_compositing_scopes, [scope])

      box = if shape == :wave, do: Map.put(box, :fill_color, nil), else: box
      box = if shape == :rectangle, do: Map.put(box, :border_radii, clip.border_radii), else: box

      assert {:ok, %{image: image}} =
               CssEffectScene.render(
                 [box],
                 %{x: 0.0, y: 0.0, width: 24.0, height: 24.0},
                 [{"drop-shadow", %{x: 1.0, y: 2.0, blur: 0.5, color: :current_color}}],
                 %{definitions: "", color: {0, 1, 0}, compositing_prefix: []},
                 {30, 30}
               )

      assert image.data != :binary.copy(<<0>>, byte_size(image.data))
    end
  end

  test "mask image permissions, hidden resources and backdrop composition use the source loader" do
    alias NativeElixirPdfUtilities.HtmlToPdf
    image = PngFixture.solid_rgb(2, 2)

    html =
      "<div style='width:24pt;height:24pt;background:red;mask-image:url(mask.png);backdrop-filter:invert(1)'>Mask</div>"

    assert {:ok, pdf} =
             HtmlToPdf.render(html,
               assets: %{"mask.png" => {:bytes, image}},
               page_size: {60, 60},
               margin: 0,
               system_font_discovery: false
             )

    assert pdf =~ "/Subtype /Image"
    assert {:error, _} = HtmlToPdf.render(html, system_font_discovery: false)

    assert {:ok, _} =
             HtmlToPdf.render(
               "<div style='display:none;mask-image:url(missing.png);mask-border-source:url(missing-border.png)'>Hidden</div>",
               system_font_discovery: false
             )

    assert {:ok, style} = CssMaskValidator.prepare(%{}, "mask-image", "url(mask.png)")
    assert {:error, :denied} = CssMaskValidator.load(style, [], fn _ -> {:error, :denied} end)
  end

  test "mask longhands exercise independent image, border and cascade ownership" do
    examples = %{
      "mask" => "linear-gradient(white,black)",
      "mask-image" => "none",
      "mask-mode" => "alpha",
      "mask-repeat" => "no-repeat",
      "mask-position" => "left top",
      "mask-size" => "50% 80%",
      "mask-origin" => "padding-box",
      "mask-clip" => "content-box",
      "mask-composite" => "intersect",
      "mask-border" => "linear-gradient(white,transparent) 20% / 3pt",
      "mask-border-source" => "none",
      "mask-border-slice" => "20% fill",
      "mask-border-width" => "3pt",
      "mask-border-outset" => "1",
      "mask-border-repeat" => "round",
      "mask-border-mode" => "luminance"
    }

    assert Enum.sort(CssMaskValidator.properties()) == Enum.sort(Map.keys(examples))
    assert Map.has_key?(CssMaskValidator.cascade_fields(), "mask-border-repeat")

    for {property, source} <- examples do
      assert {:ok, style} = CssMaskValidator.prepare(%{}, property, source)
      assert CssMaskValidator.valid_style?(style)
    end

    for {property, source} <- [
          {"mask-image", "unknown(foo)"},
          {"mask-image", ""},
          {"mask-border-mode", "unknown"},
          {"mask", "linear-gradient(white,black) alpha luminance"}
        ] do
      assert :error = CssMaskValidator.prepare(%{}, property, source)
    end

    refute CssMaskValidator.valid_style?(%{css_mask: :unknown})
  end

  test "the static effects sheet retains all source captions without a browser" do
    alias NativeElixirPdfUtilities.{HtmlToPdf, Text}

    path =
      NativeElixirPdfUtilities.TestSupport.Paths.fixture(
        "html_to_pdf/browser_parity/css_compositing.html"
      )

    assert {:ok, pdf} =
             HtmlToPdf.render(File.read!(path), system_font_discovery: false, forms: :static)

    assert {:ok, context} = NativeElixirPdfUtilities.Pdf.Reader.read_validated(pdf)
    assert length(context.document.pages) == 1
    assert {:ok, text} = Text.extract(pdf)

    for caption <- [
          "Group overlap",
          "Nested opacity",
          "Perspective function",
          "Intersecting planes",
          "Circle offset path",
          "Referenced filter"
        ] do
      assert text =~ caption
    end
  end

  test "native drawing groups combine shared compositing ancestry and reject conflicting parents" do
    alias NativeElixirPdfUtilities.Validators.WriterValidator

    rectangle = %{
      type: :rect,
      x: 1.0,
      y: 1.0,
      width: 9.0,
      height: 9.0,
      fill_color: {1.0, 0.0, 0.0},
      stroke_color: nil,
      stroke_width: 0.0,
      border_radius: 0.0
    }

    parent = %{id: [0], opacity: 0.5, mix_blend_mode: :Normal}
    child = %{id: [0, 1], opacity: 1.0, mix_blend_mode: :Multiply}

    group = %{
      type: :drawing_group,
      x: 0.0,
      y: 0.0,
      width: 30.0,
      height: 30.0,
      transform: {1, 0, 0, 1, 0, 0},
      css_compositing_scopes: [parent],
      boxes: [Map.put(rectangle, :css_compositing_scopes, [child])]
    }

    assert {:ok, context} = WriterValidator.prepare([%{size: {30, 30}, boxes: [group]}], [])

    assert Enum.any?(
             hd(context.pages).boxes,
             &(Map.get(&1, :css_compositing_scopes) == [parent, child])
           )

    assert {:ok, _} = PdfWriter.render([%{size: {30, 30}, boxes: [group]}])

    duplicate =
      put_in(group.boxes, [Map.put(rectangle, :css_compositing_scopes, [child, parent])])

    assert {:error, {:invalid_layout, _}} =
             WriterValidator.prepare([%{size: {30, 30}, boxes: [duplicate]}], [])

    conflicting = Map.put(rectangle, :css_compositing_scopes, [child])

    assert {:error, {:invalid_layout, _}} =
             WriterValidator.prepare([%{size: {30, 30}, boxes: [group, conflicting]}], [])

    owned =
      Map.put(rectangle, :initial_select_opacity, %{owner: make_ref(), alpha: 0.7})
      |> Map.put(:css_compositing_scopes, [parent])

    assert {:ok, _} = PdfWriter.render([%{size: {30, 30}, boxes: [owned]}])

    for rule <- [:nonzero, :evenodd] do
      clip = %{
        x: 0,
        y: 0,
        width: 20,
        height: 20,
        path_commands: [{0, [0, 0]}, {1, [20, 0]}, {1, [0, 20]}, {4, []}],
        fill_rule: rule
      }

      source = group |> Map.delete(:css_compositing_scopes) |> Map.put(:clip, clip)

      assert {:ok, pdf} =
               PdfWriter.render([%{size: {30, 30}, boxes: [source]}], compress_streams: false)

      assert pdf =~ if(rule == :evenodd, do: "W* n", else: "W n")
    end
  end

  test "extreme admitted projection and arc geometry fails with useful bounded diagnostics", %{
    style: style
  } do
    alias NativeElixirPdfUtilities.HtmlToPdf.ProjectionScene
    bounds = %{x: 0.0, top: 30.0, width: 30.0, height: 30.0}
    tiny = ProjectionGeometry.operation("scale3d", [1.0e-310, 1, 1])

    root = %{
      type: :drawing_group,
      role: :css_transform_plane,
      x: 0.0,
      y: 0.0,
      width: 30.0,
      height: 30.0,
      transform: {1, 0, 0, 1, 0, 0},
      css_projection: %{
        matrix: tiny,
        perspective: ProjectionGeometry.identity(),
        preserve: false,
        backface: :visible,
        bounds: bounds
      },
      boxes: [
        %{
          type: :rect,
          x: 1.0,
          y: 1.0,
          width: 9.0,
          height: 9.0,
          fill_color: {1.0, 0.0, 0.0},
          stroke_color: nil,
          stroke_width: 0.0,
          border_radius: 0.0
        }
      ]
    }

    assert {:error, {:invalid_layout, diagnostic}} = ProjectionScene.prepare(root, {30, 30}, [])
    assert diagnostic.source == "CSS 3D transform"
    assert {:ok, used} = CssTransformValidator.apply(style, "transform", "scale3d(1e8,1e8,1e8)")

    assert {:error, {:invalid_layout, _}} =
             CssProjectionValidator.resolve(used, %{bounds | x: 1.0e308})

    assert {:error, {:invalid_layout, clip_diagnostic}} =
             NativeElixirPdfUtilities.HtmlToPdf.render(
               "<div style=\"width:20pt;height:20pt;clip-path:path('M0 0A1e-200 1e-200 0 0 1 10 10')\">Extreme clip</div>",
               system_font_discovery: false
             )

    assert clip_diagnostic.source == "clip-path"

    huge = ProjectionGeometry.operation("scale3d", [1.0e308, 1, 1])

    assert {:error, {:invalid_layout, _}} =
             CssProjectionValidator.compose_group(
               huge,
               %{root | transform: {1.0e308, 0, 0, 1, 0, 0}},
               30
             )

    assert {:ok, clip} = CssClipValidator.prepare(style, "path('M0 0A1e-200 1e-200 0 0 1 10 10')")
    assert {:error, {:invalid_layout, _}} = CssClipValidator.resolve(clip, bounds)
    assert {:ok, source} = SvgPathValidator.prepare("M0 0A1e-80 1e-80 0 0 1 1e-150 0")
    assert {3, values} = List.last(PathGeometry.commands(source))
    assert Enum.take(values, -2) == [1.0e-150, 0]
  end

  test "nested filtered JPEG artwork keeps links, logical glyphs and source markers" do
    alias NativeElixirPdfUtilities.{HtmlToPdf, Text}

    jpeg =
      NativeElixirPdfUtilities.TestSupport.Paths.fixture("images/jpeg/restart_rgb.jpg")
      |> File.read!()

    html =
      "<div style='filter:drop-shadow(1px 2px 1px green);width:90pt'><div style='transform:translate(2pt,1pt)'><a href='https://example.com/jpeg'><img src='source.jpg' style='width:18pt;height:18pt'></a><span>office العربية</span></div></div>"

    assert {:ok, pdf} =
             HtmlToPdf.render(html,
               assets: %{"source.jpg" => {:bytes, jpeg}},
               system_font_discovery: false
             )

    assert pdf =~ "/Subtype /Link"
    assert {:ok, text} = Text.extract(pdf)
    assert text =~ "office"

    rectangle = %{
      type: :rect,
      x: 0.0,
      y: 0.0,
      width: 9.0,
      height: 9.0,
      fill_color: {1.0, 0.0, 0.0},
      stroke_color: nil,
      stroke_width: 0.0,
      border_radius: 0.0
    }

    effect = %{definitions: "", color: {0, 0, 0}}

    assert {:ok, _} =
             CssEffectScene.render(
               [
                 %{type: :actual_text_start, text: "Source"},
                 rectangle,
                 %{type: :actual_text_end},
                 %{type: :flow_marker, x: 0, y: 0, width: 0, height: 0}
               ],
               Map.take(rectangle, [:x, :y, :width, :height]),
               [],
               effect,
               {30, 30}
             )

    invalid =
      Map.merge(effect, %{
        filters: [],
        backdrop: [],
        bounds: %{x: 0, top: 9, width: 9, height: 9},
        style: %{css_mask: :unknown}
      })

    assert {:error, {:unsupported_html, _}} = CssEffectsValidator.validate_group(invalid)

    assert NativeElixirPdfUtilities.HtmlToPdf.CssBackground.layered?(%{
             background_blend_modes: [:Multiply]
           })

    assert {:ok, mask} = CssMaskValidator.prepare(%{}, "mask", "url(source.png) no-repeat")
    assert {:error, :denied} = CssMaskValidator.load(mask, [], fn _ -> {:error, :denied} end)
  end

  test "effect preprocessing retains structural markers and nested local drawing planes", %{
    style: style
  } do
    rectangle = %{
      type: :rect,
      x: 1,
      y: 1,
      width: 9,
      height: 9,
      fill_color: {1, 0, 0},
      stroke_color: nil,
      stroke_width: 0,
      border_radius: 0
    }

    effect = %{
      filters: [{"invert", 1.0}],
      backdrop: [],
      definitions: "",
      color: {0, 0, 0},
      bounds: %{x: 0, top: 12, width: 12, height: 12},
      style: %{}
    }

    marker = %{type: :flow_marker, x: 0, y: 0, width: 0, height: 0}

    group = %{
      type: :drawing_group,
      x: 0,
      y: 0,
      width: 12,
      height: 12,
      transform: {1, 0, 0, 1, 0, 0},
      css_effect: effect,
      boxes: [rectangle, marker]
    }

    assert {:ok, [%{boxes: [%{boxes: output}]}]} =
             CssEffectScene.prepare([%{size: {30, 30}, boxes: [group]}], [])

    assert marker in output
    assert Enum.any?(output, &(Map.get(&1, :type) == :image))

    assert {:ok, used} =
             CssTransformValidator.apply(
               Map.put(style, :css_transform_box, :fill_box),
               "transform",
               "rotateY(20deg)"
             )

    assert {:ok, _} =
             CssProjectionValidator.resolve(used, %{x: 0, top: 30, width: 30, height: 30})

    assert {:ok, clip} = CssClipValidator.prepare(style, "path('M0 0H40V40Z')")

    assert {:error, {:invalid_layout, diagnostic}} =
             CssClipValidator.resolve(clip, %{
               x: NativeElixirPdfUtilities.Limits.get(:max_numeric_magnitude),
               top: 30,
               width: 30,
               height: 30
             })

    assert diagnostic.source == "clip-path"

    assert {:error, {:invalid_layout, _}} =
             CssClipValidator.validate_clip(%{
               path_commands: [],
               fill_rule: :nonzero,
               css_source_path: :bad
             })

    invalid_clip = %{
      x: 0,
      y: 0,
      width: 12,
      height: 12,
      path_commands: [{1, [0, 0]}],
      fill_rule: :nonzero
    }

    assert {:error, {:invalid_pdf_input, diagnostic}} =
             PdfWriter.render([
               %{size: {30, 30}, boxes: [Map.put(rectangle, :clip, invalid_clip)]}
             ])

    assert diagnostic.operation == :write_pdf

    assert CssTransformValidator.valid_style?(%{css_transform_box: :content_box})
    assert CssTransformValidator.valid_style?(%{transform_origin: {0, 0}})
    refute CssTransformValidator.valid_style?(%{transform_origin: {:bad, 0}})

    for origin <- [{:bad, 0, 0}, {0, :bad, 0}, {0, 0, :bad}] do
      refute CssTransformValidator.valid_style?(%{transform_origin: origin})
    end

    for malformed <- [Map.put(rectangle, :snap_to_css_pixel_grid, :bad), %{type: :unknown}] do
      malformed_group = %{group | boxes: [malformed]} |> Map.delete(:css_effect)

      assert {:error, {:invalid_pdf_input, group_diagnostic}} =
               PdfWriter.render([%{size: {30, 30}, boxes: [malformed_group]}])

      assert group_diagnostic.message =~ "children"
    end

    assert {:ok, mask} = CssMaskValidator.prepare(%{}, "mask-image", "none")
    mask = put_in(mask.css_mask.background, %{background_image_source: "legacy.png"})

    assert {:error, :legacy_denied} =
             CssMaskValidator.load(mask, [], fn "legacy.png" -> {:error, :legacy_denied} end)
  end

  test "path sampling handles empty contours and smooth commands without preceding controls" do
    assert PathGeometry.sample([], 0, false) == %{point: {0.0, 0.0}, angle: 0.0, length: 0.0}
    assert {:ok, source} = SvgPathValidator.prepare("M0 0S10 10 20 0T40 0A0 0 0 0 1 40 0")
    commands = PathGeometry.commands(source)
    assert {0, [0.0, 0.0]} == hd(commands)
    assert elem(PathGeometry.sample(commands, 5, false).point, 0) > 0
  end

  test "blend extrema and nonseparable clipping remain bounded" do
    for mode <- [:ColorDodge, :ColorBurn, :Hue, :Saturation, :Color, :Luminosity],
        backdrop <- [{0.0, 0.0, 0.0, 1.0}, {1.0, 1.0, 1.0, 1.0}, {0.0, 1.0, 0.0, 1.0}],
        source <- [{0.0, 0.0, 0.0, 1.0}, {1.0, 1.0, 1.0, 1.0}, {1.0, 0.0, 1.0, 1.0}] do
      result = Compositing.over(source, backdrop, mode)
      assert Enum.all?(Tuple.to_list(result), &(&1 >= -0.000001 and &1 <= 1.000001))
    end

    assert :error = CssCompositingValidator.apply(%{}, "background-blend-mode", "")

    assert :ok =
             CssCompositingValidator.validate_scopes([
               %{id: {:background, make_ref(), 0}, opacity: 1.0, mix_blend_mode: :Normal}
             ])
  end
end
