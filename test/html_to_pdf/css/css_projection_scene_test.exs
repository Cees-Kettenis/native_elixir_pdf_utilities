defmodule NativeElixirPdfUtilities.HtmlToPdf.CssProjectionSceneTest do
  use ExUnit.Case, async: true
  alias NativeElixirPdfUtilities.{HtmlToPdf, Text}
  alias NativeElixirPdfUtilities.HtmlToPdf.{ProjectionScene, Compositing}

  test "projective artwork retains searchable text, links and internal destinations" do
    html = """
    <style>html,body{margin:0;font:10pt/15pt DejaVu Sans}.scene{margin:20pt;perspective:180pt;perspective-origin:50% 50%;width:100pt;height:70pt}.plane{width:80pt;height:45pt;background:red;transform:rotateY(25deg);transform-style:preserve-3d}</style>
    <div class='scene'><div class='plane' id='target'><a href='https://example.com/plane'>Projected label</a></div></div><a href='#target'>Target</a>
    """

    assert {:ok, pdf} =
             HtmlToPdf.render(html,
               margin: 0,
               page_size: {180, 120},
               system_font_discovery: false
             )

    assert pdf =~ "/Subtype /Image"
    assert pdf =~ "/QuadPoints ["
    assert {:ok, text} = Text.extract(pdf)
    assert String.replace(text, ~r/\s/u, "") =~ "Projectedlabel"
    assert text =~ "Target"
  end

  test "projected bidirectional text keeps its logical extraction order" do
    html =
      "<div style='perspective:180pt;width:120pt;height:60pt'><p dir='rtl' style='transform:rotateY(20deg)'>אב abc גד</p></div>"

    assert {:ok, pdf} =
             HtmlToPdf.render(html, system_font_discovery: false, compress_streams: false)

    assert {:ok, text} = Text.extract(pdf, layout: false)
    assert String.replace(text, ~r/\s+/u, " ") =~ "אב abc גד"
    assert pdf =~ "/ActualText"

    assert {:ok, hidden} =
             HtmlToPdf.render(
               "<div style='perspective:100pt;transform-style:preserve-3d;width:120pt;height:60pt'><p dir='rtl' style='transform:translateZ(300pt)'>אב abc גד</p></div>",
               system_font_discovery: false
             )

    assert {:error, {:no_extractable_text, _}} = Text.extract(hidden)
  end

  test "projective backfaces and singular planes omit artwork without crashing" do
    for transform <- ["rotateY(180deg)", "scale3d(0,1,1)"] do
      assert {:ok, pdf} =
               HtmlToPdf.render(
                 "<div style='perspective:100px;width:60pt;height:30pt'><div style='transform:#{transform};backface-visibility:hidden;width:40pt;height:20pt;background:red'>Hidden</div></div>",
                 margin: 0,
                 page_size: {120, 90},
                 system_font_discovery: false
               )

      assert {:error, {:no_extractable_text, _}} = Text.extract(pdf)
    end
  end

  test "preserved planes intersect by pixel depth while flat siblings paint in source order" do
    alias NativeElixirPdfUtilities.HtmlToPdf.ProjectionGeometry, as: G
    bounds = %{x: 0.0, top: 100.0, width: 100.0, height: 100.0}

    children =
      for {angle, color} <- [{0.5, {1.0, 0.0, 0.0}}, {-0.5, {0.0, 0.0, 1.0}}] do
        %{
          type: :drawing_group,
          role: :css_transform_plane,
          x: 0.0,
          y: 0.0,
          width: 100.0,
          height: 100.0,
          transform: {1, 0, 0, 1, 0, 0},
          css_projection: %{
            matrix: G.around(G.operation("rotatey", [angle]), 50, 50, 0),
            perspective: G.identity(),
            preserve: false,
            backface: :visible,
            bounds: bounds
          },
          boxes: [
            %{
              type: :rect,
              x: 20.0,
              y: 30.0,
              width: 60.0,
              height: 40.0,
              fill_color: color,
              stroke_color: nil,
              stroke_width: 0.0,
              border_radius: 0.0
            }
          ]
        }
      end

    for preserve <- [true, false] do
      root = %{
        type: :drawing_group,
        role: :css_transform_plane,
        x: 0.0,
        y: 0.0,
        width: 100.0,
        height: 100.0,
        transform: {1, 0, 0, 1, 0, 0},
        boxes: children,
        css_projection: %{
          matrix: G.identity(),
          perspective: G.around(G.operation("perspective", [180]), 50, 50, 0),
          preserve: preserve,
          backface: :visible,
          bounds: bounds
        }
      }

      assert {:ok, %{boxes: [image | _]}} = ProjectionScene.prepare(root, {100, 100}, [])
      {lr, _, lb, _} = ProjectionScene.sample(image, 35, 50)
      {rr, _, rb, _} = ProjectionScene.sample(image, 65, 50)

      if preserve do
        assert lr > 0.95 and lb < 0.01
      else
        assert lb > 0.95 and lr < 0.01
      end

      assert rb > 0.95 and rr < 0.01
    end
  end

  test "projected links intersect their ancestor source clip" do
    html =
      "<style>html,body{margin:0;font:10pt/15pt DejaVu Sans}</style><div style='margin:20pt;width:100pt;height:70pt;perspective:180pt'><div style='width:80pt;height:30pt;transform:rotateY(30deg);transform-origin:0 0;clip-path:inset(0 50% 0 0)'><a style='display:block;width:80pt;height:30pt' href='https://example.com/projected-clip'>Projected clipped link</a></div></div>"

    assert {:ok, pdf} =
             HtmlToPdf.render(html,
               margin: 0,
               page_size: {180, 140},
               system_font_discovery: false
             )

    assert {:ok, context} = NativeElixirPdfUtilities.Pdf.Reader.read_validated(pdf)

    links =
      context.document.objects
      |> Map.values()
      |> Enum.map(& &1.value)
      |> Enum.filter(&(is_map(&1) and Map.get(&1, "Subtype") == {:name, "Link"}))

    assert links != []
    assert Enum.all?(links, fn %{"Rect" => [left, _, right, _]} -> left >= 15 and right <= 60 end)
  end

  test "filtered and projected source links keep destinations on later physical pages" do
    for effect <- ["filter:invert(1)", "transform:perspective(180pt) rotateY(20deg)"] do
      html =
        "<style>html,body{margin:0;font:10pt/15pt DejaVu Sans}</style><div style='margin:15pt;width:80pt;height:30pt;#{effect}'><a href='#later'>Later target</a></div><p id='later' style='break-before:page'>Second-page target</p>"

      assert {:ok, pdf} =
               HtmlToPdf.render(html,
                 margin: 0,
                 page_size: {160, 110},
                 system_font_discovery: false
               )

      assert {:ok, context} = NativeElixirPdfUtilities.Pdf.Reader.read_validated(pdf)
      assert length(context.document.pages) == 2
      target_ref = Enum.at(context.document.pages, 1).ref

      links =
        context.document.objects
        |> Map.values()
        |> Enum.map(& &1.value)
        |> Enum.filter(&(is_map(&1) and Map.get(&1, "Subtype") == {:name, "Link"}))

      assert Enum.any?(links, fn link ->
               match?([{:ref, ^target_ref} | _], Map.get(link, "Dest"))
             end)

      assert {:ok, text} = Text.extract(pdf)
      assert text =~ "Second-page target"
      assert pdf =~ "/Subtype /Link"
    end
  end

  test "projected fragment targets report hidden and excessive accumulated geometry" do
    for transform <- ["rotateY(180deg)", "translateZ(300pt)"] do
      html =
        "<div style='margin:20pt;perspective:100pt;width:80pt;height:40pt'><div id='hidden' style='transform:#{transform};backface-visibility:hidden'>Hidden target</div></div><a href='#hidden'>Target link</a>"

      assert {:error, {:unsupported_html, diagnostic}} =
               HtmlToPdf.render(html,
                 page_size: {160, 110},
                 margin: 0,
                 system_font_discovery: false
               )

      assert diagnostic.source == "hidden"
    end

    html =
      "<div style='transform:scale3d(100000,100000,1)'><div id='large' style='transform:scale3d(100000,100000,1)'>Large</div></div><a href='#large'>Link</a>"

    assert {:error, {:invalid_layout, diagnostic}} =
             HtmlToPdf.render(html, system_font_discovery: false)

    assert diagnostic.source == "CSS 3D transform"
  end

  test "nested preserved contexts flatten once and share pixel blend modes" do
    alias NativeElixirPdfUtilities.HtmlToPdf.ProjectionGeometry, as: G
    bounds = %{x: 0.0, top: 30.0, width: 30.0, height: 30.0}

    descriptor = %{
      matrix: G.identity(),
      perspective: G.identity(),
      preserve: true,
      backface: :visible,
      bounds: bounds
    }

    rectangle = %{
      type: :rect,
      x: 3.0,
      y: 3.0,
      width: 12.0,
      height: 9.0,
      fill_color: {1.0, 0.0, 0.0},
      stroke_color: nil,
      stroke_width: 0.0,
      border_radius: 0.0
    }

    source = %{
      type: :drawing_group,
      x: 0.0,
      y: 0.0,
      width: 30.0,
      height: 30.0,
      transform: {1, 0, 0, 1, 0, 0},
      boxes: [rectangle]
    }

    blue =
      put_in(source.boxes, [
        %{rectangle | fill_color: {0.0, 0.0, 1.0}}
        |> Map.put(:css_compositing_scopes, [
          %{id: [0, 1], opacity: 1.0, mix_blend_mode: :Multiply}
        ])
      ])

    root = source |> Map.put(:css_projection, descriptor) |> Map.put(:boxes, [source, blue])
    assert {:ok, %{boxes: [image | _]}} = ProjectionScene.prepare(root, {30, 30}, [])
    {r, g, b, a} = ProjectionScene.sample(image, 9, 7)
    assert max(r, max(g, b)) < 0.01 and a > 0.99
    nested = put_in(root.css_projection.preserve, false) |> Map.put(:boxes, [root])
    assert {:ok, _} = ProjectionScene.prepare(nested, {30, 30}, [])

    near =
      put_in(
        root.css_projection.matrix,
        G.multiply(
          G.operation("perspective", [5]),
          G.around(G.operation("rotatey", [1.0]), 9, 9, 0)
        )
      )

    assert {:error, {:invalid_layout, diagnostic}} =
             ProjectionScene.prepare(%{nested | boxes: [near]}, {30, 30}, [])

    assert diagnostic.message =~ "eye plane"

    opaque = %{
      x: 0.0,
      y: 0.0,
      width: 1.0,
      height: 1.0,
      image: %{width_px: 1, height_px: 1, data: <<255, 0, 0>>}
    }

    assert ProjectionScene.sample(opaque, 0.5, 0.5) == {1.0, 0.0, 0.0, 1.0}
  end

  test "source paint and annotation phases share the operation's existing allowance" do
    alias NativeElixirPdfUtilities.HtmlToPdf.ProjectionGeometry, as: G
    alias NativeElixirPdfUtilities.Validators.{HtmlValidator, ResourceBudgetValidator}
    alias NativeElixirPdfUtilities.Limits

    source = %{
      type: :drawing_group,
      x: 0.0,
      y: 0.0,
      width: 30.0,
      height: 30.0,
      transform: {1, 0, 0, 1, 0, 0},
      css_projection: %{
        matrix: G.identity(),
        perspective: G.identity(),
        preserve: true,
        backface: :visible,
        bounds: %{x: 0.0, top: 30.0, width: 30.0, height: 30.0}
      },
      boxes: [
        %{
          type: :link_region,
          x: 3.0,
          y: 3.0,
          width: 12.0,
          height: 9.0,
          link_url: "https://example.com/plane-budget"
        }
      ]
    }

    reservations =
      ResourceBudgetValidator.with_budget(fn ->
        assert {:ok, _} =
                 HtmlValidator.with_render_budget(fn ->
                   ProjectionScene.prepare(source, {30, 30}, [])
                 end)

        ResourceBudgetValidator.snapshot()
      end)

    for limit <- [:max_entries, :max_work],
        headroom <- [reservations[limit] - 1, div(reservations[limit], 2)] do
      ResourceBudgetValidator.with_budget(fn ->
        assert :ok = ResourceBudgetValidator.reserve(limit, Limits.get(limit) - headroom, :layout)

        assert {:error, {:resource_limit_exceeded, diagnostic}} =
                 HtmlValidator.with_render_budget(fn ->
                   ProjectionScene.prepare(source, {30, 30}, [])
                 end)

        assert diagnostic.message =~ Atom.to_string(limit)
      end)
    end
  end

  test "near-plane rasterization omits the hidden half-plane and preserves authored link corners" do
    alias NativeElixirPdfUtilities.HtmlToPdf.ProjectionGeometry, as: G
    bounds = %{x: 0.0, top: 30.0, width: 30.0, height: 30.0}

    descriptor = %{
      matrix: G.identity(),
      perspective: G.identity(),
      preserve: true,
      backface: :visible,
      bounds: bounds
    }

    root = %{
      type: :drawing_group,
      role: :css_transform_plane,
      x: 0,
      y: 0,
      width: 30,
      height: 30,
      transform: {1, 0, 0, 1, 0, 0},
      css_projection: descriptor,
      boxes: []
    }

    link = %{
      type: :link_region,
      x: 3,
      y: 3,
      width: 12,
      height: 9,
      link_url: "https://example.com/corners"
    }

    for geometry <- [
          %{link_quad_points: [3, 12, 15, 12, 3, 3, 15, 3]},
          %{link_quads: [[{3, 3}, {15, 3}, {15, 12}, {3, 12}]]}
        ] do
      assert {:ok, %{boxes: boxes}} =
               ProjectionScene.prepare(%{root | boxes: [Map.merge(link, geometry)]}, {30, 30}, [])

      annotations = Enum.filter(boxes, &(Map.get(&1, :type) == :link_region))
      assert annotations != []
      assert Enum.all?(annotations, &(&1.x >= 3 and &1.x + &1.width <= 15.001))
    end

    rectangle = %{
      type: :rect,
      x: 0,
      y: 0,
      width: 30,
      height: 30,
      fill_color: {1, 0, 0},
      stroke_color: nil,
      stroke_width: 0,
      border_radius: 0
    }

    crossing = {1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0.02, 0.02, 0, -0.02}
    root = %{root | css_projection: %{descriptor | matrix: crossing}, boxes: [rectangle]}
    assert {:ok, %{boxes: [image | _]}} = ProjectionScene.prepare(root, {100, 100}, [])
    assert elem(ProjectionScene.sample(image, 20, 20), 3) == 0.0
    {r, _, _, a} = ProjectionScene.sample(image, 75, 75)
    assert r > 0.99 and a > 0.99
  end

  test "projected source ActualText spans remain paired and reject malformed group spans" do
    alias NativeElixirPdfUtilities.HtmlToPdf.{ProjectionGeometry, PdfWriter}

    source = %{
      type: :drawing_group,
      x: 0,
      y: 0,
      width: 30,
      height: 30,
      transform: {1, 0, 0, 1, 0, 0},
      css_projection: %{
        matrix: ProjectionGeometry.identity(),
        perspective: ProjectionGeometry.identity(),
        preserve: true,
        backface: :visible,
        bounds: %{x: 0, top: 30, width: 30, height: 30}
      },
      boxes: [
        %{type: :actual_text_start, text: "Source artwork"},
        %{
          type: :rect,
          x: 3,
          y: 3,
          width: 12,
          height: 9,
          fill_color: {1, 0, 0},
          stroke_color: nil,
          stroke_width: 0,
          border_radius: 0
        },
        %{type: :actual_text_end}
      ]
    }

    assert {:ok, prepared} = ProjectionScene.prepare(source, {30, 30}, [])
    assert Enum.any?(prepared.boxes, &(&1.type == :actual_text_start))
    assert Enum.any?(prepared.boxes, &(&1.type == :actual_text_end))
    assert {:ok, _} = PdfWriter.render([%{size: {30, 30}, boxes: [prepared]}])

    hidden =
      put_in(source.css_projection.matrix, ProjectionGeometry.operation("rotatey", [:math.pi()]))
      |> put_in([:css_projection, :backface], :hidden)

    assert {:ok, %{boxes: []}} = ProjectionScene.prepare(hidden, {30, 30}, [])

    group = Map.delete(source, :css_projection)

    for markers <- [
          [%{type: :actual_text_end}],
          [%{type: :actual_text_start, text: <<255>>}, %{type: :actual_text_end}]
        ] do
      assert {:error, {:invalid_pdf_input, diagnostic}} =
               PdfWriter.render([%{size: {30, 30}, boxes: [%{group | boxes: markers}]}])

      assert diagnostic.message =~ "ActualText"
    end
  end

  test "projected source sampling uses premultiplied bilinear alpha" do
    source = %{
      x: 0,
      y: 0,
      width: 2,
      height: 1,
      image: %{width_px: 2, height_px: 1, data: <<255, 0, 0, 0, 0, 255>>, alpha_data: <<255, 0>>}
    }

    assert {1.0, 0.0, 0.0, 0.5} == ProjectionScene.sample(source, 1, 0.5)
    assert {0.0, 0.0, 0.0, 0.0} == ProjectionScene.sample(source, 20, 20)

    assert {1.0, 0.0, 0.0, 1.0} ==
             ProjectionScene.sample(put_in(source.image.alpha_data, <<255, 255>>), 0.5, 0.5)
  end

  test "projected blend compositing follows independent encoded channel equations" do
    assert Compositing.over({1.0, 0.0, 0.0, 0.0}, {0.0, 1.0, 0.0, 1.0}, :Normal) ==
             {0.0, 1.0, 0.0, 1.0}

    assert Compositing.over({1.0, 0.0, 0.0, 0.5}, {0.0, 1.0, 0.0, 0.0}, :Normal) ==
             {1.0, 0.0, 0.0, 0.5}

    for mode <- [
          :Normal,
          :Multiply,
          :Screen,
          :Overlay,
          :Darken,
          :Lighten,
          :ColorDodge,
          :ColorBurn,
          :HardLight,
          :SoftLight,
          :Difference,
          :Exclusion,
          :Hue,
          :Saturation,
          :Color,
          :Luminosity
        ] do
      result = Compositing.over({0.8, 0.2, 0.6, 0.5}, {0.2, 0.6, 0.1, 1.0}, mode)
      assert elem(result, 3) == 1.0
      assert Enum.all?(Tuple.to_list(result), &(&1 >= 0 and &1 <= 1))
    end

    {r, g, b, 1.0} = Compositing.over({0.8, 0.2, 0.6, 0.5}, {0.2, 0.6, 0.1, 1.0}, :Multiply)
    assert_in_delta r, 0.18, 0.000001
    assert_in_delta g, 0.36, 0.000001
    assert_in_delta b, 0.08, 0.000001
  end
end
