defmodule NativeElixirPdfUtilities.HtmlToPdf.FlowShapeTest do
  use ExUnit.Case, async: true
  alias NativeElixirPdfUtilities.HtmlToPdf.{FlowShape, HtmlParser, Layout, Style}
  alias NativeElixirPdfUtilities.Validators.{CssShapeValidator, HtmlValidator}
  alias NativeElixirPdfUtilities.TestSupport.PngFixture

  # CSS Shapes 1, 12 June 2025, sections 2, 3, 4 and 6. Expected
  # circle intersections use sqrt(r*r-dy*dy), independently of native layout.
  test "circle and shape margins use their reference box and remain clipped to the float margin box" do
    HtmlValidator.with_render_budget(fn ->
      {:ok, %{shape_outside: shape}} =
        CssShapeValidator.prepare("shape-outside", "circle(20pt at 20pt 20pt)")

      float = float(%{shape_outside: shape, shape_margin: 5.0})
      prepared = FlowShape.prepare_float(float, 100)

      assert_in_delta elem(FlowShape.interval(prepared, 100, 99), 1),
                      20 + :math.sqrt(264),
                      0.00001

      assert FlowShape.interval(prepared, 80, 70) == {0, 40}
      assert FlowShape.interval(prepared, 50, 40) == nil
    end)
  end

  test "polygon exclusions follow edges rather than the float rectangle" do
    layout =
      layout("""
      <div style='width:100pt;font-size:10pt;line-height:10pt'>
        <div style='float:left;width:40pt;height:40pt;shape-outside:polygon(0 0,100% 100%,0 100%)'></div>
        <p style='margin:0'>a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a</p>
      </div>
      """)

    lines =
      layout.boxes
      |> Enum.filter(&(&1.type == :text))
      |> Enum.group_by(& &1.y)
      |> Enum.sort_by(&elem(&1, 0), :desc)

    for {{_, boxes}, expected} <- Enum.zip(Enum.take(lines, 3), [10, 20, 30]),
        do: assert_in_delta(Enum.min(Enum.map(boxes, & &1.x)), expected, 0.75 / 64)
  end

  test "a wide first word beside a curve descends only to the first fitting band" do
    tree =
      layout(
        "<div style='width:100pt;font-size:10pt;line-height:10pt'><div style='float:left;background:red;width:80pt;height:80pt;shape-outside:circle(40pt at 40pt 40pt)'></div><p style='margin:30pt 0 0'>abcdefgh</p></div>"
      )

    word = Enum.find(tree.boxes, &(Map.get(&1, :text) == "abcdefgh"))
    # The word needs about 47pt. A circle leaves that space above its bottom,
    # so moving the word all the way below the 80pt float loses usable space.
    float = Enum.find(tree.boxes, &(Map.get(&1, :fill_color) == {1, 0, 0}))

    assert elem(NativeElixirPdfUtilities.HtmlToPdf.PageGeometry.box_vertical_bounds(word), 0) >
             float.y

    assert word.y < float.y + 40
    assert word.x < 65
  end

  test "shape image alpha thresholds reuse the ordinary authorized image loader" do
    owner = self()

    png =
      PngFixture.build(3, 3, 6, 8, 0, fn x, y ->
        alpha =
          cond do
            y == 0 and x < 2 -> 255
            y == 1 -> 128
            true -> 0
          end

        [0, 0, 0, alpha]
      end)

    assert {:ok, dom} =
             HtmlParser.parse("""
             <div style='float:left;width:30pt;height:30pt;shape-outside:url(mask);shape-image-threshold:51%'></div>
             """)

    assert {:ok, styled} =
             Style.compute(dom,
               system_font_discovery: false,
               asset_resolver: fn request ->
                 send(owner, {:shape_requested, request.reference})
                 {:ok, png}
               end
             )

    assert_received {:shape_requested, "mask"}
    refute_received {:shape_requested, _}
    [node] = styled.children

    prepared =
      FlowShape.prepare_float(float(Map.merge(node.style, %{shape_outside: :image})), 100)

    assert FlowShape.interval(prepared, 100, 90) == {0, 40 * 2 / 3}
    assert FlowShape.interval(prepared, 85, 80) == nil
  end

  test "shape grammar rejects malformed vertices, negative margins and invalid alpha values" do
    for {property, value} <- [
          {"shape-outside", "polygon(0 0, 1pt)"},
          {"shape-outside", ""},
          {"shape-outside", "circle(1pt 2pt)"},
          {"shape-outside", "inset(auto)"},
          {"shape-margin", "-1pt"},
          {"shape-image-threshold", "nan"}
        ] do
      assert :error = CssShapeValidator.prepare(property, value)
    end

    assert {:ok, %{shape_image_threshold: 1.0}} =
             CssShapeValidator.prepare("shape-image-threshold", "200%")

    assert {:error, {:invalid_layout, diagnostic}} =
             CssShapeValidator.validate_style(%{shape_outside: %{type: :circle}}, "shape")

    assert diagnostic.stage == :layout
    assert diagnostic.source == "shape"
  end

  test "selected inside polygons restrict complete line bands and let overflow continue below the box" do
    layout =
      layout("""
      <p style='margin:0;width:100pt;height:30pt;font-size:10pt;line-height:10pt;shape-inside:polygon(20pt 0,80pt 0,80pt 30pt,20pt 30pt)'>
        word word word word word word word word word word word word word word word word
      </p>
      """)

    lines =
      layout.boxes
      |> Enum.filter(&(&1.type == :text))
      |> Enum.group_by(& &1.y)
      |> Enum.sort_by(&elem(&1, 0), :desc)

    assert length(lines) > 3

    for {_, boxes} <- Enum.take(lines, 3),
        do: assert_in_delta(Enum.min(Enum.map(boxes, & &1.x)), 20, 0.75 / 64)

    assert Enum.min(Enum.map(elem(Enum.at(lines, 3), 1), & &1.x)) == 0
  end

  test "inside shapes without a definite height preserve ordinary text wrapping" do
    [ordinary, shaped] =
      for shape <- ["auto", "circle(5pt)"] do
        layout(
          "<p style='margin:0;width:100pt;font-size:10pt;line-height:10pt;shape-inside:#{shape}'>word word word word word word word word</p>"
        ).boxes
        |> Enum.filter(&(&1.type == :text))
        |> Enum.map(&Map.take(&1, [:text, :x, :y, :width, :height]))
      end

    assert ordinary != []
    assert shaped == ordinary
  end

  test "ellipse, inset and rounded reference boxes follow independent edge intersections" do
    for {source, expected} <- [
          {"ellipse(20pt 10pt at 20pt 20pt)", {0, 40}},
          {"ellipse(closest-side farthest-side at 20pt 20pt)", {0, 40}},
          {"circle(closest-side)", {0, 40}},
          {"circle(farthest-side)", {0, 40}},
          {"inset(5pt)", {5, 35}},
          {"inset(5pt round 10pt / 5pt)", {5, 35}},
          {"border-box", {0, 40}},
          {"padding-box", {2, 38}},
          {"content-box", {5, 35}},
          {"inset(30pt)", {20, 20}}
        ] do
      {:ok, fields} = CssShapeValidator.prepare("shape-outside", source)
      edges = fn n -> %{top: n, right: n, bottom: n, left: n} end

      prepared =
        FlowShape.prepare_float(
          float(
            Map.merge(fields, %{border_widths: edges.(2), padding: edges.(3), border_radius: 5})
          ),
          100
        )

      case {FlowShape.interval(prepared, 80, 79), expected} do
        {nil, nil} ->
          :ok

        {{left, right}, {a, b}} ->
          assert_in_delta(left, a, 0.02)
          assert_in_delta(right, b, 0.02)
      end
    end
  end

  test "rounded polygons and elliptical margins follow expanded curved edges" do
    for source <- [
          "polygon(evenodd,0 0,20pt 20pt,0 40pt)",
          "ellipse(10pt 20pt)",
          "inset(0 round 40pt)",
          "margin-box"
        ] do
      {:ok, fields} = CssShapeValidator.prepare("shape-outside", source)

      prepared =
        FlowShape.prepare_float(
          float(Map.merge(fields, %{shape_margin: 3.0, border_radius: 8.0})),
          100
        )

      assert {left, right} = FlowShape.interval(prepared, 82, 78)
      assert left >= 0 and right <= 40 and right > left
    end

    {:ok, fields} =
      CssShapeValidator.prepare("shape-outside", "polygon(10pt 10pt,10pt 10pt,20pt 20pt)")

    assert {_, _} =
             fields
             |> Map.put(:shape_margin, 3.0)
             |> float()
             |> FlowShape.prepare_float(100)
             |> FlowShape.interval(90, 88)
  end

  test "inside circles move a wide first word to the first fitting curved band" do
    {:ok, fields} = CssShapeValidator.prepare("shape-inside", "circle(20pt)")
    inside = FlowShape.prepare_inside(fields, 0, 100, 40, 40)
    assert FlowShape.inside_interval(inside, 100, 90) == nil
    top = FlowShape.fit_inside(inside, 100, 10, 20)
    assert_in_delta top, 80 + :math.sqrt(300), 0.02
    assert FlowShape.fit_inside(inside, 100, 10, 100) == 60
    assert FlowShape.prepare_inside(%{}, 0, 100, 40, nil) == nil
  end

  test "alpha geometry handles opaque and sixteen-bit decoded image rows without new assets" do
    image = %{
      width_px: 2,
      height_px: 2,
      bits_per_component: 16,
      alpha_data: <<65535::16, 0::16, 0::16, 65535::16>>
    }

    prepared =
      FlowShape.prepare_float(
        float(%{shape_outside: :image, shape_image: image, shape_image_threshold: 0.5}),
        100
      )

    assert FlowShape.interval(prepared, 100, 90) == {0, 20}
    assert FlowShape.interval(prepared, 79, 70) == {20, 40}

    for {threshold, expected} <- [{0.5, {0, 40}}, {1.0, nil}] do
      prepared =
        FlowShape.prepare_float(
          float(%{
            shape_outside: :image,
            shape_image: Map.delete(image, :alpha_data),
            shape_image_threshold: threshold
          }),
          100
        )

      assert FlowShape.interval(prepared, 100, 90) == expected
    end
  end

  test "TIFF orientations transform alpha-mask axes without adding fixture assets" do
    image = %{
      width_px: 3,
      height_px: 2,
      bits_per_component: 8,
      alpha_data: <<255, 0, 0, 0, 0, 0>>
    }

    # Exif 1..8 map the original top-left sample to these independent grid cells.
    for {orientation, column, row, columns, rows} <- [
          {1, 0, 0, 3, 2},
          {2, 2, 0, 3, 2},
          {3, 2, 1, 3, 2},
          {4, 0, 1, 3, 2},
          {5, 0, 0, 2, 3},
          {6, 1, 0, 2, 3},
          {7, 1, 2, 2, 3},
          {8, 0, 2, 2, 3}
        ] do
      prepared =
        FlowShape.prepare_float(
          float(%{
            shape_outside: :image,
            shape_image: Map.put(image, :image_orientation, orientation),
            shape_image_threshold: 0.5
          }),
          100
        )

      top = 100 - row * 40 / rows - 0.1
      bottom = top - 40 / rows + 0.2
      {left, right} = FlowShape.interval(prepared, top, bottom)
      assert_in_delta left, column * 40 / columns, 0.000001
      assert_in_delta right, (column + 1) * 40 / columns, 0.000001
    end
  end

  test "inside polygon fitting and asymmetric reference-box radii retain exact edges" do
    {:ok, fields} =
      CssShapeValidator.prepare("shape-inside", "polygon(20pt 0,40pt 20pt,20pt 40pt,0 20pt)")

    inside = FlowShape.prepare_inside(fields, 0, 100, 40, 40)
    assert_in_delta FlowShape.fit_inside(inside, 100, 10, 20), 90, 0.02
    {:ok, fields} = CssShapeValidator.prepare("shape-outside", "padding-box")

    prepared =
      FlowShape.prepare_float(
        float(
          Map.merge(fields, %{
            border_radius: 8,
            border_widths: %{top: 2, right: 2, bottom: 3, left: 4}
          })
        ),
        100
      )

    assert {left, right} = FlowShape.interval(prepared, 80, 79)
    assert_in_delta left, 4, 0.02
    assert_in_delta right, 38, 0.02
    {:ok, fields} = CssShapeValidator.prepare("shape-outside", "ellipse(10pt 0)")
    prepared = fields |> float() |> FlowShape.prepare_float(100)
    assert FlowShape.interval(prepared, 80, 79) == {10, 30}
    assert FlowShape.interval(prepared, 90, 89) == nil
  end

  defp float(style) do
    %{
      left: 0,
      right: 40,
      top: 100,
      bottom: 60,
      border: %{x: 0, top: 100, width: 40, height: 40},
      style: style
    }
  end

  test "first atomic runs descend at polygon edges and image alpha transitions" do
    png =
      PngFixture.build(3, 3, 6, 8, 0, fn _, row -> [0, 0, 0, if(row == 0, do: 255, else: 0)] end)

    uri = "data:image/png;base64," <> Base.encode64(png)

    for shape <- ["none", "polygon(0 0,100% 0,0 100%)", "url(#{uri})"] do
      tree =
        layout(
          "<div style='width:100pt;font-size:10pt;line-height:10pt'><img src='#{uri}' style='float:left;width:80pt;height:60pt;shape-outside:#{shape}'><span style='display:inline-block;width:50pt;height:10pt;background:red'>Atom</span></div>"
        )

      atom = Enum.find(tree.boxes, &(Map.get(&1, :fill_color) == {1, 0, 0}))
      assert atom.y >= 230
      assert atom.x <= 50 + 0.75 / 64
    end
  end

  test "an inside line entirely outside the ellipse has no usable interval" do
    shape = %{
      geometry: {:ellipse, 20, 80, 10, 10},
      clip: %{x: 0, top: 100, width: 40, height: 40}
    }

    assert FlowShape.inside_interval(shape, 100, 95) == nil
  end

  defp layout(html) do
    assert {:ok, dom} = HtmlParser.parse(html)
    assert {:ok, styled} = Style.compute(dom, system_font_discovery: false)
    assert {:ok, layout} = Layout.layout(styled, page_size: {300, 300}, margin: 0)
    layout
  end
end
