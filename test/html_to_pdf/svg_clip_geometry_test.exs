defmodule NativeElixirPdfUtilities.HtmlToPdf.SvgClipGeometryTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.{HtmlToPdf, Limits}
  alias NativeElixirPdfUtilities.HtmlToPdf.{PdfWriter, SvgClipGeometry}
  alias NativeElixirPdfUtilities.Validators.SvgClipGeometryValidator

  setup do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    %{limits: limits}
  end

  test "evenodd holes retain exact disjoint link quadrilaterals" do
    commands = [
      {0, [0, 0]},
      {1, [100, 0]},
      {1, [100, 100]},
      {1, [0, 100]},
      {4, []},
      {0, [30, 30]},
      {1, [70, 30]},
      {1, [70, 70]},
      {1, [30, 70]},
      {4, []}
    ]

    codes =
      [10, 1, length(commands)] ++ Enum.flat_map(commands, fn {op, points} -> [op | points] end)

    assert {:ok, geometry} = SvgClipGeometryValidator.prepare(codes)

    assert {:ok, quads} =
             SvgClipGeometryValidator.project(
               geometry,
               [{0, 0}, {100, 0}, {100, 100}, {0, 100}],
               {0, 0, 100, 100}
             )

    assert_in_delta area(quads), 8400, 0.00001
    refute covers?(quads, {50, 50})
    assert covers?(quads, {10, 50})
  end

  test "independent child unions preserve opposite winding while linked clips intersect" do
    left = SvgClipGeometry.polygon([{0, 0}, {60, 0}, {60, 100}, {0, 100}])
    right = SvgClipGeometry.polygon([{40, 100}, {100, 100}, {100, 0}, {40, 0}])
    triangle = SvgClipGeometry.polygon([{0, 0}, {100, 0}, {0, 100}, {0, 100}])
    geometry = {:intersection, [{:union, [left, right]}, triangle]}

    assert {:ok, quads} =
             SvgClipGeometryValidator.project(
               geometry,
               [{0, 0}, {100, 0}, {100, 100}, {0, 100}],
               {0, 0, 100, 100}
             )

    assert_in_delta area(quads), 5000, 0.00001
    assert covers?(quads, {50, 25})
    refute covers?(quads, {75, 75})
  end

  test "actual Bezier clip geometry projects curved edges without a rectangular hit area" do
    # An authored quadratic closes a curved cap. Its exact integral area is 200/3.
    codes = [10, 0, 3, 0, 0, 0, 2, 10, 20, 20, 0, 4]
    assert {:ok, geometry} = SvgClipGeometryValidator.prepare(codes)

    assert {:ok, quads} =
             SvgClipGeometryValidator.project(
               geometry,
               [{0, 0}, {20, 0}, {20, 20}, {0, 20}],
               {0, 0, 20, 20}
             )

    assert_in_delta area(quads), 400 / 3, 0.02
    assert covers?(quads, {10, 5})
    refute covers?(quads, {1, 9})
  end

  test "collinear reversing Beziers preserve their actual extent and arbitrary transformed curves terminate" do
    commands = [{0, [0, 0]}, {3, [30, 0, -10, 0, 10, 0]}, {1, [10, 10]}, {1, [0, 10]}, {4, []}]
    geometry = SvgClipGeometry.flatten(commands, 0, 1 / 1024)
    xs = geometry |> SvgClipGeometry.edges() |> Enum.flat_map(fn {{x, _}, {a, _}} -> [x, a] end)
    assert Enum.max(xs) > 10

    assert {:ok, _} =
             SvgClipGeometryValidator.project(
               geometry,
               [{0, 0}, {30, 0}, {30, 10}, {0, 10}],
               {0, 0, 30, 10}
             )

    geometry =
      SvgClipGeometry.flatten(
        [{0, [0.234, 0.456]}, {3, [30.324, 54.645, 40.328, -20.172, 80.253, 50.341]}, {4, []}],
        0,
        1 / 1024
      )

    assert length(SvgClipGeometry.edges(geometry)) < 1000
  end

  test "translated clipped drawing groups retain quad hit regions and resource failures remain recoverable",
       %{limits: limits} do
    region = %{
      type: :link_region,
      x: 0,
      y: 0,
      width: 20,
      height: 20,
      link_url: "https://example.test/group",
      link_quad_points: [0, 20, 20, 20, 0, 0, 20, 0]
    }

    group = %{
      type: :drawing_group,
      width: 20,
      height: 20,
      transform: {2, 0, 0, 2, 10, 10},
      clip: %{x: 5, y: 5, width: 10, height: 10},
      boxes: [region]
    }

    assert {:ok, pdf} = PdfWriter.render([%{size: {100, 100}, boxes: [group]}])
    assert pdf =~ "/QuadPoints [20 40 40 40 20 20 40 20]"
    Limits.install(Map.put(limits, :max_svg_clip_projection_work, 10))

    assert {:error, {:resource_limit_exceeded, %{message: message}}} =
             PdfWriter.render([%{size: {100, 100}, boxes: [group]}])

    assert message =~ "max_svg_clip_projection_work"

    for {polygon, viewport} <- [
          {nil, nil},
          {[1, 2, 3, 4], {0, 0, 100, 100}},
          {[{0, 0}, {100, 100}, {0, 100}, {100, 0}], {0, 0, 100, 100}}
        ] do
      assert {:error, {:invalid_document, %{source: "NativeHtmlClipGeometry"}}} =
               SvgClipGeometryValidator.project(nil, polygon, viewport)
    end
  end

  test "native geometry rejects malformed commands and bounded projection reports actionable limits",
       %{limits: limits} do
    for codes <- [
          nil,
          [10, 0, 1, 1, 10, 10],
          [10, 2, 0],
          [21, 1],
          [10, 0, 1, 0, 1.0e100, 2],
          [20, 0, 20, 0]
        ] do
      assert {:error, {:invalid_document, diagnostic}} = SvgClipGeometryValidator.prepare(codes)
      assert diagnostic.stage == :svg
      assert diagnostic.source == "NativeHtmlClipGeometry"
      assert diagnostic.operation == :project
      assert diagnostic.message != ""
    end

    Limits.install(Map.put(limits, :max_svg_clip_projection_work, 10))

    assert {:error, {_, diagnostic}} =
             SvgClipGeometryValidator.project(
               nil,
               [{0, 0}, {100, 0}, {100, 100}, {0, 100}],
               {0, 0, 100, 100}
             )

    assert diagnostic.message =~ "max_svg_clip_projection_work"
  end

  test "writer retains genuine quadrilateral navigation and rejects malformed hit regions" do
    region = %{
      type: :link_region,
      x: 0,
      y: 0,
      width: 20,
      height: 20,
      link_url: "https://example.test/clip",
      link_quad_points: [0, 20, 20, 0, 0, 0, 20, 0]
    }

    assert {:ok, pdf} = PdfWriter.render([%{size: {100, 100}, boxes: [region]}])
    assert pdf =~ "/QuadPoints [0 20 0 20 0 0 20 0]"

    fractional = %{
      type: :link_region,
      x: 11.3525,
      y: 44.9843,
      width: 64.29499999999999,
      height: 0.36660000000000537,
      link_url: "https://example.test/fractional",
      link_quad_points: [11.4396, 45.3509, 75.5604, 45.3509, 11.3525, 44.9843, 75.6475, 44.9843]
    }

    assert {:ok, _} = PdfWriter.render([%{size: {100, 100}, boxes: [fractional]}])

    for quad <- [
          [0, 0],
          [0, 20, 30, 20, 0, 0, 20, 0],
          [0, 0, 20, 20, 0, 20, 20, 0],
          List.duplicate(0, 8)
        ] do
      assert {:error, {_, %{stage: :pdf, reason: _, message: message}}} =
               PdfWriter.render([
                 %{size: {100, 100}, boxes: [Map.put(region, :link_quad_points, quad)]}
               ])

      assert message != ""
    end
  end

  test "page bounds and nested foreign Form projection retain genuine triangle hit geometry" do
    alias NativeElixirPdfUtilities.Validators.SvgFormSlotValidator

    triangle = %{
      type: :link_region,
      x: 0,
      y: 0,
      width: 20,
      height: 20,
      link_url: "https://example.test/triangle",
      link_quad_points: [10, 20, 10, 20, 0, 0, 20, 0]
    }

    assert {:ok, pdf} = PdfWriter.render([%{size: {10, 10}, boxes: [triangle]}])
    assert {:ok, document} = NativeElixirPdfUtilities.Pdf.Reader.read(pdf)

    [link] =
      for {_, %{value: %{"Subtype" => {:name, "Link"}} = link}} <- document.objects, do: link

    assert link["Rect"] == [0, 0, 10, 10]
    assert Enum.all?(link["QuadPoints"], &(&1 >= 0 and &1 <= 10))

    paint = %{
      type: :rect,
      x: 0,
      y: 0,
      width: 20,
      height: 20,
      fill_color: {1, 0, 0},
      stroke_color: nil,
      stroke_width: 0,
      border_radius: 0,
      shape: {:triangle, :up}
    }

    group = %{
      type: :drawing_group,
      width: 20,
      height: 20,
      transform: {1, 0, 0, 1, 0, 0},
      boxes: [paint, triangle]
    }

    placement = %{
      width: 20,
      height: 20,
      transform: {1, 0, 0, 1, 0, 0},
      group: group,
      clip: nil,
      clip_geometry: nil
    }

    assert {:ok, links} = SvgFormSlotValidator.prepare_links([placement], 20, 20)
    quads = Enum.map(links, & &1.link_quad_points)
    assert_in_delta area(quads), 200, 0.0001
    refute covers?(quads, {15, 15})
    assert covers?(quads, {2, 2})
  end

  test "cached foreign Forms retain real clip projection work and issued target identity", %{
    limits: limits
  } do
    alias NativeElixirPdfUtilities.HtmlToPdf.{ResourceContext, HtmlParser, Style, SvgScene}
    alias NativeElixirPdfUtilities.Validators.HtmlValidator

    ResourceContext.run([], HtmlValidator.new_image_budget(), fn context ->
      html =
        "<svg width='100' height='40'><defs><clipPath id='shape'><polygon points='0,40 50,0 100,40'/></clipPath></defs><foreignObject width='100' height='40' clip-path='url(#shape)'><div><a href='https://example.test/cached'>Real clipped link</a></div></foreignObject></svg>"

      assert {:ok, dom} = HtmlParser.parse_detailed(html)
      assert {:ok, %{children: [node]}} = Style.compute_detailed(dom, [], context.image_budget)
      scene = node.style.svg_scene
      assert {:ok, painted} = SvgScene.paint(scene, 75, 30)
      assert painted.links != []
      assert {:ok, ^painted} = SvgScene.paint(scene, 75, 30)
      used = Process.get({HtmlValidator, :render_budget})[:max_svg_clip_projection_work]
      assert used > 0
      Limits.install(Map.put(limits, :max_svg_clip_projection_work, used))

      assert {:error, {:resource_limit_exceeded, %{message: message}}} =
               HtmlValidator.with_render_budget(fn -> SvgScene.paint(scene, 75, 30) end)

      assert message =~ "max_svg_clip_projection_work"
    end)
  end

  test "real foreign HTML image links follow resolved evenodd clip holes in the delivered PDF" do
    png =
      File.read!(
        Path.expand(
          "../fixtures/html_to_pdf/browser_parity/assets/svg-resource-colors.png",
          __DIR__
        )
      )

    html =
      "<svg width='100' height='100'><defs><clipPath id='ring'><path d='M0 0H100V100H0Z M30 30H70V70H30Z' clip-rule='evenodd'/></clipPath></defs><foreignObject width='100' height='100' clip-path='url(#ring)'><div style='line-height:0'><a href='https://example.test/ring'><img src='paint.png' width='100' height='100'/></a></div></foreignObject></svg>"

    assert {:ok, pdf} =
             HtmlToPdf.render(html,
               assets: %{"paint.png" => {:bytes, png}},
               page_size: "100pt 100pt",
               margin: 0
             )

    assert {:ok, document} = NativeElixirPdfUtilities.Pdf.Reader.read(pdf)

    links =
      document.objects
      |> Map.values()
      |> Enum.filter(&(is_map(&1.value) and &1.value["Subtype"] == {:name, "Link"}))

    assert links != []

    quads =
      Enum.map(links, fn link ->
        assert link.value["A"]["URI"] == {:string, "https://example.test/ring"}
        assert length(link.value["QuadPoints"]) == 8
        link.value["QuadPoints"]
      end)

    refute covers?(quads, {37.5, 62.5})
    assert covers?(quads, {7.5, 62.5})
    assert_in_delta area(quads), 8400 * 0.75 * 0.75, 0.01
  end

  defp covers?(quads, {x, y}) do
    Enum.any?(quads, fn [a, top, b, _, c, bottom, d, _] ->
      if y > bottom and y < top do
        t = (y - bottom) / (top - bottom)
        x > c + t * (a - c) and x < d + t * (b - d)
      else
        false
      end
    end)
  end

  defp area(quads) do
    Enum.reduce(quads, 0, fn [a, top, b, _, c, bottom, d, _], area ->
      area + (top - bottom) * (b - a + d - c) / 2
    end)
  end
end
