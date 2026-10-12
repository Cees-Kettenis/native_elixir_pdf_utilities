defmodule NativeElixirPdfUtilities.HtmlToPdf.SvgSceneTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.{HtmlToPdf, Limits, Text, Forms}
  alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, Style, Layout, SvgScene, SvgBackend}
  alias NativeElixirPdfUtilities.Validators.{HtmlValidator, SvgSceneValidator}

  setup do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    %{limits: limits}
  end

  test "public inline SVG paints shapes and retains surrounding Unicode and parent links" do
    html = """
    <div style='font-family:DejaVu Sans;font-size:10pt;line-height:14pt'>Before Ω
    <a href='https://example.test/svg'><svg width=40 height=20 viewBox='0 0 40 20'>
    <rect width=40 height=20 fill=red /></svg></a>After café</div>
    """

    assert {:ok, pdf} = HtmlToPdf.render(html, page_size: {200, 100}, margin: 0)
    assert {:ok, text} = Text.extract(pdf)
    assert text =~ "Before Ω" and text =~ "After café"
    assert pdf =~ "/Subtype /Form"
    assert pdf =~ "https://example.test/svg"
    assert {:ok, []} = Forms.fields(pdf)

    assert {:ok, dom} = HtmlParser.parse_detailed(html)
    assert {:ok, styled} = Style.compute_detailed(dom)
    assert {:ok, layout} = Layout.layout(styled, page_size: {200, 100}, margin: 0)
    group = Enum.find(layout.boxes, &(&1.type == :drawing_group))
    after_text = Enum.find(layout.boxes, &(Map.get(&1, :text) == "After café"))
    assert_in_delta group.y, after_text.y, 0.001
    assert {group.width, group.height} == {30.0, 15.0}
    assert group.svg_scene_contract.logical_text == []
    assert group.svg_scene_contract.selected_fonts == []
    assert group.svg_scene_contract.ink_extent == {+0.0, +0.0, 30.0, 15.0}
    assert hd(group.boxes).link_rect == %{x: +0.0, y: +0.0, width: 30.0, height: 15.0}
  end

  test "public block SVG scenes keep their page count, text, and link annotations" do
    fixture =
      Path.expand("../fixtures/html_to_pdf/browser_parity/inline_svg_atomic_pages.html", __DIR__)

    assert {:ok, pdf} = fixture |> File.read!() |> HtmlToPdf.render()
    assert {:ok, context} = NativeElixirPdfUtilities.Pdf.Reader.read_validated(pdf)
    assert length(context.pages) == 2
    assert {:ok, text} = Text.extract(pdf)
    assert text =~ "First atomic scene Ω"
    assert text =~ "Second atomic scene café"
    assert text =~ "Both atomic scenes survived"
    assert pdf =~ "https://example.test/svg-first"
    assert pdf =~ "https://example.test/svg-second"
    assert pdf =~ "/Subtype /Link"
  end

  test "vector Form objects obey the shared PDF object budget", %{limits: limits} do
    Limits.install(Map.put(limits, :max_pdf_objects, 8))

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             HtmlToPdf.render("<svg width=20 height=20><rect width=20 height=20 /></svg>")

    assert diagnostic.stage == :writer
    assert diagnostic.reason == :resource_limit_exceeded
    assert diagnostic.message =~ "max_pdf_objects"
  end

  test "computed scene retains affine artwork and separates transparent ink from layout" do
    html = """
    <svg width=80 height=40 color=blue><title>Metadata</title><desc>Validated</desc>
    <style>rect{fill:currentColor}</style><g transform='translate(10 5) rotate(20) skewX(10) scale(-1 1)'>
    <rect x=-20 width=20 height=10 /></g></svg>
    """

    assert {:ok, dom} = HtmlParser.parse_detailed(html)
    assert {:ok, %{children: [svg]}} = Style.compute_detailed(dom)
    scene = svg.style.svg_scene
    assert scene.source =~ "matrix("
    assert scene.source =~ "rgb(0,0,255)"
    refute scene.source =~ "Metadata"
    assert {:ok, painted} = SvgScene.paint(scene, 200.0)
    assert painted.layout_extent == {+0.0, +0.0, 60.0, 30.0}
    assert {x, y, w, h} = painted.ink_extent
    assert x >= 0 and y >= 0 and w < 60 and h < 30
    assert painted.baseline == 30.0
  end

  test "zero and hidden scenes validate their contents, preserve flow and paint no artwork" do
    for attrs <- [
          "width=0 height=20",
          "width=20 height=0",
          "width=20 height=20 viewBox='0 0 0 20'",
          "width=20 height=20 visibility=hidden",
          "width=20 height=20 style='display:none'"
        ] do
      html = "<svg #{attrs}><rect width=20 height=20 /></svg><p>Survives</p>"
      assert {:ok, pdf} = HtmlToPdf.render(html)
      refute pdf =~ "/Subtype /Image"
      refute pdf =~ "/Subtype /Form"
      assert {:ok, text} = Text.extract(pdf)
      assert text =~ "Survives"
    end

    assert {:error, {:unsupported_html, diagnostic}} =
             HtmlToPdf.render("<svg width=0 height=0><text>Cannot omit</text></svg>")

    assert diagnostic.message =~ "cannot omit"
  end

  test "missing logical content and effects diagnose before successful artwork" do
    for content <- [
          "<text>Ω</text>",
          "<tspan>text</tspan>",
          "<foreignObject><div>HTML</div></foreignObject>",
          "<defs><linearGradient id='g' /></defs>",
          "<image href='https://example.test/private.png' />",
          "<a href='https://example.test/'><rect width=10 height=10 /></a>",
          "<clipPath id='clip'><rect width=10 height=10 /></clipPath>"
        ] do
      assert {:error, {_reason, diagnostic}} =
               HtmlToPdf.render("<svg width=20 height=20>#{content}</svg>")

      assert diagnostic.stage in [:html, :style, :layout]
      assert is_binary(diagnostic.message) and diagnostic.message != ""
    end

    for attrs <- [
          "filter='url(#f)'",
          "mask='url(#m)'",
          "vector-effect='non-scaling-stroke'",
          "pathLength='20'"
        ] do
      assert {:error, {:unsupported_html, %{source: "rect"}}} =
               HtmlToPdf.render(
                 "<svg width=20 height=20><rect width=10 height=10 #{attrs} /></svg>"
               )
    end
  end

  test "outer geometry that needs complete composition diagnoses instead of discarding it" do
    for declaration <- [
          "transform:rotate(10)",
          "overflow:visible",
          "box-sizing:border-box",
          "border-radius:4px",
          "position:absolute",
          "object-fit:contain",
          "display:flex",
          "opacity:.5;background:red",
          "opacity:.5;border:1px solid red"
        ] do
      assert {:error, {:unsupported_html, diagnostic}} =
               HtmlToPdf.render(
                 "<svg width=20 height=20 style='#{declaration}'><rect width=20 height=20 /></svg>"
               )

      assert diagnostic.module == SvgSceneValidator
      assert diagnostic.stage == :layout
    end

    for parent <- ["flex", "grid"] do
      assert {:error, {:unsupported_html, %{message: message}}} =
               HtmlToPdf.render("<div style='display:#{parent}'><svg width=20 height=20 /></div>")

      assert message =~ "flex/grid"
    end
  end

  test "path and points validation prevents native parser recovery from omitting malformed geometry" do
    for content <- [
          "<path d='L 1 2' />",
          "<path d='M 1 2 L 3' />",
          "<path d='M 0 0 A 10 10 0 2 0 20 20' />",
          "<path d='M 1e999 0' />",
          "<polygon points='0,0 10' />",
          "<polyline points='0,0,,10,10' />",
          "<circle r=-1 />",
          "<ellipse rx=nope />",
          "<rect width=-1 />"
        ] do
      assert {:error, {:invalid_document, diagnostic}} =
               HtmlToPdf.render("<svg width=20 height=20>#{content}</svg>")

      assert diagnostic.module == SvgSceneValidator
      assert diagnostic.operation == :prepare
    end

    for d <- [
          "",
          "M0 0 L10 10 H15 V5 Z",
          "m1 1 c1 2 3 4 5 6 s2 3 4 5 q1 2 3 4 t5 6 a2 3 10 0 1 4 5 z",
          "M0. 0. L10. 10."
        ] do
      assert {:ok, _} =
               HtmlToPdf.render(
                 "<svg width=40 height=40><path d='#{d}' stroke=red fill=none /></svg>"
               )
    end
  end

  test "authored metadata, IDs and processing instructions validate before omission" do
    for content <- [
          "<title onload='x'>Unsafe</title>",
          "<rect id=duplicate /><circle id=duplicate />",
          "<style>@import url(https://example.test/private.css);</style>",
          "<?renderer bad?>",
          "<rect width=1 height=1><circle r=1 /></rect>"
        ] do
      assert {:error, {_reason, diagnostic}} =
               HtmlToPdf.render("<svg width=20 height=20>#{content}</svg>")

      assert diagnostic.message != ""
    end
  end

  test "scene aggregate path and opacity accounting use central limits", %{limits: limits} do
    Limits.install(Map.put(limits, :max_svg_path_segments, 2))

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             HtmlToPdf.render("<svg width=20 height=20><path d='M0 0 L1 1 L2 2' /></svg>")

    assert diagnostic.message =~ "max_svg_path_segments"
    Limits.install(Map.put(limits, :max_aggregate_svg_surface_pixels, 100))

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             SvgBackend.rasterize(
               "<svg width='10' height='10'><g opacity='.5'><rect width='10' height='10' /></g></svg>",
               [width: 10, height: 10],
               %{mode: :inline}
             )

    assert diagnostic.message =~ "max_aggregate_svg_surface_pixels"
  end

  test "vector output uses the pinned shape tree and expands curved dashed strokes" do
    source =
      "<svg width='80' height='40'><path d='M2 30 C5 0 40 0 65 30' fill='none' stroke='red' stroke-width='4' stroke-linecap='round' stroke-dasharray='10 4'/></svg>"

    assert {:ok, pdf} = SvgBackend.vectorize(source, [width: 80, height: 40], %{mode: :inline})
    assert {:ok, context} = NativeElixirPdfUtilities.Pdf.Reader.read(pdf)

    streams =
      context.objects |> Map.values() |> Enum.map_join("\n", &(Map.get(&1, :stream) || ""))

    assert streams =~ " c"
    assert streams =~ "f\n"
    refute streams =~ "[10 4]"
    refute streams =~ "S\n"
    refute pdf =~ "/Font"
    refute pdf =~ "/Subtype /Image"

    assert {:error, {:unsupported_html, diagnostic}} =
             SvgBackend.vectorize(source, [width: 80, height: 40], %{mode: :image})

    assert diagnostic.operation == :vectorize
    assert diagnostic.message =~ "inline shape scene"
  end

  test "sibling opacity groups retain every form and graphics state" do
    source =
      "<svg width='40' height='20'><g opacity='.5'><rect width='20' height='20' fill='red'/></g><g opacity='.25'><rect x='20' width='20' height='20' fill='blue'/></g></svg>"

    assert {:ok, pdf} = SvgBackend.vectorize(source, [width: 40, height: 20], %{mode: :inline})
    assert {:ok, document} = NativeElixirPdfUtilities.Pdf.Reader.read(pdf)
    assert [page] = document.pages
    assert map_size(page.resources["XObject"]) == 2
    assert map_size(page.resources["ExtGState"]) == 2
  end

  test "stroke work is bounded before rasterization can omit an excessive dash pattern", %{
    limits: limits
  } do
    html =
      "<svg width=100 height=20><path d='M0 10 H100' stroke=red stroke-width=2 stroke-dasharray='.000001 .000001'/></svg>"

    assert {:error, {:resource_limit_exceeded, diagnostic}} = HtmlToPdf.render(html)
    assert diagnostic.message =~ "max_svg_stroke_work"
    assert diagnostic.source == "SVG"

    Limits.install(Map.put(limits, :max_svg_stroke_work, 1))

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             HtmlToPdf.render("<svg width=20 height=20><rect width=10 height=10 /></svg>")

    assert diagnostic.message =~ "max_svg_stroke_work"
  end

  test "geometry fixture renders all authored styles through the public API" do
    html =
      File.read!(
        Path.expand("../fixtures/html_to_pdf/browser_parity/inline_svg_geometry.html", __DIR__)
      )

    assert {:ok, pdf} = HtmlToPdf.render(html)
    assert {:ok, text} = Text.extract(pdf)
    assert text =~ "End of geometry"

    html =
      "<svg width=80 height=40><g fill=transparent><rect width=10 height=10 /></g><rect x='2pt' y='10%' width='1em' height='1rem' fill='rgba(0,0,255,.5)' stroke-dashoffset='-1' stroke-miterlimit='2' stroke-dasharray='none' paint-order='stroke fill'/></svg>"

    assert {:ok, pdf} = HtmlToPdf.render(html)
    assert pdf =~ "/Subtype /Form"

    for value <- ["unset", "initial"] do
      assert {:ok, _} =
               HtmlToPdf.render(
                 "<svg color='#{value}' width=10 height=10><rect width=10 height=10 /></svg>"
               )
    end
  end

  test "shared translation keeps local group coordinates and moves image link rectangles once" do
    alias NativeElixirPdfUtilities.HtmlToPdf.DrawingGeometry
    rect = %{x: 1, y: 2, width: 3, height: 4}
    image = %{type: :image, x: 1, y: 2, clip: rect, link_rect: rect}
    shifted = DrawingGeometry.translate_box(image, 10, -20)
    assert shifted.x == 11 and shifted.y == -18
    assert shifted.clip == %{rect | x: 11, y: -18}
    assert shifted.link_rect == shifted.clip

    group = %{
      type: :drawing_group,
      x: 1,
      y: 2,
      transform: {2, 0, 0, 3, 1, 2},
      clip: rect,
      boxes: [image]
    }

    shifted = DrawingGeometry.translate_box(group, 10, -20)
    assert shifted.transform == {2, 0, 0, 3, 11, -18}
    assert shifted.clip == rect and shifted.boxes == [image]
    assert DrawingGeometry.translate_box(%{type: :marker}, 1, 2) == %{type: :marker}
  end

  test "malformed prepared sources and invalid form page structure diagnose" do
    alias NativeElixirPdfUtilities.Validators.{SvgFormValidator, SvgStyleValidator}

    assert {:ok, dom} =
             HtmlParser.parse_detailed("<svg width=20 height=20><foreignObject/></svg>")

    assert {:error, {:unsupported_html, _}} = Style.load_stylesheets(dom, [])

    assert {:error, {:invalid_document, _}} =
             SvgBackend.rasterize("<svg width='0' height='20'/>", [width: 20, height: 20], %{
               mode: :inline
             })

    assert {:error, {:invalid_document, _}} =
             SvgBackend.rasterize(
               "<svg width='20' height='20'><path d='L1 2'/></svg>",
               [width: 20, height: 20],
               %{mode: :inline}
             )

    assert {:ok, pdf} =
             NativeElixirPdfUtilities.HtmlToPdf.PdfWriter.render([
               %{size: {20, 20}, boxes: []},
               %{size: {20, 20}, boxes: []}
             ])

    assert {:error, {:invalid_pdf_input, %{stage: :svg}}} = SvgFormValidator.prepare_source(pdf)

    for {property, value} <- [
          {"stroke-linecap", "bad"},
          {"stroke-linejoin", "bad"},
          {"shape-rendering", "bad"},
          {"paint-order", "fill fill"},
          {"stroke-miterlimit", "0"}
        ] do
      assert :error = SvgStyleValidator.parse(property, value)
    end

    assert {:ok, %{children: [svg]}} =
             Style.compute_detailed(
               elem(HtmlParser.parse_detailed("<svg width=20 height=20 />"), 1)
             )

    assert {:ok, viewport} = SvgScene.measure(svg.style.svg_scene, 20.0)
    assert viewport.width == 15.0

    assert SvgScene.authored_source(%{svg | children: [%{type: :processing_instruction}]}) =~
             "<?unsupported?>"

    assert {:ok, _} =
             NativeElixirPdfUtilities.Validators.SvgValidator.validate_processing(
               "<svg xmlns='http://www.w3.org/2000/svg' width='20' height='20' opacity='inherit'/>",
               %{mode: :inline}
             )

    assert {:error, {_reason, diagnostic}} =
             HtmlToPdf.render("<div style='width:1000000px'><svg style='width:1000000%'/></div>")

    assert diagnostic.message != ""
  end

  test "prepared contract rejects malformed maps and budgets through shared diagnostics" do
    assert {:error, {:invalid_document, _}} = SvgSceneValidator.validate_prepared(:bad)
    assert {:ok, dom} = HtmlParser.parse_detailed("<svg width=20 height=20 />")
    assert {:ok, %{children: [svg]}} = Style.compute_detailed(dom)
    scene = svg.style.svg_scene
    assert :ok = SvgSceneValidator.validate_prepared(%{scene | budget: nil})

    for budget <- [make_ref(), :atomics.new(2, signed: false)] do
      assert {:error, {:invalid_document, %{message: message}}} =
               SvgSceneValidator.validate_prepared(%{scene | budget: budget})

      assert message =~ "issued image budget"
    end

    HtmlValidator.with_render_budget(fn ->
      assert {:ok, _} =
               SvgScene.paint(%{scene | style: Map.put(scene.style, :display, :none)}, 20.0)
    end)
  end
end
