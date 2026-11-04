alias NativeElixirPdfUtilities.TestSupport.Paths

Code.require_file(Paths.support("svg_public_workflow.ex"))
Code.require_file(Paths.support("png_fixture.ex"))

defmodule NativeElixirPdfUtilities.HtmlToPdf.SvgSceneTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.{HtmlToPdf, Limits, Text}
  alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, Style, SvgScene}

  setup do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    %{limits: limits}
  end

  test "public block SVG scenes keep their page count, text, and link annotations" do
    fixture =
      Paths.fixture("html_to_pdf/browser_parity/inline_svg_atomic_pages.html")

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

  test("converter image objects obey the shared PDF object budget", %{limits: limits}) do
    Limits.install(Map.put(limits, :max_pdf_objects, 2))

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             HtmlToPdf.render("<svg width=20 height=20><rect width=20 height=20 /></svg>")

    assert diagnostic.stage == :writer
    assert diagnostic.reason == :resource_limit_exceeded
    assert diagnostic.message =~ "max_pdf_objects"
  end

  test "computed scene retains affine artwork and separates transparent ink from layout" do
    html =
      "<svg width=80 height=40 color=blue><title>Metadata</title><desc>Validated</desc>\n<style>rect{fill:currentColor}</style><g transform='translate(10 5) rotate(20) skewX(10) scale(-1 1)'>\n<rect x=-20 width=20 height=10 /></g></svg>\n"

    assert {:ok, dom} = HtmlParser.parse_detailed(html)
    assert {:ok, %{children: [svg]}} = Style.compute_detailed(dom)
    scene = svg.style.svg_scene
    assert scene.source =~ "translate(10 5) rotate(20) skewX(10) scale(-1 1)"
    assert scene.source =~ "rgb(0,0,255)"
    assert {:ok, painted} = SvgScene.paint(scene, 200.0)
    assert painted.layout_extent == {+0.0, +0.0, 60.0, 30.0}
    assert {x, y, w, h} = painted.ink_extent
    assert x >= 0 and y >= 0 and w < 60 and h < 30
    assert painted.baseline == 30.0
  end

  test "zero and hidden scenes validate their contents, preserve flow and paint no artwork" do
    (
      assert {:error, {:unsupported_html, diagnostic}} =
               HtmlToPdf.render(
                 "<svg width=20 height=20><foreignObject><div>HTML</div></foreignObject></svg>"
               )

      assert diagnostic.reason == :unsupported_html
      assert diagnostic.stage == :layout
      assert diagnostic.module == NativeElixirPdfUtilities.Validators.SvgConverterValidator
      assert diagnostic.operation == :convert
      assert diagnostic.source == "foreignObject"
      assert diagnostic.message =~ "foreignObject"
    )

    for attrs <- [
          "width=0 height=20",
          "width=20 height=0",
          "width=20 height=20 viewBox='0 0 0 20'",
          "width=20 height=20 visibility=hidden",
          "width=20 height=20 style='display:none'"
        ] do
      html = "<svg #{attrs}><rect width=20 height=20 /></svg><p>Survives</p>"
      assert {:ok, pdf} = HtmlToPdf.render(html)
      assert {:ok, document} = NativeElixirPdfUtilities.Pdf.Reader.read(pdf)

      for %{value: value} <- Map.values(document.objects),
          value["Subtype"] == {:name, "Image"} and value["ColorSpace"] == {:name, "DeviceRGB"} do
        assert {:ref, mask_ref} = value["SMask"]
        alpha = :zlib.uncompress(document.objects[mask_ref].stream)
        assert Enum.all?(:binary.bin_to_list(alpha), &(&1 == 0))
      end

      refute pdf =~ "/Subtype /Form"
      assert {:ok, text} = Text.extract(pdf)
      assert text =~ "Survives"
    end

    assert {:error, {:invalid_document, diagnostic}} =
             HtmlToPdf.render(
               "<svg width=0 height=0><text><textPath>Cannot omit</textPath></text></svg>"
             )

    assert diagnostic.stage == :layout and diagnostic.operation == :prepare
    assert diagnostic.source == "textPath href"
    assert diagnostic.message =~ "same-scene #id"
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

  alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, Style, Layout, SvgScene}
  alias NativeElixirPdfUtilities.Validators.{SvgSceneValidator}
  alias NativeElixirPdfUtilities.{Limits, Text, Forms}

  test "public inline SVG paints shapes and retains surrounding Unicode and parent links" do
    html =
      "<div style='font-family:DejaVu Sans;font-size:10pt;line-height:14pt'>Before Ω\n<a href='https://example.test/svg'><svg width=40 height=20 viewBox='0 0 40 20'>\n<rect width=40 height=20 fill=red /></svg></a>After café</div>\n"

    assert {:ok, pdf} =
             NativeElixirPdfUtilities.HtmlToPdf.render(html, page_size: {200, 100}, margin: 0)

    assert {:ok, text} = Text.extract(pdf)
    assert text =~ "Before Ω" and text =~ "After café"
    assert pdf =~ "https://example.test/svg"
    assert {:ok, []} = Forms.fields(pdf)
    assert {:ok, dom} = HtmlParser.parse_detailed(html)
    assert {:ok, styled} = Style.compute_detailed(dom)
    assert {:ok, layout} = Layout.layout(styled, page_size: {200, 100}, margin: 0)
    image = Enum.find(layout.boxes, &(&1.type == :image))
    after_text = Enum.find(layout.boxes, &(Map.get(&1, :text) == "After café"))
    assert_in_delta image.y, after_text.y, 0.001
    assert {image.width, image.height} == {30.0, 15.0}
    assert image.link_rect == %{x: image.x, y: image.y, width: 30.0, height: 15.0}
  end

  test "missing logical content and effects diagnose before successful artwork" do
    for content <- [
          "<text><textPath>Ω</textPath></text>",
          "<tspan>text</tspan>",
          "<image href='https://example.test/private.png' />"
        ] do
      assert {:error, {_reason, diagnostic}} =
               NativeElixirPdfUtilities.HtmlToPdf.render(
                 "<svg width=20 height=20>#{content}</svg>"
               )

      assert diagnostic.stage in [:html, :style, :layout, :asset]
      assert is_binary(diagnostic.message) and diagnostic.message != ""
    end

    for definition <- [
          "<defs><linearGradient id='g'/></defs>",
          "<clipPath id='clip'><rect width=10 height=10 /></clipPath>"
        ] do
      assert {:ok, pdf} =
               NativeElixirPdfUtilities.HtmlToPdf.render(
                 "<svg width=20 height=20>#{definition}</svg>"
               )

      assert {:ok, _document} = NativeElixirPdfUtilities.Pdf.Reader.read(pdf)
      assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf, false)
    end

    assert {:ok, linked_pdf} =
             NativeElixirPdfUtilities.HtmlToPdf.render(
               "<svg width=20 height=20><a href='https://example.test/'><rect width=10 height=10 /></a></svg>"
             )

    refute linked_pdf =~ "/Subtype /Link"

    assert :ok =
             NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(
               linked_pdf,
               false
             )

    assert {:ok, document} = NativeElixirPdfUtilities.Pdf.Reader.read(linked_pdf)
    [_page] = document.pages

    assert {:error, {:invalid_document, %{source: "rect mask"}}} =
             NativeElixirPdfUtilities.HtmlToPdf.render(
               "<svg width=20 height=20><rect width=10 height=10 mask='url(#m)' /></svg>"
             )

    assert {:error, {:invalid_document, %{source: "rect filter"}}} =
             NativeElixirPdfUtilities.HtmlToPdf.render(
               "<svg width=20 height=20><rect width=10 height=10 filter='url(#f)' /></svg>"
             )

    for attrs <- ["vector-effect='non-scaling-size'"] do
      assert {:error, {:invalid_css, %{source: "vector-effect", message: message}}} =
               NativeElixirPdfUtilities.HtmlToPdf.render(
                 "<svg width=20 height=20><rect width=10 height=10 #{attrs} /></svg>"
               )

      assert message =~ "invalid SVG property value"
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
      if declaration == "display:flex" do
        assert {:error, {:unsupported_html, diagnostic}} =
                 NativeElixirPdfUtilities.HtmlToPdf.render(
                   "<svg width=20 height=20 style='#{declaration}'><rect width=20 height=20 /></svg>"
                 )

        assert diagnostic.module == SvgSceneValidator
        assert diagnostic.stage == :layout and diagnostic.source == "svg display"
        assert diagnostic.message =~ "replaced inline or block"
      else
        assert {:ok, pdf} =
                 NativeElixirPdfUtilities.HtmlToPdf.render(
                   "<svg width=20 height=20 style='#{declaration}'><rect width=20 height=20 /></svg>"
                 )

        assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf)
        assert {:ok, _document} = NativeElixirPdfUtilities.Pdf.Reader.read(pdf)
      end
    end

    for parent <- ["flex", "grid"] do
      assert {:ok, pdf} =
               NativeElixirPdfUtilities.HtmlToPdf.render(
                 "<div style='display:#{parent}'><svg width=20 height=20 /></div>"
               )

      assert {:ok, document} = NativeElixirPdfUtilities.Pdf.Reader.read(pdf)
      assert length(document.pages) == 1
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
               NativeElixirPdfUtilities.HtmlToPdf.render(
                 "<svg width=20 height=20>#{content}</svg>"
               )

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
               NativeElixirPdfUtilities.HtmlToPdf.render(
                 "<svg width=40 height=40><path d='#{d}' stroke=red fill=none /></svg>"
               )
    end
  end

  test "sibling opacity groups retain every form and graphics state" do
    source =
      "<svg width='40' height='20'><g opacity='.5'><rect width='20' height='20' fill='red'/></g><g opacity='.25'><rect x='20' width='20' height='20' fill='blue'/></g></svg>"

    assert {:ok, pdf} =
             NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.pdf(
               source,
               [width: 40, height: 20],
               %{mode: :inline}
             )

    refute pdf =~ "/Subtype /Link"
    assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf, false)
    assert {:ok, document} = NativeElixirPdfUtilities.Pdf.Reader.read(pdf)
    assert length(document.pages) == 1
  end

  test "geometry fixture renders all authored styles through the public API" do
    html =
      File.read!(Paths.fixture("html_to_pdf/browser_parity/inline_svg_geometry.html"))

    assert {:ok, pdf} = NativeElixirPdfUtilities.HtmlToPdf.render(html)
    refute pdf =~ "/Subtype /Link"
    assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf, false)
    assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf)

    (
      assert {:ok, text} = NativeElixirPdfUtilities.Text.extract(pdf, layout: false)
      assert text =~ "Inline baseline Ω"
      assert text =~ "End of geometry"
    )

    html =
      "<svg width=80 height=40><g fill=transparent><rect width=10 height=10 /></g><rect x='2pt' y='10%' width='1em' height='1rem' fill='rgba(0,0,255,.5)' stroke-dashoffset='-1' stroke-miterlimit='2' stroke-dasharray='none' paint-order='stroke fill'/></svg>"

    assert {:ok, png_pdf} = NativeElixirPdfUtilities.HtmlToPdf.render(html)
    refute png_pdf =~ "/Subtype /Link"

    assert :ok =
             NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(
               png_pdf,
               false
             )

    for value <- ["unset", "initial"] do
      assert {:ok, _} =
               NativeElixirPdfUtilities.HtmlToPdf.render(
                 "<svg color='#{value}' width=10 height=10><rect width=10 height=10 /></svg>"
               )
    end
  end

  test "malformed prepared sources and invalid form page structure diagnose" do
    assert {:ok, dom} =
             HtmlParser.parse_detailed("<svg width=20 height=20><foreignObject/></svg>")

    assert {:ok, []} = Style.load_stylesheets(dom, [])

    assert {:error, {:invalid_document, _}} =
             NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.png(
               "<svg width='0' height='20'/>",
               [width: 20, height: 20],
               %{mode: :inline}
             )

    assert {:error, {:invalid_document, _}} =
             NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.png(
               "<svg width='20' height='20'><path d='L1 2'/></svg>",
               [width: 20, height: 20],
               %{mode: :inline}
             )

    assert {:ok, __pdf} =
             NativeElixirPdfUtilities.HtmlToPdf.PdfWriter.render([
               %{size: {20, 20}, boxes: []},
               %{size: {20, 20}, boxes: []}
             ])

    for {property, value} <- [
          {"stroke-linecap", "bad"},
          {"stroke-linejoin", "bad"},
          {"shape-rendering", "bad"},
          {"paint-order", "fill fill"},
          {"stroke-miterlimit", "0"}
        ] do
      assert :error = NativeElixirPdfUtilities.Validators.SvgStyleValidator.parse(property, value)
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
             NativeElixirPdfUtilities.HtmlToPdf.render(
               "<div style='width:1000000px'><svg style='width:1000000%'/></div>"
             )

    assert diagnostic.message != ""
  end
end
