Code.require_file("../support/svg_public_workflow.ex", __DIR__)

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

  test "original SVG anchor links retain their annotation behavior" do
    assert {:ok, linked_pdf} =
             HtmlToPdf.render(
               "<svg width=20 height=20><a href='https://example.test/'><rect width=10 height=10 /></a></svg>"
             )

    assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(linked_pdf)
    refute linked_pdf =~ "/Subtype /Link"
    assert {:error, {:no_extractable_text, _}} = Text.extract(linked_pdf, layout: false)
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
    assert pdf =~ "/Subtype /Image"

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
end
