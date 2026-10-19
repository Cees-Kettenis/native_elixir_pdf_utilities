alias NativeElixirPdfUtilities.TestSupport.Paths

defmodule NativeElixirPdfUtilities.HtmlToPdf.SpecialControlGeometryTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.{HtmlToPdf, Text}
  alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, Layout, PageGeometry, PdfWriter, Style}

  @image Paths.fixture("html_to_pdf/browser_parity/assets/iframe-bars.png")
  @fonts [
    %{
      family: "DejaVu Sans Mono",
      path: Path.expand("priv/fonts/dejavu/DejaVuSansMono.ttf"),
      weight: 400,
      style: :normal
    }
  ]

  test "color percentage and calculated sizes resolve in the containing box and auto retains native size" do
    for display <- ["inline-block", "block"],
        {css, parent, expected} <- [
          {"width:50%;height:calc(50% - 2pt)", "width:200pt;height:80pt", {100, 38}},
          {"width:auto;height:auto", "width:200pt;height:80pt", {9, 10.5}},
          {"width:50%;height:50%", "width:200pt", {100, 10.5}},
          {"width:3pt;height:3pt", "width:200pt", {4.5, 3}}
        ] do
      html =
        "<div style='#{parent}'><input type=color value=red style='display:#{display};#{css}'></div>"

      assert {:ok, dom} = HtmlParser.parse_detailed(html)
      assert {:ok, styled} = Style.compute_detailed(dom, forms: :static)
      assert {:ok, layout} = Layout.layout(styled)

      host =
        Enum.find(layout.boxes, &(get_in(&1, [:form_control, :attributes, "type"]) == "color"))

      assert {host.width, host.height} == expected
      swatches = Enum.filter(layout.boxes, &(Map.get(&1, :role) == :color_swatch))
      if expected == {4.5, 3}, do: assert(swatches == []), else: assert(length(swatches) == 1)
      assert {:ok, _} = HtmlToPdf.render(html, forms: :static)
    end
  end

  test "unequal rounded image edges retain distinct horizontal and vertical content radii" do
    html =
      "<input type=image src=tile style='width:60pt;height:30pt;border-radius:12pt;border-width:1pt 2pt 3pt 4pt;border-style:solid;padding:2pt 3pt 4pt 5pt'>"

    opts = [forms: :static, assets: %{"tile" => {:bytes, File.read!(@image)}}]
    assert {:ok, dom} = HtmlParser.parse_detailed(html)
    assert {:ok, styled} = Style.compute_detailed(dom, opts)
    assert {:ok, layout} = Layout.layout(styled)
    image = Enum.find(layout.boxes, &(&1.type == :image))
    assert image.clip.width == 60 and image.clip.height == 30

    assert image.clip.border_radii == %{
             bottom_left: {3.2578125, 5.00390625},
             bottom_right: {7.5, 5.00390625},
             top_left: {3.2578125, 9.2578125},
             top_right: {7.5, 9.2578125}
           }

    assert {:ok, _} = HtmlToPdf.render(html, opts)
  end

  test "rounded temporal borders preserve scalar and shared curved paint across border styles and small boxes" do
    for style <- ["solid", "dashed", "dotted", "double", "groove", "ridge", "inset", "outset"],
        extra <- ["", "disabled", "style='width:10pt;height:6pt'"] do
      size = if String.starts_with?(extra, "style"), do: "width:10pt;height:6pt;", else: ""

      html =
        "<input type=date readonly #{if(extra == "disabled", do: extra)} value=2026-10-13 style='#{size}font-family:DejaVu Sans Mono;border-radius:12pt;border-width:1pt 2pt 3pt 4pt;border-style:#{style}'>"

      assert {:ok, pdf} =
               HtmlToPdf.render(html, forms: :static, fonts: @fonts, system_font_discovery: false)

      assert {:ok, text} = Text.extract(pdf, layout: false)
      assert text =~ "2026-10-13"
    end
  end

  test "inset clip collapses safely and writer rejects malformed elliptical corner metadata" do
    assert %{width: +0.0, height: +0.0, border_radius: +0.0} =
             PageGeometry.inset_rounded_clip(
               %{x: 0, y: 0, width: 3, height: 3, border_radius: 9},
               %{left: 4, right: 4, top: 4, bottom: 4}
             )

    clip =
      PageGeometry.inset_rounded_clip(%{x: 0, y: 0, width: 30, height: 20, border_radius: 12}, %{
        left: 1,
        right: 2,
        top: 3,
        bottom: 4
      })

    box = %{
      type: :rect,
      x: 0,
      y: 0,
      width: 30,
      height: 20,
      border_radius: 0,
      fill_color: {1, 0, 0},
      stroke_width: 0,
      stroke_color: nil,
      clip: clip
    }

    assert {:ok, _} = PdfWriter.render([%{size: {100, 100}, boxes: [box]}])

    for radii <- [
          false,
          %{},
          %{clip.border_radii | top_left: {-1, 2}},
          %{clip.border_radii | top_left: [1, 2]},
          %{clip.border_radii | top_left: {:bad, 2}}
        ] do
      assert {:error, {reason, diagnostic}} =
               PdfWriter.render([
                 %{size: {100, 100}, boxes: [%{box | clip: %{clip | border_radii: radii}}]}
               ])

      assert diagnostic.reason == reason and diagnostic.stage == :pdf
      assert diagnostic.message =~ "clip"
    end
  end

  test "elliptical clipping follows translated groups and the same transformed link boundary" do
    alias NativeElixirPdfUtilities.HtmlToPdf.DrawingGeometry
    alias NativeElixirPdfUtilities.Validators.WriterValidator

    clip =
      PageGeometry.inset_rounded_clip(%{x: 0, y: 0, width: 30, height: 20, border_radius: 8}, %{
        left: 1,
        right: 2,
        top: 3,
        bottom: 4
      })

    box = %{
      type: :text,
      text: "Link",
      x: 0,
      y: 5,
      width: 30,
      height: 12,
      line_height: 12,
      font_size: 10,
      font: "Helvetica",
      color: {0, 0, 0},
      link_url: "https://example.test",
      clip: clip,
      link_rect: %{x: 0, y: 0, width: 30, height: 20}
    }

    rectangle = %{x: 0, y: 0, width: 30, height: 20}
    [clipped] = DrawingGeometry.clip_boxes([box], rectangle)
    assert clipped.clip.inner_clip == clip
    [clipped_twice] = DrawingGeometry.clip_boxes([clipped], rectangle)
    assert clipped_twice.clip.inner_clip == clip
    [rounded_group] = DrawingGeometry.clip_boxes([clipped], clip)
    assert rounded_group.type == :drawing_group
    shifted = DrawingGeometry.translate_box(rounded_group, 7, 9)
    assert shifted.clip.x == clip.x + 7 and shifted.clip.y == clip.y + 9
    assert hd(shifted.boxes).clip.inner_clip.x == clip.x + 7
    inner_group = %{rounded_group | clip: clipped.clip}
    local = DrawingGeometry.local_clip(inner_group, 200)
    assert local.inner_clip.x - local.x == clip.x - clipped.clip.x
    assert local.inner_clip.y - local.y == clip.y - clipped.clip.y

    group = %{
      type: :drawing_group,
      width: 60,
      height: 40,
      transform: {2, 0, 0, 3, 4, 5},
      boxes: [box],
      clip: clip
    }

    assert {:ok, prepared} = WriterValidator.prepare([%{size: {200, 200}, boxes: [group]}], [])
    painted = Enum.find(hd(prepared.pages).boxes, &(&1.type == :text))
    {left, bottom, right, top} = painted.drawing_annotation_rect
    assert right > left and top > bottom
    assert left >= 6 and right <= 60 and bottom >= 17 and top <= 56

    assert {:ok, direct} =
             WriterValidator.prepare([%{size: {200, 200}, boxes: [%{group | clip: nil}]}], [])

    assert Enum.find(hd(direct.pages).boxes, &(&1.type == :text)).drawing_annotation_rect

    assert {:ok, pdf} =
             PdfWriter.render([%{size: {200, 200}, boxes: [group]}], compress_streams: false)

    assert pdf =~ "/Annots" and pdf =~ "W n"
  end

  test "image device clipping retains both content boundaries and mirrored corner radii" do
    alias NativeElixirPdfUtilities.Validators.WriterValidator

    clip =
      PageGeometry.inset_rounded_clip(
        %{x: 1, y: 2, width: 30, height: 20, border_radius: 8},
        %{left: 1, right: 2, top: 3, bottom: 4}
      )

    rectangle = %{x: 0, y: 0, width: 40, height: 30, border_radius: 2}

    for content <- [Map.put(clip, :inner_clip, rectangle), Map.put(rectangle, :inner_clip, clip)] do
      box = %{
        type: :image,
        x: 0,
        y: 0,
        width: 40,
        height: 30,
        snap_to_css_pixel_grid: true,
        clip: content
      }

      paint = WriterValidator.prepare_image_paint(box, 100)
      assert length(paint.content_clips) == 2
      curved = Enum.find(paint.content_clips, &(&1.width < 150))
      {rx, ry} = clip.border_radii.top_left
      {dx, dy} = curved.border_radii.bottom_left
      assert_in_delta dx, rx / 0.75 * 3.125, 0.000001
      assert_in_delta dy, ry / 0.75 * 3.125, 0.000001
      assert_in_delta curved.y, (100 - clip.y - clip.height) / 0.75 * 3.125, 0.000001
    end
  end
end
