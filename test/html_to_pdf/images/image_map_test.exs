alias NativeElixirPdfUtilities.TestSupport.Paths

Code.require_file(Paths.support("png_fixture.ex"))

defmodule NativeElixirPdfUtilities.HtmlToPdf.ImageMapTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.HtmlToPdf

  alias NativeElixirPdfUtilities.HtmlToPdf.{
    ResourceContext,
    HtmlParser,
    Style,
    Layout,
    DrawingGeometry,
    PdfWriter,
    ImageMapGeometry
  }

  alias NativeElixirPdfUtilities.Validators.{HtmlValidator, ImageMapValidator, WriterValidator}

  @image "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABAAAAAICAIAAAB/FOjAAAAAGElEQVR4nGM4oSGHFYkELMCKGEY10EIDAJ7zhIFJAvtOAAAAAElFTkSuQmCC"

  test "circle approximation keeps subnormal radii and the fixed sagitta precision bounded" do
    for radius <- [5.0e-324, 1.0e-320, 1.0e-200, 0.015625, 0.015626] do
      assert ImageMapValidator.circle_segments(radius) == 4
    end

    for radius <- [0.02, 0.05, 0.1, 1.0, 8.0, 100.0] do
      count = ImageMapValidator.circle_segments(radius)
      assert count >= 4
      assert radius * 0.75 * (1 - :math.cos(:math.pi() / count)) <= 0.75 / 64 + 1.0e-12

      if count > 4 do
        assert radius * 0.75 * (1 - :math.cos(:math.pi() / (count - 1))) > 0.75 / 64
      end
    end
  end

  test "tiny positive circle areas render without losing the following default region" do
    for radius <- ["5e-324", "1e-320", "1e-200", "0.015625", "0.015626"] do
      html =
        "<img src='#{@image}' usemap='#m'><map name=m><area shape=circle coords='5,5,#{radius}' href='https://example.com/tiny'><area shape=default href='https://example.com/default'></map>"

      assert {:ok, dom} = HtmlParser.parse_detailed(html)
      assert {:ok, styled} = Style.compute_detailed(dom)
      image = Enum.find(styled.children, &(&1.tag == "img"))
      assert {:circle, 5.0, 5.0, actual_radius} = hd(image.style._image_map.areas).shape
      assert actual_radius > 0

      assert {:ok, pdf} = HtmlToPdf.render(html, compress_streams: false)
      assert pdf =~ "/Subtype /Image"
      assert pdf =~ "https://example.com/default"
    end
  end

  test "circle plans retain work-limit diagnostics even beyond the old cancellation range" do
    alias NativeElixirPdfUtilities.Limits
    original = Limits.effective()
    on_exit(fn -> Limits.install(original) end)
    Limits.install(Map.put(original, :max_image_map_geometry_work, 16))
    assert ImageMapValidator.circle_segments(1.0e-320) == 4
    Limits.install(Map.put(original, :max_image_map_geometry_work, 15))

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             HtmlValidator.with_render_budget(fn ->
               ImageMapValidator.circle_segments(1.0e-320)
             end)

    assert diagnostic.stage == :layout
    assert diagnostic.reason == :resource_limit_exceeded
    assert diagnostic.message =~ "max_image_map_geometry_work"

    Limits.install(Map.put(original, :max_css_numeric_magnitude, 1_000_000_000_000_000_000))

    html =
      "<img src='#{@image}' usemap='#m'><map name=m><area shape=circle coords='5,5,1e18' href='https://example.com/large'></map>"

    assert {:error, {:resource_limit_exceeded, diagnostic}} = HtmlToPdf.render(html)
    assert diagnostic.stage == :layout
    assert diagnostic.reason == :resource_limit_exceeded
    assert diagnostic.message =~ "max_image_map_geometry_work"
  end

  test "map association, permissive shapes and first-area masks use authored nodes" do
    html =
      "<img src='#{@image}' usemap='#regions' width=80 height=40><map name='regions'><area coords='10,10,20,20'><area shape=circ coords='30;20;8;99' href='https://example.com/circle'><area shape=polygon coords='40,0,70,0,55,30,999' href='https://example.com/poly'><area coords='30,30,0,0,99' href='https://example.com/rect'><area shape=default href='https://example.com/default'></map><map name='regions'><area shape=default href='https://example.com/duplicate'></map>"

    assert {:ok, dom} = HtmlParser.parse_detailed(html)
    assert {:ok, styled} = Style.compute_detailed(dom)
    image = Enum.find(styled.children, &(&1.tag == "img"))
    model = image.style._image_map
    assert length(model.areas) == 5
    assert {:blocked, _} = ImageMapValidator.hit_test(model, 15, 15, 80, 40)

    assert {:ok, {:uri, "https://example.com/circle"}} =
             ImageMapValidator.hit_test(model, 30, 20, 80, 40)

    assert {:ok, {:uri, "https://example.com/poly"}} =
             ImageMapValidator.hit_test(model, 55, 15, 80, 40)

    assert {:ok, {:uri, "https://example.com/rect"}} =
             ImageMapValidator.hit_test(model, 5, 5, 80, 40)

    assert {:ok, {:uri, "https://example.com/default"}} =
             ImageMapValidator.hit_test(model, 75, 35, 80, 40)

    assert :none = ImageMapValidator.hit_test(model, 81, 0, 80, 40)

    assert {:error, {:invalid_layout, diagnostic}} =
             ImageMapValidator.hit_test(model, "bad", 0, 80, 40)

    assert diagnostic.source == "map" and diagnostic.stage == :layout

    assert HtmlValidator.parse_html_float_list("garbage+10;foo20, .bad, -2e1") == [
             10.0,
             20.0,
             0.0,
             -20.0
           ]
  end

  test "author-exposed popover images retain their intrinsic artwork and image-map links" do
    html =
      "<img popover src='#{@image}' usemap='#visible' style='display:block'>" <>
        "<map name=visible><area shape=default href='https://example.com/popover-image'></map>"

    assert {:ok, pdf} =
             HtmlToPdf.render(html, system_font_discovery: false, compress_streams: false)

    assert pdf =~ "/Subtype /Image"
    assert pdf =~ "https://example.com/popover-image"
    assert pdf =~ "/Subtype /Link"
  end

  test "PDF quadrilaterals remove deliberate holes and clip fit geometry" do
    html =
      "<body style='margin:0'><img src='#{@image}' usemap='#regions' style='display:block;width:60pt;height:30pt;object-fit:contain'><map name=regions><area coords='10,10,20,20'><area coords='0,0,40,40' href='https://example.com/rect'><area shape=default href='https://example.com/default'></map></body>"

    ResourceContext.run([], HtmlValidator.new_image_budget(), fn _ ->
      assert {:ok, dom} = HtmlParser.parse_detailed(html)
      assert {:ok, styled} = Style.compute_detailed(dom)
      assert {:ok, layout} = Layout.layout(styled, page_size: {100, 100}, margin: 0)
      assert {:ok, layout} = Layout.resolve_images(layout)
      regions = Enum.filter(layout.boxes, &(&1.type == :link_region))
      assert length(regions) == 2
      [rect, default] = regions
      assert rect.x == 0 and rect.y == 70 and rect.width == 30 and rect.height == 30
      assert default.x == 30 and default.width == 30
      refute Enum.any?(rect.link_quads, &quad_covers?(&1, 11.25, 88.75))
      assert Enum.any?(rect.link_quads, &quad_covers?(&1, 3.75, 96.25))
      assert :ok = WriterValidator.validate_native_boxes(layout.boxes)

      assert {:ok, pdf} =
               PdfWriter.render([%{number: 1, size: {100, 100}, boxes: layout.boxes}],
                 compress_streams: false
               )

      assert pdf =~ "/QuadPoints ["
      assert pdf =~ "/Rect [0 70 30 100]"
      assert pdf =~ "https://example.com/rect"
      pages = [%{number: 1, size: {100, 100}, boxes: layout.boxes}]
      assert ImageMapValidator.lookup(pages, 1, 11.25, 88.75) == :none

      assert ImageMapValidator.lookup(pages, 1, 3.75, 96.25) ==
               {:ok, {:uri, "https://example.com/rect"}}
    end)
  end

  test "concave and self-intersecting polygons retain the even-odd interior" do
    for points <- [
          [{0, 0}, {40, 0}, {40, 10}, {10, 10}, {10, 40}, {0, 40}],
          [{0, 0}, {40, 40}, {0, 40}, {40, 0}]
        ] do
      area = %{
        identity: make_ref(),
        shape: {:poly, points},
        destination: {:uri, "https://example.com/poly"}
      }

      model = %{identity: make_ref(), areas: [area]}
      viewport = %{x: 0, y: 0, width: 30, height: 30}
      [region] = ImageMapGeometry.regions(model, viewport, viewport)

      for x <- [5, 15, 25, 35], y <- [5, 15, 25, 35] do
        inside = ImageMapValidator.covers?(area.shape, x, y, 40, 40)
        covered = Enum.any?(region.link_quads, &quad_covers?(&1, x * 0.75, 30 - y * 0.75))
        assert covered == inside, inspect({points, x, y})
      end
    end
  end

  test "pagination translation and scaled clipped groups preserve region coordinates" do
    region = %{
      type: :link_region,
      x: 0.0,
      y: 0.0,
      width: 10.0,
      height: 10.0,
      link_url: "https://example.com/group",
      link_quads: [[{0, 0}, {10, 0}, {10, 10}, {0, 10}]]
    }

    shifted = DrawingGeometry.translate_box(region, 5, 7)
    assert shifted.link_quads == [[{5, 7}, {15, 7}, {15, 17}, {5, 17}]]

    group = %{
      type: :drawing_group,
      x: 0,
      y: 0,
      width: 20,
      height: 20,
      transform: {2, 0, 0, 2, 10, 20},
      clip: %{x: 0, y: 0, width: 5, height: 5},
      boxes: [region]
    }

    assert {:ok, pdf} =
             PdfWriter.render([%{number: 1, size: {100, 100}, boxes: [group]}],
               compress_streams: false
             )

    assert pdf =~ "/Rect [10 20 20 30]"
    pages = [%{number: 1, size: {100, 100}, boxes: [group]}]

    assert ImageMapValidator.lookup(pages, 1, 15, 25) ==
             {:ok, {:uri, "https://example.com/group"}}

    assert ImageMapValidator.lookup(pages, 1, 25, 25) == :none
    assert {:error, {:invalid_layout, diagnostic}} = ImageMapValidator.lookup(pages, 2, 15, 25)
    assert diagnostic.source == "map" and diagnostic.stage == :layout

    [_, points] = Regex.run(~r/\/QuadPoints \[([^\]]+)\]/, pdf)

    actual =
      points
      |> HtmlValidator.parse_html_float_list()
      |> Enum.chunk_every(2)
      |> Enum.map(fn [x, y] -> {x, y} end)
      |> Enum.sort()

    assert actual == [{10.0, 20.0}, {10.0, 30.0}, {20.0, 20.0}, {20.0, 30.0}]
  end

  test "area fragment links resolve to the paginated target" do
    html =
      "<img src='#{@image}' usemap='#m' width=64 height=32><map name=m><area shape=default href='#target'></map><p id=target>Target</p>"

    assert {:ok, pdf} = HtmlToPdf.render(html, compress_streams: false)
    assert pdf =~ "/Subtype /Link" and pdf =~ "/XYZ"
  end

  test "image-map resource ceilings and malformed PDF quadrilaterals use shared diagnostics" do
    alias NativeElixirPdfUtilities.Limits
    original = Limits.effective()
    on_exit(fn -> Limits.install(original) end)
    Limits.install(Map.put(original, :max_image_map_coordinates, 3))

    html =
      "<img src='#{@image}' usemap='#m'><map name=m><area coords='0,0,10,10' href='https://example.com'></map>"

    assert {:error, {:resource_limit_exceeded, diagnostic}} = HtmlToPdf.render(html)
    assert diagnostic.stage == :html and diagnostic.message =~ "max_image_map_coordinates"
    Limits.install(Map.put(original, :max_image_map_geometry_work, 1))

    html =
      "<img src='#{@image}' usemap='#m'><map name=m><area shape=circle coords='8,4,3' href='https://example.com'></map>"

    assert {:error, {:resource_limit_exceeded, diagnostic}} = HtmlToPdf.render(html)
    assert diagnostic.stage == :layout and diagnostic.message =~ "max_image_map_geometry_work"
    Limits.install(original)

    malformed = %{
      type: :link_region,
      x: 0,
      y: 0,
      width: 10,
      height: 10,
      link_url: "https://example.com",
      link_quads: [[{0, 0}, {10, 10}, {0, 10}, {10, 0}]]
    }

    assert {:error, {:invalid_pdf_input, diagnostic}} =
             PdfWriter.render([%{number: 1, size: {100, 100}, boxes: [malformed]}])

    assert diagnostic.stage == :pdf and diagnostic.message =~ "link_region"
  end

  test "lazy maps use only the selected candidate and keep source coordinates after final flex shrink" do
    png =
      NativeElixirPdfUtilities.TestSupport.PngFixture.build(64, 32, 2, 8, 0, fn _, _ ->
        [0, 128, 64]
      end)

    source = "data:image/png;base64," <> Base.encode64(png)

    html =
      "<body style='margin:0'><div style='display:flex;width:48pt;height:24pt'><img loading=lazy sizes=auto srcset='#{source} 64w, unused 128w' usemap='#m' style='width:96pt;height:24pt;min-width:0;flex-shrink:1'></div><map name=m><area coords='0,0,16,8' href='https://example.com/lazy'></map></body>"

    assert {:ok, pdf} =
             HtmlToPdf.render(html,
               asset_resolver: fn _ -> flunk("unselected candidate fetched") end,
               compress_streams: false,
               margin: 0,
               page_size: {100, 100}
             )

    assert pdf =~ "/QuadPoints [" and pdf =~ "https://example.com/lazy"
    assert pdf =~ "/Rect [0 94 12 100]"
  end

  @tag :browser_parity
  @tag :image_map_original
  test "image maps retain original Chromium image paint and source geometry" do
    fixture = Paths.fixture("html_to_pdf/browser_parity/html_image_maps.html")

    stats =
      NativeElixirPdfUtilities.TestSupport.PdfVisualCompare.assert_browser_match!(fixture,
        artifact_dir: "tmp/browser_parity/html_image_maps",
        max_changed_ratio: 0.01,
        max_average_delta: 0.01
      )

    assert stats.page_count == 1
  end

  defp quad_covers?(quad, x, y) do
    ImageMapValidator.covers?({:poly, quad}, x, y, 1000, 1000)
  end
end
