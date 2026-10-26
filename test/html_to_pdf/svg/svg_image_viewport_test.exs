alias NativeElixirPdfUtilities.TestSupport.Paths
Code.require_file(Paths.support("svg_converted_image.ex"))

defmodule NativeElixirPdfUtilities.HtmlToPdf.SvgImageViewportTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.HtmlToPdf.{PngDecoder, ResourceContext, SvgRasterizer}
  alias NativeElixirPdfUtilities.Validators.{HtmlValidator, SvgImageValidator}
  alias NativeElixirPdfUtilities.TestSupport.SvgConvertedImage
  alias NativeElixirPdfUtilities.Limits

  setup do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    :ok
  end

  @source "<svg xmlns='http://www.w3.org/2000/svg' width='20' height='10' viewBox='10 10 20 10'><rect x='10' y='10' width='10' height='10' fill='red'/></svg>"

  test "contained SVG images resolve their raster and root transform at the used image dimensions" do
    source =
      "<svg width='20' height='10' viewBox='0 0 20 10' transform='translate(2 1)'><rect width='20' height='10' fill='red'/></svg>"

    uri = "data:image/svg+xml;base64," <> Base.encode64(source)

    image =
      SvgConvertedImage.render(
        "<img src='#{uri}' style='width:30pt;height:30pt;object-fit:contain'>"
      )

    assert {image.width, image.height} == {80, 40}
    assert :binary.at(image.alpha, 2 * 80 + 3) == 0
    # awaiting upstream fixes for resvg
    # The published renderer doubles this root translation; keep both exact bounds.
    assert :binary.at(image.alpha, 2 * 80 + 4) == 255 or
             :binary.at(image.alpha, 4 * 80 + 8) == 255
  end

  test "percentage background dimensions rerender the authored viewBox at actual raster density" do
    source = "data:image/svg+xml;base64," <> Base.encode64(@source)

    image =
      SvgConvertedImage.render(
        "<div style='width:12pt;height:12pt;background-image:url(#{source});background-size:100% 100%;background-repeat:no-repeat'></div>"
      )

    assert {image.width, image.height} == {32, 32}
    assert :binary.at(image.alpha, 0) == 0
    assert :binary.at(image.alpha, 16 * 32 + 8) == 255
    assert :binary.at(image.alpha, 16 * 32 + 24) == 0
    assert :binary.at(image.alpha, 31 * 32) == 0
  end

  test "SVG image-map regions retain their CSS viewport after final image resolution" do
    uri = "data:image/svg+xml;base64," <> Base.encode64(@source)

    html =
      "<body style='margin:0'><img src='#{uri}' usemap='#art' style='display:block;width:30pt;height:30pt;object-fit:contain'><map name='art'><area shape='rect' coords='0,0,40,40' href='https://example.test/art'></map></body>"

    assert {:ok, pdf} =
             NativeElixirPdfUtilities.HtmlToPdf.render(html, page_size: {100, 100}, margin: 0)

    assert pdf =~ "/Subtype /Image" and pdf =~ "/Subtype /Link"
    assert pdf =~ "/Rect [0 77.5 30 92.5]"
    assert pdf =~ "https://example.test/art"
  end

  test "cached final viewports charge each occurrence against the actual raster ceiling" do
    Limits.install(%{Limits.effective() | max_aggregate_svg_raster_pixels: 2500})
    budget = HtmlValidator.new_image_budget()

    HtmlValidator.with_render_budget(fn ->
      ResourceContext.run([], budget, fn _ ->
        assert {:ok, png, descriptor} =
                 SvgRasterizer.rasterize_image(@source, [], budget, [],
                   system_font_discovery: false
                 )

        assert {:ok, image} = PngDecoder.decode(png, budget, false)
        image = Map.put(image, :svg_viewport, descriptor)
        assert {:ok, resized} = SvgRasterizer.resize_image(image, 12, 12)
        assert {:ok, ^resized} = SvgRasterizer.resize_image(image, 12, 12)

        assert {:error, {:resource_limit_exceeded, diagnostic}} =
                 SvgRasterizer.resize_image(image, 12, 12)

        assert diagnostic.source == "SVG" and
                 diagnostic.message =~ "max_aggregate_svg_raster_pixels"
      end)
    end)
  end

  test "a failed viewport conversion remains a recoverable scoped result on a repeated request" do
    Limits.install(%{Limits.effective() | max_svg_raster_pixels: 600})
    budget = HtmlValidator.new_image_budget()

    ResourceContext.run([], budget, fn _ ->
      assert {:ok, png, descriptor} =
               SvgRasterizer.rasterize_image(@source, [], budget, [],
                 system_font_discovery: false
               )

      assert {:ok, image} = PngDecoder.decode(png, budget, false)
      image = Map.put(image, :svg_viewport, descriptor)

      assert {:error, {:resource_limit_exceeded, diagnostic}} =
               SvgRasterizer.resize_image(image, 12, 12)

      assert {:error, {:resource_limit_exceeded, ^diagnostic}} =
               SvgRasterizer.resize_image(image, 12, 12)
    end)
  end

  test "viewport requests keep their approved source and font bytes in the issuing document" do
    budget = HtmlValidator.new_image_budget()

    ResourceContext.run([], budget, fn parent ->
      assert {:ok, png, descriptor} =
               SvgRasterizer.rasterize_image(@source, [], budget, [],
                 system_font_discovery: false
               )

      assert {:ok, image} = PngDecoder.decode(png, budget, false)
      image = Map.put(image, :svg_viewport, Map.put(descriptor, :source, "<svg/>"))
      assert {:ok, resized} = SvgRasterizer.resize_image(image, 12, 12)
      assert {resized.width_px, resized.height_px} == {32, 32}
      assert :binary.at(resized.alpha_data, 16 * 32 + 8) == 255
      assert ResourceContext.current() == parent

      for descriptor <- [nil, %{}, %{id: make_ref(), context: parent}] do
        assert {:error, {:invalid_document, diagnostic}} =
                 SvgImageValidator.viewport_request(descriptor, 12, 12)

        assert diagnostic.source == "SVG" and diagnostic.message =~ "issued source scope"
      end

      assert {:error, {:invalid_document, _}} =
               SvgImageValidator.viewport_request(descriptor, 0, 12)

      assert {:error, {_reason, diagnostic}} =
               SvgImageValidator.viewport_request(%{descriptor | context: %{}}, 12, 12)

      assert is_atom(diagnostic.stage) and is_binary(diagnostic.message)

      assert {:error, {:invalid_document, _}} =
               SvgRasterizer.resize_image(%{image | svg_viewport: %{}}, 12, 12)
    end)
  end

  test "ordinary decoded images have no deferred SVG viewport" do
    image = %{width: 12, height: 12}
    assert {:ok, ^image} = SvgRasterizer.resize_image(image, 24, 24)

    assert {:ok, _png, nil} =
             SvgRasterizer.rasterize_image(@source, [], nil, [], system_font_discovery: false)
  end
end
