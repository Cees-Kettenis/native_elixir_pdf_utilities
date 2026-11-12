defmodule NativeElixirPdfUtilities.HtmlToPdf.SvgAdaptiveDensityTest do
  use ExUnit.Case, async: false

  alias NativeElixirPdfUtilities.{HtmlToPdf, Limits}
  alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, Style, SvgRasterizer}
  alias NativeElixirPdfUtilities.Pdf.Reader

  alias NativeElixirPdfUtilities.Validators.{
    HtmlValidator,
    SvgValidator,
    SvgDocumentViewportValidator
  }

  @source "<svg xmlns='http://www.w3.org/2000/svg' width='5000' height='1'><rect width='5000' height='1' fill='red'/></svg>"

  setup do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    %{limits: limits}
  end

  test "optional density retains scale one admission while explicit density remains strict" do
    attributes = %{"width" => "5000", "height" => "1"}

    assert {:ok, [width: 5000, height: 1], 1} =
             SvgValidator.prepare_dimensions(attributes, [scale: {:up_to, 2}], nil)

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             SvgRasterizer.rasterize(@source, [scale: 2], nil)

    assert diagnostic.message =~ "10000x2"
    assert diagnostic.source == "SVG"

    assert {:ok, _, _, 1} =
             SvgRasterizer.rasterize_image_with_scale(@source, [scale: {:up_to, 2}], nil, [],
               system_font_discovery: false
             )
  end

  test "ordinary images retain the requested density and pixel budgets can reduce it", %{
    limits: limits
  } do
    attributes = %{"width" => "40", "height" => "10"}

    assert {:ok, [width: 80, height: 20], 2} =
             SvgValidator.prepare_dimensions(attributes, [scale: {:up_to, 2}], nil)

    Limits.install(%{limits | max_raster_pixels: 1000})

    assert {:ok, [width: 40, height: 10], 1} =
             SvgValidator.prepare_dimensions(attributes, [scale: {:up_to, 2}], nil)

    assert SvgValidator.default_viewport_dimensions(40, 10) == [width: 40, height: 10]
    assert SvgValidator.default_viewport_dimensions(10.25, 5.25) == [width: 21, height: 11]
  end

  test "optional density also respects decoded bytes and PNG working buffers", %{limits: limits} do
    attributes = %{"width" => "40", "height" => "10"}
    Limits.install(%{limits | max_decoded_bytes: 2000})

    assert {:ok, [width: 40, height: 10], 1} =
             SvgValidator.prepare_dimensions(
               attributes,
               [scale: {:up_to, 2}],
               HtmlValidator.new_image_budget()
             )

    assert SvgValidator.default_viewport_dimensions(40, 10) == [width: 40, height: 10]

    Limits.install(%{limits | max_operation_memory_bytes: 30_000})

    assert {:ok, [width: 40, height: 10], 1} =
             SvgValidator.prepare_dimensions(
               attributes,
               [scale: {:up_to, 2}],
               HtmlValidator.new_image_budget()
             )

    assert SvgValidator.default_viewport_dimensions(40, 10) == [width: 40, height: 10]
  end

  test "adaptive density does not admit oversized base canvases or malformed scale options" do
    Limits.install(%{Limits.effective() | max_raster_pixels: 16_777_216})

    for attributes <- [
          %{"width" => "8193", "height" => "1"},
          %{"width" => "5000", "height" => "5000"}
        ] do
      assert {:error, {:resource_limit_exceeded, _}} =
               SvgValidator.prepare_dimensions(attributes, [scale: {:up_to, 2}], nil)
    end

    for scale <- [{:up_to, 0}, {:up_to, 1.5}, {:up_to, nil}, {:unexpected, 2}] do
      assert {:error, {:invalid_document, %{stage: :style, source: "SVG"}}} =
               SvgValidator.prepare_dimensions(
                 %{"width" => "5", "height" => "1"},
                 [scale: scale],
                 nil
               )
    end
  end

  test "cached images retain natural dimensions and charge their actual pixel count", %{
    limits: limits
  } do
    uri = "data:image/svg+xml;base64," <> Base.encode64(@source)
    html = "<img src='#{uri}'><img src='#{uri}'>"
    assert {:ok, dom} = HtmlParser.parse_detailed(html)

    Limits.install(%{limits | max_raster_pixels: 15_000})
    assert {:ok, styled} = Style.compute_detailed(dom, system_font_discovery: false)
    assert length(styled.children) == 2

    for node <- styled.children do
      assert node.style.image.width == 3750.0
      assert node.style.image.height == 0.75
      assert node.style.image.width_px == 5000
      assert node.style.image.height_px == 1
    end

    Limits.install(%{limits | max_raster_pixels: 14_999})

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             Style.compute_detailed(dom, system_font_discovery: false)

    assert diagnostic.message =~ "max_raster_pixels"
  end

  test "public image background and inline SVG paths retain large thin canvases" do
    uri = "data:image/svg+xml;base64," <> Base.encode64(@source)

    for html <- [
          "<img src='#{uri}'>",
          "<div style='width:100pt;height:10pt;background-image:url(#{uri});background-repeat:no-repeat'></div>",
          @source
        ] do
      assert {:ok, pdf} = HtmlToPdf.render(html, system_font_discovery: false)
      assert {:ok, document} = Reader.read(pdf)

      images =
        for %{value: %{"Subtype" => {:name, "Image"}} = image} <- Map.values(document.objects),
            do: image

      assert Enum.any?(images, &(&1["Width"] == 5000 and &1["Height"] == 1))
    end
  end

  test "embedded SVG documents preserve the baseline viewport canvas" do
    assert {:ok, plan} =
             SvgDocumentViewportValidator.prepare(%{
               source_root: %{"width" => "5000", "height" => "1"},
               root_style: %{}
             })

    assert {:ok, %{width: 5000, height: 1}} =
             SvgDocumentViewportValidator.prepare_raster(
               plan,
               3750,
               0.75,
               HtmlValidator.new_image_budget()
             )
  end

  test "actual dense allocations still obey aggregate budgets", %{limits: limits} do
    Limits.install(%{limits | max_raster_pixels: 4999})

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             HtmlValidator.with_render_budget(fn ->
               SvgValidator.prepare_dimensions(
                 %{"width" => "5000", "height" => "1"},
                 [scale: {:up_to, 2}],
                 nil
               )
             end)

    assert diagnostic.message =~ "max_raster_pixels"
  end
end
