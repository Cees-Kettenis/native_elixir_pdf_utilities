Code.require_file("../support/png_fixture.ex", __DIR__)

defmodule NativeElixirPdfUtilities.HtmlToPdf.SvgConverterResourceAccountingTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.Limits
  alias NativeElixirPdfUtilities.HtmlToPdf.{PngDecoder, SvgRasterizer}
  alias NativeElixirPdfUtilities.Validators.SvgValidator
  alias NativeElixirPdfUtilities.TestSupport.{JpegFixture, PngFixture}

  setup do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    png = PngFixture.build(2, 1, 2, 8, 0, fn _, _ -> [255, 0, 0] end)

    %{
      limits: limits,
      context: %{
        mode: :reference,
        resources: %{"svg-resource:logo" => %{data: png, media_type: "image/png"}}
      }
    }
  end

  test "undefined XML entities remain actionable input failures before conversion" do
    source = "<svg width='1' height='1'><g>&undefined;</g></svg>"

    for result <- [
          SvgValidator.validate_processing(source, %{mode: :image}),
          SvgRasterizer.rasterize(source, [], nil)
        ] do
      assert {:error, {:invalid_document, diagnostic}} = result
      assert diagnostic.reason == :invalid_document
      assert diagnostic.source == "SVG"
      assert diagnostic.line == 1
      assert String.downcase(diagnostic.message) =~ "entity"
    end
  end

  test "viewport re-encoding remains inside the cumulative source budget", %{limits: limits} do
    source = "<svg width='10' height='20'><rect width='10' height='20'/></svg>"
    Limits.install(%{limits | max_aggregate_svg_source_bytes: byte_size(source) * 2})

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             SvgRasterizer.rasterize(source, [width: 20, height: 10], nil)

    assert diagnostic.stage == :style
    assert diagnostic.source == "SVG"
    assert diagnostic.reason == :resource_limit_exceeded
    assert diagnostic.message =~ "max_aggregate_svg_source_bytes"
  end

  test "arc rotation requires a separator before its first flag" do
    source = "<svg width='10' height='10'><path d='M0 0A1 1 0-1 1 1 1'/></svg>"

    assert {:error, {:invalid_document, diagnostic}} = SvgRasterizer.rasterize(source, [], nil)
    assert diagnostic.stage == :layout
    assert diagnostic.source == "path d"
    assert diagnostic.reason == :invalid_document
    assert diagnostic.message =~ "arc flags"
  end

  test "XML built-in and numeric references remain valid while DTDs are rejected" do
    source =
      "<svg width='1' height='1'><desc>&amp;&#65;</desc><![CDATA[&undefined;]]><rect width='1' height='1'/></svg>"

    assert {:ok, _} = SvgValidator.validate_processing(source, %{mode: :image})

    for declaration <- [
          "<!DOCTYPE svg>",
          "<!DOCTYPE svg SYSTEM 'file:///unapproved.dtd'>",
          "<!DOCTYPE svg SYSTEM 'https://example.invalid/unapproved.dtd'>",
          "<!DOCTYPE svg PUBLIC '-//Original//DTD SVG//EN' 'https://example.invalid/unapproved.dtd'>",
          "<!DOCTYPE svg [<!ENTITY secret SYSTEM 'https://example.invalid/unapproved'>]>"
        ] do
      assert {:error, {:invalid_document, diagnostic}} =
               SvgValidator.validate_processing(declaration <> "<svg/>", %{mode: :image})

      assert diagnostic.source == "SVG" and diagnostic.line == 1
      assert diagnostic.message =~ "DTD"
    end
  end

  test "standalone referenced PNG bytes obey the existing decoded-image ceiling", %{
    limits: limits,
    context: context
  } do
    Limits.install(Map.put(limits, :max_decoded_image_bytes, 1))

    source =
      "<svg width='1' height='1'><image href='svg-resource:logo' width='1' height='1'/></svg>"

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             SvgRasterizer.rasterize(source, [], nil, [], context)

    assert diagnostic.reason == :resource_limit_exceeded
    assert diagnostic.stage == :limits
    assert diagnostic.message =~ "decoded image"
  end

  test "reused PNG image occurrences consume the shared original pixel and byte ceilings", %{
    limits: limits,
    context: context
  } do
    source =
      "<svg width='1' height='1'><defs><image id='asset' href='svg-resource:logo' width='1' height='1'/></defs><use href='#asset'/><use href='#asset'/></svg>"

    for {limit, ceiling, message} <- [
          {:max_aggregate_svg_raster_pixels, 5, "max_aggregate_svg_raster_pixels"},
          {:max_aggregate_decoded_image_bytes, 20, "decoded image bytes"}
        ] do
      Limits.install(Map.put(limits, limit, ceiling))

      assert {:error, {:resource_limit_exceeded, diagnostic}} =
               SvgRasterizer.rasterize(source, [], nil, [], context)

      assert diagnostic.reason == :resource_limit_exceeded
      assert diagnostic.message =~ message
    end
  end

  test "JPEG references obey the same decoded and occurrence pixel ceilings", %{limits: limits} do
    context = %{
      mode: :reference,
      resources: %{
        "svg-resource:logo" => %{data: JpegFixture.baseline(2, 1), media_type: "image/jpeg"}
      }
    }

    source =
      "<svg width='1' height='1'><defs><image id='asset' href='svg-resource:logo' width='1' height='1'/></defs><use href='#asset'/><use href='#asset'/></svg>"

    for {limit, ceiling, message} <- [
          {:max_decoded_image_bytes, 1, "decoded image"},
          {:max_aggregate_svg_raster_pixels, 5, "max_aggregate_svg_raster_pixels"},
          {:max_aggregate_decoded_image_bytes, 20, "decoded image bytes"}
        ] do
      Limits.install(Map.put(limits, limit, ceiling))

      assert {:error, {:resource_limit_exceeded, diagnostic}} =
               SvgRasterizer.rasterize(source, [], nil, [], context)

      assert diagnostic.stage in [:limits, :style]
      assert diagnostic.reason == :resource_limit_exceeded
      assert diagnostic.message =~ message
    end

    Limits.install(limits)
    assert {:ok, png} = SvgRasterizer.rasterize(source, [], nil, [], context)
    assert {:ok, image} = PngDecoder.decode(png, nil, false)
    assert image.width_px == 1 and image.height_px == 1
  end
end
