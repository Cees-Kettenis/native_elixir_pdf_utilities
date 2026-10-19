defmodule NativeElixirPdfUtilities.HtmlToPdf.SvgImageLinksTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.HtmlToPdf.{SvgRasterizer, SvgScene, PngDecoder, PdfWriter}
  alias NativeElixirPdfUtilities.Validators.{HtmlValidator, SvgConverterValidator}

  test "SVG navigation links retain their source and paint pixels in image and inline modes" do
    for mode <- [:image, :inline],
        attribute <- ["href", "xlink:href"],
        destination <- ["https://example.test/original?x=1&amp;y=2", "#unpainted-destination"] do
      source =
        "<svg xmlns='http://www.w3.org/2000/svg' xmlns:xlink='http://www.w3.org/1999/xlink' width='8' height='4'><a #{attribute}='#{destination}'><rect width='8' height='4' fill='red'/></a></svg>"

      assert {:ok, request} = SvgConverterValidator.prepare(source, [], nil, [], %{mode: mode})
      assert request.source =~ "#{attribute}=\"#{destination}\""
      assert {:ok, png} = SvgRasterizer.rasterize(source, [], nil, [], %{mode: mode})
      assert {:ok, image} = PngDecoder.decode(png, nil, false)
      assert image.data == :binary.copy(<<255, 0, 0>>, 32)
      refute Map.has_key?(image, :logical_text)
    end
  end

  test "navigation href is neither resolved nor published as a PDF annotation" do
    source =
      "<svg width='8' height='4'><a href='https://example.test/original'><rect width='8' height='4' fill='blue'/></a></svg>"

    caller = self()

    resolver = fn reference ->
      send(caller, {:asset_resolution, reference})
      {:error, :not_found}
    end

    assert {:ok, scene} =
             SvgScene.extract(source, HtmlValidator.new_image_budget(),
               system_font_discovery: false,
               asset_resolver: resolver
             )

    assert scene.source =~ "href=\"https://example.test/original\""
    assert {:ok, %{image: image, links: []}} = SvgScene.paint(scene, 6)
    assert image.data == :binary.copy(<<0, 0, 255>>, image.width_px * image.height_px)
    refute_receive {:asset_resolution, _}

    box = %{type: :image, x: 0, y: 0, width: 6, height: 3, image: image}
    assert {:ok, pdf} = PdfWriter.render([%{size: {20, 20}, boxes: [box]}])
    refute pdf =~ "/Subtype /Link"
    refute pdf =~ "/Annots"
    refute pdf =~ "/ActualText"
    assert pdf =~ "/Subtype /Image"
  end

  test "an authorized resource descriptor cannot replace a navigation destination" do
    source =
      "<svg width='8' height='4'><a href='svg-resource:destination'><rect width='8' height='4' fill='red'/></a></svg>"

    context = %{
      mode: :inline,
      resources: %{"svg-resource:destination" => %{data: "unused", media_type: "image/png"}}
    }

    assert {:ok, request} = SvgConverterValidator.prepare(source, [], nil, [], context)
    assert request.source =~ "href=\"svg-resource:destination\""
    refute request.source =~ "data:image"
    assert {:ok, png} = SvgRasterizer.rasterize(source, [], nil, [], context)
    assert {:ok, image} = PngDecoder.decode(png, nil, false)
    assert image.data == :binary.copy(<<255, 0, 0>>, 32)
  end

  test "active navigation URLs and executable source retain actionable diagnostics" do
    for attribute <- ["href", "xlink:href"],
        destination <- ["javascript:alert(1)", "vbscript:msgbox(1)", "j&#xA;avascript:alert(1)"] do
      source =
        "<svg xmlns:xlink='http://www.w3.org/1999/xlink' width='8' height='4'><a #{attribute}='#{destination}'><rect width='8' height='4'/></a></svg>"

      for mode <- [:image, :inline] do
        assert {:error, {:unsupported_html, diagnostic}} =
                 SvgRasterizer.rasterize(source, [], nil, [], %{mode: mode})

        assert diagnostic.message =~ "Active URL schemes"
        assert diagnostic.source == "href"
        assert diagnostic.module == HtmlValidator
        assert diagnostic.line == 1
      end
    end

    source = "<svg><a href='https://example.test'><script>alert(1)</script></a></svg>"
    assert {:error, {:unsupported_html, diagnostic}} = SvgRasterizer.rasterize(source, [], nil)
    assert diagnostic.message =~ "script"
  end
end
