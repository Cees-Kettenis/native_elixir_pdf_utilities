defmodule NativeElixirPdfUtilities.HtmlToPdf.SvgConverterContractTest do
  use ExUnit.Case, async: false

  alias NativeElixirPdfUtilities.{HtmlToPdf, Limits}
  alias NativeElixirPdfUtilities.HtmlToPdf.{PngDecoder, SvgRasterizer}
  alias NativeElixirPdfUtilities.Validators.HtmlValidator

  @source "<svg xmlns='http://www.w3.org/2000/svg' width='4' height='2'><rect width='2' height='2' fill='#112233'/><rect x='2' width='2' height='2' fill='#aabbcc'/></svg>"

  setup do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    %{limits: limits}
  end

  test "the adapter converts real SVG to the existing converter's PNG bytes" do
    assert {:ok, png} = SvgRasterizer.rasterize(@source, [], nil)
    assert binary_part(png, 0, 8) == <<137, 80, 78, 71, 13, 10, 26, 10>>

    assert {:ok, expected} =
             Resvg.svg_string_to_png_binary(@source,
               skip_system_fonts: true,
               resources_dir: System.tmp_dir!()
             )

    assert png == expected
    assert {:ok, image} = PngDecoder.decode(png, nil, false)
    assert {image.width_px, image.height_px} == {4, 2}
    assert image.data == :binary.copy(<<17, 34, 51, 17, 34, 51, 170, 187, 204, 170, 187, 204>>, 2)
  end

  test "width and height options allocate the real target canvas and preserve transparent ink" do
    source =
      "<svg xmlns='http://www.w3.org/2000/svg' width='4' height='2'><rect width='2' height='2' fill='red'/></svg>"

    for scale <- [1, 2] do
      assert {:ok, png} =
               SvgRasterizer.rasterize(source, [width: 8, height: 4, scale: scale], nil)

      assert {:ok, image} = PngDecoder.decode(png, nil, false)
      assert {image.width_px, image.height_px} == {8 * scale, 4 * scale}
      assert :binary.at(image.alpha_data, 1) == 255
      assert :binary.at(image.alpha_data, 8 * scale - 1) == 0
      assert binary_part(image.data, 0, 3) == <<255, 0, 0>>
    end
  end

  test "HTML SVG source becomes ordinary PDF image content while surrounding Unicode remains text" do
    html = "<p>Before Ω</p>#{@source}<p>After café</p>"
    assert {:ok, pdf} = HtmlToPdf.render(html, system_font_discovery: false)
    assert pdf =~ "/Subtype /Image"
    refute pdf =~ "/Subtype /Type3"
    refute pdf =~ "NativeOriginalGID"
    assert {:ok, text} = NativeElixirPdfUtilities.Text.extract(pdf)
    assert text =~ "Before Ω" and text =~ "After café"
    assert {:ok, document} = NativeElixirPdfUtilities.Pdf.Reader.read(pdf)

    assert Enum.any?(
             Map.values(document.objects),
             &match?(%{value: %{"Subtype" => {:name, "Image"}}}, &1)
           )
  end

  test "real authorized PNG resources reach the converter through the declared byte context" do
    assert {:ok, png} = SvgRasterizer.rasterize(@source, [], nil)

    source =
      "<svg xmlns='http://www.w3.org/2000/svg' width='4' height='2'><image href='svg-resource:tile' width='4' height='2'/></svg>"

    context = %{
      mode: :inline,
      resources: %{"svg-resource:tile" => %{data: png, media_type: "image/png"}}
    }

    assert {:ok, rendered} = SvgRasterizer.rasterize(source, [], nil, [], context)
    assert {:ok, image} = PngDecoder.decode(rendered, nil, false)
    assert binary_part(image.data, 0, 3) == <<17, 34, 51>>
    assert binary_part(image.data, 9, 3) == <<170, 187, 204>>
  end

  test "authorized font bytes paint actual Unicode glyph pixels with system discovery disabled" do
    font = File.read!("priv/fonts/dejavu/DejaVuSans.ttf")

    source =
      "<svg xmlns='http://www.w3.org/2000/svg' width='90' height='35'><text x='3' y='26' font-family='DejaVu Sans' font-size='25' fill='#174a86'>Ωé ffi</text></svg>"

    assert {:ok, png} =
             SvgRasterizer.rasterize(source, [], nil, [%{data: font, family: "DejaVu Sans"}])

    assert {:ok, image} = PngDecoder.decode(png, nil, false)
    assert Enum.count(:binary.bin_to_list(image.alpha_data), &(&1 > 0)) > 100

    assert {:ok, reference} =
             Resvg.svg_string_to_png_binary(source,
               skip_system_fonts: true,
               font_files: [Path.expand("priv/fonts/dejavu/DejaVuSans.ttf")],
               font_family: "DejaVu Sans",
               resources_dir: System.tmp_dir!()
             )

    assert {:ok, actual_reference} = PngDecoder.decode(reference, nil, false)
    assert image.data == actual_reference.data and image.alpha_data == actual_reference.alpha_data
  end

  test "malformed XML, active content and unauthorized dependencies keep actionable diagnostics" do
    for source <- [
          "<svg><g></svg>",
          "<!DOCTYPE svg [<!ENTITY ext SYSTEM 'file:///etc/passwd'>]><svg>&ext;</svg>",
          "<svg><script>alert(1)</script></svg>",
          "<svg><image href='https://example.invalid/paint.png'/></svg>"
        ] do
      assert {:error, {reason, diagnostic}} = SvgRasterizer.rasterize(source, [], nil)
      assert reason == diagnostic.reason
      assert is_atom(diagnostic.stage) and is_binary(diagnostic.message)
      assert is_binary(diagnostic.source)
    end
  end

  test "invalid options and source inputs fail before converter execution" do
    for {source, options} <- [
          {nil, []},
          {<<255>>, []},
          {@source, [width: 0]},
          {@source, [height: -1]},
          {@source, [resources_dir: "/tmp"]}
        ] do
      assert {:error, {reason, diagnostic}} = SvgRasterizer.rasterize(source, options, nil)
      assert reason == diagnostic.reason and is_binary(diagnostic.message)
    end
  end

  test "invalid font bytes and resource descriptors cannot authorize substitutes" do
    source = "<svg width='20' height='20'><text y='15'>A</text></svg>"

    assert {:error, {reason, diagnostic}} =
             SvgRasterizer.rasterize(source, [], nil, [%{data: "not a font"}])

    assert reason == diagnostic.reason and is_binary(diagnostic.message)

    assert {:error, {reason, diagnostic}} =
             SvgRasterizer.rasterize(@source, [], nil, [], %{
               mode: :inline,
               resources: %{"unsafe" => %{data: "bad", media_type: "image/png"}}
             })

    assert reason == diagnostic.reason and is_binary(diagnostic.message)
  end

  test "source, raster allocation and output charge their unchanged shared resource limits", %{
    limits: limits
  } do
    for {key, value} <- [
          {:max_svg_bytes, 1},
          {:max_svg_raster_pixels, 1},
          {:max_svg_output_bytes, 1}
        ] do
      Limits.install(Map.put(limits, key, value))

      assert {:error, {:resource_limit_exceeded, diagnostic}} =
               SvgRasterizer.rasterize(@source, [], HtmlValidator.new_image_budget())

      assert diagnostic.reason == :resource_limit_exceeded and is_binary(diagnostic.message)
    end
  end
end
