defmodule NativeElixirPdfUtilities.HtmlToPdf.ImageProcessingPolicyTest do
  use ExUnit.Case, async: true
  alias NativeElixirPdfUtilities.HtmlToPdf
  alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, Layout, Style}

  alias NativeElixirPdfUtilities.Validators.{
    CssImageValueValidator,
    ImageMetadataValidator,
    JpegValidator
  }

  alias NativeElixirPdfUtilities.TestSupport.{Paths, PngFixture}

  # CSS Images 3, 18 December 2023, section 5; Images 4, 30 September
  # 2025, section 6. TIFF orientation is the fixed Exif 1..8 enumeration.
  test "source orientation changes natural geometry and the PDF image transform while preserving encoded pixels" do
    tiff =
      <<"II", 42::little-16, 8::little-32, 1::little-16, 0x112::little-16, 3::little-16,
        1::little-32, 6::little-16, 0::16, 0::little-32>>

    png =
      PngFixture.build(2, 4, 2, 8, 0, fn x, y -> [x * 255, y * 60, 0] end,
        chunks: PngFixture.chunk("eXIf", tiff)
      )

    uri = "data:image/png;base64," <> Base.encode64(png)
    assert ImageMetadataValidator.tiff(tiff) == %{exif_orientation: 6}

    for {policy, width, height, orientation} <- [
          {"", 3.0, 1.5, 6},
          {"image-orientation:none", 1.5, 3.0, 1},
          {"image-orientation:90deg flip", 3.0, 1.5, 5}
        ] do
      html = "<img src='#{uri}' style='#{policy}'>"
      assert {:ok, dom} = HtmlParser.parse(html)

      assert {:ok, %{children: [node]} = styled} =
               Style.compute(dom, system_font_discovery: false)

      assert node.style.image.width == width and node.style.image.height == height
      assert node.style.image.width_px == 2 and node.style.image.height_px == 4
      assert Map.get(node.style.image, :image_orientation, 1) == orientation
      assert {:ok, layout} = Layout.layout(styled, page_size: {50, 50}, margin: 0)
      [box] = Enum.filter(layout.boxes, &(&1.type == :image))
      assert box.width == width and box.height == height

      assert {:ok, pdf} =
               HtmlToPdf.render(html, compress_streams: false, system_font_discovery: false)

      if orientation == 6, do: assert(pdf =~ "0 -1 1 0 0 1 cm /")
    end
  end

  test "metadata resolution is opt-in and snap uses the declared print density" do
    png =
      PngFixture.build(4, 2, 2, 8, 0, fn _, _ -> [0, 255, 0] end,
        chunks: PngFixture.chunk("pHYs", <<7560::32, 7560::32, 1>>)
      )

    uri = "data:image/png;base64," <> Base.encode64(png)

    for {policy, width} <- [
          {"", 3.0},
          {"image-resolution:from-image", 3 / (7560 * 0.0254 / 96)},
          {"image-resolution:from-image snap", 1.5},
          {"image-resolution:192dpi", 1.5}
        ] do
      assert {:ok, dom} = HtmlParser.parse("<img src='#{uri}' style='#{policy}'>")
      assert {:ok, %{children: [node]}} = Style.compute(dom)
      assert_in_delta node.style.image.width, width, 0.000001
      assert_in_delta node.style.image.height, width / 2, 0.000001
    end

    assert ImageMetadataValidator.png_resolution(<<1::32, 1::32, 0>>) == %{}

    assert ImageMetadataValidator.jfif(<<"JFIF", 0, 1, 1, 1, 192::16, 96::16, 0, 0>>) == %{
             resolution_densities: {2.0, 1.0}
           }
  end

  test "JPEG APP metadata uses the same orientation parser without changing compressed sample bytes" do
    original = File.read!(Paths.fixture("images/jpeg/progressive_rgb.jpg"))
    <<255, 216, rest::binary>> = original

    tiff =
      <<"MM", 42::big-16, 8::big-32, 1::big-16, 0x112::big-16, 3::big-16, 1::big-32, 8::big-16,
        0::16, 0::big-32>>

    bytes = "Exif" <> <<0, 0>> <> tiff

    jpeg =
      <<255, 216, 255, 226, 4::16, "xy", 255, 225, byte_size(bytes) + 2::16, bytes::binary,
        rest::binary>>

    assert {:ok, %{exif_orientation: 8}} = JpegValidator.metadata(jpeg)
    assert {:ok, dom} = HtmlParser.parse("<img src='photo'>")
    assert {:ok, %{children: [node]}} = Style.compute(dom, assets: %{"photo" => {:bytes, jpeg}})
    assert node.style.image.data == jpeg
    assert node.style.image.width == node.style.image.height_px * 0.75
  end

  test "static processing grammar rejects invalid controls and returns actionable styled-tree errors" do
    for {property, value} <- [
          {"image-orientation", "45%"},
          {"image-orientation", "flip flip"},
          {"image-orientation", "none flip"},
          {"image-resolution", "0dpi"},
          {"image-resolution", "snap"},
          {"image-resolution", "2x 3x"},
          {"image-resolution", "from-image from-image"},
          {"image-rendering", "animated"}
        ] do
      assert :error = CssImageValueValidator.prepare_policy(property, value)
    end

    assert {:ok, %{image_orientation: {:turns, 0, false}}} =
             CssImageValueValidator.prepare_policy("image-orientation", "-45deg")

    for fields <- [
          %{image_orientation: :other},
          %{image_orientation: {:turns, 5, false}},
          %{image_resolution: %{density: 0, from_image?: false, snap?: false}},
          %{image_rendering: :dynamic}
        ] do
      assert {:error, {:invalid_layout, %{stage: :layout, source: "img", message: message}}} =
               CssImageValueValidator.validate_style(fields, "img")

      assert message =~ "image policies"
    end

    assert ImageMetadataValidator.tiff(<<"II", 42::little-16, 0xFFFFFFFF::little-32>>) == %{}
    assert ImageMetadataValidator.tiff(<<"invalid">>) == %{}
  end

  test "rendering modes reuse the writer's existing interpolation flag" do
    png = PngFixture.build(2, 2, 2, 8, 0, fn x, y -> [x * 255, y * 255, 0] end)

    for {mode, interpolation} <- [
          {"smooth", true},
          {"high-quality", true},
          {"crisp-edges", false},
          {"pixelated", false}
        ] do
      assert {:ok, dom} = HtmlParser.parse("<img src=tile style='image-rendering:#{mode}'>")
      assert {:ok, %{children: [node]}} = Style.compute(dom, assets: %{"tile" => {:bytes, png}})
      assert node.style.image.interpolate == interpolation
    end
  end

  test "TIFF rational resolutions honor inch and centimeter units and ignore malformed optional tags" do
    for {unit, factor} <- [{2, 1 / 96}, {3, 2.54 / 96}] do
      entries =
        <<0x128::little-16, 3::little-16, 1::little-32, unit::little-16, 0::16, 0x11A::little-16,
          5::little-16, 1::little-32, 74::little-32, 0x11B::little-16, 5::little-16, 1::little-32,
          82::little-32, 0x112::little-16, 3::little-16, 1::little-32, 9::little-16, 0::16,
          999::little-16, 1::little-16, 1::little-32, 0::32>>

      tiff =
        <<"II", 42::little-16, 8::little-32, 5::little-16, entries::binary, 0::32, 192::little-32,
          1::little-32, 96::little-32, 1::little-32>>

      metadata = ImageMetadataValidator.tiff(tiff)
      assert metadata.resolution_densities == {192 * factor, 96 * factor}
      refute Map.has_key?(metadata, :exif_orientation)
      malformed = binary_part(tiff, 0, byte_size(tiff) - 4) <> <<0::32>>
      assert ImageMetadataValidator.tiff(malformed) == %{}
    end

    assert ImageMetadataValidator.tiff(
             <<"II", 42::little-16, 8::little-32, 1::little-16, 0::128>>
           ) == %{}

    assert ImageMetadataValidator.jfif(<<"JFIF", 0, 1, 1, 2, 100::16, 50::16, 0, 0>>).resolution_densities ==
             {100 * 2.54 / 96, 50 * 2.54 / 96}
  end

  test "zero natural dimensions retain the oriented aspect ratio during object fitting" do
    tiff =
      <<"II", 42::little-16, 8::little-32, 1::little-16, 0x112::little-16, 3::little-16,
        1::little-32, 6::little-16, 0::16, 0::little-32>>

    png =
      PngFixture.build(2, 4, 2, 8, 0, fn x, y -> [x * 255, y * 60, 0] end,
        chunks: PngFixture.chunk("eXIf", tiff)
      )

    for {policy, width, height} <- [{"from-image", 60, 30}, {"none", 30, 60}] do
      assert {:ok, dom} =
               HtmlParser.parse(
                 "<img srcset='photo 10w' sizes='0' style='width:60pt;height:60pt;object-fit:contain;image-orientation:#{policy}'>"
               )

      assert {:ok, styled} =
               Style.compute_detailed(dom,
                 assets: %{"photo" => {:bytes, png}},
                 system_font_discovery: false
               )

      assert {:ok, plan} = Layout.layout(styled, page_size: {100, 100}, margin: 0)
      [image] = Enum.filter(plan.boxes, &(&1.type == :image))
      assert image.image.width == 0 and image.image.height == 0
      assert image.width == width and image.height == height
    end
  end
end
