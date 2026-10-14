defmodule NativeElixirPdfUtilities.HtmlToPdf.SvgConverterWorkflowTest do
  use ExUnit.Case, async: false

  alias NativeElixirPdfUtilities.{HtmlToPdf, Text}
  alias NativeElixirPdfUtilities.Pdf.Reader
  alias NativeElixirPdfUtilities.TestSupport.PdfVisualCompare

  @moduletag :svg_converter_workflow

  test "inline SVG becomes real converted pixels in the public PDF" do
    source = """
    <svg xmlns="http://www.w3.org/2000/svg" width="16" height="8">
    <rect width="16" height="8" fill="#ff0000" />
    </svg>
    """

    assert {:ok, pdf} = HtmlToPdf.render(source, system_font_discovery: false)
    assert :ok = PdfVisualCompare.assert_svg_image_artwork!(pdf)
    assert {:ok, document} = Reader.read(pdf)

    images =
      for {ref,
           %{value: %{"Subtype" => {:name, "Image"}, "ColorSpace" => {:name, "DeviceRGB"}}} =
             image} <- document.objects,
          do: {ref, image}

    assert length(images) == 1
    {ref, image} = hd(images)
    pixels = image.value["Width"] * image.value["Height"]
    assert {:ok, decoded} = Reader.decoded_stream(document, {:ref, ref})
    assert decoded == :binary.copy(<<255, 0, 0>>, pixels)
  end

  test "hidden and zero SVG canvases retain the whole-input foreignObject diagnostic" do
    for attributes <- [
          "width='0' height='0'",
          "width='20' height='20' style='display:none'",
          "width='20' height='20' opacity='0'"
        ] do
      source =
        "<svg #{attributes}><foreignObject width='10' height='10'><div>Original HTML</div></foreignObject></svg>"

      assert {:error, {:unsupported_html, diagnostic}} = HtmlToPdf.render(source)
      assert diagnostic.reason == :unsupported_html and diagnostic.stage == :layout
      assert diagnostic.operation == :convert and diagnostic.source == "foreignObject"
      assert diagnostic.module == NativeElixirPdfUtilities.Validators.SvgConverterValidator
      assert diagnostic.message =~ "cannot be omitted"
    end
  end

  test "computed identity transforms keep actual clipped converter artwork visible" do
    source =
      "<svg width='40' height='20'><defs><clipPath id='c'><rect width='20' height='20'/></clipPath></defs><rect width='40' height='20' fill='red' clip-path='url(#c)'/></svg>"

    assert {:ok, pdf} = HtmlToPdf.render(source, system_font_discovery: false)
    assert {:ok, document} = Reader.read(pdf)

    masks =
      for {_,
           %{value: %{"Subtype" => {:name, "Image"}, "ColorSpace" => {:name, "DeviceGray"}}} =
             image} <- document.objects do
        plane = :zlib.uncompress(image.stream)
        assert byte_size(plane) == image.value["Width"] * image.value["Height"]
        :binary.bin_to_list(plane)
      end

    assert [mask] = masks
    assert Enum.count(mask, &(&1 == 255)) == div(length(mask), 2)
    assert Enum.count(mask, &(&1 == 0)) == div(length(mask), 2)
  end

  test "SVG source captions paint image pixels without PDF text with real font bytes" do
    source = """
    <svg xmlns="http://www.w3.org/2000/svg" width="160" height="40">
    <text x="5" y="25" font-family="DejaVu Sans Mono" font-size="18">Original café Ω</text>
    </svg>
    """

    bytes = File.read!("priv/fonts/dejavu/DejaVuSansMono.ttf")

    assert {:ok, pdf} =
             HtmlToPdf.render(source,
               system_font_discovery: false,
               fonts: [%{family: "DejaVu Sans Mono", data: bytes, weight: 400}]
             )

    assert :ok = PdfVisualCompare.assert_svg_image_artwork!(pdf)
    refute pdf =~ "/ActualText"
    assert {:error, {:no_extractable_text, diagnostic}} = Text.extract(pdf, layout: false)
    assert diagnostic.stage == :text_extraction
    assert {:ok, document} = Reader.read(pdf)

    # This actual generated mask contains long transparent runs. Decode the
    # trusted test output directly and check its exact bounded pixel plane.
    masks =
      for {_,
           %{value: %{"Subtype" => {:name, "Image"}, "ColorSpace" => {:name, "DeviceGray"}}} =
             image} <- document.objects do
        pixels = :zlib.uncompress(image.stream)
        assert byte_size(pixels) == image.value["Width"] * image.value["Height"]
        :binary.bin_to_list(pixels)
      end

    assert Enum.any?(masks, fn pixels -> 0 in pixels and Enum.any?(pixels, &(&1 > 0)) end)
  end

  test "SVG navigation remains inert artwork while an enclosing HTML anchor stays clickable" do
    source = """
    <a href="https://example.test/outer"><svg width="20" height="20">
    <a href="https://example.test/inside"><rect width="20" height="20" fill="red" /></a>
    </svg></a>
    """

    assert {:ok, pdf} =
             HtmlToPdf.render(source,
               asset_resolver: fn _ -> flunk("inert navigation must not fetch assets") end
             )

    assert :ok = PdfVisualCompare.assert_svg_image_artwork!(pdf)
    assert {:ok, document} = Reader.read(pdf)

    links =
      for {_, %{value: %{"Subtype" => {:name, "Link"}} = link}} <- document.objects,
          do: link["A"]["URI"]

    assert links == [{:string, "https://example.test/outer"}]

    encoded =
      Base.encode64(
        "<svg xmlns='http://www.w3.org/2000/svg' width='20' height='20'><a href='https://example.test/inside'><rect width='20' height='20' fill='red'/></a></svg>"
      )

    assert {:ok, image_pdf} = HtmlToPdf.render("<img src='data:image/svg+xml;base64,#{encoded}'>")
    assert :ok = PdfVisualCompare.assert_svg_image_artwork!(image_pdf)
    assert {:ok, image_document} = Reader.read(image_pdf)

    refute Enum.any?(image_document.objects, fn {_, object} ->
             object.value["Subtype"] == {:name, "Link"}
           end)
  end

  test "inert SVG navigation still obeys canonical active URL validation" do
    for href <- ["javascript:alert(1)", "java&#10;script:alert(1)"] do
      source =
        "<svg width='20' height='20'><a href='#{href}'><rect width='20' height='20'/></a></svg>"

      assert {:error, {:unsupported_html, diagnostic}} = HtmlToPdf.render(source)
      assert diagnostic.stage == :html
      assert diagnostic.message =~ "Active URL"
    end
  end

  test "unapproved SVG image dependencies return an actionable public diagnostic" do
    reference = "https://example.invalid/unapproved.png"

    source = """
    <svg xmlns="http://www.w3.org/2000/svg" width="20" height="20">
    <image href="#{reference}" width="20" height="20" />
    </svg>
    """

    assert {:error, {reason, diagnostic}} = HtmlToPdf.render(source)
    assert diagnostic.reason == reason
    assert diagnostic.stage in [:asset, :resource]
    assert diagnostic.source == reference
    assert is_binary(diagnostic.message) and diagnostic.message != ""
  end
end
