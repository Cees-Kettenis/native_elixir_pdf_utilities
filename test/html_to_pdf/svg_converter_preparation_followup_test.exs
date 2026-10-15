defmodule NativeElixirPdfUtilities.HtmlToPdf.SvgConverterPreparationFollowupTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, SvgScene, SvgRasterizer, PngDecoder}

  alias NativeElixirPdfUtilities.Validators.{
    HtmlValidator,
    SvgConverterValidator,
    SvgSceneValidator
  }

  test "unavailable authored image font falls back to actual approved bytes and paints text" do
    fixture =
      File.read!("test/fixtures/html_to_pdf/browser_parity/svg_image_selected_host_font.html")

    [_, encoded] = Regex.run(~r/data:image\/svg\+xml;base64,([^']+)/, fixture)
    assert {:ok, source} = Base.decode64(encoded)
    fonts = [%{data: File.read!("priv/fonts/dejavu/DejaVuSansMono.ttf")}]
    assert {:ok, request} = SvgConverterValidator.prepare(source, [], nil, fonts, %{mode: :image})
    assert request.options[:font_family] == "DejaVu Sans Mono"
    assert request.options[:sans_serif_family] == "DejaVu Sans Mono"
    assert request.source =~ "DejaVu Sans"
    assert {:ok, png} = SvgRasterizer.rasterize(source, [], nil, fonts)
    assert {:ok, %{width_px: 80, height_px: 80} = image} = PngDecoder.decode(png, nil, false)
    colors = for <<r, g, b <- image.data>>, into: MapSet.new(), do: {r, g, b}
    assert MapSet.member?(colors, {255, 255, 255})
    assert MapSet.size(colors) > 1
    refute Map.has_key?(image, :svg_glyph_runs)
  end

  test "author namespaces survive while HTML descendants inherit the actual integration namespace" do
    source =
      "<svg xmlns:xlink='http://www.w3.org/1999/xlink'><foreignObject width='40' height='20'><div xmlns='http://www.w3.org/1999/xhtml' id='original'>Original <span data-label='preserved'>text</span></div></foreignObject></svg>"

    assert {:ok, document} = HtmlParser.parse_detailed(source)
    assert {:ok, root} = SvgSceneValidator.extraction_root(document)
    serialized = SvgScene.authored_source(root)
    assert serialized =~ "xmlns:xlink=\"http://www.w3.org/1999/xlink\""
    assert serialized =~ "<div id=\"original\" xmlns=\"http://www.w3.org/1999/xhtml\">Original "
    assert serialized =~ "<span data-label=\"preserved\">text</span>"

    refute serialized =~
             "<div id=\"original\" xmlns=\"http://www.w3.org/1999/xhtml\" xmlns:xlink="

    assert {:error, {:unsupported_html, %{source: "foreignObject"}}} =
             SvgConverterValidator.prepare_extraction(serialized)
  end

  test "authored XHTML void elements remain well-formed converter input without invented end tags" do
    source =
      "<svg width='40' height='20'><foreignObject width='40' height='20'><div xmlns='http://www.w3.org/1999/xhtml'><input name='original' value='Original &amp; exact'><br><hr><textarea>Original text</textarea></div></foreignObject></svg>"

    assert {:ok, document} = HtmlParser.parse_detailed(source)
    assert {:ok, root} = SvgSceneValidator.extraction_root(document)
    serialized = SvgScene.authored_source(root)
    assert serialized =~ "<input name=\"original\" value=\"Original &amp; exact\"/>"
    assert serialized =~ "<br />" and serialized =~ "<hr />"
    assert serialized =~ "<textarea >Original text</textarea>"
    refute serialized =~ "</input>" or serialized =~ "</br>" or serialized =~ "</hr>"

    assert {:error, {:unsupported_html, %{source: "foreignObject"}}} =
             SvgConverterValidator.prepare_extraction(serialized)

    assert {:error, {:unsupported_html, diagnostic}} =
             SvgScene.extract(serialized, HtmlValidator.new_image_budget(),
               system_font_discovery: false
             )

    assert diagnostic.stage == :layout and diagnostic.source == "foreignObject"
    assert diagnostic.module == SvgConverterValidator
    assert diagnostic.operation == :convert
  end

  test "legitimate XHTML reaches the genuine converter capability diagnostic" do
    source =
      "<svg width='40' height='20'><foreignObject width='40' height='20'><div xmlns='http://www.w3.org/1999/xhtml'>Original <span>text</span></div></foreignObject></svg>"

    assert {:error, {:unsupported_html, diagnostic}} =
             SvgScene.extract(source, HtmlValidator.new_image_budget(),
               system_font_discovery: false
             )

    assert diagnostic.stage == :layout and diagnostic.source == "foreignObject"
    assert diagnostic.message =~ "existing SVG converter does not render foreignObject"
  end

  test "unsupported authored namespace declarations remain ordinary input failures" do
    source =
      "<svg><foreignObject><div xmlns='urn:other'>Original text</div></foreignObject></svg>"

    assert {:error, {:invalid_document, diagnostic}} =
             SvgConverterValidator.prepare_extraction(source)

    assert diagnostic.source == "SVG"
    assert diagnostic.stage == :style
    assert diagnostic.message =~ "namespace"
  end
end
