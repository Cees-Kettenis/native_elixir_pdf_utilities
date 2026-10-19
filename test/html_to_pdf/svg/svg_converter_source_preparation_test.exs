defmodule NativeElixirPdfUtilities.HtmlToPdf.SvgConverterSourcePreparationTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, SvgScene, SvgRasterizer, PngDecoder}
  alias NativeElixirPdfUtilities.Limits

  alias NativeElixirPdfUtilities.Validators.{
    HtmlValidator,
    SvgConverterValidator,
    SvgSceneValidator
  }

  alias NativeElixirPdfUtilities.TestSupport.PngFixture

  setup do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    :ok
  end

  test "extracted scenes support optional budgets and independently discovered text fonts" do
    source = "<svg width='24' height='24'><text y='18'>A</text></svg>"
    assert {:ok, scene, [_ | _] = fonts} = SvgScene.extract(source, nil, [], [])
    assert :ok = SvgSceneValidator.validate_prepared(scene)
    assert {:ok, %{image: image}} = SvgScene.paint(scene, 18.0)
    assert image.width_px == 48
    assert Enum.any?(:binary.bin_to_list(image.alpha_data), &(&1 > 0))
    assert hd(scene.selected_fonts).data == hd(fonts).data
  end

  test "marker conversion retains authored viewBox and omitted default dimensions" do
    source = """
    <svg width="20" height="20"><defs>
    <marker id="m" viewBox="0 0 6 6"><circle cx="3" cy="3" r="3" fill="red"/></marker>
    </defs><path d="M2 10H10" stroke="black" marker-end="url(#m)"/></svg>
    """

    assert {:ok, request} = SvgConverterValidator.prepare(source, [], nil, [], %{mode: :image})
    assert request.source =~ "viewBox=\"0 0 6 6\""
    refute request.source =~ "markerWidth="
    assert {:ok, png} = SvgRasterizer.rasterize(source, [], nil)
    assert {:ok, image} = PngDecoder.decode(png, nil, false)
    assert Enum.any?(for(<<r, g, b <- image.data>>, do: r > 200 and g < 50 and b < 50))
  end

  test "unavailable authored image font falls back to approved bytes and paints text" do
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

  test "author namespaces survive while HTML descendants inherit the integration namespace" do
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

  test "legitimate XHTML reaches the converter capability diagnostic" do
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

  test "with_root_viewport serializes the selected root size by default" do
    source =
      "<svg xmlns='http://www.w3.org/2000/svg' xml:space='preserve' width='4' height='4'><rect width='4' height='4'/></svg>"

    assert {:ok, projected} = SvgConverterValidator.with_root_viewport(source, {10, 5})
    assert projected =~ "width=\"10.0\""
    assert projected =~ "height=\"5.0\""
    assert projected =~ "xml:space=\"preserve\""
    # Descendant geometry stays untouched; only the root canvas is re-encoded.
    assert projected =~ "<rect height=\"4\" width=\"4\">"
  end

  test "with_root_viewport forwards source admission and bounded viewport diagnostics" do
    assert {:error, {:invalid_document, malformed}} =
             SvgConverterValidator.with_root_viewport("<svg><rect></svg>", {10, 5})

    assert malformed.source == "SVG"
    assert malformed.stage == :style
    assert malformed.message =~ "malformed"

    source =
      "<svg xmlns='http://www.w3.org/2000/svg' width='4' height='4'><rect width='4' height='4'/></svg>"

    assert {:error, {:invalid_document, viewport}} =
             SvgConverterValidator.with_root_viewport(source, {0, 5})

    assert viewport.module == SvgConverterValidator
    assert viewport.message =~ "SVG viewport requires positive bounded dimensions"
  end

  test "tree font staging rejects selected text faces without authorized bytes" do
    root = %{
      tag: "svg",
      attributes: %{},
      children: [
        %{
          type: :element,
          tag: "text",
          attributes: %{"x" => "0"},
          children: [],
          style: %{display: :inline, font_face: %{type: :system, family: "System Face"}}
        }
      ]
    }

    assert {:error, {:unsupported_html, diagnostic}} =
             SvgConverterValidator.prepare_tree_fonts(root)

    assert diagnostic.module == SvgConverterValidator
    assert diagnostic.operation == :convert
    assert diagnostic.source == "text"
    assert diagnostic.stage == :layout
    assert diagnostic.message =~ "SVG text requires an authorized byte-backed font"
  end

  test "marker conversion preserves authored viewport geometry and child content" do
    for attributes <- [
          "viewBox='5 5 10 10' markerWidth='20' markerHeight='20' preserveAspectRatio='none' refX='0' refY='0'",
          "viewBox='0 0 10 10' markerWidth='20' markerHeight='30' preserveAspectRatio='xMidYMid slice' refX='0' refY='0'",
          "viewBox='0 0 10 10' markerWidth='7' markerHeight='7' refX='1' refY='1'",
          "viewBox='0 0 100000000 100000000' markerWidth='3' markerHeight='3' refX='0' refY='0'",
          "viewBox='0 0 10 10' markerWidth='1px' markerHeight='3'"
        ] do
      source =
        "<svg xmlns='http://www.w3.org/2000/svg' width='40' height='40'><defs><marker id='m' #{attributes}>x<path d='M0 0L10 10'/></marker></defs><path d='M5 5L15 15' marker-start='url(#m)' stroke='black'/></svg>"

      assert {:ok, request} = SvgConverterValidator.prepare(source, [], nil, [], %{mode: :image})

      for [_, key, value] <- Regex.scan(~r/(\w+)='([^']*)'/, attributes) do
        assert request.source =~ ~s(#{key}="#{value}")
      end

      assert request.source =~ ">x<path d=\"M0 0L10 10\""
      refute request.source =~ "transform="
    end
  end

  test "glyph turn reduction reduces authored angles and keeps inexpressible or invalid turns" do
    fonts = [%{data: File.read!("priv/fonts/dejavu/DejaVuSans.ttf"), family: "DejaVu Sans"}]

    for {rotate, kept} <- [
          {"400", "rotate=\"40\""},
          {"360.00000000000006", "rotate=\"360.00000000000006\""},
          {"1px", "rotate=\"1px\""}
        ] do
      source =
        "<svg xmlns='http://www.w3.org/2000/svg' width='50' height='20'><text x='0' y='15' font-family='DejaVu Sans' rotate='#{rotate}'>t</text></svg>"

      assert {:ok, request} =
               SvgConverterValidator.prepare(source, [], nil, fonts, %{mode: :image})

      assert request.source =~ kept
    end
  end

  test "reused nested raster resources charge their decoded planes per expansion" do
    png = PngFixture.build(20, 20, 2, 8, 0, fn _, _ -> [10, 20, 30] end)

    nested =
      "<svg xmlns='http://www.w3.org/2000/svg' width='20' height='20'><image id='px' href='svg-resource:px' width='20' height='20'/><use href='#px'/><use href='#px'/></svg>"

    source =
      "<svg xmlns='http://www.w3.org/2000/svg' width='20' height='20'><image href='svg-resource:nested' width='20' height='20'/></svg>"

    context = %{
      mode: :inline,
      resources: %{
        "svg-resource:nested" => %{data: nested, media_type: "image/svg+xml"},
        "svg-resource:px" => %{data: png, media_type: "image/png"}
      }
    }

    assert {:ok, request} = SvgConverterValidator.prepare(source, [], nil, [], context)
    assert request.source =~ "data:image/svg+xml;base64,"
    refute request.source =~ "svg-resource:px"

    Limits.install(Map.put(Limits.effective(), :max_aggregate_decoded_image_bytes, 20 * 20 * 4))

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             SvgConverterValidator.prepare(source, [], nil, [], context)

    assert diagnostic.stage == :limits
    assert diagnostic.message =~ "aggregate decoded image bytes exceed the limit"
  end
end
