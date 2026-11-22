defmodule NativeElixirPdfUtilities.HtmlToPdf.SvgConverterSourcePreparationTest do
  use ExUnit.Case, async: true
  alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, SvgScene, SvgRasterizer, PngDecoder}

  alias NativeElixirPdfUtilities.Validators.{
    HtmlValidator,
    SvgConverterValidator,
    SvgSceneValidator
  }

  alias NativeElixirPdfUtilities.TestSupport.PngFixture

  test "qualified image references retain approved PNG bytes across XML transport" do
    png = PngFixture.build(2, 1, 2, 8, 0, fn _, _ -> [10, 20, 30] end)

    source = """
    <s:svg xmlns:s="http://www.w3.org/2000/svg" xmlns:q="http://www.w3.org/1999/xlink" width="2" WIDTH="8" height="1">
      <s:image q:href="svg-resource:tile" width="2" height="1"/>
    </s:svg>
    """

    context = %{
      mode: :inline,
      resources: %{"svg-resource:tile" => %{data: png, media_type: "image/png"}}
    }

    assert {:ok, request} = SvgConverterValidator.prepare(source, [], nil, [], context)
    assert request.source =~ "data:image/png;base64,#{Base.encode64(png)}"
    assert request.source =~ ~s(xmlns:q="http://www.w3.org/1999/xlink")
    refute request.source =~ "svg-resource:tile"
    assert {:ok, rendered} = SvgRasterizer.rasterize(source, [], nil, [], context)
    assert {:ok, image} = PngDecoder.decode(rendered, nil, false)
    assert image.data == :binary.copy(<<10, 20, 30>>, 2)
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
end

defmodule NativeElixirPdfUtilities.HtmlToPdf.SvgConverterSourcePreparationGlobalStateTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.Limits
  alias NativeElixirPdfUtilities.HtmlToPdf.{ResourceContext, SvgRasterizer}

  alias NativeElixirPdfUtilities.Validators.{
    ResourceBudgetValidator,
    HtmlValidator,
    SvgConverterValidator
  }

  alias NativeElixirPdfUtilities.TestSupport.PngFixture

  setup do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    :ok
  end

  test "extraction charges XML source before building its canonical tree" do
    Limits.install(Map.put(Limits.effective(), :max_input_bytes, 1))

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             HtmlValidator.with_render_budget(fn ->
               SvgConverterValidator.prepare_extraction("<svg/>")
             end)

    assert diagnostic.source == "SVG" and diagnostic.message =~ "max_input_bytes"
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

    Limits.install(Map.put(Limits.effective(), :max_decoded_bytes, 20 * 20 * 4))

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             SvgConverterValidator.prepare(source, [], nil, [], context)

    assert diagnostic.stage == :limits
    assert diagnostic.message =~ "max_decoded_bytes"
  end

  test "repeated vector placements charge every level of native parsing and drawing" do
    image = "<image id='tile' href='svg-resource:tile' width='8' height='8'/>"
    reuse = "<use href='#tile'/>"
    path = "<path d='M0 0h2v2z'/>"
    nested = "<image href='svg-resource:leaf' width='8' height='8'/>"
    single = svg(image)
    repeated = svg(image <> reuse)
    context = images(tile: svg(nested), leaf: svg(path))
    first = prepare_receipt(single, context)
    second = prepare_receipt(repeated, context)
    third = prepare_receipt(svg(image <> reuse <> reuse), context)

    for key <- [:max_work, :max_entries, :max_raster_pixels, :max_decoded_bytes] do
      assert third[key] - second[key] == second[key] - first[key]
    end

    assert second.max_raster_pixels - first.max_raster_pixels == 2 * 8 * 8

    assert second.max_decoded_bytes - first.max_decoded_bytes ==
             2 * 8 * 8 * 4 + byte_size("M0 0h2v2z")

    simple_context = images(tile: svg("<rect/>"))
    simple_first = prepare_receipt(single, simple_context)
    simple_second = prepare_receipt(repeated, simple_context)
    assert second.max_work - first.max_work > simple_second.max_work - simple_first.max_work

    limits = Limits.effective()
    Limits.install(%{limits | max_work: second.max_work - 1})
    assert {:ok, _} = SvgConverterValidator.prepare(single, [], nil, [], context)

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             SvgRasterizer.rasterize(repeated, [], nil, [], context)

    assert diagnostic.message =~ "max_work"

    Limits.install(%{limits | max_work: second.max_work})

    assert {:ok, <<137, "PNG", _::binary>>} =
             SvgRasterizer.rasterize(repeated, [], nil, [], context)
  end

  test "nested images inherit the canvas of the group in which each use is drawn" do
    image = "<image id='tile' href='svg-resource:tile' width='8' height='8'/>"
    use = "<use href='#tile'/>"
    context = images(tile: svg("<rect/>"))
    inside = prepare_receipt(svg(image <> "<g opacity='.5'>" <> use <> "</g>"), context)
    outside = prepare_receipt(svg(image <> "<g opacity='.5'><rect/></g>" <> use), context)
    assert inside.max_raster_pixels - outside.max_raster_pixels == 24 * 8 * 8
    assert inside.max_decoded_bytes - outside.max_decoded_bytes == 24 * 8 * 8 * 4

    child_group = fn opacity ->
      images(tile: svg("<g opacity='#{opacity}'><rect width='4' height='4'/></g>"))
    end

    half = prepare_receipt(svg(image <> use), child_group.("0.5"))

    for opacity <- ["50%", "0"] do
      assert prepare_receipt(svg(image <> use), child_group.(opacity)).max_raster_pixels ==
               half.max_raster_pixels
    end

    assert {:ok, _} = SvgConverterValidator.prepare_bounds(svg(image <> use), "tile", context)
  end

  test "percentage and zero opacity isolate the target for a reused vector" do
    image = "<image id='tile' href='svg-resource:tile' width='8' height='8'/>"
    context = images(tile: svg("<rect/>"))
    make = fn opacity -> svg(image <> "<g opacity='#{opacity}'><use href='#tile'/></g>") end
    normal = prepare_receipt(make.("100%"), context)

    for opacity <- ["50%", "0"] do
      receipt = prepare_receipt(make.(opacity), context)
      assert receipt.max_decoded_bytes - normal.max_decoded_bytes == 24 * 8 * 8 * 4
    end
  end

  test "a pattern rendering an embedded SVG uses its actual tile extent" do
    image = "<image href='svg-resource:tile' width='8' height='8'/>"
    context = images(tile: svg("<rect width='4' height='4'/>"))

    make = fn size, units ->
      svg(
        "<defs><pattern id='p' patternUnits='#{units}' width='#{size}' height='#{size}'>#{image}</pattern></defs><rect width='8' height='8' fill='url(#p)'/>"
      )
    end

    small = make.(4, "userSpaceOnUse")
    large = make.(64, "userSpaceOnUse")
    assert {:ok, <<137, "PNG", _::binary>>} = SvgRasterizer.rasterize(small, [], nil, [], context)
    small_cost = prepare_receipt(small, context)
    large_cost = prepare_receipt(large, context)
    assert large_cost.max_raster_pixels >= 64 * 64
    assert large_cost.max_raster_pixels > small_cost.max_raster_pixels

    inherited = fn rectangles ->
      svg(
        "<defs><pattern id='p' patternUnits='userSpaceOnUse' width='4' height='4'>#{image}</pattern></defs><g fill='url(#p)'>#{String.duplicate("<rect width='2' height='2'/>", rectangles)}</g>"
      )
    end

    one_path = prepare_receipt(inherited.(1), context)
    three_paths = prepare_receipt(inherited.(3), context)
    assert three_paths.max_raster_pixels >= one_path.max_raster_pixels + 2 * 4 * 4

    for {source, resources} <- [
          {make.("0.5", "objectBoundingBox"), context},
          {svg("<image href='svg-resource:tile' width='8' height='8'/>"),
           images(
             tile: String.replace(small, "svg-resource:tile", "svg-resource:leaf"),
             leaf: svg("<rect/>")
           )}
        ] do
      assert {:error, {:resource_limit_exceeded, diagnostic}} =
               SvgRasterizer.rasterize(source, [], nil, [], resources)

      assert diagnostic.message =~ "pattern tile"
    end
  end

  test "reusing a converted PNG avoids the native work of the repeated vector placement" do
    source =
      svg("<image id='tile' href='svg-resource:tile' width='8' height='8'/><use href='#tile'/>")

    context = images(tile: svg("<rect width='4' height='4'/>"))
    budget = HtmlValidator.new_image_budget()

    assert true ==
             ResourceContext.run([], budget, fn _ ->
               before = Map.get(ResourceBudgetValidator.snapshot(), :max_raster_pixels, 0)

               assert {:ok, <<137, "PNG", _::binary>> = png} =
                        SvgRasterizer.rasterize(source, [], budget, [], context)

               cold = ResourceBudgetValidator.snapshot().max_raster_pixels
               assert {:ok, ^png} = SvgRasterizer.rasterize(source, [], budget, [], context)
               warm = ResourceBudgetValidator.snapshot().max_raster_pixels
               assert cold - before - (warm - cold) == 8 * 8
             end)
  end

  test "nested raster planes count each reuse of their containing SVG" do
    png = PngFixture.build(2, 2, 2, 8, 0, fn _, _ -> [10, 20, 30] end)
    tile = svg("<image href='svg-resource:px' width='2' height='2'/>")
    context = images(tile: tile)
    context = put_in(context.resources["svg-resource:px"], %{media_type: "image/png", data: png})
    image = "<image id='tile' href='svg-resource:tile' width='8' height='8'/>"
    source = svg(image <> "<use href='#tile'/>")
    single = prepare_receipt(svg(image), context)
    double = prepare_receipt(source, context)
    assert double.max_raster_pixels - single.max_raster_pixels == 8 * 8 + 2 * 2
    assert double.max_decoded_bytes > single.max_decoded_bytes + 8 * 8 * 4
    Limits.install(%{Limits.effective() | max_decoded_bytes: double.max_decoded_bytes - 1})

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             SvgConverterValidator.prepare(source, [], nil, [], context)

    assert diagnostic.source == "SVG"
  end

  defp svg(body), do: "<svg xmlns='http://www.w3.org/2000/svg' width='8' height='8'>#{body}</svg>"

  defp images(pairs) do
    %{
      mode: :inline,
      resources:
        Map.new(pairs, fn {id, data} ->
          {"svg-resource:#{id}", %{media_type: "image/svg+xml", data: data}}
        end)
    }
  end

  defp prepare_receipt(source, context) do
    ResourceBudgetValidator.with_budget(fn ->
      assert {:ok, _} = SvgConverterValidator.prepare(source, [], nil, [], context)
      ResourceBudgetValidator.snapshot()
    end)
  end
end
