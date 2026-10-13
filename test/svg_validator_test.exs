Code.require_file("support/png_fixture.ex", __DIR__)

defmodule NativeElixirPdfUtilities.SvgValidatorTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.Limits
  alias NativeElixirPdfUtilities.Validators.SvgValidator
  alias NativeElixirPdfUtilities.HtmlToPdf.{SvgBackend, SvgRasterizer, PngDecoder}
  alias NativeElixirPdfUtilities.Validators.{HtmlValidator, SvgBackendValidator}
  alias NativeElixirPdfUtilities.TestSupport.PngFixture
  alias NativeElixirPdfUtilities.HtmlToPdf

  setup do
    old = Limits.effective()
    on_exit(fn -> Limits.install(old) end)
    %{limits: old}
  end

  test "SVG asset static input policy diagnoses executable and event content before conversion" do
    for content <- [
          "<script>run()</script>",
          "<script type=\"application/json\">{}</script>",
          "<g onload=\"run()\"/>",
          "<g ONLOAD=\"run()\"/>",
          "<g OnLoad=\"run()\"/>",
          "<g xmlns:e=\"urn:events\" e:onclick=\"run()\"/>",
          "<foreignObject><div xmlns=\"http://www.w3.org/1999/xhtml\" onclick=\"run()\">Text</div></foreignObject>",
          "<foreignObject><script xmlns=\"http://www.w3.org/1999/xhtml\">run()</script></foreignObject>",
          "<foreignObject><script xmlns=\"http://www.w3.org/1999/xhtml\" xmlns:e=\"urn:other\" e:type=\"application/json\">run()</script></foreignObject>"
        ] do
      source = svg("\n" <> content)

      for result <- [
            SvgValidator.validate(source, [], nil),
            SvgRasterizer.rasterize(source, [], nil)
          ] do
        assert {:error, {:unsupported_html, diagnostic}} = result
        assert diagnostic.stage == :style
        assert diagnostic.reason == :unsupported_html
        assert diagnostic.source == "SVG"
        assert diagnostic.line == 2
        assert is_binary(diagnostic.message)
      end

      uri = "data:image/svg+xml;base64," <> Base.encode64(source)

      assert {:error, {:unsupported_html, %{source: "SVG", line: 2, operation: :rasterize}}} =
               HtmlToPdf.render("<img src='#{uri}'>")
    end

    assert {:ok, _} =
             SvgValidator.validate(
               svg(
                 "<foreignObject><script xmlns=\"http://www.w3.org/1999/xhtml\" type=\"application/json\">{}</script></foreignObject>"
               ),
               [],
               nil
             )

    assert {:ok, _} =
             SvgRasterizer.rasterize(
               svg("<rect width=\"1\" height=\"1\" fill=\"red\"/>"),
               [],
               nil
             )
  end

  test "namespace-aware parsing checks root, declarations, and decoded resource references" do
    assert {:ok, _} = SvgValidator.validate(svg("<!-- <!DOCTYPE svg> -->"), [], nil)

    valid =
      "<s:svg xmlns:s=\"http://www.w3.org/2000/svg\" width=\"1\" height=\"1\"><!-- hi --><s:defs><s:linearGradient id=\"g\"/></s:defs><s:rect fill=\"url(#g)\"/><s:use href=\"&#35;g\"/></s:svg>"

    assert {:ok, [width: 1, height: 1]} = SvgValidator.validate(valid, [], nil)

    for content <- [
          "<image/>",
          "<feImage/>",
          "<use href=\"&#47;tmp/private\"/>",
          "<use href=\"file:///tmp/a\"/>",
          "<rect fill=\"url(https://example.org/a)\"/>",
          "<style>@import 'a.css';</style>",
          "<rect style=\"fill:u\\72l(a)\"/>"
        ] do
      assert {:error, {:invalid_document, %{source: "SVG", line: 1}}} =
               SvgValidator.validate(svg(content), [], nil)
    end

    for source <- [
          "<html/>",
          "<svg xmlns=\"urn:other\"/>",
          "<svg>",
          svg("") <> "bad",
          svg("<bad></oops>")
        ] do
      assert {:error, {:invalid_document, %{source: "SVG"}}} =
               SvgValidator.validate(source, [], nil)
    end

    assert {:error, {:invalid_document, %{message: message}}} =
             SvgValidator.validate(<<255>>, [], nil)

    assert message =~ "UTF-8"

    for options <- [[1], [zoom: 2], :invalid] do
      assert {:error, {:invalid_document, _}} = SvgValidator.validate(svg(""), options, nil)
    end
  end

  test "all DTD forms return an actionable diagnostic before conversion" do
    for declaration <- [
          "<!DOCTYPE svg>",
          "<!DOCTYPE svg []>",
          "<!DOCTYPE svg SYSTEM \"file:///does-not-exist.dtd\">",
          "<!DOCTYPE svg PUBLIC \"example\" \"https://example.org/svg.dtd\">",
          "<!DOCTYPE svg [<!ENTITY content \"expanded\">]>"
        ] do
      assert {:error,
              {:invalid_document, %{stage: :style, source: "SVG", line: 1, message: message}}} =
               SvgRasterizer.rasterize(declaration <> svg(""), [], nil)

      assert message =~ "DTD/entity declarations"
    end
  end

  test "structural limits stop parsing and a subsequent valid request recovers", %{limits: old} do
    for {key, content} <- [
          {:max_svg_nodes, "<rect/>"},
          {:max_svg_depth, "<g/>"},
          {:max_svg_path_bytes, "<path d=\"M0 0\"/>"},
          {:max_svg_filter_primitives, "<filter><feFlood/><feBlend/></filter>"},
          {:max_svg_references, "<use href=\"#a\"/><use href=\"#a\"/>"}
        ] do
      Limits.install(Map.put(old, key, 1))

      assert {:error, {:resource_limit_exceeded, %{message: message, line: 1}}} =
               SvgValidator.validate(svg(content), [], nil)

      assert message =~ Atom.to_string(key)
      assert {:ok, _} = SvgValidator.validate(svg(""), [], nil)
    end
  end

  test "conversion errors retain their explanation and output size is bounded", %{limits: old} do
    assert {:error, {:invalid_document, %{message: message, source: "SVG"}}} =
             SvgValidator.conversion_result({:error, "invalid size"})

    assert message =~ "invalid size"

    assert {:error, {:invalid_document, %{message: message}}} =
             SvgValidator.conversion_result(:unexpected)

    assert message =~ "unexpected"
    Limits.install(%{old | max_svg_output_bytes: 1})
    assert {:ok, "x"} = SvgValidator.conversion_result({:ok, "x"})
    assert {:error, {:resource_limit_exceeded, _}} = SvgValidator.conversion_result({:ok, "xx"})

    assert {:error, {:resource_limit_exceeded, _}} =
             SvgRasterizer.rasterize(svg("<rect width=\"1\" height=\"1\"/>"), [], nil)
  end

  test "native artifact returns a PNG binary and public failures have the diagnostics contract" do
    assert {:ok, <<137, 80, 78, 71, _::binary>>} =
             SvgRasterizer.rasterize(svg("<rect width=\"1\" height=\"1\"/>"), [], nil)

    source = "data:image/svg+xml;base64," <> Base.encode64(svg("<image/>"))

    assert {:error,
            {:invalid_document,
             %{stage: :style, source: "SVG", operation: :render, module: HtmlToPdf}}} =
             HtmlToPdf.render("<img src=\"#{source}\">")
  end

  test "text remains literal while CSS character events are validated together" do
    assert {:ok, _} =
             SvgValidator.validate(svg("<text>C:\\temp and url(example)</text>"), [], nil)

    assert {:error, {:invalid_document, _}} =
             SvgValidator.validate(
               svg("<style>rect{fill:ur&#108;(https://example.org/a)}</style>"),
               [],
               nil
             )

    assert {:ok, _} = SvgValidator.validate(svg("<style>rect{fill:url('#a')}</style>"), [], nil)
  end

  test "foreign attributes cannot replace the SVG path budget", %{limits: old} do
    Limits.install(%{old | max_svg_path_bytes: 1})

    assert {:error, {:resource_limit_exceeded, _}} =
             SvgValidator.validate(svg("<path xmlns:x=\"urn:x\" d=\"M0 0\" x:d=\"\"/>"), [], nil)
  end

  test "reference budgets include namespaced href attributes", %{limits: old} do
    Limits.install(%{old | max_svg_references: 1})

    for attributes <- [
          ["xlink:href", "xlink:href"],
          ["href", "xlink:href"]
        ] do
      content = Enum.map_join(attributes, &"<use #{&1}=\"#a\"/>")
      source = svg("<g xmlns:xlink=\"http://www.w3.org/1999/xlink\">#{content}</g>")

      assert {:error,
              {:resource_limit_exceeded,
               %{stage: :limits, source: "SVG", line: 1, message: message}}} =
               SvgValidator.validate(source, [], nil)

      assert message =~ "max_svg_references"
    end

    assert {:ok, _} =
             SvgValidator.validate(
               svg(
                 "<rect id=\"a\"/><use xmlns:xlink=\"http://www.w3.org/1999/xlink\" xlink:href=\"#a\"/>"
               ),
               [],
               nil
             )
  end

  test "overflowing SVG dimensions return diagnostics instead of raising" do
    for {source, options} <- [
          {"<svg width=\"1e308in\" height=\"1\"/>", []},
          {"<svg viewBox=\"0 0 1e308 1\" width=\"1000%\"/>", []},
          {"<svg width=\"1e-308\" height=\"1e308\"/>", [width: 1]},
          {"<svg width=\"1e308\" height=\"1e-308\"/>", [height: 1]}
        ] do
      assert {:error, {:invalid_document, %{stage: :style, source: "SVG", message: message}}} =
               SvgRasterizer.rasterize(source, options, nil)

      assert message =~ "numeric range"
    end

    source =
      "data:image/svg+xml;base64," <> Base.encode64("<svg width=\"1e308in\" height=\"1\"/>")

    assert {:error,
            {:invalid_document,
             %{stage: :style, source: "SVG", operation: :render, module: HtmlToPdf}}} =
             HtmlToPdf.render("<img src=\"#{source}\">")
  end

  test "native PNG dimension bounds cannot be disabled by configuration", %{limits: old} do
    Limits.install(%{
      old
      | max_svg_raster_dimension: 4_294_967_296,
        max_svg_raster_pixels: 4_294_967_296
    })

    assert {:error, {:invalid_document, %{message: message}}} =
             SvgValidator.validate(svg(""), [width: 2_147_483_648, height: 1], nil)

    assert message =~ "31-bit"
  end

  test "duplicate expanded XML attributes are rejected before native conversion" do
    source = svg("<path xmlns:a=\"urn:x\" xmlns:b=\"urn:x\" a:d=\"x\" b:d=\"y\"/>")

    assert {:error, {:invalid_document, %{message: message}}} =
             SvgRasterizer.rasterize(source, [], nil)

    assert message =~ "unique expanded namespace/name"
  end

  test "processing modes inherit image restrictions and accept only byte context options" do
    content = svg("<image href=\"svg-resource:logo\" width=\"1\" height=\"1\"/>")
    resource = %{data: "bytes", media_type: "image/png"}
    context = %{mode: :reference, resources: %{"svg-resource:logo" => resource}}

    assert {:ok, %{context: %{external_resources?: true}}} =
             SvgValidator.validate_processing(content, context)

    for mode <- [:image, :reference, :inline] do
      assert {:error, {:invalid_document, %{source: "SVG", message: message}}} =
               SvgValidator.validate_processing(
                 content,
                 %{context | mode: mode} |> Map.put(:parent_mode, :image)
               )

      assert message =~ "not authorized"
    end

    for invalid <- [
          %{},
          %{mode: :dynamic},
          %{mode: :image, parent_mode: :dynamic},
          %{mode: :inline, resources: []},
          %{mode: :image, fonts: "font.ttf"},
          %{mode: :image, resources_dir: "/tmp"},
          %{mode: :inline, resolver: fn -> raise "called" end}
        ] do
      assert {:error, {:invalid_options, %{stage: :options, source: "SVG", operation: :prepare}}} =
               SvgBackend.rasterize(svg(""), [], invalid)
    end
  end

  test "complete input validation rejects a second document, entities, instructions and active URLs" do
    for source <- [
          "",
          "<!--comment-->",
          " ",
          svg("") <> svg(""),
          svg("") <> "visible",
          "<!DOCTYPE svg [<!ENTITY a 'x'>]>" <> svg("&a;"),
          "<?xml-stylesheet href='private.css'?>" <> svg(""),
          svg("<g>&undefined;</g>"),
          svg("<a href=\"javascript:run()\"/>"),
          svg("<image href=\"data:image/png;base64,eA==\"/>"),
          svg("<use href=\"file:///tmp/private\"/>")
        ] do
      assert {:error, {:invalid_document, %{source: "SVG", message: message}}} =
               SvgBackend.rasterize(source, [], %{mode: :image})

      assert is_binary(message)
    end

    assert {:ok, _} =
             SvgValidator.validate_processing(
               "<?xml version='1.0'?>" <> svg("") <> "\n<!--end-->",
               %{mode: :inline}
             )

    for content <- [
          "<animate attributeName='x'/>",
          "<set attributeName='fill'/>",
          "<g onclick='run()'/>",
          "<script/>"
        ] do
      assert {:error, {:unsupported_html, %{source: "SVG", line: 1}}} =
               SvgBackend.rasterize(svg(content), [], %{mode: :image})
    end

    assert {:error, {:invalid_document, _}} =
             SvgValidator.validate_processing(nil, %{mode: :image})
  end

  test "mapped active URLs and XML processing instructions fail before backend resolution" do
    for href <- [
          "javascript:run()",
          "vbscript:run()",
          "https://example.invalid/image.png",
          "file:///tmp/private",
          "data:image/png;base64,eA==",
          "//example.invalid/image.png"
        ] do
      context = %{
        mode: :reference,
        resources: %{href => %{data: "never decoded", media_type: "image/png"}}
      }

      assert {:error, {:invalid_document, %{source: "SVG", message: message}}} =
               SvgBackend.rasterize(svg("<image href='#{href}'/>"), [], context)

      assert message =~ "active URLs"
    end

    assert {:error, {:invalid_document, %{message: message}}} =
             SvgBackend.rasterize(svg("<?instruction data?>"), [], %{mode: :image})

    assert message =~ "processing instructions"
  end

  test "namespace integration validates flow and phrasing content while incomplete scenes diagnose" do
    html = "xmlns=\"http://www.w3.org/1999/xhtml\""

    for content <- [
          "<foreignObject><div #{html}><p>Flow</p></div></foreignObject>",
          "<title><span #{html}>Title</span></title>"
        ] do
      assert {:ok, _} = SvgValidator.validate_processing(svg(content), %{mode: :inline})

      assert {:error, {:unsupported_html, %{stage: :layout, message: message}}} =
               SvgBackend.rasterize(svg(content), [], %{mode: :inline})

      assert message =~ "complete scenes remain pending"
    end

    for content <- [
          "<g><div #{html}/></g>",
          "<title><div #{html}/></title>",
          "<foreignObject><div #{html}><p><div/></p></div></foreignObject>",
          "<g xmlns='urn:unknown'/>",
          "<foreignObject><div #{html}><g xmlns='http://www.w3.org/2000/svg'/></div></foreignObject>"
        ] do
      assert {:error, {:invalid_document, %{source: "SVG", line: 1}}} =
               SvgValidator.validate_processing(svg(content), %{mode: :inline})
    end

    assert {:error, {:unsupported_html, %{message: message, operation: :rasterize}}} =
             SvgBackend.rasterize(svg("<foreignObject/>"), [], %{mode: :image})

    assert message =~ "usvg 0.47.0 has no foreignObject"
    assert {:ok, pdf} = HtmlToPdf.render("<svg></svg>")
    refute pdf =~ "/Subtype /Image"
  end

  test "internal reference graph bounds cycles, missing targets, duplicate IDs and repeated expansion",
       %{limits: old} do
    for content <- [
          "<use href='#missing'/>",
          "<g id='a'><use href='#a'/></g>",
          "<defs><use id='a' href='#b'/><use id='b' href='#a'/></defs>",
          "<rect id='a'/><circle id='a'/>"
        ] do
      assert {:error, {:invalid_document, %{source: "SVG", message: message}}} =
               SvgValidator.validate_processing(svg(content), %{mode: :image})

      assert is_binary(message)
    end

    content = svg("<defs><g id='a'><rect/><circle/></g></defs><use href='#a'/><use href='#a'/>")
    assert {:ok, _} = SvgValidator.validate_processing(content, %{mode: :image})
    Limits.install(%{old | max_svg_expansion_work: 8})

    assert {:error, {:resource_limit_exceeded, %{message: message}}} =
             SvgValidator.validate_processing(content, %{mode: :image})

    assert message =~ "max_svg_expansion_work"
    assert {:ok, _} = SvgValidator.validate_processing(svg(""), %{mode: :image})
    Limits.install(%{old | max_svg_depth: 4})

    assert {:error, {:invalid_document, %{message: message}}} =
             SvgValidator.validate_processing(
               svg("<defs><g id='a'><rect/></g></defs><g><g><use href='#a'/></g></g>"),
               %{mode: :image}
             )

    assert message =~ "max_svg_depth"
  end

  test "reused path geometry charges expanded bytes rather than only authored source", %{
    limits: old
  } do
    source = svg("<defs><path id='p' d='M0 0 L1 1'/></defs><use href='#p'/><use href='#p'/>")
    Limits.install(%{old | max_svg_expanded_path_bytes: 20})

    assert {:error, {:resource_limit_exceeded, %{message: message, source: "SVG"}}} =
             SvgValidator.validate_processing(source, %{mode: :image})

    assert message =~ "max_svg_expanded_path_bytes"
    Limits.install(%{old | max_svg_expanded_path_bytes: 30})
    assert {:ok, _} = SvgValidator.validate_processing(source, %{mode: :image})

    assert {:error, {:resource_limit_exceeded, %{message: message}}} =
             HtmlValidator.with_render_budget(fn ->
               assert {:ok, _} = SvgValidator.validate_processing(source, %{mode: :image})
               SvgValidator.validate_processing(source, %{mode: :image})
             end)

    assert message =~ "max_svg_expanded_path_bytes"
  end

  test "SVG work is cumulative across nested scopes and cleans up after exhaustion", %{
    limits: old
  } do
    for {key, content, limit} <- [
          {:max_aggregate_svg_source_bytes, "", byte_size(svg(""))},
          {:max_aggregate_svg_nodes, "<rect/>", 2},
          {:max_aggregate_svg_path_bytes, "<path d='M0 0'/>", 4},
          {:max_aggregate_svg_filter_primitives, "<filter><feFlood/></filter>", 1},
          {:max_aggregate_svg_references, "<rect id='a'/><use href='#a'/>", 1},
          {:max_svg_expansion_work, "", 1}
        ] do
      Limits.install(Map.put(old, key, limit))

      result =
        HtmlValidator.with_render_budget(fn ->
          assert {:ok, _} = SvgValidator.validate_processing(svg(content), %{mode: :image})

          HtmlValidator.with_render_budget(fn ->
            SvgValidator.validate_processing(svg(content), %{mode: :image})
          end)
        end)

      assert {:error, {:resource_limit_exceeded, %{message: message}}} = result
      assert message =~ Atom.to_string(key)

      assert {:ok, _} =
               HtmlValidator.with_render_budget(fn ->
                 SvgValidator.validate_processing(svg(content), %{mode: :image})
               end)
    end
  end

  test "raster work and font sources retain cumulative budgets before native allocation", %{
    limits: old
  } do
    Limits.install(%{old | max_aggregate_svg_raster_pixels: 1})

    assert {:error, {:resource_limit_exceeded, %{message: message}}} =
             HtmlValidator.with_render_budget(fn ->
               assert {:ok, _} = SvgBackend.rasterize(svg("<rect/>"), [], %{mode: :image})
               SvgBackend.rasterize(svg("<rect/>"), [], %{mode: :image})
             end)

    assert message =~ "max_aggregate_svg_raster_pixels"
    data = File.read!("priv/fonts/dejavu/DejaVuSans.ttf")
    Limits.install(%{old | max_font_count: 1})

    assert {:error, {:resource_limit_exceeded, _}} =
             SvgBackend.rasterize(svg(""), [], %{
               mode: :image,
               fonts: [%{data: data}, %{data: data}]
             })

    Limits.install(%{old | max_svg_font_work_bytes: byte_size(data)})

    assert {:error, {:resource_limit_exceeded, %{message: message}}} =
             HtmlValidator.with_render_budget(fn ->
               assert {:ok, _} =
                        SvgBackend.rasterize(svg(""), [], %{mode: :image, fonts: [%{data: data}]})

               SvgBackend.rasterize(svg(""), [], %{mode: :image, fonts: [%{data: data}]})
             end)

    assert message =~ "max_svg_font_work_bytes"
    Limits.install(%{old | max_svg_font_work_bytes: 1})

    assert {:error, {:resource_limit_exceeded, %{source: "SVG", operation: :prepare}}} =
             SvgBackendValidator.prepare(
               svg(""),
               [],
               %{mode: :image, fonts: [%{data: data}]},
               nil
             )

    Limits.install(%{old | max_font_source_bytes: 1})

    assert {:error, {:resource_limit_exceeded, %{stage: :font}}} =
             SvgBackend.rasterize(svg(""), [], %{mode: :image, fonts: [%{data: data}]})
  end

  test "byte adapter dimension failures use the shared asset validation contract" do
    for source <- [
          "<svg xmlns='http://www.w3.org/2000/svg' width='0%' height='1' viewBox='0 0 1 1'/>",
          "<svg xmlns='http://www.w3.org/2000/svg' width='50%' height='1'/>"
        ] do
      assert {:error,
              {:invalid_document,
               %{stage: :style, source: "SVG", operation: :rasterize, message: message}}} =
               SvgBackend.rasterize(source, [], %{mode: :image})

      assert message =~ "intrinsic dimensions"
    end
  end

  test "byte backend paints supplied PNG pixels and changed dependency bytes" do
    source = svg("<image href='svg-resource:logo' width='1' height='1'/>")

    for rgb <- [[255, 0, 0], [0, 0, 255]] do
      png = PngFixture.build(1, 1, 2, 8, 0, fn _, _ -> rgb end)

      context = %{
        mode: :reference,
        resources: %{"svg-resource:logo" => %{data: png, media_type: "image/png"}}
      }

      assert {:ok, output} = SvgBackend.rasterize(source, [], context)
      assert {:ok, image} = PngDecoder.decode(output, HtmlValidator.new_image_budget(), false)
      assert image.width_px == 1 and image.height_px == 1
      assert image.data == :erlang.list_to_binary(rgb)
      assert image[:alpha_data] == nil
    end

    assert {:ok, output} =
             SvgBackend.rasterize(svg("<rect width='1' height='1' fill='red'/>"), [], %{
               mode: :image
             })

    assert {:ok, %{data: <<255, 0, 0>>}} =
             PngDecoder.decode(output, HtmlValidator.new_image_budget(), false)
  end

  test "byte backend validates every resource descriptor and decoded allocation before native access",
       %{limits: old} do
    png = PngFixture.build(1, 1, 2, 8, 0, fn _, _ -> [255, 0, 0] end)
    source = svg("<image href='svg-resource:logo' width='1' height='1'/>")

    for descriptor <- [
          "/tmp/private.png",
          %{path: "/tmp/private.png"},
          %{data: "invalid", media_type: "image/png"},
          %{data: png, media_type: "image/svg+xml"}
        ] do
      assert {:error, {_reason, %{stage: stage, message: message}}} =
               SvgBackend.rasterize(source, [], %{
                 mode: :reference,
                 resources: %{"svg-resource:logo" => descriptor}
               })

      assert stage in [:style, :limits] and is_binary(message)
    end

    assert {:error, {:invalid_document, %{message: message}}} =
             SvgBackend.rasterize(svg(""), [], %{
               mode: :reference,
               resources: %{"file:///tmp/private" => %{data: png, media_type: "image/png"}}
             })

    assert message =~ "opaque"

    for key <- [
          :max_svg_resource_bytes,
          :max_aggregate_svg_resource_bytes,
          :max_decoded_image_bytes
        ] do
      Limits.install(Map.put(old, key, 1))

      assert {:error, {:resource_limit_exceeded, _}} =
               SvgBackend.rasterize(source, [], %{
                 mode: :reference,
                 resources: %{"svg-resource:logo" => %{data: png, media_type: "image/png"}}
               })
    end

    Limits.install(old)

    assert {:error, {:invalid_document, _}} =
             SvgBackend.rasterize(svg(""), [zoom: 2], %{mode: :image})
  end

  test "selected font bytes paint text while missing fonts and glyphs are diagnosed" do
    source =
      "<svg xmlns='http://www.w3.org/2000/svg' width='80' height='20'><text x='0' y='16' font-family='DejaVu Sans' font-size='16'>SVG</text></svg>"

    data = File.read!("priv/fonts/dejavu/DejaVuSans.ttf")

    assert {:ok, output} =
             SvgBackend.rasterize(source, [], %{mode: :image, fonts: [%{data: data}]})

    assert {:ok, image} = PngDecoder.decode(output, HtmlValidator.new_image_budget(), false)
    assert image.alpha_data != :binary.copy(<<0>>, 80 * 20)

    assert {:error, {:unsupported_html, %{message: message}}} =
             SvgBackend.rasterize(source, [], %{mode: :image})

    assert message =~ "font bytes"

    for font <- ["font.ttf", %{data: "invalid"}] do
      assert {:error, {:invalid_document, %{stage: :font, source: "SVG"}}} =
               SvgBackend.rasterize(source, [], %{mode: :image, fonts: [font]})
    end

    assert {:error, {:unsupported_html, %{message: message}}} =
             SvgBackend.rasterize(
               String.replace(source, "SVG</text>", "\u{10FFFF}</text>"),
               [],
               %{mode: :image, fonts: [%{data: data}]}
             )

    assert message =~ "cover all text"
  end

  test "a host-readable font that the backend cannot import fails instead of omitting text" do
    data = File.read!("priv/fonts/dejavu/DejaVuSans.ttf")
    <<_scaler::32, count::16, _rest::binary>> = data

    records =
      for <<record::binary-size(16) <- binary_part(data, 12, count * 16)>> do
        if binary_part(record, 0, 4) == "name",
          do: "zzzz" <> binary_part(record, 4, 12),
          else: record
      end

    # Preserve sorted table records and offsets while removing the embedded
    # family table. The shared host glyph parser can still read these bytes.
    font =
      IO.iodata_to_binary([
        binary_part(data, 0, 12),
        Enum.sort(records),
        binary_part(data, 12 + count * 16, byte_size(data) - 12 - count * 16)
      ])

    assert {:ok, _} = NativeElixirPdfUtilities.Validators.FontValidator.prepare_ttf(font)

    assert {:error,
            {:invalid_document,
             %{
               stage: :style,
               reason: :invalid_document,
               source: "SVG",
               operation: :rasterize,
               message: message
             }}} =
             SvgBackend.rasterize(svg("<text x='0' y='1'>A</text>"), [], %{
               mode: :image,
               fonts: [%{data: font}]
             })

    assert message =~ "selected SVG font bytes must import exactly one named static TrueType face"
  end

  test "font parser diagnostics retain their limit explanation", %{limits: old} do
    data = File.read!("priv/fonts/dejavu/DejaVuSans.ttf")
    Limits.install(%{old | max_font_cmap_work: 1})

    assert {:error, {:resource_limit_exceeded, %{message: message}}} =
             SvgBackend.rasterize(svg(""), [], %{mode: :image, fonts: [%{data: data}]})

    assert message =~ "character-map"
  end

  test "backend omissions fail instead of returning successful blank images" do
    for content <- [
          "<filter/>",
          "<mask/>",
          "<pattern/>",
          "<marker/>",
          "<clipPath/>",
          "<style>rect { opacity: 0.5 }</style>",
          "<unsupported/>",
          "<image/>"
        ] do
      assert {:error, {_reason, %{source: "SVG"}}} =
               SvgBackend.rasterize(svg(content), [], %{mode: :reference})
    end

    assert {:error, {:invalid_document, %{message: message}}} =
             SvgBackend.rasterize(
               svg("<path xmlns:a='urn:x' xmlns:b='urn:x' a:d='x' b:d='y'/>"),
               [],
               %{mode: :image}
             )

    assert message =~ "unique expanded namespace/name"

    resource = %{
      data: PngFixture.build(1, 1, 2, 8, 0, fn _, _ -> [255, 0, 0] end),
      media_type: "image/png"
    }

    context = %{mode: :reference, resources: %{"svg-resource:logo" => resource}}

    for content <- [
          "<image href='svg-resource:logo'/><image/>",
          "<rect id='target'/><image href='#target'/>",
          "<use href='svg-resource:logo'/>",
          "<textPath href='svg-resource:logo'/>"
        ] do
      assert {:error, {:unsupported_html, %{source: "SVG"}}} =
               SvgBackend.rasterize(svg(content), [], context)
    end

    assert {:ok, _} = SvgBackendValidator.prepare(svg("<rect/>"), [], %{mode: :image}, nil)
  end

  test "native resolver has no implicit file, network, data URL or system-font fallback" do
    assert {:module, SvgBackend.NativeBytes} = SvgBackend.load_native()
    # Deliberately bypass Elixir input policy to verify the native isolation boundary itself.
    png = PngFixture.build(1, 1, 2, 8, 0, fn _, _ -> [255, 0, 0] end)
    path = Path.join(System.tmp_dir!(), "svg-isolation-#{System.unique_integer([:positive])}.png")
    File.write!(path, png)
    on_exit(fn -> File.rm(path) end)

    for href <- [
          path,
          "file://#{path}",
          "https://example.invalid/image.png",
          "data:image/png;base64,#{Base.encode64(png)}"
        ] do
      assert {:ok, output} =
               SvgBackend.NativeBytes.render(
                 svg("<image href='#{href}' width='1' height='1'/>"),
                 1,
                 1,
                 [],
                 %{}
               )

      assert {:ok, %{alpha_data: <<0>>}} =
               PngDecoder.decode(output, HtmlValidator.new_image_budget(), false)
    end

    assert {:ok, output} =
             SvgBackend.NativeBytes.render(
               svg("<text x='0' y='1' font-family='DejaVu Sans'>X</text>"),
               1,
               1,
               [],
               %{}
             )

    assert {:ok, %{alpha_data: <<0>>}} =
             PngDecoder.decode(output, HtmlValidator.new_image_budget(), false)

    assert {:error, message} =
             SvgBackend.NativeBytes.render("<!DOCTYPE svg>" <> svg(""), 1, 1, [], %{})

    assert is_binary(message)
  end

  test "expanded text and PNG occurrences charge work before native rendering", %{limits: old} do
    text_source =
      svg("<defs><text id='label'>ABCD</text></defs><use href='#label'/><use href='#label'/>")

    assert {:ok, %{expanded_work: 19, expansion_counts: %{3 => 3}}} =
             SvgValidator.validate_processing(text_source, %{mode: :image})

    Limits.install(%{old | max_svg_expansion_work: 18})

    assert {:error, {:resource_limit_exceeded, %{message: message}}} =
             SvgValidator.validate_processing(text_source, %{mode: :image})

    assert message =~ "max_svg_expansion_work"

    data = PngFixture.build(2, 1, 2, 8, 0, fn _, _ -> [255, 0, 0] end)

    context = %{
      mode: :reference,
      resources: %{"svg-resource:logo" => %{data: data, media_type: "image/png"}}
    }

    source =
      svg(
        "<defs><image id='asset' href='svg-resource:logo' width='1' height='1'/></defs><use href='#asset'/><use href='#asset'/>"
      )

    Limits.install(%{old | max_aggregate_svg_raster_pixels: 5})

    assert {:error, {:resource_limit_exceeded, %{source: "SVG", message: message}}} =
             SvgBackend.rasterize(source, [], context)

    assert message =~ "max_aggregate_svg_raster_pixels"

    Limits.install(%{old | max_aggregate_decoded_image_bytes: 20})

    assert {:error, {:resource_limit_exceeded, %{source: "SVG", message: message}}} =
             SvgBackend.rasterize(source, [], context)

    assert message =~ "aggregate decoded image bytes"
    Limits.install(old)
    assert {:ok, _} = SvgBackend.rasterize(source, [], context)
  end

  test "temporary surface work is bounded cumulatively before backend allocation", %{limits: old} do
    source = svg("<g opacity='0.5'><rect width='1' height='1'/></g>")
    Limits.install(%{old | max_aggregate_svg_surface_pixels: 224})

    assert {:error,
            {:resource_limit_exceeded, %{source: "SVG", stage: :layout, message: message}}} =
             SvgBackend.rasterize(source, [], %{mode: :image})

    assert message =~ "max_aggregate_svg_surface_pixels"
    Limits.install(%{old | max_aggregate_svg_surface_pixels: 225})
    assert {:ok, _} = SvgBackend.rasterize(source, [], %{mode: :image})

    HtmlValidator.with_render_budget(fn ->
      assert {:ok, _} = SvgBackend.rasterize(svg("<rect/>"), [], %{mode: :image})

      assert {:error, {:resource_limit_exceeded, %{source: "SVG", message: message}}} =
               SvgBackend.rasterize(svg("<rect/>"), [], %{mode: :image})

      assert message =~ "max_aggregate_svg_surface_pixels"
    end)
  end

  test "missing native artifact diagnostics identify the reproducible build step" do
    assert {:error,
            {:svg_backend_unavailable, %{source: "SVG", operation: :rasterize, message: message}}} =
             SvgBackendValidator.native_load_result({:error, :badfile})

    assert message =~ "native/svg_backend" and message =~ "Cargo"
  end

  @tag :browser_parity
  test "byte adapter artwork matches Chromium without enabling inline SVG composition" do
    fixture = Path.expand("fixtures/html_to_pdf/svg_backend/byte_artwork.html", __DIR__)
    source = File.read!(Path.join(Path.dirname(fixture), "byte_artwork.svg"))
    image = PngFixture.build(2, 2, 2, 8, 0, fn _, _ -> [22, 163, 74] end)
    image_uri = "data:image/png;base64," <> Base.encode64(image)
    font = File.read!("priv/fonts/dejavu/DejaVuSans.ttf")
    font_uri = "data:font/ttf;base64," <> Base.encode64(font)

    font_style =
      "<style>@font-face { font-family: 'DejaVu Sans'; src: url('#{font_uri}') format('truetype'); }</style>"

    reference_svg =
      source
      |> String.replace("svg-resource:logo", image_uri)
      |> String.replace("viewBox=\"0 0 240 80\">", "viewBox=\"0 0 240 80\">" <> font_style)

    reference_uri = "data:image/svg+xml;base64," <> Base.encode64(reference_svg)

    context = %{
      mode: :reference,
      fonts: [%{data: font}],
      resources: %{"svg-resource:logo" => %{data: image, media_type: "image/png"}}
    }

    stats =
      NativeElixirPdfUtilities.TestSupport.PdfVisualCompare.assert_browser_match!(fixture,
        artifact_dir: "tmp/browser_parity/svg_backend_byte_artwork",
        chromium_transform: fn html ->
          String.replace(html, "SVG_BACKEND_ARTWORK", reference_uri)
        end,
        native_renderer: fn html, options ->
          with {:ok, artwork} <- SvgBackend.rasterize(source, [width: 480, height: 160], context) do
            assert {:ok, %{width_px: 480, height_px: 160}} =
                     PngDecoder.decode(artwork, HtmlValidator.new_image_budget(), false)

            uri = "data:image/png;base64," <> Base.encode64(artwork)
            HtmlToPdf.render(String.replace(html, "SVG_BACKEND_ARTWORK", uri), options)
          end
        end,
        max_changed_ratio: 0.01,
        max_average_delta: 0.01
      )

    assert stats.page_count == 1

    for name <- ["native.pdf", "chromium.pdf"] do
      pdf = File.read!(Path.join(stats.artifact_dir, name))
      assert {:ok, text} = NativeElixirPdfUtilities.Text.extract(pdf)
      assert text =~ "SVG byte adapter" and text =~ "Validated resources"
      assert {:ok, []} = NativeElixirPdfUtilities.Forms.fields(pdf)
    end
  end

  defp svg(content) do
    "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"1\" height=\"1\">#{content}</svg>"
  end
end
