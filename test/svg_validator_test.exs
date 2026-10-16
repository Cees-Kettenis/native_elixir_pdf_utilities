Code.require_file("support/png_fixture.ex", __DIR__)

defmodule NativeElixirPdfUtilities.SvgValidatorTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.Limits
  alias NativeElixirPdfUtilities.Validators.SvgValidator
  alias NativeElixirPdfUtilities.HtmlToPdf.{SvgRasterizer, PngDecoder}
  alias NativeElixirPdfUtilities.Validators.{HtmlValidator, SvgConverterValidator, FontValidator}
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

    for options <- [[1], [zoom: 2], :invalid, [scale: 0], [scale: -1], [scale: 1.5], [scale: nil]] do
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
               convert_svg(svg(""), [], invalid)
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
          svg("<image href=\"data:image/png;base64,eA==\"/>"),
          svg("<use href=\"file:///tmp/private\"/>")
        ] do
      assert {:error, {:invalid_document, %{source: "SVG", message: message}}} =
               convert_svg(source, [], %{mode: :image})

      assert is_binary(message)
    end

    assert {:error,
            {:unsupported_html,
             %{module: HtmlValidator, source: "href", stage: :html, line: 1, message: message}}} =
             convert_svg(svg("<a href=\"javascript:run()\"/>"), [], %{mode: :image})

    assert message =~ "Active URL schemes"

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
               convert_svg(svg(content), [], %{mode: :image})
    end

    assert {:error, {:invalid_document, _}} =
             SvgValidator.validate_processing(nil, %{mode: :image})
  end

  test "undefined XML entities remain invalid converter input" do
    assert {:error, {:invalid_document, %{source: "SVG", message: message}}} =
             convert_svg(svg("<g>&undefined;</g>"), [], %{mode: :image})

    assert is_binary(message)
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
               convert_svg(svg("<image href='#{href}'/>"), [], context)

      assert message =~ "active URLs"
    end

    assert {:error, {:invalid_document, %{message: message}}} =
             convert_svg(svg("<?instruction data?>"), [], %{mode: :image})

    assert message =~ "processing instructions"
  end

  test "namespace integration validates flow and phrasing content while incomplete scenes diagnose" do
    html = "xmlns=\"http://www.w3.org/1999/xhtml\""

    for content <- [
          "<foreignObject><div #{html}><p>Flow</p></div></foreignObject>",
          "<title><span #{html}>Title</span></title>"
        ] do
      assert {:ok, _} = SvgValidator.validate_processing(svg(content), %{mode: :inline})

      if String.starts_with?(content, "<foreignObject>") do
        assert {:error,
                {:unsupported_html,
                 %{stage: :layout, source: "foreignObject", operation: :convert, message: message}}} =
                 convert_svg(svg(content), [], %{mode: :inline})

        assert message =~ "existing SVG converter does not render foreignObject"
      else
        assert {:ok, png} = convert_svg(svg(content), [], %{mode: :inline})
        assert {:ok, %{alpha_data: <<0>>}} = PngDecoder.decode(png, nil, false)
      end
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

    assert {:error,
            {:unsupported_html, %{message: message, operation: :convert, source: "foreignObject"}}} =
             convert_svg(svg("<foreignObject/>"), [], %{mode: :image})

    assert message =~ "existing SVG converter does not render foreignObject"
    assert {:ok, pdf} = HtmlToPdf.render("<svg></svg>")
    assert pdf =~ "/Subtype /Image"
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
               assert {:ok, _} = convert_svg(svg("<rect/>"), [], %{mode: :image})
               convert_svg(svg("<rect/>"), [], %{mode: :image})
             end)

    assert message =~ "max_aggregate_svg_raster_pixels"
    data = File.read!("priv/fonts/dejavu/DejaVuSans.ttf")
    Limits.install(%{old | max_font_count: 1})

    assert {:error, {:resource_limit_exceeded, _}} =
             convert_svg(svg(""), [], %{
               mode: :image,
               fonts: [%{data: data}, %{data: data}]
             })

    Limits.install(%{old | max_svg_font_work_bytes: byte_size(data)})

    assert {:error, {:resource_limit_exceeded, %{message: message}}} =
             HtmlValidator.with_render_budget(fn ->
               assert {:ok, _} =
                        convert_svg(svg(""), [], %{mode: :image, fonts: [%{data: data}]})

               convert_svg(svg(""), [], %{mode: :image, fonts: [%{data: data}]})
             end)

    assert message =~ "max_svg_font_work_bytes"
    Limits.install(%{old | max_svg_font_work_bytes: 1})

    assert {:error,
            {:resource_limit_exceeded, %{stage: :font, module: HtmlValidator, message: message}}} =
             SvgConverterValidator.prepare(
               svg(""),
               [],
               nil,
               [%{data: data}],
               %{mode: :image}
             )

    assert message =~ "max_svg_font_work_bytes"
    Limits.install(%{old | max_font_source_bytes: 1})

    assert {:error, {:resource_limit_exceeded, %{stage: :font}}} =
             convert_svg(svg(""), [], %{mode: :image, fonts: [%{data: data}]})
  end

  test "byte adapter dimension failures use the shared asset validation contract" do
    for source <- [
          "<svg xmlns='http://www.w3.org/2000/svg' width='0%' height='1' viewBox='0 0 1 1'/>",
          "<svg xmlns='http://www.w3.org/2000/svg' width='50%' height='1'/>"
        ] do
      assert {:error, {:invalid_document, %{stage: :style, source: "SVG", message: message}}} =
               convert_svg(source, [], %{mode: :image})

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

      assert {:ok, output} = convert_svg(source, [], context)
      assert {:ok, image} = PngDecoder.decode(output, HtmlValidator.new_image_budget(), false)
      assert image.width_px == 1 and image.height_px == 1
      assert image.data == :erlang.list_to_binary(rgb)
      assert image[:alpha_data] == nil
    end

    assert {:ok, output} =
             convert_svg(svg("<rect width='1' height='1' fill='red'/>"), [], %{
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
               convert_svg(source, [], %{
                 mode: :reference,
                 resources: %{"svg-resource:logo" => descriptor}
               })

      assert stage in [:style, :limits] and is_binary(message)
    end

    assert {:error, {:invalid_document, %{message: message}}} =
             convert_svg(svg(""), [], %{
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
               convert_svg(source, [], %{
                 mode: :reference,
                 resources: %{"svg-resource:logo" => %{data: png, media_type: "image/png"}}
               })
    end

    Limits.install(old)

    assert {:error, {:invalid_document, _}} =
             convert_svg(svg(""), [zoom: 2], %{mode: :image})
  end

  test "selected font bytes paint text while missing fonts and glyphs are diagnosed" do
    source =
      "<svg xmlns='http://www.w3.org/2000/svg' width='80' height='20'><text x='0' y='16' font-family='DejaVu Sans' font-size='16'>SVG</text></svg>"

    data = File.read!("priv/fonts/dejavu/DejaVuSans.ttf")
    assert {:ok, "DejaVu Sans"} = FontValidator.prepare_font_family_name(data)

    assert {:ok, output} =
             convert_svg(source, [], %{mode: :image, fonts: [%{data: data}]})

    assert {:ok, image} = PngDecoder.decode(output, HtmlValidator.new_image_budget(), false)
    assert image.alpha_data != :binary.copy(<<0>>, 80 * 20)

    assert {:error, {:unsupported_html, %{message: message}}} =
             convert_svg(source, [], %{mode: :image})

    assert message =~ "font bytes"

    for font <- ["font.ttf", %{data: "invalid"}] do
      assert {:error,
              {:invalid_document,
               %{stage: :style, source: "SVG", module: SvgConverterValidator, operation: :convert}}} =
               convert_svg(source, [], %{mode: :image, fonts: [font]})
    end

    assert {:error, {:unsupported_html, %{message: message}}} =
             convert_svg(
               String.replace(source, "SVG</text>", "\u{10FFFF}</text>"),
               [],
               %{mode: :image, fonts: [%{data: data}]}
             )

    assert message =~ "do not cover the original text"
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
            {:invalid_font,
             %{
               stage: :font,
               reason: :invalid_font,
               module: FontValidator,
               operation: :prepare_font_family_name,
               message: message
             }}} =
             convert_svg(svg("<text x='0' y='1'>A</text>"), [], %{
               mode: :image,
               fonts: [%{data: font}]
             })

    assert message =~ "usable internal family name"
  end

  test "font parser diagnostics retain their limit explanation", %{limits: old} do
    data = File.read!("priv/fonts/dejavu/DejaVuSans.ttf")
    Limits.install(%{old | max_font_cmap_work: 1})

    assert {:error, {:resource_limit_exceeded, %{message: message}}} =
             convert_svg(svg(""), [], %{mode: :image, fonts: [%{data: data}]})

    assert message =~ "character-map"
  end

  test "unused empty marker has no ink while active missing paint and font resources still fail" do
    assert {:ok, png} = convert_svg(svg("<marker/>"), [], %{mode: :reference})
    assert {:ok, image} = PngDecoder.decode(png, HtmlValidator.new_image_budget(), false)
    assert image.alpha_data == <<0>>

    assert {:ok, painted} =
             convert_svg(
               svg(
                 "<defs><marker id='m' markerWidth='1' markerHeight='1' refX='0' refY='0'><rect width='1' height='1' fill='red'/></marker></defs><path d='M0 0L1 1' stroke='red' marker-end='url(#m)'/>"
               ),
               [],
               %{mode: :reference}
             )

    assert {:ok, marker_image} =
             PngDecoder.decode(painted, HtmlValidator.new_image_budget(), false)

    # Chromium paints the single 1 CSS px canvas fully red (verified with
    # /usr/bin/chromium print-to-pdf at 96 dpi): the 1-unit diagonal stroke
    # covers the only pixel while the fitted 1x1 marker viewport moves its
    # rect outside the canvas with the reference offset. resvg drops width-1
    # strokes when the canvas is at most 2 device pixels, so the native
    # converter retains the genuine empty 1x1 paint here.
    assert marker_image.data == <<0, 0, 0>>
    assert marker_image.alpha_data == <<0>>

    for content <- ["<text>Missing face</text>", "<image href='missing.png'/>"] do
      assert {:error, {reason, diagnostic}} =
               convert_svg(svg(content), [], %{mode: :reference})

      # Text diagnostics attribute themselves to the "SVG text" subtree.
      assert is_atom(reason) and String.starts_with?(diagnostic.source, "SVG")
      assert is_binary(diagnostic.message) and is_atom(diagnostic.stage)
    end
  end

  test "unused SVG definitions and stylesheet metadata produce no invented ink" do
    for content <- [
          "<filter/>",
          "<mask/>",
          "<pattern/>",
          "<clipPath/>",
          "<style>rect { opacity: 0.5 }</style>"
        ] do
      assert {:ok, png} = convert_svg(svg(content), [], %{mode: :reference})
      assert {:ok, %{alpha_data: <<0>>}} = PngDecoder.decode(png, nil, false)
    end

    assert {:error, {:invalid_document, %{message: message}}} =
             convert_svg(
               svg("<path xmlns:a='urn:x' xmlns:b='urn:x' a:d='x' b:d='y'/>"),
               [],
               %{mode: :image}
             )

    assert message =~ "unique expanded namespace/name"
    assert {:ok, _} = SvgConverterValidator.prepare(svg("<rect/>"), [], nil, [], %{mode: :image})
  end

  test "unsupported SVG elements cannot silently discard original source" do
    assert {:error, {:unsupported_html, diagnostic}} =
             convert_svg(svg("<unsupported/>"), [], %{mode: :reference})

    assert diagnostic.source == "unsupported"
    assert diagnostic.stage == :layout and diagnostic.reason == :unsupported_html
    assert diagnostic.message =~ "cannot be silently omitted"
  end

  test "SVG image elements require an explicit supported asset reference" do
    assert {:error, {:invalid_document, diagnostic}} =
             convert_svg(svg("<image/>"), [], %{mode: :reference})

    assert diagnostic.source == "image href" and diagnostic.stage == :style
    assert diagnostic.reason == :invalid_document
    assert diagnostic.message =~ "explicit"
  end

  test "a valid SVG image does not authorize a second missing or internal image resource" do
    resource = %{
      data: PngFixture.build(1, 1, 2, 8, 0, fn _, _ -> [255, 0, 0] end),
      media_type: "image/png"
    }

    context = %{mode: :reference, resources: %{"svg-resource:logo" => resource}}

    for content <- [
          "<image href='svg-resource:logo'/><image/>",
          "<rect id='target'/><image href='#target'/>"
        ] do
      assert {:error, {:invalid_document, diagnostic}} =
               convert_svg(svg(content), [], context)

      assert diagnostic.source == "image href" and diagnostic.reason == :invalid_document
      assert diagnostic.stage == :style and diagnostic.message =~ "explicit"
    end
  end

  test "SVG use and textPath cannot consume an image byte descriptor" do
    resource = %{
      data: PngFixture.build(1, 1, 2, 8, 0, fn _, _ -> [255, 0, 0] end),
      media_type: "image/png"
    }

    context = %{mode: :reference, resources: %{"svg-resource:logo" => resource}}

    for content <- ["<use href='svg-resource:logo'/>", "<textPath href='svg-resource:logo'/>"] do
      assert {:error, {:invalid_document, diagnostic}} =
               convert_svg(svg(content), [], context)

      assert diagnostic.source == "SVG" and diagnostic.stage == :style
      assert diagnostic.reason == :invalid_document and diagnostic.line == 1
      assert diagnostic.message =~ "not authorized"
    end
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
             convert_svg(source, [], context)

    assert message =~ "max_aggregate_svg_raster_pixels"

    Limits.install(%{old | max_aggregate_decoded_image_bytes: 20})

    assert {:error, {:resource_limit_exceeded, %{source: "SVG", message: message}}} =
             convert_svg(source, [], context)

    assert message =~ "aggregate decoded image bytes"
    Limits.install(old)
    assert {:ok, _} = convert_svg(source, [], context)
  end

  test "actual converter canvas and cumulative surface budgets retain their bounds", %{
    limits: old
  } do
    source = svg("<g opacity='0.5'><rect width='1' height='1'/></g>")
    Limits.install(%{old | max_aggregate_svg_surface_pixels: 1})

    assert {:error, {:resource_limit_exceeded, %{source: "SVG", stage: :style, message: message}}} =
             convert_svg(source, [], %{mode: :image})

    assert message =~ "max_aggregate_svg_surface_pixels"
    Limits.install(old)
    assert {:ok, png} = convert_svg(source, [], %{mode: :image})
    assert {:ok, image} = PngDecoder.decode(png, HtmlValidator.new_image_budget(), false)
    assert image.width_px == 1 and image.height_px == 1
    assert image.alpha_data == <<128>>

    Limits.install(%{old | max_aggregate_svg_surface_pixels: 1})

    HtmlValidator.with_render_budget(fn ->
      assert {:ok, _} = convert_svg(svg("<rect/>"), [], %{mode: :image})

      assert {:error, {:resource_limit_exceeded, %{source: "SVG", message: message}}} =
               convert_svg(svg("<rect/>"), [], %{mode: :image})

      assert message =~ "max_aggregate_svg_surface_pixels"
    end)
  end

  # The byte context carries the same approved fonts and resources as before;
  # the existing converter adapter accepts fonts in its separate argument.
  defp convert_svg(source, options, context) do
    SvgRasterizer.rasterize(
      source,
      options,
      nil,
      Map.get(context, :fonts, []),
      Map.delete(context, :fonts)
    )
  end

  test "inline stylesheet non-scaling strokes are recorded as a converter limitation" do
    source =
      "<svg width='20' height='20'><style>rect{vector-effect:non-scaling-stroke}</style><rect width='20' height='20'/></svg>"

    assert {:ok, summary} = SvgValidator.validate_processing(source, %{mode: :image})
    assert MapSet.member?(summary.converter_features, :non_scaling_stroke)

    assert {:error, {:unsupported_html, diagnostic}} =
             SvgConverterValidator.validate_capabilities(summary)

    assert diagnostic.source == "vector-effect"
    assert diagnostic.message =~ "non-scaling strokes"
  end

  test "stretch viewport attributes keep unchanged transport for non-integer raster targets" do
    attributes = %{"width" => "10", "height" => "10"}

    assert {:ok, nil} = SvgValidator.stretch_viewport_attributes(attributes, 10.5, 10)
    assert {:ok, nil} = SvgValidator.stretch_viewport_attributes(attributes, nil, 10)
  end

  defp svg(content) do
    "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"1\" height=\"1\">#{content}</svg>"
  end
end
