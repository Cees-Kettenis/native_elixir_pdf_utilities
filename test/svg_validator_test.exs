defmodule NativeElixirPdfUtilities.SvgValidatorTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.Limits
  alias NativeElixirPdfUtilities.Validators.SvgValidator
  alias NativeElixirPdfUtilities.HtmlToPdf.SvgRasterizer
  alias NativeElixirPdfUtilities.HtmlToPdf

  setup do
    old = Limits.effective()
    on_exit(fn -> Limits.install(old) end)
    %{limits: old}
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
               svg("<use xmlns:xlink=\"http://www.w3.org/1999/xlink\" xlink:href=\"#a\"/>"),
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

  test "returned native parser errors reach the caller with their explanation" do
    source = svg("<path xmlns:a=\"urn:x\" xmlns:b=\"urn:x\" a:d=\"x\" b:d=\"y\"/>")

    assert {:error, {:invalid_document, %{message: message}}} =
             SvgRasterizer.rasterize(source, [], nil)

    assert message =~ "SVG conversion failed:"
  end

  defp svg(content) do
    "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"1\" height=\"1\">#{content}</svg>"
  end
end
