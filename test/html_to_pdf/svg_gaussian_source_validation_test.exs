defmodule NativeElixirPdfUtilities.HtmlToPdf.SvgGaussianSourceValidationTest do
  use ExUnit.Case, async: false

  alias NativeElixirPdfUtilities.{HtmlToPdf, Limits}
  alias NativeElixirPdfUtilities.HtmlToPdf.{Font, HtmlParser, ResourceContext, Style}
  alias NativeElixirPdfUtilities.Validators.{HtmlValidator, SvgFilterValidator, SvgStyleValidator}

  setup do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    :ok
  end

  test "Gaussian source admits its default and one or two bounded nonnegative deviations" do
    for attrs <- [
          %{},
          %{"stdDeviation" => "0"},
          %{"stdDeviation" => "1"},
          %{"stdDeviation" => "0 2"},
          %{"stdDeviation" => "1, 2"},
          %{"stdDeviation" => ".5\t+2e-1"},
          %{"stdDeviation" => " -0 "}
        ] do
      assert :ok = SvgFilterValidator.validate_source_attributes("feGaussianBlur", attrs)

      assert {:ok, []} =
               SvgStyleValidator.presentation_declarations(%{
                 tag: "feGaussianBlur",
                 attributes: attrs
               })
    end

    assert :ok =
             SvgFilterValidator.validate_source_attributes("feOffset", %{"stdDeviation" => "-1"})
  end

  test "malformed negative nonfinite and overlong Gaussian parameters preserve actionable diagnostics" do
    for value <- [
          "-1",
          "1 -2",
          "-1,0",
          "NaN",
          "Infinity",
          "1e999",
          "",
          "1,",
          ",1",
          "1,,2",
          "1, ,2",
          "1 2 3",
          "1px",
          "1%",
          "1;2",
          "1 bad",
          "1\u00a02",
          nil
        ] do
      assert {:error, {:invalid_document, diagnostic}} =
               SvgFilterValidator.validate_source_attributes("feGaussianBlur", %{
                 "stdDeviation" => value
               })

      assert diagnostic.reason == :invalid_document
      assert diagnostic.stage == :layout and diagnostic.operation == :prepare
      assert diagnostic.module == SvgFilterValidator and diagnostic.source == "feGaussianBlur"
      assert diagnostic.message =~ "stdDeviation" and diagnostic.message =~ "nonnegative"
    end

    assert {:error, {:invalid_document, %{source: "SVG filter attributes"}}} =
             SvgFilterValidator.validate_source_attributes(nil, [])
  end

  test "Gaussian scalars use the existing central numeric magnitude bound" do
    Limits.install(Map.put(Limits.effective(), :max_css_numeric_magnitude, 2))

    assert :ok =
             SvgFilterValidator.validate_source_attributes("feGaussianBlur", %{
               "stdDeviation" => "2 0"
             })

    assert {:error, {:invalid_document, _}} =
             SvgFilterValidator.validate_source_attributes("feGaussianBlur", %{
               "stdDeviation" => "2.1"
             })
  end

  test "qualified lookalike attributes do not replace the actual Gaussian parameter" do
    assert {:ok, []} =
             SvgStyleValidator.presentation_declarations(%{
               tag: "feGaussianBlur",
               attributes: %{"stdDeviation" => "-1"},
               attribute_namespaces: %{"stdDeviation" => "urn:other"}
             })
  end

  test "the original foreignObject source prepares valid Gaussian input but rejects its negative parameter" do
    original =
      "<svg><defs><filter id='f'><feGaussianBlur stdDeviation='1'/></filter></defs><g filter='url(#f)'><foreignObject width='100' height='30'><p>Real HTML input</p></foreignObject></g></svg>"

    assert {:ok, dom} = HtmlParser.parse_detailed(original)
    options = Font.normalize_options([])

    ResourceContext.run([], HtmlValidator.new_image_budget(), fn _context ->
      assert {:ok, false} = HtmlValidator.prepare_style_input(dom, [], options, :prepare_svg)
      assert {:ok, _styled} = Style.compute_detailed(dom, [])

      invalid = String.replace(original, "stdDeviation='1'", "stdDeviation='-1'")
      assert {:ok, invalid_dom} = HtmlParser.parse_detailed(invalid)

      assert {:ok, false} =
               HtmlValidator.prepare_style_input(invalid_dom, [], options, :prepare_svg)

      assert {:error, {:invalid_document, diagnostic}} = Style.compute_detailed(invalid_dom, [])
      assert diagnostic.stage == :layout and diagnostic.operation == :prepare
      assert diagnostic.module == SvgFilterValidator and diagnostic.source == "feGaussianBlur"
      assert diagnostic.message =~ "stdDeviation" and diagnostic.message =~ "-1"
      assert {:ok, _styled} = Style.compute_detailed(dom, [])
    end)

    assert {:error, {:unsupported_html, diagnostic}} = HtmlToPdf.render(original)
    assert diagnostic.source == "foreignObject"
  end

  test "raw conversion uses the same unqualified Gaussian source validator" do
    source =
      "<svg width='2' height='2'><filter id='f'><feGaussianBlur stdDeviation='-1'/></filter><rect width='2' height='2' filter='url(#f)'/></svg>"

    assert {:error, {:invalid_document, diagnostic}} =
             NativeElixirPdfUtilities.HtmlToPdf.SvgRasterizer.rasterize(source, [], nil)

    assert diagnostic.module == SvgFilterValidator and diagnostic.source == "feGaussianBlur"
    assert diagnostic.stage == :layout and diagnostic.operation == :prepare
    assert diagnostic.reason == :invalid_document and diagnostic.line == 1
    assert diagnostic.message =~ "stdDeviation"

    qualified =
      String.replace(
        source,
        "stdDeviation='-1'",
        "xmlns:other='urn:other' other:stdDeviation='-1'"
      )

    assert {:ok, _} =
             NativeElixirPdfUtilities.HtmlToPdf.SvgRasterizer.rasterize(qualified, [], nil)
  end
end
