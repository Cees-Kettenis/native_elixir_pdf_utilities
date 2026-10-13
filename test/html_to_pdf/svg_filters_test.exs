defmodule NativeElixirPdfUtilities.HtmlToPdf.SvgFiltersTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.{HtmlToPdf, Limits, Text}
  alias NativeElixirPdfUtilities.HtmlToPdf.SvgBackend

  setup do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    %{limits: limits}
  end

  test "static graphs paint real effected shapes and selected text at the group position" do
    html =
      File.read!(
        Path.expand("../fixtures/html_to_pdf/browser_parity/inline_svg_filters.html", __DIR__)
      )

    assert {:ok, pdf} = HtmlToPdf.render(html)
    assert pdf =~ "/Subtype /Image"
    assert pdf =~ "/SMask"
    assert pdf =~ "/ActualText"
    assert {:ok, text} = Text.extract(pdf, layout: false)
    assert text =~ "Filtered café Ω"
    assert text =~ "Plain selected text"
    assert {:ok, %{pages: [%{spans: spans}]}} = Text.extract_spans(pdf)
    effect = Enum.find(spans, &(&1.text == "Filtered café Ω"))
    assert effect.font_resource == nil
    assert is_binary(effect.image_resource)
    assert effect.end_x > effect.x
    assert effect.font_size > 0
    assert Enum.any?(spans, &(&1.text == "P" and is_binary(&1.font_resource)))
  end

  test "required static operation families produce genuine bounded image ink" do
    for operations <- [
          "<feTile/>",
          "<feTurbulence baseFrequency='.07 .08' numOctaves='2'/>",
          "<feTurbulence result='noise' baseFrequency='.05'/><feDisplacementMap in='SourceGraphic' in2='noise' scale='5'/>",
          "<feDiffuseLighting surfaceScale='2'><feDistantLight azimuth='45' elevation='45'/></feDiffuseLighting>",
          "<feSpecularLighting surfaceScale='2' specularExponent='5'><fePointLight x='30' y='10' z='60'/></feSpecularLighting>",
          "<feDiffuseLighting><feSpotLight x='50' y='30' z='80' pointsAtX='30' pointsAtY='30' pointsAtZ='0'/></feDiffuseLighting>",
          "<feFlood flood-color='blue' result='color'/><feComposite in='SourceGraphic' in2='color' operator='arithmetic' k2='.5' k3='.5'/>",
          "<feComponentTransfer><feFuncR type='table' tableValues='0 .5 1'/><feFuncG type='discrete' tableValues='1 0'/><feFuncB type='gamma' exponent='2'/></feComponentTransfer>"
        ] do
      assert {:ok, pdf} =
               SvgBackend.vectorize(svg(operations), [width: 200, height: 100], %{mode: :inline})

      assert pdf =~ "/Subtype /Image"
      assert pdf =~ "/ColorSpace /DeviceRGB"
      refute pdf =~ "/ActualText"
    end
  end

  test "invalid numeric parameters and graph input dependencies diagnose before backend normalization" do
    for primitive <- [
          "<feGaussianBlur stdDeviation='-1'/>",
          "<feOffset in='missing'/>",
          "<feBlend in='later'/><feFlood result='later'/>",
          "<feComposite operator='bad'/>",
          "<feColorMatrix values='1 2'/>",
          "<feMorphology radius='1,,2'/>",
          "<feTurbulence numOctaves='1.5'/>",
          "<feConvolveMatrix order='2' kernelMatrix='1 0 0'/>",
          "<feConvolveMatrix order='1' kernelMatrix='1' divisor='0'/>",
          "<feMerge><feOffset/></feMerge>",
          "<feComponentTransfer><feFuncR/><feFuncR/></feComponentTransfer>"
        ] do
      assert {:error,
              {:invalid_document, %{stage: :layout, message: message, operation: :prepare}}} =
               SvgBackend.vectorize(svg(primitive), [width: 200, height: 100], %{mode: :inline})

      assert message != ""
    end
  end

  test "filter temporary surfaces and parameter-dependent work are reserved before painting", %{
    limits: limits
  } do
    source = svg("<feGaussianBlur stdDeviation='2'/>")
    Limits.install(Map.put(limits, :max_aggregate_svg_surface_pixels, 100))

    assert {:error, {:resource_limit_exceeded, %{message: message}}} =
             SvgBackend.vectorize(source, [width: 200, height: 100], %{mode: :inline})

    assert message =~ "max_aggregate_svg_surface_pixels"
    Limits.install(Map.put(limits, :max_svg_filter_work, 100))

    assert {:error, {:resource_limit_exceeded, %{message: message}}} =
             SvgBackend.vectorize(source, [width: 200, height: 100], %{mode: :inline})

    assert message =~ "max_svg_filter_work"
  end

  test "empty, hidden and fully clipped effected text produces no logical surrogate" do
    for attributes <- [
          "opacity='0'",
          "visibility='hidden'",
          "display='none'",
          "transform='translate(500 0)'"
        ] do
      assert {:ok, pdf} =
               HtmlToPdf.render(
                 "<svg width='200' height='60'><defs><filter id='f'><feGaussianBlur stdDeviation='1'/></filter></defs><g filter='url(#f)' #{attributes}><text x='5' y='30' font-family='DejaVu Sans'>Hidden</text></g></svg>"
               )

      assert {:error, {:no_extractable_text, _}} = Text.extract(pdf, layout: false)
      refute pdf =~ "/ActualText"
    end
  end

  test "original HTML under an ancestor filter cannot be evaluated as an empty slot" do
    assert {:error, {:unsupported_html, %{message: message}}} =
             HtmlToPdf.render(
               "<svg><defs><filter id='f'><feGaussianBlur/></filter></defs><g filter='url(#f)'><foreignObject width='100' height='30'><div>Original HTML</div></foreignObject></g></svg>"
             )

    assert message =~ "actual issued surface"
  end

  defp svg(operations),
    do:
      "<svg width='200' height='100'><defs><filter id='f'>#{operations}</filter></defs><rect x='20' y='20' width='120' height='40' fill='red' filter='url(#f)'/></svg>"
end
