defmodule NativeElixirPdfUtilities.Validators.CssPrintConditionsTest do
  use ExUnit.Case
  alias NativeElixirPdfUtilities.Validators.{CssSupportsValidator, PrintMediaValidator}

  # Media Queries 4/5, 19 February 2026. Conditional Rules 5, 30 October 2025.
  test "print color depth and preferences are deterministic across range and nested conditions" do
    assert {:ok, context} = PrintMediaValidator.renderer_context(page_size: {200, 100})

    for query <- [
          "print and (color)",
          "(resolution)",
          "(color:8)",
          "(color:calc(8))",
          "(7 < color <= 8)",
          "(min-color:8)",
          "(monochrome:0)",
          "(color-index:0)",
          "(grid:0)",
          "(prefers-color-scheme:light)",
          "(prefers-contrast:no-preference)",
          "(prefers-reduced-motion:no-preference)",
          "(prefers-reduced-transparency:no-preference)",
          "(prefers-reduced-data:no-preference)",
          "(forced-colors:none)",
          "(inverted-colors:none)",
          "(dynamic-range:standard)",
          "(color-gamut:srgb)",
          "(overflow-block:paged)",
          "(overflow-inline:none)",
          "(update:none)",
          "(scripting:none)",
          "not (prefers-color-scheme:dark)"
        ] do
      assert PrintMediaValidator.matches?(query, context), query
    end

    for query <- [
          "(color:9)",
          "(color > 8)",
          "(monochrome)",
          "(grid)",
          "(update)",
          "(forced-colors)",
          "(prefers-color-scheme:dark)",
          "(prefers-color-scheme:calc(1))",
          "(color-gamut:p3)",
          "(prefers-reduced-motion:reduce)",
          "not (prefers-color-scheme:unknown)",
          "(min-prefers-color-scheme:light)",
          "(color:8.0)",
          "(color:-1)",
          "(resolution:invalid)",
          "(min-color:invalid)",
          "(color < 8 < 9)",
          "(unknown:value)"
        ] do
      refute PrintMediaValidator.matches?(query, context), query
    end
  end

  test "supports font capabilities agree with the native source and feature validators" do
    for query <- [
          "font-format(truetype)",
          "font-format(opentype)",
          "font-format(woff2)",
          "font-tech(features-opentype)",
          "(font-format(truetype) and selector(div:has(span)))",
          "font-tech(variations)",
          "font-tech(color-COLRv1)",
          "not font-tech(unknown)",
          "(width:calc-size(max-content, size / 2))",
          "(width:2ch)"
        ] do
      assert CssSupportsValidator.matches?(query), query
    end

    for query <- [
          "font-format(\"truetype\")",
          "font-tech(color-unknown)",
          "font-tech(features-opentype, variations)",
          "font-format()",
          "(width:calc(1pt + 1deg))"
        ] do
      refute CssSupportsValidator.matches?(query), query
    end
  end
end
