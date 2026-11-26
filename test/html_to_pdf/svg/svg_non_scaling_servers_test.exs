defmodule NativeElixirPdfUtilities.HtmlToPdf.SvgNonScalingServersTest do
  use ExUnit.Case, async: true

  alias NativeElixirPdfUtilities.HtmlToPdf
  alias NativeElixirPdfUtilities.Validators.SvgConverterValidator

  test "non-scaling strokes return a converter diagnostic for gradients and patterns" do
    transformed_gradients =
      for {gradient, server} <- [
            {"linear",
             "<linearGradient id='p' gradientUnits='userSpaceOnUse' x1='5' y1='10' x2='160' y2='90' gradientTransform='rotate(15)'><stop stop-color='red'/><stop offset='1' stop-color='blue'/></linearGradient>"},
            {"radial",
             "<radialGradient id='p' cx='.6' cy='.5' r='.65' fx='.3' fy='.4' gradientTransform='scale(1 .8)'><stop stop-color='red'/><stop offset='1' stop-color='blue'/></radialGradient>"}
          ],
          transform <- ["matrix(1.2 .2 .4 .8 10 15)", "translate(220 0) scale(-1 .7)"] do
        {"#{gradient} gradient with #{transform}",
         "<svg width='240' height='120'><defs>#{server}</defs><path transform='#{transform}' d='M10 40 Q80 0 160 70 L190 90' fill='none' stroke='url(#p)' stroke-width='4' stroke-opacity='.6' vector-effect='non-scaling-stroke'/></svg>"}
      end

    cases = [
      {"object bounding box gradient",
       "<svg width='240' height='120'><defs><linearGradient id='p'><stop stop-color='red'/><stop offset='1' stop-color='blue'/></linearGradient></defs><path transform='scale(2 .5)' d='M10 40 H180 L190 60' fill='none' stroke='url(#p)' stroke-width='3' vector-effect='non-scaling-stroke'/></svg>"},
      {"gradient fill and stroke",
       "<svg width='240' height='120'><defs><linearGradient id='p'><stop stop-color='red'/><stop offset='1' stop-color='blue'/></linearGradient></defs><path transform='scale(2 .5)' d='M10 40 H180 L190 60 Z' fill='url(#p)' stroke='url(#p)' stroke-width='3' paint-order='stroke fill' vector-effect='non-scaling-stroke'/></svg>"},
      {"transformed pattern",
       "<svg width='240' height='120'><defs><pattern id='p' patternUnits='userSpaceOnUse' x='3' y='4' width='12' height='10' patternTransform='translate(2 3)'><rect width='12' height='10' fill='gold'/><circle cx='4' cy='4' r='2' fill='blue'/></pattern></defs><path transform='scale(2 .5)' d='M10 40 H100 L110 60' fill='none' stroke='url(#p)' stroke-width='5' vector-effect='non-scaling-stroke'/></svg>"},
      {"stroke opacity",
       "<svg width='240' height='120'><defs><pattern id='p' patternUnits='userSpaceOnUse' width='12' height='10'><rect width='12' height='10' fill='gold'/><circle cx='4' cy='4' r='2' fill='blue'/></pattern></defs><path transform='scale(2 .5)' d='M10 40 H100 L110 60' fill='none' stroke='url(#p)' stroke-width='5' stroke-opacity='.4' vector-effect='non-scaling-stroke'/></svg>"},
      {"clipped pattern",
       "<svg width='240' height='120'><defs><clipPath id='c'><rect width='230' height='110'/></clipPath><pattern id='p' width='.2' height='.5' viewBox='0 0 12 10' preserveAspectRatio='none'><rect width='12' height='10' fill='gold'/><circle cx='4' cy='4' r='2' fill='blue'/></pattern></defs><g opacity='.5' clip-path='url(#c)'><path transform='scale(2 .5)' d='M10 40 H100 L110 60' fill='none' stroke='url(#p)' stroke-width='5' vector-effect='non-scaling-stroke'/></g></svg>"}
    ]

    for {scenario, svg} <- cases ++ transformed_gradients do
      case HtmlToPdf.render(svg, system_font_discovery: false) do
        {:error, {:unsupported_html, diagnostic}} ->
          assert diagnostic.stage == :layout, scenario
          assert diagnostic.source == "vector-effect", scenario
          assert diagnostic.module == SvgConverterValidator, scenario
          assert diagnostic.message =~ "non-scaling strokes", scenario

        result ->
          flunk("#{scenario} expected an unsupported stroke diagnostic, got #{inspect(result)}")
      end
    end
  end
end
