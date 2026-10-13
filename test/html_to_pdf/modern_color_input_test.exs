defmodule NativeElixirPdfUtilities.HtmlToPdf.ModernColorInputTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.{HtmlToPdf, Limits}
  alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, Style}
  alias NativeElixirPdfUtilities.Validators.{CssColorValidator, HtmlFormValidator, HtmlValidator}

  @original_cases [
    {"color(rec2100-linear .4 .5 .6)", "#000000"},
    {"color(rec2020-linear .4 .5 .6)", "#000000"},
    {"color(xyz .2 .3 .4)", "#00a7a4"},
    {"light-dark(red, blue)", "#000000"},
    {"rgb(clamp(none, 10, none) 20 30)", "#0a141e"},
    {"rgb(round(10.7) 20 30)", "#0b141e"},
    {"rgb(calc(10/**/ + 20) 20 30)", "#1e141e"},
    {"hsl(asin(1) 100% 50%)", "#80ff00"},
    {"color(display-p3-linear .4 .5 .6)", "#a5bccd"},
    {"rgb(calc(NaN) 200 0)", "#00c800"},
    {"rgb(calc(infinity) 200 0)", "#ffc800"},
    {"rgb(calc(-infinity) 200 0)", "#00c800"},
    {"rgb(calc(10 + 20) 20 30)", "#1e141e"},
    {"rgb(min(10,20) max(20,30) clamp(0,40,30))", "#0a1e1e"},
    {"rgb(round(10.7,1) 20 30)", "#0b141e"},
    {"rgb(round(up,10.1,1) 20 30)", "#0b141e"},
    {"rgb(mod(10,3) rem(-10,3) 30)", "#01001e"},
    {"rgb(pow(2,3) sqrt(16) hypot(3,4))", "#080405"},
    {"rgb(sin(90deg) cos(0) tan(0))", "#010100"},
    {"hsl(atan2(1,0) 100% 50%)", "#80ff00"},
    {"rgb(log(100,10) exp(1) abs(-10))", "#02030a"},
    {"rgb(sign(-10) 20 30)", "#00141e"},
    {"rgb(calc(10% + 10) 20 30)", "#000000"},
    {"rgb(calc(10 / 1%) 20 30)", "#000000"},
    {"rgb(calc(10 + 20 ) 20 30)", "#1e141e"},
    {"rgb(calc(1/0) 20 30)", "#ff141e"},
    {"hsl(calc(120deg / 1deg) 100% 50%)", "#000000"},
    {"rgb(calc(1 * (2 + 3)) 20 30)", "#05141e"},
    {"hsl(120 100% 50%)", "#00ff00"},
    {"hsla(1turn,100%,50%,.2)", "#ff0000"},
    {"hwb(60 10% 20%)", "#cccc1a"},
    {"lab(50% 30% -30%)", "#9c5eb8"},
    {"lch(60% 40 120)", "#7d9a51"},
    {"oklab(.6 .1 -.1)", "#9f63ba"},
    {"oklch(65% .2 40)", "#ee560c"},
    {"color(srgb .1 .2 .3)", "#1a334d"},
    {"color(srgb-linear .1 .2 .3)", "#597c95"},
    {"color(display-p3 .4 .5 .6)", "#5f809c"},
    {"color(a98-rgb .4 .5 .6)", "#59809b"},
    {"color(prophoto-rgb .4 .5 .6)", "#4597ad"},
    {"color(rec2020 .4 .5 .6)", "#5c8ea6"},
    {"color(xyz-d50 .2 .3 .4)", "#00a8bd"},
    {"color(xyz-d65 .2 .3 .4)", "#00a7a4"},
    {"color-mix(in srgb, red, blue)", "#000000"},
    {"rgb(from red r g b)", "#000000"},
    {"Canvas", "#ffffff"},
    {"ButtonFace", "#efefef"},
    {"rgb(1/**/ 2 3)", "#010203"},
    {"rgb(10% 20% 30% / none)", "#1a334d"},
    {"color(display-p3 none .2 .3)", "#00344f"},
    {"rgb(calc(1 * 10) 20 30)", "#0a141e"},
    {"rgb(1 2 3) nonsense", "#000000"},
    {"#1234", "#112233"},
    {"rgba(0%,0,0%,.2)", "#000000"},
    {"rgb(1e999 0 0)", "#ff0000"},
    {"AccentColor", "#000000"},
    {"AccentColorText", "#000000"},
    {"ActiveText", "#ff0000"},
    {"ButtonBorder", "#767676"},
    {"ButtonFace", "#efefef"},
    {"ButtonText", "#000000"},
    {"Canvas", "#ffffff"},
    {"CanvasText", "#000000"},
    {"Field", "#ffffff"},
    {"FieldText", "#000000"},
    {"GrayText", "#808080"},
    {"Highlight", "#0041c6"},
    {"HighlightText", "#ffffff"},
    {"LinkText", "#0000ee"},
    {"Mark", "#ffff00"},
    {"MarkText", "#000000"},
    {"SelectedItem", "#1967d2"},
    {"SelectedItemText", "#ffffff"},
    {"VisitedText", "#551a8b"},
    {"ActiveBorder", "#767676"},
    {"ActiveCaption", "#ffffff"},
    {"AppWorkspace", "#ffffff"},
    {"Background", "#ffffff"},
    {"ButtonHighlight", "#efefef"},
    {"ButtonShadow", "#efefef"},
    {"CaptionText", "#000000"},
    {"InactiveBorder", "#767676"},
    {"InactiveCaption", "#ffffff"},
    {"InactiveCaptionText", "#808080"},
    {"InfoBackground", "#ffffff"},
    {"InfoText", "#000000"},
    {"Menu", "#ffffff"},
    {"MenuText", "#000000"},
    {"Scrollbar", "#ffffff"},
    {"ThreeDDarkShadow", "#767676"},
    {"ThreeDFace", "#efefef"},
    {"ThreeDHighlight", "#767676"},
    {"ThreeDLightShadow", "#767676"},
    {"ThreeDShadow", "#767676"},
    {"Window", "#ffffff"},
    {"WindowFrame", "#767676"},
    {"WindowText", "#000000"}
  ]

  test "modern initial values match original pinned Chromium opaque sRGB serialization" do
    for {value, expected} <- @original_cases do
      assert {:ok, state} = HtmlFormValidator.prepare_initial_color(%{"value" => value})
      assert state.value == expected, "#{value}: expected #{expected}, got #{state.value}"
      assert state.color_input.value == expected
    end
  end

  test "alpha and colorspace preserve authored metadata and default-profile opaque state" do
    for attributes <- [
          %{"alpha" => ""},
          %{"colorspace" => "display-p3"},
          %{"alpha" => "false", "colorspace" => "DISPLAY-P3"},
          %{"colorspace" => "unknown"}
        ] do
      assert {:ok, %{value: "#ff0000", color_input: %{rgb: rgb}}} =
               HtmlFormValidator.prepare_initial_color(
                 Map.merge(attributes, %{"value" => "color(display-p3 1 0 0 / .5)"})
               )

      assert rgb == {1.0, 0.0, 0.0}
    end

    assert {:ok, dom} =
             HtmlParser.parse_detailed(
               "<input type=color alpha=false colorspace=display-p3 value='color(display-p3 1 0 0 / .5)'>"
             )

    assert {:ok, %{children: [node]}} = Style.compute_detailed(dom, forms: :static)
    assert node.style._form_control.attributes == hd(dom.children).attributes
    assert node.style._color_state.value == "#ff0000"
  end

  test "generic CSS modern color uses typed shared parser and current-color authority" do
    assert {:ok, {red, green, blue}} = HtmlValidator.parse_css_color("hsl(120 100% 50%)", nil)
    assert_in_delta red, 0.0, 0.000001
    assert_in_delta green, 1.0, 0.000001
    assert_in_delta blue, 0.0, 0.000001
    assert {:ok, {1, 0, 0}} = CssColorValidator.parse_function_color("currentcolor", {1, 0, 0})
    assert :error = CssColorValidator.parse_function_color("currentcolor", nil)
    assert :error = HtmlValidator.parse_css_color("hsl(none,100%,50%)", nil)
    assert :error = HtmlValidator.parse_css_color("hsl(120,100,50)", nil)
    assert {:ok, {r, g, b}} = HtmlValidator.parse_css_color("rgb(min(10,20) 20 30)", nil)
    assert_in_delta r, 10 / 255, 0.000001
    assert_in_delta g, 20 / 255, 0.000001
    assert_in_delta b, 30 / 255, 0.000001
  end

  test "native color calculation budget failures remain actionable public diagnostics" do
    previous = Limits.effective()
    on_exit(fn -> Limits.install(previous) end)
    Limits.install(%{previous | max_css_work: 1})

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             HtmlToPdf.render("<input type=color value='rgb(calc(1 * 10) 20 30)'>",
               forms: :static
             )

    assert diagnostic.stage == :style and diagnostic.module == HtmlValidator
    assert diagnostic.message =~ "max_css_work"
    assert diagnostic.source == "input/color"
  end

  test "shared math depth exhaustion is recoverable and source-specific" do
    previous = Limits.effective()
    on_exit(fn -> Limits.install(previous) end)
    Limits.install(%{previous | max_media_query_depth: 1})

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             HtmlToPdf.render("<input type=color value='rgb(calc(calc(10)) 20 30)'>",
               forms: :static
             )

    assert diagnostic.stage == :style
    assert diagnostic.message =~ "max_media_query_depth"
    assert diagnostic.source == "input/color"
    Limits.install(previous)

    assert {:ok, state} =
             HtmlFormValidator.prepare_initial_color(%{"value" => "rgb(calc(10) 20 30)"})

    assert state.value == "#0a141e"
  end

  test "color calculations cannot acquire lengths or cancel dimensional products" do
    alias NativeElixirPdfUtilities.Validators.CssMathValidator
    assert :error = CssMathValidator.evaluate_color_component("calc(1ex / 1ex)")
    assert :error = CssMathValidator.evaluate_color_component("calc(120deg / 1deg)")
    assert :error = CssMathValidator.evaluate_color_component("calc(1% * 2%)")
    assert {:ok, value, %{angle: 1}} = CssMathValidator.evaluate_color_component("atan2(1,0)")
    assert_in_delta value, :math.pi() / 2, 0.00000001
    assert {:ok, 10.0, %{}} = CssMathValidator.evaluate_color_component("calc(1/**/ * 10)")
    assert :error = CssMathValidator.evaluate_color_component("calc(1/**/+/**/2)")

    assert {:ok, general_angle, %{}} =
             CssMathValidator.evaluate("calc(120deg / 1deg)", %{width: 0, height: 0})

    assert_in_delta general_angle, 120.0, 0.00000001
  end

  test "one pica remains sixteen CSS pixels under the actual selected twenty-pixel font" do
    alias NativeElixirPdfUtilities.Validators.CssMathValidator

    font = %{
      family: "DejaVu Sans",
      path: Path.expand("priv/fonts/dejavu/DejaVuSans.ttf"),
      weight: 400,
      style: :normal
    }

    assert {:ok, dom} =
             HtmlParser.parse_detailed(
               "<p style='font-family:DejaVu Sans;font-size:20px'>Selected font</p>"
             )

    assert {:ok, %{children: [node]}} =
             Style.compute_detailed(dom, fonts: [font], system_font_discovery: false)

    assert node.style.font_face.type == :embedded
    assert node.style.font_size == 15.0

    environment = %{
      width: 300,
      height: 400,
      font_size: node.style.font_size / 0.75,
      root_font_size: node.style.font_size / 0.75
    }

    assert {:ok, 16.0, %{length: 1}} = CssMathValidator.evaluate("1pc", environment)
    assert {:ok, 20.0, %{length: 1}} = CssMathValidator.evaluate("1em", environment)
  end
end
