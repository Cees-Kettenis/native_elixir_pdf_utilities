defmodule NativeElixirPdfUtilities.HtmlToPdf.FontRuleTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.TestSupport.FontCorpus
  alias NativeElixirPdfUtilities.HtmlToPdf.{CssParser, Font, HtmlParser, Style}
  alias NativeElixirPdfUtilities.Validators.{CssFontRuleValidator, FontRenderingValidator}

  test "font-face defaults, localized named instances and source technology hints share property grammar" do
    css = """
    @font-face { font-family: Variable; src:url(variable.woff2) format("woff2-variations") tech(variations);
      font-feature-settings:"kern" off; font-language-override:"TRK";
      font-named-instance:"hEaVy"; font-variation-settings:"wdth" 112.5; }
    """

    assert {:ok, %{font_faces: [face]}} = CssParser.prepare_stylesheet(css)
    assert face.feature_settings == %{"kern" => 0}
    assert face.language_override == "TRK "
    assert face.named_instance == "hEaVy"
    data = FontCorpus.path("selected-variable.ttf") |> File.read!()

    assert {:ok, [font]} =
             Font.load_configured_faces([face |> Map.delete(:sources) |> Map.put(:data, data)])

    assert font.variation_coordinates == %{"wght" => 900, "wdth" => 112.5}
    assert {:ok, selected} = FontRenderingValidator.prepare(font, %{font_weight: 400})
    assert selected.variation_coordinates == font.variation_coordinates
    assert selected.feature_plan.features["kern"] == 0
    assert selected.feature_plan.language == "TRK "
    assert selected.kerning == %{}

    assert {:ok, explicit} =
             FontRenderingValidator.prepare(font, %{
               font_controls: %{
                 :"feature-settings" => %{"kern" => 1},
                 :"variation-settings" => %{"wght" => 650}
               }
             })

    assert explicit.feature_plan.features["kern"] == 1
    assert explicit.variation_coordinates["wght"] == 650

    assert {:ok, [auto]} =
             Font.load_configured_faces([
               %{family: "Variable", data: data, named_instance: "Missing"}
             ])

    assert auto.variation_coordinates == %{"wght" => 400, "wdth" => 100}

    assert {:ok, %{font_faces: [fallback]}} =
             CssParser.prepare_stylesheet(
               "@font-face{font-family:F;src:url(no.woff2) tech(unknown),url(yes.woff2) tech(color-COLRv1, variations)}"
             )

    assert fallback.sources == ["yes.woff2"]
  end

  test "named alternate definitions retain case and explicit feature settings win" do
    css = """
    @font-feature-values "DejaVu Sans", Alias {
      @styleset { Pretty: 1 4; Other: 2; }
      @character-variant { Curved: 3 2; Default: 4; }
      @stylistic { Flow: 2; }
      @swash { Flow: 3; }
      @ornaments { Bullet: 2; }
      @annotation { Circle: 1; }
    }
    @supports (unknown: no) { @font-feature-values Alias { @styleset { Pretty: 9; } } }
    """

    assert {:ok, prepared} = CssParser.prepare_stylesheet(css)
    definitions = prepared.cascade.font_definitions
    assert definitions.features["alias"]["styleset"]["Pretty"] == [1, 4]
    assert definitions.features["alias"]["styleset"]["pretty"] == nil

    alternates = [
      {"styleset", ["Pretty", "Missing"]},
      {"character-variant", ["Curved", "Default"]},
      {"stylistic", ["Flow"]},
      {"swash", ["Flow"]},
      {"ornaments", ["Bullet"]},
      {"annotation", ["Circle"]}
    ]

    assert CssFontRuleValidator.features(definitions, "Alias", alternates) == %{
             "ss01" => 1,
             "ss04" => 1,
             "cv03" => 2,
             "cv04" => 1,
             "salt" => 2,
             "swsh" => 3,
             "cswh" => 3,
             "ornm" => 2,
             "nalt" => 1
           }

    assert CssFontRuleValidator.features(definitions, "Missing", alternates) == %{}

    assert {:ok, dom} =
             HtmlParser.parse_detailed(
               "<style>#{css}</style><p style='font-variant-alternates:styleset(Pretty);font-feature-settings:\"ss01\" off'>A</p>"
             )

    assert {:ok, %{children: children}} =
             Style.compute_detailed(dom, system_font_discovery: false)

    paragraph = Enum.find(children, &(&1.type == :element and &1.tag == "p"))
    assert paragraph.style.font_face.feature_plan.features["ss01"] == 0
    assert paragraph.style.font_face.feature_plan.features["ss04"] == 1
  end

  test "named palettes select their declared font, base palette and explicit RGBA overrides" do
    css =
      "@font-palette-values --Brand {font-family:Vector; base-palette:dark; override-colors:0 #ff00ff,1 rgba(0,255,0,.25),99 blue;}"

    assert {:ok, prepared} = CssParser.prepare_stylesheet(css)
    definitions = prepared.cascade.font_definitions

    assert %{base: :dark, overrides: overrides} =
             CssFontRuleValidator.palette(definitions, "Vector", "--Brand")

    assert overrides[0] == {1.0, 0.0, 1.0, 1.0}
    assert overrides[1] == {0.0, 1.0, 0.0, 0.25}
    assert :normal = CssFontRuleValidator.palette(definitions, "Other", "--Brand")
    data = FontCorpus.path("selected-colr.ttf") |> File.read!()
    assert {:ok, [font]} = Font.load_configured_faces([%{family: "Vector", data: data}])

    assert {:ok, font} =
             FontRenderingValidator.prepare(font, %{
               font_controls: %{palette: "--Brand"},
               _font_definitions: %{nil => definitions}
             })

    assert font.palette.base == :dark

    assert {:ok, paint} =
             NativeElixirPdfUtilities.Validators.ColrFontValidator.prepare(
               font,
               font.cmap[?H],
               {0, 0, 0, 1}
             )

    assert elem(paint.colors, 0) == {1.0, 0.0, 1.0, 1.0}
    assert elem(paint.colors, 1) == {0.0, 1.0, 0.0, 0.25}

    scopes =
      CssFontRuleValidator.combine([
        %{prepared: prepared},
        %{shadow_scope_id: :shadow, prepared: prepared}
      ])

    assert scopes[nil] == scopes.shadow
  end

  test "palette names keep independent family definitions and inline family declarations use their stylesheet scope" do
    css = """
    @font-palette-values --Brand {font-family: Vector; base-palette:dark;override-colors:0 red;}
    @font-palette-values --Brand {font-family: Other; base-palette:1;}
    @font-palette-values --Brand {font-family: Vector; override-colors:0 green;}
    """

    assert {:ok, prepared} = CssParser.prepare_stylesheet(css)
    definitions = prepared.cascade.font_definitions
    assert %{base: 1} = CssFontRuleValidator.palette(definitions, "Other", "--Brand")

    assert %{base: 0, overrides: %{0 => color}} =
             CssFontRuleValidator.palette(definitions, "Vector", "--Brand")

    {red, green, blue, alpha} = color
    assert red == 0 and blue == 0 and alpha == 1
    assert_in_delta green, 128 / 255, 1.0e-10
    data = File.read!(FontCorpus.path("selected-colr.ttf"))

    assert {:ok, dom} =
             HtmlParser.parse_detailed(
               "<style>#{css}</style><p style='font-family:Vector;font-palette:--Brand'>H</p>"
             )

    assert {:ok, %{children: children}} =
             Style.compute_detailed(dom,
               fonts: [%{family: "Vector", data: data}],
               system_font_discovery: false
             )

    paragraph = Enum.find(children, &(&1.type == :element and &1.tag == "p"))

    assert paragraph.style.font_face.palette ==
             CssFontRuleValidator.palette(definitions, "Vector", "--Brand")

    assert {:ok, resolved} =
             NativeElixirPdfUtilities.HtmlToPdf.FontFallback.resolve(%{
               type: :document,
               children: children
             })

    paragraph = Enum.find(resolved.children, &(&1.type == :element and &1.tag == "p"))
    [text] = paragraph.children
    assert text.style.font_face.palette == paragraph.style.font_face.palette
  end

  test "rule recovery, zero indices, repeated definitions and tree scopes use the admitted records" do
    records = [
      %{
        kind: "font-feature-values",
        name: "Alias",
        declarations:
          "@styleset{Zero:0 100; Good:2; Bad:-1; Twice:2 3;} @character-variant{Zero:0; Big:100; Bad:1 2 3;} @stylistic{Bad:1 2;} @unknown{No:1;}"
      },
      %{
        kind: "font-feature-values",
        name: "Alias",
        declarations: "@styleset{Good:4;} @swash{Fine:0;}"
      },
      %{kind: "font-feature-values", name: "12", declarations: "@styleset{No:1;}"},
      %{kind: "font-feature-values", name: "\"\"", declarations: "@styleset{No:1;}"},
      %{kind: "font-palette-values", name: "bad", declarations: "font-family:Alias;"},
      %{
        kind: "font-palette-values",
        name: "--bad",
        declarations: "font-family:Alias;base-palette:-1;"
      },
      %{
        kind: "font-palette-values",
        name: "--bad",
        declarations: "font-family:Alias;override-colors:0 currentColor;"
      },
      %{
        kind: "font-palette-values",
        name: "--Light",
        declarations: "font-family:Alias;base-palette:light;"
      }
    ]

    definitions = CssFontRuleValidator.prepare(records)

    assert CssFontRuleValidator.features(definitions, "Alias", [
             {"styleset", ["Zero", "Good"]},
             {"character-variant", ["Zero", "Big"]}
           ]) == %{"ss04" => 1}

    assert CssFontRuleValidator.features(definitions, "Alias", :normal) == %{}
    assert %{base: :light} = CssFontRuleValidator.palette(definitions, "Alias", "--Light")
    assert :normal = CssFontRuleValidator.palette(definitions, "Alias", "--bad")
    prepared = %{cascade: %{font_definitions: definitions}}

    scopes =
      CssFontRuleValidator.combine([
        %{prepared: %{}},
        %{prepared: Map.put(prepared, :stylesheet_applicable, false)},
        %{prepared: prepared},
        %{prepared: prepared},
        %{
          shadow_scope_id: :shadow,
          prepared: %{cascade: %{font_definitions: %{features: %{}, palettes: %{}}}}
        }
      ])

    assert scopes == %{nil => definitions}
  end
end
