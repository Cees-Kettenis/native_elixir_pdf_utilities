Code.require_file(NativeElixirPdfUtilities.TestSupport.Paths.support("font_fixture.ex"))

defmodule NativeElixirPdfUtilities.HtmlToPdf.FontControlsTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.TestSupport.FontCorpus
  alias NativeElixirPdfUtilities.{HtmlToPdf, Text}
  alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, Style, Font}
  alias NativeElixirPdfUtilities.HtmlToPdf.PdfWriter
  alias NativeElixirPdfUtilities.Pdf.Reader
  alias NativeElixirPdfUtilities.TestSupport.Paths
  alias NativeElixirPdfUtilities.Validators.{CssFontControlsValidator, FontRenderingValidator}

  test "control grammars preserve case-sensitive OpenType tags, alternates and feature precedence" do
    for {property, value} <- [
          {"font-feature-settings", ~s("liga" off, "kern" ON, "ss01" 3, "liga" 1)},
          {"font-variation-settings", ~s("wght" 650, "wdth" -10.5)},
          {"font-language-override", ~s("TRK")},
          {"font-kerning", "none"},
          {"font-optical-sizing", "none"},
          {"font-variant-ligatures", "no-common-ligatures discretionary-ligatures no-contextual"},
          {"font-variant-caps", "all-petite-caps"},
          {"font-variant-numeric",
           "oldstyle-nums tabular-nums stacked-fractions ordinal slashed-zero"},
          {"font-variant-east-asian", "jis04 proportional-width ruby"},
          {"font-variant-position", "super"},
          {"font-variant-emoji", "emoji"},
          {"font-variant-alternates", "historical-forms styleset(Pretty, Alternate) swash(Flow)"},
          {"font-variant", "small-caps oldstyle-nums styleset(Pretty)"},
          {"font-synthesis", "weight small-caps position"},
          {"font-synthesis", "none"},
          {"font-synthesis-style", "oblique-only"},
          {"font-synthesis-weight", "none"},
          {"font-synthesis-small-caps", "auto"},
          {"font-synthesis-position", "none"},
          {"font-size-adjust", "cap-height from-font"},
          {"font-size-adjust", "0.5"},
          {"font-size-adjust", "none"},
          {"font-palette", "DARK"},
          {"font-palette", "--Brand"}
        ] do
      assert {:ok, controls} = CssFontControlsValidator.prepare(property, value),
             inspect({property, value})

      assert is_map(controls)
    end

    {:ok, ligatures} = CssFontControlsValidator.prepare("font-variant-ligatures", "none")

    {:ok, features} =
      CssFontControlsValidator.prepare("font-feature-settings", ~s("liga" on, "kern" off))

    plan =
      CssFontControlsValidator.feature_plan(Map.merge(ligatures, features), %{
        "liga" => 0,
        "dlig" => 1
      })

    assert plan.features == %{
             "liga" => 1,
             "clig" => 0,
             "dlig" => 0,
             "hlig" => 0,
             "calt" => 0,
             "kern" => 0
           }

    assert {:ok, %{:"language-override" => "TRK "}} =
             CssFontControlsValidator.prepare("font-language-override", ~s("TRK"))

    assert {:ok, %{:"variation-settings" => %{"WGHT" => 650}}} =
             CssFontControlsValidator.prepare("font-variation-settings", ~s("WGHT" 650))
  end

  test "malformed controls and mutually exclusive variants are rejected by their owner" do
    for {property, value} <- [
          {"font-feature-settings", ~s("lig" 1)},
          {"font-feature-settings", ~s("liga" -1)},
          {"font-feature-settings", ~s("liga" 1.0)},
          {"font-feature-settings", ~s("liga" 1,)},
          {"font-feature-settings", ~s(liga on)},
          {"font-feature-settings", ~s("liga" 1 2)},
          {"font-variation-settings", ~s("wght")},
          {"font-variation-settings", ~s("wght" on)},
          {"font-variation-settings", ~s("wght" 650px)},
          {"font-variant-ligatures", "common-ligatures no-common-ligatures"},
          {"font-variant-numeric", "oldstyle-nums lining-nums"},
          {"font-variant-east-asian", "jis78 traditional"},
          {"font-variant-caps", "small-caps unicase"},
          {"font-variant-alternates", "styleset(A,)"},
          {"font-variant-alternates", "swash(A,B)"},
          {"font-variant-alternates", "styleset(A) styleset(B)"},
          {"font-variant", "normal small-caps"},
          {"font-synthesis", "weight weight"},
          {"font-synthesis-style", "italic"},
          {"font-language-override", ~s("TOO-LONG")},
          {"font-size-adjust", "-1"},
          {"font-size-adjust", "cap-height"},
          {"font-palette", "Brand"},
          {"unknown-font-control", "normal"},
          {"font-kerning", nil},
          {"font-kerning", "auto none"},
          {"font-variant", "sub super"},
          {"font-variant-alternates", "(bad)"}
        ] do
      result = CssFontControlsValidator.prepare(property, value)
      assert result == :error, inspect({property, value, result})
    end
  end

  test "inherited feature controls, CSS-wide defaults and font shorthand reset use the same cascade fields" do
    assert {:ok, dom} =
             HtmlParser.parse_detailed(
               ~s(<div style='font-feature-settings:"liga" off;font-kerning:none;font-synthesis:none'><span>A</span><b style='font:small-caps 16px serif'>B</b><i style='font-feature-settings:initial'>C</i></div>)
             )

    assert {:ok, %{children: [parent]}} =
             Style.compute_detailed(dom, system_font_discovery: false)

    [inherited, shorthand, reset] = parent.children
    assert inherited.style.font_controls[:"feature-settings"] == %{"liga" => 0}
    assert shorthand.style.font_controls[:"variant-caps"] == ["small-caps"]
    assert shorthand.style.font_controls[:"feature-settings"] in [nil, %{}]
    assert shorthand.style.font_controls[:"synthesis-weight"] == :none
    assert reset.style.font_controls[:"feature-settings"] in [nil, %{}]
  end

  test "CSS axis controls instantiate the selected font and preserve text in PDF output" do
    source = FontCorpus.path("selected-variable.ttf") |> File.read!()

    assert {:ok, dom} =
             HtmlParser.parse_detailed(
               ~s(<p style='font-family:Variable;font-variation-settings:"wght" 650,"wdth" 112.5'>Hx0</p>)
             )

    options = [fonts: [%{family: "Variable", data: source}], system_font_discovery: false]
    assert {:ok, %{children: [paragraph]}} = Style.compute_detailed(dom, options)
    assert paragraph.style.font_face.variation_coordinates == %{"wght" => 650, "wdth" => 112.5}

    assert {:ok, pdf} =
             HtmlToPdf.render(
               ~s(<p style='font-family:Variable;font-variation-settings:"wght" 650,"wdth" 112.5'>Hx0</p>),
               options
             )

    assert {:ok, "Hx0"} = Text.extract(pdf, layout: false)
  end

  test "executed kerning/ligature controls agree across measurement, rendering and shaping gates" do
    assert {:ok, registry} = Font.load_registry(system_font_discovery: false)
    assert {:ok, _, font} = Font.resolve("DejaVu Sans", 400, :normal, registry)

    {:ok, controls} =
      CssFontControlsValidator.prepare("font-feature-settings", ~s("kern" off, "liga" off))

    assert {:ok, disabled} = FontRenderingValidator.prepare(font, %{font_controls: controls})
    assert disabled.kerning == %{}
    assert disabled.latin_ligatures == %{}
    assert Font.text_width("AV", disabled, 24) > Font.text_width("AV", font, 24)
    assert Font.shape_ligatures("fi", disabled) == "fi"

    assert {:ok, pdf} =
             HtmlToPdf.render(
               ~s(<p style='font-feature-settings:"kern" off,"liga" off'>AV fi</p>),
               system_font_discovery: false
             )

    assert {:ok, "AV fi"} = Text.extract(pdf, layout: false)
    {:ok, controls} = CssFontControlsValidator.prepare("font-feature-settings", ~s("dlig" on))
    assert {:ok, pending} = FontRenderingValidator.prepare(font, %{font_controls: controls})

    assert {:error, {:unsupported_font, diagnostic}} =
             FontRenderingValidator.validate_execution(pending, "st")

    assert diagnostic.message =~ "script shaping"
    assert :ok = FontRenderingValidator.validate_execution(pending, " ")

    assert {:ok, again} =
             FontRenderingValidator.prepare(disabled, %{
               font_controls: disabled.rendering_request |> elem(0)
             })

    assert again == disabled
  end

  test "synthetic bold and oblique painting retains extraction and both fill/stroke alpha" do
    data = File.read!(FontCorpus.path("selected-container-sans.ttf"))
    assert {:ok, [font]} = Font.load_configured_faces([%{family: "Synthetic", data: data}])

    assert {:ok, synthetic} =
             FontRenderingValidator.prepare(font, %{font_weight: 700, font_style: :italic})

    assert synthetic.synthesis.weight
    assert_in_delta synthetic.synthesis.shear, :math.tan(14 * :math.pi() / 180), 1.0e-12

    box = %{
      type: :text,
      text: "Hx0",
      x: 10,
      y: 20,
      font: Font.pdf_name(synthetic),
      font_face: synthetic,
      font_size: 24,
      color: {1, 0, 0, 0.5}
    }

    assert {:ok, pdf} =
             PdfWriter.render([%{size: {100, 100}, boxes: [box]}], compress_streams: false)

    assert pdf =~ "2 Tr 0.75 w"
    assert pdf =~ "0.2493 1 10"
    assert pdf =~ "1 0 0 RG"
    assert {:ok, "Hx0"} = Text.extract(pdf, layout: false)
    assert {:ok, document} = Reader.read(pdf)

    states =
      for {_ref, %{value: %{"Type" => {:name, "ExtGState"}} = value}} <- document.objects,
          do: value

    assert Enum.any?(states, &(&1["CA"] == 0.5))
    assert Enum.any?(states, &(&1["ca"] == 0.5))

    html =
      "<p style='font-family:Synthetic;font-weight:700;font-style:oblique 25deg;color:rgba(255,0,0,.5)'>Hx0</p>"

    opts = [
      fonts: [%{family: "Synthetic", data: data}],
      system_font_discovery: false,
      compress_streams: false
    ]

    assert {:ok, styled} = HtmlToPdf.render(html, opts)
    assert styled =~ "2 Tr"
    assert styled =~ "0.4663 -1"
    assert {:ok, "Hx0"} = Text.extract(styled, layout: false)

    assert {:ok, ordinary} =
             HtmlToPdf.render(
               String.replace(html, "font-weight:700", "font-synthesis:none;font-weight:700"),
               opts
             )

    refute ordinary =~ "2 Tr"
    refute ordinary =~ "0.4663 -1"

    for synthesis <- [
          %{weight: :bold, shear: 0},
          %{weight: false, shear: "0"},
          %{weight: false, shear: 100},
          %{}
        ] do
      malformed = %{box | font_face: Map.put(synthetic, :synthesis, synthesis)}

      assert {:error, {:invalid_font, diagnostic}} =
               PdfWriter.render([%{size: {100, 100}, boxes: [malformed]}])

      assert diagnostic.stage == :font
      assert diagnostic.operation == :write_pdf
      assert diagnostic.source == "Synthetic"
    end
  end

  test "metric descriptors scale measurement and PDF paint while overrides govern line boxes" do
    data = File.read!(FontCorpus.path("selected-container-sans.ttf"))
    assert {:ok, [original]} = Font.load_configured_faces([%{family: "Metrics", data: data}])

    assert {:ok, [font]} =
             Font.load_configured_faces([
               %{
                 family: "Metrics",
                 data: data,
                 size_adjust: 150,
                 ascent_override: 80,
                 descent_override: "20%",
                 line_gap_override: 0
               }
             ])

    assert {:ok, adjusted} = FontRenderingValidator.prepare(font, %{})
    assert adjusted.metric_scale == 1.5

    assert_in_delta Font.text_width("Hx0", adjusted, 20),
                    Font.text_width("Hx0", original, 30),
                    1.0e-9

    assert Font.line_metrics(adjusted).ascent == adjusted.units_per_em * 0.8
    assert Font.line_metrics(adjusted).descent == -adjusted.units_per_em * 0.2
    assert Font.line_metrics(adjusted).line_gap == 0

    assert {:ok, controls} =
             CssFontControlsValidator.prepare("font-size-adjust", "cap-height 0.5")

    assert {:ok, by_aspect} = FontRenderingValidator.prepare(font, %{font_controls: controls})

    assert {:ok, ratio} =
             NativeElixirPdfUtilities.Validators.FontMetricValidator.aspect(
               original,
               "cap-height"
             )

    assert_in_delta by_aspect.metric_scale, 0.5 / ratio, 1.0e-9

    box = %{
      type: :text,
      text: "Hx0",
      x: 10,
      y: 20,
      font: Font.pdf_name(adjusted),
      font_face: adjusted,
      font_size: 20,
      color: {0, 0, 0}
    }

    assert {:ok, pdf} =
             PdfWriter.render([%{size: {200, 100}, boxes: [box]}], compress_streams: false)

    assert pdf =~ "30 Tf"
    assert {:ok, "Hx0"} = Text.extract(pdf, layout: false)

    assert {:ok, dom} =
             HtmlParser.parse_detailed(
               "<p style='font-family:Metrics;font-size:20px;font-size-adjust:cap-height from-font'>Hx0</p>"
             )

    assert {:ok, %{children: [paragraph]}} =
             Style.compute_detailed(dom,
               fonts: [%{family: "Metrics", data: data}],
               system_font_discovery: false
             )

    assert paragraph.style.font_size == 15
    assert paragraph.style.font_controls[:"size-adjust"] == {"cap-height", ratio}
    assert {:ok, unscaled} = FontRenderingValidator.prepare(original, %{})
    assert unscaled == original

    assert {:ok, zero} =
             FontRenderingValidator.prepare(%{font | metric_descriptors: %{size_adjust: 0}}, %{})

    assert Font.used_size(zero, 20) == 0

    for config <- [%{size_adjust: -1}, %{size_adjust: :normal}, %{ascent_override: "10px"}] do
      assert {:error, _} =
               Font.load_configured_faces([Map.merge(%{family: "Metrics", data: data}, config)])
    end
  end

  test "synthetic caps and script forms preserve original Unicode and computed line struts" do
    assert {:ok, registry} = Font.load_registry(system_font_discovery: false)
    assert {:ok, _, font} = Font.resolve("DejaVu Sans", 400, :normal, registry)

    for {variant, scale} <- [
          {"small-caps", 0.7},
          {"all-small-caps", 0.7},
          {"petite-caps", 0.7},
          {"all-petite-caps", 0.7}
        ] do
      assert {:ok, controls} = CssFontControlsValidator.prepare("font-variant-caps", variant)
      assert {:ok, prepared} = FontRenderingValidator.prepare(font, %{font_controls: controls})
      assert {:ok, caps} = FontRenderingValidator.prepare_grapheme(prepared, "x")
      assert caps.cmap[?x] == font.cmap[?X]
      assert_in_delta caps.glyph_scale, scale, 1.0e-9
      assert Font.line_size(caps, 20) == 20
      assert Font.used_size(caps, 20) == 20 * scale
      assert {:ok, upper} = FontRenderingValidator.prepare_grapheme(prepared, "X")

      assert Map.get(upper, :glyph_scale, 1) ==
               if(String.starts_with?(variant, "all-"), do: scale, else: 1)
    end

    for position <- ["sub", "super"] do
      assert {:ok, controls} = CssFontControlsValidator.prepare("font-variant-position", position)
      assert {:ok, prepared} = FontRenderingValidator.prepare(font, %{font_controls: controls})
      assert {:ok, script} = FontRenderingValidator.prepare_grapheme(prepared, "x")
      assert script.glyph_scale > 0 and script.glyph_scale < 1
      assert Font.line_size(script, 20) == 20
      assert Font.baseline_offset(script, 20) > 0 == (position == "super")

      box = %{
        type: :text,
        text: "x",
        x: 10,
        y: 20,
        font: Font.pdf_name(script),
        font_face: script,
        font_size: 20,
        color: {0, 0, 0}
      }

      assert {:ok, pdf} =
               PdfWriter.render([%{size: {100, 100}, boxes: [box]}], compress_streams: false)

      assert {:ok, "x"} = Text.extract(pdf, layout: false)
    end

    for value <- ["none", "small-caps"] do
      html = "<p style='font-variant-caps:small-caps;font-synthesis:#{value}'>Ax</p>"
      assert {:ok, pdf} = HtmlToPdf.render(html, system_font_discovery: false)
      assert {:ok, %{pages: pages}} = Text.extract_spans(pdf)
      assert Enum.flat_map(pages, & &1.spans) |> Enum.map_join(& &1.text) == "Ax"
    end
  end

  test "font control registry, defaulting and shorthand grammar stay consistent with cascade fields" do
    properties = CssFontControlsValidator.properties()

    assert Map.keys(CssFontControlsValidator.cascade_fields()) |> Enum.sort() ==
             Enum.sort(properties)

    assert CssFontControlsValidator.variant_properties() -- properties == []
    assert CssFontControlsValidator.synthesis_properties() -- properties == []
    refute "font-palette" in CssFontControlsValidator.font_resets()

    for property <- CssFontControlsValidator.variant_properties(),
        do: assert({:ok, _} = CssFontControlsValidator.prepare(property, "normal"))

    assert {:ok, controls} = CssFontControlsValidator.prepare("font-variant", "none")
    assert CssFontControlsValidator.feature_plan(controls).features["liga"] == 0
    assert {:ok, _} = CssFontControlsValidator.prepare("font-variant", "normal")

    assert {:ok, _} =
             CssFontControlsValidator.prepare("font-variant", "oldstyle-nums tabular-nums")

    assert CssFontControlsValidator.feature_plan(%{kerning: :normal}).features["kern"] == 1

    for {property, value} <- [
          {"font-language-override", "normal"},
          {"font-language-override", "a b"},
          {"font-palette", "normal dark"},
          {"font-variant-alternates", "broken"},
          {"font-variant-alternates", "broken("},
          {"font-variant", "small-caps unknown"}
        ] do
      if value == "normal",
        do: assert({:ok, _} = CssFontControlsValidator.prepare(property, value)),
        else: assert(CssFontControlsValidator.prepare(property, value) == :error)
    end

    assert {:ok, %{}} =
             CssFontControlsValidator.prepare_defaults(%{
               feature_settings: "normal",
               language_override: "normal"
             })

    assert {:ok, %{feature_defaults: %{"liga" => 0}}} =
             CssFontControlsValidator.prepare_defaults(%{feature_settings: ~s("liga" off)})

    for config <- [
          %{feature_settings: ~s("bad" on)},
          %{feature_settings: nil},
          %{language_override: :invalid},
          %{language_override: "excessive"}
        ] do
      assert :error = CssFontControlsValidator.prepare_defaults(config)
    end

    assert {:ok, registry} = Font.load_registry(system_font_discovery: false)
    assert {:ok, _, font} = Font.resolve("DejaVu Sans", 400, :normal, registry)

    assert {:ok, controls} =
             CssFontControlsValidator.prepare("font-synthesis-style", "oblique-only")

    assert {:ok, italic} =
             FontRenderingValidator.prepare(font, %{font_style: :italic, font_controls: controls})

    assert Map.get(italic, :synthesis, %{shear: 0}).shear == 0

    assert {:ok, oblique} =
             FontRenderingValidator.prepare(font, %{
               font_style: :italic,
               font_oblique: 25,
               font_controls: controls
             })

    assert oblique.synthesis.shear > 0
  end

  test "automatic optical sizing uses the adjusted physical size and explicit axes retain precedence" do
    data = File.read!(FontCorpus.path("selected-control-axes.ttf"))

    assert {:ok, [font]} =
             Font.load_configured_faces([
               %{family: "Axes", data: data, variation_settings: %{"opsz" => 20}}
             ])

    style = %{font_size: 22.5, font_style: :italic}
    assert {:ok, selected} = FontRenderingValidator.prepare(font, style)

    assert selected.variation_coordinates == %{
             "ital" => 1,
             "slnt" => 0,
             "opsz" => 30,
             "TEST" => 0
           }

    manifest = File.read!(Paths.fixture("fonts/font-control-axes.json")) |> :json.decode()
    expected = Enum.find(manifest["instances"], &(&1["opsz"] == 30))
    assert {:ok, bounds} = Font.glyph_bounds(selected)
    assert Tuple.to_list(elem(bounds, selected.cmap[?H])) == expected["bounds"]

    assert {:ok, slanted} =
             FontRenderingValidator.prepare(font, Map.put(style, :font_oblique, 20))

    assert slanted.variation_coordinates["ital"] == 0
    assert slanted.variation_coordinates["slnt"] == -20

    assert {:ok, normal} =
             FontRenderingValidator.prepare(font, %{
               font_size: 22.5,
               font_controls: %{:"optical-sizing" => :none}
             })

    assert normal.variation_coordinates["opsz"] == 20

    assert {:ok, explicit} =
             FontRenderingValidator.prepare(font, %{
               font_size: 22.5,
               font_controls: %{:"variation-settings" => %{"opsz" => 60}}
             })

    assert explicit.variation_coordinates["opsz"] == 60

    assert {:ok, adjusted} =
             FontRenderingValidator.prepare(
               Map.put(font, :metric_descriptors, %{size_adjust: 200}),
               style
             )

    assert adjusted.variation_coordinates["opsz"] == 60
    assert {:ok, ^adjusted} = FontRenderingValidator.prepare(adjusted, style)
    assert {:ok, defaults} = Font.load_configured_faces([%{family: "Axes", data: data}])

    assert {:ok, [none]} =
             FontRenderingValidator.prepare_candidates(defaults, %{
               font_controls: %{:"optical-sizing" => :none}
             })

    assert none.variation_coordinates["opsz"] == 12
  end

  test "petite fallback, unicase synthesis, missing capitals and emoji requests preserve explicit feature precedence" do
    data = File.read!(FontCorpus.path("selected-control-axes.ttf"))
    assert {:ok, [font]} = Font.load_configured_faces([%{family: "Controls", data: data}])
    font = Map.drop(font, [:variation_source, :variation_axes, :variation_coordinates])
    gsub = <<1::16, 0::16, 0::16, 10::16, 0::16, 1::16, "smcp", 0::16>>

    smcp = %{
      font
      | data:
          NativeElixirPdfUtilities.TestSupport.FontFixture.rebuild(font.data, %{"GSUB" => gsub})
    }

    for caps <- [["petite-caps"], ["all-petite-caps"]] do
      assert {:ok, selected} =
               FontRenderingValidator.prepare(smcp, %{font_controls: %{:"variant-caps" => caps}})

      assert selected.feature_plan.features["smcp"] == 1

      assert {:error, {:unsupported_font, _}} =
               FontRenderingValidator.validate_execution(selected, "x")

      assert {:ok, plain} =
               FontRenderingValidator.prepare(smcp, %{
                 font_controls: %{:"variant-caps" => caps, :"feature-settings" => %{"smcp" => 0}}
               })

      assert :ok = FontRenderingValidator.validate_execution(plain, "x")
      assert {:ok, ^plain} = FontRenderingValidator.prepare_grapheme(plain, "x")
    end

    pcap = %{
      smcp
      | data:
          NativeElixirPdfUtilities.TestSupport.FontFixture.rebuild(font.data, %{
            "GSUB" => String.replace(gsub, "smcp", "pcap")
          })
    }

    assert {:ok, selected} =
             FontRenderingValidator.prepare(pcap, %{
               font_controls: %{:"variant-caps" => ["petite-caps"]}
             })

    assert {:ok, ^selected} = FontRenderingValidator.prepare_grapheme(selected, "x")

    assert {:ok, selected} =
             FontRenderingValidator.prepare(font, %{
               font_controls: %{:"variant-caps" => ["unicase"]}
             })

    assert {:ok, lower} = FontRenderingValidator.prepare_grapheme(selected, "x")
    refute Map.has_key?(lower, :glyph_scale)
    assert {:ok, upper} = FontRenderingValidator.prepare_grapheme(selected, "H")
    assert upper.glyph_scale == 0.7

    assert {:ok, caps} =
             FontRenderingValidator.prepare(Map.put(font, :unicode_cmap, font.cmap), %{
               font_controls: %{:"variant-caps" => ["small-caps"]}
             })

    assert {:ok, selected} = FontRenderingValidator.prepare_grapheme(caps, "x")
    refute FontRenderingValidator.supports_text?(caps, "x")
    assert selected.unicode_cmap[?x] == 0
    assert {:error, {:unsupported_font, _}} = FontRenderingValidator.validate_execution(caps, "ß")
    assert {:ok, ^font} = FontRenderingValidator.prepare_grapheme(font, "x")

    emoji = %{font | cmap: Map.put(font.cmap, ?0, font.cmap[?H])}

    for mode <- [:text, :unicode] do
      assert {:ok, selected} =
               FontRenderingValidator.prepare(emoji, %{
                 font_controls: %{:"variant-emoji" => [Atom.to_string(mode)]}
               })

      assert {:ok, selected} = FontRenderingValidator.prepare_grapheme(selected, "0")
      assert selected.feature_plan.emoji == mode
      assert :ok = FontRenderingValidator.validate_execution(selected, "0")
    end

    assert {:ok, selected} =
             FontRenderingValidator.prepare(emoji, %{
               font_controls: %{:"variant-emoji" => ["emoji"]}
             })

    assert {:error, {:unsupported_font, _}} =
             FontRenderingValidator.validate_execution(Map.delete(selected, :color_tables), "0")

    assert {:ok, selected} =
             FontRenderingValidator.prepare(Map.put(emoji, :color_tables, ["COLR"]), %{
               font_controls: %{:"variant-emoji" => ["unicode"]}
             })

    assert {:error, {:unsupported_font, _}} =
             FontRenderingValidator.validate_execution(selected, "0")

    stripped =
      NativeElixirPdfUtilities.TestSupport.FontFixture.rebuild(font.data, %{"OS/2" => nil})

    assert {:ok, selected} =
             FontRenderingValidator.prepare(%{font | data: stripped}, %{
               font_controls: %{:"variant-position" => ["super"]}
             })

    assert {:ok, script} = FontRenderingValidator.prepare_grapheme(selected, "H")
    assert script.glyph_scale == 0.65
  end

  test "feature table ownership failures and active GPOS kerning return recoverable diagnostics" do
    data = File.read!(FontCorpus.path("selected-variable.ttf"))
    assert {:ok, [font]} = Font.load_configured_faces([%{family: "Broken features", data: data}])

    for bytes <- [<<>>, <<1::16, 0::16, 0::16, 10::16, 0::16, 100::16>>] do
      broken = %{
        font
        | data:
            NativeElixirPdfUtilities.TestSupport.FontFixture.rebuild(font.data, %{"GSUB" => bytes})
      }

      assert {:ok, selected} =
               FontRenderingValidator.prepare(broken, %{font_controls: %{:kerning => :normal}})

      assert {:error, {:invalid_font, _}} =
               FontRenderingValidator.validate_execution(selected, "H")

      assert {:error, {:invalid_font, _}} =
               FontRenderingValidator.prepare_candidates([broken], %{
                 font_controls: %{:"variant-caps" => ["petite-caps"]}
               })

      assert {:ok, caps} =
               FontRenderingValidator.prepare(broken, %{
                 font_controls: %{:"variant-caps" => ["small-caps"]}
               })

      assert {:font_resource_limit, {:error, {:invalid_font, _}}} =
               catch_throw(FontRenderingValidator.supports_text?(caps, "H"))
    end

    gpos = <<1::16, 0::16, 0::16, 10::16, 0::16, 1::16, "kern", 0::16>>

    font = %{
      font
      | data:
          NativeElixirPdfUtilities.TestSupport.FontFixture.rebuild(font.data, %{"GPOS" => gpos}),
        kerning: %{}
    }

    for value <- [1, 3] do
      assert {:ok, selected} =
               FontRenderingValidator.prepare(font, %{
                 font_controls: %{
                   :kerning => :normal,
                   :"language-override" => "TRK ",
                   :"feature-settings" => %{"kern" => value}
                 }
               })

      assert {:error, {:unsupported_font, %{message: message}}} =
               FontRenderingValidator.validate_execution(selected, "H")

      assert message =~ "GPOS kerning"
      assert message =~ "language-specific OpenType selection"
    end

    gsub = <<1::16, 0::16, 0::16, 10::16, 0::16, 1::16, "clig", 0::16>>

    contextual = %{
      font
      | data:
          NativeElixirPdfUtilities.TestSupport.FontFixture.rebuild(font.data, %{"GSUB" => gsub})
    }

    assert {:ok, selected} =
             FontRenderingValidator.prepare(contextual, %{
               font_controls: %{:"feature-settings" => %{"clig" => 1}}
             })

    assert {:error, {:unsupported_font, %{message: message}}} =
             FontRenderingValidator.validate_execution(selected, "H")

    assert message =~ "clig"
  end

  test "resetting prepared static controls restores the physical face for selected and fallback candidates" do
    assert {:ok, [font]} =
             Font.load_configured_faces([
               %{
                 family: "Reset",
                 data: File.read!(FontCorpus.path("selected-container-sans.ttf"))
               }
             ])

    assert {:ok, bold} = FontRenderingValidator.prepare(font, %{font_weight: 700})
    assert bold.synthesis.weight
    assert {:ok, restored} = FontRenderingValidator.prepare(bold, %{font_weight: 400})
    refute Map.has_key?(restored, :synthesis)
    assert restored.data == font.data
    assert {:ok, [restored]} = FontRenderingValidator.prepare_candidates([bold], %{})
    refute Map.has_key?(restored, :synthesis)

    assert {:ok, caps} =
             FontRenderingValidator.prepare(font, %{
               font_controls: %{:"variant-caps" => ["small-caps"]}
             })

    assert {:ok, glyph} = FontRenderingValidator.prepare_grapheme(caps, "x")
    assert glyph.glyph_scale == 0.7
    assert {:ok, restored} = FontRenderingValidator.prepare(glyph, %{})
    refute Map.has_key?(restored, :glyph_scale)
    refute Map.has_key?(restored, :feature_plan)
    assert restored.cmap == font.cmap
  end

  test "fixed style axes retain slant fallback and permit synthesis when neither axis can provide a style" do
    alias NativeElixirPdfUtilities.TestSupport.FontFixture
    data = File.read!(FontCorpus.path("selected-control-axes.ttf"))
    {_, tables} = FontFixture.tables(data)
    ital_fixed = FontFixture.replace(tables["fvar"], 28, <<0::32>>)

    assert {:ok, [font]} =
             Font.load_configured_faces([
               %{family: "Fixed italic", data: FontFixture.rebuild(data, %{"fvar" => ital_fixed})}
             ])

    assert {:ok, selected} = FontRenderingValidator.prepare(font, %{font_style: :italic})
    assert selected.variation_coordinates["ital"] == 0
    assert selected.variation_coordinates["slnt"] == -14
    refute Map.has_key?(selected, :synthesis)
    both_fixed = FontFixture.replace(ital_fixed, 40, <<0::32>>)

    assert {:ok, [font]} =
             Font.load_configured_faces([
               %{family: "Fixed styles", data: FontFixture.rebuild(data, %{"fvar" => both_fixed})}
             ])

    assert {:ok, selected} = FontRenderingValidator.prepare(font, %{font_style: :italic})
    assert selected.synthesis.shear > 0
    assert {:ok, [font]} = Font.load_configured_faces([%{family: "Explicit upright", data: data}])

    assert {:ok, selected} =
             FontRenderingValidator.prepare(font, %{
               font_style: :italic,
               font_controls: %{:"variation-settings" => %{"ital" => 0}}
             })

    assert selected.variation_coordinates["ital"] == 0
    refute Map.has_key?(selected, :synthesis)
  end
end
