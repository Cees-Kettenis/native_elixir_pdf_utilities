defmodule NativeElixirPdfUtilities.HtmlToPdf.FontSelectionTest do
  use ExUnit.Case, async: true

  alias NativeElixirPdfUtilities.{HtmlToPdf, Text}
  alias NativeElixirPdfUtilities.HtmlToPdf.{Font, FontFallback, HtmlParser, Style}
  alias NativeElixirPdfUtilities.HtmlToPdf.Font.Selection
  alias NativeElixirPdfUtilities.TestSupport.Paths
  alias NativeElixirPdfUtilities.Validators.{CssFontValidator, CssFontFaceValidator}

  # CSS Fonts 4, 1 February 2024, §5.2. These expected physical-face choices
  # follow the directional searches, rather than the old nearest-weight rule.
  test "directional weight and width searches select the specified physical face" do
    for {desired, available, expected} <- [
          {400, [300, 500], 500},
          {450, [400, 500], 500},
          {500, [400, 600], 400},
          {350, [100, 400], 100},
          {550, [500, 900], 900},
          {500, [500, 600], 500},
          {1, [100, 200], 100},
          {1000, [800, 900], 900}
        ] do
      faces = Enum.map(available, &%{weight: &1, style: :normal})
      chosen = Enum.min_by(faces, &Selection.rank(&1, desired, :normal))
      assert chosen.weight == expected
    end

    for {desired, widths, expected} <- [
          {100, [75, 112.5], 75},
          {110, [75, 125], 125},
          {75, [75, 125], 75},
          {80, [100, 125], 100},
          {150, [75, 125], 125}
        ] do
      faces = Enum.map(widths, &%{weight: 400, style: :normal, width: &1})
      chosen = Enum.min_by(faces, &Selection.rank(&1, 400, :normal, %{width: desired}))
      assert chosen.width == expected
    end

    assert Selection.rank(%{weight: 300, weight_range: {300, 600}, style: :normal}, 450, :normal) <
             Selection.rank(%{weight: 500, style: :normal}, 450, :normal)

    assert Selection.rank(%{weight: 500, width_range: {70, 100}, style: :normal}, 400, :normal, %{
             width: 90
           }) <
             Selection.rank(%{weight: 400, width: 110, style: :normal}, 400, :normal, %{width: 90})
  end

  test "physical italic, normal and signed oblique faces follow style search order" do
    normal = %{weight: 400, style: :normal}
    italic = %{weight: 400, style: :italic}
    oblique = fn angle -> Map.put(italic, :oblique, {angle, angle}) end
    assert Selection.rank(normal, 400, :normal) < Selection.rank(italic, 400, :normal)
    assert Selection.rank(oblique.(10), 400, :normal) < Selection.rank(italic, 400, :normal)
    assert Selection.rank(italic, 400, :normal) < Selection.rank(oblique.(-10), 400, :normal)
    assert Selection.rank(normal, 400, :normal) == Selection.rank(oblique.(0), 400, :normal)

    assert Selection.rank(normal, 400, :italic, %{oblique: 0}) ==
             Selection.rank(oblique.(0), 400, :italic, %{oblique: 0})

    assert Selection.rank(oblique.(13), 400, :italic, %{oblique: 12}) <
             Selection.rank(oblique.(10), 400, :italic, %{oblique: 12})

    assert Selection.rank(italic, 400, :italic, %{oblique: 15}) <
             Selection.rank(oblique.(0), 400, :italic, %{oblique: 15})

    assert Selection.rank(normal, 400, :italic, %{oblique: -15}) <
             Selection.rank(italic, 400, :italic, %{oblique: -15})

    assert Selection.rank(italic, 400, :italic) < Selection.rank(oblique.(14), 400, :italic)
    assert Selection.rank(oblique.(20), 400, :italic) < Selection.rank(normal, 400, :italic)
    assert Selection.rank(normal, 400, :italic) < Selection.rank(oblique.(-10), 400, :italic)

    assert Selection.rank(oblique.(20), 400, :italic, %{oblique: 15}) <
             Selection.rank(oblique.(10), 400, :italic, %{oblique: 15})

    assert Selection.rank(oblique.(5), 400, :italic, %{oblique: 10}) <
             Selection.rank(oblique.(15), 400, :italic, %{oblique: 10})

    assert Selection.rank(oblique.(-20), 400, :italic, %{oblique: -15}) <
             Selection.rank(oblique.(-10), 400, :italic, %{oblique: -15})

    assert Selection.rank(italic, 400, :italic, %{oblique: 15}) <
             Selection.rank(normal, 400, :italic, %{oblique: 15})

    assert Selection.rank(italic, 400, :italic, %{oblique: 15}) <
             Selection.rank(oblique.(-10), 400, :italic, %{oblique: 15})
  end

  test "configured ranges retain distinct physical and unicode faces" do
    sans = File.read!(Paths.fixture("fonts/selected-container-sans.ttf"))
    mono = File.read!(Paths.fixture("fonts/selected-container-mono.ttf"))

    assert {:ok, registry} =
             Font.load_registry(
               fonts: [
                 %{family: "Boundary", data: mono, weight: 300},
                 %{family: "Boundary", data: sans, weight: "450 550"},
                 %{family: "Width", data: mono, width: "expanded"},
                 %{family: "Width", data: sans, width: "condensed"},
                 %{family: "Composite", data: sans, unicode_range: "U+48"},
                 %{family: "Composite", data: mono, unicode_range: "U+30,U+78"},
                 %{family: "Report, Inc", data: mono}
               ],
               system_font_discovery: false
             )

    assert {:ok, ["Report, Inc", "Missing"], quoted} =
             Font.resolve("\"Report, Inc\", Missing", 400, :normal, registry)

    assert quoted.data == mono
    assert {:ok, _, selected} = Font.resolve("Boundary", 400, :normal, registry)
    assert selected.data == sans
    assert {:ok, _, selected} = Font.resolve("Width", 400, :normal, registry)
    assert selected.data == sans

    assert {:ok, _, selected} =
             Font.resolve("Width", 400, :normal, Map.put(registry, :selection, %{width: 110}))

    assert selected.data == mono
    assert [first, second] = Font.requested_faces("Composite", 400, :normal, registry)
    assert Font.supports_text?(first, "H")
    refute Font.supports_text?(first, "x")
    assert Font.supports_text?(second, "x0")
    refute Font.supports_text?(second, "Hx")

    assert Enum.map(Font.fallback_faces(registry, 400, :normal), & &1.family) |> Enum.take(2) == [
             "Boundary",
             "Boundary"
           ]
  end

  test "CSS face order and Unicode fallback preserve physical bytes and extracted text" do
    sans = Paths.fixture("fonts/selected-container-sans.ttf")
    mono = Paths.fixture("fonts/selected-container-mono.ttf")

    html = """
    <style>
    @font-face{font-family:Composite;src:url(sans.ttf);unicode-range:U+48}
    @font-face{font-family:Composite;src:url(mono.ttf);unicode-range:U+30,U+78}
    p{font-family:Composite}
    </style><p>Hx0</p>
    """

    opts = [
      assets: %{"sans.ttf" => {:file, sans}, "mono.ttf" => {:file, mono}},
      system_font_discovery: false
    ]

    assert {:ok, dom} = HtmlParser.parse_detailed(html)
    assert {:ok, styled} = Style.compute_detailed(dom, opts)
    assert {:ok, resolved} = FontFallback.resolve(styled, :error)
    [paragraph] = resolved.children
    assert Enum.map(paragraph.children, & &1.text) == ["H", "x0"]

    assert Enum.map(paragraph.children, & &1.style.font_face.data) == [
             File.read!(sans),
             File.read!(mono)
           ]

    assert {:ok, pdf} = HtmlToPdf.render(html, opts)
    assert {:ok, %{pages: [%{spans: spans}]}} = Text.extract_spans(pdf)
    assert Enum.map_join(spans, & &1.text) == "Hx0"

    css =
      "@font-face{font-family:A;src:url(sans.ttf)}@font-face{font-family:A;src:url(mono.ttf)}p{font-family:A}"

    assert {:ok, dom} = HtmlParser.parse_detailed("<style>#{css}</style><p>Hx0</p>")
    assert {:ok, styled} = Style.compute_detailed(dom, opts)
    assert hd(styled.children).style.font_face.data == File.read!(mono)
  end

  test "inherited widths, relative fractional weights and shorthand reset share validation" do
    html = """
    <div style='font-weight:450.5;font-stretch:condensed;font-style:oblique 10deg'>
      <p style='font-weight:bolder'>Hx0</p>
      <p style='font:oblique 0.05turn 525 semi-expanded 12pt/2 serif'>Hx0</p>
      <p style='font:12pt serif'>Hx0</p>
    </div>
    """

    assert {:ok, dom} = HtmlParser.parse_detailed(html)

    assert {:ok, %{children: [parent]}} =
             Style.compute_detailed(dom, system_font_discovery: false)

    [first, second, third] = Enum.filter(parent.children, &(&1.type == :element))
    assert first.style.font_weight == 700 and first.style.font_width == 75
    assert first.style.font_oblique == 10
    assert second.style.font_weight == 525 and second.style.font_width == 112.5
    assert second.style.font_oblique == 18 and second.style.line_height == 24
    assert third.style.font_weight == 400 and third.style.font_width == 100
    assert third.style.font_style == :normal and is_nil(third.style.font_oblique)
  end

  test "descriptor and property parsers reject malformed boundaries without expanding Unicode ranges" do
    for {value, expected} <- [
          {"normal", 400},
          {"bold", 700},
          {1, 1},
          {"450.5", 450.5},
          {"1000", 1000}
        ] do
      assert {:ok, actual} = CssFontValidator.prepare_weight(value)
      assert actual == expected
    end

    # SVG's static font bridge must retain integer spelling for integral values.
    assert {:ok, 700} = CssFontValidator.prepare_weight("700.0")
    assert {:ok, 700} = CssFontValidator.prepare_weight(700.0)

    for {parent, expected} <- [{100, 100}, {550, 400}, {750, 700}] do
      assert {:ok, ^expected} = CssFontValidator.prepare_weight("lighter", parent)
    end

    for {parent, expected} <- [{100, 400}, {400, 700}, {700, 900}] do
      assert {:ok, ^expected} = CssFontValidator.prepare_weight("bolder", parent)
    end

    for value <- [false, "0", "1001", "2px", "NaN"] do
      assert :error = CssFontValidator.prepare_weight(value)
    end

    assert {:ok, 62.5} = CssFontValidator.prepare_width("extra-condensed")
    assert {:ok, 80.5} = CssFontValidator.prepare_width("80.5%")

    for value <- [nil, "0%", "narrow", "-1%", "20px"] do
      assert :error = CssFontValidator.prepare_width(value)
    end

    for {value, degrees} <- [{"oblique", 14}, {"oblique 10grad", 9.0}, {"oblique 0rad", 0.0}] do
      assert {:ok, %{style: :italic, oblique: {angle, angle}}} =
               CssFontValidator.prepare_style(value)

      assert angle == degrees
    end

    assert {:ok, %{oblique: {-10.0, 20.0}}} =
             CssFontValidator.prepare_style("oblique -10deg 20deg", true)

    for value <- [
          false,
          "oblique 91deg",
          "oblique 1px",
          "italic 1deg",
          "oblique 20deg 10deg",
          "oblique 1deg 2deg"
        ] do
      assert :error = CssFontValidator.prepare_style(value)
    end

    assert {:ok, [{0, 0x10FFFF}]} = CssFontValidator.prepare_unicode_range("U+??????")

    assert {:ok, [{0, 255}, {0x400, 0x4FF}, {0x10000, 0x10000}]} =
             CssFontValidator.prepare_unicode_range("u+00-ff,U+4??,U+10000")

    for value <- [nil, "", "U+FF-0", "U+110000", "U+???????", "U+?0", "U+0,"] do
      assert :error = CssFontValidator.prepare_unicode_range(value)
    end

    for value <- ["bolder", "lighter"] do
      assert :error = CssFontValidator.prepare_absolute_weight(value)
    end

    for value <- ["500 400", "100 200 300"] do
      assert :error =
               CssFontValidator.prepare_range(value, &CssFontValidator.prepare_absolute_weight/1)
    end

    for properties <- [
          %{weight_range: {500, 400}},
          %{width_range: {0, 100}},
          %{weight_range: {1, 2, 3}},
          %{oblique: {0, 1}},
          %{style: :italic, oblique: {20, 10}},
          %{oblique: :bad},
          %{unicode_range: []},
          %{unicode_range: [{20, 10}]},
          %{unicode_range: [:bad]},
          %{unicode_range: [{1, 2} | :bad]},
          %{unicode_range: false}
        ] do
      assert :error = CssFontValidator.prepare_face_properties(properties)
    end

    assert {:ok, %{weight_range: {1, 1000}, width_range: {75, 125}, oblique: {-10, 20}}} =
             CssFontValidator.prepare_face_properties(%{
               weight_range: {1, 1000},
               width_range: {75, 125},
               style: :italic,
               oblique: {-10, 20}
             })

    for tokens <- [
          ["italic", "italic"],
          ["bold", "500"],
          ["expanded", "condensed"],
          ["normal", "normal", "normal", "normal", "normal"],
          ["oblique", "95deg"]
        ] do
      assert :error = CssFontValidator.prepare_shorthand_prefix(tokens)
    end

    assert {:ok, %{style: "italic"}} =
             CssFontValidator.prepare_shorthand_prefix(["normal", "italic"])

    assert {:ok, %{style: "oblique"}} = CssFontValidator.prepare_shorthand_prefix(["oblique"])

    assert {:error, {:invalid_css, _}} = HtmlToPdf.render("<p style='font-stretch:0%'>Hx0</p>")

    assert {:ok, %{width: 75.0}} =
             CssFontFaceValidator.validate([
               {"font-family", "A"},
               {"src", "url(a.ttf)"},
               {"font-width", "75%"}
             ])

    assert {:ok, face} =
             CssFontFaceValidator.validate([
               {"font-family", "A"},
               {"src", "url(a.ttf)"},
               {"font-weight", "100 900"},
               {"font-stretch", "75% 125%"},
               {"unicode-range", "U+4??"}
             ])

    assert face.weight_range == {100, 900} and face.width_range == {75, 125}

    assert {:error, %{property: "font-width"}} =
             CssFontFaceValidator.validate([
               {"font-family", "A"},
               {"src", "url(a.ttf)"},
               {"font-width", "wrong"}
             ])
  end
end
