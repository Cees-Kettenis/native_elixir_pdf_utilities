defmodule NativeElixirPdfUtilities.HtmlToPdf.CssParserTest do
  use ExUnit.Case

  alias NativeElixirPdfUtilities.HtmlToPdf.CssParser

  test "parse accepts strict selector groups and declarations" do
    assert {:ok, [rule]} =
             CssParser.parse("p, .copy, #intro, p.lead { color: red; margin-bottom: 4pt }")

    assert rule.declarations == [{"color", "red"}, {"margin-bottom", "4pt"}]
    assert rule.order == 0

    assert Enum.map(rule.selectors, & &1.specificity) == [
             {0, 0, 1},
             {0, 1, 0},
             {1, 0, 0},
             {0, 1, 1}
           ]
  end

  test "parse accepts descendant and child selectors" do
    assert {:ok, [rule]} = CssParser.parse("table > tbody td.note { color: blue; }")
    [selector] = rule.selectors

    assert Enum.map(selector.parts, &{&1.tag, &1.classes, &1.combinator}) == [
             {"table", [], nil},
             {"tbody", [], :child},
             {"td", ["note"], :descendant}
           ]
  end

  test "parse accepts first-child pseudo selectors" do
    assert {:ok, [rule]} =
             CssParser.parse(".address-section > .section p:first-child { margin-top: 0; }")

    [selector] = rule.selectors

    assert selector.specificity == {0, 3, 1}

    assert Enum.map(selector.parts, &{&1.tag, &1.classes, &1.pseudo_classes, &1.combinator}) == [
             {nil, ["address-section"], [], nil},
             {nil, ["section"], [], :child},
             {"p", [], [:first_child], :descendant}
           ]
  end

  test "parse accepts print template universal and child pseudo selectors" do
    assert {:ok, [universal_rule, last_rule, nth_rule]} =
             CssParser.parse("""
             * { box-sizing: border-box; }
             .sheet:last-child { page-break-after: auto; }
             .left-side div:nth-child(2) { display: flex; }
             """)

    [universal_selector] = universal_rule.selectors
    assert universal_selector.specificity == {0, 0, 0}
    assert hd(universal_selector.parts).tag == nil

    [last_selector] = last_rule.selectors
    assert hd(last_selector.parts).pseudo_classes == [:last_child]

    [nth_selector] = nth_rule.selectors
    assert List.last(nth_selector.parts).pseudo_classes == [{:nth_child, 2}]
  end

  test "parse accepts root custom properties and important declarations" do
    assert {:ok, [root_rule, hidden_rule]} =
             CssParser.parse("""
             :root { --row-height: 8rem; }
             .none { display: none !important; }
             """)

    [root_selector] = root_rule.selectors
    assert root_rule.declarations == [{"--row-height", "8rem"}]
    assert hd(root_selector.parts).pseudo_classes == [:root]

    assert hidden_rule.declarations == [{"display", "none", :important}]
  end

  test "parse accepts simple page rules outside the style cascade" do
    assert {:ok, [rule]} =
             CssParser.parse("""
             @page { size: A4 landscape; margin: 7mm; }
             .sheet { height: 196mm; }
             """)

    assert rule.declarations == [{"height", "196mm"}]
    [selector] = rule.selectors
    assert hd(selector.parts).classes == ["sheet"]
  end

  test "parse activates print media rules and skips non-print rules" do
    assert {:ok, [base_rule, print_rule]} =
             CssParser.parse("""
             p { color: black; }
             @media screen { p { color: blue; } }
             @media only print { p { color: red; } }
             """)

    assert base_rule.declarations == [{"color", "black"}]
    assert print_rule.declarations == [{"color", "red"}]

    assert {:ok, [all_rule]} = CssParser.parse("@media all { p { color: green; } }")
    assert all_rule.declarations == [{"color", "green"}]

    assert CssParser.parse("@media print and (color) { p { color: red; } }") ==
             {:ok, []}

    assert CssParser.parse("@media print { @media print { p { color: red; } } }") ==
             {:error, :invalid_css}
  end

  test "font_faces extracts supported local TrueType and OpenType sources" do
    assert {:ok, [regular, italic]} =
             CssParser.font_faces("""
             @font-face {
               font-family: "Report Sans";
               src: local("Report Sans"), url('../fonts/report.woff2') format('woff2'), url("../fonts/report.ttf") format("truetype");
               font-display: swap;
             }
             @media print {
               @font-face {
                 font-family: 'Report Sans';
               src: url(../fonts/report.otf) format(opentype);
                 font-weight: 500;
                 font-style: italic;
               }
             }
             """)

    assert regular == %{
             family: "Report Sans",
             sources: ["../fonts/report.ttf"],
             weight: 400,
             style: :normal
           }

    assert italic == %{
             family: "Report Sans",
             sources: ["../fonts/report.otf"],
             weight: 500,
             style: :italic
           }

    assert {:ok, [fallback]} =
             CssParser.font_faces(
               "@font-face { font-family: Fallback; src: url(missing.ttf), url(valid.otf) format(opentype); }"
             )

    assert fallback.sources == ["missing.ttf", "valid.otf"]

    assert {:ok, [comma_path]} =
             CssParser.font_faces(
               "@font-face { font-family: CommaPath; src: url(\"report,sans.ttf\") format(\"truetype\"); }"
             )

    assert comma_path.sources == ["report,sans.ttf"]

    assert {:ok, [normal]} =
             CssParser.font_faces(
               "@font-face { font-family: Normal; src: url(normal.ttf); font-weight: normal; font-style: normal; }"
             )

    assert normal.weight == 400
    assert normal.style == :normal

    assert {:ok, [bold]} =
             CssParser.font_faces(
               "@font-face { font-family: Bold; src: url(bold.ttf); font-weight: bold; }"
             )

    assert bold.weight == 700

    assert {:ok, []} = CssParser.parse("@font-face { font-family: X; src: url(x.ttf); }")
  end

  test "font_faces rejects remote, WOFF-only, and malformed declarations" do
    for css <- [
          "@font-face { font-family: X; src: url(https://example.com/x.ttf); }",
          "@font-face { font-family: X; src: url(x.woff2) format(woff2); }",
          "@font-face { font-family: X; src: url(x.png) format(truetype); }",
          "@font-face { font-family: X; font-weight: bold; }",
          "@font-face { font-family: ''; src: url(x.ttf); }",
          "@font-face { src: url(x.ttf); }",
          "@font-face { font-family: X; src: url(x.ttf); font-style: oblique; }",
          "@font-face { font-family: X; src: url(x.ttf); font-style: italic !important; }",
          "@font-face { font-family: X; src: url(x.ttf); font-weight: 950; }",
          "@font-face { font-family: X; src: url(x.ttf); unicode-range: U+0-FF; }",
          "@font-face { font-family: X; src: url(x.ttf); font-display: eager; }"
        ] do
      assert CssParser.font_faces(css) == {:error, :invalid_css}
      assert CssParser.parse(css) == {:error, :invalid_css}
    end

    assert CssParser.font_faces(:not_css) == {:error, :invalid_css}

    assert CssParser.font_faces("@media print { @media print { p { color: red; } } }") ==
             {:error, :invalid_css}

    assert {:error,
            {:invalid_css,
             %{
               source: "src: url(x.woff2) format(woff2)",
               message:
                 "line 1: @font-face declaration \"src: url(x.woff2) format(woff2)\" is invalid or unsupported"
             }}} =
             CssParser.parse_detailed(
               "@font-face { font-family: X; src: url(x.woff2) format(woff2); }"
             )

    assert {:error,
            {:invalid_css,
             %{
               source: "font-family X",
               message:
                 "line 1: @font-face declaration \"font-family X\" is invalid or unsupported"
             }}} =
             CssParser.parse_detailed("@font-face { font-family X; src: url(valid.ttf); }")

    assert {:error,
            {:invalid_css,
             %{
               source: "@font-face",
               message: "line 1: @font-face is missing required \"src\" descriptor"
             }}} =
             CssParser.parse_detailed("@font-face { font-family: X; }")
  end

  test "page_options extracts supported page size and margin defaults" do
    assert {:ok, options} =
             CssParser.page_options("""
             @page { size: A4 landscape; margin: 7mm; }
             @page rotated { size: letter portrait; margin: 0; }
             """)

    assert Keyword.fetch!(options, :page_size) == :letter
    assert Keyword.fetch!(options, :margin) == 0.0

    assert {:ok, landscape_options} =
             CssParser.page_options("@page { size: landscape A4; margin: 10px; }")

    assert Keyword.fetch!(landscape_options, :page_size) == {841.89, 595.28}
    assert Keyword.fetch!(landscape_options, :margin) == "10px"

    for {size, expected} <- [
          {"A4", :a4},
          {"A4 portrait", :a4},
          {"portrait A4", :a4},
          {"letter", :letter},
          {"portrait letter", :letter},
          {"letter landscape", {792.0, 612.0}},
          {"landscape letter", {792.0, 612.0}}
        ] do
      assert {:ok, options} = CssParser.page_options("@page { size: #{size}; }")
      assert Keyword.fetch!(options, :page_size) == expected
    end

    assert CssParser.page_options("""
           @page { size: tabloid; margin: auto; color: red; }
           @page broken { color }
           """) == {:ok, []}

    assert CssParser.page_options(:not_css) == {:error, :invalid_css}

    assert {:ok, print_page_options} =
             CssParser.page_options("@media print { @page { size: letter; margin: 0; } }")

    assert Keyword.fetch!(print_page_options, :page_size) == :letter
    assert Keyword.fetch!(print_page_options, :margin) == 0.0
  end

  test "parse_declarations normalizes inline declaration blocks" do
    assert CssParser.parse_declarations(" COLOR : #336699 ; font-weight: bold ") ==
             {:ok, [{"color", "#336699"}, {"font-weight", "bold"}]}

    assert CssParser.parse_declarations("display: none !important") ==
             {:ok, [{"display", "none", :important}]}
  end

  test "parse rejects malformed CSS" do
    assert CssParser.parse("p color: red;") == {:error, :invalid_css}
    assert CssParser.parse("p { color: red; } dangling") == {:error, :invalid_css}
    assert CssParser.parse("{}") == {:error, :invalid_css}
    assert CssParser.parse("p, { color: red; }") == {:error, :invalid_css}
    assert CssParser.parse("> p { color: red; }") == {:error, :invalid_css}
    assert CssParser.parse("p > { color: red; }") == {:error, :invalid_css}
    assert CssParser.parse("p > span > { color: red; }") == {:error, :invalid_css}
    assert CssParser.parse("p > { color: red; }") == {:error, :invalid_css}
    assert CssParser.parse("123 { color: red; }") == {:error, :invalid_css}
    assert CssParser.parse("p#one#two { color: red; }") == {:error, :invalid_css}
    assert CssParser.parse("p:active { color: red; }") == {:error, :invalid_css}
    assert CssParser.parse("p { : red; }") == {:error, :invalid_css}
    assert CssParser.parse("p { color: ; }") == {:error, :invalid_css}
    assert CssParser.parse("p { color }") == {:error, :invalid_css}
    assert CssParser.parse("") == {:ok, []}
    assert CssParser.parse(:not_css) == {:error, :invalid_css}
    assert CssParser.parse_declarations(:not_css) == {:error, :invalid_css}
  end

  test "parse_detailed returns source details for malformed CSS" do
    assert {:error,
            {:invalid_css,
             %{
               stage: :css,
               reason: :invalid_css,
               line: 1,
               column: 1,
               source: "",
               message: ~s(line 1: declaration "" is invalid or unsupported)
             }}} = CssParser.parse_detailed("p {}")

    assert {:error,
            {:invalid_css,
             %{
               stage: :css,
               reason: :invalid_css,
               line: 1,
               source: "",
               message: ~s(line 1: declaration "" is invalid or unsupported)
             }}} = CssParser.parse_detailed("p { color: red; } div {}")

    assert {:error,
            {:invalid_css,
             %{
               stage: :css,
               reason: :invalid_css,
               message: "CSS input must be a string"
             }}} = CssParser.parse_detailed(:not_css)

    assert {:error,
            {:invalid_css,
             %{
               stage: :css,
               reason: :invalid_css,
               message: "CSS declaration input must be a string"
             }}} = CssParser.parse_declarations_detailed(:not_css)
  end
end
