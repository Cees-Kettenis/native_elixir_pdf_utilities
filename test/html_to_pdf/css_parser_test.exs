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
end
