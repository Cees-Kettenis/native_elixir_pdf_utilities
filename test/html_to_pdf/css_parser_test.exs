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

  test "parse_declarations normalizes inline declaration blocks" do
    assert CssParser.parse_declarations(" COLOR : #336699 ; font-weight: bold ") ==
             {:ok, [{"color", "#336699"}, {"font-weight", "bold"}]}
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
