defmodule NativeElixirPdfUtilities.Validators.CssSupportsValidatorTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.Validators.{CssSupportsValidator, HtmlValidator}
  alias NativeElixirPdfUtilities.HtmlToPdf.{CssParser, HtmlParser, Style}
  alias NativeElixirPdfUtilities.TestSupport.CssStyles

  test "static declaration and selector conditions use canonical supported syntax" do
    for query <- [
          "(color:red)",
          "( color: red )",
          "(width:var(--x))",
          "(--typed:bananas)",
          "selector(p:has(> span))",
          "((color:red) and (display:grid))",
          "not (unknown:value)",
          "(color:bad) or (display:grid)"
        ] do
      assert CssSupportsValidator.matches?(query), query
    end

    for query <- [
          "(unknown:value)",
          "(color:bananas)",
          "selector(:unknown)",
          "unknown(feature)",
          "(color:red) and (display:grid) or (width:1pt)",
          "not not (color:red)",
          "(color:red",
          "(color:red) xor (display:grid)",
          "(color:red) and (display:grid) and",
          "(color:red) and [invalid] next",
          "[invalid]",
          "color",
          "(font-weight:heavy)",
          "(fill:red)",
          ": (color:red)",
          "(color:red) and (color:red) [invalid] (color:red)",
          "(width:bananas)",
          ""
        ] do
      refute CssSupportsValidator.matches?(query), query
    end
  end

  test "conditional nesting selects declarations without activating inactive resources or layers" do
    nodes =
      CssStyles.by_id("<p class='parent' id='a'>Text</p>", [
        """
        @layer base,last;
        @supports (color:bad) { @layer last {#a {color:red}} @font-face {font-family:Inactive;src:url(missing.ttf)} }
        @supports (display:grid) { @layer base {.parent{color:blue;@media print {font-weight:bold}}} }
        """
      ])

    assert nodes["a"].color == {0, 0, 1}
    assert nodes["a"].font_weight == 700
  end

  test "inactive conditions retain original declaration obligations without pretending to compute them" do
    css = "@supports (unknown:value){p{width:var(--source)}}p{color:blue}"
    assert {:ok, prepared} = CssParser.prepare_source_stylesheet(css)
    assert [%{active: false}, %{active: true}] = prepared.authored_rules
    assert [%{declarations: [{"color", "blue"}]}] = prepared.rules
    assert {:ok, dom} = HtmlParser.parse_document_detailed("<p>Text</p>")

    assert {:ok, collected} =
             Style.with_source_style_contexts(dom, [css], HtmlValidator.new_css_budget(), fn _,
                                                                                             _,
                                                                                             _ ->
               :ok
             end)

    obligations = collected.source_obligations.declaration_contexts

    assert Enum.any?(
             obligations,
             &(&1.declaration == {"width", "var(--source)"} and &1.status == :checked_symbolically and
                 &1.contexts == [])
           )
  end
end
