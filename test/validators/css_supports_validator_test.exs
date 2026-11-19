defmodule NativeElixirPdfUtilities.Validators.CssSupportsValidatorTest do
  use ExUnit.Case, async: true
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

  test "selector support requires one complex selector with all nested branches implemented" do
    for selector <- [
          "p, span",
          "p,",
          ":where(p, :unknown)",
          ":is(:is(p, :unknown))",
          ":has(> :where(p, :unknown))",
          ":nth-child(1 of :is(p, :unknown))",
          ":is(p, ::before)",
          "p::first-line",
          "p:first-line",
          "::nth-fragment(2)",
          "::first-letter::prefix"
        ] do
      refute CssSupportsValidator.matches?("selector(#{selector})"), selector
    end

    for selector <- [
          "p :is(em, span)",
          ~S([title="x,y"]),
          ~S(.a\,b),
          "p::before",
          "p::first-letter",
          "p:where(:not(.x, .y))"
        ] do
      assert CssSupportsValidator.matches?("selector(#{selector})"), selector
    end

    assert CssSupportsValidator.matches?("not selector(p, span)")
    assert CssSupportsValidator.matches?("selector(p, span) or selector(p)")

    assert CssStyles.by_id("<p id='a'>ordinary</p>", [":is(:unknown, p){color:blue}"])["a"].color ==
             {0, 0, 1}
  end

  test "conditional selectors use the declaring stylesheet's namespace bindings" do
    # CSS Conditional 4 §2 uses declared attribute prefixes in selector().
    css = """
    @namespace h url(http://www.w3.org/1999/xhtml);
    @namespace x url(http://www.w3.org/1999/xlink);
    @supports selector(h|p[x|href]) {
      @page {margin:7pt}
      @font-face {font-family:Local;src:url(local.ttf)}
      p {color:blue}
    }
    @supports selector(unknown|p) {p{color:red}}
    """

    assert {:ok, %{rules: [rule]}} = CssParser.prepare_stylesheet(css)
    assert rule.declarations == [{"color", "blue"}]
    assert {:ok, prepared} = CssParser.prepare_source_stylesheet(css)
    assert Enum.map(prepared.authored_media, & &1.active) == [true, false]
    assert {:ok, [_]} = CssParser.font_faces(css)
    assert {:ok, [margin: "7pt"]} = CssParser.page_options(css)

    function = "result:10pt; @supports selector(h|p) {result:20pt}"

    assert {:ok, %{"result" => "10pt"}} =
             NativeElixirPdfUtilities.Validators.CssFunctionValidator.body(function, nil, %{})

    nodes =
      CssStyles.by_id("<p id='a'>Text</p>", [
        """
        @namespace h url(http://www.w3.org/1999/xhtml);
        @function --height() {result:10pt; @supports selector(h|p) {result:20pt}}
        @when supports(selector(h|p)) {p{color:blue}}
        p{width:if(supports(selector(h|p)):30pt; else:11pt);height:--height()}
        """
      ])

    assert nodes["a"].color == {0, 0, 1}
    assert nodes["a"].width == 30.0
    assert nodes["a"].height == 20.0
  end

  test "a recognized pseudo without a static paint owner leaves its guarded rule inactive" do
    html =
      "<style>@supports selector(p::first-line){p::first-line{color:red}}</style><p>Fallback</p>"

    assert {:ok, pdf} = NativeElixirPdfUtilities.HtmlToPdf.render(html)
    assert {:ok, "Fallback"} = NativeElixirPdfUtilities.Text.extract(pdf)
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
