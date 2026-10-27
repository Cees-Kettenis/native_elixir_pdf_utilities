defmodule NativeElixirPdfUtilities.HtmlToPdf.AuthoredCssSourceTest do
  use ExUnit.Case, async: false

  alias NativeElixirPdfUtilities.Limits
  alias NativeElixirPdfUtilities.HtmlToPdf.{CssParser, HtmlParser, Style}

  alias NativeElixirPdfUtilities.Validators.{
    CssFontFaceValidator,
    CssPageValidator,
    CssReferenceValidator,
    HtmlValidator
  }

  setup do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    :ok
  end

  test "all media rules retain repeated original declaration spans across comments" do
    css =
      "/* é\n */p{color/**/:red}\n@media screen { p { color: var(--inherited); } }\n@media only print { p { color/**/:blue; } }\np { color/**/:blue; }"

    assert {:ok, prepared} = CssParser.prepare_source_stylesheet(css)
    assert {:ok, ordinary} = CssParser.prepare_stylesheet(css)

    assert Enum.map(prepared.rules, &Map.take(&1, [:selectors, :declarations, :order])) ==
             ordinary.rules

    assert Enum.map(prepared.authored_rules, & &1.active) == [true, false, true, true]
    assert Enum.map(prepared.authored_media, & &1.active) == [false, true]
    assert prepared.media_policy == :unresolved_provider_context

    sources =
      for rule <- prepared.authored_rules, record <- rule.source_declarations, do: record.source

    assert Enum.map(sources, & &1.text) == [
             "color/**/:red",
             "color: var(--inherited)",
             "color/**/:blue",
             "color/**/:blue"
           ]

    assert Enum.map(sources, & &1.line) == [2, 3, 4, 5]
    assert List.last(sources).column == 5
    assert Enum.uniq(Enum.map(sources, & &1.offset)) |> length() == 4

    for source <- sources,
        do: assert(binary_part(css, source.offset, source.length) == source.text)
  end

  test "comment markers and at-rule names inside strings remain ordinary authored values" do
    css = ~S|p::before{content:'/* @media print {} */';color:red}|
    assert {:ok, source} = CssParser.prepare_source_stylesheet(css)
    assert source.authored_media == []
    assert [rule] = source.authored_rules
    assert [content, color] = rule.source_declarations
    assert content.source.text == ~S|content:'/* @media print {} */'|
    assert color.source.text == "color:red"
    assert {:ok, empty} = CssParser.prepare_source_stylesheet("/* only a comment */")
    assert empty.rules == [] and empty.authored_rules == []
    assert {:error, {:invalid_css, _}} = CssParser.prepare_source_stylesheet(:invalid)
  end

  test "matched cross-rule inherited failures identify the declaration without replay" do
    css =
      "/* untouched */html { --bad:bananas }\np {width:3pt}\n@media print { p.target { width/**/:var(--bad); } }"

    assert {:ok, dom} = HtmlParser.parse_document_detailed("<p class='target'>Text</p>")
    budget = HtmlValidator.new_css_budget()

    assert {:error, {:invalid_css, diagnostic}} =
             Style.with_source_style_contexts(dom, [css], budget, fn _, _, _ -> :ok end)

    assert diagnostic.stage == :css and diagnostic.reason == :invalid_css
    assert diagnostic.source == "width/**/:var(--bad)"
    assert diagnostic.line == 3 and diagnostic.column == 27
    assert diagnostic.operation == :source_style_contexts
    # Existing original html/head/body/p custom contexts plus the two p values.
    # Mapping the failure does not resolve the failed value a second time.
    assert :atomics.get(budget, 1) == 45
  end

  test "unmatched and inactive inherited values remain explicit without invented context" do
    css = "@media screen{p{width:var(--host)}}.absent{height:var(--host)}p{color:red}"
    assert {:ok, dom} = HtmlParser.parse_document_detailed("<p>Text</p>")

    assert {:ok, result} =
             Style.with_source_style_contexts(dom, [css], HtmlValidator.new_css_budget(), fn _,
                                                                                             _,
                                                                                             context ->
               assert context.status == :resolved
               :ok
             end)

    [authored] = result.source_obligations.authored
    assert [inactive, unmatched, _] = authored.authored_rules
    assert inactive.active == false and unmatched.active == true

    for rule <- [inactive, unmatched], record <- rule.source_declarations do
      assert record.context_policy == :requires_applicable_original_context
    end

    assert result.source_obligations.unmatched_rules == :unresolved
    assert result.source_obligations.inactive_media == :unresolved
  end

  test "all font candidates retain superseded unsupported escaped and fallback roles" do
    css =
      "@media screen { @font-face {font-family:X;src:url('old.ttf');src:local('X'),url('skip.woff2') format('woff2'),url('first.ttf'),url('a\\2c b.otf') format(opentype);font-display:swap} }"

    assert {:ok, prepared} = CssParser.prepare_source_stylesheet(css)
    assert prepared.font_faces == []
    assert [font] = prepared.authored_fonts
    refute font.active
    assert font.face.sources == ["first.ttf", ~S|a\2c b.otf|]

    assert Enum.map(font.candidates, & &1.role) == [
             :superseded,
             :unsupported,
             :unsupported,
             :fallback_candidate,
             :fallback_candidate
           ]

    assert Enum.at(font.candidates, 1).reference_policy == :unresolved_syntax
    assert List.last(font.candidates).decoded_reference == "a,b.otf"

    for candidate <- font.candidates do
      assert candidate.authorization == :unresolved and candidate.contents == :unresolved

      assert binary_part(css, candidate.source.offset, candidate.source.length) ==
               candidate.source.text
    end

    assert {:ok, info} =
             CssFontFaceValidator.validate_source_candidate("url(https://example.org/font.ttf)")

    assert info.reference_policy == :static_url_syntax_only

    assert {:ok, font.face} ==
             CssFontFaceValidator.validate(Enum.map(font.declarations, & &1.declaration))
  end

  test "candidate provenance is confined to its own original descriptor" do
    css = ~S|@font-face{font-family:"url('repeat.ttf')";src:url('repeat.ttf')}|
    assert {:ok, source} = CssParser.prepare_source_stylesheet(css)
    [font] = source.authored_fonts
    [candidate] = font.candidates
    assert candidate.source.text == "url('repeat.ttf')"
    assert candidate.source.offset == 47
    assert candidate.declaration_source.text == "src:url('repeat.ttf')"
  end

  test "pure original URL syntax checks also cover inactive unmatched and discarded references" do
    for css <- [
          ~S|@media screen{.absent{--background:url('\6a avascript:evil')}}|,
          ~S|.absent{background-image:url('javascript:evil')}|,
          ~S|@font-face{font-family:X;src:url('javascript:evil.ttf');src:url('safe.ttf')}|,
          ~S|@font-face{font-family:X;src:url('javascript:evil.woff2') format(woff2),url('safe.ttf')}|
        ] do
      assert {:error, {reason, diagnostic}} = CssParser.prepare_source_stylesheet(css)
      assert is_atom(reason) and diagnostic.stage == :css
      assert is_binary(diagnostic.message) and diagnostic.message != ""
      assert diagnostic.source =~ "url("
      assert diagnostic.line == 1 and diagnostic.column > 1
    end

    assert {:ok, prepared} =
             CssParser.prepare_source_stylesheet(
               ~S|.absent{background-image:url('https://example.org/x.png')}|
             )

    [reference] = hd(hd(prepared.authored_rules).source_declarations).references
    assert reference.policy == :static_url_syntax_only and reference.authorization == :unresolved
    assert reference.contents == :unresolved
  end

  test "inactive font and page blocks preserve source while ordinary rendering selection survives" do
    css =
      "@media screen{@font-face{font-family:X;src:url('x.ttf')}@page{size:letter;margin:0}}@media print{@page{size:a4;margin:2pt}}"

    assert {:ok, source} = CssParser.prepare_source_stylesheet(css)
    assert {:ok, ordinary} = CssParser.prepare_stylesheet(css)
    assert source.page_options == ordinary.page_options
    assert Enum.map(source.authored_pages, & &1.active) == [false, true]
    assert [font] = source.authored_fonts
    assert font.source.text == "@font-face{font-family:X;src:url('x.ttf')}"

    assert {:error, {:invalid_css, font_error}} =
             CssParser.prepare_source_stylesheet(
               "@media screen{@font-face{font-family:X;src:url(x.woff2)}}"
             )

    assert font_error.source == "src:url(x.woff2)"

    assert {:error, {:invalid_css, page_error}} =
             CssParser.prepare_source_stylesheet("@media screen{@page{margin:bananas}}")

    assert page_error.source == "margin:bananas"

    assert {:error, {:invalid_css, syntax_error}} =
             CssParser.prepare_source_stylesheet("@media screen{p{broken}}")

    assert syntax_error.stage == :css

    assert {:ok, nested} =
             CssParser.prepare_source_stylesheet("@media print{@media print{p{color:red}}}")

    assert length(nested.authored_media) == 2
    assert length(nested.rules) == 1
  end

  test "original grammar and reference failures retain precise source details" do
    for {css, source, line, column} <- [
          {"/* first */\n@media screen{@font-face{font-family X;src:url(x.ttf)}}",
           "font-family X", 2, 26},
          {"/* first */\n@font-face{font-family:X}", "@font-face{font-family:X}", 2, 1},
          {"/* first */\np{background-image:url()}", "url()", 2, 20}
        ] do
      assert {:error, {_reason, diagnostic}} = CssParser.prepare_source_stylesheet(css)
      assert diagnostic.stage == :css and diagnostic.source == source
      assert diagnostic.line == line and diagnostic.column == column
      assert diagnostic.message != ""
    end
  end

  test "original inline reference failures precede expansion and preserve ordinary tuple diagnostics" do
    assert {:ok, dom} =
             HtmlParser.parse_document_detailed(
               "<p style=\"--url:url('javascript:run()')\">Text</p>"
             )

    css = "/* original */\np{background-image:var(--url)}"

    assert {:error, {:unsupported_html, diagnostic}} =
             Style.with_source_style_contexts(
               dom,
               [css],
               HtmlValidator.new_css_budget(),
               fn _, _, _ -> :ok end
             )

    # Inline original references now share the rule-source static URL policy.
    # The authored malicious token fails before the inherited use is expanded.
    assert diagnostic.source == "url('javascript:run()')"
    assert diagnostic.line == 1 and diagnostic.column == 17
    assert diagnostic.operation == :source_references
    assert diagnostic.module == CssReferenceValidator

    assert {:error, {:unsupported_html, diagnostic}} =
             NativeElixirPdfUtilities.Validators.CssValueValidator.validate_source_cascade(
               %{_custom_properties: %{}},
               [{"background-image", "url('javascript:run()')"}],
               HtmlValidator.new_css_budget(),
               dom.children |> List.last()
             )

    assert diagnostic.source == "background-image" and diagnostic.stage == :css
  end

  test "font failures use the validator selected offending descriptor identity" do
    for {css, snippet, column} <- [
          {"@font-face{font-family:X;font-style:italic!important;font-style:normal;src:url(x.ttf)}",
           "font-style:italic!important", 26},
          {"@font-face{font-family:X;src:url(x.ttf);src:url(x.woff2)}", "src:url(x.woff2)", 41}
        ] do
      assert {:error, {:invalid_css, diagnostic}} = CssParser.prepare_source_stylesheet(css)
      assert diagnostic.source == snippet and diagnostic.line == 1 and diagnostic.column == column
    end
  end

  test "authored source work stays in the enclosing invocation budget" do
    css = "p{color:red}"
    assert {:ok, _} = CssParser.prepare_source_stylesheet(css)

    Limits.install(
      Map.put(
        Limits.effective(),
        :max_aggregate_css_source_bytes,
        byte_size(css) + byte_size("color:red")
      )
    )

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             HtmlValidator.with_render_budget(fn ->
               assert {:ok, _} = CssParser.prepare_source_stylesheet(css)
               CssParser.prepare_source_stylesheet(css)
             end)

    assert diagnostic.message =~ "max_aggregate_css_source_bytes"
    assert diagnostic.stage == :css
    Limits.install(Map.put(Limits.effective(), :max_css_work, 1))

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             CssParser.prepare_source_stylesheet(css)

    assert diagnostic.message =~ "max_css_work"
  end

  test "superseded supported font values use the existing responsible descriptor rules" do
    for {property, bad, good} <- [
          {"font-family", "''", "X"},
          {"font-weight", "950", "400"},
          {"font-style", "oblique", "normal"}
        ] do
      declarations = [
        {"font-family", "X"},
        {property, bad},
        {property, good},
        {"src", "url(x.ttf)"}
      ]

      # Selection-only validation was already valid. The added original check
      # addresses the earlier supported descriptor, not selection or display.
      assert {:ok, _face} = CssFontFaceValidator.validate(declarations)

      assert {:error, %{property: ^property, index: 1}} =
               CssFontFaceValidator.validate_authored(declarations)

      css =
        "@media screen{@font-face{font-family:X;#{property}:#{bad};#{property}:#{good};src:url(x.ttf)}}"

      assert {:error, {:invalid_css, diagnostic}} = CssParser.prepare_source_stylesheet(css)
      assert diagnostic.source == "#{property}:#{bad}"
      assert diagnostic.stage == :css and diagnostic.line == 1 and diagnostic.column == 40
      assert diagnostic.message =~ "@font-face"

      assert {:ok, dom} =
               HtmlParser.parse_document_detailed("<template><style>#{css}</style></template>")

      assert {:error, {:invalid_css, diagnostic}} =
               Style.with_source_style_contexts(dom, [], HtmlValidator.new_css_budget(), fn _,
                                                                                            _,
                                                                                            _ ->
                 :ok
               end)

      assert diagnostic.source == "#{property}:#{bad}" and diagnostic.column == 57
    end

    assert {:ok, prepared} =
             CssParser.prepare_source_stylesheet(
               "@font-face{font-family:A;font-family:B;font-weight:100;font-weight:400;font-style:italic;font-style:normal;src:url(x.ttf)}"
             )

    [font] = prepared.authored_fonts
    assert font.face == %{family: "B", sources: ["x.ttf"], weight: 400, style: :normal}
    assert font.policy == :authored_descriptor_values_and_selection
    assert font.runtime == [:authorization, :font_bytes, :font_metrics]
  end

  test "page values have one validator owner and retain every original declaration" do
    css =
      "@media screen{@page{margin:1pt;margin-left:2pt;marks:crop;size:var(--page);color:var(--color)}}@page{size:a5;margin:3pt}"

    assert {:ok, source} = CssParser.prepare_source_stylesheet(css)
    [inactive, active] = source.authored_pages
    assert inactive.policy == :validator_owned_page_declarations

    assert Enum.map(inactive.declarations, & &1.source.text) == [
             "margin:1pt",
             "margin-left:2pt",
             "marks:crop",
             "size:var(--page)",
             "color:var(--color)"
           ]

    assert Enum.map(inactive.declarations, & &1.value_policy) == [
             :checked_static_page_value,
             :checked_static_page_value,
             :checked_page_descriptor_syntax,
             :requires_page_context,
             :requires_page_context
           ]

    assert Enum.all?(inactive.declarations, &(&1.context_policy == :page_context))
    refute inactive.active
    assert active.active
    assert source.page_options == [margin: "3pt", page_size: "a5"]
    assert {:ok, ordinary} = CssParser.prepare_stylesheet(css)
    assert source.page_options == ordinary.page_options

    for page <- source.authored_pages, record <- page.declarations do
      assert binary_part(css, record.source.offset, record.source.length) == record.source.text
      assert {:ok, _} = CssPageValidator.validate_declaration(record.declaration)
    end

    assert {:ok, %{policy: :checked_static_page_value}} =
             CssPageValidator.validate_declaration({"margin-left", "4pt", :important})

    assert {:ok, %{policy: :checked_page_descriptor_syntax}} =
             CssPageValidator.validate_declaration({"size", "auto"})

    assert :error = CssPageValidator.validate_declaration({"margin", "bananas"})
  end

  test "page failure provenance selects the declaration instead of matching an earlier string" do
    css = "@page{font-family:'margin:bananas';\n  margin:bananas}"
    assert {:error, {:invalid_css, diagnostic}} = CssParser.prepare_source_stylesheet(css)
    assert diagnostic.module == CssPageValidator and diagnostic.source == "margin:bananas"
    assert diagnostic.line == 2 and diagnostic.column == 3
    html = "<template><style>#{css}</style></template>"
    assert {:ok, dom} = HtmlParser.parse_document_detailed(html)

    assert {:error, {:invalid_css, diagnostic}} =
             Style.with_source_style_contexts(dom, [], HtmlValidator.new_css_budget(), fn _,
                                                                                          _,
                                                                                          _ ->
               :ok
             end)

    assert diagnostic.module == CssPageValidator and diagnostic.line == 2 and
             diagnostic.column == 3

    assert {:error, {:invalid_css, diagnostic}} =
             CssParser.prepare_source_stylesheet("@media screen{@page{margin:1pt;\n  margin}}")

    assert diagnostic.source == "margin" and diagnostic.line == 2 and diagnostic.column == 3
  end

  test "escaped URL function names and inline fallbacks cannot hide executable original references" do
    for css <- [
          ~S|.absent{--x:u\72l('javascript:run()')}|,
          ~S|@media screen{.absent{background-image:U\000052L('javascript:run()')}}|,
          ~S|@page{background-image:u\72l('javascript:run()')}|,
          ~S|@font-face{font-family:X;src:u\72l('javascript:run()');src:url('safe.ttf')}|,
          ~S|@font-face{font-family:X;src:other(u\72l('javascript:run()'));src:url('safe.ttf')}|
        ] do
      assert {:error, {:unsupported_html, diagnostic}} = CssParser.prepare_source_stylesheet(css)
      assert diagnostic.stage == :css and diagnostic.operation == :source_references
      assert diagnostic.module == CssReferenceValidator
      assert diagnostic.source =~ "javascript:run()"
      assert diagnostic.line == 1 and diagnostic.column > 1
    end

    for value <- [
          ~S|--x:u\72l('javascript:run()')|,
          ~S|background-image:var(--x,u\72l('javascript:run()'))|
        ] do
      assert {:ok, dom} =
               HtmlParser.parse_document_detailed(
                 "<template><p style=\"#{value}\">Inert</p></template>"
               )

      assert {:error, {:unsupported_html, diagnostic}} =
               Style.with_source_style_contexts(dom, [], HtmlValidator.new_css_budget(), fn _,
                                                                                            _,
                                                                                            _ ->
                 :ok
               end)

      assert diagnostic.source == ~S|u\72l('javascript:run()')|
      assert diagnostic.line == 1 and diagnostic.column > 1
    end

    # URL text in strings and longer identifiers is opaque, including escapes.
    assert {:ok, []} =
             CssReferenceValidator.scan(
               ~S|content:"u\72l('javascript:run()')";--x:noturl('javascript:run()')|
             )

    assert {:ok, []} = CssReferenceValidator.scan(~S|content:'\'url("javascript:run()")'|)
  end

  test "escaped source references retain exact spans and unsupported font candidates remain unresolved" do
    css =
      ~S|.absent{--x:var(--missing,u\72l('a\2c b.png'));background-image:url('next.png')}@font-face{font-family:X;src:u\72l('old.ttf');src:url('safe.ttf')}|

    assert {:ok, source} = CssParser.prepare_source_stylesheet(css)
    [rule] = source.authored_rules
    [custom, background] = rule.source_declarations
    assert [reference] = custom.references
    assert reference.decoded == "a,b.png" and reference.policy == :static_url_syntax_only
    assert reference.source.text == ~S|u\72l('a\2c b.png')|
    assert reference.authorization == :unresolved and reference.contents == :unresolved
    assert hd(background.references).decoded == "next.png"
    [font] = source.authored_fonts
    [superseded, selected] = font.candidates
    refute superseded.supported

    assert superseded.role == :superseded and
             superseded.reference_policy == :static_url_syntax_only

    assert hd(superseded.source_references).source.text == ~S|u\72l('old.ttf')|
    assert selected.supported and selected.decoded_reference == "safe.ttf"
    assert font.face.sources == ["safe.ttf"]

    for reference <-
          custom.references ++
            background.references ++ superseded.source_references ++ selected.source_references do
      assert binary_part(css, reference.source.offset, reference.source.length) ==
               reference.source.text
    end

    html = ~S|<template><p style="--x:u\72l(&quot;/missing.png&quot;)">Inert</p></template>|
    assert {:ok, dom} = HtmlParser.parse_document_detailed(html)

    assert {:ok, result} =
             Style.with_source_style_contexts(dom, [], HtmlValidator.new_css_budget(), fn _,
                                                                                          _,
                                                                                          _ ->
               :ok
             end)

    [record] = result.source_obligations.inline |> Map.values() |> List.flatten()
    assert hd(record.references).source.text == ~S|u\72l(&quot;/missing.png&quot;)|
    assert hd(result.source_obligations.declaration_contexts).status == :unresolved
  end

  test "source reference failures use grapheme columns and share cumulative pre-scan limits" do
    source = "--x:'é🙂';\r\n  --y:u\\72l('javascript:run()')"
    assert {:error, {:unsupported_html, diagnostic}} = CssReferenceValidator.scan(source)
    assert diagnostic.source == ~S|u\72l('javascript:run()')|
    assert diagnostic.line == 2 and diagnostic.column == 7
    assert {:error, {:invalid_css, diagnostic}} = CssReferenceValidator.scan("url()")
    assert diagnostic.source == "url()" and diagnostic.line == 1 and diagnostic.column == 1
    assert {:error, {:invalid_css, _}} = CssReferenceValidator.scan("url('x.png') url(")
    assert {:error, {:invalid_css, _}} = CssReferenceValidator.scan(<<255>>)
    assert {:error, {:invalid_options, diagnostic}} = CssReferenceValidator.scan("none", :bad)
    assert diagnostic.source == "source_mapper"
    assert {:error, {:invalid_css, _}} = CssReferenceValidator.scan("url(calc(1))")
    assert {:error, {:invalid_css, _}} = CssReferenceValidator.scan("url ('x.png')")
    limits = Limits.effective()
    good = "url('safe.png')"
    Limits.install(Map.put(limits, :max_css_work, 4 * byte_size(good) - 1))
    assert {:error, {:resource_limit_exceeded, diagnostic}} = CssReferenceValidator.scan(good)
    assert diagnostic.message =~ "max_css_work" and diagnostic.stage == :css
    Limits.install(Map.put(limits, :max_css_work, 4 * byte_size(good) + 1))

    assert {:error, {:resource_limit_exceeded, _}} =
             HtmlValidator.with_render_budget(fn ->
               assert {:ok, [_]} = CssReferenceValidator.scan(good)
               CssReferenceValidator.scan(good)
             end)
  end
end
