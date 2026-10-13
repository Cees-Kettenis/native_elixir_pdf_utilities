defmodule NativeElixirPdfUtilities.HtmlToPdf.AuthoredCssSourceTest do
  use ExUnit.Case, async: false

  alias NativeElixirPdfUtilities.Limits
  alias NativeElixirPdfUtilities.HtmlToPdf.{CssParser, HtmlParser, Style}
  alias NativeElixirPdfUtilities.Validators.{CssFontFaceValidator, HtmlValidator}

  setup do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    :ok
  end

  test "all media rules retain repeated original declaration spans across comments" do
    css =
      "/* é\n */p{co/**/lor:red}\n@media screen { p { color: var(--inherited); } }\n@media only print { p { co/**/lor:blue; } }\np { co/**/lor:blue; }"

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
             "co/**/lor:red",
             "color: var(--inherited)",
             "co/**/lor:blue",
             "co/**/lor:blue"
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

  test "matched cross-rule inherited failures identify the actual declaration without replay" do
    css =
      "/* untouched */html { --bad:bananas }\np {width:3pt}\n@media print { p.target { wi/**/dth:var(--bad); } }"

    assert {:ok, dom} = HtmlParser.parse_document_detailed("<p class='target'>Text</p>")
    budget = HtmlValidator.new_css_budget()

    assert {:error, {:invalid_css, diagnostic}} =
             Style.with_source_style_contexts(dom, [css], budget, fn _, _, _ -> :ok end)

    assert diagnostic.stage == :css and diagnostic.reason == :invalid_css
    assert diagnostic.source == "wi/**/dth:var(--bad)"
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

  test "inherited reference failures map the actual declaration and preserve ordinary tuple diagnostics" do
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

    assert diagnostic.source == "background-image:var(--url)"
    assert diagnostic.line == 2 and diagnostic.column == 3
    assert diagnostic.operation == :source_style_contexts

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
end
