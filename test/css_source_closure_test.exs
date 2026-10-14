defmodule NativeElixirPdfUtilities.Validators.CssSourceClosureTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.Limits
  alias NativeElixirPdfUtilities.HtmlToPdf.{CssParser, HtmlParser, HtmlTree, Style}

  alias NativeElixirPdfUtilities.Validators.{
    CssValueValidator,
    CssSourceContextValidator,
    HtmlCssSourceValidator,
    CssFontFaceValidator,
    HtmlValidator,
    PrintMediaValidator,
    MediaConditionValidator,
    HtmlFileValidator,
    TemplatePatchValidator
  }

  @opts [system_font_discovery: false, compress_streams: false]
  setup do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    :ok
  end

  test "authored important literals and symbolic font keywords retain exact source grammar" do
    css = "width:3pt!important;font-size:math!important;accent-color:auto!important;fill:inherit"

    assert {:ok, records} =
             CssParser.prepare_source_declarations(css, HtmlCssSourceValidator.local(css))

    [literal, font, accent, unsupported] = records
    budget = HtmlValidator.new_css_budget()
    assert :ok = CssValueValidator.validate_source_literal(literal, budget)

    assert {:ok, %{kind: :font_keyword, dependencies: [:inherited_font]}} =
             CssValueValidator.prepare_unapplied_declaration(font, budget)

    assert {:ok, %{kind: :color, color: :auto}} =
             CssValueValidator.prepare_unapplied_declaration(accent, budget)

    assert :requires_element_context =
             CssValueValidator.prepare_unapplied_declaration(unsupported, budget)
  end

  test "a selected list image can be reset and malformed references remain errors" do
    assert {:ok, dom} =
             HtmlParser.parse_detailed(
               "<ul><li style=\"list-style-image:url('asset:unused');list-style-image:none\">Item</li></ul>"
             )

    resolver = fn request -> flunk("superseded list source fetched #{inspect(request)}") end

    assert {:ok, tree} =
             Style.compute_detailed(dom, Keyword.put(@opts, :asset_resolver, resolver))

    node = hd(hd(tree.children).children)
    assert node.style.list_style_image == nil
    refute Map.has_key?(node.style, :list_style_image_source)

    assert {:error, :invalid_document} =
             CssValueValidator.validate_resolved(node.style, "list-style-image", "url()")

    assert {:error, {:unsupported_html, diagnostic}} =
             CssValueValidator.validate_resolved(
               node.style,
               "list-style-image",
               "url('javascript:alert(1)')"
             )

    assert diagnostic.stage == :css
  end

  test "three-component background position uses the unshifted axis and rejects ambiguous forms" do
    assert {:ok, {{:percent, +0.0}, {:end, 4.0}}} =
             CssValueValidator.parse_two_axis_position("left bottom 4pt", nil, true)

    assert {:ok, {{:end, 3.0}, {:percent, +0.0}}} =
             CssValueValidator.parse_two_axis_position("top right 3pt", nil, true)

    assert :error = CssValueValidator.parse_two_axis_position("2pt right 3pt", nil, true)
    assert :error = CssValueValidator.parse_two_axis_position("bananas right 3pt", nil, true)
    assert :error = CssValueValidator.parse_two_axis_position("left center bottom", nil, true)
    assert :error = CssValueValidator.parse_two_axis_position("left bottom 4pt", nil, false)
  end

  test "noncaptured source selection has no authority or invented owner" do
    assert {:ok, dom} = HtmlParser.parse_document_detailed("<p>Actual</p>")
    assert {:ok, provenance} = HtmlCssSourceValidator.prepare(dom)
    assert {:ok, sheet} = CssParser.prepare_source_stylesheet(".absent{width:50%}")

    assert {:ok, :observed, [record]} =
             CssSourceContextValidator.with_context_obligations(
               [%{prepared: Map.put(sheet, :stylesheet_applicable, true)}],
               provenance,
               fn entries, _inline, _observe, select ->
                 [rule] = hd(entries).prepared.rules
                 assert :ok = select.(hd(rule.declarations), hd(hd(dom.children).children), nil)
                 {:ok, :observed}
               end
             )

    assert record.status == :checked_symbolically and record.contexts == []
  end

  test "an unbound legacy tree cannot claim a bounded original tokenizer prefix" do
    dom = %{
      type: :document,
      children: [%{type: :element, tag: "p", attributes: %{}, children: []}]
    }

    assert {:error, {:invalid_document, diagnostic}} =
             HtmlCssSourceValidator.prepare_prefix(dom, 1)

    assert diagnostic.stage == :html and
             diagnostic.message =~ "canonical original token identities"
  end

  test "font original resource recipes require prepared entries and preserve loader resource failures" do
    assert {:error, {:invalid_document, diagnostic}} =
             CssFontFaceValidator.prepare_original_resources([%{}])

    assert diagnostic.stage == :font and diagnostic.operation == :original_font_sources

    assert {:ok, sheet} =
             CssParser.prepare_source_stylesheet(
               "@font-face{font-family:Source;src:url('source.ttf')}"
             )

    assert {:ok, [entry]} = CssFontFaceValidator.prepare_original_resources([%{prepared: sheet}])

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             CssFontFaceValidator.with_original_resource(entry, fn ->
               HtmlValidator.check_render_resource(
                 :max_css_work,
                 Limits.effective().max_css_work + 1,
                 :font
               )
             end)

    assert diagnostic.stage == :font and diagnostic.source == "src:url('source.ttf')"
  end

  test "malformed media nesting is invalid rather than activating a print rule" do
    assert {:ok, context} = PrintMediaValidator.context([])
    feature = fn _, _ -> true end

    for query <- ["print and not", "not ()", "only print and not"] do
      refute MediaConditionValidator.evaluate(query, context, feature)
    end
  end

  test "source-size metrics fail through the same selected font resource diagnostic" do
    assert {:ok, context} =
             PrintMediaValidator.context(
               default_font: "Broken",
               fonts: [%{family: "Broken", data: "invalid font bytes"}],
               system_font_discovery: false
             )

    assert {:error, {:invalid_font, diagnostic}} =
             PrintMediaValidator.source_length("1ch", context)

    assert diagnostic.stage == :font
    assert diagnostic.operation == :prepare_context
    assert diagnostic.source == "font-relative source sizing"
  end

  test "shadow selector language compounds and malformed nested compounds use shared selector parsing" do
    assert {:ok, [rule]} = CssParser.parse(":host(:lang(en)){color:red}")
    assert hd(rule.selectors).language_dependent

    for selector <- [":host([)", ":host(:unknown)", "::slotted(:unknown)", "::slotted"] do
      assert {:error, {:invalid_css, diagnostic}} =
               CssParser.prepare_source_stylesheet(selector <> "{color:red}")

      assert diagnostic.stage == :css and diagnostic.source =~ selector
    end
  end

  test "empty file layout uses actual selected fonts and rejects a different input kind" do
    fonts = [%{family: "Arial", path: Path.expand("priv/fonts/dejavu/DejaVuSans.ttf")}]
    assert {:ok, dom} = HtmlParser.parse_detailed("<input type=file>")

    assert {:ok, %{children: [node]}} =
             Style.compute_detailed(dom, Keyword.merge(@opts, fonts: fonts, forms: :static))

    assert :ok = HtmlFileValidator.validate_initial_layout(node)
    changed = put_in(node.style._file_control.attributes["type"], "text")

    assert {:error, {:invalid_layout, diagnostic}} =
             HtmlFileValidator.validate_initial_layout(changed)

    assert diagnostic.stage == :layout

    Limits.install(%{Limits.effective() | max_layout_boxes: 1})

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             HtmlFileValidator.prepare_display(
               node.style,
               node.style._file_control.initial_state,
               node.style._file_display.button_face
             )

    assert diagnostic.reason == :resource_limit_exceeded
  end

  test "superseded unsupported original font sources cannot silently use later valid bytes" do
    css = "@font-face{font-family:Original;src:local('Missing');src:url('actual.ttf')}"
    assert {:ok, sheet} = CssParser.prepare_source_stylesheet(css)

    assert {:error, {:invalid_document, diagnostic}} =
             CssFontFaceValidator.prepare_original_resources([%{prepared: sheet}])

    assert diagnostic.stage == :font and diagnostic.source == "src:local('Missing')"
    assert diagnostic.line == 1 and is_integer(diagnostic.column)
  end

  test "symbolic source numeric-token ceilings preserve the authored failure location" do
    css = "font-size:123pt"

    assert {:ok, [record]} =
             CssParser.prepare_source_declarations(css, HtmlCssSourceValidator.local(css))

    Limits.install(%{Limits.effective() | max_html_numeric_token_bytes: 3})

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             CssValueValidator.prepare_unapplied_declaration(
               record,
               HtmlValidator.new_css_budget()
             )

    assert diagnostic.source == css and diagnostic.line == 1 and diagnostic.column == 1
  end

  test "prospective foreign style placement cannot borrow original HTML tokenizer ownership" do
    prefix = "<body><svg width=20 height=20><?marker name='A'></svg>"
    token = "<template for='A'>"
    source = prefix <> token <> "<style>svg{color:red}</style></template></body>"
    assert {:ok, dom} = HtmlParser.parse_document_detailed(prefix <> "</body>")
    tree = HtmlTree.from_document(dom)
    root = tree.nodes[tree.root]

    tree = %{
      tree
      | nodes:
          Map.put(tree.nodes, tree.root, %{root | source: %{text: source, line: 1, column: 1}})
    }

    body = List.last(hd(dom.children).children)

    origin = %{
      source: %{text: token, line: 1, column: String.length(prefix) + 1},
      offset: byte_size(prefix),
      tokenizer_context: "body",
      ancestors: []
    }

    assert :checked =
             TemplatePatchValidator.with_parser_invocation(source, fn _ ->
               TemplatePatchValidator.with_insertion_context(
                 tree,
                 body.node_id,
                 "A",
                 origin,
                 fn plan ->
                   TemplatePatchValidator.with_planned_insertion(plan, tree, fn _ ->
                     TemplatePatchValidator.with_candidate_projection(plan, tree, fn handle ->
                       TemplatePatchValidator.with_candidate_evidence(handle, tree, fn _ ->
                         assert {:error, {:invalid_document, diagnostic}} =
                                  HtmlCssSourceValidator.prepare_candidate_projection(
                                    handle,
                                    tree
                                  )

                         assert diagnostic.message =~ "original tokenizer interpretation"
                         assert diagnostic.source == "<style>"
                         :checked
                       end)
                     end)
                   end)
                 end
               )
             end)
  end
end
