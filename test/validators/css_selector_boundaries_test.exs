defmodule NativeElixirPdfUtilities.Validators.CssSelectorBoundariesTest do
  use ExUnit.Case, async: true

  alias NativeElixirPdfUtilities.Validators.{
    CssSelectorValidator,
    CssSyntaxValidator,
    HtmlValidator
  }

  alias NativeElixirPdfUtilities.HtmlToPdf.{CssParser, CssSelector, HtmlParser, Style}

  test "namespace-qualified attributes preserve local names flags and namespace direction" do
    node = %{
      tag: "a",
      namespace: :svg,
      attributes: %{
        "xlink:href" => "#target",
        "href" => "#plain",
        "data:mark" => "yes",
        "viewBox" => "0 0 10 10"
      },
      attribute_namespaces: %{"xlink:href" => "http://www.w3.org/1999/xlink"},
      children: [],
      _selector_id: make_ref()
    }

    context = %{
      namespaces: %{
        "x" => "http://www.w3.org/1999/xlink",
        "m" => "http://www.w3.org/1998/Math/MathML"
      }
    }

    for {source, expected} <- [
          {"[x|href='#target']", true},
          {"[*|href='#plain']", true},
          {"[|href='#plain']", true},
          {"[x|href='#plain']", false},
          {~S|[data\:mark=yes]|, true},
          {"x|*", false},
          {"*|a", true},
          {"[viewBox]", true},
          {"[viewbox]", false},
          {"[*|mark]", false},
          {~S([|data\:mark=yes]), true}
        ] do
      assert {:ok, [selector]} = CssSelectorValidator.parse(source, context)
      assert CssSelector.matches?(Enum.reverse(selector.parts), node, [], %{}) == expected
    end

    math = %{node | tag: "mi", namespace: :mathml}
    assert {:ok, [selector]} = CssSelectorValidator.parse("m|mi", context)
    assert CssSelector.matches?(Enum.reverse(selector.parts), math, [], %{})
    no_namespace = %{node | namespace: nil}
    assert {:ok, [selector]} = CssSelectorValidator.parse("|a")
    assert CssSelector.matches?(Enum.reverse(selector.parts), no_namespace, [], %{})

    for source <- ["unknown|a", "x|", "[x|]", "[  ]", ":dir()", ":dir(1)", ":dir(", ":1"] do
      assert {:error, :invalid_css} = CssSelectorValidator.parse(source, context)
    end
  end

  test "empty and scope use source nodes and the pinned whitespace rule" do
    parent = %{
      type: :element,
      tag: "div",
      attributes: %{},
      children: [],
      _selector_id: make_ref()
    }

    assert {:ok, [selector]} = CssSelectorValidator.parse(":empty")

    for {children, expected} <- [
          {[], true},
          {[%{type: :text, text: " \n\t"}], true},
          {[%{type: :text, text: "x"}], false},
          {[%{type: :comment}], true},
          {[parent], false}
        ] do
      assert CssSelector.matches?(
               Enum.reverse(selector.parts),
               %{parent | children: children},
               [],
               %{}
             ) == expected
    end

    assert {:ok, [selector]} = CssSelectorValidator.parse(":scope")
    assert CssSelector.matches?(selector.parts, %{parent | tag: "html"}, [], %{})
    refute CssSelector.matches?(selector.parts, parent, [], %{})
  end

  test "export mappings ignore invalid and absent names and preserve non-ASCII name bytes" do
    assert CssSelectorValidator.exported_parts("a, b:c, a:d, missing, a:, a:x y, a:b:c", [
             "a",
             "b"
           ]) == ["a", "c", "d"]

    assert CssSelectorValidator.exported_parts("\u00a0x:label", ["\u00a0x"]) == ["label"]
  end

  test "namespace recovery keeps unrelated rules and explicit default namespaces" do
    for prefix <- [
          "@namespace;",
          "@namespace bad;",
          "@namespace * 'uri';",
          "@namespace a fn('uri');",
          "@namespace a [foo];",
          "@namespace a 'uri' extra;",
          "@;",
          "@namespace a url('uri' extra);"
        ] do
      assert {:ok, [%{declarations: [{"color", "red"}]}]} =
               CssParser.parse(prefix <> "p{color:red}")
    end

    assert {_, %{"x" => "urn:example"}} =
             CssSyntaxValidator.stylesheet_projection("@namespace x url('urn:example');")

    assert {_, %{"" => nil}} =
             CssSyntaxValidator.stylesheet_projection("@namespace ''; p{color:red}")

    assert {_, %{}} = CssSyntaxValidator.stylesheet_projection("p{} @namespace x 'late';")

    assert {_, %{"x" => "urn:example"}} =
             CssSyntaxValidator.stylesheet_projection("@namespace x url(urn\\:example);")

    assert {:ok, [%{declarations: [{"color", "red"}]}]} =
             CssParser.parse("@media print{p{color:red}")

    assert {:ok, source} = CssParser.prepare_source_stylesheet("@media print{p{color:red}")
    assert hd(hd(source.rules).source_declarations).source.text == "color:red"
  end

  test "EOF component serialization and malformed URLs retain neighboring declarations" do
    assert Enum.map(
             CssSyntaxValidator.declarations("--a:[x;y];--b:{x;y};content:\"EOF"),
             & &1.declaration
           ) == [{"--a", "[x;y]"}, {"--b", "{x;y}"}, {"content", "\"EOF\""}]

    assert Enum.map(
             CssSyntaxValidator.declarations("background-image:url(\"x\" extra);color:red"),
             & &1.declaration
           ) == [{"color", "red"}]

    assert [%{kind: :url}] = CssSyntaxValidator.components(~S|url(a\))|)
    assert [%{kind: :bad_url}] = CssSyntaxValidator.components("url((bad)")

    for {pattern, good, bad} <- [
          {HtmlValidator.css_identifier_pattern(), ~S|\31 x|, "1x"},
          {HtmlValidator.css_number_pattern(), "-1.25e+2", ~S|\31|},
          {HtmlValidator.css_escape_pattern(), ~S|\31 |, "\\\n"},
          {HtmlValidator.css_url_name_pattern(), ~S|u\72l|, "ure"}
        ] do
      regex = Regex.compile!("\\A(?:" <> pattern <> ")\\z", "u")
      assert Regex.match?(regex, good)
      refute Regex.match?(regex, bad)
    end
  end

  test "legacy lexical helpers delegate to the shared component grammar" do
    for {source, expected} <- [
          {":host", {:ok, :host, nil, ""}},
          {":host(.a).b", {:ok, :host, ".a", ".b"}},
          {"::slotted(.a)", {:ok, :slotted, ".a", ""}},
          {"::slotted(", :error},
          {":root", nil},
          {"plain", nil},
          {"::other(.a)", nil}
        ],
        do: assert(HtmlValidator.parse_css_shadow_selector(source) == expected)

    assert HtmlValidator.parse_css_language_selector(":other(en)") == nil
    assert HtmlValidator.parse_css_language_selector("plain") == nil
    assert HtmlValidator.parse_css_nth_pseudo_class("plain") == nil
    assert HtmlValidator.parse_css_list_style("list-style", "") == :error

    assert {:ok, [%{language_dependent: true}]} =
             CssSelectorValidator.parse("p:nth-child(1 of :lang(en))")

    NativeElixirPdfUtilities.HtmlToPdf.RenderCache.with_stylesheets(fn ->
      for _ <- 1..2,
          do:
            assert(
              match?(
                {:error, {:invalid_css, _}},
                CssParser.prepare_stylesheet("@page{margin:bananas}")
              )
            )
    end)

    assert {:error, {:invalid_css, diagnostic}} =
             CssParser.parse_detailed("\n@page{margin:/* original */bananas}")

    assert diagnostic.line == 2 and diagnostic.column == 7

    assert {:ok, _, [%{active: false}]} =
             NativeElixirPdfUtilities.Validators.PrintMediaValidator.project_source_rules(
               "@media print;",
               nil
             )
  end

  test "An+B escapes preserve identifier and numeric token distinctions" do
    for {expression, expected} <- [
          {~S|\6f dd|, :odd},
          {~S|e\76 en|, :even},
          {~S|2\6e +1|, {2, 1}},
          {~S|-\6e-2|, {-1, -2}},
          {~S|n-\31|, {1, -1}},
          {~S|\2d n-2|, {-1, -2}},
          {~S|\N|, {1, 0}},
          {"n/**/-1", {1, -1}},
          {"odd/**/", :odd},
          {"+/**/n", {1, 0}}
        ] do
      assert {:ok, [%{parts: [%{pseudo_classes: [{:nth_child, ^expected}]}]}]} =
               CssSelectorValidator.parse(":nth-child(" <> expression <> ")")
    end

    for expression <- [
          ~S|\32 n|,
          ~S|\31|,
          ~S|\6e +\31|,
          "2/**/n",
          ~S|2\00004E0|,
          ~S|\6f/**/dd|,
          "2n 1of .a"
        ] do
      assert {:error, :invalid_css} =
               CssSelectorValidator.parse(":nth-child(" <> expression <> ")")
    end
  end

  test "filtered positions and part names consume CSS tokens including escape terminators" do
    for source <- [~S|:nth-child(2n \6f f .a)|, ":nth-child(2n/**/of/**/.a)"] do
      assert {:ok, [%{parts: [%{pseudo_classes: [{:nth_child, {2, 0}, [_]}]}]}]} =
               CssSelectorValidator.parse(source)
    end

    assert {:ok, [%{parts: [%{shadow_selector: {:part, ["label", "active"]}}]}]} =
             CssSelectorValidator.parse(~S|::part(\6c abel/**/active)|)

    for source <- [
          "::part(a).class",
          "::part(a)#id",
          "::part(a)[x]",
          "::part(a):disabled.foo",
          "::slotted(span):disabled",
          "::slotted(span).foo",
          "::part('a')"
        ] do
      assert {:error, :invalid_css} = CssSelectorValidator.parse(source)
    end

    assert {:ok, [%{specificity: {1, 1, 2}}]} =
             CssSelectorValidator.parse("div#host::part(control):disabled")

    assert {:ok, [%{specificity: {0, 0, 4}}]} =
             CssSelectorValidator.parse("slot::slotted(span)::before")

    for source <- [
          "::part(a):has(span)",
          "::part(a):nth-child(1 of .x)",
          "::part(a):lang(fr)",
          "::part(a)::before"
        ] do
      assert {:ok, _} = CssSelectorValidator.parse(source)
    end

    assert HtmlValidator.normalize_css_comments(~S(\72/**/ed), :value) == ~S(\72  ed)
  end

  test "logical host eligibility and detached ancestry do not expose unrelated trees" do
    assert {:ok, [%{parts: [part]}]} = CssSelectorValidator.parse(":host:hover")
    refute Map.get(part, :host_eligible, false)
    assert {:ok, [%{specificity: {3, 0, 0}}]} = CssSelectorValidator.parse("#a#a#a")
    fragment = make_ref()

    host = %{
      type: :element,
      tag: "div",
      attributes: %{},
      children: [],
      _selector_id: make_ref(),
      node_id: make_ref(),
      parent_id: fragment
    }

    context = %{
      shadow_host_id: host.node_id,
      logical_nodes: %{fragment => %{type: :document_fragment, node_id: fragment}},
      consumed_shadow_roots: %{}
    }

    assert {:ok, [selector]} = CssSelectorValidator.parse(":host-context(.outside)")
    refute CssSelector.matches?(selector.parts, host, [], context)
    assert HtmlValidator.parse_css_nth_pseudo_class(":other(1)") == nil
    foreign = Map.put(host, :namespace, :svg)

    refute NativeElixirPdfUtilities.Validators.HtmlSelectorStateValidator.matches?(
             :unchecked,
             foreign,
             [],
             %{}
           )

    refute NativeElixirPdfUtilities.Validators.CssPseudoValidator.owner?(
             :placeholder,
             foreign,
             %{}
           )
  end

  test "qualified HTML attribute names fold without folding namespaced values" do
    html = %{
      tag: "input",
      namespace: :html,
      attributes: %{"title" => "x", "x:type" => "search"},
      attribute_namespaces: %{"x:type" => "urn:schema"},
      children: [],
      _selector_id: make_ref()
    }

    for {source, expected} <- [
          {"[|TiTle=x]", true},
          {"[x|type=SEARCH]", false},
          {"[x|type=SEARCH i]", true}
        ] do
      assert {:ok, [selector]} =
               CssSelectorValidator.parse(source, %{namespaces: %{"x" => "urn:schema"}})

      assert CssSelector.matches?(selector.parts, html, [], %{}) == expected
    end
  end

  test "direction selectors resolve source contexts and computed RTL inheritance" do
    assert {:ok, dom} = HtmlParser.parse_document_detailed("<div dir='rtl'><p>Text</p></div>")
    caller = self()

    assert {:ok, _} =
             Style.with_source_style_contexts(
               dom,
               ["p:dir(rtl){color:red}"],
               HtmlValidator.new_css_budget(),
               fn node, pseudo, context ->
                 if node.tag == "p" and pseudo == nil,
                   do: send(caller, {:source_color, context.style.color})

                 :ok
               end
             )

    assert_received {:source_color, {1, 0, 0}}
    assert {:ok, %{children: [%{tag: "body", children: [div]}]}} = Style.compute_detailed(dom)
    assert div.style.direction == :rtl
    [paragraph] = div.children
    assert paragraph.style.direction == :rtl
  end
end
