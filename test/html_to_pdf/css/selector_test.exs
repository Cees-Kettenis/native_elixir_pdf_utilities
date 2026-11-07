defmodule NativeElixirPdfUtilities.HtmlToPdf.SelectorTest do
  use ExUnit.Case, async: false

  alias NativeElixirPdfUtilities.HtmlToPdf.{CssParser, HtmlParser, Style}
  alias NativeElixirPdfUtilities.Validators.CssSelectorValidator
  alias NativeElixirPdfUtilities.Limits

  # Independently authored expectations from Selectors 4, 22 January 2026,
  # sections 4, 5, 12, 13 and 15. No production-parser fixture generator.
  test "sibling relationships ignore text and only point forward" do
    nodes =
      styles(
        "<div><p id='a'>A</p>text<span id='b'>B</span><span id='c'>C</span><p id='d'>D</p></div>",
        "p + span{color:red}p ~ p{color:blue}span + p{font-weight:bold}"
      )

    assert nodes["a"].color == {0, 0, 0}
    assert nodes["b"].color == {1.0, 0.0, 0.0}
    assert nodes["c"].color == {0, 0, 0}
    assert nodes["d"].color == {0.0, 0.0, 1.0}
    assert nodes["d"].font_weight == 700
  end

  test "logical specificity uses the maximum argument independently of which matches" do
    for {selector, expected} <- [
          {":is(.a,#missing)", {1, 0, 0}},
          {":where(#a,.a)", {0, 0, 0}},
          {"p:not(div > span, #x)", {1, 0, 1}},
          {"p:has(> .a,+ #b)", {1, 0, 1}},
          {"p:nth-child(2n of .a,#x)", {1, 1, 1}},
          {":host(.a)", {0, 2, 0}},
          {"slot::slotted(.a)", {0, 1, 2}},
          {"div::part(a b)", {0, 0, 2}}
        ] do
      assert {:ok, [%{specificity: ^expected}]} = CssSelectorValidator.parse(selector), selector
    end

    nodes =
      styles(
        "<div><p id='a' class='a'>A</p><p id='b'>B</p></div>",
        "p{color:blue}.a{color:red}:where(#a){color:green}:is(.a,#missing){color:purple}p:not(div > .a, #missing){color:green}"
      )

    assert nodes["a"].color == {128 / 255, 0, 128 / 255}
    assert nodes["b"].color == {0, 0.5019607843, 0}
  end

  test "is and where forgive invalid branches while not and has reject them" do
    for name <- ["is", "where"] do
      assert {:ok, [selector]} =
               CssSelectorValidator.parse(":" <> name <> "(.a,:unknown,::before,)")

      assert [{_, [%{parts: [%{classes: ["a"]}]}]}] = hd(selector.parts).pseudo_classes
      assert {:ok, _} = CssSelectorValidator.parse(":" <> name <> "(:unknown)")
    end

    for selector <- [
          ":not(.a,:unknown)",
          ":not()",
          ":has()",
          ":has(:has(.a))",
          ":has(::before)",
          ":nth-child(2 of ::before)",
          ":nth-of-type(2 of p)",
          ":is(.a)::before p",
          ":host(div span)",
          "::part()",
          "p > + span"
        ] do
      assert {:error, :invalid_css} = CssSelectorValidator.parse(selector), selector
    end
  end

  test "relational traversal anchors each relative selector and permits siblings and descendants" do
    html =
      "<div><section id='a'><p class='hit'>A</p></section><section id='b'><div><p class='hit'>B</p></div></section><section id='c'>C</section><p class='next'><b>D</b></p><section id='d'>E</section></div>"

    nodes =
      styles(
        html,
        "section:has(> p.hit){color:red}section:has(.hit){font-weight:bold}section:has(+ p.next > b){color:blue}section:has(~ p.next){font-style:italic}section:not(:has(.hit)):not(:has(+ p)){color:green}"
      )

    assert nodes["a"].color == {1.0, 0.0, 0.0}
    assert nodes["b"].color == {0, 0, 0}
    assert nodes["b"].font_weight == 700
    assert nodes["c"].color == {0.0, 0.0, 1.0}
    assert nodes["d"].color == {0, 0.5019607843, 0}
    assert nodes["a"].font_style == :italic
    assert nodes["d"].font_style == :normal
  end

  test "filtered positions count the matching subset in both directions" do
    html =
      "<div><p id='a' class='item'>A</p><span id='b'>B</span><p id='c' class='item'>C</p><p id='d' class='item'>D</p><p id='e'>E</p></div>"

    nodes =
      styles(
        html,
        ".item:nth-child(2n of .item){color:red}.item:nth-last-child(1 of p.item){font-weight:bold}p:nth-child(-n+2 of .item){font-style:italic}"
      )

    assert nodes["a"].color == {0, 0, 0}
    assert nodes["c"].color == {1.0, 0.0, 0.0}
    assert nodes["d"].font_weight == 700
    assert nodes["d"].font_style == :normal
    assert nodes["a"].font_style == :italic
    assert nodes["e"].font_style == :normal
  end

  test "namespaces are local to each stylesheet and do not apply to bare attributes" do
    css =
      "@namespace h 'http://www.w3.org/1999/xhtml';@namespace s url(http://www.w3.org/2000/svg);h|p{color:red}s|text{color:blue}|p{font-weight:bold}*|p[data-x]{font-style:italic}"

    nodes =
      styles(
        "<p id='a' data-x=''>A</p><svg width='30' height='20'><text id='b'>B</text></svg>",
        css
      )

    assert nodes["a"].color == {1.0, 0.0, 0.0}
    assert nodes["a"].font_weight == 400
    assert nodes["a"].font_style == :italic

    assert {:ok, dom} =
             HtmlParser.parse_detailed("<svg width='30' height='20'><text id='b'>B</text></svg>")

    assert {:ok, svg} =
             Style.prepare_svg_detailed(dom,
               stylesheets: [css: css],
               system_font_discovery: false
             )

    assert collect(svg.children)["b"].color == {0.0, 0.0, 1.0}
    assert {:ok, []} = CssParser.parse("h|p{color:red}")
    assert {:ok, [rule]} = CssParser.parse("@namespace 'urn:other'; *|p:is(.a){color:red}")
    assert hd(rule.selectors).parts |> hd() |> Map.get(:namespace, :any) == :any
  end

  test "tree selector nesting and matching obey configured resource limits" do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    Limits.install(Map.put(limits, :max_css_selector_depth, 2))

    assert {:error, {:resource_limit_exceeded, %{stage: :css, message: message}}} =
             CssParser.parse_detailed(":is(:not(:is(.a))){color:red}")

    assert message =~ "max_css_selector_depth"
  end

  test "initial link disclosure placeholder range and default states have positive and negative owners" do
    html = """
    <div id='link'><a href='#anchor'>Link</a></div><div id='plain'><a>Plain</a></div>
    <div id='opened'><details open><summary>Open</summary></details></div><div id='shut'><details><summary>Shut</summary></details></div>
    <div id='hint'><input placeholder='Hint'></div><div id='value'><input placeholder='Hint' value='Value'></div>
    <div id='inside'><input type='number' min='1' max='4' value='2'></div><div id='outside'><input type='number' min='1' max='4' value='8'></div><div id='unbounded'><input type='number' value='2'></div>
    <form><button id='first'>First</button><button id='second'>Second</button></form>
    <div id='off'><input type='checkbox'></div><div id='on'><input type='checkbox' checked></div>
    """

    nodes =
      styles(
        html,
        "div:has(> a:any-link),div:has(> details:open),div:has(> input:placeholder-shown),div:has(> input:in-range),button:default,div:has(> input:unchecked){color:blue}div:has(> input:out-of-range){color:red}a:visited,button:user-invalid,button:focus,button:hover,details:modal{font-size:40pt}"
      )

    for id <- ["link", "opened", "hint", "inside", "first", "off"],
        do: assert(nodes[id].color == {0, 0, 1}, id)

    for id <- ["plain", "shut", "value", "unbounded", "second", "on"],
        do: assert(nodes[id].color == {0, 0, 0}, id)

    assert nodes["outside"].color == {1, 0, 0}
    assert nodes["first"].font_size < 40
  end

  test "shadow hosts slots parts and forwarded parts retain authored scope boundaries" do
    html = """
    <style>#host::part(label){color:blue}#host::part(exported){color:purple}#same{font-weight:bold}.outer:has(#same){background-color:red}</style>
    <div class='outer' id='outer'><div id='host' class='selected'>
      <span slot='label' id='light'>Assigned</span>
      <template shadowrootmode='open'>
        <style>:host(.selected){font-style:italic}:host-context(.outer) ::slotted(span){color:green}#same{font-weight:normal}</style>
        <slot name='label'></slot><span part='label' id='same' data-case='part'>Part</span>
        <div exportparts='inner:exported'><template shadowrootmode='open'><span part='inner' id='nested'>Forwarded</span><span part='private' id='private'>Private</span></template></div>
      </template>
    </div><span id='same' data-case='outside'>Outside</span></div>
    """

    nodes = styles(html, "")
    assert nodes["light"].color == {0, 0.5019607843, 0}
    assert nodes["part"].color == {0, 0, 1}
    assert nodes["part"].font_weight == 400
    assert nodes["outside"].font_weight == 700
    assert nodes["nested"].color == {128 / 255, 0, 128 / 255}
    assert nodes["private"].color == {0, 0, 0}
    assert nodes["host"].font_style == :italic
  end

  test "pseudo-element owner identities are selected while later painting stays explicitly gated" do
    alias NativeElixirPdfUtilities.HtmlToPdf.CssSelector

    node = %{
      type: :element,
      tag: "p",
      attributes: %{"class" => "a"},
      children: [],
      _selector_id: make_ref()
    }

    for name <- ["first-line", "first-letter", "column", "nth-fragment(2n)"] do
      assert {:ok, selectors} = CssSelectorValidator.parse("p.a::" <> name)
      assert [owner] = CssSelector.pseudo_owners(selectors, node, [], %{})
      assert owner.owner == CssSelector.owner_identity(node, owner.pseudo)
      refute owner.paint_ready

      assert {:error, {:not_implemented, diagnostic}} =
               Style.compute_detailed(%{type: :document, children: [node]},
                 stylesheets: [css: "p::#{name}{color:red}"]
               )

      assert diagnostic.source =~ "::"
      assert diagnostic.message =~ "layout and painting"
    end

    assert {:ok, selectors} = CssSelectorValidator.parse(".other::first-letter")
    assert CssSelector.pseudo_owners(selectors, node, [], %{}) == []

    assert {:ok, _} =
             Style.compute_detailed(%{type: :document, children: [node]},
               stylesheets: [css: ".other::first-letter{color:red}"]
             )
  end

  test "part states belong to the exposed control and cannot inspect internal structure" do
    nodes =
      styles(
        """
        <div id='host'><template shadowrootmode='open'>
          <input part='control' id='disabled' disabled><input part='control' id='enabled'>
          <span part='label' id='label'>Label</span>
        </template></div>
        """,
        """
        #host::part(control):disabled { color:blue }
        #host::part(control):is(:enabled,.absent) { color:green }
        #host::part(control):first-child { font-weight:bold }
        #host::part(control):is(div > :disabled) { font-style:italic }
        #host::part(control):not(:disabled) { background-color:yellow }
        #host::part(label):empty { color:red }
        #host::part(label):not(.absent) { font-weight:bold }
        """
      )

    assert nodes["disabled"].color == {0, 0, 1}
    assert nodes["enabled"].color == {0, 0.5019607843, 0}
    assert nodes["disabled"].font_weight == 400
    assert nodes["disabled"].font_style == :normal
    assert nodes["enabled"].background_color == {1, 1, 0}
    assert nodes["label"].color == {0, 0, 0}
    assert nodes["label"].font_weight == 700
  end

  test "initial URL targets use the first authored ID and ignore shadow duplicates" do
    html =
      "<div><template shadowrootmode='open'><p id='here' data-case='shadow'>Shadow</p></template></div><p id='here' data-case='first'>First</p><p id='here' data-case='second'>Second</p>"

    nodes =
      styles(html, "p:target{color:blue}", base_url: "https://example.com/report#here")

    assert nodes["first"].color == {0, 0, 1}
    assert nodes["second"].color == {0, 0, 0}
    assert nodes["shadow"].color == {0, 0, 0}

    nodes =
      styles("<p id='a b'>Encoded</p>", "p:target{color:blue}",
        base_url: "https://example.com/report#a%20b"
      )

    assert nodes["a b"].color == {0, 0, 1}

    nodes =
      styles("<p id='a'>Missing</p>", "p:target{color:blue}",
        base_url: "https://example.com/report#missing"
      )

    assert nodes["a"].color == {0, 0, 0}
  end

  test "repeated IDs are valid requirements and escaped attribute names retain their tokens" do
    assert {:ok, [%{specificity: {2, 0, 0}}]} = CssSelectorValidator.parse("#a#a")

    nodes =
      styles(
        "<p id='a' data-mark='yes'>A</p><p id='b'>B</p>",
        ~S|#a#a{color:blue}#a#b{font-weight:bold}p[\64 ata-mark=yes]{font-style:italic}|
      )

    assert nodes["a"].color == {0, 0, 1}
    assert nodes["a"].font_weight == 400
    assert nodes["a"].font_style == :italic
  end

  test "logical host selectors require an explicitly eligible subject and preserve shadow relatives" do
    html =
      "<div id='host' class='selected'><template shadowrootmode='open'><style>:is(:host,.other){color:blue}.selected:host{font-weight:bold}:not(.missing:host){font-style:italic}:host:has(> p){background-color:yellow}</style><p>Shadow child</p></template></div>"

    nodes = styles(html, "")
    assert nodes["host"].color == {0, 0, 1}
    assert nodes["host"].font_weight == 400
    assert nodes["host"].font_style == :normal
    assert nodes["host"].background_color == {1, 1, 0}

    nodes =
      styles(
        "<div id='empty'><template shadowrootmode='open'><p>Shadow only</p></template></div>",
        "div:empty{color:blue}"
      )

    assert nodes["empty"].color == {0, 0, 1}
  end

  test "nested shadow host context follows attached hosts through their fragment roots" do
    html =
      "<div class='outer'><div><template shadowrootmode='open'><div><template shadowrootmode='open'><style>:host-context(.outer){color:blue}</style><p id='deep'>Nested text</p></template></div></template></div></div>"

    assert styles(html, "")["deep"].color == {0, 0, 1}
  end

  test "direction matching resolves logical ancestor metadata during normal styling" do
    assert styles("<div dir='ltr'><p id='direction'>Text</p></div>", "p:dir(ltr){color:blue}")[
             "direction"
           ].color == {0, 0, 1}
  end

  defp styles(html, css, opts \\ []) do
    assert {:ok, dom} = HtmlParser.parse_detailed(html)

    assert {:ok, styled} =
             Style.compute_detailed(
               dom,
               Keyword.merge([stylesheets: [css: css], system_font_discovery: false], opts)
             )

    collect(styled.children)
  end

  defp collect(nodes) do
    Enum.reduce(nodes, %{}, fn node, result ->
      result = Map.merge(result, collect(Map.get(node, :children, [])))

      case get_in(node, [:attributes, "data-case"]) || get_in(node, [:attributes, "id"]) do
        nil -> result
        id -> Map.put(result, id, node.style)
      end
    end)
  end
end
