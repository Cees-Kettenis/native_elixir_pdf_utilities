defmodule NativeElixirPdfUtilities.HtmlToPdf.CssDeclarativeAuthoringTest do
  use ExUnit.Case, async: true

  alias NativeElixirPdfUtilities.HtmlToPdf.{CssParser, Layout}
  alias NativeElixirPdfUtilities.TestSupport.CssStyles
  alias NativeElixirPdfUtilities.Validators.{CssSubstitutionValidator, HtmlValidator}

  test "custom selector aliases keep the most specific branch and original selector span" do
    source = """
    @custom-selector :--item #other, .item;
    :--item { width: 24pt; }
    .item { width: 12pt; }
    """

    assert CssStyles.by_id("<style>#{source}</style><div id='x' class='item'></div>")["x"].width ==
             24

    assert {:ok, %{authored_rules: [rule | _]}} = CssParser.prepare_source_stylesheet(source)
    assert rule.source.text == ":--item"
    assert rule.source.line == 2
    assert hd(rule.source_declarations).source.text == "width: 24pt"
  end

  test "media aliases are grouped, support forward references and preserve unknown cycles" do
    source = """
    @custom-media --A (--B), screen;
    @custom-media --B print;
    @custom-media --cycle (--other);
    @custom-media --other (--cycle);
    @media (--A) and ((not (--cycle)) or (--B)) { #x { width: 17pt; } }
    @media not (--other) { #x { width: 1pt; } }
    @media (--a) { #x { width: 1pt; } }
    """

    assert CssStyles.by_id("<style>#{source}</style><div id='x'></div>")["x"].width == 17
  end

  test "selector aliases stay inside their stylesheet and ignore missing and cyclic branches" do
    styles =
      CssStyles.by_id("""
      <style>
      @custom-selector :--one :--two;
      @custom-selector :--two :--one;
      @custom-selector :--local #x;
      :--one { height: 1pt }
      :--local { width: 20pt }
      </style>
      <style>:--local { width: 2pt } :--missing { height: 2pt }</style>
      <div id='x'></div>
      """)

    assert styles["x"].width == 20.0
    refute Map.has_key?(styles["x"], :height)
  end

  test "typed functions check arguments and return values and evaluate declarations in definition order" do
    styles =
      CssStyles.by_id("""
      <style>
      @function --plus(--left <length>, --right <length>: 2pt,) returns <length> {
        --sum: calc(var(--left) + var(--right));
        result: 99pt;
        @media print { result: var(--sum); }
      }
      @function --double(--arg type(<length> | <percentage>)) returns <length> {
        result: --plus(--left: var(--arg), --right: var(--arg));
      }
      @function --wrong() returns <length> { result: red; }
      @function --duplicate(--a, --a) { result: 20pt; }
      #ok { width: --double(9pt); height: --plus(7pt); }
      #bad { width: 14pt; width: --plus(red); height: --wrong(); padding: --duplicate(1pt); }
      </style>
      <div id='ok'></div><div id='bad'></div>
      """)

    assert_in_delta Layout.resolve_content_size(styles["ok"], :width, 100, nil), 18.0, 0.0001
    assert_in_delta Layout.resolve_content_size(styles["ok"], :height, 100, nil), 9.0, 0.0001
    assert %{width: :auto, height: :auto} = styles["bad"]
    assert styles["bad"].padding.left == 0.0
  end

  test "function layers, inherited variables and recursion use the normal cascade" do
    styles =
      CssStyles.by_id("""
      <style>@layer low, high;
      @layer high { @function --size(--x) { --x: initial; result: var(--x); } }
      @layer low { @function --size(--x) { result: 1pt; } }
      @function --a() { result: --b(); }
      @function --b() { result: --a(); }
      @function --inherited() { --x: inherit; result: var(--x); }
      </style>
      <div style='--x: 19pt'>
        <p id='ok' style='width: --size(11pt); height: --inherited();'></p>
        <p id='recursive' style='width: --a();'></p>
      </div>
      """)

    assert %{width: 11.0, height: 19.0} = styles["ok"]
    assert styles["recursive"].width == :auto
  end

  test "conditional groups and values choose only a matching branch" do
    styles =
      CssStyles.by_id("""
      <style>
      @else { #x { padding: 99pt } }
      @when media(screen) { #x { width: 1pt } }
      @else supports(display: grid) { #x { width: 17pt } }
      @else { #x { width: 2pt } }
      @media print { #x { margin: 3pt } }
      @else { #x { margin: 5pt } }
      #x {
        --gate: yes;
        height: if(style(--gate: yes) and media(print): 18pt; else: var(--missing));
        padding: first-valid(plaid, if(supports(display: grid): 4pt 6pt;));
      }
      </style><div id='x'></div>
      """)

    assert %{width: 17.0, height: 18.0, padding: %{left: 6.0, top: 4.0}, margin: %{left: 3.0}} =
             styles["x"]
  end

  test "inherited and sibling values use the declaring element and its element siblings" do
    styles =
      CssStyles.by_id("""
      <div style='--base: 10pt'>
         text <span id='a' style='--base: 40pt; width: calc(inherit(--base, 1pt) * sibling-index()); height: calc(2pt * sibling-count())'></span>
         <i id='b' style='width: calc(inherit(--base, 1pt) * sibling-index())'></i>
      </div>
      <div id='c' style='width: inherit(--missing, 7pt)'></div>
      """)

    assert_in_delta Layout.resolve_content_size(styles["a"], :width, 100, nil), 10.0, 0.0001
    assert_in_delta Layout.resolve_content_size(styles["a"], :height, 100, nil), 4.0, 0.0001
    assert_in_delta Layout.resolve_content_size(styles["b"], :width, 100, nil), 20.0, 0.0001
    assert styles["c"].width == 7.0
  end

  test "mixins bind arguments and preserve declaration, contents and fallback order" do
    styles =
      CssStyles.by_id("""
      <style>
      @mixin --card(--w <length>: 8pt) {
        @private { --edge: calc(var(--w) / 2); }
        width: var(--w);
        padding: var(--edge);
        @contents { height: 5pt; }
        border-width: 1pt;
      }
      #a { width: 1pt; @apply --card(20pt) { height: 11pt; }; width: 21pt; }
      #b { @apply --card; }
      #c { @private { --edge: 3pt; } height: var(--edge); }
      #c { width: var(--edge, 4pt); }
      </style>
      <div id='a'></div><div id='b'></div><div id='c'></div>
      """)

    assert %{width: 21.0, height: 11.0, padding: %{left: 10.0}} = styles["a"]
    assert %{width: 8.0, height: 5.0, padding: %{left: 4.0}} = styles["b"]
    assert %{width: 4.0, height: 3.0} = styles["c"]
    refute Map.has_key?(styles["a"]._custom_properties, "--edge")
  end

  test "mixin recursion produces a source diagnostic" do
    source = "@mixin --a { @apply --b; } @mixin --b { @apply --a; } div { @apply --a; }"
    assert {:error, {:invalid_css, diagnostic}} = CssParser.parse_detailed(source)
    assert diagnostic.operation == :expand_css_mixin
    assert diagnostic.source == "@apply --a;"
  end

  test "numeric progress, mixing, interpolation and fixed random values agree with independent arithmetic" do
    context = %{}
    budget = HtmlValidator.new_css_budget()

    for {source, expected} <- [
          {"progress(12pt, 4pt, 20pt)", "0.5"},
          {"progress(no-clamp 20, 0, 10)", "2.0"},
          {"calc-mix(0pt, 20px 50%, 10px 20%)", "12.0px"},
          {"calc-interpolate(75%, 0%: 10px, 50%: 20px, 100%: 40px)", "30.0px"},
          {"calc-interpolate(40% steps(2), 0%: 10px, 100%: 20px)", "10.0px"},
          {"random(fixed .25, 10px, 50px)", "20.0px"},
          {"random(fixed .99, 10px, 50px, 10px)", "50.0px"},
          {"random-item(fixed 1, red, green, blue)", "blue"}
        ] do
      assert {:ok, actual} = CssSubstitutionValidator.resolve_value(source, context, budget),
             source

      case {HtmlValidator.parse_css_number(String.trim(actual)),
            HtmlValidator.parse_css_number(expected)} do
        {{actual, unit}, {expected, unit}} -> assert_in_delta actual, expected, 0.0000001, source
        _ -> assert String.trim(actual) == expected, "computed #{inspect(actual)} for #{source}"
      end
    end
  end
end

defmodule NativeElixirPdfUtilities.HtmlToPdf.CssDeclarativeValueGrammarTest do
  use ExUnit.Case, async: true

  alias NativeElixirPdfUtilities.HtmlToPdf.{CssParser, Layout}
  alias NativeElixirPdfUtilities.TestSupport.CssStyles

  alias NativeElixirPdfUtilities.Validators.{
    CssConditionalValueValidator,
    CssStaticNumericValidator,
    CssSubstitutionValidator,
    HtmlValidator,
    PrintMediaValidator
  }

  test "conditional rules combine feature, style and supports queries" do
    media = %{width: 300.0, height: 200.0, density: 1.0, type: :print}
    properties = %{"--name" => "on"}

    for condition <- [
          "media(width > 200px)",
          "media((width > 200px) and (orientation: landscape))",
          "supports(selector(.item))",
          "style(--name)",
          "style((--name: on) and (not (--absent)))"
        ] do
      assert CssConditionalValueValidator.matches?(condition, media, %{}, properties), condition
    end

    for condition <- [
          "style(--name <= a)",
          "style(--absent: a)",
          "style(color: red)",
          "unknown()",
          "(unrecognized)"
        ] do
      refute CssConditionalValueValidator.matches?(condition, media, %{}, properties), condition
    end

    refute CssSubstitutionValidator.valid_syntax?("if(else; 1px)")
    refute CssSubstitutionValidator.valid_syntax?("sibling-count(1)")
    assert CssSubstitutionValidator.valid_syntax?("sibling-index()")
    assert CssSubstitutionValidator.valid_syntax?("if(media(print): 1px; else: 2px;)")
  end

  test "boolean aliases distinguish false and unknown, including cycles with a true branch" do
    sheet = """
    @custom-media --yes true;
    @custom-media --no false;
    @custom-media --cycle (--cycle), all;
    @custom-media --wide (width > 200px);
    @media (--yes) and (not (--no)) { .keep { width: 1pt } }
    @media (--cycle) { .drop { width: 2pt } }
    @media (--wide) { .viewport { width: 3pt } }
    """

    assert {:ok, selected} = PrintMediaValidator.select_rules(sheet, nil)
    assert selected =~ ".keep"
    refute selected =~ ".drop"
    refute selected =~ ".viewport"

    media = %{width: 300.0, height: 200.0, density: 1.0, type: :print}
    assert {:ok, selected} = PrintMediaValidator.select_rules(sheet, media)
    assert selected =~ ".viewport"
  end

  test "alias expansion handles nested selector grammar and stays out of attributes" do
    styles =
      CssStyles.by_id("""
      <style>
      @custom-selector --ignored #item;
      @custom-selector :--cycle :is(:--cycle, #item);
      @custom-selector :--place .place;
      @custom-selector :--target #item;
      :--ignored, :--cycle { padding: 9pt; }
      @scope (:--place) {
        :is(:--target, .other):not(.no)[data-name=":--target"] { width: 13pt; }
      }
      </style>
      <div class='place'><div id='item' data-name=':--target'></div></div>
      """)

    assert styles["item"].width == 13.0
    assert styles["item"].padding.left == 0.0
  end

  test "mixin calls capture outer arguments and keep original definition and contents spans" do
    source = """
    @layer base {
      @supports (display: grid) {
        @mixin --inner(--argument <length>) {
          @media print { width: var(--argument) }
          @media screen { width: 900pt }
          @private { color: orange; --offset: 2pt }
          --published: var(--offset);
          @contents { margin: 15pt }
        }
      }
      @supports (display: unknown) { @mixin --unused { padding: 88pt } }
      @mixin --outer(--argument <length>) {
        @apply --inner(calc(var(--argument) + 3pt)) {
          height: var(--argument);
        };
      }
    }
    #box { @apply --unused(false); @apply --inner(blue); @apply --outer(10pt); }
    #child { @apply --inner(var(--outside, 20pt)) {}; height: var(--published); }
    """

    assert {:ok, %{rules: rules}} = CssParser.prepare_source_stylesheet(source)
    records = Enum.flat_map(rules, & &1.source_declarations)

    assert Enum.any?(
             records,
             &(&1.source.text == "width: var(--argument)" and &1.source.line == 4)
           )

    assert Enum.any?(
             records,
             &(&1.source.text == "height: var(--argument)" and &1.source.line == 14)
           )

    refute Enum.any?(records, &String.contains?(&1.source.text, "900pt"))

    styles = CssStyles.by_id("<style>#{source}</style><div id='box'><div id='child'></div></div>")
    assert_in_delta Layout.resolve_content_size(styles["box"], :width, 100, nil), 13, 0.0001
    assert styles["box"].height == 10.0
    assert styles["box"].margin.left == 15.0
    assert styles["child"].width == 20.0
    assert styles["child"].height == 2.0
    assert styles["child"].margin.left == 0.0
  end

  test "static expansions cannot introduce untracked URL tokens" do
    source = """
    @function --image() { result: url(local.svg) }
    @mixin --image-mixin(--image <image>) { background-image: var(--image) }
    #function { background-image: --image() }
    #mixin { @apply --image-mixin(url(local.svg)); }
    """

    styles =
      CssStyles.by_id("<style>#{source}</style><div id='function'></div><div id='mixin'></div>")

    assert styles["function"].background_image == nil
    assert styles["mixin"].background_image == nil
  end

  test "lazy substitution and cycle values use computed declarations including their shorthands" do
    styles =
      CssStyles.by_id("""
      <div id='outer' style='font-style: cycle(italic, normal); --repeat: cycle(2pt, 4pt);'>
        <div id='inner' style='font-style: cycle(italic, normal); --repeat: cycle(2pt, 4pt);
              width: var(--repeat);
              margin: var(--none, first-valid(inherit(--missing), if(media(print): 6pt; else: var(--unused))));
              height: if(var(--unknown): 9pt; else: 8pt)'></div>
      </div>
      """)

    assert styles["outer"].font_style == :italic
    assert styles["inner"].font_style == :normal
    assert styles["inner"].width == 4.0
    assert styles["inner"].margin.left == 6.0
    assert styles["inner"].height == 8.0
    context = %{_cascade_inherited: %{_custom_properties: %{}}}
    budget = HtmlValidator.new_css_budget()

    for {property, value} <- [
          {"font-style", "cycle(normal, bad)"},
          {"height", "calc(1pt + first-valid(2pt))"},
          {"width", "random-item(fixed 1)"},
          {"padding", "cycle(cycle(1pt, 2pt), 3pt)"},
          {"width", "cycle(1px, 2px)"}
        ] do
      context = if value == "cycle(1px, 2px)", do: %{}, else: context

      assert :error =
               CssSubstitutionValidator.resolve_value(value, context, budget, {property, value})
    end
  end

  test "custom functions and tree counts respect shadow scope" do
    styles =
      CssStyles.by_id("""
      <style>
      @function --public() { result: 20pt }
      #host::part(counted) { width: calc(10pt * sibling-count()) }
      </style>
      <div id='host'><template shadowrootmode='open'>
        <style>
          @function --private() { result: 30pt }
          :host { height: calc(5pt * sibling-index()) }
        </style>
        <section id='part' part='counted' style='height: --public()'>
          <div><template shadowrootmode='open'>
            <div id='nested' style='height: --private()'></div>
          </template></div>
        </section>
      </template></div>
      <div id='outside' style='width: --private()'></div>
      """)

    assert styles["part"].height == 20.0
    assert_in_delta Layout.resolve_content_size(styles["part"], :width, 100, nil), 0, 0.0001
    assert_in_delta Layout.resolve_content_size(styles["host"], :height, 100, nil), 5, 0.0001
    assert styles["nested"].height == 30.0
    assert styles["outside"].width == :auto
  end

  test "source-context evaluation uses typed definitions and reportable spans" do
    alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, Style}

    css =
      "@function --half(--width <length>) returns <length> { result: first-valid(red, calc(var(--width) / 2)) }"

    html = "<div id='probe' style='width: --half(20pt)'></div>"
    assert {:ok, dom} = HtmlParser.parse_document_detailed(html)

    assert {:ok, _} =
             Style.with_source_style_contexts(dom, [css], HtmlValidator.new_css_budget(), fn node,
                                                                                             _,
                                                                                             context ->
               if node.attributes["id"] == "probe" do
                 assert_in_delta Layout.resolve_content_size(context.style, :width, 100, nil),
                                 10,
                                 0.0001
               end

               :ok
             end)
  end

  test "identification survives token boundaries and selector fallbacks" do
    assert {:ok, "--edge-2"} =
             CssSubstitutionValidator.resolve_value(
               ~s/ident(--edge- sibling-index())/,
               %{_css_siblings: {2, 3}},
               HtmlValidator.new_css_budget()
             )

    assert {:ok, encoded} = CssStaticNumericValidator.identifier(~s/"9.2" "pt"/)
    assert {:ok, "9.2pt", ""} = HtmlValidator.parse_css_identifier(encoded)
    assert :error = CssStaticNumericValidator.identifier(~s/""/)
    assert :error = CssStaticNumericValidator.identifier("2.5")
  end

  test "numeric and expansion failures preserve resource diagnostics" do
    for {name, source} <- [
          {"progress", "1vw, 0px, 20px"},
          {"random", "--name, 0vw, 1vw"},
          {"calc-mix", "0vw, 1vw"},
          {"calc-interpolate", "1vw, 0:1, 1:2"},
          {"calc-interpolate", "0.4, 1vw:1, 2vw:2"},
          {"calc-interpolate", ".5, 0:1vw, 1:2px"},
          {"calc-interpolate", ".5, 0:1px, 1:2vw"}
        ] do
      assert {:error, {:not_implemented, diagnostic}} =
               CssStaticNumericValidator.evaluate(name, source, %{}, true),
             source

      assert diagnostic.operation == :compute_css_units
    end

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             CssStaticNumericValidator.evaluate("progress", "no-clamp 100, 0, 1e-309", %{}, true)

    assert diagnostic.operation == :evaluate_css_math
  end

  test "unused and malformed declarative rules recover at their authored boundary" do
    alias NativeElixirPdfUtilities.HtmlToPdf.CssSelector
    alias NativeElixirPdfUtilities.Validators.{CssAliasValidator, CssFunctionValidator}

    assert CssAliasValidator.selector(".part:empty", %{"--name" => ".a"}) == ".part:empty"

    assert %{"--foo" => ":empty, .bar"} =
             CssAliasValidator.definitions("@custom-selector :--foo :empty, .bar;", :selector)

    assert {1, 1} =
             CssSelector.sibling_index_count(
               %{_selector_id: make_ref()},
               [],
               %{root_selector_id: make_ref(), root_children: [], cache: make_ref()}
             )

    source = """
    @layer base {
      @mixin --bad(invalid) { top: 9px }
      @property --not-a-mixin { syntax: "*"; inherits: false; }
      @supports (display: changed-display) { @mixin --ignored { width: 99px } }
      @mixin --noop { @contents; }
    }
    .item { @apply --noop; @apply --noop junk; @apply --missing; }
    """

    assert {:ok, %{rules: []}} = CssParser.prepare_stylesheet(source)
    assert {:ok, _} = PrintMediaValidator.select_rules("@() { .item { width: 20pt } }", nil)
    refute PrintMediaValidator.matches?("(--false)", nil, %{"--false" => "screen"})

    assert CssConditionalValueValidator.matches?(
             "supports((display: grid) and (display: flex))",
             nil,
             %{},
             %{}
           )

    refute CssConditionalValueValidator.matches?("style((unknown-thing()))", nil, %{}, %{})

    test_scope = %{"--overflow" => "ident(--size)"}

    assert {:ok, "--size"} =
             CssSubstitutionValidator.resolve_value(
               "var(--overflow)",
               %{_custom_properties: test_scope},
               HtmlValidator.new_css_budget()
             )

    assert :error = CssSubstitutionValidator.resolve("first-valid(red, blue)", %{})
    assert :error = CssSubstitutionValidator.resolve("inherit(color)", %{})

    assert {:ok, definition} =
             CssFunctionValidator.definition("--args(--x <number>:1)", :function)

    check_type = &CssFunctionValidator.check_type/2

    assert {:ok, %{"--x" => "1"}} =
             CssFunctionValidator.bind(definition.parameters, "red", check_type)

    assert {:ok, %{"--x" => "2"}} =
             CssFunctionValidator.bind(definition.parameters, "2,", check_type)

    assert :error = CssFunctionValidator.bind(definition.parameters, "2, 3", check_type)

    assert {:ok, %{"--items" => "1, 2"}} =
             CssFunctionValidator.bind([{"--items", :any, nil}], "{ 1, 2 }", check_type)

    assert :error = CssFunctionValidator.bind(definition.parameters, "--missing: 2", check_type)
    assert :error = CssFunctionValidator.bind(definition.parameters, "", fn _, _ -> :error end)
    assert {:error, {:invalid_css, _}} = CssFunctionValidator.reference_free("url(bad url)")

    assert {:error, {:invalid_css, _}} =
             CssParser.parse_detailed(
               "@mixin --image(--src) { width: 1px } div { @apply --image(url(bad url)); }"
             )
  end

  test "resource limits are reported from conditional tests, arguments and function calls" do
    context = %{_custom_properties: %{}}
    budget = HtmlValidator.new_css_budget()

    assert {:error, {:not_implemented, _}} =
             CssSubstitutionValidator.resolve_value(
               "if(progress(1vw, 0px, 200px):1px;else:2px)",
               context,
               budget
             )

    assert {:error, {:not_implemented, _}} =
             CssSubstitutionValidator.resolve_value(
               "inherit(progress(1vw, 0px, 200px))",
               context,
               budget
             )

    assert {:error, {:not_implemented, _}} =
             CssSubstitutionValidator.resolve_value("var(--length)", context, budget, %{
               declaration: {"width", "var(--length)"},
               css_locals: %{"--length" => {:argument, :any, "progress(1vw,0px,10px)", %{}}}
             })

    function = %{
      parameters: [{"--dimension", :any, nil}],
      body: "result: var(--dimension)",
      media_aliases: %{},
      type: :any
    }

    context = Map.put(context, :_css_functions, %{{nil, "--v"} => function})

    assert {:error, {:not_implemented, _}} =
             CssSubstitutionValidator.resolve_value(
               "--v(progress(1vw, 0px, 10px))",
               context,
               budget
             )

    assert {:error, {:resource_limit_exceeded, _}} =
             CssSubstitutionValidator.resolve_value(
               "first-valid(progress(no-clamp 100, 0, 1e-309), 2px)",
               context,
               budget
             )
  end

  test "cache keys share a base and distinguish element and property index scopes" do
    context = %{
      _css_random_document: make_ref(),
      _css_element: :button,
      _css_property: "width",
      _css_random_index: [0]
    }

    assert {:ok, shared} = CssStaticNumericValidator.random_base("--named", context)

    assert {:ok, ^shared} =
             CssStaticNumericValidator.random_base("--named", %{
               context
               | _css_element: :other,
                 _css_property: "height"
             })

    assert {:ok, automatic} = CssStaticNumericValidator.random_base("auto", context)

    assert {:ok, ^automatic} =
             CssStaticNumericValidator.random_base(
               "element-scoped property-index-scoped",
               context
             )

    assert {:ok, index} =
             CssStaticNumericValidator.random_base("auto", %{context | _css_random_index: [10]})

    assert automatic != index

    assert {:ok, scoped} =
             CssStaticNumericValidator.random_base("--named property-scoped", context)

    assert {:ok, ^scoped} =
             CssStaticNumericValidator.random_base("property-scoped --named", %{
               context
               | _css_element: :other
             })

    assert {:ok, id} = CssStaticNumericValidator.random_base("ua-probe", context)
    assert id >= 0 and id < 1

    assert :error =
             CssStaticNumericValidator.random_base(
               "property-scoped property-index-scoped",
               context
             )

    assert :error = CssStaticNumericValidator.random_base("fixed 1.1", context)
  end

  test "numeric functions preserve consistent dimensions, easing and exact stop positions" do
    check = fn name, source, expected, expected_unit ->
      assert {:ok, output} = CssStaticNumericValidator.evaluate(name, source, %{}, true), source
      assert {amount, ^expected_unit} = HtmlValidator.parse_css_number(output)
      assert_in_delta amount, expected, 0.00001, source
    end

    for {name, source, value, unit} <- [
          {"progress", "8, 4, 4", 0, ""},
          {"progress", "no-clamp 4, 4, 4", 0, ""},
          {"progress", "-8, 0, 8", 0, ""},
          {"calc-mix", "10 100%, 30 100%", 20, ""},
          {"calc-mix", "0%, 50%", 25, "%"},
          {"calc-mix", "0deg, 180deg", :math.pi() / 2, "rad"},
          {"calc-mix", "2s, 4s", 3, "s"},
          {"calc-mix", "0Hz, 16Hz", 8, "Hz"},
          {"calc-mix", "1dppx, 2dppx", 1.5, "dppx"},
          {"calc-mix", "1fr, 4fr", 2.5, "fr"},
          {"calc-interpolate", "30px, 0px:0, 40px:40", 30, ""},
          {"calc-interpolate", "75%, 0px:0, 40px:40", 30, ""},
          {"calc-interpolate", "50%, 0%:0, 50% 70%:10, 40%:20, 100%:30", 10, ""},
          {"calc-interpolate", "50% by ease-in-out, 0:0, 1:40", 20, ""},
          {"calc-interpolate", "50%, 0:0, ease, 1:1", 0.802403, ""},
          {"calc-interpolate", "50% ease-in, 0:0, 1:1", 0.315357, ""},
          {"calc-interpolate", "50% ease-out, 0:0, 1:1", 0.684643, ""},
          {"calc-interpolate", "40% step-start, 0:0, 1:10", 10, ""},
          {"calc-interpolate", "40% step-end, 0:0, 1:10", 0, ""},
          {"calc-interpolate", "40% steps(3, jump-none), 0:0, 1:10", 5, ""},
          {"calc-interpolate", "40% steps(3, jump-both), 0:0, 1:10", 5, ""},
          {"random", "20px, 10px", 20, "px"},
          {"random", "20px, 20px, 2px", 20, "px"},
          {"random", "ua-numeric, 10px, 10px", 10, "px"},
          {"random", "fixed 1, 0, 100, 33.333", 100, ""}
        ] do
      check.(name, source, value, unit)
    end

    assert {:ok, "calc(infinity)"} =
             CssStaticNumericValidator.evaluate("progress", "no-clamp 1, 0, 0", %{}, true)

    assert {:ok, "calc(-infinity)"} =
             CssStaticNumericValidator.evaluate("progress", "no-clamp -1, 0, 0", %{}, true)

    for {name, source} <- [
          {"progress", "5px, 0, 10"},
          {"random", "0px"},
          {"random", "fixed 0, 0px, 10px, 0px"},
          {"calc-mix", "1px, 2"},
          {"calc-mix", "1px, 2px 120%"},
          {"calc-mix", "1px * 2px, 1px * 2px"},
          {"calc-interpolate", "0.2s, 0px:1, 10px:2"},
          {"calc-interpolate", "40%, 0:1, ease"},
          {"calc-interpolate", "40% bogus, 0:1, 1:2"},
          {"calc-interpolate", "40% bogus(2), 0:1, 1:2"},
          {"calc-interpolate", "40% steps(1, jump-none), 0:1, 1:2"},
          {"calc-interpolate", "40% steps(2, bad), 0:1, 1:2"},
          {"calc-interpolate", "40% steps(2, end, extra), 0:1, 1:2"},
          {"calc-interpolate", "40% steps(-1), 0:1, 1:2"},
          {"calc-interpolate", "40%, invalid:0, 1:1"},
          {"calc-interpolate", "40%, :0, 1:1"},
          {"calc-interpolate", "bogus, 0:1, 1:2"},
          {"calc-interpolate", ", 0:1, 1:2"},
          {"calc-interpolate", "40%, 0:1:2, 1:3"},
          {"calc-interpolate", "40%, 0:1, 1:2, ease, ease-out"},
          {"calc-interpolate", "40% linear junk, 0:1, 1:2"},
          {"calc-interpolate", "40%, 0:1px, 1:2"},
          {"progress", "nonsense"}
        ] do
      assert :error = CssStaticNumericValidator.evaluate(name, source, %{}, true),
             "#{name}(#{source})"
    end
  end

  test "transform, color and property interpolation select their computed value family" do
    assert {:ok, "none"} =
             CssStaticNumericValidator.evaluate("transform-mix", "100% none", %{}, true)

    assert {:ok, "calc(10.0px + 25.0%)"} =
             CssStaticNumericValidator.evaluate("calc-mix", "20px, 50%", %{}, true)

    assert {:ok, "color-mix(in srgb" <> _} =
             CssStaticNumericValidator.evaluate(
               "color-interpolate",
               ".6 in srgb, 0:red, 1:blue",
               %{},
               true
             )

    assert {:ok, _} =
             CssStaticNumericValidator.evaluate(
               "transform-mix",
               "translate(0) scale(2), translate(1px)",
               %{},
               true
             )

    assert :error = CssStaticNumericValidator.random_base("nonsense", %{})

    assert :error =
             CssStaticNumericValidator.evaluate("transform-mix", "matrix(1,0,0,1,0,0)", %{}, true)

    assert {:ok, transform} =
             CssStaticNumericValidator.evaluate(
               "transform-mix",
               "scale(3) 50%, rotate(40deg), translate(12px)",
               %{},
               true
             )

    assert transform =~ "scale("
    assert transform =~ "rotate("
    assert transform =~ "translate("

    assert {:ok, _} =
             CssStaticNumericValidator.evaluate(
               "transform-interpolate",
               "0.5, 0:rotate(10deg), 1:none",
               %{},
               true
             )

    assert :error =
             CssStaticNumericValidator.evaluate(
               "transform-interpolate",
               "0.5, 0:unknown(4), 1:none",
               %{},
               true
             )

    assert :error =
             CssStaticNumericValidator.evaluate(
               "transform-interpolate",
               "0.5, 0:scale(2), 1:rotate(10deg)",
               %{},
               true
             )

    assert {:ok, "color-mix(in hsl longer hue" <> _} =
             CssStaticNumericValidator.evaluate(
               "color-interpolate",
               ".3 in hsl longer hue, 0:red, 1:blue",
               %{},
               true
             )

    assert {:ok, "color-mix(in oklab" <> _} =
             CssStaticNumericValidator.evaluate(
               "interpolate",
               ".3, 0:red, 1:blue",
               %{_css_property: "color"},
               true
             )

    assert {:ok, "color-mix(in oklab" <> _} =
             CssStaticNumericValidator.evaluate(
               "interpolate",
               ".3, 0:red, 1:blue",
               %{_css_property: "border-top-color"},
               true
             )

    assert {:ok, "none"} =
             CssStaticNumericValidator.evaluate(
               "interpolate",
               ".5, 0:none, 1:none",
               %{_css_property: "transform"},
               true
             )

    assert {:ok, "block"} =
             CssStaticNumericValidator.evaluate(
               "interpolate",
               ".3, 0:block, 1:inline",
               %{_css_property: "display"},
               true
             )

    assert {:ok, "inline"} =
             CssStaticNumericValidator.evaluate("interpolate", ".8, 0:block, 1:inline", %{}, true)

    assert :error =
             CssStaticNumericValidator.evaluate(
               "interpolate",
               ".3, 0:block, 1:inline",
               %{_css_property: "display"},
               false
             )
  end
end
