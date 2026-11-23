defmodule NativeElixirPdfUtilities.HtmlToPdf.PublishingCounterTest do
  use ExUnit.Case, async: true
  alias NativeElixirPdfUtilities.HtmlToPdf.{CounterStyle, HtmlParser, Style}

  alias NativeElixirPdfUtilities.Validators.{
    CssCounterStyleValidator,
    CssGeneratedContentValidator
  }

  test "custom systems retain affixes, padding, ranges and fallback" do
    css = """
    @counter-style binary {system:numeric;symbols:'0' '1';pad:4 '0';negative:'(' ')';prefix:'[';suffix:']';range:-4 8;fallback:decimal}
    @counter-style letters {system:alphabetic;symbols:'a' 'b'}
    @counter-style repeat {system:symbolic;symbols:'*' '#'}
    @counter-style fixed {system:fixed 3;symbols:'A' 'B'}
    @counter-style roman {system:additive;additive-symbols:10 'X',5 'V',1 'I',0 'Z'}
    @counter-style padded {system:extends decimal-leading-zero;suffix:')'}
    @counter-style inherited {system:extends binary;suffix:'!'}
    @counter-style cyclic {system:cyclic;symbols:'A' 'B'}
    """

    assert {:ok, styles} = CssCounterStyleValidator.prepare(css)
    assert CounterStyle.marker("binary", 3, styles) == "[0011]"
    assert CounterStyle.format("binary", -3, styles) == "(11)"
    assert CounterStyle.format("binary", 9, styles) == "9"
    assert CounterStyle.marker("binary", 9, styles) == "[9]"
    assert CounterStyle.marker("inherited", 9, styles) == "[9!"
    assert CounterStyle.marker("padded", 1, styles) == "01)"

    for ordinal <- [-12, -1, 0, 1, 10] do
      assert CounterStyle.format("padded", ordinal, styles) ==
               CounterStyle.format("decimal-leading-zero", ordinal, styles)
    end

    assert CounterStyle.format("letters", 3, styles) == "aa"
    assert CounterStyle.format("repeat", 3, styles) == "**"
    assert CounterStyle.format("fixed", 3, styles) == "A"
    assert CounterStyle.format("fixed", 9, styles) == "9"
    assert CounterStyle.format("roman", 16, styles) == "XVI"
    assert CounterStyle.format("roman", 0, styles) == "Z"
    assert CounterStyle.marker("inherited", 2, styles) == "[0010!"
    assert CounterStyle.format("cyclic", 0, styles) == "B"
    assert CounterStyle.format("arabic-indic", 23, %{}) == "٢٣"
    assert CounterStyle.format("lower-greek", 25, %{}) == "αα"
  end

  test "fixed negatives, inherited digits and generated bullets retain their symbols" do
    html = """
    <style>
    @counter-style label {system:fixed -1;symbols:'FIRST' 'SECOND';negative:'(' ')'}
    @counter-style digits {system:extends arabic-indic}
    @counter-style bullet {system:extends disc}
    .fixed {counter-reset:x -1}.fixed::before{content:counter(x,label)}
    .digits {counter-reset:x 12}.digits::before{content:counter(x,digits)}
    .bullet::before{content:counter(x,disc) counter(x,bullet)}
    </style><p class='fixed'> fixed</p><p class='digits'> inherited</p><p class='bullet'> bullet</p>
    """

    assert {:ok, pdf} = NativeElixirPdfUtilities.HtmlToPdf.render(html)
    assert {:ok, text} = NativeElixirPdfUtilities.Text.extract(pdf)
    assert text =~ "FIRST fixed"
    refute text =~ "(FIRST)"
    assert text =~ "١٢ inherited"
    assert text =~ "•• bullet"
  end

  test "counter styles are available to markers and generated nested counters" do
    html = """
    <style>
    @counter-style binary {system:numeric;symbols:'0' '1';suffix:')'}
    ol{list-style-type:binary;counter-reset:section list-item}
    li{counter-increment:section}
    li::before{content:counters(section,'.',binary) ' ' counter(list-item,upper-roman)}
    </style><ol><li>Outer<ol><li>Inner</li></ol></li><li>Next</li></ol>
    """

    assert {:ok, dom} = HtmlParser.parse_detailed(html)
    assert {:ok, styled} = Style.compute_detailed(dom)

    texts =
      walk(styled)
      |> Enum.filter(&(Map.get(&1, :tag) == "::before"))
      |> Enum.map(&hd(&1.children).text)

    assert texts == ["1 I", "1.1 I", "10 II"]
  end

  test "invalid descriptors fail with CSS diagnostics and cycles fall back" do
    for declaration <- [
          "system:numeric;symbols:'a'",
          "system:fixed a;symbols:'a'",
          "range:5 1;symbols:'a'",
          "pad:-1 'a';symbols:'a'",
          "additive-symbols:1 'a',2 'b';system:additive",
          "unknown:a"
        ] do
      assert {:error, {:invalid_css, %{stage: :css, source: source}}} =
               CssCounterStyleValidator.prepare("@counter-style bad {#{declaration}}")

      assert source =~ "@counter-style"
    end

    assert {:ok, styles} =
             CssCounterStyleValidator.prepare(
               "@counter-style a{system:fixed;symbols:'A';fallback:b}@counter-style b{system:fixed;symbols:'B';fallback:a}"
             )

    assert CounterStyle.format("a", 20, styles) == "20"
  end

  test "generated tokens preserve quoted delimiters and pagination references" do
    assert {:ok, [{:string, "a,b"}, {:counters, "section", ".", "decimal"}]} =
             CssGeneratedContentValidator.prepare("'a,b' counters(section,'.')")

    assert {:ok, [{:target_counter, "attr(href)", "page", "decimal"}, {:leader, "."}]} =
             CssGeneratedContentValidator.prepare(
               "target-counter(attr(href),page) leader(dotted)"
             )

    assert :error = CssGeneratedContentValidator.prepare("counter(a,)")
    assert :error = CssGeneratedContentValidator.prepare("counters(a, unquoted)")
    assert :error = CssGeneratedContentValidator.prepare("bad-function(a)")
  end

  test "generic reversed counters count visible increments and support explicit starts" do
    for {reset, expected} <- [
          {"reversed(section)", ["3", "2", "1"]},
          {"reversed(section) 10", ["9", "8", "7"]}
        ] do
      html =
        "<style>article{counter-reset:#{reset}}p{counter-increment:section}p::before{content:counter(section)}</style><article><p>A</p><p>B</p><p>C</p></article>"

      assert {:ok, dom} = HtmlParser.parse_detailed(html)
      assert {:ok, styled} = Style.compute_detailed(dom)

      assert walk(styled)
             |> Enum.filter(&(Map.get(&1, :tag) == "::before"))
             |> Enum.map(&hd(&1.children).text) == expected
    end
  end

  test "generated replacement text and marker styling retain public extraction" do
    alias NativeElixirPdfUtilities.{HtmlToPdf, Text}

    html =
      "<style>p{content:'Replacement'}li::marker{content:'Item: ';color:red}</style><p>Original</p><ul><li>Entry</li></ul>"

    assert {:ok, pdf} = HtmlToPdf.render(html)
    assert {:ok, extracted} = Text.extract(pdf)
    assert extracted =~ "Replacement"
    refute extracted =~ "Original"
    assert extracted =~ "Item:"
  end

  test "literal pseudo content joins the surrounding capitalization context" do
    alias NativeElixirPdfUtilities.{HtmlToPdf, Text}

    assert {:ok, pdf} =
             HtmlToPdf.render(
               "<style>p{text-transform:capitalize}p::before{content:'ab'}p::after{content:'gh'}</style><p>cd<span>ef</span></p>"
             )

    assert {:ok, text} = Text.extract(pdf)
    assert String.trim(text) == "Abcdefgh"
  end

  test "reversed counters keep explicit arithmetic and implicit increments distinct" do
    html = """
    <style>article{counter-reset:reversed(section) 10}
    p{counter-increment:section calc(1 + 2) section}
    p::before{counter-reset:reversed(unused) fixed 7;content:counter(section) ':' counter(unused)}
    p::after{counter-reset:reversed(local);counter-increment:local;content:counter(local)}</style>
    <article><p>Body</p><p>Next</p></article>
    """

    assert {:ok, dom} = HtmlParser.parse_detailed(html)
    assert {:ok, styled} = Style.compute_detailed(dom)

    generated =
      for %{tag: tag, children: [%{text: text}]} <- walk(styled),
          tag in ["::before", "::after"],
          do: {tag, text}

    assert generated == [
             {"::before", "12:0"},
             {"::after", "1"},
             {"::before", "14:0"},
             {"::after", "1"}
           ]
  end

  test "generated sequences interleave images and text, suppress markers and retain alternative text" do
    alias NativeElixirPdfUtilities.{HtmlToPdf, Text}

    png =
      File.read!(
        NativeElixirPdfUtilities.TestSupport.Paths.fixture(
          "html_to_pdf/browser_parity/assets/initial-small.png"
        )
      )

    source = "data:image/png;base64," <> Base.encode64(png)

    html =
      "<style>p::before{content:'Before' url('#{source}') 'After'}li::marker{content:none}.alt::after{content:'Visual' / 'Accessible'}.image::before{content:url('#{source}') / 'ImageAlternative'}</style><p>Body</p><ul><li>Item</li></ul><div class=alt></div><div class=image></div>"

    assert {:ok, dom} = HtmlParser.parse_detailed(html)
    assert {:ok, styled} = Style.compute_detailed(dom)

    before =
      Enum.find(walk(styled), &(Map.get(&1, :tag) == "::before" and length(&1.children) == 3))

    assert Enum.map(before.children, & &1.type) == [:text, :element, :text]
    assert Enum.at(before.children, 1).style.display == :image
    assert {:ok, pdf} = HtmlToPdf.render(html)
    assert {:ok, text} = Text.extract(pdf)
    assert text =~ "Before"
    assert text =~ "After"
    assert text =~ "Accessible"
    assert text =~ "ImageAlternative"
    refute text =~ "Visual"
    refute text =~ "•"
  end

  test "attribute fallbacks and invalid generated grammars have deterministic admission" do
    assert {:ok, [{:attr, "data-label", "Missing"}]} =
             CssGeneratedContentValidator.prepare("attr(data-label raw-string,'Missing')")

    for source <- [
          "counter(x,,decimal)",
          "counter(,x)",
          "[x]",
          "leader(unquoted)",
          "'a' /",
          "'a' / url(x)",
          "attr(x number)"
        ] do
      assert :error = CssGeneratedContentValidator.prepare(source)
    end
  end

  test "custom styles fall back when explicit ranges exceed system representation" do
    assert {:ok, styles} =
             CssCounterStyleValidator.prepare("""
             @counter-style symbolic{system:symbolic;symbols:'A';range:0 2}
             @counter-style fixed{system:fixed 2;symbols:'B';range:0 3}
             @counter-style additive{system:additive;additive-symbols:2 'C';range:0 3}
             """)

    for {style, ordinal} <- [
          {"symbolic", 0},
          {"fixed", 1},
          {"fixed", 3},
          {"additive", 0},
          {"additive", 3}
        ] do
      assert CounterStyle.format(style, ordinal, styles) == Integer.to_string(ordinal)
    end
  end

  test "anonymous list styles and alphabetic extensions retain their authored symbols" do
    assert {:ok, styles} =
             CssCounterStyleValidator.prepare("""
             @counter-style greek{system:extends lower-greek;prefix:'[';suffix:']'}
             @counter-style letters{system:extends upper-latin;range:1 infinite}
             @counter-style bilateral{system:numeric;symbols:'0' '1';range:-infinite infinite}
             """)

    assert CounterStyle.marker("greek", 3, styles) == "[γ]"
    assert CounterStyle.format("letters", 27, styles) == "AA"
    assert CounterStyle.format("bilateral", -4, styles) == "-100"
    assert CounterStyle.marker("missing", 2, styles) == "2. "

    alias NativeElixirPdfUtilities.{HtmlToPdf, Text}

    assert {:ok, pdf} =
             HtmlToPdf.render(
               "<ol style=\"list-style-type:symbols(cyclic 'A' 'B')\"><li>First</li><li>Second</li></ol>"
             )

    assert {:ok, text} = Text.extract(pdf)
    assert text =~ "A First"
    assert text =~ "B Second"

    assert {:error, _} =
             HtmlToPdf.render("<ol style='list-style-type:symbols(numeric)'><li>Bad</li></ol>")
  end

  test "counter descriptors reject malformed bounds and additive weights" do
    for descriptors <- [
          "system:additive;additive-symbols:1.5 'A'",
          "system:additive;additive-symbols:2",
          "system:numeric;symbols:'0' '1';range:bad 2",
          "system:numeric;symbols:'0' '1';range:2",
          "system:numeric;symbols:'0' '1';unknown:1"
        ] do
      assert :error = CssCounterStyleValidator.definition(descriptors)
    end

    assert :error = CssCounterStyleValidator.symbols("symbolic")

    assert {:ok, %{suffix: "!"}} =
             CssCounterStyleValidator.definition(
               "system:cyclic;symbols:'A';suffix:'!' !important"
             )
  end

  test "images in selected pseudo and replacement owners report normal asset errors" do
    alias NativeElixirPdfUtilities.HtmlToPdf

    for {selector, body} <- [
          {"li::marker", "<ul><li>Item</li></ul>"},
          {"p", "<p>Original</p>"},
          {"p::before", "<p>Body</p>"}
        ] do
      html = "<style>#{selector}{content:url(missing.png)}</style>" <> body
      assert {:error, {:invalid_document, diagnostic}} = HtmlToPdf.render(html)
      assert diagnostic.stage == :asset
      assert diagnostic.source == "missing.png"
    end
  end

  defp walk(node), do: [node | Enum.flat_map(Map.get(node, :children, []), &walk/1)]
end
