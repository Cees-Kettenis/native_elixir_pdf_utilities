defmodule NativeElixirPdfUtilities.HtmlToPdf.InlineTextContextTest do
  use ExUnit.Case, async: true

  alias NativeElixirPdfUtilities.HtmlToPdf.{Font, HtmlParser, Layout, Style, TextTransform}
  alias NativeElixirPdfUtilities.Validators.HtmlValidator

  test "direct body whitespace reaches shared normal, pre and pre-line layout" do
    for {white_space, separator} <- [{"normal", " "}, {"pre", "  \n "}, {"pre-line", "  \n "}] do
      html =
        "<body style='margin:0;font-size:10pt;line-height:12pt;white-space:#{white_space}'><span>Hello</span>#{separator}<span>world</span></body>"

      assert {:ok, dom} = HtmlParser.parse_detailed(html)
      assert [first, %{type: :text, text: ^separator}, last] = hd(dom.children).children
      assert first.tag == "span" and last.tag == "span"
      assert {:ok, styled} = Style.compute_detailed(dom, system_font_discovery: false)
      assert {:ok, layout} = Layout.layout(styled, page_size: {240, 120}, margin: 0)
      texts = Enum.filter(layout.boxes, &(&1.type == :text))
      hello = Enum.find(texts, &String.contains?(&1.text, "Hello"))
      world = Enum.find(texts, &String.contains?(&1.text, "world"))

      if white_space == "normal" do
        assert hello.y == world.y

        assert_in_delta world.x - hello.x,
                        Font.css_text_width("Hello ", hello.font_face, hello.font_size),
                        0.05
      else
        assert_in_delta hello.y - world.y, 12, 0.001
        if white_space == "pre", do: assert(world.x > 0), else: assert(world.x == 0)
      end
    end

    assert HtmlValidator.html_text_action("body", " \n ") == :text
    assert HtmlValidator.html_text_action("head", " \n ") == :ignore
  end

  test "capitalization carries word context across styled inline nodes and comments" do
    html =
      "<p style='text-transform:capitalize'>ab<span style='color:red'>cd</span><!--split-->ef <b>gh</b>ij <span style='text-transform:none'>kl</span>mn</p>"

    assert {:ok, dom} = HtmlParser.parse_detailed(html)
    assert {:ok, %{children: [paragraph]}} = Style.compute_detailed(dom)
    [ab, span, ef, bold, ij, unchanged, mn] = paragraph.children
    assert ab.text == "Ab"
    assert hd(span.children).text == "cd" and span.style.color == {1, 0, 0}
    assert ef.text == "ef "
    assert hd(bold.children).text == "Gh" and bold.style.font_weight == 700
    assert ij.text == "ij "
    assert hd(unchanged.children).text == "kl" and mn.text == "mn"
    assert hd(span.children).node_id == hd(Enum.at(hd(dom.children).children, 1).children).node_id
  end

  test "block, line break and atomic boundaries reset capitalization while hidden content does not" do
    html =
      "<div style='text-transform:capitalize'>ab<span hidden>hidden</span>cd<br>ef<div>gh</div>ij<span style='display:inline-block'>kl</span>mn<wbr>op</div>"

    assert {:ok, dom} = HtmlParser.parse_detailed(html)
    assert {:ok, tree} = Style.compute_detailed(dom)
    assert text(tree) == "AbcdEfGhIjKlMnop"
  end

  test "known and explicitly unknown language select casing across inline boundaries" do
    for lang <- ["tr", "az"] do
      html =
        "<div lang='#{lang}'><p style='text-transform:uppercase'>istanbul ıi</p><p style='text-transform:lowercase'>I<span>̇</span> Iİ</p><p style='text-transform:capitalize'>istanbul <span>izmir</span></p><p lang='' style='text-transform:uppercase'>istanbul</p></div>"

      assert {:ok, dom} = HtmlParser.parse_detailed(html)
      assert {:ok, %{children: [container]}} = Style.compute_detailed(dom)

      assert Enum.map(container.children, &text/1) == [
               "İSTANBUL Iİ",
               "i ıi",
               "İstanbul İzmir",
               "ISTANBUL"
             ]
    end

    html =
      "<head><meta http-equiv=content-language content=tr></head><body style='text-transform:uppercase'>istanbul</body>"

    assert {:ok, dom} = HtmlParser.parse_detailed(html)
    assert {:ok, tree} = Style.compute_detailed(dom)
    assert text(tree) == "İSTANBUL"

    assert {:ok, dom} =
             HtmlParser.parse_detailed("<style>body{text-transform:uppercase}</style>istanbul")

    assert {:ok, tree} = Style.compute_detailed(dom)
    assert text(tree) == "ISTANBUL"
  end

  test "collapsible tails after overflowing atomic inlines cannot create empty wrapped lines" do
    for tail <- ["", " ", "\n"] do
      html =
        "<body style='margin:0;line-height:12pt'><span style='display:inline-block;width:135pt;height:30pt'></span>#{tail}</body>"

      assert {:ok, dom} = HtmlParser.parse_detailed(html)
      assert {:ok, tree} = Style.compute_detailed(dom)
      assert {:ok, layout} = Layout.layout_viewport(tree, 131.25, 37.5)
      assert layout.content_height <= 37.5
    end
  end

  test "listbox intrinsic dimensions use the same transformed labels that are painted" do
    for {mode, source, expected} <- [
          {"lowercase", "WWWW", "wwww"},
          {"capitalize", "wwww", "Wwww"}
        ] do
      [transformed, control] =
        for {mode, value} <- [{mode, source}, {"none", expected}] do
          html =
            "<select size=2 style='text-transform:#{mode}'><option>#{value}</option></select>"

          assert {:ok, dom} = HtmlParser.parse_detailed(html)
          assert {:ok, %{children: [select]}} = Style.compute_detailed(dom, forms: :static)
          assert text(select) == expected
          select
        end

      assert transformed.style.width == control.style.width
      assert transformed.style.height == control.style.height
    end
  end

  test "deferred HTML text leaves already prepared SVG and MathML islands intact" do
    html =
      "<div style='text-transform:lowercase'>HELLO<svg width='20' height='20'><text x='0' y='12'>SVG</text></svg><math><mtext>WORLD</mtext></math>TAIL</div>"

    assert {:ok, dom} = HtmlParser.parse_detailed(html)
    assert {:ok, %{children: [container]}} = Style.compute_detailed(dom)
    assert hd(container.children).text == "hello"
    assert List.last(container.children).text == "tail"

    for node <- [container | container.children],
        key <- [:_case_language, :_case_id, :_case_island] do
      refute Map.has_key?(node, key)
    end

    assert {:ok, dom} = HtmlParser.parse_detailed("<p style='text-transform:capitalize'>x</p>")
    [%{children: [node]} = paragraph] = dom.children
    dom = %{dom | children: [%{paragraph | children: [%{node | text: ""}]}]}
    assert {:ok, tree} = Style.compute_detailed(dom)
    assert text(tree) == ""
  end

  test "Unicode casing retains word context, marks and final sigma across inline boundaries" do
    html =
      "<div><p style='text-transform:lowercase'>Ο<span>Σ</span> Ο<span>Σ</span>Α Σ ΟΣ'Α</p><p style='text-transform:capitalize'>ǳuro e<span>́cole</span> don't hello-world 123abc</p></div>"

    assert {:ok, dom} = HtmlParser.parse_detailed(html)
    assert {:ok, %{children: [container]}} = Style.compute_detailed(dom)

    assert Enum.map(container.children, &text/1) == [
             "ος οσα σ οσ'α",
             "ǲuro École Don't Hello-World 123abc"
           ]
  end

  test "pseudo-content participates in the same word without losing its color" do
    html =
      "<style>p{text-transform:capitalize}p::before{content:'ab';color:red}p::after{content:'gh';color:blue}</style><p>cd<span>ef</span></p>"

    assert {:ok, dom} = HtmlParser.parse_detailed(html)
    assert {:ok, %{children: [paragraph]}} = Style.compute_detailed(dom)
    assert text(paragraph) == "Abcdefgh"
    assert hd(paragraph.children).style.color == {1, 0, 0}
    assert List.last(paragraph.children).style.color == {0, 0, 1}
  end

  test "out-of-flow text gets its own context and cannot split the surrounding word" do
    html =
      "<p style='text-transform:capitalize'>ab<span style='position:absolute'>xy</span>cd<span style='position:absolute'>ef</span>gh</p>"

    assert {:ok, dom} = HtmlParser.parse_detailed(html)
    assert {:ok, %{children: [paragraph]}} = Style.compute_detailed(dom)
    [ab, positioned, cd, fixed, gh] = paragraph.children
    assert ab.text == "Ab" and cd.text == "cd" and gh.text == "gh"
    assert hd(positioned.children).text == "Xy"
    assert hd(fixed.children).text == "Ef"
  end

  test "text conversion state restores outer pending work after nested success and failures" do
    node = %{type: :text, text: "abc", style: %{text_transform: :capitalize}}

    assert {:ok, %{text: "Abc"}} =
             TextTransform.with_document(fn ->
               pending = TextTransform.defer(node, [])
               assert {:error, :stop} = TextTransform.with_document(fn -> {:error, :stop} end)

               assert {:ok, %{text: "inner"}} =
                        TextTransform.with_document(fn -> {:ok, %{text: "inner"}} end)

               {:ok, pending}
             end)

    assert Process.get({TextTransform, :pending}) == nil
    assert TextTransform.convert("ǳuro", :capitalize, []) == "ǲuro"
    assert TextTransform.convert("ΟΣ", :lowercase, []) == "ος"
    assert TextTransform.convert("I", :lowercase, ["tr"]) == "ı"
    assert TextTransform.convert("istanbul", :capitalize, ["tr"]) == "İstanbul"
    assert TextTransform.convert("MiXeD", :none, []) == "MiXeD"
  end

  defp text(node) do
    case node do
      %{type: :text, text: value} -> value
      %{children: children} -> Enum.map_join(children, &text/1)
    end
  end
end
