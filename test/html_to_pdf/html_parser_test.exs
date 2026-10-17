defmodule NativeElixirPdfUtilities.HtmlToPdf.HtmlParserTest do
  use ExUnit.Case

  import NativeElixirPdfUtilities.HtmlTreeHelpers

  alias NativeElixirPdfUtilities.HtmlToPdf.HtmlParser

  test "original CSS token spans are independent of decoded declaration provenance" do
    html = "\uFEFF<style>/*é🙂*/\r\np{width:3pt}</style>\r<p STYLE='width&colon;3pt'>Good</p>"
    assert {:ok, dom} = HtmlParser.parse_document_detailed(html)
    [%{children: [head, body]}] = dom.children
    [style] = head.children
    [text] = style.children
    [paragraph] = body.children
    assert style.source == %{text: "<style>", line: 1, column: 2}
    assert text.source == %{text: "/*é🙂*/\r\np{width:3pt}", line: 1, column: 9}
    assert text.text == "/*é🙂*/\np{width:3pt}"
    assert paragraph.source == %{text: "<p STYLE='width&colon;3pt'>", line: 3, column: 1}
    assert paragraph.attributes["style"] == "width:3pt"
    assert text.parent_id == style.node_id
    assert style.document_id == paragraph.document_id
    assert text.tree_scope_id == paragraph.tree_scope_id
    assert map_size(text.source) == 3 and map_size(paragraph.source) == 3
  end

  test "XML PI getter preserves tokenizer data, original authored bytes and canonical identities" do
    alias NativeElixirPdfUtilities.Validators.ProcessingInstructionValidator, as: PI

    token = "<?marker name='A&amp;B' Name='&amp;lt;'\r\nspace=' \t\r\nCafé'?>"
    html = "\uFEFF<p>e\u0301</p>\r\n  " <> token <> "<?marker name='A' name='B'?>"
    assert {:ok, %{children: [_, marker, malformed]}} = HtmlParser.parse_detailed(html)
    assert marker.source == %{text: token, line: 2, column: 3}
    assert marker.data == "name='A&amp;B' Name='&amp;lt;'\nspace=' \t\nCafé'"

    assert {:ok, %{"name" => "A&B", "Name" => "&lt;", "space" => " \t\nCafé"}} =
             PI.attributes(marker)

    assert {:ok, nil} = PI.attribute(malformed, "name")
    assert marker.node_id != malformed.node_id
    assert marker.document_id == malformed.document_id
    assert marker.tree_scope_id == malformed.tree_scope_id
    assert marker.parent_id == malformed.parent_id
    assert marker.source.text == token
    refute Map.has_key?(marker, :attributes)
    refute Map.has_key?(marker, :patch_state)
  end

  test "processing instructions retain case-sensitive targets, literal data and original source identity" do
    html = "<?_Mark-2 \tname='A&amp;B' ? literal\r\nCafé?>\r<p><?marker>Text<?end ?></p>"
    assert {:ok, %{children: [first, paragraph]}} = HtmlParser.parse_detailed(html)
    assert first.type == :processing_instruction
    assert first.target == "_Mark-2"
    assert first.data == "name='A&amp;B' ? literal\nCafé"
    assert is_reference(first.node_id)

    assert first.source == %{
             text: "<?_Mark-2 \tname='A&amp;B' ? literal\r\nCafé?>",
             line: 1,
             column: 1
           }

    assert [marker, %{text: "Text"}, ending] = paragraph.children
    assert marker.target == "marker" and marker.data == ""
    assert ending.target == "end" and ending.data == ""
    assert marker.source == %{text: "<?marker>", line: 3, column: 4}
    assert marker.node_id != ending.node_id

    assert {:ok, %{children: [%{data: "quoted='"}, %{text: "tail'?>"}]}} =
             HtmlParser.parse_fragment("<?marker quoted='>tail'?>", "div")
  end

  test "processing instructions occupy authored document and inert fragment positions" do
    html =
      "<?before><!doctype html><html><?inside><head><?head></head><?between><body><p>Text</p><?body></body><?after-body></html><?after>"

    assert {:ok, %{children: [before, root, after_node]}} =
             HtmlParser.parse_document_detailed(html)

    assert before.target == "before" and after_node.target == "after"
    assert [inside, head, between, body, after_body] = root.children

    assert inside.target == "inside" and between.target == "between" and
             after_body.target == "after-body"

    assert hd(head.children).target == "head"
    assert List.last(body.children).target == "body"

    assert {:ok, %{children: [before, %{tag: "html", children: [head, body]}]}} =
             HtmlParser.parse_document_detailed("<?before><title>Title</title><p>Body</p>")

    assert before.target == "before" and head.tag == "head" and body.tag == "body"

    assert {:ok, %{children: [template]}} =
             HtmlParser.parse_detailed(
               "<template><?marker name=A><template><?start name=B><p>Inert</p><?end></template></template>"
             )

    assert template.children == []
    assert [marker, inner] = template.content.children
    assert marker.data == "name=A"
    assert [start, _, ending] = inner.content.children
    assert start.target == "start" and ending.target == "end"

    for context <- [
          "div",
          "template",
          "select",
          "tr",
          "svg",
          "math",
          %{namespace: :svg, tag: "foreignObject"}
        ] do
      assert {:ok, %{children: [%{target: "marker", data: "name=A"}]}} =
               HtmlParser.parse_fragment("<?marker name=A>", context)
    end
  end

  test "processing instructions do not invalidate element content models or optional boundaries" do
    for html <- [
          "<ul><?marker><li>A<?end><li>B</ul>",
          "<dl><?marker><dt>Term<?end><dd>Definition</dl>",
          "<figure><?marker><figcaption>Caption</figcaption><p>Body</p><?end></figure>",
          "<table><?marker><colgroup><?marker><col></colgroup><tbody><?start><tr><?marker><td>A</td></tr><?end></tbody></table>",
          "<select><?marker><option>A<?end><option>B</select>",
          "<button><?marker><b>Button</b><?end></button>",
          "<a><?marker><p>Block</p><?end></a>",
          "<head><noscript><?marker><style>p{color:red}</style><?end></noscript></head>",
          "<title>Title</title><?marker></head><p>Body</p>"
        ] do
      assert {:ok, _} = HtmlParser.parse_document_detailed(html)
    end
  end

  test "processing instructions stay in implied table groups in insertion order" do
    for {html, expected} <- [
          {"<table><col><?between><col></colgroup><tr><td>A</td></tr><?between><tr><td>B</td></tr><?end></tbody></table>",
           ["colgroup", "tbody"]},
          {"<table><col><?between><col><tr><td>A</td></tr><?between><tr><td>B</td></tr><?end></table>",
           ["colgroup", "tbody"]}
        ] do
      assert {:ok, %{children: [%{children: [_, %{children: [table]}]}]}} =
               HtmlParser.parse_document_detailed(html)

      assert Enum.map(table.children, & &1.tag) == expected
      [columns, rows] = table.children
      assert [%{tag: "col"}, %{target: "between"}, %{tag: "col"}] = columns.children
      assert [%{tag: "tr"}, %{target: "between"}, %{tag: "tr"}, %{target: "end"}] = rows.children
    end
  end

  test "raw text and RCDATA never tokenize processing instructions" do
    for tag <- ~w(style title textarea script xmp noembed plaintext) do
      attrs = if tag == "script", do: " type=text/plain", else: ""
      closing = if tag == "plaintext", do: "", else: "</#{tag}>"

      assert {:ok, %{children: [%{children: [%{type: :text, text: text}]}]}} =
               HtmlParser.parse_detailed("<#{tag}#{attrs}><?marker name='A&amp;B'?>" <> closing)

      assert text ==
               if(tag in ~w(title textarea),
                 do: "<?marker name='A&B'?>",
                 else: "<?marker name='A&amp;B'?>"
               )
    end

    assert {:ok, %{children: [svg]}} =
             HtmlParser.parse_detailed("<svg><?marker><style><?inside></style></svg>")

    assert [%{target: "marker"}, %{children: [%{target: "inside"}]}] = svg.children
  end

  test "malformed processing instructions preserve original actionable diagnostics" do
    for token <- [
          "<?>",
          "<?1bad>",
          "<?é>",
          "<?bad.name>",
          "<?xml?>",
          "<?XML-stylesheet href='x'?>",
          "<?marker",
          "<?marker a??>",
          "<?marker a\0>"
        ] do
      for newline <- ["\r\n", "\r", "\n"] do
        assert {:error, {:unsupported_html, diagnostic}} =
                 HtmlParser.parse_detailed("<div>é#{newline}  #{token}")

        assert diagnostic.stage == :html and diagnostic.reason == :unsupported_html
        assert diagnostic.source == token and diagnostic.line == 2 and diagnostic.column == 3
        assert diagnostic.message =~ "processing instructions require"
      end
    end

    assert {:error, {:unsupported_html, diagnostic}} =
             HtmlParser.parse_detailed(
               "<template><?marker><p onclick='run()'>Hidden</p></template>"
             )

    assert diagnostic.source == "<p onclick='run()'>"
  end

  test "processing instructions consume shared node, source, depth and tree work budgets" do
    alias NativeElixirPdfUtilities.Limits
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    html = "<template><?marker><?end></template>"
    Limits.install(%{limits | max_html_tree_work: 3 * byte_size(html)})
    assert {:error, {:resource_limit_exceeded, diagnostic}} = HtmlParser.parse_detailed(html)
    assert diagnostic.stage == :html and diagnostic.message =~ "max_html_tree_work"

    Limits.install(%{limits | max_html_nodes: 2})

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             HtmlParser.parse_detailed("<?marker><?end><?marker>")

    assert diagnostic.message =~ "max_html_nodes"

    Limits.install(%{limits | max_html_source_bytes: 8})

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             HtmlParser.parse_detailed("<?marker>")

    assert diagnostic.message =~ "max_html_source_bytes"

    Limits.install(%{limits | max_aggregate_html_source_bytes: 8})
    assert {:error, {:resource_limit_exceeded, _}} = HtmlParser.parse_detailed("<?marker>")

    Limits.install(%{limits | max_html_depth: 1})

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             HtmlParser.parse_detailed("<template><div><?marker></div></template>")

    assert diagnostic.message =~ "max_html_depth"
  end

  test "legacy raw text preserves markup, entities and initial newlines after preprocessing" do
    for tag <- ["xmp", "noembed"], newline <- ["\r", "\r\n", "\n"] do
      literal =
        "#{newline}é &amp; &#13; <img src=denied.png><script>run()</script>" <>
          "<!doctype html><!--literal-->#{newline}</#{tag}suffix> </#{tag}é>"

      expected = String.replace(literal, ~r/\r\n?/, "\n")

      assert {:ok, %{children: [element, after_element]}} =
               HtmlParser.parse_detailed(
                 "<#{tag}>#{literal}</#{String.upcase(tag)} \t\f#{newline}><p>After</p>"
               )

      assert element.tag == tag
      assert tree_content(element.children) == [%{type: :text, text: expected}]
      assert tree_content(after_element.children) == [%{type: :text, text: "After"}]

      for context <- [tag, String.upcase(tag)] do
        fragment = literal <> "</#{tag}><p>Still literal</p>"
        expected_fragment = String.replace(fragment, ~r/\r\n?/, "\n")

        assert {:ok, %{children: [%{type: :text, text: ^expected_fragment}]}} =
                 HtmlParser.parse_fragment(fragment, context)
      end

      assert {:ok, %{children: []}} = HtmlParser.parse_fragment("", tag)

      assert {:ok, %{children: [%{children: []}]}} =
               HtmlParser.parse_detailed("<#{tag}></#{tag}>")
    end
  end

  test "plaintext consumes all remaining input including its own and ancestor closing tags" do
    for newline <- ["\r", "\r\n", "\n"] do
      suffix = "#{newline}é &amp; &#13;<b>Literal</b></plaintext></div></body></html>"
      expected = String.replace(suffix, ~r/\r\n?/, "\n")

      assert {:ok, %{children: [html]}} =
               HtmlParser.parse_document_detailed(
                 "<html><body><div><p>Before<plaintext>" <> suffix
               )

      [_, body] = html.children
      assert [%{tag: "div", children: [%{tag: "p"}, plaintext]}] = body.children
      assert plaintext.tag == "plaintext"
      assert tree_content(plaintext.children) == [%{type: :text, text: expected}]

      assert {:ok, %{children: [%{text: ^expected}]}} =
               HtmlParser.parse_fragment(suffix, "PLAINTEXT")
    end

    assert {:ok, %{children: [%{tag: "plaintext", children: []}]}} =
             HtmlParser.parse_detailed("<plaintext>")

    assert {:ok, %{children: [%{tag: "div", children: [%{tag: "plaintext", children: []}]}]}} =
             HtmlParser.parse_detailed("<div><plaintext>")

    assert {:ok, %{children: [%{tag: "div", children: [template]}]}} =
             HtmlParser.parse_detailed(
               "<div><template><plaintext>é &amp;</plaintext></template></div>"
             )

    assert template.children == []

    assert [%{tag: "plaintext", children: [%{text: "é &amp;</plaintext></template></div>"}]}] =
             template.content.children
  end

  test "raw text boundaries respect HTML nesting and foreign integration contexts" do
    assert {:ok, %{children: [div]}} =
             HtmlParser.parse_detailed(
               "<div><p>Before<xmp><xmp>Nested-looking</xmp><p>After</p>" <>
                 "<a><xmp>Anchor block</xmp></a><p><noembed><p>Literal</p></noembed></p>" <>
                 "<template><xmp><input name=unused></xmp></template></div>"
             )

    assert Enum.map(div.children, & &1.tag) == ~w(p xmp p a p template)

    assert tree_content(Enum.at(div.children, 1).children) == [
             %{type: :text, text: "<xmp>Nested-looking"}
           ]

    assert tree_content(Enum.at(div.children, 4).children) == [
             %{
               type: :element,
               tag: "noembed",
               attributes: %{},
               children: [%{type: :text, text: "<p>Literal</p>"}]
             }
           ]

    [inert] = List.last(div.children).content.children
    assert tree_content(inert.children) == [%{type: :text, text: "<input name=unused>"}]

    for tag <- ~w(xmp noembed plaintext) do
      assert {:ok, %{children: [svg]}} =
               HtmlParser.parse_detailed("<svg><#{tag}><path/></#{tag}></svg>")

      assert [%{tag: ^tag, namespace: :svg, children: [%{tag: "path", namespace: :svg}]}] =
               svg.children

      context = %{namespace: :svg, tag: tag}

      assert {:ok, %{children: [%{tag: "path", namespace: :svg}]}} =
               HtmlParser.parse_fragment("<path/>", context)

      assert {:error, {:unsupported_html, _}} = HtmlParser.parse_detailed("<svg><#{tag}>")
    end

    for {prefix, suffix} <- [
          {"<svg><foreignObject>", "</foreignObject></svg>"},
          {"<math><mtext>", "</mtext></math>"},
          {"<math><annotation-xml encoding=text/html>", "</annotation-xml></math>"}
        ] do
      assert {:ok, _} =
               HtmlParser.parse_detailed(
                 prefix <> "<xmp><script>Literal</script></xmp>" <> suffix
               )

      assert {:ok, _} = HtmlParser.parse_detailed(prefix <> "<plaintext>Literal" <> suffix)
    end
  end

  test "raw text syntax failures preserve caller closing boundaries and source locations" do
    for tag <- ~w(xmp noembed plaintext) do
      assert {:error, {:unsupported_html, diagnostic}} =
               HtmlParser.parse_detailed(
                 "<#{tag}/><script>Cannot leave raw mode this way</script>"
               )

      assert diagnostic.stage == :html and diagnostic.source == "<#{tag}/>"
    end

    for tag <- ["xmp", "noembed"], newline <- ["\r", "\r\n", "\n"] do
      token = "</#{String.upcase(tag)} #{newline}bad=1>"

      assert {:error, {:unsupported_html, diagnostic}} =
               HtmlParser.parse_detailed("<#{tag}>é#{newline}  " <> token)

      assert diagnostic.stage == :html and diagnostic.reason == :unsupported_html
      assert diagnostic.line == 2 and diagnostic.column == 3 and diagnostic.source == token

      assert {:error, {:unsupported_html, detail}} =
               HtmlParser.parse_detailed("<#{tag}>é#{newline}</#{tag}/>")

      assert detail.line == 2 and detail.column == 1 and detail.source == "</#{tag}/>"

      for html <- ["<#{tag}>Unclosed", "<#{tag}>Unclosed</#{tag} "] do
        assert {:error, {:unsupported_html, failure}} = HtmlParser.parse_detailed(html)
        assert failure.stage == :html and failure.line == 1 and is_binary(failure.source)
      end
    end
  end

  test "gauges preserve authored numeric values and fallback phrasing but forbid same-element descendants" do
    html =
      "<label>Usage <meter value=' +.5tail' min=-2 max=2 low=-1 high=1 optimum=0><span>Fallback</span><progress></progress></meter></label>"

    assert {:ok, %{children: [label]}} = HtmlParser.parse_detailed(html)
    meter = Enum.at(label.children, 1)
    assert meter.attributes["value"] == " +.5tail"
    assert Enum.map(meter.children, & &1.tag) == ["span", "progress"]

    for tag <- ~w(progress meter) do
      assert {:ok, _} = HtmlParser.parse_detailed("<a href='#g'><#{tag} id=g></#{tag}></a>")
      assert {:ok, _} = HtmlParser.parse_detailed("<button><#{tag}></#{tag}></button>")
      assert {:ok, _} = HtmlParser.parse_fragment_detailed("<span>Fallback</span>", tag)

      for html <- [
            "<#{tag}><span><#{tag}></#{tag}></span></#{tag}>",
            "<#{tag}><div>Invalid</div></#{tag}>",
            "<#{tag} form=f></#{tag}>",
            "<#{tag} name=g></#{tag}>"
          ] do
        assert {:error, {:unsupported_html, diagnostic}} = HtmlParser.parse_detailed(html)
        assert diagnostic.stage == :html and diagnostic.reason == :unsupported_html
        assert is_binary(diagnostic.source) and is_integer(diagnostic.line)
      end
    end
  end

  test "raw text retains source, node and depth budgets without treating literal tags as nodes" do
    alias NativeElixirPdfUtilities.Limits
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)

    for tag <- ~w(xmp noembed plaintext) do
      html = "<#{tag}>é\r\n" <> String.duplicate("<img src=denied.png>&amp;", 20)
      html = if tag == "plaintext", do: html, else: html <> "</#{tag}>"

      Limits.install(%{limits | max_html_source_bytes: byte_size(html) - 1})
      assert {:error, {:resource_limit_exceeded, diagnostic}} = HtmlParser.parse_detailed(html)
      assert diagnostic.stage == :html and diagnostic.message =~ "max_html_source_bytes"

      Limits.install(%{limits | max_aggregate_html_source_bytes: byte_size(html) - 1})
      assert {:error, {:resource_limit_exceeded, diagnostic}} = HtmlParser.parse_detailed(html)
      assert diagnostic.stage == :html and diagnostic.message =~ "max_aggregate_html_source_bytes"

      Limits.install(%{limits | max_html_nodes: if(tag == "plaintext", do: 2, else: 3)})
      assert {:ok, %{children: [%{children: [%{type: :text}]}]}} = HtmlParser.parse_detailed(html)

      Limits.install(%{limits | max_html_nodes: 1})
      assert {:error, {:resource_limit_exceeded, diagnostic}} = HtmlParser.parse_detailed(html)

      assert diagnostic.stage == :html and
               diagnostic.module == NativeElixirPdfUtilities.Validators.HtmlValidator

      assert diagnostic.message =~ "max_html_nodes"

      assert {:ok, _} = HtmlParser.parse_fragment(html, tag)
      Limits.install(%{limits | max_html_depth: 1})

      assert {:error, {:resource_limit_exceeded, diagnostic}} =
               HtmlParser.parse_detailed("<div>" <> html <> "</div>")

      assert diagnostic.stage == :html and diagnostic.message =~ "max_html_depth"
    end
  end

  test "contenteditable retains authored keywords across HTML foreign and inert content" do
    for value <- ["", "TRUE", "false", "plaintext-only", "invalid", " true ", " false ", "truſ"] do
      assert {:ok, %{children: [node]}} =
               HtmlParser.parse_detailed("<div contenteditable='#{value}'>Text</div>")

      assert node.attributes["contenteditable"] == value
    end

    assert {:ok, %{children: [%{attributes: %{"contenteditable" => ""}}]}} =
             HtmlParser.parse_detailed("<x-editor contenteditable>Text</x-editor>")

    assert {:ok, %{children: [svg, math, template]}} =
             HtmlParser.parse_detailed(
               "<svg contenteditable=true><text contenteditable=false>Foreign</text></svg><math contenteditable=true><mi>x</mi></math><template><div contenteditable>Unused</div></template>"
             )

    assert svg.namespace == :svg and math.namespace == :mathml
    assert hd(svg.children).attributes["contenteditable"] == "false"
    assert hd(template.content.children).attributes["contenteditable"] == ""

    for html <- [
          "<div contenteditable><script>run()</script></div>",
          "<div contenteditable=false><span onclick='run()'>Text</span></div>"
        ] do
      assert {:error, {:unsupported_html, diagnostic}} = HtmlParser.parse_detailed(html)

      assert diagnostic.stage == :html and is_integer(diagnostic.line) and
               is_integer(diagnostic.column)
    end
  end

  test "preprocesses literal newlines in every tokenizer context before decoding references" do
    for newline <- ["\r", "\r\n", "\n"] do
      html =
        "<div data-lines='A#{newline}B&#13;C' title=unquoted>" <>
          "A#{newline}B&#13;C<pre>#{newline}First#{newline}Second</pre>" <>
          "<textarea>#{newline}First#{newline}Second&#13;Third</textarea>" <>
          "<script type=text/plain>A#{newline}B&amp;&#13;</script>" <>
          "<template><p data-lines='A#{newline}B'>A#{newline}B</p></template>" <>
          "<svg><text data-lines='A#{newline}B'>A#{newline}B&#13;</text></svg></div>"

      assert {:ok, %{children: [div]}} = HtmlParser.parse_detailed(html)
      assert div.attributes["data-lines"] == "A\nB\rC"
      [text, pre, textarea, script, template, svg] = div.children
      assert text.text == "A\nB\rC"
      assert tree_content(pre.children) == [%{type: :text, text: "First\nSecond"}]
      assert tree_content(textarea.children) == [%{type: :text, text: "First\nSecond\rThird"}]
      assert tree_content(script.children) == [%{type: :text, text: "A\nB&amp;&#13;"}]
      [p] = template.content.children
      assert p.attributes["data-lines"] == "A\nB"
      assert tree_content(p.children) == [%{type: :text, text: "A\nB"}]
      [foreign_text] = svg.children
      assert foreign_text.attributes["data-lines"] == "A\nB"
      assert tree_content(foreign_text.children) == [%{type: :text, text: "A\nB\r"}]

      for context <- ["p", "title", "textarea", "style", "script", "svg"] do
        expected = if context in ["style", "script"], do: "A\nB&#13;", else: "A\nB\r"

        assert {:ok, %{children: [%{text: ^expected}]}} =
                 HtmlParser.parse_fragment("A#{newline}B&#13;", context)
      end

      assert {:ok, expected} = HtmlParser.parse_document_detailed("<p>A\nB</p>")

      assert tree_content(HtmlParser.parse_document_detailed("<p>A#{newline}B</p>")) ==
               {:ok, tree_content(expected)}
    end

    assert {:ok, %{children: [%{text: "A\n\nB\nC\n"}]}} =
             HtmlParser.parse_fragment("A\r\r\nB\rC\r", "textarea")

    assert {:ok, %{children: [%{tag: "p", attributes: %{"title" => "A"}}]}} =
             HtmlParser.parse_detailed("<p\rtitle=A\r\n>Text</p>")
  end

  test "newline preprocessing preserves original diagnostic tokens and coordinates" do
    for newline <- ["\r", "\r\n", "\n"] do
      token = "<span title='A#{newline}B' onclick=bad>"
      html = "<div>é#{newline}  " <> token <> "Text</span></div>"
      assert {:error, {:unsupported_html, diagnostic}} = HtmlParser.parse_detailed(html)
      assert diagnostic.stage == :html and diagnostic.reason == :unsupported_html
      assert diagnostic.line == 2 and diagnostic.column == 3
      assert diagnostic.source == token
      assert diagnostic.message =~ "attributes"

      for parse <- [&HtmlParser.parse_document_detailed/1, &HtmlParser.parse_fragment(&1, "div")] do
        assert {:error, {:unsupported_html, detail}} = parse.(html)
        assert detail.line == 2 and detail.column == 3 and detail.source == token
      end

      assert {:error, {:unsupported_html, detail}} =
               HtmlParser.parse_detailed("<div>é#{newline}  <marquee>Bad</marquee></div>")

      assert detail.line == 2 and detail.column == 3 and detail.source == "<marquee>"
    end

    assert {:error, {:unsupported_html, detail}} =
             HtmlParser.parse_detailed("\r\n<div>é\rX\r\n  <span bad=1></span></div>")

    assert detail.line == 4 and detail.column == 3 and detail.source == "<span bad=1>"

    for prefix <- [
          "<p>e\u0301🙂&#13;&#10;</p>",
          "<p title='e\u0301🙂&#13;&#10;'>Text</p>",
          "<textarea>e\u0301🙂&#13;&#10;</textarea>",
          "<script type=text/plain>e\u0301🙂&#13;&#10;</script>",
          "<template><p>e\u0301🙂&#13;&#10;</p></template>"
        ] do
      token = "<span onclick=bad>"
      html = "<div>\r\n  " <> prefix <> token <> "Bad</span></div>"

      assert {:error, {:unsupported_html, diagnostic}} = HtmlParser.parse_detailed(html)
      assert diagnostic.stage == :html and diagnostic.reason == :unsupported_html
      assert diagnostic.source == token and diagnostic.line == 2
      assert diagnostic.column == 3 + String.length(prefix)
    end
  end

  test "anchors inherit parent content models and buttons accept static phrasing content" do
    assert {:ok, %{children: [div]}} =
             HtmlParser.parse_detailed(
               "<div><a href='https://example.com'><p>Prose</p><ul><li>List<li>Next</ul><div>Block</div></a></div>"
             )

    [anchor] = div.children
    assert Enum.map(anchor.children, & &1.tag) == ["p", "ul", "div"]

    assert tree_content(Enum.map(Enum.at(anchor.children, 1).children, & &1.children)) ==
             [[%{type: :text, text: "List"}], [%{type: :text, text: "Next"}]]

    valid =
      "<button><code>Code</code><mark>Mark</mark><ins><span>Added</span></ins>" <>
        "<del>Removed</del><abbr title=Title>Abbr</abbr><br><img src=asset.png>" <>
        "<a>Placeholder</a><input type=hidden><template><div><button>Inert</button></div></template></button>"

    assert {:ok, %{children: [button]}} = HtmlParser.parse_detailed(valid)

    assert Enum.map(button.children, & &1.tag) ==
             ~w(code mark ins del abbr br img a input template)

    assert {:ok, _} = HtmlParser.parse_fragment(valid, "div")

    assert {:ok, _} =
             HtmlParser.parse_detailed("<a><template><div><a>Inert</a></div></template></a>")

    assert {:ok, _} = HtmlParser.parse_detailed("<div><a><svg><a>Foreign</a></svg></a></div>")
    assert {:ok, _} = HtmlParser.parse_detailed("<button><math><mi>x</mi></math></button>")

    for html <- [
          "<p><a><div>Invalid</div></a></p>",
          "<p><a><ins><div>Invalid</div></ins></a></p>",
          "<div><a><span><a>Nested</a></span></a></div>",
          "<a><span><input></span></a>",
          "<a><div><details><summary>Interactive</summary></details></div></a>",
          "<a><label>Interactive</label></a>",
          "<button><div>Block</div></button>",
          "<button><span><button>Nested</button></span></button>",
          "<button><a href='https://example.com'>Interactive</a></button>",
          "<button><ins><input></ins></button>",
          "<a><svg><foreignObject><input></foreignObject></svg></a>"
        ] do
      assert {:error, {:unsupported_html, detail}} = HtmlParser.parse_detailed(html)
      assert detail.stage == :html and detail.line == 1
      assert is_integer(detail.column) and is_binary(detail.source)
    end
  end

  test "newline preprocessing accounts for caller bytes before normalization" do
    alias NativeElixirPdfUtilities.Limits
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    html = "<p>A\r\nB</p>"
    bytes = byte_size(html)
    Limits.install(%{limits | max_html_source_bytes: bytes - 1})
    assert {:error, {:resource_limit_exceeded, diagnostic}} = HtmlParser.parse_detailed(html)
    assert diagnostic.stage == :html and diagnostic.message =~ "max_html_source_bytes"
    Limits.install(%{limits | max_aggregate_html_source_bytes: bytes - 1})
    assert {:error, {:resource_limit_exceeded, diagnostic}} = HtmlParser.parse_detailed(html)
    assert diagnostic.stage == :html and diagnostic.message =~ "max_aggregate_html_source_bytes"

    Limits.install(%{
      limits
      | max_html_source_bytes: bytes,
        max_aggregate_html_source_bytes: bytes
    })

    assert {:ok, _} = HtmlParser.parse_detailed(html)
  end

  test "anchor and button fragment contexts reject interactive descendants" do
    for context <- ["a", "button"],
        child <- ["<input>", "<span><select><option>A</select></span>", "<label>Label</label>"] do
      assert {:error, {:unsupported_html, diagnostic}} = HtmlParser.parse_fragment(child, context)
      assert diagnostic.stage == :html and diagnostic.line == 1
    end

    assert {:ok, _} = HtmlParser.parse_fragment("<a>Placeholder</a><input type=hidden>", "button")
    assert {:ok, _} = HtmlParser.parse_fragment("<p>Block</p><input type=hidden>", "a")
  end

  test "named and numeric references retain pinned tokenizer semantics after preprocessing" do
    source =
      "&notit; &notin; &amp= &copyé &#13; &#x80; &#x81; &#xFDD0; &#x10FFFF; &#x110000; &#xD800; &#0; &#x; &unknown;"

    text = "¬it; ∉ &= ©é \r € \u0081 \uFDD0 \u{10FFFF} � � � &#x; &unknown;"
    attribute = "&notit; ∉ &amp= ©é \r € \u0081 \uFDD0 \u{10FFFF} � � � &#x; &unknown;"

    for quote <- ["'", "\""] do
      assert {:ok, %{children: [%{attributes: attrs, children: [%{text: ^text}]}]}} =
               HtmlParser.parse_detailed("<p title=#{quote}#{source}#{quote}>#{source}</p>")

      assert attrs["title"] == attribute
    end

    assert {:ok, %{children: [%{attributes: %{"title" => "&notit;"}}]}} =
             HtmlParser.parse_detailed("<p title=&notit;>Text</p>")

    assert {:ok, %{children: [%{text: ^text}]}} = HtmlParser.parse_fragment(source, "textarea")
  end

  test "microdata and translate retain authored global values on builtins and custom hosts" do
    metadata =
      " ITEMSCOPE=false itemtype='https://schema.org/Person' itemid=' ./people/42 ' itemprop='name https://example.org/label' itemref='extra\tother' TRANSLATE=No"

    for {tag, content} <- [
          {"div", "Words"},
          {"span", "Words"},
          {"html", "Words"},
          {"body", "Words"},
          {"table", ""},
          {"meta", ""},
          {"input", ""},
          {"x-box", "Words"},
          {"script", "{}"},
          {"template", "<p>Inert</p>"}
        ] do
      type = if tag == "script", do: " type=application/json", else: ""
      ending = if tag in ~w(meta input), do: "", else: content <> "</#{tag}>"

      assert {:ok, %{children: [node]}} =
               HtmlParser.parse_detailed("<#{tag}#{type}#{metadata}>#{ending}")

      assert node.attributes["itemscope"] == "false"
      assert node.attributes["itemtype"] == "https://schema.org/Person"
      assert node.attributes["itemid"] == " ./people/42 "
      assert node.attributes["itemprop"] == "name https://example.org/label"
      assert node.attributes["itemref"] == "extra\tother"
      assert node.attributes["translate"] == "No"
    end

    assert {:ok, %{children: [custom]}} =
             HtmlParser.parse_detailed(
               "<x-box arbitrary='Unchanged' xml:itemtype=relative></x-box>"
             )

    assert custom.attributes == %{"arbitrary" => "Unchanged", "xml:itemtype" => "relative"}
  end

  test "foreign metadata stays inert and HTML integration hosts apply HTML semantics" do
    for {source, namespace} <- [
          {"<svg itemtype=relative itemid='bad URL' itemprop='bad.name' itemref='a a' translate=no><g xml:itemtype=relative></g></svg>",
           :svg},
          {"<math itemtype=relative itemid='bad URL' itemprop='bad.name' itemref='a a' translate=no><mi xml:translate=no>x</mi></math>",
           :mathml}
        ] do
      assert {:ok, %{children: [foreign]}} = HtmlParser.parse_detailed(source)
      assert foreign.namespace == namespace
      assert foreign.attributes["itemtype"] == "relative"
      assert foreign.attributes["translate"] == "no"

      assert Map.get(
               hd(foreign.children).attributes,
               "xml:itemtype",
               Map.get(hd(foreign.children).attributes, "xml:translate")
             ) in ["relative", "no"]
    end

    for source <- [
          "<svg><foreignObject><div itemtype=relative>HTML</div></foreignObject></svg>",
          "<math><mtext><span itemtype=relative>HTML</span></mtext></math>"
        ] do
      assert {:error, {:unsupported_html, diagnostic}} = HtmlParser.parse_detailed(source)
      assert diagnostic.message =~ "itemscope"
      assert diagnostic.line == 1 and is_integer(diagnostic.column)
    end

    assert {:ok, %{children: [%{children: [%{children: [html]}]}]}} =
             HtmlParser.parse_detailed(
               "<svg><foreignObject><p itemscope itemtype='urn:example:type' translate=YES>HTML</p></foreignObject></svg>"
             )

    assert html.namespace == :html and html.attribute_namespaces == %{}
    assert html.attributes["translate"] == "YES"
  end

  test "unselected responsive candidates retain the static active-URL policy" do
    for html <- [
          "<img src=valid srcset='javascript:bad 2x'>",
          "<picture><source media=screen srcset='vbscript:bad 1x'><img src=valid></picture>",
          "<template><img srcset='javascript:bad 1x'></template>"
        ] do
      assert {:error, {:unsupported_html, diagnostic}} = HtmlParser.parse_detailed(html)
      assert diagnostic.stage == :html and diagnostic.source =~ "srcset"
    end
  end

  test "picture/source parsing preserves order, descriptors and script-supporting children" do
    html =
      "<picture> <source srcset='a 1x, b 2x' media=print type=image/png sizes=20px width=20 height=10><script type=text/plain>Data</script><img src=c alt=Image><template><img src=inert></template></picture>"

    assert {:ok, %{children: [picture]}} = HtmlParser.parse_detailed(html)
    assert picture.tag == "picture"

    assert Enum.map(Enum.filter(picture.children, &(&1.type == :element)), & &1.tag) ==
             ~w(source script img template)

    source = Enum.find(picture.children, &match?(%{tag: "source"}, &1))
    assert source.children == [] and source.attributes["srcset"] == "a 1x, b 2x"

    for html <- [
          "<source srcset=a>",
          "<picture></picture>",
          "<picture><img src=a><source srcset=b></picture>",
          "<picture><img src=a><img src=b></picture>",
          "<picture>Prose<img src=a></picture>",
          "<picture><source src=a><img src=b></picture>",
          "<picture><div></div><img src=a></picture>"
        ] do
      assert {:error, {reason, diagnostic}} = HtmlParser.parse_detailed(html)
      assert reason in [:invalid_html, :unsupported_html]
      assert diagnostic.stage == :html and diagnostic.line == 1 and diagnostic.column > 0
    end
  end

  test "popover attributes survive parsing while hidden descendants retain input validation" do
    for value <- ["", "AUTO", "hint", "manual", "invalid", " auto "] do
      assert {:ok, %{children: [%{tag: "x-box", attributes: attributes}]}} =
               HtmlParser.parse_detailed("<x-box popover='#{value}'><p>Closed</p></x-box>")

      assert attributes["popover"] == value
    end

    assert {:ok, _} =
             HtmlParser.parse_detailed("<script popover type=application/json>{}</script>")

    for html <- [
          "<div popover><script>run()</script></div>",
          "<div popover><span onclick='run()'>Closed</span></div>",
          "<div popover><img src='java&#9;script:run()'></div>",
          "<a popover href='vbscript:run()'>Closed</a>"
        ] do
      assert {:error, {:unsupported_html, diagnostic}} = HtmlParser.parse_detailed(html)
      assert diagnostic.stage == :html
      assert is_integer(diagnostic.line) and is_integer(diagnostic.column)
      assert is_binary(diagnostic.message)
    end
  end

  test "form and label containers retain inert submission and explicit ownership attributes" do
    html =
      "<input form='late' name='outside'><form id='late' action='https://example.com/send' method='post' name='record'><label for='inside'>Name</label><input id='inside' form='late' name='inside'></form>"

    assert {:ok, %{children: [outside, form]}} = HtmlParser.parse_detailed(html)
    assert outside.attributes["form"] == "late"
    assert form.tag == "form"
    assert form.attributes["action"] == "https://example.com/send"
    assert form.attributes["method"] == "post"
    [label, inside] = form.children
    assert label.tag == "label" and label.attributes["for"] == "inside"
    assert inside.attributes["form"] == "late"

    for source <- [
          "<form><div><form></form></div></form>",
          "<label><span><label>Nested</label></span></label>"
        ] do
      assert {:error, {reason, diagnostic}} = HtmlParser.parse_detailed(source)
      assert diagnostic.reason == reason
      assert is_binary(diagnostic.message) and diagnostic.message != ""
    end
  end

  test "language attributes remain global, literal and namespace-aware" do
    for tag <- ["html", "body", "div", "p", "span", "h1", "x-box"] do
      assert {:ok, %{children: [node]}} =
               HtmlParser.parse_detailed("<#{tag} LANG='en-US' xml:lang='EN-us'>Words</#{tag}>")

      assert node.attributes["lang"] == "en-US"
      assert node.attributes["xml:lang"] == "EN-us"
      assert Map.get(node, :attribute_namespaces, %{}) == %{}
    end

    for source <- [
          "<script type='text/plain' lang=fr xml:lang=FR>Data</script>",
          "<template lang=fr xml:lang=FR><p>Inert</p></template>"
        ] do
      assert {:ok, %{children: [%{attributes: attributes}]}} = HtmlParser.parse_detailed(source)
      assert attributes["lang"] == "fr" and attributes["xml:lang"] == "FR"
    end

    for value <- ["", "xyzzy", " en ", "und", "zh-Hant-TW"] do
      assert {:ok, %{children: [%{attributes: attributes}]}} =
               HtmlParser.parse_detailed("<p lang='#{value}'>Words</p>")

      assert attributes["lang"] == value
    end

    assert {:ok, %{children: [%{attributes: attributes}]}} =
             HtmlParser.parse_detailed("<input name='field' value='Text' lang=fr xml:lang=FR>")

    assert attributes["lang"] == "fr" and attributes["xml:lang"] == "FR"

    assert {:ok, %{children: [node]}} =
             HtmlParser.parse_detailed("<svg><text xml:lang='de'>Words</text></svg>")

    [text] = node.children

    assert text.attribute_namespaces["xml:lang"].namespace ==
             "http://www.w3.org/XML/1998/namespace"
  end

  test "heading offset and reset attributes retain global values and custom container context" do
    for tag <- ["html", "body", "div", "section", "h1", "span", "x-box"] do
      html = "<#{tag} headingoffset='+03tail' headingreset='false'>Heading</#{tag}>"
      assert {:ok, %{children: [node]}} = HtmlParser.parse_detailed(html)
      assert node.attributes["headingoffset"] == "+03tail"
      assert node.attributes["headingreset"] == "false"
    end

    for html <- [
          "<script type='text/plain' headingoffset='1' headingreset>Data</script>",
          "<template headingoffset='2' headingreset><h1>Content</h1></template>"
        ] do
      assert {:ok, %{children: [node]}} = HtmlParser.parse_detailed(html)
      assert node.attributes["headingoffset"] in ["1", "2"]
      assert node.attributes["headingreset"] == ""
    end

    assert {:error, {:unsupported_html, diagnostic}} =
             HtmlParser.parse_detailed("<h1 headingoffset=1 HEADINGOFFSET=2>Heading</h1>")

    assert diagnostic.stage == :html and diagnostic.message =~ "attributes"
  end

  test "wbr is void and nobr preserves inline structure and authored global attributes" do
    assert {:ok, %{children: [%{tag: "p", children: [first, marker, bold, marker2, last]}]}} =
             HtmlParser.parse_detailed(
               "<p>ABC<WBR id='break' class='optional'><strong>DEF</strong><wbr/>GHI</p>"
             )

    assert first.text == "ABC" and last.text == "GHI" and bold.tag == "strong"
    assert marker.tag == "wbr" and marker.children == [] and marker2.children == []
    assert marker.attributes == %{"id" => "break", "class" => "optional"}

    assert {:ok, %{children: [%{tag: "nobr", children: [_, %{tag: "wbr"}, _]}]}} =
             HtmlParser.parse_detailed("<nobr>ABC<wbr>DEF</nobr>")

    for html <- [
          "<button>A<wbr><nobr>B</nobr></button>",
          "<p><nobr></nobr><wbr><wbr></p>",
          "<template><wbr><nobr>Stored</nobr></template>"
        ] do
      assert {:ok, _} = HtmlParser.parse_detailed(html)
    end

    for html <- [
          "<wbr onclick='run()'>",
          "<nobr onclick='run()'>Text</nobr>",
          "<ul><wbr></ul>",
          "<nobr><div>Invalid</div></nobr>"
        ] do
      assert {:error, {_, diagnostic}} = HtmlParser.parse_detailed(html)
      assert diagnostic.stage == :html and is_binary(diagnostic.message)
      assert is_binary(diagnostic.source)
    end
  end

  test "static highlight and modification elements retain metadata and transparent contexts" do
    assert {:ok, %{children: [%{tag: "ins", attributes: attrs, children: [%{tag: "div"}]}]}} =
             HtmlParser.parse_detailed(
               "<INS cite='https://example.com/change' datetime='2026-10-09T00:00Z'><div>Added</div></INS>"
             )

    assert attrs["cite"] == "https://example.com/change"
    assert attrs["datetime"] == "2026-10-09T00:00Z"

    assert {:ok, %{children: [%{attributes: %{"datetime" => "invalid"}}]}} =
             HtmlParser.parse_detailed("<del datetime=invalid>Removed</del>")

    for tag <- ["mark", "ins", "del", "strike"] do
      assert {:ok, _} =
               HtmlParser.parse_detailed(
                 "<p><" <> tag <> ">Text<strong>Nested</strong></" <> tag <> "></p>"
               )

      assert {:ok, _} = HtmlParser.parse_detailed("<" <> tag <> "></" <> tag <> ">")

      assert {:error, {:unsupported_html, %{stage: :html, source: source, message: message}}} =
               HtmlParser.parse_detailed("<" <> tag <> " onclick='run()'>Text</" <> tag <> ">")

      assert source =~ tag and is_binary(message)
    end

    for html <- [
          "<p><ins><div>Invalid</div></ins></p>",
          "<mark><div>Invalid</div></mark>",
          "<ul><del>Invalid</del></ul>"
        ] do
      assert {:error, {:unsupported_html, %{stage: :html, message: message}}} =
               HtmlParser.parse_detailed(html)

      assert is_binary(message)
    end
  end

  test "inert attributes retain authored values and enforce executable policy" do
    for value <- ["", "false", "anything"] do
      assert {:ok, %{children: [%{attributes: %{"inert" => ^value}}]}} =
               HtmlParser.parse_detailed("<x-box inert='" <> value <> "'>Visible</x-box>")
    end

    for html <- [
          "<div inert onclick='run()'>Text</div>",
          "<div inert><script>run()</script></div>"
        ] do
      assert {:error, {_, diagnostic}} = HtmlParser.parse_detailed(html)
      assert is_binary(diagnostic.message) and diagnostic.message != ""
      assert is_binary(diagnostic.source)
    end
  end

  test "initial hidden attributes retain authored values including until-found boxes" do
    for {tag, value} <- [{"div", "false"}, {"x-box", ""}, {"input", "HIDDEN"}] do
      html =
        "<" <>
          tag <>
          " hidden='" <> value <> "'>" <> if(tag == "input", do: "", else: "</" <> tag <> ">")

      assert {:ok, %{children: [%{attributes: attributes}]}} = HtmlParser.parse_detailed(html)
      assert attributes["hidden"] == value
    end

    for html <- [
          "<p hidden=until-found>Text</p>",
          "<x-box hidden=UNTIL-FOUND>Text</x-box>",
          "<template><p hidden=until-found>Unused</p></template>"
        ] do
      assert {:ok, _} = HtmlParser.parse_detailed(html)
    end
  end

  test "unregistered custom names preserve Unicode capitals punctuation and harmless attributes" do
    for name <- [
          "x-card",
          "x-É",
          "x-😀",
          ~S|x-"quote|,
          "x-'quote",
          "x-<angle",
          "x-=eq",
          "x-:colon",
          "x-@mark"
        ] do
      html =
        "<" <>
          name <>
          " disabled readonly multiple src='metadata' HREF='metadata' custom-note='Value' Δ='Unicode'>Text<span>Nested</span></" <>
          name <> ">"

      assert {:ok, %{children: [node]}} = HtmlParser.parse_detailed(html)
      assert node.tag == name
      assert node.attributes["custom-note"] == "Value"
      assert node.attributes["Δ"] == "Unicode"
      assert node.attributes["href"] == "metadata"
      assert node.attributes["readonly"] == ""
      assert [%{type: :text, text: "Text"}, %{tag: "span"}] = node.children
    end

    assert {:ok, %{children: [%{tag: "x-É"}]}} = HtmlParser.parse_detailed("<X-É></X-É>")

    assert {:ok,
            %{children: [%{tag: "input", attributes: %{"is" => "x-field", "readonly" => ""}}]}} =
             HtmlParser.parse_detailed("<input is=x-field readonly name=actual value=Initial>")

    assert {:ok, %{children: [%{tag: "script", attributes: %{"is" => "x-data"}}]}} =
             HtmlParser.parse_detailed("<script is=x-data type=application/json>{}</script>")

    assert {:error, {:unsupported_html, %{stage: :html}}} =
             HtmlParser.parse_detailed("<script is=x-data>run()</script>")
  end

  test "custom containers use transparent contexts and retain input policy" do
    assert {:ok, _} =
             HtmlParser.parse_detailed("<x-box><div>Block</div><x-inner>Text</x-inner></x-box>")

    assert {:ok, _} = HtmlParser.parse_detailed("<p><x-box>Text<span>Inline</span></x-box></p>")
    assert {:ok, _} = HtmlParser.parse_detailed("<button><x-box>Label</x-box></button>")

    for html <- [
          "<p><x-box><div>Invalid</div></x-box></p>",
          "<ul><x-box></x-box></ul>",
          "<head><x-box></x-box></head>",
          "<select><x-box></x-box></select>",
          "<figure><x-box><figcaption>Invalid</figcaption></x-box></figure>",
          "<x-box onclick='run()'>Text</x-box>",
          "<x-box dir=rtl>Direction</x-box>",
          "<template><x-box onclick='run()'>Text</x-box></template>",
          "<x-box><marquee>Excluded</marquee></x-box>"
        ] do
      assert {:error, {:unsupported_html, diagnostic}} = HtmlParser.parse_detailed(html)
      assert diagnostic.stage == :html
      assert diagnostic.reason == :unsupported_html
      assert is_binary(diagnostic.message)
    end

    for name <- ["x", "font-face", "annotation-xml", "missing-glyph"] do
      assert {:error, {:unsupported_html, _}} =
               HtmlParser.parse_detailed("<" <> name <> "></" <> name <> ">")
    end
  end

  test "custom fragment contexts preserve namespace integration and nonvoid content" do
    assert {:ok, %{children: [%{tag: "x-box", children: [%{text: "Kept"}]}]}} =
             HtmlParser.parse_detailed("<x-box/>Kept</x-box>")

    assert {:ok, %{children: [%{tag: "x-box", children: []}, %{tag: "p"}]}} =
             HtmlParser.parse_detailed("<x-box></x-box><p>Sibling</p>")

    assert {:ok, %{children: [%{tag: "div"}]}} =
             HtmlParser.parse_fragment_detailed("<div>Fallback flow</div>", "X-É")

    for context <- [
          %{namespace: :svg, tag: "foreignObject"},
          %{namespace: :mathml, tag: "annotation-xml", attributes: %{"encoding" => "text/html"}}
        ] do
      assert {:ok, %{children: [%{tag: "x-É", namespace: :html, children: [%{tag: "div"}]}]}} =
               HtmlParser.parse_fragment_detailed("<x-É><div>HTML</div></x-É>", context)
    end

    assert {:ok, %{children: [%{tag: "x-box", namespace: :svg}]}} =
             HtmlParser.parse_fragment_detailed("<x-box/>", "svg")

    assert {:ok, %{children: [%{tag: "x-box", namespace: :mathml}]}} =
             HtmlParser.parse_fragment_detailed("<x-box/>", "math")

    for html <- [
          "<template><x-box/><div>Kept</div></x-box></template>",
          "<button><x-box><input name=invalid></x-box></button>"
        ] do
      case html do
        "<template>" <> _ ->
          assert {:ok,
                  %{
                    children: [
                      %{
                        tag: "template",
                        content: %{children: [%{tag: "x-box", children: [%{tag: "div"}]}]}
                      }
                    ]
                  }} =
                   HtmlParser.parse_detailed(html)

        _ ->
          assert {:error, {:unsupported_html, %{stage: :html}}} = HtmlParser.parse_detailed(html)
      end
    end
  end

  test "scripting-disabled noscript parses active transparent markup and head styles" do
    html =
      "<html><head><noscript><style>p{color:red}</style><meta name=author content=Fallback></noscript></head><body><p>Before <noscript>A &amp; <b>B</b></noscript> after</p><noscript><div>Block</div></noscript><template><noscript>Unused</noscript></template></body></html>"

    assert {:ok, %{children: [%{children: [head, body]}]}} =
             HtmlParser.parse_document_detailed(html)

    assert [%{tag: "noscript", children: [%{tag: "style"}, %{tag: "meta"}]}] = head.children

    assert [
             %{
               tag: "p",
               children: [_, %{tag: "noscript", children: [%{text: "A & "}, %{tag: "b"}]}, _]
             },
             %{tag: "noscript", children: [%{tag: "div"}]},
             %{tag: "template", content: %{children: [%{tag: "noscript"}]}}
           ] = body.children

    assert {:ok, %{children: [%{tag: "b"}]}} =
             HtmlParser.parse_fragment_detailed("<b>Fallback</b>", "noscript")
  end

  test "noscript rejects nested and invalid transparent or head contents with locations" do
    for html <- [
          "<body><noscript><span><noscript>Nested</noscript></span></noscript></body>",
          "<head><noscript><title>Wrong</title></noscript></head>",
          "<head><noscript>Text</noscript></head>",
          "<p><noscript><div>Block</div></noscript></p>",
          "<table><noscript>Wrong</noscript></table>"
        ] do
      assert {:error, {:unsupported_html, diagnostic}} = HtmlParser.parse_document_detailed(html)
      assert diagnostic.stage == :html
      assert diagnostic.reason == :unsupported_html
      assert is_binary(diagnostic.source)
      assert diagnostic.line == 1
      assert diagnostic.column >= 1
    end

    assert {:error, {:unsupported_html, _}} =
             HtmlParser.parse_document_detailed("<noscript><script>run()</script></noscript>")
  end

  test "raw text boundaries use ASCII tag casing without rescanning validated UTF-8" do
    for {tag, text} <- [
          {"style", "before </ſtyle> after &amp;"},
          {"title", "before </tİtle> after &amp;"},
          {"textarea", "before </textareſ> after &amp;"}
        ] do
      assert {:ok, %{children: [%{tag: ^tag, children: [%{text: actual}]}]}} =
               HtmlParser.parse_detailed("<#{tag}>#{text}</#{String.upcase(tag)}>")

      assert actual == if(tag == "style", do: text, else: String.replace(text, "&amp;", "&"))
    end

    assert {:error, {:invalid_encoding, %{stage: :html, reason: :invalid_encoding}}} =
             HtmlParser.parse_detailed("<style>" <> <<255>> <> "</style>")
  end

  test "script-supporting elements retain actual specialized-parent children" do
    inert = "<script type='application/json'>&amp;<li></script><template><p>Unused</p></template>"

    for {tag, content, tags} <- [
          {"ul", inert <> "<li>Item</li>" <> inert, ~w(script template li script template)},
          {"ol", "<li>One</li>" <> inert <> "<li>Two</li>", ~w(li script template li)},
          {"dl", inert <> "<dt>Term</dt>" <> inert <> "<dd>Description</dd>" <> inert,
           ~w(script template dt script template dd script template)},
          {"dl", inert <> "<div><dt>Term</dt>" <> inert <> "<dd>Description</dd></div>" <> inert,
           ~w(script template div script template)},
          {"table",
           inert <>
             "<caption>Caption</caption>" <>
             inert <> "<tbody><tr><td>Cell</td></tr></tbody>" <> inert,
           ~w(script template caption script template tbody script template)},
          {"tbody", inert <> "<tr><td>Cell</td></tr>" <> inert,
           ~w(script template tr script template)},
          {"thead", inert <> "<tr><th>Heading</th></tr>" <> inert,
           ~w(script template tr script template)},
          {"tfoot", inert <> "<tr><td>Footer</td></tr>" <> inert,
           ~w(script template tr script template)},
          {"tr", inert <> "<td>Cell</td>" <> inert, ~w(script template td script template)},
          {"colgroup", "<col><template><p>Unused</p></template><col>", ~w(col template col)},
          {"select", inert <> "<option>Choice</option>" <> inert,
           ~w(script template option script template)},
          {"button", "<span>Label</span>" <> inert, ~w(span script template)}
        ] do
      assert {:ok, %{children: children}} = HtmlParser.parse_fragment(content, tag)
      assert Enum.map(children, & &1.tag) == tags

      for child <- children, child.tag == "template" do
        assert child.children == []
        assert %{type: :document_fragment, children: [_]} = child.content
      end
    end

    for html <- [
          "<dl><dt>Missing description</dt>#{inert}</dl>",
          "<dl>#{inert}<dd>Missing term</dd></dl>",
          "<dl><div><dt>A</dt><dd>B</dd></div>#{inert}<dt>C</dt><dd>D</dd></dl>",
          "<table><tbody><tr><td>A</td></tr></tbody>#{inert}<thead><tr><th>B</th></tr></thead></table>",
          "<table><caption>A</caption>#{inert}<caption>B</caption></table>",
          "<table><colgroup span=1><template></template></colgroup></table>"
        ] do
      assert {:error,
              {:unsupported_html, %{stage: :html, reason: :unsupported_html, message: message}}} =
               HtmlParser.parse_detailed(html)

      assert is_binary(message)
    end

    for tag <- ~w(ul ol dl table tbody thead tfoot tr colgroup select button) do
      assert {:error, {:unsupported_html, %{stage: :html, source: source, message: message}}} =
               HtmlParser.parse_fragment("<script type=module></script>", tag)

      assert source == "<script type=module>"
      assert message =~ "script"
    end
  end

  test "implicit table groups retain script-supporting insertion ancestry" do
    inert = "<script type='text/plain'>Data</script><template><tr><td>Unused</td></tr></template>"
    html = "<body><table><tr><td>One</td></tr>#{inert}<tr><td>Two</td></tr></table></body>"

    assert {:ok, %{children: [%{children: [_, %{children: [table]}]}]}} =
             HtmlParser.parse_document_detailed(html)

    assert [%{tag: "tbody", children: children}] = table.children
    assert Enum.map(children, & &1.tag) == ~w(tr script template tr)
    assert {:ok, %{children: [%{children: [%{children: raw}]}]}} = HtmlParser.parse_detailed(html)
    assert Enum.map(raw, & &1.tag) == ~w(tr script template tr)

    assert {:ok, %{children: [%{children: [_, %{children: [table]}]}]}} =
             HtmlParser.parse_document_detailed(
               "<body><table><col><template></template><col><script type='text/plain'>Data</script><tr><td>Cell</td></tr></table></body>"
             )

    assert [%{tag: "colgroup", children: columns}, %{tag: "script"}, %{tag: "tbody"}] =
             table.children

    assert Enum.map(columns, & &1.tag) == ~w(col template col)

    for {kind, child, contents} <- [
          {"tbody", "tr", "<td>Cell</td>"},
          {"colgroup", "col", ""}
        ] do
      template = "<template></template>"

      markup =
        "<body><table><#{child}>#{contents}" <>
          if(child == "tr", do: "</tr>", else: "") <> template <> "</#{kind}></table></body>"

      assert {:ok, %{children: [%{children: [_, %{children: [%{children: [group]}]}]}]}} =
               HtmlParser.parse_document_detailed(markup)

      assert group.tag == kind
      assert Enum.map(group.children, & &1.tag) == [child, "template"]
    end

    assert {:ok, %{children: [%{children: [_, %{children: [table]}]}]}} =
             HtmlParser.parse_document_detailed(
               "<body><table><colgroup><col><template></template><col></colgroup><tr><td>Cell</td></tr></table></body>"
             )

    assert Enum.map(hd(table.children).children, & &1.tag) == ~w(col template col)
  end

  test "ordinary template contents occupy separate document fragments" do
    html =
      "<template id='outer'><p>A &amp; B</p><template id=inner><input value=secret></template><script type='application/json'>&amp;<img></script></template><p>After</p>"

    assert {:ok, %{children: [template, after_node]}} = HtmlParser.parse_detailed(html)
    assert template.children == []
    assert template.attributes == %{"id" => "outer"}
    assert %{type: :document_fragment, children: [paragraph, nested, script]} = template.content
    assert tree_content(paragraph.children) == [%{type: :text, text: "A & B"}]
    assert nested.children == []
    assert %{type: :document_fragment, children: [%{tag: "input"}]} = nested.content
    assert tree_content(script.children) == [%{type: :text, text: "&amp;<img>"}]
    assert after_node.tag == "p"

    assert {:ok, %{children: [html_node]}} =
             HtmlParser.parse_document_detailed(
               "<template><p>Unused</p></template><p>Visible</p>"
             )

    assert [
             %{tag: "head", children: [%{tag: "template"}]},
             %{tag: "body", children: [%{tag: "p"}]}
           ] = html_node.children

    assert {:ok, %{children: [%{tag: "tr", children: [%{tag: "td"}]}]}} =
             HtmlParser.parse_fragment("<tr><td>Cell</td></tr>", "template")

    assert {:ok, %{children: [empty]}} = HtmlParser.parse("<template></template>")
    assert empty.children == []
    assert tree_content(empty.content) == %{type: :document_fragment, children: []}
  end

  test "template contents preserve foreign namespaces and diagnose active input" do
    assert {:ok, %{children: [template]}} =
             HtmlParser.parse_detailed(
               "<template><svg><path d='M0 0'/></svg><math><mi>x</mi></math></template>"
             )

    assert [%{namespace: :svg}, %{namespace: :mathml}] = template.content.children

    for html <- [
          "<template><script>run()</script></template>",
          "<template><script type=module></script></template>",
          "<template><svg><script/></svg></template>",
          "<template><svg><path onload='run()'/></svg></template>",
          "<template><span onclick='run()'>text</span></template>"
        ] do
      assert {:error, {:unsupported_html, %{stage: :html, reason: :unsupported_html, line: 1}}} =
               HtmlParser.parse_detailed(html)
    end

    for attribute <- ["for=target"] do
      assert {:error, {:unsupported_html, %{stage: :html, source: source, message: message}}} =
               HtmlParser.parse_detailed("<template #{attribute}></template>")

      assert source == "<template #{attribute}>"
      assert message =~ "composition"
    end

    for attribute <- [
          "shadowrootmode=open",
          "shadowrootdelegatesfocus",
          "shadowrootserializable",
          "shadowrootslotassignment=manual",
          "shadowrootclonable",
          "shadowrootcustomelementregistry"
        ] do
      assert {:ok, %{children: [%{tag: "template", content: %{children: []}}]}} =
               HtmlParser.parse_detailed("<template #{attribute}></template>")
    end
  end

  test "data scripts preserve literal contents, attributes and document placement" do
    text =
      ~s({"markup":"<img src='denied.png'><style>p{color:red}</style>","entity":"&amp;","broken":)

    assert {:ok, %{children: [script, paragraph]}} =
             HtmlParser.parse_detailed(
               "<script TYPE='application/json' src='https://invalid.test/data' data-kind=demo>#{text}</script><p>After</p>"
             )

    assert script.tag == "script"

    assert script.attributes == %{
             "type" => "application/json",
             "src" => "https://invalid.test/data",
             "data-kind" => "demo"
           }

    assert tree_content(script.children) == [%{type: :text, text: text}]
    assert tree_content(paragraph.children) == [%{type: :text, text: "After"}]

    assert {:ok, %{children: [html]}} =
             HtmlParser.parse_document_detailed(
               "<script type='application/json'>{}</script><p>Body</p>"
             )

    assert [%{tag: "head", children: [%{tag: "script"}]}, %{tag: "body", children: [%{tag: "p"}]}] =
             html.children

    assert {:ok, %{children: [%{type: :text, text: "<script>&amp;</script>"}]}} =
             HtmlParser.parse_fragment("<script>&amp;</script>", "SCRIPT")

    assert {:ok, %{children: [empty]}} = HtmlParser.parse("<script type='text/plain'></script>")
    assert empty.children == []
  end

  test "script escaped and double-escaped boundaries preserve the actual closing tag" do
    for text <- [
          "literal &amp; <img>",
          "<!--escaped",
          "<!--<script>inner</script>tail-->",
          "<!--<ScRiPt >inner</SCRIPT >tail",
          "<!--<script>inner-->tail",
          "<!--><script>tail",
          "<!---><script>tail",
          "<!--<scriptx>tail",
          "</scriptx><scripture>",
          "</ſcript>literal",
          "<!--<script/inner</script>tail-->"
        ] do
      assert {:ok, %{children: [script, after_node]}} =
               HtmlParser.parse_detailed(
                 "<script type='text/plain'>#{text}</ScRiPt \t\n><p>After</p>"
               )

      assert tree_content(script.children) == [%{type: :text, text: text}]
      assert after_node.tag == "p"
    end

    for html <- [
          "<script type='text/plain'>unfinished",
          "<script type='text/plain'><!--<script>inner</script>",
          "<script type='text/plain'>text</scripture>",
          "<script type='text/plain'>text</script foo>",
          "<script type='text/plain'>text</script",
          "<script type='text/plain'>text</script "
        ] do
      assert {:error, {:unsupported_html, %{stage: :html}}} = HtmlParser.parse_detailed(html)
    end
  end

  test "executable script kinds fail with locations even when visually hidden" do
    for attributes <- [
          "",
          "type=''",
          "type='TEXT/JAVASCRIPT'",
          "language=javascript",
          "type=MODULE",
          "type=importmap",
          "type=speculationrules"
        ] do
      source = "<script #{attributes} style='display:none'>"

      assert {:error, {:unsupported_html, diagnostic}} =
               HtmlParser.parse_detailed("<p>Before</p>\n  #{source}example</script>")

      assert diagnostic.stage == :html
      assert diagnostic.reason == :unsupported_html
      assert diagnostic.module == NativeElixirPdfUtilities.Validators.HtmlValidator
      assert diagnostic.line == 2
      assert diagnostic.column == 3
      assert diagnostic.source == source
      assert diagnostic.message =~ "only non-executable data blocks"
    end

    for html <- [
          "<script type='text/plain' onclick='example'></script>",
          "<svg><script type='application/json'/></svg>",
          "<math><script type='application/json'/></math>"
        ] do
      assert {:error, {:unsupported_html, %{stage: :html}}} = HtmlParser.parse_detailed(html)
    end
  end

  describe "HTML, SVG, and MathML namespaces" do
    test "foreign elements and fragment contexts consume shared source and depth budgets" do
      alias NativeElixirPdfUtilities.Limits
      limits = Limits.effective()
      on_exit(fn -> Limits.install(limits) end)
      Limits.install(%{limits | max_html_depth: 2})

      assert {:error, {:resource_limit_exceeded, %{stage: :html, message: message}}} =
               HtmlParser.parse_detailed("<math><mrow><mi>x</mi></mrow></math>")

      assert message =~ "max_html_depth"
      Limits.install(%{limits | max_aggregate_html_source_bytes: 18})

      assert {:error, {:resource_limit_exceeded, %{stage: :html}}} =
               HtmlParser.parse_fragment("<span>text</span>", %{
                 namespace: :mathml,
                 tag: "annotation-xml",
                 attributes: %{"encoding" => "text/html"}
               })
    end

    test "keeps nested foreign elements, self-closing flags, and adjusted SVG names" do
      assert {:ok, %{children: [svg, math]}} =
               HtmlParser.parse_detailed(
                 "<svg viewBox='0 0 10 20'><lineargradient gradientUnits='userSpaceOnUse'/><clipPath><path d='M0 0'/></clipPath></svg><math><mrow><mi>x</mi><mspace width='1em'/></mrow></math>"
               )

      assert svg.namespace == :svg
      assert svg.attributes == %{"viewBox" => "0 0 10 20"}

      assert [
               %{
                 tag: "linearGradient",
                 namespace: :svg,
                 attributes: %{"gradientUnits" => "userSpaceOnUse"}
               },
               %{tag: "clipPath", children: [%{tag: "path", namespace: :svg}]}
             ] = svg.children

      assert math.namespace == :mathml

      assert [
               %{
                 tag: "mrow",
                 namespace: :mathml,
                 children: [%{tag: "mi"}, %{tag: "mspace", children: []}]
               }
             ] = math.children
    end

    test "retains MathML adjustments and foreign attribute namespace identities" do
      assert {:ok, %{children: [math, svg]}} =
               HtmlParser.parse_detailed(
                 "<math definitionURL='local-definition' xmlns='http://www.w3.org/1998/Math/MathML'><mi xml:lang='en'>x</mi></math><svg xmlns:xlink='http://www.w3.org/1999/xlink'><use xlink:href='#shape' xml:space='preserve'/></svg>"
               )

      assert math.attributes["definitionURL"] == "local-definition"

      assert math.attribute_namespaces["xmlns"] == %{
               prefix: nil,
               local_name: "xmlns",
               namespace: "http://www.w3.org/2000/xmlns/"
             }

      assert svg.attribute_namespaces["xmlns:xlink"].prefix == "xmlns"
      assert [use] = svg.children

      assert use.attribute_namespaces["xlink:href"] == %{
               prefix: "xlink",
               local_name: "href",
               namespace: "http://www.w3.org/1999/xlink"
             }

      assert use.attribute_namespaces["xml:space"].namespace ==
               "http://www.w3.org/XML/1998/namespace"
    end

    test "uses HTML integration points without treating SVG title as RCDATA" do
      assert {:ok, %{children: [svg]}} =
               HtmlParser.parse_detailed(
                 "<svg><title>A &amp; <b>bold</b></title><desc><p>Description</p></desc><foreignObject><div><svg><path/></svg></div></foreignObject></svg>"
               )

      assert [title, desc, foreign] = svg.children

      assert tree_content(title.children) == [
               %{type: :text, text: "A & "},
               %{
                 type: :element,
                 tag: "b",
                 attributes: %{},
                 children: [%{type: :text, text: "bold"}],
                 namespace: :html,
                 attribute_namespaces: %{}
               }
             ]

      assert [%{tag: "p", namespace: :html}] = desc.children

      assert [%{tag: "div", namespace: :html, children: [%{tag: "svg", namespace: :svg}]}] =
               foreign.children
    end

    test "uses MathML text and annotation integration points with their exceptions" do
      for token <- ~w(mi mo mn ms mtext) do
        assert {:ok, %{children: [%{children: [point]}]}} =
                 HtmlParser.parse_detailed(
                   "<math><#{token}><span>HTML</span><mglyph/><malignmark/></#{token}></math>"
                 )

        assert [
                 %{tag: "span", namespace: :html},
                 %{tag: "mglyph", namespace: :mathml},
                 %{tag: "malignmark", namespace: :mathml}
               ] = point.children
      end

      for encoding <- ["text/html", "APPLICATION/XHTML+XML"] do
        assert {:ok, %{children: [%{children: [%{children: [%{tag: "p", namespace: :html}]}]}]}} =
                 HtmlParser.parse_detailed(
                   "<math><annotation-xml encoding='#{encoding}'><p>HTML</annotation-xml></math>"
                 )
      end

      assert {:ok, %{children: [%{children: [annotation]}]}} =
               HtmlParser.parse_detailed(
                 "<math><annotation-xml encoding='application/custom'><svg><path/></svg><unknown-math/></annotation-xml></math>"
               )

      assert [%{tag: "svg", namespace: :svg}, %{tag: "unknown-math", namespace: :mathml}] =
               annotation.children
    end

    test "CDATA stays literal only in foreign content and raw text remains context dependent" do
      assert {:ok, %{children: [svg, math]}} =
               HtmlParser.parse_detailed(
                 "<svg><![CDATA[<path>&amp;]]></svg><math><mtext>&amp;</mtext></math>"
               )

      assert tree_content(svg.children) == [%{type: :text, text: "<path>&amp;"}]
      assert [%{children: [%{text: "&"}]}] = math.children

      for source <- ["<![CDATA[text]]>", "<svg><![CDATA[missing</svg>", "<svg><path></svg>"] do
        assert {:error, {:unsupported_html, %{stage: :html, line: 1}}} =
                 HtmlParser.parse_detailed(source)
      end
    end

    test "foreign fragments use the supplied namespace and integration attributes" do
      assert {:ok, %{children: [%{tag: "mfrac", namespace: :mathml}]}} =
               HtmlParser.parse_fragment("<mfrac><mi>x</mi><mn>2</mn></mfrac>", "math")

      assert {:ok, %{children: [%{tag: "linearGradient", namespace: :svg}]}} =
               HtmlParser.parse_fragment("<linearGradient/>", "SVG")

      assert {:ok, %{children: [%{tag: "span", namespace: :html}]}} =
               HtmlParser.parse_fragment("<span>HTML</span>", %{
                 namespace: :mathml,
                 tag: "annotation-xml",
                 attributes: %{"encoding" => "TEXT/HTML"}
               })

      assert {:ok, %{children: [%{tag: "p", namespace: :html}]}} =
               HtmlParser.parse_fragment("<p>HTML</p>", %{namespace: :svg, tag: "foreignObject"})

      for context <- [
            nil,
            %{namespace: :other, tag: "svg"},
            %{namespace: :svg, tag: "invalid tag"},
            %{namespace: :mathml, tag: "mtext", attributes: %{bad: 1}},
            %{namespace: :svg, tag: "g", attributes: %{"onclick" => "run()"}}
          ] do
        assert {:error, {:unsupported_html, %{stage: :html}}} =
                 HtmlParser.parse_fragment("text", context)
      end
    end

    test "policy and namespace failures keep source locations" do
      for content <- [
            "<svg onload='run()'/>",
            "<svg><script/></svg>",
            "<math><script/></math>",
            "<svg><p>breakout</p></svg>",
            "<svg><font color='red'/></svg>",
            "<svg xmlns='wrong'/>",
            "<math xmlns:xlink='wrong'/>",
            "<svg viewBox='a' VIEWBOX='b'/>"
          ] do
        assert {:error, {:unsupported_html, diagnostic}} =
                 HtmlParser.parse_detailed("<div>\n#{content}</div>")

        assert diagnostic.stage == :html
        assert diagnostic.line == 2
        {offset, _length} = :binary.match(content, diagnostic.source)
        assert diagnostic.column == String.length(binary_part(content, 0, offset)) + 1
        assert is_binary(diagnostic.source)
        assert is_binary(diagnostic.message)
      end
    end
  end

  test "preformatted initial newline removal respects intervening comments" do
    for tag <- ["pre", "listing"] do
      assert {:ok, %{children: [%{children: [%{text: "\nfirst"}]}]}} =
               HtmlParser.parse("<#{tag}><!-- boundary -->\nfirst</#{tag}>")

      assert {:ok, %{children: [%{children: [%{text: "first"}]}]}} =
               HtmlParser.parse("<#{tag}>&NewLine;first</#{tag}>")
    end
  end

  test "description groups and figure captions follow their structure" do
    for html <- [
          "<dl></dl>",
          "<dl><dt>A</dt><dt>B</dt><dd>C</dd><dd>D</dd></dl>",
          "<figure><figcaption>First</figcaption>Body</figure>",
          "<figure>Body<figcaption>Last</figcaption> </figure>"
        ] do
      assert {:ok, _} = HtmlParser.parse(html)
    end

    for html <- [
          "<dl><dd>No term</dd></dl>",
          "<dl><dt>No description</dt></dl>",
          "<dl><div><p>Invalid group</p></div></dl>",
          "<dl><div><dt>A</dt><dd>B</dd></div><dt>C</dt><dd>D</dd></dl>",
          "<figure>A<figcaption>Middle</figcaption>B</figure>",
          "<figure><figcaption>One</figcaption><figcaption>Two</figcaption></figure>",
          "<li>Outside</li>"
        ] do
      assert {:error, :unsupported_html} = HtmlParser.parse(html)
    end
  end

  test "static element content models retain block terms and reject invalid descendants" do
    assert {:ok, _} =
             HtmlParser.parse("<dl><div><dt><p>A term</p></dt><dd>A description</dd></div></dl>")

    for html <- [
          "<dt>Outside</dt>",
          "<dd>Outside</dd>",
          "<figcaption>Outside</figcaption>",
          "<dl><dt><div><h2>Invalid heading</h2></div></dt><dd>D</dd></dl>",
          "<a href='https://example.com'><span><a href='https://example.com'>Nested</a></span></a>"
        ] do
      assert {:error, :unsupported_html} = HtmlParser.parse(html)
    end

    assert NativeElixirPdfUtilities.Validators.HtmlValidator.valid_html_child?("unknown", "p") ==
             false
  end

  test "description lists accept omitted item ends and reject structural text" do
    explicit = "<dl><dt>Term</dt><dd>Meaning</dd><dt>Next</dt><dd><p>Detail</p></dd></dl>"
    omitted = "<dl><dt>Term<dd>Meaning<dt>Next<dd><p>Detail</dl>"
    assert tree_content(HtmlParser.parse(explicit)) == tree_content(HtmlParser.parse(omitted))
    assert {:error, :unsupported_html} = HtmlParser.parse("<dl>outside<dt>Term</dt></dl>")
    assert {:error, :unsupported_html} = HtmlParser.parse("<dl><dt>Term</dl>")

    assert {:ok, %{children: [%{children: [%{text: "  first\nnext"}]}]}} =
             HtmlParser.parse("<pre>\r\n  first\nnext</pre>")

    assert {:ok, %{children: [%{children: [%{text: "first"}]}]}} =
             HtmlParser.parse("<listing>\rfirst</listing>")
  end

  test "parse accepts a strict paragraph document" do
    assert tree_content(HtmlParser.parse("<p>Hello &amp; PDF</p>")) ==
             {:ok,
              %{
                type: :document,
                children: [
                  %{
                    type: :element,
                    tag: "p",
                    attributes: %{},
                    children: [%{type: :text, text: "Hello & PDF"}]
                  }
                ]
              }}
  end

  test "parse accepts headings and inline text elements with style attributes" do
    assert tree_content(
             HtmlParser.parse(
               ~s(<h1 style="color: #336699">Title</h1><p>Hello <strong>bold</strong> and <em>italic</em></p>)
             )
           ) ==
             {:ok,
              %{
                type: :document,
                children: [
                  %{
                    type: :element,
                    tag: "h1",
                    attributes: %{"style" => "color: #336699"},
                    children: [%{type: :text, text: "Title"}]
                  },
                  %{
                    type: :element,
                    tag: "p",
                    attributes: %{},
                    children: [
                      %{type: :text, text: "Hello "},
                      %{
                        type: :element,
                        tag: "strong",
                        attributes: %{},
                        children: [%{type: :text, text: "bold"}]
                      },
                      %{type: :text, text: " and "},
                      %{
                        type: :element,
                        tag: "em",
                        attributes: %{},
                        children: [%{type: :text, text: "italic"}]
                      }
                    ]
                  }
                ]
              }}
  end

  test "parse accepts style tags and CSS targeting attributes" do
    assert tree_content(
             HtmlParser.parse(
               ~s(<style>p.copy { color: red; }</style><p id="intro" class="copy lead">Hello</p>)
             )
           ) ==
             {:ok,
              %{
                type: :document,
                children: [
                  %{
                    type: :element,
                    tag: "style",
                    attributes: %{},
                    children: [%{type: :text, text: "p.copy { color: red; }"}]
                  },
                  %{
                    type: :element,
                    tag: "p",
                    attributes: %{"id" => "intro", "class" => "copy lead"},
                    children: [%{type: :text, text: "Hello"}]
                  }
                ]
              }}
  end

  test "parse keeps quoted tag delimiters inside attribute values" do
    assert {:ok, %{children: [paragraph]}} =
             HtmlParser.parse(~s(<p title="1 > 0" data-check='2 > 1'>Valid</p>))

    assert paragraph.attributes == %{"data-check" => "2 > 1", "title" => "1 > 0"}
    assert tree_content(paragraph.children) == [%{type: :text, text: "Valid"}]
  end

  test "parse preserves style raw text and decodes RCDATA entities" do
    style_text = ~s(p::before { content: "< &lt;"; })

    html =
      "<style>#{style_text}</style>" <>
        "<title>1 < 2 &amp; 3</title>" <>
        "<div><textarea>4 < 5 &amp; 6</textarea></div>"

    assert {:ok, %{children: [style, title, %{children: [textarea]}]}} = HtmlParser.parse(html)
    assert tree_content(style.children) == [%{type: :text, text: style_text}]
    assert tree_content(title.children) == [%{type: :text, text: "1 < 2 & 3"}]
    assert tree_content(textarea.children) == [%{type: :text, text: "4 < 5 & 6"}]

    assert {:ok, %{children: [%{children: [raw_text]}]}} =
             HtmlParser.parse("<style><span>No element</span></style>")

    assert tree_content(raw_text) == %{type: :text, text: "<span>No element</span>"}
  end

  test "parse accepts generated-content and attribute-selector metadata" do
    assert {:ok, dom} =
             HtmlParser.parse(
               ~s(<p data-label="Status" aria-label="Current status" role="note" title="Ready">Complete</p>)
             )

    [paragraph] = dom.children

    assert paragraph.attributes == %{
             "aria-label" => "Current status",
             "data-label" => "Status",
             "role" => "note",
             "title" => "Ready"
           }
  end

  test "parse accepts structural html head and body tags" do
    assert {:ok, dom} =
             HtmlParser.parse(
               ~s(<html><head><style>p { color: blue; }</style></head><body><p>Hello</p></body></html>)
             )

    [html] = dom.children
    [head, body] = html.children
    [style] = head.children
    [paragraph] = body.children

    assert html.tag == "html"
    assert style.tag == "style"
    assert paragraph.tag == "p"
  end

  test "parse accepts semantic block containers" do
    assert {:ok, dom} =
             HtmlParser.parse(
               ~s(<main><header><p>Head</p></header><nav><p>Nav</p></nav><aside><p>Side</p></aside><footer><p>Foot</p></footer></main>)
             )

    [main] = dom.children
    assert main.tag == "main"
    assert Enum.map(main.children, & &1.tag) == ["header", "nav", "aside", "footer"]
  end

  test "parse accepts document metadata void tags and line breaks" do
    assert {:ok, dom} =
             HtmlParser.parse(
               ~s(<!DOCTYPE html><html lang="en"><meta charset="utf-8" /><title>PO</title><body><p>One<br />Two<br>Three</p><img src="logo.png" /></body></html>)
             )

    [html] = dom.children
    [meta, title, body] = html.children
    [paragraph, image] = body.children

    assert html.attributes == %{"lang" => "en"}
    assert meta.tag == "meta"
    assert meta.attributes == %{"charset" => "utf-8"}
    assert tree_content(title.children) == [%{type: :text, text: "PO"}]

    assert Enum.map(paragraph.children, &Map.get(&1, :tag, :text)) == [
             :text,
             "br",
             :text,
             "br",
             :text
           ]

    assert image.children == []
  end

  test "parse accepts static form controls and boolean attributes" do
    html = """
    <div>
      <input type="text" value="Alice &amp; Bob">
      <input type="checkbox" checked disabled="disabled">
      <input type="radio" checked="checked">
      <input type=HIDDEN name=reference value="Alice &amp; Bob" disabled>
      <input type=SEARCH name=query value="A&#10;B" disabled>
      <select name="status">
        <option>Pending</option>
        <option value="approved" selected>Approved</option>
      </select>
      <textarea name="notes" value="Fallback"></textarea>
      <button type="button"><strong>Save</strong></button>
    </div>
    """

    assert {:ok, %{children: [%{children: controls}]}} = HtmlParser.parse(html)

    [text, checkbox, radio, hidden, search, select, textarea, button] =
      Enum.reject(controls, &match?(%{type: :text}, &1))

    assert text.attributes == %{"type" => "text", "value" => "Alice & Bob"}

    assert checkbox.attributes == %{
             "checked" => "",
             "disabled" => "disabled",
             "type" => "checkbox"
           }

    assert radio.attributes == %{"checked" => "checked", "type" => "radio"}

    assert hidden.attributes == %{
             "type" => "HIDDEN",
             "name" => "reference",
             "value" => "Alice & Bob",
             "disabled" => ""
           }

    assert hidden.children == []

    assert search.attributes == %{
             "type" => "SEARCH",
             "name" => "query",
             "value" => "A\nB",
             "disabled" => ""
           }

    assert Enum.map(select.children, & &1.tag) == ["option", "option"]
    assert Enum.at(select.children, 1).attributes == %{"selected" => "", "value" => "approved"}
    assert textarea.attributes == %{"name" => "notes", "value" => "Fallback"}
    assert [%{tag: "strong"}] = button.children
  end

  test "selects accept empty options and multiple authored selections without mutating attributes" do
    for html <- [
          "<select></select>",
          "<select><option></option></select>",
          "<select><script type='text/plain'>Data</script><template><option selected>Unused</option></template></select>",
          "<select><option selected>Alpha</option><option selected>Beta</option></select>"
        ] do
      assert {:ok, %{children: [%{tag: "select"}]}} = HtmlParser.parse_detailed(html)
    end
  end

  test "optgroups support omitted option and group endings and diagnose unsupported contents" do
    assert {:ok, %{children: [select]}} =
             HtmlParser.parse_detailed("""
             <select><optgroup label='One'><option label='Alpha' value='a'><option>B
             <optgroup label='Two' disabled><option>C</select>
             """)

    assert Enum.map(select.children, & &1.tag) == ["optgroup", "optgroup"]
    assert Enum.map(hd(select.children).children, & &1.tag) == ["option", "option"]
    assert hd(hd(select.children).children).attributes["label"] == "Alpha"

    assert {:ok, %{children: [separated]}} =
             HtmlParser.parse_detailed(
               "<select><optgroup label='Group'><option>A<hr><option selected>B</select>"
             )

    assert Enum.map(separated.children, & &1.tag) == ["optgroup", "hr", "option"]
    [group, separator, selected] = separated.children
    assert Enum.map(group.children, & &1.tag) == ["option"]
    assert separator.children == []
    assert selected.attributes["selected"] == ""

    assert tree_content(
             Enum.map(
               NativeElixirPdfUtilities.Validators.HtmlValidator.select_options(
                 separated.children
               ),
               & &1.children
             )
           ) == [
             [%{type: :text, text: "A"}],
             [%{type: :text, text: "B"}]
           ]

    for html <- [
          "<select><optgroup><option>A</option></optgroup></select>",
          "<select><optgroup label='Group'>Unexpected</optgroup></select>",
          "<select><optgroup label='Group'><span>Unsupported</span></optgroup></select>"
        ] do
      assert {:error, {reason, diagnostic}} = HtmlParser.parse_detailed(html)
      assert diagnostic.stage == :html and diagnostic.reason == reason
      assert is_binary(diagnostic.message) and diagnostic.message != ""
      assert is_integer(diagnostic.line) and is_integer(diagnostic.column)
    end
  end

  test "parse rejects invalid static form structures and attributes" do
    invalid_html = [
      ~s(<div><input type="checkbox">child</input></div>),
      ~s(<div><select><span>Wrong</span></select></div>),
      ~s(<div><button><input type="text"></button></div>),
      ~s(<div><button><strong><input type="text"></strong></button></div>)
    ]

    Enum.each(invalid_html, fn html ->
      assert tree_content(HtmlParser.parse(html)) == {:error, :unsupported_html}
    end)
  end

  test "parse ignores structural whitespace and decodes HTML character references once" do
    assert {:ok, dom} =
             HtmlParser.parse("""

             <html>
               <head>
                 <style>p::before { content: &quot;x&quot;; }</style>
               </head>
               <body>
                 <ul>
                  <li>&lt;One&gt; &apos;Two&apos; &#39;Three&#39;&nbsp;Four &copy; &#169; &#x2122; &NotEqualTilde; &amp;lt;</li>
                 </ul>
                 <table>
                   <tr><td>A</td></tr>
                 </table>
               </body>
             </html>
             """)

    [html] = dom.children
    [_head, body] = html.children
    [list, table] = body.children
    [item] = list.children
    [text] = item.children
    [row] = table.children

    assert text.text == "<One> 'Two' 'Three'\u00A0Four © © ™ ≂̸ &lt;"
    assert row.tag == "tr"
  end

  test "parse applies legacy named-reference rules in text and attributes" do
    assert {:ok, dom} =
             HtmlParser.parse(~s(<p class="&copy; &copyy &amp=1">&copy &copyy &unknown;</p>))

    [paragraph] = dom.children
    [text] = paragraph.children

    assert paragraph.attributes == %{"class" => "© &copyy &amp=1"}
    assert text.text == "© ©y &unknown;"
  end

  test "parse normalizes invalid and legacy numeric character references" do
    assert {:ok, dom} =
             HtmlParser.parse("<p>&#x80; &#0; &#xD800; &#x110000; &#x; &#;</p>")

    [paragraph] = dom.children
    [text] = paragraph.children

    assert text.text == "€ � � � &#x; &#;"
  end

  test "parse accepts div containers for flex layouts" do
    assert {:ok, dom} =
             HtmlParser.parse(~s(<div class="row"><div>A</div><span>B</span></div>))

    [container] = dom.children
    [first, second] = container.children

    assert container.tag == "div"
    assert container.attributes == %{"class" => "row"}
    assert first.tag == "div"
    assert second.tag == "span"
  end

  test "parse accepts semantic section and article block containers" do
    assert {:ok, dom} =
             HtmlParser.parse(~s(<section class="sheet"><article><p>Trim</p></article></section>))

    [section] = dom.children
    [article] = section.children
    [paragraph] = article.children

    assert section.tag == "section"
    assert article.tag == "article"
    assert paragraph.tag == "p"
  end

  test "parse accepts strict image source attributes" do
    assert tree_content(
             HtmlParser.parse(
               ~s(<img src="photo.png" alt="Product photo"><div><img src='nested.jpg'></div>)
             )
           ) ==
             {:ok,
              %{
                type: :document,
                children: [
                  %{
                    type: :element,
                    tag: "img",
                    attributes: %{"alt" => "Product photo", "src" => "photo.png"},
                    children: []
                  },
                  %{
                    type: :element,
                    tag: "div",
                    attributes: %{},
                    children: [
                      %{
                        type: :element,
                        tag: "img",
                        attributes: %{"src" => "nested.jpg"},
                        children: []
                      }
                    ]
                  }
                ]
              }}
  end

  test "parse accepts strict lists and link href attributes" do
    assert tree_content(
             HtmlParser.parse(
               ~s(<ul><li>Read <a href="https://example.com">docs</a></li><li>Ship</li></ul><ol><li>First</li></ol>)
             )
           ) ==
             {:ok,
              %{
                type: :document,
                children: [
                  %{
                    type: :element,
                    tag: "ul",
                    attributes: %{},
                    children: [
                      %{
                        type: :element,
                        tag: "li",
                        attributes: %{},
                        children: [
                          %{type: :text, text: "Read "},
                          %{
                            type: :element,
                            tag: "a",
                            attributes: %{"href" => "https://example.com"},
                            children: [%{type: :text, text: "docs"}]
                          }
                        ]
                      },
                      %{
                        type: :element,
                        tag: "li",
                        attributes: %{},
                        children: [%{type: :text, text: "Ship"}]
                      }
                    ]
                  },
                  %{
                    type: :element,
                    tag: "ol",
                    attributes: %{},
                    children: [
                      %{
                        type: :element,
                        tag: "li",
                        attributes: %{},
                        children: [%{type: :text, text: "First"}]
                      }
                    ]
                  }
                ]
              }}
  end

  test "parse accepts strict table markup" do
    assert tree_content(
             HtmlParser.parse(
               ~s(<table><caption>Totals</caption><thead><tr><th>Name</th><th>Count</th></tr></thead><tbody><tr><td>Alpha</td><td><a href="https://example.com">2</a></td></tr></tbody><tfoot><tr><td>Total</td><td>2</td></tr></tfoot></table>)
             )
           ) ==
             {:ok,
              %{
                type: :document,
                children: [
                  %{
                    type: :element,
                    tag: "table",
                    attributes: %{},
                    children: [
                      %{
                        type: :element,
                        tag: "caption",
                        attributes: %{},
                        children: [%{type: :text, text: "Totals"}]
                      },
                      %{
                        type: :element,
                        tag: "thead",
                        attributes: %{},
                        children: [
                          %{
                            type: :element,
                            tag: "tr",
                            attributes: %{},
                            children: [
                              %{
                                type: :element,
                                tag: "th",
                                attributes: %{},
                                children: [%{type: :text, text: "Name"}]
                              },
                              %{
                                type: :element,
                                tag: "th",
                                attributes: %{},
                                children: [%{type: :text, text: "Count"}]
                              }
                            ]
                          }
                        ]
                      },
                      %{
                        type: :element,
                        tag: "tbody",
                        attributes: %{},
                        children: [
                          %{
                            type: :element,
                            tag: "tr",
                            attributes: %{},
                            children: [
                              %{
                                type: :element,
                                tag: "td",
                                attributes: %{},
                                children: [%{type: :text, text: "Alpha"}]
                              },
                              %{
                                type: :element,
                                tag: "td",
                                attributes: %{},
                                children: [
                                  %{
                                    type: :element,
                                    tag: "a",
                                    attributes: %{"href" => "https://example.com"},
                                    children: [%{type: :text, text: "2"}]
                                  }
                                ]
                              }
                            ]
                          }
                        ]
                      },
                      %{
                        type: :element,
                        tag: "tfoot",
                        attributes: %{},
                        children: [
                          %{
                            type: :element,
                            tag: "tr",
                            attributes: %{},
                            children: [
                              %{
                                type: :element,
                                tag: "td",
                                attributes: %{},
                                children: [%{type: :text, text: "Total"}]
                              },
                              %{
                                type: :element,
                                tag: "td",
                                attributes: %{},
                                children: [%{type: :text, text: "2"}]
                              }
                            ]
                          }
                        ]
                      }
                    ]
                  }
                ]
              }}
  end

  test "parse accepts cell spans and block table children inside cells" do
    assert {:ok, dom} =
             HtmlParser.parse(
               ~s(<table><tr><td colspan="2" rowspan="3"><p>Address</p><table><tr><td>Nested</td></tr></table></td></tr></table>)
             )

    [table] = dom.children
    [row] = table.children
    [cell] = row.children
    [paragraph, nested_table] = cell.children

    assert cell.attributes == %{"colspan" => "2", "rowspan" => "3"}
    assert paragraph.tag == "p"
    assert nested_table.tag == "table"
  end

  test "parse accepts a table without a caption" do
    assert {:ok, dom} =
             HtmlParser.parse(
               ~s(<table><thead><tr><th>Name</th></tr></thead><tbody><tr><td>Alpha</td></tr></tbody></table>)
             )

    [table] = dom.children
    [head, body] = table.children

    assert head.tag == "thead"
    assert body.tag == "tbody"
  end

  test "parse preserves invalid scope for Auto recovery only on table headers" do
    assert {:ok, dom} =
             HtmlParser.parse("""
             <table>
               <tr>
                 <th scope="col">Column</th>
                 <th scope="row">Row</th>
                 <th scope="rowgroup">Row group</th>
                 <th scope="colgroup">Column group</th>
               </tr>
             </table>
             """)

    [table] = dom.children
    [row] = table.children

    assert Enum.map(row.children, & &1.attributes["scope"]) ==
             ~w(col row rowgroup colgroup)

    assert {:ok, invalid_scope} =
             HtmlParser.parse("<table><tr><th scope=\"column\">Auto</th></tr></table>")

    assert hd(hd(hd(invalid_scope.children).children).children).attributes["scope"] == "column"

    assert tree_content(HtmlParser.parse("<table><tr><td scope=\"col\">Bad</td></tr></table>")) ==
             {:error, :unsupported_html}
  end

  test "parse accepts column groups, column spans, and strict table ordering" do
    assert {:ok, dom} =
             HtmlParser.parse("""
             <table>
               <caption>Inventory</caption>
               <colgroup><col span="2" /><col /></colgroup>
               <colgroup span="2"></colgroup>
               <tr><td>A</td><td>B</td><td>C</td><td>D</td><td>E</td></tr>
             </table>
             """)

    [table] = dom.children
    [caption, first_group, second_group, row] = table.children
    [spanned_column, column] = first_group.children

    assert caption.tag == "caption"
    assert spanned_column.attributes == %{"span" => "2"}
    assert column.attributes == %{}
    assert second_group.attributes == %{"span" => "2"}
    assert second_group.children == []
    assert row.tag == "tr"

    invalid_tables = [
      "<table><tr><td>A</td></tr><colgroup><col /></colgroup></table>",
      "<table><colgroup span=\"2\"><col /></colgroup><tr><td>A</td></tr></table>"
    ]

    Enum.each(invalid_tables, fn html ->
      assert tree_content(HtmlParser.parse(html)) == {:error, :unsupported_html}
    end)
  end

  test "parse rejects unsupported markup" do
    assert tree_content(HtmlParser.parse("")) == {:ok, %{type: :document, children: []}}
    assert tree_content(HtmlParser.parse("<>")) == {:error, :unsupported_html}
    assert {:ok, _dom} = HtmlParser.parse("<p>Hello")
    assert tree_content(HtmlParser.parse("<style>")) == {:error, :unsupported_html}

    assert tree_content(HtmlParser.parse("<style>missing closing tag")) ==
             {:error, :unsupported_html}

    assert tree_content(HtmlParser.parse("<script></script>")) == {:error, :unsupported_html}
    assert tree_content(HtmlParser.parse("<p>Hello</script></p>")) == {:error, :unsupported_html}
    assert tree_content(HtmlParser.parse("<p>Hello</canvas></p>")) == {:error, :unsupported_html}
    assert tree_content(HtmlParser.parse("<p>Hello</>")) == {:error, :unsupported_html}

    assert tree_content(HtmlParser.parse(~s(<p style="a" style="b">Hello</p>))) ==
             {:error, :unsupported_html}

    assert tree_content(HtmlParser.parse(~S|<p onclick="run()">Hello</p>|)) ==
             {:error, :unsupported_html}

    assert {:ok, _dom} = HtmlParser.parse(~s(<a href="https://example.com">Inline root</a>))

    assert tree_content(HtmlParser.parse(~s(<ul><p>No item</p></ul>))) ==
             {:error, :unsupported_html}

    assert tree_content(HtmlParser.parse(~s(<table><td>No row</td></table>))) ==
             {:error, :unsupported_html}

    assert tree_content(HtmlParser.parse(~s(<head><p>No paragraph</p></head>))) ==
             {:error, :unsupported_html}

    assert tree_content(
             HtmlParser.parse(
               ~s(<table><caption>One</caption><caption>Two</caption><tr><td>Cell</td></tr></table>)
             )
           ) == {:error, :unsupported_html}

    assert {:ok, _dom} = HtmlParser.parse(~s(<table><caption>Only caption</caption></table>))

    assert {:ok, _dom} = HtmlParser.parse(~s(<table><tbody></tbody></table>))

    assert tree_content(HtmlParser.parse(~s(<table><tbody><td>No row</td></tbody></table>))) ==
             {:error, :unsupported_html}

    assert {:ok, _dom} = HtmlParser.parse(~s(<table><tbody><tr></tr></tbody></table>))

    assert tree_content(
             HtmlParser.parse(
               ~s(<p><a href="https://example.com"><a href="https://nested.example">Nested</a></a></p>)
             )
           ) ==
             {:error, :unsupported_html}

    assert tree_content(HtmlParser.parse("<p>Hello</strong></p>")) == {:error, :unsupported_html}
    assert tree_content(HtmlParser.parse(:not_html)) == {:error, :invalid_html}
  end

  test "parse_detailed returns source details for malformed tags" do
    assert {:error,
            {:unsupported_html,
             %{
               stage: :html,
               reason: :unsupported_html,
               line: 1,
               column: 1,
               source: "<div>Hello",
               message: ~s(line 1: HTML source "<div>Hello" is unsupported or malformed)
             }}} = HtmlParser.parse_detailed("<div>Hello")

    assert {:error,
            {:unsupported_html,
             %{
               stage: :html,
               reason: :unsupported_html,
               line: 1,
               column: 1,
               source: "<p Hello",
               message: ~s(line 1: HTML source "<p Hello" is unsupported or malformed)
             }}} = HtmlParser.parse_detailed("<p Hello")
  end

  test "parse accepts empty and unquoted attributes with ASCII whitespace and references" do
    assert {:ok, %{children: [paragraph]}} =
             HtmlParser.parse(
               "<P\tID=intro\nCLASS=copy\rtitle\fdata-label=Ready&amp;waiting style=color:red>Text</P>"
             )

    assert paragraph.attributes == %{
             "id" => "intro",
             "class" => "copy",
             "title" => "",
             "data-label" => "Ready&waiting",
             "style" => "color:red"
           }

    assert {:ok, %{children: [%{children: [input]}]}} =
             HtmlParser.parse("<div><input type=checkbox checked disabled name=ready></div>")

    assert input.attributes == %{
             "type" => "checkbox",
             "checked" => "",
             "disabled" => "",
             "name" => "ready"
           }
  end

  test "parse preserves slashes in unquoted values and delimiters in quoted values" do
    assert {:ok, %{children: [first, second, third]}} =
             HtmlParser.parse(
               ~s(<img src=logo.png/><img src=logo.png /><p title="<tag> 'quoted'" data-check='a>b'>Text</p>)
             )

    assert first.attributes == %{"src" => "logo.png/"}
    assert second.attributes == %{"src" => "logo.png"}
    assert third.attributes == %{"title" => "<tag> 'quoted'", "data-check" => "a>b"}
  end

  test "parse rejects malformed attribute syntax and duplicate names regardless of quoting" do
    for attributes <- [
          "id=",
          "id=x=y",
          "id=x`y",
          "id=x'y",
          "id=x\"y",
          "id=x<y",
          ~s(id="x"class="y"),
          "id=x ID='y'",
          "id=x / /",
          "id=x\u00a0class=y"
        ] do
      assert {:error, {:unsupported_html, diagnostic}} =
               HtmlParser.parse_detailed("<p #{attributes}>Text</p>")

      assert diagnostic.stage == :html
      assert diagnostic.reason == :unsupported_html
      assert diagnostic.line == 1
      assert diagnostic.column >= 1
      assert is_binary(diagnostic.message)
      assert diagnostic.source != ""
      assert String.contains?("<p #{attributes}>Text</p>", diagnostic.source)
    end
  end

  test "parse ignores valid comments in document, inline, list, and table contexts" do
    html = """
    <!-- before > ' \" <script> text -->
    <div><!-- start --><p>One<!-- inline -->Two<!----></p><!-- end --></div>
    <ul><!-- list --><li>Item</li></ul>
    <table><!-- table --><tr><!-- row --><td><!-- cell -->Value</td></tr></table>
    <!-- after -->
    """

    assert {:ok, %{children: [div, list, table]}} = HtmlParser.parse(html)

    assert tree_content(div.children) == [
             %{
               type: :element,
               tag: "p",
               attributes: %{},
               children: [
                 %{type: :text, text: "One"},
                 %{type: :text, text: "Two"}
               ]
             }
           ]

    assert [%{tag: "li"}] = list.children
    assert [%{tag: "tr", children: [%{tag: "td", children: [%{text: "Value"}]}]}] = table.children

    assert {:ok, _dom} = HtmlParser.parse("<!-- operators > and <! --><p>Valid</p>")
  end

  test "comment delimiters remain literal text in raw text and RCDATA" do
    assert {:ok, %{children: [style, title, %{children: [textarea]}]}} =
             HtmlParser.parse(
               "<style><!-- literal --></style><title><!-- title --></title><div><textarea><!-- value --></textarea></div>"
             )

    assert tree_content(style.children) == [%{type: :text, text: "<!-- literal -->"}]
    assert tree_content(title.children) == [%{type: :text, text: "<!-- title -->"}]
    assert tree_content(textarea.children) == [%{type: :text, text: "<!-- value -->"}]
  end

  test "raw text mode detection does not accept unsupported attributes" do
    for html <- [
          "<style onclick=run()>p { color: red; }</style>",
          "<title onclick=run()>Title</title>",
          "<div><textarea onclick=run()>Value</textarea></div>"
        ] do
      assert tree_content(HtmlParser.parse(html)) == {:error, :unsupported_html}
    end
  end

  test "parse_detailed returns exact comment locations for malformed comments" do
    for comment <- [
          "<!--",
          "<!-- missing",
          "<!-->bad -->",
          "<!--->bad -->",
          "<!-- nested <!-- bad -->",
          "<!-- bad --!> -->",
          "<!-- bad <!--->"
        ] do
      html = "<div>\n  #{comment}\n</div>"

      assert {:error, {:unsupported_html, diagnostic}} = HtmlParser.parse_detailed(html)
      assert diagnostic.stage == :html
      assert diagnostic.reason == :unsupported_html
      assert diagnostic.line == 2
      assert diagnostic.column == 3
      assert diagnostic.message =~ "comment"
      assert String.starts_with?(diagnostic.source, comment)
    end
  end

  test "optional end tags preserve explicit paragraph, list, option, and table trees" do
    for {omitted, explicit} <- [
          {"<p>First<p>Second", "<p>First</p><p>Second</p>"},
          {"<div><p>First<div>Second</div></div>", "<div><p>First</p><div>Second</div></div>"},
          {"<ul><li>First<li>Second</ul>", "<ul><li>First</li><li>Second</li></ul>"},
          {"<div><select><option>First<option>Second</select></div>",
           "<div><select><option>First</option><option>Second</option></select></div>"},
          {"<html><head><title>Title</title><body><p>Body",
           "<html><head><title>Title</title></head><body><p>Body</p></body></html>"},
          {"<html><body><p>Body</html>", "<html><body><p>Body</p></body></html>"},
          {"<table><caption>Totals<colgroup><col><thead><tr><th>A<th>B<tbody><tr><td>C<td>D<tr><td>E<td>F<tfoot><tr><td>G<td>H</table>",
           "<table><caption>Totals</caption><colgroup><col></colgroup><thead><tr><th>A</th><th>B</th></tr></thead><tbody><tr><td>C</td><td>D</td></tr><tr><td>E</td><td>F</td></tr></tbody><tfoot><tr><td>G</td><td>H</td></tr></tfoot></table>"},
          {"<table><tr><td>A<td>B</table>", "<table><tr><td>A</td><td>B</td></tr></table>"},
          {"<table><colgroup><col></table>", "<table><colgroup><col></colgroup></table>"},
          {"<p>First<!-- remains inside p --><p>Second", "<p>First</p><p>Second</p>"}
        ] do
      assert {:ok, dom} = HtmlParser.parse(omitted)
      assert tree_content(HtmlParser.parse(explicit)) == tree_content({:ok, dom})
    end
  end

  test "omission never repairs required end tags or invalid table section transitions" do
    for html <- [
          "<div><p>Missing div",
          "<ul><li><strong>Missing strong</ul>",
          "<table><thead><tr><th>Header</table>",
          "<table><tbody><tr><td>Body<thead><tr><th>Late</thead></table>",
          "<div><select><option>Missing select</div>",
          "<table><tr><td><span>Missing span</table>"
        ] do
      assert {:error, {:unsupported_html, diagnostic}} = HtmlParser.parse_detailed(html)
      assert diagnostic.stage == :html
      assert diagnostic.line == 1
      assert diagnostic.column > 0
      assert diagnostic.source != ""
    end
  end

  test "valid empty tables and row containers retain their DOM structure" do
    for html <- [
          "<table></table>",
          "<table><caption>Caption</caption></table>",
          "<table><colgroup></colgroup></table>",
          "<table><thead></thead><tbody><tr></tr></tbody><tfoot></tfoot></table>"
        ] do
      assert {:ok, %{children: [%{tag: "table"}]}} = HtmlParser.parse(html)
    end

    assert {:ok, %{children: []}} = HtmlParser.parse("<!-- only a comment -->")
    assert {:ok, %{children: []}} = HtmlParser.parse("<!DOCTYPE html> ")
  end

  test "fragment contexts accept cells, options, inline content, and raw text" do
    assert {:ok, %{children: [first, second]}} = HtmlParser.parse_fragment("<td>A<td>B", "TR")
    assert [first.tag, second.tag] == ["td", "td"]

    assert {:ok, %{children: [%{tag: "option"}, %{tag: "option"}]}} =
             HtmlParser.parse_fragment("<option>A<option>B", "select")

    assert {:ok, %{children: [%{type: :text, text: "Text "}, %{tag: "strong"}]}} =
             HtmlParser.parse_fragment("Text <strong>bold</strong>", "p")

    assert {:ok, %{children: [%{type: :text, text: "<p>&amp;</p>"}]}} =
             HtmlParser.parse_fragment("<p>&amp;</p>", "style")

    assert {:ok, %{children: [%{type: :text, text: "<p>&</p>"}]}} =
             HtmlParser.parse_fragment("<p>&amp;</p>", "textarea")

    assert {:ok, %{children: []}} = HtmlParser.parse_fragment("", "title")
    assert {:ok, %{children: []}} = HtmlParser.parse_fragment("", "style")
    assert {:ok, %{children: []}} = HtmlParser.parse_fragment("", "body")

    assert {:error, {:unsupported_html, _diagnostic}} =
             HtmlParser.parse_fragment("<td>A</td>", "div")

    assert {:error, {:unsupported_html, %{stage: :html, line: 1, column: 1}}} =
             HtmlParser.parse_fragment_detailed("<li>A</li>", "tr")

    for context <- ["html", "img", "unknown", nil, 42] do
      assert {:error, {:unsupported_html, diagnostic}} =
               HtmlParser.parse_fragment_detailed("", context)

      assert diagnostic.message =~ "fragment context"
      assert diagnostic.source == inspect(context)
      assert {:error, {:unsupported_html, _diagnostic}} = HtmlParser.parse_fragment("", context)
    end

    assert {:error, {:invalid_encoding, _diagnostic}} = HtmlParser.parse_fragment(<<255>>, "p")
  end

  test "wrapper-free optional groups and document sections retain their strict legacy contexts" do
    for html <- [
          "<title>T</title></head><p>B</body></html>",
          "<table><col><?column><col></colgroup><tr><td>A</tbody></table>",
          "<table><tr><td>A</td></tr><script type=application/json>{}</script><template></template></tbody></table>"
        ] do
      assert {:ok, _document} = HtmlParser.parse_detailed(html)
    end

    assert {:ok, %{children: [%{tag: "html", children: [head, body]}]}} =
             HtmlParser.parse_document_detailed("Body <p>Next</p>")

    assert head.children == []
    assert hd(body.children).text == "Body "

    for html <- [
          "<head>Visible</head>",
          "<head></head><style>p{color:red}</style><p>Body</p>",
          "<head></head><template><p>Inert</p></template><p>Body</p>"
        ] do
      assert {:error, {:unsupported_html, diagnostic}} = HtmlParser.parse_document_detailed(html)
      assert diagnostic.stage == :html and diagnostic.reason == :unsupported_html
    end

    assert {:ok, %{children: [%{tag: "html", children: [_, %{children: [%{tag: "noscript"}]}]}]}} =
             HtmlParser.parse_document_detailed("<noscript><span>Visible</span></noscript>")
  end

  test "complete document parsing constructs document and table ancestry" do
    implied = "<title>Title</title><p>Text<table><col span=2><tr><td>A<td>B</table>"

    explicit =
      "<html><head><title>Title</title></head><body><p>Text</p><table><colgroup><col span=2></colgroup><tbody><tr><td>A</td><td>B</td></tr></tbody></table></body></html>"

    assert {:ok, dom} = HtmlParser.parse_document_detailed(implied)
    assert tree_content(HtmlParser.parse_document_detailed(explicit)) == tree_content({:ok, dom})
    [html] = dom.children
    [head, body] = html.children
    assert head.tag == "head"
    assert body.tag == "body"
    [paragraph, table] = body.children
    assert paragraph.tag == "p"
    assert Enum.map(table.children, & &1.tag) == ["colgroup", "tbody"]

    assert {:ok,
            %{
              children: [
                %{tag: "title"},
                %{tag: "p"},
                %{tag: "table", children: [%{tag: "col"}, %{tag: "tr"}]}
              ]
            }} = HtmlParser.parse(implied)

    for html <- ["", "<html></html>", "<head></head><body></body>", "<body></body>"] do
      assert {:ok, %{children: [%{tag: "html", children: [%{tag: "head"}, %{tag: "body"}]}]}} =
               HtmlParser.parse_document_detailed(html)
    end

    assert {:ok, %{children: [%{attributes: %{"lang" => "en"}, children: [head, body]}]}} =
             HtmlParser.parse_document_detailed(
               "<html lang=en><head id=metadata></head><body class=copy></body></html>"
             )

    assert head.attributes == %{"id" => "metadata"}
    assert body.attributes == %{"class" => "copy"}
  end

  test "complete documents retain authored groups and reject duplicate structural elements" do
    html =
      "<table><colgroup id=first><col><col></colgroup><col><col><tr><td>One</td></tr><tr><td>Two</td></tr></table>"

    assert {:ok, %{children: [%{children: [_, %{children: [table]}]}]}} =
             HtmlParser.parse_document_detailed(html)

    [authored, implied_columns, implied_rows] = table.children
    assert authored.attributes == %{"id" => "first"}
    assert length(authored.children) == 2
    assert length(implied_columns.children) == 2
    assert length(implied_rows.children) == 2

    assert tree_content(hd(hd(implied_rows.children).children).children) == [
             %{type: :text, text: "One"}
           ]

    for html <- [
          "<html></html><p>Sibling</p>",
          "<html></html><html></html>",
          "<body></body><body></body>",
          "<head></head><head></head>",
          "<body></body><head></head>",
          "<html><body><title>Wrong</title></body></html>"
        ] do
      assert {:error, {:unsupported_html, diagnostic}} = HtmlParser.parse_document_detailed(html)
      assert diagnostic.stage == :html
      assert diagnostic.reason == :unsupported_html
      assert diagnostic.message =~ "document"
    end
  end

  test "UTF-8 BOM metadata does not paint or displace diagnostics" do
    assert tree_content(HtmlParser.parse("\uFEFF<p>Text")) ==
             tree_content(HtmlParser.parse("<p>Text"))

    assert {:error, {:unsupported_html, %{line: 2, column: 1, source: "<video>"}}} =
             HtmlParser.parse_document_detailed("\uFEFF\n<video>")
  end

  test "table content order is validated independently of optional end tags" do
    for html <- [
          "<table><thead></thead><thead></thead></table>",
          "<table><tfoot></tfoot><tfoot></tfoot></table>",
          "<table><tfoot></tfoot><tbody></tbody></table>",
          "<table><tr><td>A</td></tr><caption>Late</caption></table>",
          "<ul>Non-item text</ul>"
        ] do
      assert {:error, {:unsupported_html, %{stage: :html}}} = HtmlParser.parse_detailed(html)
    end
  end

  test "omitted start tags may have explicit closing tags" do
    for {omitted, explicit} <- [
          {"<title>Title</title></head><p>Text</body></html>",
           "<html><head><title>Title</title></head><body><p>Text</p></body></html>"},
          {"</head></body></html>", "<html><head></head><body></body></html>"},
          {"<body><p>Text</body></html>", "<html><head></head><body><p>Text</p></body></html>"},
          {"<table><col><col></colgroup><tr><td>First</tbody><tr><td>Second</tbody></table>",
           "<html><head></head><body><table><colgroup><col><col></colgroup><tbody><tr><td>First</td></tr></tbody><tbody><tr><td>Second</td></tr></tbody></table></body></html>"}
        ] do
      assert {:ok, dom} = HtmlParser.parse_document_detailed(omitted)

      assert tree_content(HtmlParser.parse_document_detailed(explicit)) ==
               tree_content({:ok, dom})
    end

    for html <- [
          "</head></head>",
          "</body>Text",
          "</html><p>After</p>",
          "<table><col></colgroup></colgroup></table>",
          "<table><tr><td>A</tbody></tbody></table>",
          "<p></ p>",
          "<style>x</ style>",
          "<p></p\u00A0>"
        ] do
      assert {:error, {:unsupported_html, %{stage: :html}}} =
               HtmlParser.parse_document_detailed(html)
    end
  end

  test "doctype validation accepts the UTF-8 HTML contract and rejects quirks or misplaced declarations" do
    assert {:ok, _dom} =
             HtmlParser.parse_document_detailed(
               ~s(<!doctype html SYSTEM "about:legacy-compat"><p>Text)
             )

    for html <- [
          "<!doctype html><!doctype html>",
          "<p>Text</p><!doctype html>",
          "<!doctype html PUBLIC 'legacy'>",
          "<div><!doctype html></div>"
        ] do
      assert {:error, {:unsupported_html, diagnostic}} = HtmlParser.parse_document_detailed(html)
      assert diagnostic.message =~ "doctype"
      assert diagnostic.source =~ "<!doctype"
      assert diagnostic.line == 1
    end
  end

  test "parse retains readonly boolean values and input applicability" do
    for attribute <- [
          "readonly",
          "READONLY",
          ~s(readonly=""),
          ~s(readonly="readonly"),
          ~s(readonly="false")
        ] do
      for type <- ["text", "TEXT", "search", "SeArCh"] do
        assert {:ok, %{children: [node]}} =
                 HtmlParser.parse(~s(<input type="#{type}" #{attribute} value="A&#10;B">))

        assert Map.has_key?(node.attributes, "readonly")
        assert node.attributes["type"] == type
        assert node.attributes["value"] == "A\nB"
      end

      assert {:ok, %{children: [node]}} =
               HtmlParser.parse("<textarea " <> attribute <> ">Line1\nLine2</textarea>")

      assert Map.has_key?(node.attributes, "readonly")
      assert Enum.map_join(node.children, & &1.text) == "Line1\nLine2"
    end

    for html <- [
          "<select readonly><option>X</option></select>",
          "<button readonly>X</button>"
        ] do
      assert {:ok, _} = HtmlParser.parse(html)
    end
  end

  test "telephone URL and single email retain authored attributes and readonly presence" do
    for type <- ["tel", "TEL", "url", "URL", "email", "EmAiL"] do
      assert {:ok, %{children: [node]}} =
               HtmlParser.parse(
                 ~s(<input type="#{type}" readonly="false" value=" \tA&#13;B&#10;C ">)
               )

      assert node.attributes == %{"type" => type, "readonly" => "false", "value" => " \tA\rB\nC "}

      for attribute <- ["onclick='go()'"] do
        assert {:error, :unsupported_html} =
                 HtmlParser.parse(~s(<input type="#{type}" #{attribute}>))
      end
    end
  end

  test "multiple applies by boolean presence to supported email inputs and selects" do
    for type <- ["email", "EMAIL"],
        attribute <- ["multiple", ~s(multiple=""), ~s(MULTIPLE="false")] do
      assert {:ok, %{children: [node]}} =
               HtmlParser.parse(
                 ~s(<input type="#{type}" #{attribute} value=" a@example.com ,&#10; b@example.com ">)
               )

      assert Map.has_key?(node.attributes, "multiple")
      assert node.attributes["value"] == " a@example.com ,\n b@example.com "
    end

    for attribute <- ["multiple", ~s(multiple=""), ~s(MULTIPLE="false")] do
      assert {:ok, %{children: [node]}} =
               HtmlParser.parse("<select #{attribute}><option>A</option></select>")

      assert Map.has_key?(node.attributes, "multiple")
    end

    for html <- [
          "<textarea multiple>A</textarea>"
        ] do
      assert {:ok, _} = HtmlParser.parse(html)
    end
  end

  test "input type defaults retain authored strings and admitted specialized states" do
    for type <- ["", " ", "unknown", "hidden ", "checKbox", "ſearch"] do
      assert {:ok, %{children: [node]}} =
               HtmlParser.parse(~s(<input TYPE="#{type}" readonly value="A&#13;B&#10;C">))

      assert node.attributes["type"] == type
      assert node.attributes["value"] == "A\rB\nC"
      assert {:ok, _} = HtmlParser.parse(~s(<input type="#{type}" checked>))
    end

    for type <- ["FILE", "color", "image"] do
      assert {:ok, %{children: [node]}} =
               HtmlParser.parse(~s(<input type="#{type}" value="Initial">))

      assert node.attributes == %{"type" => type, "value" => "Initial"}
    end
  end

  test "button inputs retain authored types and values with boolean disabled" do
    for type <- ["button", "RESET", "SuBmIt"] do
      assert {:ok, %{children: [node]}} =
               HtmlParser.parse(
                 ~s(<input TYPE="#{type}" disabled="false" value=" A&#13;B&#10;C ">)
               )

      assert node.attributes == %{"type" => type, "disabled" => "false", "value" => " A\rB\nC "}
      assert {:ok, %{children: [missing]}} = HtmlParser.parse(~s(<input type="#{type}">))
      refute Map.has_key?(missing.attributes, "value")

      for inactive <- ["readonly", "multiple", "checked", "src='denied.png'"] do
        assert {:ok, _} =
                 HtmlParser.parse(~s(<input type="#{type}" #{inactive}>))
      end
    end
  end

  test "image dimension attributes preserve original values for rendering hints and CSS" do
    assert {:ok, %{children: [image]}} =
             HtmlParser.parse(~s(<img src="supplied.png" WIDTH=" 24.5%junk" height>))

    assert image.attributes == %{"src" => "supplied.png", "width" => " 24.5%junk", "height" => ""}
  end

  test "password inputs preserve authored initial attributes before sanitization" do
    assert {:ok, %{children: [node]}} =
             HtmlParser.parse(
               ~s(<input type="PASSWORD" readonly="false" name="secret" value="A&#13;B&#10;C">)
             )

    assert node.attributes == %{
             "type" => "PASSWORD",
             "readonly" => "false",
             "name" => "secret",
             "value" => "A\rB\nC"
           }
  end

  test "parse accepts standard scope values only on table headers" do
    assert {:ok, dom} =
             HtmlParser.parse("""
             <table>
               <tr>
                 <th scope="col">Column</th>
                 <th scope="row">Row</th>
                 <th scope="rowgroup">Row group</th>
                 <th scope="colgroup">Column group</th>
               </tr>
             </table>
             """)

    [table] = dom.children
    [row] = table.children

    assert Enum.map(row.children, & &1.attributes["scope"]) ==
             ~w(col row rowgroup colgroup)

    assert {:ok,
            %{children: [%{children: [%{children: [%{attributes: %{"scope" => "column"}}]}]}]}} =
             HtmlParser.parse("<table><tr><th scope=\"column\">Bad</th></tr></table>")

    assert HtmlParser.parse("<table><tr><td scope=\"col\">Bad</td></tr></table>") ==
             {:error, :unsupported_html}
  end

  test "fieldsets retain boolean disabling, ownership, legends and valid flow contents" do
    html = """
    <form id=outer><fieldset disabled=false form=later name=group>
    <legend align=CENTER><h2>Caption</h2><label>Toggle<input type=checkbox></label></legend>
    <p>Flow</p><fieldset><legend>Nested</legend><input name=value></fieldset>
    <template><fieldset disabled><legend>Inert</legend><img src=denied.png></fieldset></template>
    </fieldset></form><form id=later></form>
    """

    assert {:ok, %{children: [form, later]}} = HtmlParser.parse_detailed(html)
    [fieldset] = form.children
    assert fieldset.attributes == %{"disabled" => "false", "form" => "later", "name" => "group"}
    [legend, paragraph, nested, template] = Enum.filter(fieldset.children, &(&1.type == :element))
    assert legend.attributes == %{"align" => "CENTER"}
    assert Enum.map(legend.children, & &1.tag) == ["h2", "label"]
    assert paragraph.tag == "p" and nested.tag == "fieldset"
    assert template.children == [] and template.content.children != []
    assert later.attributes == %{"id" => "later"}

    assert {:ok, %{children: [%{tag: "fieldset", children: []}]}} =
             HtmlParser.parse_detailed("<fieldset></fieldset>")
  end

  test "fieldset admits file controls while preserving source values without loading files" do
    assert {:ok, %{children: [fieldset]}} =
             HtmlParser.parse(
               "<fieldset disabled><input type=FILE value='/not-a-file/private.txt' multiple></fieldset>"
             )

    assert [input] = fieldset.children

    assert input.attributes == %{
             "type" => "FILE",
             "value" => "/not-a-file/private.txt",
             "multiple" => ""
           }

    assert fieldset.attributes == %{"disabled" => ""}
  end

  test "fieldset and legend parsing rejects unsupported attributes and invalid parents" do
    for html <- [
          "<legend>Orphan</legend>",
          "<fieldset><legend><div>Block</div></legend></fieldset>",
          "<p><fieldset></fieldset></p>"
        ] do
      assert {:error, {reason, diagnostic}} = HtmlParser.parse_detailed(html)
      assert reason in [:unsupported_html, :invalid_html]
      assert diagnostic.stage == :html and diagnostic.reason == reason
      assert is_binary(diagnostic.message) and diagnostic.message != ""
      assert is_integer(diagnostic.line) and is_integer(diagnostic.column)
    end
  end
end
