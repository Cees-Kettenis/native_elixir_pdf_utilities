defmodule NativeElixirPdfUtilities.HtmlToPdf.ComposedMetadataTest do
  use ExUnit.Case, async: true

  alias NativeElixirPdfUtilities.HtmlToPdf.{
    HtmlComposition,
    HtmlParser,
    HtmlSelectedContent,
    Style
  }

  alias NativeElixirPdfUtilities.Validators.{
    HtmlFormValidator,
    HtmlMetadataValidator,
    HtmlValidator
  }

  test "translation uses logical parents while inertness uses the flat tree" do
    html =
      "<div translate=no><template shadowrootmode=open><section translate=no inert><slot translate=yes></slot><span id=shadow>Shadow</span></section></template><span id=assigned itemscope itemtype='https://vocab.test/Kind' itemid='/identifier' itemprop=name>Assigned</span></div>"

    assert {:ok, dom} = HtmlParser.parse_detailed(html)
    assert {:ok, composition} = HtmlComposition.compose(dom)

    [host] =
      HtmlFormValidator.prepare_composed_initial_nodes(
        dom,
        composition.roots,
        composition.composed.children,
        %{}
      )

    [section] = host.children
    [slot, shadow] = section.children
    [assigned] = slot.children
    refute host._html_translate
    refute section._html_translate
    assert slot._html_translate
    refute assigned._html_translate
    refute shadow._html_translate
    assert assigned._html_inert
    [original] = Enum.filter(hd(dom.children).children, &(Map.get(&1, :tag) == "span"))

    assert Map.take(assigned, [:node_id, :parent_id, :source, :attributes]) ==
             Map.take(original, [:node_id, :parent_id, :source, :attributes])
  end

  test "shadow-root translation starts enabled and does not inherit the host" do
    assert {:ok, dom} =
             HtmlParser.parse_detailed(
               "<div translate=no><template shadowrootmode=open><span>Shadow</span><slot></slot></template><span>Assigned</span></div>"
             )

    assert {:ok, composition} = HtmlComposition.compose(dom)

    [host] =
      HtmlFormValidator.prepare_composed_initial_nodes(
        dom,
        composition.roots,
        composition.composed.children,
        %{}
      )

    [shadow, slot] = host.children
    assert shadow._html_translate
    assert slot._html_translate
    refute hd(slot.children)._html_translate
  end

  test "inert shadow ancestors preserve slotted artwork and only suppress their own interactions" do
    html =
      "<div><template shadowrootmode=open><section inert><slot></slot></section></template>" <>
        "<a href='https://example.test/inert'>Slotted link</a>" <>
        "<input name=slotted value='Slotted value'></div><input name=active value=Active>"

    assert {:ok, pdf} =
             NativeElixirPdfUtilities.HtmlToPdf.render(html,
               system_font_discovery: false,
               compress_streams: false
             )

    assert {:ok, text} = NativeElixirPdfUtilities.Text.extract(pdf)
    assert text =~ "Slotted link" and text =~ "Slotted value"
    assert {:ok, [field]} = NativeElixirPdfUtilities.Forms.fields(pdf)
    assert field.name == "active" and field.value == "Active"
    refute pdf =~ "/Subtype /Link"
    refute pdf =~ "https://example.test/inert"
  end

  test "read-write selectors use logical editability of slotted children" do
    for {host_editable, slot_editable, color} <- [
          {"true", "false", {1, 0, 0}},
          {"false", "true", {0, 0, 1}}
        ] do
      html =
        "<style>span:read-write{color:red}span:read-only{color:blue}</style><div contenteditable=#{host_editable}><template shadowrootmode=open><slot contenteditable=#{slot_editable}></slot></template><span>Assigned</span></div>"

      assert {:ok, dom} = HtmlParser.parse_detailed(html)
      assert {:ok, tree} = Style.compute_detailed(dom)
      [host] = tree.children
      [assigned] = host.children
      assert assigned.style.color == color
    end
  end

  test "cloned metadata inherits its destination and preserves actual source identity" do
    html =
      "<select translate=no><button contenteditable><selectedcontent></selectedcontent></button><option selected><span itemscope itemtype='urn:kind' itemprop=name>Clone</span></option></select>"

    assert {:ok, dom} = HtmlParser.parse_detailed(html)
    assert {:ok, composition} = HtmlSelectedContent.compose(dom)
    [select] = composition.composed.children
    [button, option] = select.children
    [content] = button.children
    [clone] = content.children
    [original] = option.children
    refute clone._html_translate
    assert clone._html_editable
    assert clone.node_id != original.node_id
    assert clone.cloned_from_node_id == original.node_id
    assert clone.attributes == original.attributes
    assert clone.source == original.source
  end

  test "foreign and namespaced inert attributes do not establish HTML inertness" do
    foreign = %{
      type: :element,
      namespace: :svg,
      tag: "svg",
      attributes: %{"inert" => ""},
      children: []
    }

    qualified = %{
      type: :element,
      tag: "div",
      attributes: %{"inert" => ""},
      attribute_namespaces: %{"inert" => "urn:foreign"},
      children: []
    }

    for node <- HtmlValidator.assign_render_metadata([foreign, qualified]),
        do: refute(Map.has_key?(node, :_html_inert))

    assert [%{children: [%{_html_inert: true}]}] =
             HtmlValidator.assign_render_metadata([
               %{type: :element, tag: "div", attributes: %{"inert" => ""}, children: [foreign]}
             ])
  end

  test "styled owner retains selector-visible metadata and source; generated pseudo nodes have no copied identities" do
    html =
      "<style>span[itemprop=name]::before{content:attr(itemprop)}</style><span translate=no itemscope itemtype='urn:kind' itemprop=name>Source</span>"

    assert {:ok, dom} = HtmlParser.parse_detailed(html)
    original = List.last(dom.children)
    assert {:ok, %{children: [span]}} = Style.compute_detailed(dom)

    assert Map.take(span, [:node_id, :parent_id, :source, :attributes]) ==
             Map.take(original, [:node_id, :parent_id, :source, :attributes])

    refute span._html_translate
    [generated, _text] = span.children
    assert generated.tag == "::before"
    refute Map.has_key?(generated, :node_id)
    assert hd(generated.children).text == "name"
    assert HtmlMetadataValidator.translation_mode(span, true) == false
  end

  test "shadow stylesheet titles and default-style pragmas do not select document sets" do
    html =
      "<div><template shadowrootmode=open><style title=First>p{color:red}</style><style title=Second>p{color:blue}</style><meta http-equiv=default-style content=First><p>Shadow</p></template></div>"

    assert {:ok, dom} = HtmlParser.parse_detailed(html)
    assert {:ok, tree} = Style.compute_detailed(dom)
    assert hd(hd(tree.children).children).style.color == {0, 0, 1}
  end

  test "inert global interaction hints remain exact selector-visible metadata" do
    attributes =
      "accesskey='a b' autocapitalize=words autocorrect=off autofocus draggable=false enterkeyhint=search inputmode=decimal nonce=source spellcheck=false tabindex=-1 writingsuggestions=false"

    html =
      "<style>span[spellcheck=false]::before{content:attr(inputmode)}</style><div translate=no><span #{attributes}>Visible</span></div>"

    assert {:ok, dom} = HtmlParser.parse_detailed(html)
    assert {:ok, tree} = Style.compute_detailed(dom)
    [span] = hd(tree.children).children
    assert span.attributes == hd(List.last(dom.children).children).attributes
    assert span.attributes["accesskey"] == "a b"
    assert hd(hd(span.children).children).text == "decimal"
    refute span._html_translate

    assert {:ok, _} =
             HtmlParser.parse_detailed(
               "<script type=application/json #{attributes}>{}</script><template #{attributes}>Hidden</template>"
             )

    assert {:error, {:unsupported_html, diagnostic}} =
             HtmlParser.parse_detailed("<a><span tabindex=-1>Invalid descendant</span></a>")

    assert diagnostic.stage == :html
  end
end
