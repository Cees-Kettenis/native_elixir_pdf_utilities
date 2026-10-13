defmodule NativeElixirPdfUtilities.HtmlToPdf.ShadowFragmentLinksTest do
  use ExUnit.Case, async: false

  alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, Style}
  alias NativeElixirPdfUtilities.{HtmlToPdf, Text}
  alias NativeElixirPdfUtilities.Pdf.Reader

  test "slotted light-tree targets stay selectable below shadow ancestors" do
    html =
      "<a href='#light'>Outer jump</a><div><template shadowrootmode=open><section><slot></slot><a href='#light'>Shadow jump</a></section></template><p id=light>Light target</p></div>"

    assert {:ok, dom} = HtmlParser.parse_document_detailed(html)
    assert {:ok, styled} = Style.compute(dom, system_font_discovery: false)
    nodes = elements(styled.children)
    target = Enum.find(nodes, &(&1.tag == "p"))
    assert Map.get(target.style, :html_target) != nil

    for anchor <- Enum.filter(nodes, &(&1.tag == "a")) do
      assert anchor.style.link_destination == {:target, target.style.html_target}
    end
  end

  test "shadow-only IDs and legacy names never become document fragment targets" do
    html =
      "<a href='#private'>ID jump</a><a href='#legacy'>Name jump</a><div><template shadowrootmode=open><p id=private>Private target</p><a name=legacy>Shadow legacy</a><a href='#private'>Shadow jump</a></template></div>"

    assert {:ok, pdf} = HtmlToPdf.render(html, system_font_discovery: false)
    assert {:ok, context} = Reader.read_validated(pdf)

    refute Enum.any?(context.document.objects, fn {_id, object} ->
             object.value["Subtype"] == {:name, "Link"}
           end)

    assert {:ok, text} = Text.extract(pdf)
    assert text =~ "Private target" and text =~ "Shadow legacy"
  end

  test "unassigned first light ID blocks a later visible duplicate" do
    html =
      "<a href='#duplicate'>Jump</a><div><template shadowrootmode=open><slot name=shown></slot></template><p id=duplicate>Unassigned first</p></div><p id=duplicate>Visible second</p>"

    assert {:ok, pdf} = HtmlToPdf.render(html, system_font_discovery: false)
    assert {:ok, context} = Reader.read_validated(pdf)

    refute Enum.any?(context.document.objects, fn {_id, object} ->
             object.value["Subtype"] == {:name, "Link"}
           end)

    assert {:ok, text} = Text.extract(pdf)
    refute text =~ "Unassigned first"
    assert text =~ "Visible second"
  end

  test "slot order does not reorder first document IDs and consumed templates are absent" do
    html =
      "<a href='#duplicate'>Jump</a><a href='#consumed'>Consumed jump</a><div><template id=consumed shadowrootmode=open><slot name=second></slot><slot name=first></slot></template><p slot=first id=duplicate>Authored first</p><p slot=second id=duplicate>Rendered first</p></div>"

    assert {:ok, dom} = HtmlParser.parse_document_detailed(html)
    assert {:ok, styled} = Style.compute(dom, system_font_discovery: false)
    nodes = elements(styled.children)
    targets = Enum.filter(nodes, &(&1.tag == "p"))
    assert Enum.count(targets, &(Map.get(&1.style, :html_target) != nil)) == 1

    assert Enum.find(targets, &(Map.get(&1.style, :html_target) != nil)).children
           |> Enum.map_join(&Map.get(&1, :text, "")) == "Authored first"

    consumed = Enum.filter(nodes, &(&1.tag == "a")) |> Enum.at(1)
    assert Map.get(consumed.style, :link_destination) == nil
  end

  test "actual selected-content clones precede their option originals in document lookup" do
    alias NativeElixirPdfUtilities.HtmlToPdf.HtmlSelectedContent
    alias NativeElixirPdfUtilities.Validators.HtmlLinkValidator

    html =
      "<style>select::picker-icon{margin-left:0}</style><a href='#copied'>Clone jump</a><a href='#private'>Private jump</a><select style='appearance:base-select'><button><selectedcontent></selectedcontent></button><option><div><template shadowrootmode=open shadowrootclonable><slot></slot><p id=private>Shadow copy</p></template><span id=copied>Copied target</span></div></option></select>"

    assert {:ok, dom} = HtmlParser.parse_document_detailed(html)
    original = Enum.find(elements(dom.children), &(&1.attributes["id"] == "copied"))
    assert {:ok, composition} = HtmlSelectedContent.compose(dom)
    nodes = elements(composition.composed.children)
    clone = Enum.find(nodes, &(Map.get(&1, :cloned_from_node_id) == original.node_id))
    assert clone != nil
    assert clone.node_id != original.node_id
    assert clone.source == original.source

    assert {:ok, context} =
             HtmlLinkValidator.prepare_composition(
               composition.composed.children,
               %{fallback_base: nil, resolution_base: nil, authored_base?: false},
               composition,
               nil
             )

    assert context.links["#copied"] ==
             {:target, {context.document_id, context.tree_scope_id, clone._selector_id}}

    assert context.links["#private"] == nil
    assert composition.logical == dom
    select = Enum.find(nodes, &(&1.tag == "select"))
    alias NativeElixirPdfUtilities.Validators.HtmlValidator

    refute HtmlValidator.valid_form_element?(
             "select",
             select.attributes,
             select._initial_form_children
           )

    assert HtmlValidator.valid_form_element?(
             "select",
             select.attributes,
             select._initial_form_children,
             :initial_dom
           )

    assert {:ok, pdf} = HtmlToPdf.render(html, system_font_discovery: false)
    assert {:ok, document} = Reader.read_validated(pdf)

    assert Enum.any?(document.document.objects, fn {_id, object} ->
             object.value["Subtype"] == {:name, "Link"}
           end)
  end

  test "executed selected-content validation keeps interactive descendants denied" do
    alias NativeElixirPdfUtilities.Validators.HtmlValidator

    for tag <- ["a", "button", "input"] do
      attributes = if tag == "a", do: %{"href" => "https://example.test"}, else: %{}
      child = %{type: :element, tag: tag, attributes: attributes, children: []}
      selected = %{type: :element, tag: "selectedcontent", attributes: %{}, children: [child]}
      button = %{type: :element, tag: "button", attributes: %{}, children: [selected]}
      refute HtmlValidator.valid_form_element?("select", %{}, [button], :initial_dom)
    end
  end

  defp elements(nodes) do
    Enum.flat_map(nodes, fn node ->
      case node do
        %{type: :element, children: children} -> [node | elements(children)]
        _ -> []
      end
    end)
  end
end
