defmodule NativeElixirPdfUtilities.HtmlToPdf.HtmlSelectedContentTest do
  use ExUnit.Case, async: true
  alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, HtmlSelectedContent, Style}

  test "initial clones use the actual selected children instead of the option label" do
    assert {:ok, dom} =
             HtmlParser.parse_detailed(
               "<select><button><selectedcontent></selectedcontent></button><option label='Different label'><span>A</span></option><option selected disabled><strong>B</strong><span> C</span></option></select>"
             )

    assert {:ok, projection} = HtmlSelectedContent.compose(dom)
    assert projection.logical == dom
    [select] = projection.composed.children
    [button, _first, selected] = select.children
    [content] = button.children
    [strong, span] = content.children
    assert strong.tag == "strong"
    assert [%{text: "B"}] = strong.children
    assert span.tag == "span"
    assert strong.node_id != hd(selected.children).node_id
    assert strong.cloned_from_node_id == hd(selected.children).node_id
    assert strong.source == hd(selected.children).source
    assert strong.document_id == content.document_id
    assert strong.tree_scope_id == content.tree_scope_id
    assert strong.parent_id == content.node_id
    assert content._selected_content_option_id == selected.node_id
    assert select._initial_form_state.selection.value == "B C"
  end

  test "recomposition after a validated authored child change replaces clones with fresh identities" do
    assert {:ok, dom} =
             HtmlParser.parse_detailed(
               "<select><button><selectedcontent></selectedcontent></button><option selected><span>Before</span></option></select>"
             )

    assert {:ok, first} = HtmlSelectedContent.compose(dom)
    [select] = dom.children
    [button, option] = select.children
    [span] = option.children
    [text] = span.children
    # This models a validated tree update, without enabling template-for mutation.
    edited = %{span | children: [%{text | text: "After"}]}
    dom = %{dom | children: [%{select | children: [button, %{option | children: [edited]}]}]}
    assert {:ok, second} = HtmlSelectedContent.compose(dom)
    old_clone = hd(hd(hd(hd(first.composed.children).children).children).children)
    new_clone = hd(hd(hd(hd(second.composed.children).children).children).children)
    assert new_clone.node_id != old_clone.node_id
    assert new_clone.cloned_from_node_id == old_clone.cloned_from_node_id
    assert [%{text: "After"}] = new_clone.children
  end

  test "multiple selects skip updates and option descendants disable cloning" do
    assert {:ok, dom} =
             HtmlParser.parse_detailed(
               "<select multiple><button><selectedcontent></selectedcontent></button><option selected><span>Choice</span><selectedcontent></selectedcontent></option></select>"
             )

    assert {:ok, projection} = HtmlSelectedContent.compose(dom)
    [select] = projection.composed.children
    [button, option] = select.children
    [content] = button.children
    assert content.children == []
    refute content._selected_content_disabled
    [_span, nested] = option.children
    assert nested.children == []
    assert nested._selected_content_disabled
  end

  test "base-select renders cloned rich text through the authored button" do
    html =
      "<style>select::picker-icon{display:none}</style><select style='appearance:base-select;width:120pt;height:24pt;padding:0;border:0'><button style='display:block;width:auto;height:auto;border:0;padding:0;font-family:DejaVu Sans;font-size:12pt'><selectedcontent></selectedcontent></button><option selected><strong>Actual</strong><span> tree</span></option></select>"

    assert {:ok, dom} = HtmlParser.parse_detailed(html)

    assert {:ok, styled} =
             Style.compute_detailed(dom, forms: :static, system_font_discovery: false)

    [select] = styled.children
    assert select.style.form_kind == :base_select
    [button] = select.children
    [content] = button.children
    assert [%{tag: "strong", style: %{font_weight: 700}}, %{tag: "span"}] = content.children
  end

  test "cloned inert fragments receive independent scope identities and hidden controls keep canonical state" do
    html =
      "<select><button><selectedcontent></selectedcontent></button><option selected><input type=hidden value=A><template><span>Inert</span></template>Value</option></select>"

    assert {:ok, dom} = HtmlParser.parse_detailed(html)
    assert {:ok, projection} = HtmlSelectedContent.compose(dom)
    [select] = projection.composed.children
    [button, option] = select.children
    [content] = button.children
    [input, template, _text] = content.children
    assert input._initial_form_state.disabled == false
    assert template.content.node_id != Enum.at(option.children, 1).content.node_id
    assert template.content.parent_id == nil
    assert template.content.tree_scope_id == template.content.node_id
    assert hd(template.content.children).tree_scope_id == template.content.node_id

    assert :ok =
             NativeElixirPdfUtilities.Validators.HtmlTreeValidator.validate_transport(
               projection.composed
             )

    assert {:ok, _styled} = Style.compute_detailed(dom, system_font_discovery: false)
  end

  test "no selected option clears initial selected content without changing authored nodes" do
    assert {:ok, dom} =
             HtmlParser.parse_detailed(
               "<select size=2><button><selectedcontent></selectedcontent></button><option>Unselected</option></select>"
             )

    assert {:ok, projection} = HtmlSelectedContent.compose(dom)
    [select] = projection.composed.children
    [button, _option] = select.children
    [content] = button.children
    assert content.children == []
    assert content._selected_content_option_id == nil
    assert projection.logical == dom
  end

  test "rich option text skips scripts while descendant DOM text preserves them" do
    assert {:ok, dom} =
             HtmlParser.parse_detailed(
               "<select><option><span>Rich</span><script type=application/json>data</script> text</option></select>"
             )

    [select] = dom.children
    [option] = select.children

    assert NativeElixirPdfUtilities.Validators.HtmlValidator.descendant_text_content(
             option.children
           ) == "Richdata text"

    assert NativeElixirPdfUtilities.Validators.HtmlValidator.initial_option_text(option.children) ==
             "Rich text"

    assert {:ok, projection} = HtmlSelectedContent.compose(dom)
    assert hd(projection.composed.children)._initial_form_state.selection.value == "Rich text"
  end
end

defmodule NativeElixirPdfUtilities.HtmlToPdf.HtmlSelectedContentBudgetTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.{Limits, HtmlToPdf.HtmlParser, HtmlToPdf.HtmlSelectedContent}

  test "clone growth shares the configured authored node budget" do
    assert {:ok, dom} =
             HtmlParser.parse_detailed(
               "<select><button><selectedcontent></selectedcontent></button><option><span>A</span></option></select>"
             )

    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    assert :ok = Limits.install(Map.put(limits, :max_html_nodes, 6))
    assert {:error, {:resource_limit_exceeded, diagnostic}} = HtmlSelectedContent.compose(dom)
    assert diagnostic.stage == :html
    assert diagnostic.reason == :resource_limit_exceeded
    assert diagnostic.message =~ "max_html_nodes"
  end
end
