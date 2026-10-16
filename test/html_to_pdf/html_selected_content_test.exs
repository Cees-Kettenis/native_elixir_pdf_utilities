defmodule NativeElixirPdfUtilities.HtmlToPdf.HtmlSelectedContentTest do
  use ExUnit.Case, async: true
  alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, HtmlSelectedContent, Style}

  test "validated prepared children retain source identity through the direct population entry" do
    assert {:ok, dom} =
             HtmlParser.parse_detailed(
               "<select><button><selectedcontent></selectedcontent></button><option selected><em>Prepared choice</em></option></select>"
             )

    assert {:ok, composition} =
             NativeElixirPdfUtilities.HtmlToPdf.HtmlComposition.compose_validated(dom)

    nodes =
      NativeElixirPdfUtilities.Validators.HtmlFormValidator.prepare_composed_initial_nodes(
        dom,
        composition.roots,
        composition.composed.children,
        %{}
      )

    assert {:ok, [select]} = HtmlSelectedContent.populate_validated(nodes, dom)
    [button, option] = select.children
    [content] = button.children
    [clone] = content.children
    assert clone.cloned_from_node_id == hd(option.children).node_id
    assert clone.source == hd(option.children).source
    assert clone.parent_id == content.node_id
    assert [%{text: "Prepared choice"}] = clone.children
    assert dom.children != [select]
  end

  test "an empty select has no selection and standalone selected content stays inert" do
    assert {:ok, dom} =
             HtmlParser.parse_detailed(
               "<selectedcontent>Standalone</selectedcontent><select><button><selectedcontent>Before</selectedcontent></button></select>"
             )

    assert {:ok, projection} = HtmlSelectedContent.compose(dom)
    [standalone, select] = projection.composed.children
    assert [%{text: "Standalone"}] = standalone.children
    refute standalone._selected_content_disabled
    [button] = select.children
    [content] = button.children
    assert content.children == []
    assert select._initial_form_state.selection.id == nil
    assert select._initial_form_state.selection.selected_ids == []
    assert projection.logical == dom
  end

  test "base-select dropdowns retain inherited explicit and unitless line height" do
    for {line_height, expected} <- [{"24pt", 24.0}, {"1.5", 30.0}] do
      assert {:ok, dom} =
               HtmlParser.parse_detailed(
                 "<div style='font-size:20pt;line-height:#{line_height}'><select style='appearance:base-select'><option>Choice</option></select></div>"
               )

      assert {:ok, %{children: [%{children: [select]}]}} =
               Style.compute_detailed(dom, forms: :static, system_font_discovery: false)

      assert select.style.line_height == expected
    end
  end

  test "invalid default picker declarations return actionable diagnostics instead of raising" do
    html =
      "<style>select::picker-icon{color:bad}</style><select style='appearance:base-select'><option>Choice</option></select>"

    assert {:error, {:invalid_css, diagnostic}} = NativeElixirPdfUtilities.HtmlToPdf.render(html)
    assert diagnostic.stage == :css
    assert diagnostic.reason == :invalid_css
    assert diagnostic.operation == :render
    assert diagnostic.source == "color:bad"
    assert diagnostic.line == 1 and diagnostic.column > 0
    assert diagnostic.message =~ "invalid or unsupported"
  end

  test "base-select listbox defaults use inherited line height and authored important appearance" do
    assert {:ok, dom} =
             HtmlParser.parse_detailed(
               "<div style='font-size:20pt;line-height:24pt'><select size=3 style='appearance:base-select!important'><option>One</option><option>Two</option></select></div>"
             )

    assert {:ok, %{children: [%{children: [select]}]}} =
             Style.compute_detailed(dom, forms: :static, system_font_discovery: false)

    assert select.style.form_kind == :listbox
    assert select.style.height == 72.0
    assert select.style.min_height == 24.0
    assert select.style.overflow == :auto
    assert select.style.border_radius == 0

    # Source-bound declarations carry original spans beside the same parsed
    # tuple. They must preserve the listbox defaults computed from that tuple.
    assert {:ok, sources} =
             NativeElixirPdfUtilities.Validators.HtmlCssSourceValidator.prepare(dom)

    [parent] = dom.children
    [original_select] = parent.children

    assert {:ok, %{children: [styled_parent]}} =
             Style.compute_detailed(dom, forms: :static, system_font_discovery: false)

    declarations = Map.fetch!(sources.records, original_select.node_id)

    assert [%{declaration: {"appearance", "base-select", :important}, source: source}] =
             declarations

    assert source.text =~ "appearance:base-select"

    defaults =
      NativeElixirPdfUtilities.Validators.HtmlSelectedContentValidator.prepare_select_defaults(
        Map.put(select.style, :_select_inherited_style, styled_parent.style),
        declarations
      )

    assert defaults.form_kind == select.style.form_kind
    assert defaults.height == select.style.height
    assert defaults.padding == select.style.padding
    refute Map.has_key?(defaults, :_select_inherited_style)
  end

  test "invalid authored appearance returns canonical CSS diagnostics for both priorities" do
    for priority <- ["", "!important"] do
      assert {:ok, dom} =
               HtmlParser.parse_detailed(
                 "<select style='appearance:unknown#{priority}'><option>Choice</option></select>"
               )

      assert {:error, {:invalid_css, diagnostic}} =
               Style.compute_detailed(dom, system_font_discovery: false)

      assert diagnostic.stage == :css and diagnostic.reason == :invalid_css
      assert diagnostic.message =~ "appearance"
    end
  end

  test "nested select ancestry disables selected-content updates without changing the source option" do
    # Projected ancestor chains are used by metadata-free legacy DOM transport.
    # Each ancestor and the selected-content node comes from the canonical parser.
    assert {:ok, outer_dom} = HtmlParser.parse_detailed("<select><option>Outer</option></select>")
    assert {:ok, inner_dom} = HtmlParser.parse_detailed("<select><option>Inner</option></select>")

    assert {:ok, content_dom} =
             HtmlParser.parse_detailed("<selectedcontent>Original</selectedcontent>")

    [outer] = outer_dom.children
    [inner] = inner_dom.children
    [content] = content_dom.children

    plan =
      NativeElixirPdfUtilities.Validators.HtmlSelectedContentValidator.selected_content_plan(
        Map.delete(content, :parent_id),
        [inner, outer],
        %{},
        %{}
      )

    assert plan.disabled and not plan.update
    assert plan.option == nil
    assert [%{text: "Original"}] = content.children
    assert [%{text: "Outer"}] = hd(outer.children).children
  end

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

  test "unclonable shadow hosts copy actual light children and omit shadow contents" do
    html =
      "<select><button><selectedcontent></selectedcontent></button><option><span><template shadowrootmode=open><strong>Shadow</strong></template><em>Light</em></span></option></select>"

    assert {:ok, dom} = HtmlParser.parse_detailed(html)
    assert {:ok, projection} = HtmlSelectedContent.compose(dom)
    [select] = projection.composed.children
    [button, _option] = select.children
    [content] = button.children
    [host] = content.children
    refute Map.has_key?(host, :shadow_root_id)
    assert [%{tag: "em", children: [%{text: "Light"}]}] = host.children
    assert map_size(projection.roots) == 1
  end

  test "clonable roots receive fresh scopes, slot assignment and scoped stylesheet owners" do
    html =
      "<style>select::picker-icon{display:none}</style><select style='appearance:base-select'><button><selectedcontent></selectedcontent></button><option><span><template shadowrootmode=closed shadowrootclonable><style>::slotted(em){color:blue}</style><slot></slot></template><em>Light</em></span></option></select>"

    assert {:ok, dom} = HtmlParser.parse_detailed(html)
    assert {:ok, projection} = HtmlSelectedContent.compose(dom)
    [select] = Enum.reject(projection.composed.children, &(Map.get(&1, :tag) == "style"))
    [button, option] = select.children
    [content] = button.children
    [clone] = content.children
    [original] = option.children
    assert clone.shadow_root_id != original.shadow_root_id
    assert map_size(projection.roots) == 2
    root = projection.roots[clone.shadow_root_id]
    assert root.host_id == clone.node_id
    assert root.host_tree_scope_id == clone.tree_scope_id
    assert root.cloned_from_node_id == original.shadow_root_id
    assert root.tree_scope_id == root.node_id
    [_style, slot] = clone.children
    [assigned] = slot.children
    assert assigned.parent_id == clone.node_id
    assert assigned.assigned_shadow_scope_id == root.node_id
    assert assigned.node_id in projection.assignments[slot.node_id]
    assert {:ok, styled} = Style.compute_detailed(dom, system_font_discovery: false)
    [select] = styled.children
    [content] = select.children
    [clone] = content.children
    assert hd(clone.children).style.color == {0, 0, 1}
  end

  test "slotted clone positional selectors use actual logical light siblings" do
    html =
      "<style>select::picker-icon{display:none}</style><select style='appearance:base-select'><button><selectedcontent></selectedcontent></button><option><span><template shadowrootmode=open shadowrootclonable><style>::slotted(em:first-child){color:red}</style><slot name=second></slot><slot name=first></slot></template><em slot=first>First</em><em slot=second>Second</em></span></option></select>"

    assert {:ok, dom} = HtmlParser.parse_detailed(html)
    assert {:ok, styled} = Style.compute_detailed(dom, system_font_discovery: false)
    [select] = styled.children
    [content] = select.children
    [host] = content.children
    [second, first] = host.children
    assert hd(second.children).text == "Second"
    assert hd(first.children).text == "First"
    assert second.style.color == {0, 0, 0}
    assert first.style.color == {1, 0, 0}
  end

  test "writer validates logical option children while painting actual cloned shadow children" do
    html =
      "<style>select::picker-icon{display:none}</style><select style='appearance:base-select'><button><selectedcontent></selectedcontent></button><option><span><template shadowrootmode=open shadowrootclonable><style>strong{color:blue}</style><strong>Shadow</strong></template>Light</span></option></select>"

    assert {:ok, dom} = HtmlParser.parse_detailed(html)
    assert {:ok, projection} = HtmlSelectedContent.compose(dom)
    select = Enum.find(projection.composed.children, &(Map.get(&1, :tag) == "select"))
    [_button, option] = select._initial_form_children
    [host] = option.children
    assert hd(host.children).tag == "template"

    assert {:ok, pdf} =
             NativeElixirPdfUtilities.HtmlToPdf.render(html,
               forms: :static,
               system_font_discovery: false
             )

    assert String.starts_with?(pdf, "%PDF-")
    assert {:ok, text} = NativeElixirPdfUtilities.Text.extract(pdf, layout: false)
    assert length(Regex.scan(~r/Shadow/, text)) == 1
    refute text =~ "Light"
  end

  test "a cloned shadow root initializes its own nested select content within the new scope" do
    html =
      "<select><button><selectedcontent></selectedcontent></button><option><span><template shadowrootmode=open shadowrootclonable><select><button><selectedcontent></selectedcontent></button><option selected><strong>Inner choice</strong></option></select></template>Outer light</span></option></select>"

    assert {:ok, dom} = HtmlParser.parse_detailed(html)
    assert {:ok, projection} = HtmlSelectedContent.compose(dom)
    [outer] = projection.composed.children
    [button, _option] = outer.children
    [content] = button.children
    [host] = content.children
    [inner] = host.children
    [inner_button, _inner_option] = inner.children
    [inner_content] = inner_button.children
    refute inner_content._selected_content_disabled
    assert [%{tag: "strong", children: [%{text: "Inner choice"}]}] = inner_content.children
    assert inner_content.tree_scope_id == host.shadow_root_id
  end

  test "metadata-free clones retain unbound transport and exact source-copy validation diagnoses changes" do
    content = %{type: :element, tag: "selectedcontent", attributes: %{}, children: []}
    button = %{type: :element, tag: "button", attributes: %{}, children: [content]}
    source = %{type: :text, text: "Legacy"}
    option = %{type: :element, tag: "option", attributes: %{}, children: [source]}
    select = %{type: :element, tag: "select", attributes: %{}, children: [button, option]}
    assert {:ok, projection} = HtmlSelectedContent.compose(%{type: :document, children: [select]})

    assert :ok =
             NativeElixirPdfUtilities.Validators.HtmlTreeValidator.validate_transport(
               projection.composed
             )

    clone = hd(hd(hd(hd(projection.composed.children).children).children).children)
    assert clone.text == "Legacy"
    refute Map.has_key?(clone, :document_id)
    refute Map.has_key?(clone, :node_id)

    assert {:error, {:invalid_document, diagnostic}} =
             NativeElixirPdfUtilities.Validators.HtmlSelectedContentValidator.validate_clone_origin(
               %{clone | text: "Changed"},
               source
             )

    assert diagnostic.operation == :clone_selected_content
    assert diagnostic.stage == :html and diagnostic.reason == :invalid_document
    assert diagnostic.module == NativeElixirPdfUtilities.Validators.HtmlSelectedContentValidator
    assert diagnostic.message =~ "exact original"
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
