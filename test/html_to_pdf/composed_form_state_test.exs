defmodule NativeElixirPdfUtilities.HtmlToPdf.ComposedFormStateTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlComposition, HtmlParser}
  alias NativeElixirPdfUtilities.Validators.{HtmlFormValidator, HtmlValidator}

  test "shadow controls have no implicit light form, label or fieldset ancestor" do
    nodes =
      prepared(
        "<form id=f><fieldset disabled><label id=l><span><template shadowrootmode=open><input id=shadow required><slot></slot></template><input id=light></span></label></fieldset></form>"
      )

    shadow = find(nodes, "shadow")
    light = find(nodes, "light")
    assert shadow._initial_form_state.owner == nil
    refute shadow._initial_form_state.disabled
    assert light._initial_form_state.owner == find(nodes, "f")._selector_id
    assert light._initial_form_state.disabled
    assert find(nodes, "l")._initial_label_control == light._selector_id
  end

  test "shadow fieldsets and labels do not adopt assigned light controls" do
    nodes =
      prepared(
        "<form id=f><div><template shadowrootmode=open><fieldset disabled><label id=shadowlabel><slot></slot><input id=shadow></label></fieldset></template><input id=light></div></form>"
      )

    refute find(nodes, "light")._initial_form_state.disabled
    assert find(nodes, "light")._initial_form_state.owner == find(nodes, "f")._selector_id
    assert find(nodes, "shadow")._initial_form_state.disabled
    assert find(nodes, "shadowlabel")._initial_label_control == find(nodes, "shadow")._selector_id
  end

  test "unassigned light radio peers still choose the initial winner" do
    nodes =
      prepared(
        "<div><template shadowrootmode=open><slot name=visible></slot></template><input id=visible slot=visible type=radio name=g checked><input id=unassigned type=radio name=g checked></div>"
      )

    refute find(nodes, "visible")._initial_form_state.checked
    assert find(nodes, "unassigned") == nil
  end

  test "logical ID associations include unassigned nodes and remain scope-specific" do
    nodes =
      prepared(
        "<div><template shadowrootmode=open><label id=shadowlabel for=hidden>Shadow</label><output id=shadowoutput for=hidden></output><slot name=visible></slot></template><input id=hidden><label id=lightlabel slot=visible for=hidden>Light</label><output id=lightoutput slot=visible for=hidden></output></div>"
      )

    assert find(nodes, "shadowlabel")._initial_label_control == nil
    assert hd(find(nodes, "shadowoutput")._initial_form_state.sources).ref == nil
    assert is_reference(find(nodes, "lightlabel")._initial_label_control)

    assert hd(find(nodes, "lightoutput")._initial_form_state.sources).ref ==
             find(nodes, "lightlabel")._initial_label_control
  end

  test "updated clone destination registry preserves real logical form ancestry" do
    assert {:ok, dom} =
             HtmlParser.parse_detailed(
               "<form id=f><selectedcontent id=destination></selectedcontent><input id=original type=radio name=g checked></form>"
             )

    [form] = dom.children
    [destination, original] = form.children

    clone = %{
      original
      | node_id: make_ref(),
        parent_id: destination.node_id,
        attributes: %{"id" => "clone", "type" => "radio", "name" => "g", "checked" => ""}
    }

    destination = %{destination | children: [clone]}
    projection = [%{form | children: [destination, original]}]
    registry = %{destination.node_id => destination, clone.node_id => clone}

    nodes =
      HtmlValidator.with_render_budget(fn ->
        HtmlFormValidator.prepare_composed_initial_nodes(dom, %{}, projection, registry)
      end)

    assert find(nodes, "clone")._initial_form_state.owner == find(nodes, "f")._selector_id
    refute find(nodes, "clone")._initial_form_state.checked
    assert find(nodes, "original")._initial_form_state.checked
  end

  test "composed inertness survives transfer of logical facts" do
    nodes =
      prepared(
        "<div><template shadowrootmode=open><div inert><slot></slot></div></template><input id=light></div>"
      )

    assert find(nodes, "light")._html_inert
    refute find(nodes, "light")._initial_form_state.disabled
  end

  test "logical forest traversal consumes the existing cumulative tree budget" do
    assert {:ok, dom} =
             HtmlParser.parse_detailed(
               "<div><template shadowrootmode=open><input></template></div>"
             )

    assert {:ok, composition} = HtmlComposition.compose(dom)
    previous = NativeElixirPdfUtilities.Limits.effective()
    on_exit(fn -> NativeElixirPdfUtilities.Limits.install(previous) end)
    NativeElixirPdfUtilities.Limits.install(%{previous | max_html_tree_work: 1})

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             HtmlValidator.with_render_budget(fn ->
               HtmlFormValidator.prepare_composed_initial_nodes(
                 dom,
                 composition.roots,
                 composition.composed.children,
                 %{}
               )
             end)

    assert diagnostic.stage == :html
    assert diagnostic.message =~ "max_html_tree_work"
  end

  defp prepared(html) do
    assert {:ok, dom} = HtmlParser.parse_detailed(html)
    assert {:ok, composition} = HtmlComposition.compose(dom)

    HtmlValidator.with_render_budget(fn ->
      HtmlFormValidator.prepare_composed_initial_nodes(
        dom,
        composition.roots,
        composition.composed.children,
        %{}
      )
    end)
  end

  defp find(nodes, id) do
    Enum.find_value(nodes, fn node ->
      if Map.get(node, :attributes, %{})["id"] == id,
        do: node,
        else: find(Map.get(node, :children, []), id)
    end)
  end
end
