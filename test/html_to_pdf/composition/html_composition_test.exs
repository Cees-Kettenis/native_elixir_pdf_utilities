defmodule NativeElixirPdfUtilities.HtmlToPdf.HtmlCompositionTest do
  use ExUnit.Case, async: true

  alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlComposition, HtmlParser, Style}

  test "named and default slots retain authored identities while composing first-slot assignment" do
    html =
      "<div><template shadowrootmode=open><slot name=title>Fallback</slot><slot>Empty</slot><slot name=title>Second</slot></template><span slot=title>Heading</span>Body<!--excluded--><b slot=missing>Hidden</b></div>"

    assert {:ok, dom} = HtmlParser.parse_detailed(html)
    [logical] = dom.children
    [template, heading, body, hidden] = logical.children
    assert {:ok, projection} = HtmlComposition.compose(dom)
    assert projection.logical == dom
    [host] = projection.composed.children
    [title_slot, default_slot, duplicate_slot] = host.children
    assert hd(title_slot.children).node_id == heading.node_id
    assert hd(title_slot.children).parent_id == logical.node_id
    assert hd(title_slot.children).composed_parent_id == title_slot.node_id
    assert default_slot.children |> hd() |> Map.fetch!(:node_id) == body.node_id
    assert [%{text: "Second"}] = duplicate_slot.children
    assert projection.assignments[duplicate_slot.node_id] == []
    assert projection.roots[template.content.node_id].mode == "open"
    refute Enum.any?(host.children, &(&1.node_id == hidden.node_id))
  end

  test "manual assignment uses fallback and invalid mode or invalid host retains an inert template" do
    for {html, roots} <- [
          {"<div><template shadowrootmode=closed shadowrootslotassignment=manual><slot>Fallback</slot></template>Light</div>",
           1},
          {"<div><template shadowrootmode=invalid><slot>Fallback</slot></template>Light</div>",
           0},
          {"<button><template shadowrootmode=open><slot>Fallback</slot></template>Light</button>",
           0}
        ] do
      assert {:ok, dom} = HtmlParser.parse_detailed(html)
      assert {:ok, projection} = HtmlComposition.compose(dom)
      assert map_size(projection.roots) == roots

      if roots == 1 do
        assert [%{children: [%{children: [%{text: "Fallback"}]}]}] = projection.composed.children
      else
        assert Enum.map(
                 hd(projection.composed.children).children,
                 &Map.delete(&1, :composed_parent_id)
               ) == hd(dom.children).children
      end
    end
  end

  test "nested hosts keep their own scopes and distributed slots do not claim outer light children" do
    assert {:ok, dom} =
             HtmlParser.parse_detailed(
               "<div><template shadowrootmode=open><section><template shadowrootmode=closed><slot>Inner fallback</slot></template>Inner light</section><slot>Outer fallback</slot></template>Outer light</div>"
             )

    assert {:ok, projection} = HtmlComposition.compose(dom)
    assert map_size(projection.roots) == 2
    [outer] = projection.composed.children
    [inner, outer_slot] = outer.children
    [inner_slot] = inner.children
    assert [%{text: "Inner light"}] = inner_slot.children
    assert [%{text: "Outer light"}] = outer_slot.children
    assert inner_slot.tree_scope_id != outer_slot.tree_scope_id
  end

  test "shadow styles are scoped and host/slotted selectors use original authored nodes" do
    html =
      "<style>span{color:red}</style><div class=host><template shadowrootmode=open><style>:host(.host){color:blue} span{font-weight:700} slot::slotted(.chosen){color:green!important}</style><span>Shadow</span><slot></slot></template><span class=chosen>Assigned</span></div><span>Outside</span>"

    assert {:ok, dom} = HtmlParser.parse_detailed(html)
    assert {:ok, styled} = Style.compute_detailed(dom, system_font_discovery: false)
    [host, outside] = styled.children
    [shadow, assigned] = host.children
    assert host.style.color == {0, 0, 1}
    assert shadow.style.color == {0, 0, 1}
    assert shadow.style.font_weight == 700
    assert assigned.style.color == {0, 0.5019607843, 0}
    assert assigned.style.font_weight == 400
    assert outside.style.color == {1, 0, 0}
  end

  test "ordinary shadow compounds do not match the host and prefixed slotted children use slot ancestry" do
    html =
      "<div><template shadowrootmode=open><style>div span{color:red} section > slot[name=a]::slotted(.chosen){font-weight:700}</style><span>Shadow</span><section><slot name=a></slot></section></template><span slot=a class=chosen>Assigned</span></div>"

    assert {:ok, dom} = HtmlParser.parse_detailed(html)
    assert {:ok, styled} = Style.compute_detailed(dom, system_font_discovery: false)
    [host] = styled.children
    [shadow, section] = host.children
    assert shadow.style.color == {0, 0, 0}
    assert hd(section.children).style.font_weight == 700
  end

  test "slotted positional compounds use light siblings including unassigned elements" do
    html =
      "<div><template shadowrootmode=open><style>::slotted(span:first-child){color:red} ::slotted(span:nth-child(2)){font-weight:700}</style><slot></slot></template><b slot=absent>Hidden light sibling</b><span>Assigned</span></div>"

    assert {:ok, dom} = HtmlParser.parse_detailed(html)
    assert {:ok, styled} = Style.compute_detailed(dom, system_font_discovery: false)
    [host] = styled.children
    [assigned] = host.children
    assert assigned.style.color == {0, 0, 0}
    assert assigned.style.font_weight == 700
  end

  test "fallback flow contents render only when no light child is assigned" do
    for {light, text} <- [{"", "Fallback flow"}, {"Assigned", "Assigned"}] do
      html =
        "<div><template shadowrootmode=open><slot><div>Fallback flow</div></slot></template>#{light}</div>"

      assert {:ok, dom} = HtmlParser.parse_detailed(html)
      assert {:ok, projection} = HtmlComposition.compose(dom)
      [host] = projection.composed.children
      [slot] = host.children

      assert NativeElixirPdfUtilities.Validators.HtmlValidator.descendant_text_content(
               slot.children
             ) == text
    end
  end

  test "unassigned light hosts still register roots for logical form and ID preparation" do
    html =
      "<div><template shadowrootmode=open><slot name=shown></slot></template><span slot=hidden><template shadowrootmode=closed><input id=hidden-control></template></span><span slot=shown>Visible</span></div>"

    assert {:ok, dom} = HtmlParser.parse_detailed(html)
    assert {:ok, projection} = HtmlComposition.compose(dom)
    assert map_size(projection.roots) == 2

    hidden_root =
      Enum.find_value(projection.roots, fn {_id, root} -> if root.mode == "closed", do: root end)

    assert [%{tag: "input", attributes: %{"id" => "hidden-control"}}] = hidden_root.children
    assert projection.logical == dom

    assert NativeElixirPdfUtilities.Validators.HtmlValidator.descendant_text_content(
             projection.composed.children
           ) == "Visible"
  end

  test "outer normal declarations beat shadow declarations and inner important declarations beat outer important" do
    for {outer, inner, expected} <- [
          {"span{color:red}", "slot::slotted(.chosen){color:green}", {1, 0, 0}},
          {"span{color:red!important}", "slot::slotted(.chosen){color:green!important}",
           {0, 0.5019607843, 0}}
        ] do
      assert {:ok, dom} =
               HtmlParser.parse_detailed(
                 "<style>#{outer}</style><div><template shadowrootmode=open><style>#{inner}</style><slot></slot></template><span class=chosen>Text</span></div>"
               )

      assert {:ok, styled} = Style.compute_detailed(dom, system_font_discovery: false)
      assert hd(hd(styled.children).children).style.color == expected
    end
  end

  test "ordinary documents reuse the original projection and malformed node maps diagnose" do
    assert {:ok, dom} = HtmlParser.parse_detailed("<p>Ordinary</p>")
    assert {:ok, %{composed: ^dom, logical: ^dom, roots: roots}} = HtmlComposition.compose(dom)
    assert roots == %{}

    assert {:error, {:invalid_document, %{message: message}}} =
             HtmlComposition.compose(%{type: :document, children: [nil]})

    assert message =~ "valid authored"
  end

  test "composition failures return the shared diagnostic contract" do
    assert {:error, {:invalid_document, diagnostic}} = HtmlComposition.compose(nil)
    assert diagnostic.stage == :html
    assert diagnostic.operation == :compose
    assert diagnostic.module == NativeElixirPdfUtilities.Validators.HtmlCompositionValidator
    assert diagnostic.message =~ "document"
  end
end
