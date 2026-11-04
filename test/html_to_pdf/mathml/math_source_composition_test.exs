defmodule NativeElixirPdfUtilities.HtmlToPdf.MathSourceCompositionTest do
  use ExUnit.Case, async: false

  alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, ResourceContext, Style}
  alias NativeElixirPdfUtilities.Validators.HtmlValidator

  test "parsed Math table row and cell background sources retain exact canonical identities" do
    html =
      "<math><mtable><mtr style='background:#ffeeaa'><mtd style='background:#ccddff'><mn>12</mn></mtd></mtr></mtable></math>"

    assert {:ok, dom} = HtmlParser.parse_detailed(html)
    [math] = dom.children
    [table] = math.children
    [row] = table.children
    [cell] = row.children

    assert {:ok, styled} =
             ResourceContext.run([], HtmlValidator.new_image_budget(), fn _ ->
               Style.compute_detailed(dom, system_font_discovery: false)
             end)

    [math] = styled.children

    sources =
      for item <- math.style._math_box.drawing_items,
          source = Map.get(Map.get(item, :style, %{}), :_math_source),
          is_map(source),
          do: source

    for original <- [row, cell] do
      source = Enum.find(sources, &(Map.get(&1, :node_id) == original.node_id))
      assert source != nil

      for key <- [:node_id, :parent_id, :document_id, :tree_scope_id, :source] do
        assert source[key] == original[key]
      end

      assert is_reference(source._selector_id)
    end
  end

  test "metadata-free Math backgrounds stay unbound to authored source identities" do
    token = %{
      type: :element,
      namespace: :mathml,
      tag: "mn",
      attributes: %{},
      children: [%{type: :text, text: "7"}]
    }

    cell = %{
      type: :element,
      namespace: :mathml,
      tag: "mtd",
      attributes: %{"style" => "background:lightblue"},
      children: [token]
    }

    row = %{
      type: :element,
      namespace: :mathml,
      tag: "mtr",
      attributes: %{"style" => "background:lightyellow"},
      children: [cell]
    }

    table = %{type: :element, namespace: :mathml, tag: "mtable", attributes: %{}, children: [row]}
    math = %{type: :element, namespace: :mathml, tag: "math", attributes: %{}, children: [table]}
    dom = %{type: :document, children: [math]}

    assert {:ok, styled} =
             ResourceContext.run([], HtmlValidator.new_image_budget(), fn _ ->
               Style.compute_detailed(dom, system_font_discovery: false)
             end)

    [math] = styled.children

    sources =
      for item <- math.style._math_box.drawing_items,
          source = Map.get(Map.get(item, :style, %{}), :_math_source),
          is_map(source),
          do: source

    assert Enum.sort(Enum.map(sources, & &1.tag)) == ["mtd", "mtr"]

    for source <- sources,
        key <- [:node_id, :parent_id, :document_id, :tree_scope_id, :source, :cloned_from_node_id] do
      refute Map.has_key?(source, key)
    end
  end

  test "selected-content Math clones carry their original row and cell origins" do
    html =
      "<style>select::picker-icon{display:none}</style><select style='appearance:base-select'><button><selectedcontent></selectedcontent></button><option><math><mtable><mtr style='background:#ffeeaa'><mtd style='background:#ccddff'><mn>34</mn></mtd></mtr></mtable></math></option></select>"

    assert {:ok, dom} = HtmlParser.parse_detailed(html)
    select = Enum.find(dom.children, &(Map.get(&1, :tag) == "select"))
    [_button, option] = select.children
    [math] = option.children
    [table] = math.children
    [row] = table.children
    [cell] = row.children

    assert {:ok, styled} =
             ResourceContext.run([], HtmlValidator.new_image_budget(), fn _ ->
               Style.compute_detailed(dom, system_font_discovery: false)
             end)

    [styled_select] = styled.children
    [content] = styled_select.children
    [math] = content.children
    assert math.cloned_from_node_id == hd(option.children).node_id
    refute math.node_id == math.cloned_from_node_id
    assert math.tree_scope_id == select.tree_scope_id

    sources =
      for item <- math.style._math_box.drawing_items,
          source = Map.get(Map.get(item, :style, %{}), :_math_source),
          is_map(source),
          do: source

    for original <- [row, cell] do
      source = Enum.find(sources, &(Map.get(&1, :cloned_from_node_id) == original.node_id))
      assert source != nil
      assert is_reference(source.node_id)
      assert source.node_id != original.node_id
      assert source.document_id == select.document_id
      assert source.tree_scope_id == select.tree_scope_id
      assert source.source == original.source
      assert is_reference(source.parent_id)
      refute Map.has_key?(source, :source_verified)
      refute Map.has_key?(source, :permission)
    end

    assert {:ok, pdf} =
             NativeElixirPdfUtilities.HtmlToPdf.render(html,
               forms: :static,
               system_font_discovery: false
             )

    assert String.starts_with?(pdf, "%PDF-")
  end

  test "an authored fragment link targets the native Math root containing a table" do
    html =
      "<a href='#equation'>Equation</a><math id=equation><mtable><mtr style='background:#ffeeaa'><mtd style='background:#ccddff'><mn>56</mn></mtd></mtr></mtable></math>"

    assert {:ok, dom} = HtmlParser.parse_detailed(html)
    [_, original] = dom.children

    assert {:ok, styled, issued_scope} =
             ResourceContext.run([], HtmlValidator.new_image_budget(), fn context ->
               with {:ok, styled} <- Style.compute_detailed(dom, system_font_discovery: false),
                    do: {:ok, styled, {context.document_id, context.tree_scope_id}}
             end)

    [_, math] = styled.children
    assert math.node_id == original.node_id
    assert math.source == original.source
    {issued_document, issued_tree} = issued_scope
    assert math.style.html_target == {issued_document, issued_tree, math._selector_id}

    assert {:ok, pdf} =
             NativeElixirPdfUtilities.HtmlToPdf.render(html, system_font_discovery: false)

    assert pdf =~ "/Annots"
    assert pdf =~ "/Dest"
    assert {:ok, text} = NativeElixirPdfUtilities.Text.extract(pdf)
    assert text =~ "56"
  end
end
