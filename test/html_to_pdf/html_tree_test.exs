defmodule NativeElixirPdfUtilities.HtmlToPdf.HtmlTreeTest do
  use ExUnit.Case

  alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, HtmlTree, Style}
  alias NativeElixirPdfUtilities.Validators.{HtmlTreeValidator, HtmlValidator}
  alias NativeElixirPdfUtilities.Limits
  import NativeElixirPdfUtilities.HtmlTreeHelpers

  test "closed earlier nodes and insertion locations remain live while a later subtree is open" do
    source = %{text: "", line: 1, column: 1}
    tree = HtmlTree.new(source)

    node = fn tag ->
      %{type: :element, tag: tag, attributes: %{}, children: [], source: source}
    end

    {tree, html} = HtmlTree.insert(tree, tree.root, node.("html"))
    {tree, head} = HtmlTree.insert(tree, html, node.("head"))

    {tree, marker} =
      HtmlTree.insert(tree, head, %{
        type: :processing_instruction,
        target: "marker",
        data: "name=A",
        source: %{text: "<?marker name=A>", line: 1, column: 1}
      })

    {tree, _closed} = HtmlTree.close(tree, head)
    {tree, body} = HtmlTree.insert(tree, html, node.("body"))
    {tree, body_text} = HtmlTree.insert(tree, body, %{type: :text, text: "Body", source: source})
    {tree, _cached} = HtmlTree.close(tree, tree.root)
    {tree, inserted} = HtmlTree.insert(tree, head, node.("meta"), marker)
    assert [meta, pi] = HtmlTree.children(tree, head)
    assert meta.node_id == inserted and pi.node_id == marker
    assert meta.parent_id == head and meta.tree_scope_id == tree.root
    assert HtmlTree.fetch(tree, body_text).parent_id == body
    document = HtmlTree.fetch(tree, tree.root)
    assert [html_node] = document.children

    assert [
             %{children: [%{node_id: ^inserted}, %{node_id: ^marker}]},
             %{children: [%{text: "Body"}]}
           ] = html_node.children

    assert :ok = HtmlTreeValidator.validate_transport(document)
    refute Map.has_key?(meta, :projection)
  end

  test "fragment addresses invalidate closed owner projections and keep distinct scopes" do
    source = %{text: "<template>", line: 1, column: 1}
    tree = HtmlTree.new(source)

    {tree, template} =
      HtmlTree.insert(tree, tree.root, %{
        type: :element,
        tag: "template",
        attributes: %{},
        children: [],
        source: source
      })

    {tree, fragment} = HtmlTree.open_fragment(tree, template, source)
    {tree, _closed} = HtmlTree.close(tree, fragment)
    {tree, _closed} = HtmlTree.close(tree, template)
    {tree, _closed} = HtmlTree.close(tree, tree.root)

    {tree, child} =
      HtmlTree.insert(tree, fragment, %{
        type: :text,
        text: "Inert",
        source: %{text: "Inert", line: 1, column: 11}
      })

    assert [%{content: content, children: []}] = HtmlTree.children(tree, tree.root)
    assert content.node_id == fragment and content.tree_scope_id == fragment
    assert content.parent_id == nil
    assert [%{node_id: ^child, parent_id: ^fragment, tree_scope_id: ^fragment}] = content.children
    assert content.document_id == HtmlTree.fetch(tree, tree.root).document_id
    assert :ok = HtmlTreeValidator.validate_transport(HtmlTree.fetch(tree, tree.root))
  end

  test "normalization reparents existing roots without copying completed descendants" do
    assert {:ok, document} =
             HtmlParser.parse_detailed(
               "<div><p>A</p><p>B</p></div><template><p>Inert</p></template>"
             )

    tree = HtmlTree.from_document(document)
    [div, template] = HtmlTree.children(tree, tree.root)
    [first, second] = div.children
    group = %{type: :element, tag: "section", attributes: %{}, children: [first, second]}
    tree = HtmlTree.replace_children(tree, div.node_id, [group])
    assert [%{children: [section]}, preserved] = HtmlTree.children(tree, tree.root)
    assert preserved == template
    assert [new_first, new_second] = section.children
    assert new_first.node_id == first.node_id and new_second.node_id == second.node_id
    assert new_first.parent_id == section.node_id
    assert new_first.children == first.children
    assert section.source == %{text: "", line: 1, column: 1}
    assert :ok = HtmlTreeValidator.validate_transport(HtmlTree.fetch(tree, tree.root))
  end

  test "uncached implied groups still invalidate a cached ancestor with bounded accounting" do
    assert {:ok, document} = HtmlParser.parse_detailed("<div><p>A</p></div>")
    tree = HtmlTree.from_document(document)
    [div] = HtmlTree.children(tree, tree.root)
    group = %{type: :element, tag: "section", attributes: %{}, children: div.children}
    tree = HtmlTree.replace_children(tree, div.node_id, [group])
    [group] = HtmlTree.children(tree, div.node_id)
    {tree, _closed} = HtmlTree.close(tree, tree.root)
    refute Map.has_key?(tree.nodes[group.node_id], :projection)
    refute Map.has_key?(tree.nodes[div.node_id], :projection)
    assert Map.has_key?(tree.nodes[tree.root], :projection)
    new = %{type: :text, text: "Inserted", source: %{text: "", line: 1, column: 1}}
    {changed, id} = HtmlTree.insert(tree, group.node_id, new)

    assert [%{children: [%{children: [_, %{node_id: ^id}]}]}] =
             HtmlTree.children(changed, changed.root)

    assert :ok = HtmlTreeValidator.validate_transport(HtmlTree.fetch(changed, changed.root))
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    Limits.install(%{limits | max_html_tree_work: 2})

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             HtmlValidator.with_render_budget(fn -> HtmlTree.insert(tree, group.node_id, new) end)

    assert diagnostic.stage == :html and diagnostic.message =~ "max_html_tree_work"
  end

  test "all authored nodes retain original source spans and text token boundaries" do
    html =
      "<div>é\u0301&amp;<!-- split -->\r\nB<?marker>中</div><template><pre>\r\nFirst</pre></template>"

    assert {:ok, document} = HtmlParser.parse_detailed(html)
    assert document.source == %{text: html, line: 1, column: 1}
    assert [div, template] = document.children
    assert div.source == %{text: "<div>", line: 1, column: 1}
    assert [first, second, pi, last] = div.children

    assert first.text == "é\u0301&" and
             first.source == %{text: "é\u0301&amp;", line: 1, column: 6}

    assert second.text == "\nB" and second.source == %{text: "\r\nB", line: 1, column: 26}
    assert pi.source == %{text: "<?marker>", line: 2, column: 2}
    assert last.source == %{text: "中", line: 2, column: 11}
    assert template.content.parent_id == nil
    assert [pre] = template.content.children
    assert [%{text: "First", source: %{text: "\r\nFirst", line: 2}}] = pre.children
    assert :ok = HtmlTreeValidator.validate_transport(document)
    assert {:ok, other} = HtmlParser.parse_detailed(html)
    refute document.document_id == other.document_id
    refute div.node_id == hd(other.children).node_id
    assert tree_content(document) == tree_content(other)

    assert {:ok, %{children: [svg]}} =
             HtmlParser.parse_detailed("<svg><![CDATA[<x>]]><?marker></svg>")

    assert [%{type: :text, text: "<x>", source: %{text: "<![CDATA[<x>]]>"}}, %{target: "marker"}] =
             svg.children

    assert {:ok, %{children: [%{text: "A&B", source: %{text: "A&amp;B"}}]}} =
             HtmlParser.parse_fragment("A&amp;B", "textarea")

    assert {:ok, %{children: []}} = HtmlParser.parse_fragment("", "plaintext")
  end

  test "implicit document and table nodes have unique identities and consistent parents" do
    assert {:ok, document} =
             HtmlParser.parse_document_detailed(
               "<title>T</title><table><tr><td>A</td></tr></table>"
             )

    assert [html] = document.children
    assert [head, body] = html.children
    assert html.source.text == "" and head.source.text == "" and body.source.text == ""
    assert head.parent_id == html.node_id and body.parent_id == html.node_id
    assert [%{children: [group]}] = body.children
    assert group.tag == "tbody" and group.source.text == ""
    assert :ok = HtmlTreeValidator.validate_transport(document)
    assert {:ok, _styled} = Style.compute_detailed(document)
  end

  test "canonical transport rejects mixed duplicate and mismatched identities before selection" do
    assert {:ok, document} =
             HtmlParser.parse_detailed(
               "<div><p>A</p><p>B</p><template><p>Inert</p></template></div>"
             )

    [div] = document.children
    [first, second, template] = div.children

    for forged <- [
          Map.delete(first, :document_id),
          Map.put(first, :node_id, second.node_id),
          Map.put(first, :document_id, make_ref()),
          Map.put(first, :tree_scope_id, make_ref()),
          Map.put(first, :parent_id, document.node_id),
          Map.put(first, :node_id, "caller-id"),
          Map.put(first, :projection, first),
          Map.put(first, :source, %{text: <<255>>, line: 1, column: 1}),
          Map.put(first, :source, %{text: "<p>", line: 0, column: 1}),
          Map.delete(first, :source),
          tree_content(first)
        ] do
      invalid = %{document | children: [%{div | children: [forged, second, template]}]}
      assert {:error, {:invalid_document, diagnostic}} = Style.compute_detailed(invalid)
      assert diagnostic.stage == :style and diagnostic.operation == :compute
      assert diagnostic.message =~ "identity"
      assert diagnostic.module == HtmlTreeValidator
    end

    invalid_fragment = %{template.content | tree_scope_id: document.tree_scope_id}

    invalid = %{
      document
      | children: [%{div | children: [first, second, %{template | content: invalid_fragment}]}]
    }

    assert {:error, {:invalid_document, _}} = Style.compute_detailed(invalid)
    legacy = tree_content(document)
    assert :ok = HtmlTreeValidator.validate_transport(legacy)
    assert {:ok, _} = Style.compute_detailed(legacy)
    mixed = %{legacy | children: [div]}
    assert {:error, {:invalid_document, _}} = Style.compute_detailed(mixed)
    assert {:error, {:invalid_document, _}} = HtmlTreeValidator.validate_node_metadata(nil)

    assert {:error, {:invalid_document, _}} =
             HtmlTreeValidator.validate_node_metadata(%{source: :forged})

    assert :ok = HtmlTreeValidator.validate_transport(nil)
  end

  test "legacy source metadata remains independent while duplicate PI identities and mixed nodes diagnose" do
    source = %{text: "<p>", line: 1, column: 1}

    legacy = %{
      type: :document,
      children: [
        %{
          type: :element,
          tag: "p",
          attributes: %{},
          children: [%{type: :text, text: "Text"}],
          source: source
        }
      ]
    }

    assert :ok = HtmlTreeValidator.validate_transport(legacy)
    assert {:ok, _} = Style.compute_detailed(legacy)
    assert {:ok, pi} = HtmlValidator.validate_html_processing_instruction("<?marker>")

    pi =
      pi
      |> Map.put(:node_id, make_ref())
      |> Map.put(:source, %{text: "<?marker>", line: 1, column: 1})

    assert {:ok, _} = Style.compute_detailed(%{type: :document, children: [pi]})

    assert {:error, {:invalid_document, diagnostic}} =
             Style.compute_detailed(%{type: :document, children: [pi, pi]})

    assert diagnostic.module == HtmlTreeValidator and diagnostic.source == "<?marker>"
    assert diagnostic.line == 1 and diagnostic.column == 1

    assert {:error, {:invalid_document, _}} =
             HtmlTreeValidator.validate_transport(%{
               type: :document,
               children: [%{type: :text, text: "Text", node_id: make_ref()}]
             })

    assert {:ok, canonical} = HtmlParser.parse_detailed("<p>A</p>")

    assert {:error, {:invalid_document, _}} =
             HtmlTreeValidator.validate_transport(%{canonical | children: [nil]})

    assert :ok = HtmlTreeValidator.validate_transport(%{type: :document, children: [nil]})

    assert {:error, {:invalid_document, diagnostic}} =
             HtmlTreeValidator.validate_node_metadata(%{
               source: %{text: <<255>>, line: 0, column: 2}
             })

    refute Map.has_key?(diagnostic, :line)
    assert diagnostic.column == 2 and String.valid?(diagnostic.source)

    assert {:error, {:invalid_document, %{source: ":forged"}}} =
             HtmlTreeValidator.validate_node_metadata(%{
               source: %{text: :forged, line: 1, column: 1}
             })
  end

  test "direct PI source consistency is bounded before UTF-8 and token scans" do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    source = String.duplicate(" ", 200)

    pi = %{
      type: :processing_instruction,
      target: "marker",
      data: "",
      source: %{text: "<?marker" <> source <> ">", line: 1, column: 1}
    }

    Limits.install(%{limits | max_html_tree_work: 100})

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             HtmlValidator.validate_html_processing_instruction(pi)

    assert diagnostic.stage == :html and diagnostic.message =~ "max_html_tree_work"
    Limits.install(%{limits | max_html_depth: 0})

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             HtmlTreeValidator.validate_transport(%{
               type: :document,
               children: [
                 %{
                   type: :element,
                   tag: "p",
                   attributes: %{},
                   children: [%{type: :text, text: "A"}]
                 }
               ]
             })

    assert diagnostic.message =~ "max_html_depth"
  end

  test "source capture and metadata consistency use cumulative bounded work on a long single line" do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)

    for count <- [500, 1000] do
      html = "<div>" <> String.duplicate("<p>é</p>", count) <> "</div>"
      Limits.install(%{limits | max_html_tree_work: 40 * byte_size(html)})
      assert {:ok, document} = HtmlParser.parse_detailed(html)
      assert :ok = HtmlTreeValidator.validate_transport(document)
    end

    assert {:ok, document} = HtmlParser.parse_detailed("<p>A</p>")
    source = String.duplicate("large source ", 100)
    document = put_in(document, [:children, Access.at(0), :source, :text], source)
    Limits.install(%{limits | max_html_tree_work: byte_size(source) - 1})
    assert {:error, {:resource_limit_exceeded, diagnostic}} = Style.compute_detailed(document)
    assert diagnostic.stage == :html and diagnostic.message =~ "max_html_tree_work"

    Limits.install(%{limits | max_html_tree_work: 1})
    assert {:error, {:resource_limit_exceeded, _}} = HtmlParser.parse_detailed("<p>A</p>")

    assert {:error, {:resource_limit_exceeded, _}} =
             HtmlValidator.with_render_budget(fn ->
               HtmlTreeValidator.validate_transport(document)
             end)
  end

  test "malformed metadata diagnostics bound source scanning before rejecting its shape" do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    Limits.install(%{limits | max_html_tree_work: 100})
    source = %{text: String.duplicate("source", 50), line: 0, column: 1}

    for node <- [
          %{type: :text, text: "Text", source: source},
          %{type: :text, text: "Text", document_id: make_ref(), source: source},
          %{source: %{source | text: <<255>> <> source.text}}
        ] do
      assert {:error, {:resource_limit_exceeded, diagnostic}} =
               HtmlTreeValidator.validate_node_metadata(node)

      assert diagnostic.stage == :html
      assert diagnostic.reason == :resource_limit_exceeded
      assert diagnostic.module == HtmlValidator
      assert diagnostic.message =~ "max_html_tree_work"
    end

    # Shape rejection skips normal source validation, but its diagnostic still
    # consumes the same shared work budget before examining the original bytes.
    node = %{source: %{text: "é", line: 0, column: 2}}
    Limits.install(%{limits | max_html_tree_work: 4})

    assert {:error, {:invalid_document, diagnostic}} =
             HtmlTreeValidator.validate_node_metadata(node)

    assert diagnostic.source == "é" and diagnostic.column == 2
    refute Map.has_key?(diagnostic, :line)

    assert {:error, {:resource_limit_exceeded, _}} =
             HtmlValidator.with_render_budget(fn ->
               HtmlValidator.reserve_render_resource(:max_html_tree_work, 1, :html)
               HtmlTreeValidator.validate_node_metadata(node)
             end)
  end
end
