defmodule NativeElixirPdfUtilities.HtmlToPdf.HtmlTreeTest do
  use ExUnit.Case

  alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, HtmlTree, Style}
  alias NativeElixirPdfUtilities.Validators.{HtmlTreeValidator, HtmlValidator}
  alias NativeElixirPdfUtilities.Limits
  import NativeElixirPdfUtilities.HtmlTreeHelpers

  test "canonical fragment insertion parents exist at each authored token" do
    :erlang.trace_pattern({HtmlTree, :insert, 4}, true, [:local])
    on_exit(fn -> :erlang.trace_pattern({HtmlTree, :insert, 4}, false, [:local]) end)

    source =
      "<?root><col><?columns><col></colgroup><tr><td>A</td></tr><?rows>" <>
        "<tr><td>B</td></tr><template><?inert><p>Future</p></template>"

    task =
      Task.async(fn ->
        receive do
          :parse -> HtmlParser.parse_fragment_tree_detailed(source, "TABLE")
        end
      end)

    :erlang.trace(task.pid, true, [:call])
    send(task.pid, :parse)
    assert {:ok, fragment} = Task.await(task)
    assert fragment.type == :document_fragment
    assert fragment.source == %{text: source, line: 1, column: 1}
    assert fragment.parent_id == nil and fragment.tree_scope_id == fragment.node_id
    assert :ok = HtmlTreeValidator.validate_transport(fragment)

    for {target, type, tag, earlier} <- [
          {"root", :document_fragment, nil, nil},
          {"columns", :element, "colgroup", "col"},
          {"rows", :element, "tbody", "tr"},
          {"inert", :document_fragment, nil, nil}
        ] do
      assert_receive {:trace, _, :call,
                      {HtmlTree, :insert,
                       [tree, parent, %{type: :processing_instruction, target: ^target}, nil]}}

      owner = tree.nodes[parent]
      assert owner.type == type and Map.get(owner, :tag) == tag
      assert owner.document_id == fragment.document_id
      assert tree.nodes[tree.root].type == :document_fragment

      if tag do
        assert owner.source == %{text: "", line: 1, column: 1}
        assert owner.parent_id == fragment.node_id
        assert owner.tree_scope_id == fragment.node_id
        assert Enum.any?(HtmlTree.children(tree, parent), &(Map.get(&1, :tag) == earlier))
      end

      refute Enum.any?(tree.nodes, fn {_id, node} -> Map.get(node, :text) == "Future" end)
    end

    [root, columns, rows] = fragment.children
    assert root.target == "root"
    assert columns.tag == "colgroup" and rows.tag == "tbody"
    [_, marker, _, template] = rows.children
    assert marker.target == "rows"
    assert template.content.tree_scope_id != fragment.tree_scope_id
    assert template.content.document_id == fragment.document_id
    assert template.content.parent_id == nil
  end

  test "canonical fragments preserve original source through nested table modes" do
    source = "<table><col><tr><td>e\u0301&amp;é\r\n<?marker name='A'?></table>"
    assert {:ok, fragment} = HtmlParser.parse_fragment_tree_detailed(source, "div")
    assert :ok = HtmlTreeValidator.validate_transport(fragment)
    [table] = fragment.children
    [columns, rows] = table.children
    [row] = rows.children
    [cell] = row.children
    [text, marker] = cell.children
    assert Enum.map(table.children, & &1.tag) == ["colgroup", "tbody"]
    assert columns.source.text == "" and rows.source.text == ""
    assert table.source == %{text: "<table>", line: 1, column: 1}
    assert text.source == %{text: "e\u0301&amp;é\r\n", line: 1, column: 21}
    assert text.text == "e\u0301&é\n"
    assert marker.source == %{text: "<?marker name='A'?>", line: 2, column: 1}
    assert marker.parent_id == cell.node_id
    assert marker.tree_scope_id == fragment.node_id

    assert {:ok, legacy} = HtmlParser.parse_fragment_detailed(source, "div")
    assert legacy.type == :document
    assert Enum.map(hd(legacy.children).children, & &1.tag) == ["col", "tr"]
    assert {:ok, legacy} = HtmlParser.parse_fragment("<tr><td>A", "table")
    assert legacy.type == :document and hd(legacy.children).tag == "tr"
  end

  test "canonical fragment roots preserve tokenizer and foreign integration contexts" do
    for {context, source, expected} <- [
          {"tr", "<td>A<td>B", ["td", "td"]},
          {"select", "<option>A<option>B", ["option", "option"]},
          {"template", "<tr><td>A</td></tr>", ["tr"]},
          {"svg", "<path/>", ["path"]},
          {"math", "<mi>x</mi>", ["mi"]},
          {%{namespace: :svg, tag: "foreignObject"}, "<p>HTML</p>", ["p"]}
        ] do
      assert {:ok, fragment} = HtmlParser.parse_fragment_tree_detailed(source, context)
      assert :ok = HtmlTreeValidator.validate_transport(fragment)
      assert Enum.map(fragment.children, & &1.tag) == expected
      assert Enum.all?(fragment.children, &(&1.parent_id == fragment.node_id))
    end

    for {context, expected} <- [
          {"textarea", "A&B\n"},
          {"style", "A&amp;B\n"},
          {"plaintext", "A&amp;B\n"}
        ] do
      assert {:ok, fragment} = HtmlParser.parse_fragment_tree_detailed("A&amp;B\r\n", context)
      assert [%{text: ^expected, parent_id: parent}] = fragment.children
      assert parent == fragment.node_id
      assert fragment.type == :document_fragment
      assert :ok = HtmlTreeValidator.validate_transport(fragment)

      assert {:ok, %{type: :document_fragment, children: []}} =
               HtmlParser.parse_fragment_tree_detailed("", context)
    end

    assert {:ok, %{type: :document_fragment, children: []}} =
             HtmlParser.parse_fragment_tree_detailed("", "table")
  end

  test "canonical fragment failures keep strict validators and original diagnostics" do
    assert {:error, {:unsupported_html, diagnostic}} =
             HtmlParser.parse_fragment_tree_detailed("", "html")

    assert diagnostic.stage == :html and diagnostic.message =~ "fragment context"

    assert {:error, {:invalid_encoding, diagnostic}} =
             HtmlParser.parse_fragment_tree_detailed(<<255>>, "div")

    assert diagnostic.reason == :invalid_encoding and is_binary(diagnostic.message)

    for source <- [
          "<li>A</li>",
          "<tr><td><script type=module></script></td></tr>",
          "<tr><td><a href='javascript:alert(1)'>A</a></td></tr>",
          "<tr><td><template for='A'><p>Replacement</p></template></td></tr>"
        ] do
      assert {:error, {:unsupported_html, diagnostic}} =
               HtmlParser.parse_fragment_tree_detailed(source, "table")

      assert diagnostic.stage == :html
      assert diagnostic.reason == :unsupported_html
      assert is_binary(diagnostic.source) and is_binary(diagnostic.message)
      assert diagnostic.line == 1 and diagnostic.column > 0
    end

    assert {:error, {:unsupported_html, diagnostic}} =
             HtmlParser.parse_fragment_tree_detailed("<tr><td>é\r\n<script type=module>", "table")

    assert diagnostic.source == "<script type=module>"
    assert diagnostic.line == 2 and diagnostic.column == 1
  end

  test "standalone fragment transport validates scopes parents and unique identities" do
    assert {:ok, fragment} =
             HtmlParser.parse_fragment_tree_detailed(
               "<?marker><template><p>A</p></template>",
               "div"
             )

    [marker, template] = fragment.children
    assert :ok = HtmlTreeValidator.validate_transport(fragment)

    for invalid <- [
          %{fragment | parent_id: make_ref()},
          %{fragment | children: [marker, marker]},
          %{fragment | children: [%{marker | parent_id: make_ref()}, template]},
          %{fragment | children: [%{marker | tree_scope_id: make_ref()}, template]},
          %{fragment | children: [marker, %{template | document_id: make_ref()}]},
          %{fragment | children: [marker, Map.delete(template, :document_id)]},
          Map.put(fragment, :projection, fragment),
          %{fragment | source: %{text: <<255>>, line: 1, column: 1}}
        ] do
      assert {:error, {:invalid_document, diagnostic}} =
               HtmlTreeValidator.validate_transport(invalid)

      assert diagnostic.stage == :style and diagnostic.reason == :invalid_document
      assert is_binary(diagnostic.message)
    end
  end

  test "canonical implied fragment parents consume shared node depth and work budgets" do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    Limits.install(%{limits | max_html_nodes: 5})
    assert {:ok, _fragment} = HtmlParser.parse_fragment_tree_detailed("<tr><td>A", "table")
    Limits.install(%{limits | max_html_nodes: 3})

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             HtmlParser.parse_fragment_tree_detailed("<tr><td>A", "table")

    assert diagnostic.stage == :html and diagnostic.message =~ "max_html_nodes"
    Limits.install(%{limits | max_html_depth: 2})

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             HtmlParser.parse_fragment_tree_detailed("<tr><td>A", "table")

    assert diagnostic.stage == :html and diagnostic.message =~ "max_html_depth"
    Limits.install(%{limits | max_html_tree_work: 1})

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             HtmlParser.parse_fragment_tree_detailed("<?marker>", "div")

    assert diagnostic.stage == :html and diagnostic.message =~ "max_html_tree_work"
  end

  test "implied insertion parents already own earlier nodes at the next authored token" do
    # Trace only this parsing task. A completed-DOM reconstruction cannot satisfy
    # the snapshots captured at the PI insertion calls.
    :erlang.trace_pattern({HtmlTree, :insert, 4}, true, [:local])
    on_exit(fn -> :erlang.trace_pattern({HtmlTree, :insert, 4}, false, [:local]) end)

    input =
      "<?document><title>Title</title><?head></head><?between><p>Body</p>" <>
        "<table><col><?columns><col></colgroup><tr><td>A</td></tr><?rows>" <>
        "<tr><td>B</td></tr></table><template><?inert><p>Future</p></template>"

    task =
      Task.async(fn ->
        receive do
          :parse -> HtmlParser.parse_document_detailed(input)
        end
      end)

    :erlang.trace(task.pid, true, [:call])
    send(task.pid, :parse)
    assert {:ok, document} = Task.await(task)
    assert :ok = HtmlTreeValidator.validate_transport(document)

    for {target, expected_type, expected_tag, earlier} <- [
          {"document", :document, nil, nil},
          {"head", :element, "head", "title"},
          {"between", :element, "html", "head"},
          {"columns", :element, "colgroup", "col"},
          {"rows", :element, "tbody", "tr"},
          {"inert", :document_fragment, nil, nil}
        ] do
      assert_receive {:trace, _, :call,
                      {HtmlTree, :insert,
                       [tree, parent, %{type: :processing_instruction, target: ^target}, nil]}}

      owner = Map.fetch!(tree.nodes, parent)
      assert owner.type == expected_type
      assert Map.get(owner, :tag) == expected_tag

      if expected_type == :element do
        assert owner.source == %{text: "", line: 1, column: 1}
        assert tree.nodes[owner.parent_id].node_id == owner.parent_id
      end

      if earlier do
        assert Enum.any?(HtmlTree.children(tree, parent), &(Map.get(&1, :tag) == earlier))
      end

      refute Enum.any?(tree.nodes, fn {_id, node} -> Map.get(node, :text) == "Future" end)
    end

    [_, html] = document.children
    assert [head, _, body] = html.children
    assert head.tag == "head" and body.tag == "body"
    assert [_, table, template] = body.children
    assert Enum.map(table.children, & &1.tag) == ["colgroup", "tbody"]
    assert template.content.tree_scope_id != body.tree_scope_id
  end

  test "authored tokens retain original sources inside token-time implied parents" do
    source = "\uFEFF\r\n<title>T</title>\r<p>e\u0301&amp;é<table><col><tr><td>A</table>"
    assert {:ok, document} = HtmlParser.parse_document_detailed(source)
    assert :ok = HtmlTreeValidator.validate_transport(document)
    assert document.source.text == source
    [html] = document.children
    [head, body] = html.children
    [title] = head.children
    [paragraph, table] = body.children
    [columns, rows] = table.children

    for node <- [html, head, body, columns, rows] do
      assert node.source == %{text: "", line: 1, column: 1}
      assert node.document_id == document.document_id
      assert node.tree_scope_id == document.tree_scope_id
    end

    assert title.source == %{text: "<title>", line: 2, column: 1}
    assert paragraph.source == %{text: "<p>", line: 3, column: 1}
    assert hd(paragraph.children).source == %{text: "e\u0301&amp;é", line: 3, column: 4}
    assert table.source == %{text: "<table>", line: 3, column: 11}
    assert hd(columns.children).parent_id == columns.node_id
    assert hd(rows.children).parent_id == rows.node_id

    assert {:ok, legacy} = HtmlParser.parse_detailed(source)
    assert Enum.map(legacy.children, & &1.tag) == ["title", "p", "table"]
    assert Enum.map(List.last(legacy.children).children, & &1.tag) == ["col", "tr"]
    assert {:ok, fragment} = HtmlParser.parse_fragment("<tr><td>A", "table")
    assert hd(fragment.children).tag == "tr"
  end

  test "implied parents reserve node and depth budgets before their authored children" do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)

    Limits.install(%{limits | max_html_nodes: 3})
    assert {:ok, _empty_document} = HtmlParser.parse_document_detailed("")

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             HtmlParser.parse_document_detailed("<p>A</p>")

    assert diagnostic.stage == :html
    assert diagnostic.reason == :resource_limit_exceeded
    assert diagnostic.message =~ "max_html_nodes"

    Limits.install(%{limits | max_html_depth: 2})
    assert {:ok, _document} = HtmlParser.parse_document_detailed("")

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             HtmlParser.parse_document_detailed("<p>A</p>")

    assert diagnostic.message =~ "max_html_depth"

    Limits.install(%{limits | max_html_nodes: 9})
    assert {:ok, _document} = HtmlParser.parse_document_detailed("<table><tr><td>A</table>")
    Limits.install(%{limits | max_html_nodes: 8})

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             HtmlParser.parse_document_detailed("<table><tr><td>A</table>")

    assert diagnostic.message =~ "max_html_nodes"
  end

  test "legacy document construction and live reordering preserve authored template addresses" do
    input = "<?before><title>T</title><template><p>Inert</p></template><p>Body</p>"
    assert {:ok, legacy} = HtmlParser.parse_detailed(input)
    assert {:ok, constructed} = HtmlValidator.construct_html_document(legacy)
    assert :ok = HtmlValidator.validate_constructed_html_document(constructed)

    assert {:error, {:unsupported_html, diagnostic}} =
             HtmlValidator.validate_constructed_html_document(legacy)

    assert diagnostic.stage == :html and diagnostic.message =~ "document"

    assert {:error, {:unsupported_html, diagnostic}} =
             HtmlValidator.validate_constructed_html_document(%{
               type: :document,
               children: [
                 %{type: :element, tag: "html", children: []},
                 %{type: :text, text: "Bad"}
               ]
             })

    assert diagnostic.reason == :unsupported_html and diagnostic.message =~ "document"

    tree = HtmlTree.from_document(constructed)
    [before, html] = HtmlTree.children(tree, tree.root)
    [head, body] = html.children
    [title, template] = head.children
    {tree, _cached} = HtmlTree.close(tree, tree.root)
    tree = HtmlTree.replace_children(tree, head.node_id, [template, title])
    [reordered, _title] = HtmlTree.children(tree, head.node_id)
    assert reordered.node_id == template.node_id
    assert reordered.content.node_id == template.content.node_id
    assert reordered.content.children == template.content.children
    assert before.node_id == hd(legacy.children).node_id
    assert [_, %{children: [_, final_body]}] = HtmlTree.fetch(tree, tree.root).children
    assert final_body.children == body.children
    assert :ok = HtmlTreeValidator.validate_transport(HtmlTree.fetch(tree, tree.root))
  end

  test "legacy table grouping preserves marker order and independently authored groups" do
    input =
      "<table><colgroup span=1></colgroup><col><?columns><template></template><col>" <>
        "<tr><td>A</td></tr><?rows><script type=application/json>{}</script>" <>
        "<template></template><tr><td>B</td></tr><tfoot><tr><td>C</td></tr></tfoot></table>"

    assert {:ok, %{children: [table]}} = HtmlParser.parse_detailed(input)
    groups = HtmlValidator.construct_html_table_groups(table.children)
    assert [authored, columns, rows, footer] = groups
    assert authored == hd(table.children)

    assert Enum.map(columns.children, &Map.get(&1, :tag, Map.get(&1, :target))) ==
             ["col", "columns", "template", "col"]

    assert Enum.map(rows.children, &Map.get(&1, :tag, Map.get(&1, :target))) ==
             ["tr", "rows", "script", "template", "tr"]

    assert footer == List.last(table.children)
    assert List.last(rows.children).node_id == Enum.at(table.children, -2).node_id
  end

  test "insertion-mode classification and preceding-node scans share the cumulative work budget" do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    token = "<TITLE>"
    Limits.install(%{limits | max_html_tree_work: 2 * byte_size(token)})

    assert {:imply, "html"} =
             HtmlValidator.with_render_budget(fn ->
               HtmlValidator.html_insertion_action(:document, token, nil, false)
             end)

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             HtmlValidator.with_render_budget(fn ->
               HtmlValidator.reserve_render_resource(:max_html_tree_work, 1, :html)
               HtmlValidator.html_insertion_action(:document, token, nil, false)
             end)

    assert diagnostic.stage == :html and diagnostic.message =~ "max_html_tree_work"

    source = %{text: "", line: 1, column: 1}
    tree = HtmlTree.new(source)

    tree =
      Enum.reduce(1..3, tree, fn _, tree ->
        {tree, _id} =
          HtmlTree.insert(tree, tree.root, %{
            type: :processing_instruction,
            target: "marker",
            data: "",
            source: source
          })

        tree
      end)

    Limits.install(%{limits | max_html_tree_work: 2})

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             HtmlValidator.with_render_budget(fn ->
               HtmlTree.last_element_tag(tree, tree.root)
             end)

    assert diagnostic.stage == :html and diagnostic.message =~ "max_html_tree_work"

    Limits.install(%{limits | max_html_tree_work: 3})

    assert HtmlValidator.with_render_budget(fn -> HtmlTree.last_element_tag(tree, tree.root) end) ==
             nil
  end

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
