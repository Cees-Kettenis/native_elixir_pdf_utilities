defmodule NativeElixirPdfUtilities.Validators.TemplatePatchValidatorTest do
  use ExUnit.Case

  alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, HtmlTree, Style}
  alias NativeElixirPdfUtilities.Validators.{HtmlTreeValidator, HtmlValidator}
  alias NativeElixirPdfUtilities.Limits

  test "first tree-order marker uses exact XML name values without changing the tree" do
    source =
      "<?end name='A'><?other name='A'><?Marker name='A'>" <>
        "<?marker Name='A'><?marker name='A' name='B'><?marker name='B'>" <>
        "<div><?marker name='&#65;'></div><?marker name='A'>"

    assert {:ok, document} = HtmlParser.parse_detailed(source)
    tree = HtmlTree.from_document(document)
    [_, _, _, _, _, _, div, later] = document.children
    [first] = div.children
    assert {:ok, target} = HtmlTreeValidator.prepare_content_patch(tree, tree.root, "A")

    assert target == %{
             scope: tree.root,
             parent: div.node_id,
             start: first.node_id,
             end: first.node_id,
             removed: []
           }

    refute target.start == later.node_id
    assert HtmlTree.fetch(tree, tree.root) == document
    assert :ok = HtmlTreeValidator.validate_transport(document)

    for name <- ["", "a", "Missing"] do
      assert {:ok, nil} = HtmlTreeValidator.prepare_content_patch(tree, tree.root, name)
    end
  end

  test "body broadens to HTML so earlier head markers are addressable" do
    assert {:ok, document} =
             HtmlParser.parse_document_detailed(
               "<head><?marker name='head'></head><body><?marker name='body'></body>"
             )

    [html] = document.children
    [head, body] = html.children
    [marker] = head.children
    tree = HtmlTree.from_document(document)
    assert {:ok, target} = HtmlTreeValidator.prepare_content_patch(tree, body.node_id, "head")
    assert target.scope == html.node_id and target.parent == head.node_id
    assert target.start == marker.node_id
    assert {:ok, nil} = HtmlTreeValidator.prepare_content_patch(tree, head.node_id, "body")
  end

  test "inert template scopes exclude light descendants and other template contents" do
    source =
      "<?marker name='outer'><template><template><?marker name='nested'></template>" <>
        "<?marker name='inner'></template>"

    assert {:ok, fragment} = HtmlParser.parse_fragment_tree_detailed(source, "div")
    tree = HtmlTree.from_document(fragment)
    [_, template] = fragment.children
    [nested, inner] = template.content.children
    [nested_marker] = nested.content.children
    assert {:ok, nil} = HtmlTreeValidator.prepare_content_patch(tree, tree.root, "inner")
    assert {:ok, nil} = HtmlTreeValidator.prepare_content_patch(tree, template.node_id, "outer")
    assert {:ok, nil} = HtmlTreeValidator.prepare_content_patch(tree, template.node_id, "nested")

    assert {:ok, target} =
             HtmlTreeValidator.prepare_content_patch(tree, template.node_id, "inner")

    assert target.scope == template.content.node_id and target.parent == template.content.node_id
    assert target.start == inner.node_id
    assert {:ok, target} = HtmlTreeValidator.prepare_content_patch(tree, nested.node_id, "nested")
    assert target.start == nested_marker.node_id

    # A transient template's own empty content scope cannot search its intended
    # target tree. Its private stack state will not change this scope rule.
    {tree, transient} =
      HtmlTree.insert(tree, tree.root, %{
        type: :element,
        tag: "template",
        attributes: %{},
        children: [],
        source: %{text: "<template>", line: 1, column: 1}
      })

    {tree, _content} = HtmlTree.open_fragment(tree, transient, tree.nodes[transient].source)
    assert {:ok, nil} = HtmlTreeValidator.prepare_content_patch(tree, transient, "outer")
  end

  test "ranges count every sibling start but never descendant starts or ends" do
    source =
      "<div><?start name='A'><p>Old<?end></p><?start name='B'>" <>
        "<?start malformed><?end><?end><p>Last</p><?end><p>Keep</p></div>"

    assert {:ok, document} = HtmlParser.parse_detailed(source)
    [div] = document.children
    [start, old, nested, malformed, end1, end2, last, finish, keep] = div.children
    tree = HtmlTree.from_document(document)
    assert {:ok, target} = HtmlTreeValidator.prepare_content_patch(tree, tree.root, "A")
    assert target.start == start.node_id and target.end == finish.node_id
    assert target.parent == div.node_id
    assert target.removed == Enum.map([old, nested, malformed, end1, end2, last], & &1.node_id)
    refute keep.node_id in target.removed
    assert HtmlTree.fetch(tree, tree.root) == document
  end

  test "unmatched range reaches only its own parent's end and empty ranges stay empty" do
    for source <- [
          "<div><?start name='A'><p>Old</p><?start name='B'><?end></div><?end>",
          "<div><?start name='A'></div><?end>",
          "<div><?start name='A'><?end></div>"
        ] do
      assert {:ok, document} = HtmlParser.parse_detailed(source)
      [div | _outside] = document.children
      [start | rest] = div.children
      tree = HtmlTree.from_document(document)
      assert {:ok, target} = HtmlTreeValidator.prepare_content_patch(tree, div.node_id, "A")
      assert target.start == start.node_id and target.parent == div.node_id

      case rest do
        [%{target: "end"} = finish] ->
          assert target.end == finish.node_id and target.removed == []

        _ ->
          assert target.end == nil
          assert target.removed == Enum.map(rest, & &1.node_id)
      end
    end
  end

  test "live parser snapshots prove earlier ownership and absence of future candidates" do
    :erlang.trace_pattern({HtmlTree, :insert, 4}, true, [:local])
    on_exit(fn -> :erlang.trace_pattern({HtmlTree, :insert, 4}, false, [:local]) end)

    source =
      "<head><?marker name='head'></head><body><div><?start name='A'>" <>
        "<p>Old</p><?end></div><?boundary><?marker name='future'><p>Future</p></body>"

    task =
      Task.async(fn ->
        receive do
          :parse -> HtmlParser.parse_document_detailed(source)
        end
      end)

    :erlang.trace(task.pid, true, [:call])
    send(task.pid, :parse)
    assert {:ok, _document} = Task.await(task)

    assert_receive {:trace, _, :call,
                    {HtmlTree, :insert,
                     [tree, body, %{type: :processing_instruction, target: "boundary"}, nil]}}

    assert tree.nodes[body].tag == "body"
    assert {:ok, target} = HtmlTreeValidator.prepare_content_patch(tree, body, "head")
    assert tree.nodes[target.parent].tag == "head"
    assert {:ok, target} = HtmlTreeValidator.prepare_content_patch(tree, body, "A")
    [old] = target.removed
    assert tree.nodes[old].parent_id == target.parent
    assert HtmlTree.fetch(tree, old).children |> hd() |> Map.fetch!(:text) == "Old"
    assert {:ok, nil} = HtmlTreeValidator.prepare_content_patch(tree, body, "future")
    refute Enum.any?(tree.nodes, fn {_id, node} -> Map.get(node, :text) == "Future" end)
  end

  test "foreign elements named body or template do not acquire HTML scope rules" do
    tree = HtmlTree.new(%{text: "", line: 1, column: 1})
    {tree, parent} = HtmlTree.insert(tree, tree.root, %{type: :element, tag: "div", children: []})

    {tree, marker} =
      HtmlTree.insert(tree, parent, %{
        type: :processing_instruction,
        target: "marker",
        data: "name='A'",
        source: %{text: "<?marker name='A'>", line: 1, column: 1}
      })

    {tree, body} = HtmlTree.insert(tree, parent, %{type: :element, tag: "body", children: []})
    assert {:ok, nil} = HtmlTreeValidator.prepare_content_patch(tree, body, "A")
    assert {:ok, target} = HtmlTreeValidator.prepare_content_patch(tree, parent, "A")
    assert target.start == marker

    for tag <- ["body", "template"] do
      {foreign, id} =
        HtmlTree.insert(tree, parent, %{type: :element, tag: tag, namespace: :svg, children: []})

      assert {:ok, nil} = HtmlTreeValidator.prepare_content_patch(foreign, id, "A")
    end
  end

  test "marker getter failures keep shared diagnostics and reject caller patch payloads" do
    assert {:ok, document} = HtmlParser.parse_detailed("<?marker name='A'>")
    [marker] = document.children
    tree = HtmlTree.from_document(document)

    for key <- [:patch_state, :insertion_target, :removed_source_ledger] do
      forged = put_in(tree.nodes[marker.node_id], Map.put(tree.nodes[marker.node_id], key, %{}))

      assert {:error, {:invalid_document, diagnostic}} =
               HtmlTreeValidator.prepare_content_patch(forged, forged.root, "A")

      assert diagnostic.stage == :style and diagnostic.reason == :invalid_document
      assert diagnostic.module == HtmlValidator and is_binary(diagnostic.message)

      assert {:error, {:invalid_document, _}} =
               Style.compute_detailed(%{document | children: [Map.put(marker, key, %{})]})
    end
  end

  test "lookup and sibling range scans consume cumulative shared work before traversal" do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)

    assert {:ok, document} =
             HtmlParser.parse_detailed(
               "<?start name='A'>" <> String.duplicate("<p>Old</p>", 30) <> "<?end>"
             )

    tree = HtmlTree.from_document(document)
    Limits.install(%{limits | max_html_tree_work: 1})

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             HtmlTreeValidator.prepare_content_patch(tree, tree.root, "A")

    assert diagnostic.stage == :html and diagnostic.message =~ "max_html_tree_work"

    Limits.install(%{limits | max_html_tree_work: 1000})
    assert {:ok, target} = HtmlTreeValidator.prepare_content_patch(tree, tree.root, "A")
    assert length(target.removed) == 30

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             HtmlValidator.with_render_budget(fn ->
               HtmlValidator.reserve_render_resource(:max_html_tree_work, 950, :html)
               HtmlTreeValidator.prepare_content_patch(tree, tree.root, "A")
             end)

    assert diagnostic.reason == :resource_limit_exceeded
    assert diagnostic.module == HtmlValidator
  end

  test "preparation does not enable template replacement in any public parser or renderer" do
    source = "<?marker name='A'><template for='A'><p>Replacement</p></template>"
    assert {:error, {:unsupported_html, diagnostic}} = HtmlParser.parse_document_detailed(source)
    assert diagnostic.stage == :html and diagnostic.source =~ "template"
    assert diagnostic.line == 1 and diagnostic.column == 19

    assert {:error, {:unsupported_html, _}} =
             NativeElixirPdfUtilities.HtmlToPdf.render(source)
  end
end
