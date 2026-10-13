defmodule NativeElixirPdfUtilities.Validators.TemplatePatchValidatorTest do
  use ExUnit.Case

  alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, HtmlTree, Style}

  alias NativeElixirPdfUtilities.Validators.{
    HtmlTreeValidator,
    HtmlValidator,
    TemplatePatchValidator
  }

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

  test "direct parsing still denies replacement while the renderer prepares supported source" do
    source = "<?marker name='A'><template for='A'><p>Replacement</p></template>"
    assert {:error, {:unsupported_html, diagnostic}} = HtmlParser.parse_document_detailed(source)
    assert diagnostic.stage == :html and diagnostic.source =~ "template"
    assert diagnostic.line == 1 and diagnostic.column == 19

    assert {:ok, pdf} = NativeElixirPdfUtilities.HtmlToPdf.render(source)
    # The earlier marker is in the implied head. Its non-painted insertion
    # remains distinct from body range replacement, covered by execution tests.
    assert {:error, {:no_extractable_text, _}} = NativeElixirPdfUtilities.Text.extract(pdf)
  end

  test "parser plans the original earlier range at the opening token before the public source gate" do
    source =
      "<head><?marker name='H'></head><body><?start name='A'><p style='width:bananas'>Old</p><?end><template for='A'><p>New</p></template><?marker name='Future'></body>"

    traces = [
      {TemplatePatchValidator, :with_insertion_context, 5},
      {HtmlTree, :begin_content_patch, 2},
      {HtmlTree, :adopt_content_patch, 2},
      {HtmlTree, :finish_content_patch, 1}
    ]

    Enum.each(traces, &:erlang.trace_pattern(&1, true, [:local]))

    task =
      Task.async(fn ->
        receive do
          :parse -> HtmlParser.parse_document_detailed(source)
        end
      end)

    try do
      :erlang.trace(task.pid, true, [:call])
      send(task.pid, :parse)
      assert {:error, {:unsupported_html, diagnostic}} = Task.await(task)
      assert diagnostic.stage == :html and diagnostic.operation == :parse

      assert diagnostic.module == TemplatePatchValidator and
               diagnostic.source == "<template for='A'>"

      assert diagnostic.message =~ "matched earlier body range with 1 original subtree"
      assert diagnostic.message =~ "original_source_policy"
      assert diagnostic.message =~ "selected_fonts_and_metrics"
      assert diagnostic.message =~ "active_resource_authorization_and_contents"

      assert_receive {:trace, _, :call,
                      {TemplatePatchValidator, :with_insertion_context,
                       [tree, parent, "A", origin, _]}}

      assert tree.nodes[parent].tag == "body"
      assert origin.source.text == "<template for='A'>"
      assert {:ok, target} = HtmlTreeValidator.prepare_content_patch(tree, parent, "A")
      [old] = target.removed
      assert tree.nodes[old].attributes["style"] == "width:bananas"
      assert HtmlTree.fetch(tree, old).children |> hd() |> Map.fetch!(:text) == "Old"

      refute Enum.any?(tree.nodes, fn {_, node} ->
               node[:text] == "New" or node[:data] == "name='Future'"
             end)

      refute_receive {:trace, _, :call, {HtmlTree, :begin_content_patch, _}}
      refute_receive {:trace, _, :call, {HtmlTree, :adopt_content_patch, _}}
      refute_receive {:trace, _, :call, {HtmlTree, :finish_content_patch, _}}

      assert {:error, {:invalid_css, rendered}} =
               NativeElixirPdfUtilities.HtmlToPdf.render(source)

      assert rendered.stage == :css and rendered.source == "width:bananas"
      assert rendered.operation == :source_style_contexts
      assert rendered.line == 1 and rendered.column == 65
    after
      Enum.each(traces, &:erlang.trace_pattern(&1, false, [:local]))
    end
  end

  test "missing empty and future targets remain inert-fallback plans with precise original diagnostics" do
    for name <- ["", "Missing", "Future"] do
      source = "<p>é🙂</p>\r\n<template for='#{name}'><p>New</p></template><?marker name='Future'>"

      assert {:error, {:unsupported_html, diagnostic}} =
               HtmlParser.parse_document_detailed(source)

      assert diagnostic.source == "<template for='#{name}'>"
      assert diagnostic.line == 2 and diagnostic.column == 1
      assert diagnostic.message =~ "inert fallback"
      assert diagnostic.message =~ "no original source has been discarded"
    end

    assert {:error, {:unsupported_html, diagnostic}} =
             HtmlParser.parse_detailed("<template for='A' shadowrootmode='open'></template>")

    assert diagnostic.message =~ "declarative shadow"
  end

  test "planning handles bind the original tree and budget and reject reuse alteration forgery and expiry" do
    prefix = "<?marker name='A'><p>Old</p>"
    {source, dom, tree, origin} = planning_fixture(prefix, "<template for='A'>", "div")

    keys = MapSet.new(Process.get_keys())

    escaped =
      TemplatePatchValidator.with_parser_invocation(source, fn _ ->
        TemplatePatchValidator.with_insertion_context(tree, tree.root, "A", origin, fn handle ->
          assert {:error, {:invalid_document, _}} =
                   TemplatePatchValidator.authorize_mutation(handle, tree)

          altered = %{
            tree
            | nodes:
                Map.update!(
                  tree.nodes,
                  tree.root,
                  &Map.put(&1, :source, %{text: "changed", line: 1, column: 1})
                )
          }

          assert {:error, {:invalid_document, _}} =
                   TemplatePatchValidator.with_planned_insertion(handle, altered, fn _ ->
                     flunk("altered plan consumed")
                   end)

          assert {:error, {:invalid_document, _}} =
                   TemplatePatchValidator.with_planned_insertion(%{receipt: handle}, tree, fn _ ->
                     flunk("forged plan consumed")
                   end)

          assert {:error, {:invalid_document, _}} =
                   TemplatePatchValidator.with_parser_invocation(source, fn _ ->
                     TemplatePatchValidator.with_planned_insertion(handle, tree, fn _ ->
                       flunk("cross-invocation plan consumed")
                     end)
                   end)

          budget_key = {HtmlValidator, :render_budget}
          counters = Process.get(budget_key)
          Process.put(budget_key, %{})

          assert {:error, {:invalid_document, _}} =
                   TemplatePatchValidator.with_planned_insertion(handle, tree, fn _ ->
                     flunk("reset budget accepted")
                   end)

          Process.put(budget_key, counters)

          assert {:error, {:invalid_document, _}} =
                   Task.async(fn ->
                     TemplatePatchValidator.with_planned_insertion(handle, tree, fn _ ->
                       flunk("cross-process plan consumed")
                     end)
                   end)
                   |> Task.await()

          assert :checked =
                   TemplatePatchValidator.with_planned_insertion(handle, tree, fn context ->
                     assert context.target.start == hd(dom.children).node_id

                     assert context.original.parent == tree.root and
                              context.insertion.parent == tree.root

                     assert context.insertion.context == "div"
                     assert context.pending_checks != []

                     assert {:error, {:unsupported_html, _}} =
                              TemplatePatchValidator.authorize_mutation(handle, tree)

                     :checked
                   end)

          assert {:error, {:invalid_document, _}} =
                   TemplatePatchValidator.with_planned_insertion(handle, tree, fn _ ->
                     flunk("reused plan consumed")
                   end)

          handle
        end)
      end)

    assert {:error, {:invalid_document, _}} =
             TemplatePatchValidator.with_planned_insertion(escaped, tree, fn _ ->
               flunk("expired plan consumed")
             end)

    assert MapSet.new(Process.get_keys()) == keys

    assert {:error, {:invalid_document, _}} =
             TemplatePatchValidator.with_insertion_context(tree, tree.root, "A", origin, fn _ ->
               flunk("unowned tree accepted")
             end)

    for callback <- [
          fn _ -> {:error, {:stop, %{stage: :html, reason: :stop, message: "stop"}}} end,
          fn _ -> raise "planning exception" end
        ] do
      try do
        TemplatePatchValidator.with_parser_invocation(source, fn _ ->
          TemplatePatchValidator.with_insertion_context(tree, tree.root, "A", origin, callback)
        end)
      rescue
        RuntimeError -> :ok
      end

      assert MapSet.new(Process.get_keys()) == keys
    end

    assert {:error, {:invalid_document, _}} =
             TemplatePatchValidator.with_parser_invocation("different", fn _ ->
               TemplatePatchValidator.with_insertion_context(tree, tree.root, "A", origin, fn _ ->
                 :bad
               end)
             end)

    forged = %{origin | source: %{origin.source | column: 2}}

    assert {:error, {:invalid_document, _}} =
             TemplatePatchValidator.with_parser_invocation(source, fn _ ->
               TemplatePatchValidator.with_insertion_context(tree, tree.root, "A", forged, fn _ ->
                 :bad
               end)
             end)

    false_token = %{origin | source: %{text: "<?marker name='A'>", line: 1, column: 1}, offset: 0}

    assert {:error, {:invalid_document, _}} =
             TemplatePatchValidator.with_parser_invocation(source, fn _ ->
               TemplatePatchValidator.with_insertion_context(
                 tree,
                 tree.root,
                 "A",
                 false_token,
                 fn _ -> :bad end
               )
             end)

    assert {:error, {:invalid_encoding, _}} =
             TemplatePatchValidator.with_parser_invocation(<<255>>, fn _ -> :bad end)
  end

  test "original and insertion contexts stop at fragment boundaries without borrowing host styles" do
    prefix =
      "<style>p{color:red}</style><?marker name='Outer'><template><style>p{color:blue}</style><div><?marker name='Inner'><p>Old</p></div>"

    {source, dom, tree, origin} =
      planning_fixture(prefix, "<template for='Inner'>", "template", "</template>")

    [outer_style, _, template] = dom.children
    [inner_style, div] = template.content.children

    assert :ok =
             TemplatePatchValidator.with_parser_invocation(source, fn _ ->
               TemplatePatchValidator.with_insertion_context(
                 tree,
                 template.node_id,
                 "Inner",
                 origin,
                 fn handle ->
                   TemplatePatchValidator.with_planned_insertion(handle, tree, fn context ->
                     assert context.original.parent == template.content.node_id
                     assert context.original.stylesheets == [inner_style.node_id]
                     refute outer_style.node_id in context.original.stylesheets
                     assert context.insertion.stylesheets == [inner_style.node_id]
                     assert context.original.ancestors == [template.content.node_id]
                     assert context.insertion.ancestors == [div.node_id, template.content.node_id]
                     assert context.insertion.scope == template.content.node_id

                     assert context.insertion.stylesheet_selection ==
                              :requires_renderer_media_context

                     :ok
                   end)
                 end
               )
             end)
  end

  test "patch transactions detach at start adopt before the live end and clean markers at close" do
    source =
      "<div><?start name='A'><p>Old</p><?end><p>Keep</p></div><template><p>New<span>Nested</span></p><template><b>Inert</b></template></template>"

    assert {:ok, dom} = HtmlParser.parse_detailed(source)
    tree = HtmlTree.from_document(dom)
    [div, template] = dom.children
    [start, old, finish, keep] = div.children
    [replacement, nested] = template.content.children
    assert {:ok, target} = HtmlTreeValidator.prepare_content_patch(tree, tree.root, "A")
    {tree, _} = HtmlTree.close(tree, tree.root)
    assert {:ok, transaction} = HtmlTree.begin_content_patch(tree, target)
    assert transaction.original == tree

    assert Enum.map(HtmlTree.fetch(transaction.tree, div.node_id).children, & &1.node_id) == [
             start.node_id,
             finish.node_id,
             keep.node_id
           ]

    assert HtmlTree.fetch(transaction.original, old.node_id) == old
    assert {:ok, transaction} = HtmlTree.adopt_content_patch(transaction, [replacement.node_id])
    assert {:ok, transaction} = HtmlTree.adopt_content_patch(transaction, [nested.node_id])

    assert Enum.map(HtmlTree.fetch(transaction.tree, div.node_id).children, & &1.node_id) == [
             start.node_id,
             replacement.node_id,
             nested.node_id,
             finish.node_id,
             keep.node_id
           ]

    assert {:ok, active} = HtmlTree.finish_content_patch(transaction)
    [active_div, active_template] = HtmlTree.children(active, active.root)

    assert Enum.map(active_div.children, & &1.node_id) == [
             replacement.node_id,
             nested.node_id,
             keep.node_id
           ]

    assert active_template.content.children == []
    [adopted, nested, _] = active_div.children
    assert adopted.document_id == replacement.document_id and adopted.source == replacement.source
    assert adopted.parent_id == div.node_id and adopted.tree_scope_id == div.tree_scope_id

    assert Enum.all?(
             adopted.children,
             &(&1.parent_id == adopted.node_id and &1.tree_scope_id == div.tree_scope_id)
           )

    assert nested.content.parent_id == nil and nested.content.tree_scope_id != div.tree_scope_id
    assert nested.content.children |> hd() |> Map.fetch!(:tree_scope_id) == nested.content.node_id
    assert HtmlTree.fetch(transaction.original, tree.root) == dom
    assert :ok = HtmlTreeValidator.validate_transport(HtmlTree.fetch(active, active.root))
    assert transaction.inserted == [replacement.node_id, nested.node_id]
  end

  test "marker-only and unmatched-end transactions keep source identities and reject stale or cyclic adoption" do
    for marker <- ["<?marker name='A'>", "<?start name='A'><p>Old</p>"] do
      assert {:ok, dom} =
               HtmlParser.parse_detailed("<div>#{marker}</div><template><p>New</p></template>")

      tree = HtmlTree.from_document(dom)
      [div, template] = dom.children
      [replacement] = template.content.children
      assert {:ok, target} = HtmlTreeValidator.prepare_content_patch(tree, tree.root, "A")
      assert {:ok, transaction} = HtmlTree.begin_content_patch(tree, target)

      assert {:error, {:invalid_document, _}} =
               HtmlTree.adopt_content_patch(transaction, [div.node_id])

      assert {:error, {:invalid_document, _}} =
               HtmlTree.adopt_content_patch(transaction, [target.start])

      assert {:error, {:invalid_document, _}} =
               HtmlTree.adopt_content_patch(transaction, [
                 replacement.node_id,
                 replacement.node_id
               ])

      assert {:error, {:invalid_document, _}} =
               HtmlTree.adopt_content_patch(transaction, [make_ref()])

      active_tree = transaction.tree
      detached = put_in(active_tree.nodes[replacement.node_id].parent_id, nil)
      transaction = %{transaction | tree: detached}
      assert {:ok, transaction} = HtmlTree.adopt_content_patch(transaction, [replacement.node_id])
      assert {:ok, active} = HtmlTree.finish_content_patch(transaction)
      assert [adopted] = HtmlTree.fetch(active, div.node_id).children
      assert adopted.node_id == replacement.node_id

      assert {:error, {:invalid_document, _}} =
               HtmlTree.finish_content_patch(%{transaction | tree: active})

      assert {:error, {:invalid_document, _}} =
               HtmlTree.begin_content_patch(tree, %{target | removed: [make_ref()]})
    end

    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    assert {:ok, dom} = HtmlParser.parse_detailed("<?marker name='A'>")
    tree = HtmlTree.from_document(dom)
    assert {:ok, target} = HtmlTreeValidator.prepare_content_patch(tree, tree.root, "A")
    Limits.install(%{limits | max_html_tree_work: 0})
    assert {:error, {:resource_limit_exceeded, _}} = HtmlTree.begin_content_patch(tree, target)

    assert {:error, {:resource_limit_exceeded, _}} =
             TemplatePatchValidator.with_parser_invocation(dom.source.text, fn _ ->
               TemplatePatchValidator.with_insertion_context(tree, tree.root, "A", %{}, fn _ ->
                 :bad
               end)
             end)
  end

  test "invocation plans retain distinct body-to-head and foreign insertion parents" do
    token = "<template for='A'>"

    for {prefix, closing, target_tag, target_namespace} <- [
          {"<head><style>p{color:red}</style><?marker name='A'></head><body>", "</body>", "head",
           :html},
          {"<body><svg><?marker name='A'></svg>", "</body>", "svg", :svg}
        ] do
      assert {:ok, dom} = HtmlParser.parse_document_detailed(prefix <> closing)
      [html] = dom.children
      [head, body] = html.children
      tree = HtmlTree.from_document(dom)
      source = prefix <> token
      root = tree.nodes[tree.root]

      tree = %{
        tree
        | nodes:
            Map.put(tree.nodes, tree.root, %{root | source: %{text: source, line: 1, column: 1}})
      }

      origin = %{
        source: %{text: token, line: 1, column: String.length(prefix) + 1},
        offset: byte_size(prefix),
        tokenizer_context: "body",
        ancestors: []
      }

      assert :checked =
               TemplatePatchValidator.with_parser_invocation(source, fn _ ->
                 TemplatePatchValidator.with_insertion_context(
                   tree,
                   body.node_id,
                   "A",
                   origin,
                   fn handle ->
                     TemplatePatchValidator.with_planned_insertion(handle, tree, fn context ->
                       assert context.original.parent == body.node_id
                       assert context.original.context == "body"
                       assert context.original.scope == dom.node_id
                       assert tree.nodes[context.insertion.parent].tag == target_tag
                       assert context.insertion.namespace == target_namespace
                       assert context.insertion.scope == dom.node_id
                       assert context.insertion.stylesheets == context.original.stylesheets

                       if target_namespace == :svg do
                         assert context.insertion.context == {:foreign, :svg, "svg", %{}}
                       else
                         assert context.insertion.context == "head"
                         assert context.insertion.parent == head.node_id
                         assert context.target.scope == html.node_id
                       end

                       assert {:error, {:unsupported_html, diagnostic}} =
                                TemplatePatchValidator.authorize_mutation(handle, tree)

                       assert diagnostic.source == token and diagnostic.operation == :parse
                       assert diagnostic.message =~ "insertion_content_model"
                       :checked
                     end)
                   end
                 )
               end)
    end
  end

  test "direct DOM styling cannot borrow parser plans or bypass composition policy" do
    assert {:ok, dom} = HtmlParser.parse_document_detailed("<template><p>Inert</p></template>")
    [html] = dom.children
    [head, body] = html.children
    [template] = head.children

    template = %{
      template
      | attributes: Map.put(template.attributes, "for", "A"),
        attribute_order: ["for"]
    }

    dom = %{dom | children: [%{html | children: [%{head | children: [template]}, body]}]}

    assert {:error, {:unsupported_html, diagnostic}} = Style.compute_detailed(dom)
    assert diagnostic.stage == :html and diagnostic.module == TemplatePatchValidator
    assert diagnostic.message =~ "validated parser composition contexts"
    assert Process.get({TemplatePatchValidator, :invocation}) == nil
  end

  defp planning_fixture(prefix, token, context, closing \\ "") do
    assert {:ok, dom} = HtmlParser.parse_detailed(prefix <> closing)
    tree = HtmlTree.from_document(dom)
    source = prefix <> token
    owner = tree.nodes[tree.root]

    tree = %{
      tree
      | nodes:
          Map.put(tree.nodes, tree.root, %{owner | source: %{text: source, line: 1, column: 1}})
    }

    origin = %{
      source: %{text: token, line: 1, column: String.length(prefix) + 1},
      offset: byte_size(prefix),
      tokenizer_context: context,
      ancestors: []
    }

    {source, dom, tree, origin}
  end
end
