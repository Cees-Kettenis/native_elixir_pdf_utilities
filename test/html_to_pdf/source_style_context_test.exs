defmodule NativeElixirPdfUtilities.HtmlToPdf.SourceStyleContextTest do
  use ExUnit.Case, async: false

  alias NativeElixirPdfUtilities.{Diagnostics, Limits}
  alias NativeElixirPdfUtilities.HtmlToPdf.{AssetLoader, CssParser, Font, HtmlParser, Style}
  alias NativeElixirPdfUtilities.Validators.{CssValueValidator, HtmlValidator}

  setup do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    :ok
  end

  test "actual original cascade shares roots, inheritance, defaults and selector precedence" do
    css =
      "html{font-size:20pt;--size:2rem;--color:blue}body{color:red}div{--color:green}div p{color:var(--color);width:var(--size)}#a{color:blue!important}p:nth-child(2){font-size:1.5rem}p:lang(en)::before{content:'Before';color:currentColor}"

    html =
      "<html lang='en'><head><style>p{color:red}</style></head><body><div><?marker><p id='a' style='color:black;--size:3rem'>A</p><p id='b'>B</p><h1>Heading</h1></div></body></html>"

    assert {:ok, dom} = HtmlParser.parse_document_detailed(html)
    assert {:ok, result, contexts} = collect(dom, [css])
    assert result.source_obligations.stylesheets == [css, "p{color:red}"]
    assert result.source_obligations.unmatched_rules == :unresolved

    assert result.runtime_obligations == [
             :font_selection,
             :font_metrics,
             :text_decorations,
             :resources
           ]

    {original, nil, %{status: :resolved, style: first}} =
      Enum.find(contexts, fn {node, pseudo, _} ->
        pseudo == nil and Map.get(Map.get(node, :attributes, %{}), "id") == "a"
      end)

    assert original.source.text == "<p id='a' style='color:black;--size:3rem'>"
    assert original.attributes == %{"id" => "a", "style" => "color:black;--size:3rem"}
    assert is_reference(original.node_id) and is_reference(original._selector_id)
    assert first.color == {0.0, 0.0, 1.0} and first.width == 60.0
    assert first._root_font_size == 20.0
    assert first.font_face == nil and first._font_registry == nil

    {_, _, %{style: second}} =
      Enum.find(contexts, fn {node, pseudo, _} ->
        pseudo == nil and Map.get(Map.get(node, :attributes, %{}), "id") == "b"
      end)

    assert second.font_size == 30.0 and second.width == 40.0

    {_, :before, %{style: pseudo}} =
      Enum.find(contexts, fn {node, pseudo, _} ->
        pseudo == :before and node.attributes["id"] == "a"
      end)

    assert pseudo.color == first.color

    assert {:ok, rendered} = Style.compute_detailed(dom, stylesheets: [{:css, css}])
    rendered_first = find_styled(rendered, "p")

    assert Map.take(first, [:color, :font_size, :width, :_root_font_size]) ==
             Map.take(rendered_first.style, [:color, :font_size, :width, :_root_font_size])
  end

  test "head, hidden descendants and closed disclosures retain authored contexts" do
    html =
      "<html><head style='--size:7pt'><meta name='author' content='Author' style='width:var(--size)'></head><body><div hidden style='--size:9pt'><p id='hidden' style='width:var(--size)'>Hidden</p></div><details style='--size:11pt'><p id='closed' style='width:var(--size)'>Closed</p><summary>Legend</summary></details><details><p>Fallback body</p></details></body></html>"

    assert {:ok, dom} = HtmlParser.parse_document_detailed(html)
    assert {:ok, _result, contexts} = collect(dom)

    for {id, width} <- [{"hidden", 9.0}, {"closed", 11.0}] do
      {_, nil, %{style: style}} =
        Enum.find(contexts, fn {node, _, _} ->
          Map.get(Map.get(node, :attributes, %{}), "id") == id
        end)

      assert style.width == width
    end

    {_, nil, %{style: meta}} = Enum.find(contexts, fn {node, _, _} -> node.tag == "meta" end)
    assert meta.width == 7.0

    {summary, nil, %{style: summary_style}} =
      Enum.find(contexts, fn {node, _, _} -> node.tag == "summary" end)

    assert summary._disclosure_open == false and
             summary_style.list_marker_type == :disclosure_closed

    assert Enum.count(contexts, fn {node, _, _} -> node.tag == "summary" end) == 1
  end

  test "detached template scopes remain unresolved with every exact original visible" do
    html =
      "<div style='--width:8pt'><template><style>p{width:var(--width)}</style><p style='width:var(--width)'>Detached</p><?marker name='X'?><template><p>Nested</p></template></template></div>"

    assert {:ok, dom} = HtmlParser.parse_document_detailed(html)
    assert {:ok, result, contexts} = collect(dom, ["p{color:var(--unavailable)}"])
    detached = Enum.filter(contexts, fn {_, _, context} -> context.status == :unresolved end)
    assert length(detached) == 10

    for {node, nil, context} <- detached do
      assert context == %{status: :unresolved, reason: :template_insertion_context}
      assert is_reference(node.tree_scope_id) and is_reference(node.document_id)
      refute Map.has_key?(node, :_selector_id)
    end

    fragments = for {%{type: :document_fragment} = node, _, _} <- detached, do: node
    assert length(fragments) == 2
    assert Enum.uniq(Enum.map(fragments, & &1.tree_scope_id)) |> length() == 2
    assert result.source_obligations.template_insertion_contexts == :unresolved
    assert result.source_obligations.stylesheets == ["p{color:var(--unavailable)}"]
    assert Enum.any?(detached, fn {node, _, _} -> Map.get(node, :text) == "Detached" end)
    assert Enum.any?(detached, fn {node, _, _} -> Map.get(node, :target) == "marker" end)
  end

  test "font, image and reference context preparation never reads or resolves resources" do
    traced = [
      {Font, :load_registry, 1},
      {Font, :resolve, 4},
      {AssetLoader, :resolve, 4},
      {File, :read, 1},
      {File, :stat, 1},
      {File, :lstat, 1}
    ]

    Enum.each(traced, fn {module, _, _} -> Code.ensure_loaded!(module) end)
    Enum.each(traced, fn mfa -> assert :erlang.trace_pattern(mfa, true, [:local]) == 1 end)

    task =
      Task.async(fn ->
        receive do
          :validate ->
            assert {:ok, dom} =
                     HtmlParser.parse_document_detailed(
                       "<div style='font-family:Missing;background-image:url(\"/missing.png\");text-decoration:underline'><img src='/missing.png' width='24'><details><p>Closed</p></details><template><img src='/other.png'></template></div>"
                     )

            css =
              "@font-face{font-family:Missing;src:url('/missing.ttf')}@media screen{p{width:var(--unknown)}}.unmatched{height:var(--unknown)}"

            assert {:ok, result, contexts} = collect(dom, [css])
            assert result.source_obligations.inactive_media == :unresolved
            assert result.source_obligations.resource_references == :unresolved

            {_, _, %{style: image}} =
              Enum.find(contexts, fn {node, _, context} ->
                context.status == :resolved and Map.get(node, :tag) == "img"
              end)

            assert image.width == 18.0
            refute Map.has_key?(image, :image)
            assert {:error, {:invalid_options, _}} = collect(dom, [{:file, "/missing.css"}])
            :ok
        end
      end)

    try do
      :erlang.trace(task.pid, true, [:call])
      send(task.pid, :validate)
      assert :ok = Task.await(task)
      refute_receive {:trace, _, :call, _}
    after
      Enum.each(traced, &:erlang.trace_pattern(&1, false, [:local]))
    end
  end

  test "responsible input validation rejects forged DOM, source, callback and budget shapes" do
    assert {:ok, dom} = HtmlParser.parse_document_detailed("<p>Text</p>")
    good = fn _, _, _ -> :ok end

    for {input, sources, budget, callback, reason, source} <- [
          {nil, [], HtmlValidator.new_css_budget(), good, :invalid_document, nil},
          {dom, nil, HtmlValidator.new_css_budget(), good, :invalid_options, "stylesheets"},
          {dom, [123], HtmlValidator.new_css_budget(), good, :invalid_options, "stylesheets"},
          {dom, [], nil, good, :invalid_options, "css_budget"},
          {dom, [], :atomics.new(1, signed: false), good, :invalid_options, "css_budget"},
          {dom, [], :atomics.new(2, signed: true), good, :invalid_options, "css_budget"},
          {dom, [], HtmlValidator.new_css_budget(), fn _ -> :ok end, :invalid_options, "consumer"}
        ] do
      assert {:error, {^reason, diagnostic}} =
               Style.with_source_style_contexts(input, sources, budget, callback)

      assert diagnostic.message != "" and diagnostic.reason == reason
      if source, do: assert(diagnostic.source == source)
    end

    assert {:error, {:invalid_css, diagnostic}} = collect(dom, [<<255>>])
    assert diagnostic.stage == :css
    forged = Map.put(dom, :projection, dom)
    assert {:error, {:invalid_document, _}} = collect(forged)

    assert {:ok, normal} = Style.compute_detailed(dom, source_budget: nil)
    assert find_styled(normal, "p").style.font_face != nil

    assert {:error, {:invalid_css, _}} = collect(dom, ["p{color:red"])

    assert {:error, {:invalid_options, diagnostic}} =
             Style.with_source_style_contexts(dom, [], HtmlValidator.new_css_budget(), fn _,
                                                                                          _,
                                                                                          _ ->
               {:error, :wrong_shape}
             end)

    assert diagnostic.source == "consumer"
  end

  test "declaration failures retain exact original context sources without replay expansion" do
    for css <- [
          "width:var(--size)",
          "--size:var(--missing);width:var(--size)",
          "bad-property:x",
          "width : bananas",
          "color",
          "width:1pt!important!important"
        ] do
      html = "<div style='--size:bananas'>\n  <p style='#{css}'>Bad</p></div>"
      assert {:ok, dom} = HtmlParser.parse_document_detailed(html)
      assert {:error, {:invalid_css, diagnostic}} = collect(dom)
      assert diagnostic.stage == :css and diagnostic.module == HtmlValidator
      assert diagnostic.source in ["<div style='--size:bananas'>", "<p style='#{css}'>"]
      assert diagnostic.line in [1, 2] and diagnostic.column in [1, 3]
    end

    assert {:ok, dom} =
             HtmlParser.parse_document_detailed("<p style='--x:abc;width:var(--x)'>Bad</p>")

    budget = HtmlValidator.new_css_budget()
    assert {:error, {:invalid_css, _}} = collect(dom, [], budget)
    assert :atomics.get(budget, 1) == 9
    assert :atomics.get(budget, 2) > 0
  end

  test "callback diagnostics stop resolved, pseudo and detached traversal unchanged" do
    error =
      Diagnostics.error(:html, :invalid_document, "consumer context failed", source: "original")

    for {html, css, failing} <- [
          {"<p>Text</p>", [], fn node, pseudo, _ -> node.tag == "p" and pseudo == nil end},
          {"<p>Text</p>", ["p::before{content:'before'}"],
           fn _, pseudo, _ -> pseudo == :before end},
          {"<p>Text</p>", ["p::after{content:'after'}"], fn _, pseudo, _ -> pseudo == :after end},
          {"<template><p>Detached</p></template>", [],
           fn node, _, _ -> Map.get(node, :tag) == "p" end},
          {"<template><p>Detached</p></template>", [],
           fn node, _, _ -> node.type == :document_fragment end},
          {"<template><template><p>Nested</p></template></template>", [],
           fn node, _, _ -> Map.get(node, :text) == "Nested" end}
        ] do
      assert {:ok, dom} = HtmlParser.parse_document_detailed(html)

      assert ^error =
               Style.with_source_style_contexts(dom, css, HtmlValidator.new_css_budget(), fn node,
                                                                                             pseudo,
                                                                                             context ->
                 if failing.(node, pseudo, context), do: error, else: :ok
               end)
    end

    assert {:ok, dom} = HtmlParser.parse_document_detailed("<p>Text</p>")
    assert {:error, {:invalid_css, _}} = collect(dom, ["p::before{width:bananas}"])

    assert {:error, {:unsupported_html, diagnostic}} =
             collect(dom, ["p{background-image:url('javascript:run()')}"])

    assert diagnostic.source == "background-image"
  end

  test "initial control selectors share normal metadata without runtime font metrics" do
    assert {:ok, dom} =
             HtmlParser.parse_document_detailed(
               "<input type='checkbox' checked><input type='checkbox' disabled><select><option selected>A</option></select><textarea>Text</textarea>"
             )

    assert {:ok, _, contexts} =
             collect(dom, ["input:checked{color:red}input:disabled{color:blue}"])

    inputs = for {%{tag: "input"} = node, nil, context} <- contexts, do: {node, context.style}
    [{first, first_style}, {second, second_style}] = inputs
    assert is_map(first._initial_form_state) and is_map(second._initial_form_state)
    assert first_style.color == {1.0, 0.0, 0.0} and second_style.color == {0.0, 0.0, 1.0}

    assert Enum.any?(contexts, fn {node, _, context} ->
             node.tag == "select" and context.style.font_face == nil
           end)
  end

  test "one invocation retains cumulative source, variable, node and traversal reservations" do
    assert {:ok, dom} =
             HtmlParser.parse_document_detailed("<p style='--x:3pt;width:var(--x)'>Text</p>")

    budget = HtmlValidator.new_css_budget()
    assert {:ok, _, _} = collect(dom, [], budget)
    used = :atomics.get(budget, 1)
    assert used == 9
    Limits.install(Map.put(Limits.effective(), :max_css_variable_total_bytes, used + 1))
    assert {:error, {:resource_limit_exceeded, diagnostic}} = collect(dom, [], budget)
    assert diagnostic.message =~ "max_css_variable_total_bytes"

    Limits.install(Map.put(Limits.effective(), :max_css_variable_total_bytes, 10_000_000))
    source = "p{color:red}"

    Limits.install(
      Map.put(
        Limits.effective(),
        :max_aggregate_css_source_bytes,
        byte_size(source) + byte_size("color:red")
      )
    )

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             HtmlValidator.with_render_budget(fn ->
               assert {:ok, _, _} = collect(%{type: :document, children: []}, [source])
               collect(%{type: :document, children: []}, [source])
             end)

    assert diagnostic.message =~ "max_aggregate_css_source_bytes"

    Limits.install(Map.put(Limits.effective(), :max_aggregate_css_source_bytes, 20_000_000))
    Limits.install(Map.put(Limits.effective(), :max_html_nodes, 2))

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             HtmlValidator.with_render_budget(fn ->
               assert {:ok, original} = HtmlParser.parse_detailed("<p>Text</p>")
               assert {:ok, _, _} = collect(original)
               HtmlParser.parse_detailed("<p>Later</p>")
             end)

    assert diagnostic.message =~ "max_html_nodes"
  end

  test "legacy roots still traverse hidden synthetic body contexts without minting originals" do
    assert {:ok, dom} = HtmlParser.parse_detailed("<?marker?><p>Hidden</p>")

    assert {:ok, _, [{node, nil, %{style: style}}]} =
             collect(dom, ["body{display:none;--x:7pt}p{width:var(--x)}"])

    assert node.tag == "p" and style.width == 7.0
    assert {:ok, _, []} = collect(%{type: :document, children: []})
    assert {:error, {:invalid_css, diagnostic}} = collect(dom, ["body{width:bananas}"])
    assert diagnostic.source == "body"

    assert {:error, {:invalid_css, _}} =
             CssValueValidator.validate_source_cascade(
               %{_custom_properties: %{}},
               [:invalid],
               HtmlValidator.new_css_budget(),
               node
             )

    assert {:ok, []} = CssParser.parse("")
  end

  defp collect(dom, sources \\ [], budget \\ HtmlValidator.new_css_budget()) do
    key = {__MODULE__, :contexts}
    Process.put(key, [])

    try do
      case Style.with_source_style_contexts(dom, sources, budget, fn node, pseudo, context ->
             Process.put(key, [{node, pseudo, context} | Process.get(key)])
             :ok
           end) do
        {:ok, result} -> {:ok, result, Enum.reverse(Process.get(key))}
        error -> error
      end
    after
      Process.delete(key)
    end
  end

  defp find_styled(node, tag) do
    if Map.get(node, :tag) == tag,
      do: node,
      else: Enum.find_value(Map.get(node, :children, []), &find_styled(&1, tag))
  end
end
