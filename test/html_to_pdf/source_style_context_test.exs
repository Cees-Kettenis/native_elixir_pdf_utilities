defmodule NativeElixirPdfUtilities.HtmlToPdf.SourceStyleContextTest do
  use ExUnit.Case, async: false

  alias NativeElixirPdfUtilities.{Diagnostics, Limits}
  alias NativeElixirPdfUtilities.HtmlToPdf.{AssetLoader, CssParser, Font, HtmlParser, Style}

  alias NativeElixirPdfUtilities.Validators.{
    CssValueValidator,
    HtmlCssSourceValidator,
    CssSourceContextValidator,
    HtmlValidator
  }

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

    assert result.source_obligations.stylesheets == [
             "p{color:var(--unavailable)}",
             "p{width:var(--width)}"
           ]

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
                       "<style>/*original*/\r\np{color:blue}</style><div style='font-family:Missing;background-image:url(\"/missing.png\");text-decoration:underline'><img src='/missing.png' width='24'><details><p>Closed</p></details><template><style>@font-face{font-family:Detached;src:url('/other.ttf')}p{background-image:url('/other.png')}</style><p style='background-image:url(&quot;/inert.png&quot;)'>Inert</p><img src='/other.png'></template></div>"
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
      assert diagnostic.stage == :css
      assert diagnostic.source in [css, "--size:var(--missing)", "width:var(--size)"]
      assert diagnostic.line == 2 and diagnostic.column >= 13
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

    assert diagnostic.source == "url('javascript:run()')"
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
    assert diagnostic.source == "width:bananas"

    assert {:error, {:invalid_css, _}} =
             CssValueValidator.validate_source_cascade(
               %{_custom_properties: %{}},
               [:invalid],
               HtmlValidator.new_css_budget(),
               node
             )

    assert {:ok, []} = CssParser.parse("")
  end

  test "inline original HTML mapping retains entity extents, newlines and grapheme columns" do
    html =
      "\uFEFF<div title='width:bananas'>\r\n<p title='x' style=\"--x:é🙂;\r\n width : b&#97;nanas\">Bad</p></div>"

    assert {:ok, dom} = HtmlParser.parse_document_detailed(html)
    assert {:error, {:invalid_css, diagnostic}} = collect(dom)
    assert diagnostic.source == "width : b&#97;nanas"
    assert diagnostic.line == 3 and diagnostic.column == 2
    assert diagnostic.operation == :source_style_contexts

    html = "<p title='width:bananas' style='--x:é🙂;width:bananas'>Bad</p>"
    assert {:ok, dom} = HtmlParser.parse_document_detailed(html)
    assert {:error, {:invalid_css, diagnostic}} = collect(dom)
    assert diagnostic.source == "width:bananas"
    assert diagnostic.line == 1 and diagnostic.column == 40

    html = "<p style='--x:&fjlig;;width&colon;3pt'>Good</p>"
    assert {:ok, dom} = HtmlParser.parse_document_detailed(html)
    assert {:ok, result, contexts} = collect(dom)
    {node, nil, %{style: style}} = Enum.find(contexts, fn {node, _, _} -> node.tag == "p" end)
    assert style.width == 3.0
    assert [custom, width] = result.source_obligations.inline[node.node_id]
    assert custom.source.text == "--x:&fjlig;"
    assert width.source.text == "width&colon;3pt"
    assert width.source.provenance == :original_html

    assert width.source.owner ==
             Map.take(node, [:node_id, :document_id, :tree_scope_id, :parent_id])

    assert binary_part(html, width.source.offset, width.source.length) == width.source.text
    assert node.source.text == "<p style='--x:&fjlig;;width&colon;3pt'>"

    assert {:ok, dom} =
             HtmlParser.parse_document_detailed("<p style='color:red;&NewLine;  color'>Bad</p>")

    assert {:error, {:invalid_css, diagnostic}} = collect(dom)
    assert diagnostic.source == "color"
    assert diagnostic.line == 1 and diagnostic.column == 32
    assert diagnostic.message =~ "line 1:"
  end

  test "inline, embedded and font syntax failures report exact original declaration offsets" do
    for {html, snippet} <- [
          {"<p style='color:red;\r\n  color'>Bad</p>", "color"},
          {"<style>p{color:red;\r\n  color}</style>", "color"},
          {"<style>@font-face{font-family:X;src:url(x.ttf);\r\n  font-family}</style>",
           "font-family"}
        ] do
      assert {:ok, dom} = HtmlParser.parse_document_detailed(html)
      assert {:error, {:invalid_css, diagnostic}} = collect(dom)
      assert diagnostic.source == snippet
      assert diagnostic.line == 2 and diagnostic.column == 3
      assert diagnostic.message =~ "line 2:"
    end

    assert {:error, {:invalid_css, diagnostic}} =
             CssParser.prepare_source_stylesheet("p{color:red;\n  color}")

    assert diagnostic.source == "color" and diagnostic.line == 2 and diagnostic.column == 3
  end

  test "source selector syntax failures keep their actual original stylesheet location" do
    html = "<style>/* original */\r\np:unknown{color:red}</style><p>Text</p>"
    assert {:ok, dom} = HtmlParser.parse_document_detailed(html)
    assert {:error, {:invalid_css, diagnostic}} = collect(dom)
    assert diagnostic.source == "p:unknown"
    assert diagnostic.line == 2 and diagnostic.column == 1
    assert diagnostic.message =~ "line 2:"
  end

  test "embedded stylesheet records bind actual original owners across preprocessing and repeated snippets" do
    html =
      "\uFEFF<style>.unused{width:bananas}</style>\r\n<style>/*é🙂*/\r p{wi/**/dth:bananas}</style><p>Bad</p>"

    assert {:ok, dom} = HtmlParser.parse_document_detailed(html)
    assert {:error, {:invalid_css, diagnostic}} = collect(dom)
    assert diagnostic.source == "wi/**/dth:bananas"
    assert diagnostic.line == 3 and diagnostic.column == 4

    html = "<style>p{color:blue}</style>\r\n<style>p{color:blue}</style><p>Good</p>"
    assert {:ok, dom} = HtmlParser.parse_document_detailed(html)
    assert {:ok, result, _} = collect(dom)
    [first, second] = result.source_obligations.authored
    [first_rule] = first.authored_rules
    [second_rule] = second.authored_rules
    [first_declaration] = first_rule.source_declarations
    [second_declaration] = second_rule.source_declarations
    assert first_declaration.source.text == second_declaration.source.text
    assert first_declaration.source.offset == 9 and first_declaration.source.column == 10
    assert second_declaration.source.offset == 39 and second_declaration.source.line == 2
    refute first_declaration.source.owner.node_id == second_declaration.source.owner.node_id

    html =
      "<style>/*font*/\r\n@font-face{font-family:X;src:url(x.ttf);font-display:bad;font-display:block}</style>"

    assert {:ok, dom} = HtmlParser.parse_document_detailed(html)
    assert {:error, {:invalid_css, diagnostic}} = collect(dom)
    assert diagnostic.source == "font-display:bad"
    assert diagnostic.line == 2 and diagnostic.column == 41
  end

  test "foreign style text binds entities, CDATA and original comment boundaries" do
    html =
      "<svg><style>p{color:bl&#117;e}<!--gap--><![CDATA[p{width:3pt}]]></style></svg><p>Good</p>"

    assert {:ok, dom} = HtmlParser.parse_document_detailed(html)
    assert {:ok, provenance} = HtmlCssSourceValidator.prepare(dom)
    [entry] = provenance.embedded
    assert {:ok, authored} = CssParser.prepare_source_stylesheet(entry.css, entry.descriptor)
    [first, second] = authored.authored_rules
    assert hd(first.source_declarations).source.text == "color:bl&#117;e"
    assert hd(second.source_declarations).source.text == "width:3pt"
    assert hd(second.source_declarations).source.offset == 51

    assert hd(first.source_declarations).source.owner ==
             hd(second.source_declarations).source.owner

    html = "<svg><style>p{color:blue}<!--gap-->p{width:b&#97;nanas}</style></svg><p>Bad</p>"
    assert {:ok, dom} = HtmlParser.parse_document_detailed(html)
    assert {:ok, provenance} = HtmlCssSourceValidator.prepare(dom)
    [entry] = provenance.embedded
    assert {:ok, authored} = CssParser.prepare_source_stylesheet(entry.css, entry.descriptor)
    [_, second] = authored.authored_rules
    record = hd(second.source_declarations)

    assert {:error, {:invalid_css, diagnostic}} =
             CssValueValidator.validate_source_cascade(
               %{_custom_properties: %{}},
               [record],
               HtmlValidator.new_css_budget(),
               hd(dom.children)
             )

    assert diagnostic.source == "width:b&#97;nanas"
    assert diagnostic.column == 38
  end

  test "forged canonical attributes, source ownership and text cannot create bound provenance" do
    html = "<!--<p style='width:3pt'>--><p style='width:3pt'>Good</p><style>p{color:blue}</style>"
    assert {:ok, dom} = HtmlParser.parse_document_detailed(html)

    for edit <- [
          fn node ->
            if node.tag == "p", do: put_in(node.attributes["style"], "width:4pt"), else: node
          end,
          fn node -> if node.tag == "p", do: put_in(node.source.column, 5), else: node end,
          fn node -> if node.tag == "p", do: put_in(node.source.line, 999), else: node end,
          fn node -> if node.tag == "p", do: put_in(node.source.column, 999), else: node end,
          fn node ->
            if node.tag == "p", do: put_in(node.source.text, "<p style='width:4pt'>"), else: node
          end
        ] do
      forged = edit_elements(dom, edit)
      assert {:error, {:invalid_document, diagnostic}} = collect(forged)
      assert diagnostic.module == HtmlCssSourceValidator
      assert diagnostic.stage == :html and diagnostic.message != ""
    end

    forged =
      edit_elements(dom, fn node ->
        if node.tag == "style",
          do: update_in(node.children, fn [text] -> [%{text | text: "p{color:red}"}] end),
          else: node
      end)

    assert {:error, {:invalid_document, diagnostic}} = collect(forged)
    assert diagnostic.message =~ "decoded content"

    assert {:error, {:invalid_document, _}} = collect(put_in(dom.source.text, "<p>Other</p>"))

    assert {:error, {:invalid_document, _}} =
             collect(put_in(dom.source.text, "<p title='unterminated>"))

    assert {:error, {:invalid_document, _}} = collect(put_in(dom.source.text, "<!--<p>"))
  end

  test "metadata-free sources stay local and validator descriptors reject tampering" do
    html = "<style>p{color:red}</style><p style='width:3pt'>Good</p>"
    assert {:ok, canonical} = HtmlParser.parse_document_detailed(html)
    local = strip_identity(canonical)
    assert {:ok, result, contexts} = collect(local)
    [authored] = result.source_obligations.authored
    assert hd(hd(authored.authored_rules).source_declarations).source.provenance == :unbound

    assert contexts
           |> Enum.find(fn {node, _, _} -> node.tag == "p" end)
           |> elem(2)
           |> Map.fetch!(:style)
           |> Map.fetch!(:width) == 3.0

    [[record]] = Map.values(result.source_obligations.inline)
    assert record.source.text == "width:3pt" and record.source.offset == 0
    assert record.source.owner == nil and record.source.provenance == :unbound

    assert {:ok, provenance} = HtmlCssSourceValidator.prepare(canonical)
    descriptor = hd(provenance.embedded).descriptor

    for forged <- [
          nil,
          %{},
          Map.put(descriptor, :css, "p{color:blue}"),
          Map.put(descriptor, :original, "forged"),
          Map.put(descriptor, :owner, nil),
          Map.put(descriptor, :mapper, fn _, _ -> :ok end)
        ] do
      assert {:error, {:invalid_document, diagnostic}} =
               CssParser.prepare_source_stylesheet("p{color:red}", forged || %{})

      assert diagnostic.source == "css_source_descriptor"
    end

    assert {:error, {:invalid_document, _}} =
             CssParser.prepare_source_declarations("different", descriptor)

    assert {:error, {:invalid_css, diagnostic}} =
             CssParser.prepare_source_declarations(<<255>>, HtmlCssSourceValidator.local(<<255>>))

    assert diagnostic.stage == :css and diagnostic.message =~ "UTF-8"

    assert {:error, {:invalid_document, diagnostic}} =
             HtmlValidator.with_render_budget(fn ->
               HtmlCssSourceValidator.map_span(descriptor, 99, 1)
             end)

    assert diagnostic.message =~ "outside"
    assert HtmlCssSourceValidator.map_span(HtmlCssSourceValidator.local("x"), 0, 0).text == ""
  end

  test "binding validates actual parents, unquoted values and original token completeness" do
    assert {:ok, dom} =
             HtmlParser.parse_document_detailed(
               "<div><p style=width:3pt>Text</p></div><p>Later</p>"
             )

    assert {:ok, result, _} = collect(dom)
    [[record]] = Map.values(result.source_obligations.inline)
    assert record.source.text == "width:3pt"
    [root] = dom.children
    [head, body] = root.children
    [div, later] = body.children
    [paragraph] = div.children
    moved = %{paragraph | parent_id: body.node_id}
    forged_body = %{body | children: [%{div | children: []}, moved, later]}
    forged = %{dom | children: [%{root | children: [head, forged_body]}]}
    assert {:error, {:invalid_document, diagnostic}} = collect(forged)
    assert diagnostic.message =~ "actual parent"
    assert {:ok, provenance} = HtmlCssSourceValidator.prepare(dom)
    descriptor = provenance.inline |> Map.values() |> hd()

    assert {:error, {:invalid_document, _}} =
             HtmlValidator.with_render_budget(fn ->
               HtmlCssSourceValidator.map_span(%{descriptor | original: "forged"}, 0, 1)
             end)

    html = "<style>p{color:red}</style><p>Text</p>"
    assert {:ok, dom} = HtmlParser.parse_document_detailed(html)

    forged =
      edit_elements(dom, fn node ->
        if node.tag == "style", do: %{node | source: %{text: "", line: 1, column: 1}}, else: node
      end)

    assert {:error, {:invalid_document, diagnostic}} = collect(forged)
    assert diagnostic.message =~ "requires an original opening token"
    assert {:error, {:invalid_document, _}} = collect(put_in(dom.source.column, 2))

    assert {:ok, empty} = HtmlParser.parse_document_detailed("")
    assert {:error, {:invalid_document, diagnostic}} = collect(put_in(empty.source.text, "text"))
    assert diagnostic.message =~ "no matching canonical"
    assert {:error, {:invalid_document, diagnostic}} = collect(put_in(empty.source.text, "<p"))
    assert diagnostic.message =~ "cannot be tokenized"

    html = "<!--Text--><p>Text</p>"
    assert {:ok, dom} = HtmlParser.parse_document_detailed(html)

    forged =
      edit_elements(dom, fn node ->
        if node.tag == "p",
          do: update_in(node.children, fn [text] -> [put_in(text.source.column, 5)] end),
          else: node
      end)

    assert {:error, {:invalid_document, diagnostic}} = collect(forged)
    assert diagnostic.message =~ "no matching canonical"

    assert {:ok, dom} =
             HtmlParser.parse_document_detailed(
               "<!--<p style='width:3pt'>--><p style='width:3pt'></p>"
             )

    forged =
      edit_elements(dom, fn node ->
        if node.tag == "p", do: put_in(node.source.column, 5), else: node
      end)

    assert {:error, {:invalid_document, diagnostic}} = collect(forged)
    assert diagnostic.message =~ "token boundary"

    assert {:ok, dom} = HtmlParser.parse_document_detailed("<!--Text--><p></p>")

    forged =
      edit_elements(dom, fn node ->
        if node.tag == "p" do
          text = %{
            type: :text,
            text: "Text",
            node_id: make_ref(),
            document_id: node.document_id,
            tree_scope_id: node.tree_scope_id,
            parent_id: node.node_id,
            source: %{text: "Text", line: 1, column: 5}
          }

          %{node | children: [text]}
        else
          node
        end
      end)

    assert {:error, {:invalid_document, diagnostic}} = collect(forged)
    assert diagnostic.message =~ "tokenizer boundary"

    assert {:ok, dom} =
             HtmlParser.parse_document_detailed("<p style='--x:&unknown;;width:3pt'>Good</p>")

    assert {:ok, _, _} = collect(dom)

    assert {:error, {:invalid_css, diagnostic}} =
             HtmlValidator.source_style_error(
               %{tag: "p", attributes: %{"style" => "width:bad"}},
               :inline_syntax
             )

    assert diagnostic.source == "width:bad"
  end

  test "empty boundaries and multibyte span validation retain original extents" do
    assert {:ok, dom} = HtmlParser.parse_document_detailed("<p style=''>Good</p><style></style>")
    assert {:ok, provenance} = HtmlCssSourceValidator.prepare(dom)
    descriptor = provenance.inline |> Map.values() |> hd()
    assert HtmlCssSourceValidator.map_span(descriptor, 0, 0).offset == 10
    descriptor = hd(provenance.embedded).descriptor
    assert HtmlCssSourceValidator.map_span(descriptor, 0, 0).offset == 27

    assert {:ok, dom} = HtmlParser.parse_document_detailed("<p style='--x:🙂;width:3pt'>Good</p>")
    assert {:ok, provenance} = HtmlCssSourceValidator.prepare(dom)
    descriptor = provenance.inline |> Map.values() |> hd()
    assert HtmlCssSourceValidator.map_span(descriptor, 0, 0).offset == 10
    last = HtmlCssSourceValidator.map_span(descriptor, byte_size(descriptor.css), 0)
    assert last.offset == 28

    for descriptor <- [descriptor, HtmlCssSourceValidator.local(descriptor.css)] do
      for {at, size} <- [{5, 0}, {5, 1}, {4, 1}, {-1, 1}, {0, -1}, {:invalid, 1}] do
        assert {:error, {:invalid_document, diagnostic}} =
                 HtmlValidator.with_render_budget(fn ->
                   HtmlCssSourceValidator.map_span(descriptor, at, size)
                 end)

        assert diagnostic.stage == :html
        assert diagnostic.reason == :invalid_document
        assert diagnostic.module == HtmlCssSourceValidator
        assert diagnostic.message =~ "outside"
      end

      assert HtmlCssSourceValidator.map_span(descriptor, 4, 0).length == 0
      assert HtmlCssSourceValidator.map_span(descriptor, 4, 4).text == "🙂"
    end

    assert {:error, {:invalid_document, _}} =
             HtmlValidator.with_render_budget(fn ->
               HtmlCssSourceValidator.map_span(nil, 0, 0)
             end)
  end

  test "mapping work is cumulative and exhausted before original diagnostic scans" do
    assert {:ok, dom} = HtmlParser.parse_document_detailed("<p style='width:bananas'>Bad</p>")
    assert {:ok, provenance} = HtmlCssSourceValidator.prepare(dom)
    descriptor = provenance.inline |> Map.values() |> hd()
    Limits.install(Map.put(Limits.effective(), :max_html_tree_work, 1))

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             HtmlValidator.with_render_budget(fn ->
               HtmlCssSourceValidator.map_span(descriptor, 0, 13)
             end)

    assert diagnostic.message =~ "max_html_tree_work"
    assert {:error, {:resource_limit_exceeded, _}} = collect(dom)

    scans = [{String, :split, 3}, {String, :length, 1}, {String, :valid?, 1}]
    Enum.each(scans, fn mfa -> assert :erlang.trace_pattern(mfa, true, [:local]) == 1 end)

    task =
      Task.async(fn ->
        receive do
          :map ->
            HtmlValidator.with_render_budget(fn ->
              HtmlCssSourceValidator.map_span(descriptor, 0, 13)
            end)
        end
      end)

    try do
      :erlang.trace(task.pid, true, [:call])
      send(task.pid, :map)
      assert {:error, {:resource_limit_exceeded, _}} = Task.await(task)
      refute_receive {:trace, _, :call, _}
    after
      Enum.each(scans, &:erlang.trace_pattern(&1, false, [:local]))
    end
  end

  test "each authored declaration retains the original contexts that completed shared validation" do
    html = "<div style='--x:5pt'><p style='width:var(--x)'>A</p><p>B</p></div>"
    css = "p{color:red;color:blue}p::before{content:'Before'}.missing{width:var(--missing)}"
    assert {:ok, dom} = HtmlParser.parse_document_detailed(html)
    assert {:ok, result, _} = collect(dom, [css, css])
    obligations = result.source_obligations.declaration_contexts
    assert length(obligations) == 10

    for index <- [0, 1] do
      records = Enum.filter(obligations, &(&1.origin == {:stylesheet, index}))

      assert Enum.map(records, & &1.source.text) == [
               "color:red",
               "color:blue",
               "content:'Before'",
               "width:var(--missing)"
             ]

      assert Enum.map(records, & &1.status) == [
               :checked_in_original_context,
               :checked_in_original_context,
               :checked_in_original_context,
               :unresolved
             ]

      [first, overwritten, pseudo, missing] = records
      assert length(first.contexts) == 2 and length(overwritten.contexts) == 2
      assert first.contexts |> Enum.map(& &1.owner.node_id) |> Enum.uniq() |> length() == 2
      assert Enum.all?(first.contexts, &(&1.pseudo == nil and &1.tag == "p"))
      assert length(pseudo.contexts) == 2 and Enum.all?(pseudo.contexts, &(&1.pseudo == :before))
      assert missing.reason == :unmatched_original_context and missing.contexts == []
      assert first.source.provenance == :unbound and first.source.owner == nil
    end

    [div] = dom.children |> hd() |> Map.fetch!(:children) |> List.last() |> Map.fetch!(:children)
    [first, _second] = div.children
    width = Enum.find(obligations, &(&1.declaration == {"width", "var(--x)"}))
    assert width.origin == {:inline, first.node_id}
    assert width.status == :checked_in_original_context
    assert [context] = width.contexts
    assert context.owner == Map.take(first, [:node_id, :document_id, :tree_scope_id, :parent_id])
    assert context.source == first.source and context.pseudo == nil
    assert width.source.owner == context.owner
    refute Map.has_key?(width, :context_id)
    refute Map.has_key?(context, :style)

    refute Map.has_key?(
             hd(result.source_obligations.authored).authored_rules
             |> hd()
             |> Map.fetch!(:source_declarations)
             |> hd(),
             :context_id
           )
  end

  test "inactive and detached CSS records retain exact source without invented insertion inheritance" do
    html =
      "<html><head><style media='screen'>p{height:var(--screen)}</style></head><body><div style='--x:8pt'><template><style>p{color:var(--insertion)}</style><p style='width&colon;var(--x)'>Detached</p><template><p style='height:var(--nested)'>Nested</p></template></template></div></body></html>"

    css = "@media screen{p{width:var(--screen)}}"
    assert {:ok, dom} = HtmlParser.parse_document_detailed(html)
    assert {:ok, result, contexts} = collect(dom, [css])
    obligations = result.source_obligations.declaration_contexts
    assert length(obligations) == 6
    unresolved = Enum.filter(obligations, &(&1.status == :unresolved))
    assert length(unresolved) == 5

    assert Enum.map(unresolved, & &1.reason) == [
             :inactive_media,
             :inapplicable_stylesheet,
             :template_insertion_context,
             :template_insertion_context,
             :template_insertion_context
           ]

    assert Enum.all?(unresolved, &(&1.contexts == []))
    assert result.source_obligations.template_insertion_contexts == :unresolved

    assert result.runtime_obligations == [
             :font_selection,
             :font_metrics,
             :text_decorations,
             :resources
           ]

    detached = Enum.find(unresolved, &(&1.source.text == "width&colon;var(--x)"))
    assert detached.source.provenance == :original_html

    assert binary_part(html, detached.source.offset, detached.source.length) ==
             detached.source.text

    {node, nil, context} =
      Enum.find(contexts, fn {node, _, _} -> Map.get(node, :text) == "Detached" end)

    assert context.status == :unresolved
    assert detached.source.owner.tree_scope_id == node.tree_scope_id
    assert detached.source.owner.document_id == dom.document_id
    refute detached.source.owner.tree_scope_id == dom.tree_scope_id
  end

  test "detached source syntax and static URL failures retain original actionable diagnostics" do
    for {html, reason, source, line, column} <- [
          {"<template><p style='color:red;\r\n  color'>Inert</p></template>", :invalid_css,
           "color", 2, 3},
          {"<template><style>p{color:red;\r\n  color}</style></template>", :invalid_css, "color",
           2, 3},
          {"<template><style>p{background-image:url('javascript:run()')}</style></template>",
           :unsupported_html, "url('javascript:run()')", 1, 37}
        ] do
      assert {:ok, dom} = HtmlParser.parse_document_detailed(html)
      assert {:error, {^reason, diagnostic}} = collect(dom)
      assert diagnostic.stage == :css and diagnostic.reason == reason
      assert diagnostic.source == source and diagnostic.message != ""
      assert diagnostic.line == line and diagnostic.column == column
    end
  end

  test "context collectors isolate nested traversals and clean up after consumer failures" do
    assert {:ok, outer} = HtmlParser.parse_document_detailed("<p style='width:3pt'>Outer</p>")
    assert {:ok, inner} = HtmlParser.parse_document_detailed("<p style='height:4pt'>Inner</p>")
    keys_before = Process.get_keys() |> MapSet.new()

    assert {:ok, result} =
             Style.with_source_style_contexts(outer, [], HtmlValidator.new_css_budget(), fn _,
                                                                                            _,
                                                                                            _ ->
               assert {:ok, nested, _} = collect(inner)
               assert [record] = nested.source_obligations.declaration_contexts
               assert record.declaration == {"height", "4pt"}
               :ok
             end)

    assert [record] = result.source_obligations.declaration_contexts
    assert record.declaration == {"width", "3pt"} and length(record.contexts) == 1
    assert MapSet.new(Process.get_keys()) == keys_before

    error =
      Diagnostics.error(:html, :invalid_document, "stop source traversal", source: "original")

    assert ^error =
             Style.with_source_style_contexts(outer, [], HtmlValidator.new_css_budget(), fn _,
                                                                                            _,
                                                                                            _ ->
               error
             end)

    assert MapSet.new(Process.get_keys()) == keys_before

    assert_raise RuntimeError, "consumer exception", fn ->
      Style.with_source_style_contexts(outer, [], HtmlValidator.new_css_budget(), fn _, _, _ ->
        raise "consumer exception"
      end)
    end

    assert MapSet.new(Process.get_keys()) == keys_before
    assert {:ok, _, _} = collect(outer)
  end

  test "unresolved MathML and empty inline blocks do not acquire a fabricated check" do
    assert {:ok, dom} =
             HtmlParser.parse_document_detailed(
               "<p style=''>Empty</p><math><mi style='color:var(--math)'>x</mi></math>"
             )

    assert {:ok, result, _} = collect(dom)
    assert [record] = result.source_obligations.declaration_contexts
    assert record.declaration == {"color", "var(--math)"}
    assert record.status == :unresolved and record.reason == :math_font_context
    assert record.contexts == []

    assert {:ok, dom} =
             HtmlParser.parse_document_detailed(
               "<template><p style='width:3pt'>Inert</p></template>"
             )

    [html] = dom.children
    [head, body] = html.children
    [template] = head.children
    [child] = template.content.children
    forged = %{child | attributes: %{child.attributes | "style" => "width:4pt"}}
    template = %{template | content: %{template.content | children: [forged]}}
    forged = %{dom | children: [%{html | children: [%{head | children: [template]}, body]}]}
    assert {:error, {:invalid_document, diagnostic}} = collect(forged)
    assert diagnostic.stage == :html and diagnostic.module == HtmlCssSourceValidator
    assert diagnostic.source == child.source.text
    assert diagnostic.message =~ "decoded attributes"
  end

  test "obligation allocation and every observation remain charged to the shared CSS work budget" do
    record = %{declaration: {"color", "red"}, source: %{text: "color:red", offset: 0}}
    id = make_ref()
    provenance = %{records: %{id => [record]}, insertion_contexts: %{}}
    node = %{tag: "p", node_id: id, source: %{text: "<p>"}}
    limits = Limits.effective()
    Limits.install(Map.put(limits, :max_css_work, 2))

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             CssSourceContextValidator.with_context_obligations([], provenance, fn _,
                                                                                   inline,
                                                                                   observe ->
               [record] = inline[id]
               :ok = observe.(record, node, nil)
               {:ok, :done}
             end)

    assert diagnostic.stage == :css and diagnostic.reason == :resource_limit_exceeded
    assert diagnostic.message =~ "max_css_work"

    Limits.install(Map.put(limits, :max_css_work, 4))

    assert {:ok, :done, [obligation]} =
             HtmlValidator.with_render_budget(fn ->
               CssSourceContextValidator.with_context_obligations([], provenance, fn _,
                                                                                     inline,
                                                                                     observe ->
                 [record] = inline[id]
                 :ok = observe.(record, node, nil)
                 :ok = observe.({"generated", "hint"}, node, nil)
                 {:ok, :done}
               end)
             end)

    assert obligation.status == :checked_in_original_context and length(obligation.contexts) == 1

    assert {:error, {:resource_limit_exceeded, _}} =
             HtmlValidator.with_render_budget(fn ->
               for _ <- 1..2 do
                 CssSourceContextValidator.with_context_obligations([], provenance, fn _,
                                                                                       inline,
                                                                                       observe ->
                   :ok = observe.(hd(inline[id]), node, nil)
                   {:ok, :done}
                 end)
               end
             end)
  end

  defp edit_elements(node, edit) do
    children = Enum.map(Map.get(node, :children, []), &edit_elements(&1, edit))
    node = if Map.has_key?(node, :children), do: %{node | children: children}, else: node
    if node.type == :element, do: edit.(node), else: node
  end

  defp strip_identity(node) do
    children = Enum.map(Map.get(node, :children, []), &strip_identity/1)
    node = Map.drop(node, [:node_id, :document_id, :tree_scope_id, :parent_id])
    if Map.has_key?(node, :children), do: %{node | children: children}, else: node
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
