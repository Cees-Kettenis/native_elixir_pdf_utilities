defmodule NativeElixirPdfUtilities.HtmlToPdf.OutputDatalistTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.{Forms, HtmlToPdf, Limits, Text}
  alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, Style}
  alias NativeElixirPdfUtilities.Pdf.Reader
  alias NativeElixirPdfUtilities.Validators.{FormValidator, HtmlFormValidator, HtmlValidator}

  test "output keeps descendant text, default value and ordered unique ASCII source IDs" do
    nodes =
      initial_nodes(
        "<output id=result for='b a b missing a\u00a0b'> 12<span hidden>\n H </span><b>34</b><br><template>Detached</template><!-- comment --></output><input id=a><span id=b>Value</span>"
      )

    result = node(nodes, "result")
    state = result._initial_form_state
    assert state.value == " 12\n H 34"
    assert state.default_value == state.value and state.default_value_override == nil
    assert state.source_tokens == ["b", "a", "missing", "a\u00a0b"]

    assert Enum.map(state.sources, & &1.ref) == [
             node(nodes, "b")._selector_id,
             node(nodes, "a")._selector_id,
             nil,
             nil
           ]

    assert hd(state.sources).source_id == node(nodes, "b").node_id
    assert hd(state.sources).scope == {result.document_id, result.tree_scope_id}
    assert result.attributes["for"] == "b a b missing a\u00a0b"
  end

  test "output form ownership and labels share first-ID association without fieldset disabling" do
    nodes =
      initial_nodes(
        "<label for=result>Result label</label><form id=main><fieldset disabled><output id=result for=later>7</output><label id=implicit><output id=nested>8</output></label><output id=explicit form=next>9</output></fieldset></form><form id=next></form><input id=later>"
      )

    result = node(nodes, "result")
    assert result._initial_form_state.owner == node(nodes, "main")._selector_id
    refute result._initial_form_state.disabled
    refute HtmlValidator.control_state_matches?(:disabled, result)
    refute HtmlValidator.control_state_matches?(:enabled, result)
    assert node(nodes, "explicit")._initial_form_state.owner == node(nodes, "next")._selector_id
    assert node(nodes, "implicit")._initial_label_control == node(nodes, "nested")._selector_id

    label =
      Enum.find(
        all_nodes(nodes),
        &(Map.get(&1, :tag) == "label" and &1.attributes["for"] == "result")
      )

    assert label._initial_label_control == result._selector_id
    assert hd(result._initial_form_state.sources).ref == node(nodes, "later")._selector_id

    nodes =
      initial_nodes(
        "<span id=blocked></span><form id=blocked></form><output id=result form=blocked>0</output>"
      )

    assert node(nodes, "result")._initial_form_state.owner == nil
  end

  test "file and output labels use first labelable target while datalist has no owner" do
    nodes =
      initial_nodes(
        "<form id=main><fieldset disabled><datalist id=choices><option value=x></option></datalist><label id=hidden-label for=hidden>Hidden<input id=nested type=file></label><input id=hidden type=hidden><label id=empty-label for=''><input id=ignored type=file></label><label id=file-label><input id=file type=file></label><span id=blocked></span><output id=blocked>8</output><label id=blocked-label for=blocked>Blocked</label></fieldset></form>"
      )

    assert node(nodes, "choices")._initial_form_state.owner == nil
    refute node(nodes, "choices")._initial_form_state.disabled
    assert node(nodes, "hidden-label")._initial_label_control == nil
    assert node(nodes, "empty-label")._initial_label_control == nil
    assert node(nodes, "blocked-label")._initial_label_control == nil
    assert node(nodes, "file-label")._initial_label_control == node(nodes, "file")._selector_id
    assert node(nodes, "file")._initial_form_state.owner == node(nodes, "main")._selector_id
    assert node(nodes, "file")._initial_form_state.disabled
  end

  test "unowned radio selection stays within each source tree scope" do
    assert {:ok, left} =
             HtmlParser.parse_detailed(
               "<input id=left type=radio name=group checked><input id=left-other type=radio name=group>"
             )

    assert {:ok, right} =
             HtmlParser.parse_detailed(
               "<input id=right type=radio name=group checked><input id=right-other type=radio name=group>"
             )

    nodes = HtmlFormValidator.prepare_initial_nodes(left.children ++ right.children)
    assert node(nodes, "left")._initial_form_state.checked
    assert node(nodes, "right")._initial_form_state.checked
    refute node(nodes, "left-other")._initial_form_state.indeterminate
    refute node(nodes, "right-other")._initial_form_state.indeterminate
    assert node(nodes, "left").document_id != node(nodes, "right").document_id
  end

  test "datalist preserves complete options and suggestions independently of selectedness and source value" do
    nodes =
      initial_nodes(
        "<input id=entry list=choices value=untouched readonly><datalist id=choices><option value=a label=Alpha selected>Ignored label</option><option value=''>Empty</option><option value=b disabled>Beta</option><option>  Gamma\t Delta  </option><option hidden value=c>Hidden candidate</option><option value=d label=''>Fallback label</option></datalist>"
      )

    list = node(nodes, "choices")
    state = list._initial_form_state
    assert Enum.map(state.options, & &1.value) == ["a", "", "b", "Gamma Delta", "c", "d"]
    assert Enum.map(state.suggestions, & &1.value) == ["a", "Gamma Delta", "c", "d"]

    assert Enum.map(state.options, & &1.label) == [
             "Alpha",
             "Empty",
             "Beta",
             "Gamma Delta",
             "Hidden candidate",
             "Fallback label"
           ]

    entry = node(nodes, "entry")
    assert entry._initial_form_state.value == "untouched"
    assert entry._initial_form_state.datalist.ref == list._selector_id
    assert entry._initial_form_state.datalist.source_id == list.node_id
    assert entry.attributes["value"] == "untouched"
    first = hd(list.children)
    assert HtmlValidator.control_state_matches?(:checked, first)
    disabled = Enum.at(list.children, 2)
    assert HtmlValidator.control_state_matches?(:disabled, disabled)
  end

  test "legacy hidden fallback select contributes descendant options without selecting suggestions" do
    nodes =
      initial_nodes(
        "<datalist id=legacy><label>Fallback<select><option value=z>Zulu</option><optgroup label=Blocked disabled><option value=x>Xray</option></optgroup><option value=y>Yankee</option></select></label></datalist>"
      )

    state = node(nodes, "legacy")._initial_form_state
    assert Enum.map(state.options, & &1.value) == ["z", "x", "y"]
    assert Enum.map(state.suggestions, & &1.value) == ["z", "y"]
    assert Enum.map(state.options, & &1.disabled) == [false, true, false]
  end

  test "list requires the first exact-case ID to be an HTML datalist and never crosses a tree scope" do
    nodes =
      initial_nodes(
        "<input id=wrong list=blocked readonly><span id=blocked></span><datalist id=blocked><option value=x></option></datalist><input id=case list=Case readonly><datalist id=case><option value=y></option></datalist><input id=right list=later readonly><datalist id=later><option value=z></option></datalist>"
      )

    assert node(nodes, "wrong")._initial_form_state.datalist == nil
    assert node(nodes, "case")._initial_form_state.datalist == nil

    assert node(nodes, "right")._initial_form_state.datalist.ref ==
             node(nodes, "later")._selector_id

    assert {:ok, input_dom} =
             HtmlParser.parse_detailed(
               "<input id=entry list=shared readonly><output id=result for=shared>0</output>"
             )

    assert {:ok, list_dom} =
             HtmlParser.parse_detailed("<datalist id=shared><option value=x></option></datalist>")

    nodes = HtmlFormValidator.prepare_initial_nodes(input_dom.children ++ list_dom.children)
    assert node(nodes, "entry")._initial_form_state.datalist == nil
    assert hd(node(nodes, "result")._initial_form_state.sources).ref == nil
  end

  test "default hidden datalist does not paint fallback text or create widgets, output remains ordinary text" do
    html =
      "<form><output name=result id=result>First <b>strong</b> result</output><datalist id=choices><label>Hidden fallback<select><option>Hidden choice</option></select><input name=hidden></label></datalist></form>"

    assert {:ok, dom} = HtmlParser.parse_detailed(html)
    assert {:ok, styled} = Style.compute_detailed(dom)
    list = Enum.find(all_nodes(styled.children), &(Map.get(&1, :tag) == "datalist"))
    output = Enum.find(all_nodes(styled.children), &(Map.get(&1, :tag) == "output"))
    assert list.style.display == :none and list.children == []
    assert Enum.map(list.style._datalist_state.options, & &1.value) == ["Hidden choice"]
    assert output.style._output_state.value == "First strong result"
    refute Map.has_key?(output.style, :_form_control)
    assert {:ok, pdf} = HtmlToPdf.render(html)
    assert {:ok, text} = Text.extract(pdf, layout: false)
    assert text =~ "First" and text =~ "strong" and text =~ "result"
    refute text =~ "Hidden"
    assert {:ok, context} = Reader.read_validated(pdf)
    assert {:ok, %{fields: []}} = FormValidator.inspect_document(context)
  end

  test "readonly datalist scalar values retain exact text and can be flattened without suggestion actions" do
    html =
      "<input name=entry list=choices value=authored readonly><datalist id=choices><option value=other>Other</option></datalist>"

    assert {:ok, pdf} = HtmlToPdf.render(html)
    assert {:ok, context} = Reader.read_validated(pdf)
    assert {:ok, %{fields: [field]}} = FormValidator.inspect_document(context)
    assert field.value == "authored" and Bitwise.band(field.flags, 1) == 1
    assert {:ok, _} = Forms.flatten(pdf)
  end

  test "inapplicable output attributes and malformed content return public diagnostics" do
    for html <- [
          "<output value=1>2</output>",
          "<output disabled>2</output>",
          "<output required>2</output>",
          "<output><div>Block</div></output>",
          "<datalist name=choices></datalist>",
          "<datalist><option value=x></option><span>Mixed</span></datalist>",
          "<input type=password list=choices>"
        ] do
      assert {:error, {:unsupported_html, diagnostic}} = HtmlToPdf.render(html, forms: :static)
      assert diagnostic.stage == :html and diagnostic.operation == :render
      assert diagnostic.message != ""
    end
  end

  test "visible datalist overrides, editable suggestion art and unresolved PDF actions remain diagnosed" do
    for html <- [
          "<datalist style=display:block><option value=x></option></datalist>",
          "<input list=choices><datalist id=choices><option value=x></option></datalist>"
        ] do
      assert {:error, {:unsupported_html, diagnostic}} = HtmlToPdf.render(html, forms: :static)
      assert diagnostic.stage == :style and diagnostic.operation == :compute

      assert diagnostic.source in ["datalist", "input/list"] and
               diagnostic.message =~ "not yet supported"
    end

    for html <- [
          "<input name=entry list=missing value=authored>",
          "<input name=entry list=blocked value=authored><span id=blocked></span><datalist id=blocked><option value=other></option></datalist>"
        ] do
      assert {:ok, pdf} = HtmlToPdf.render(html)
      assert {:ok, context} = Reader.read_validated(pdf)
      assert {:ok, %{fields: [field]}} = FormValidator.inspect_document(context)
      assert field.value == "authored" and Bitwise.band(field.flags, 1) == 0
    end

    assert {:ok, _} = HtmlToPdf.render("<input list=missing>", forms: :static)

    assert {:ok, dom} =
             HtmlParser.parse_detailed(
               "<input name=entry list=choices value=authored readonly><datalist id=choices><option value=other></option></datalist>"
             )

    assert {:ok, %{children: [entry, list]} = styled} = Style.compute_detailed(dom)
    mutable = update_in(entry.style._form_control.attributes, &Map.delete(&1, "readonly"))

    assert {:ok, layout} =
             NativeElixirPdfUtilities.HtmlToPdf.Layout.layout(%{
               styled
               | children: [mutable, list]
             })

    assert {:ok, pages} = NativeElixirPdfUtilities.HtmlToPdf.Pagination.paginate(layout)

    assert {:error, {:unsupported_form, diagnostic}} =
             HtmlFormValidator.prepare(pages, forms: :interactive)

    assert diagnostic.stage == :forms and diagnostic.source == "input/list"

    assert {:ok, _} =
             HtmlToPdf.render(
               "<input list=choices style=display:none><datalist id=choices><option value=x></option></datalist>"
             )

    assert {:error, {:unsupported_html, %{source: "range/list"}}} =
             HtmlToPdf.render(
               "<input type=range list=choices><datalist id=choices><option value=50></option></datalist>",
               forms: :static
             )
  end

  test "source-only style observation uses the same source associations without changing original identities" do
    assert {:ok, dom} =
             HtmlParser.parse_detailed(
               "<output id=result for=source>4</output><input id=entry list=choices readonly><span id=source>4</span><datalist id=choices><option value=other></option></datalist>"
             )

    originals =
      Map.new(all_nodes(dom.children), fn n ->
        {Map.get(n, :node_id),
         Map.take(n, [:node_id, :document_id, :tree_scope_id, :parent_id, :source])}
      end)

    parent = self()

    assert {:ok, _} =
             Style.with_source_style_contexts(dom, [], HtmlValidator.new_css_budget(), fn n,
                                                                                          pseudo,
                                                                                          _ctx ->
               if pseudo == nil and Map.has_key?(n, :_initial_form_state),
                 do: send(parent, {:prepared_source, n})

               :ok
             end)

    assert_receive {:prepared_source, %{tag: "output", _initial_form_state: state} = output}

    assert hd(state.sources).source_id ==
             Enum.find(all_nodes(dom.children), &(&1.attributes["id"] == "source")).node_id

    assert Map.take(output, [:node_id, :document_id, :tree_scope_id, :parent_id, :source]) ==
             originals[output.node_id]

    assert_receive {:prepared_source, %{tag: "input", _initial_form_state: input_state}}

    assert input_state.datalist.source_id ==
             Enum.find(all_nodes(dom.children), &(&1.attributes["id"] == "choices")).node_id
  end

  test "semantic descendant work uses the shared cumulative tree budget" do
    previous = Limits.effective()
    on_exit(fn -> Limits.install(previous) end)
    Limits.install(%{previous | max_html_tree_work: 20})
    assert Limits.get(:max_html_tree_work) == 20

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             HtmlValidator.with_render_budget(fn ->
               HtmlValidator.descendant_text_content([%{type: :text, text: "abcdefghij"}])
             end)

    assert diagnostic.stage == :style and diagnostic.message =~ "max_html_tree_work"
  end

  defp initial_nodes(html) do
    assert {:ok, dom} = HtmlParser.parse_detailed(html)
    HtmlFormValidator.prepare_initial_nodes(dom.children)
  end

  defp all_nodes(nodes) do
    Stream.iterate(nodes, &Enum.flat_map(&1, fn n -> Map.get(n, :children, []) end))
    |> Enum.take_while(&(&1 != []))
    |> List.flatten()
  end

  defp node(nodes, id),
    do: Enum.find(all_nodes(nodes), &(Map.get(&1, :attributes, %{})["id"] == id))
end
