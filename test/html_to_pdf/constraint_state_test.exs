defmodule NativeElixirPdfUtilities.HtmlToPdf.ConstraintStateTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.HtmlToPdf.HtmlParser
  alias NativeElixirPdfUtilities.Validators.HtmlFormValidator

  test "initial email type mismatch follows ASCII HTML grammar and multiple token sanitation" do
    for {value, extra, mismatch} <- [
          {"a@b", "", false},
          {"..a..@b", "", false},
          {"bad", "", true},
          {"a@-b", "", true},
          {"a@b.", "", true},
          {"a@" <> String.duplicate("b", 63), "", false},
          {"a@" <> String.duplicate("b", 64), "", true},
          {"a@ümlaut.test", "", true},
          {"a@xn--mlaut-jva.test", "", false},
          {"a@b, c@d", "multiple", false},
          {",", "multiple", true},
          {"a@b,", "multiple", true},
          {"", "required multiple", false},
          {"bad", "disabled", true},
          {"bad", "readonly", true}
        ] do
      state =
        initial("<input id=control type=email value='#{value}' #{extra}>")["control"]

      assert state.constraint_validation.checks.type_mismatch == mismatch
    end

    state =
      initial("<input id=control type=email multiple value=' a&#10;@b , c&#13;@d '>")["control"]

    assert state.value == "a@b,c@d"
    refute state.constraint_validation.checks.type_mismatch

    assert {:ok, dom} =
             HtmlParser.parse_detailed(
               "<input type=email multiple value=' a&#10;@b , c&#13;@d '>"
             )

    assert hd(dom.children).attributes["value"] == " a\n@b , c\r@d "
  end

  test "summary reports actual validity separately from validation candidacy" do
    states =
      initial(
        "<input id=valid value=Ready><input id=missing required><input id=barred type=email value=bad readonly><input id=pattern value=Ready pattern=R.*><input id=empty pattern=x><input id=url type=url value=https://example.test><input id=url_empty type=url>"
      )

    assert states["valid"].constraint_validation.valid
    refute states["missing"].constraint_validation.valid
    refute states["barred"].constraint_validation.valid
    refute states["barred"].constraint_validation.will_validate
    assert states["pattern"].constraint_validation.valid
    refute states["pattern"].constraint_validation.checks.pattern_mismatch
    assert states["empty"].constraint_validation.valid
    assert states["url"].constraint_validation.valid
    refute states["url"].constraint_validation.checks.type_mismatch
    assert states["url_empty"].constraint_validation.valid
  end

  test "initial v patterns match original full-string and multiple-email semantics" do
    states =
      initial(
        File.read!(
          Path.expand(
            "../fixtures/html_to_pdf/browser_parity/html_initial_constraint_values.html",
            __DIR__
          )
        )
      )

    for {id, pattern_mismatch, type_mismatch, candidate} <- [
          {"intersection", false, false, true},
          {"subtraction", true, false, true},
          {"strings", false, false, true},
          {"emoji", false, false, true},
          {"anchored", true, false, true},
          {"backref", false, false, true},
          {"lookahead", false, false, true},
          {"invalid_pattern", false, false, true},
          {"pattern_email", false, false, true},
          {"email_newlines", false, false, true},
          {"email_bad", false, true, false},
          {"url_opaque", false, false, true},
          {"url_special", false, false, true},
          {"url_bad", false, true, true}
        ] do
      constraint = states[id].constraint_validation
      assert constraint.will_validate == candidate, id
      assert constraint.checks.pattern_mismatch == pattern_mismatch, id
      assert constraint.checks.type_mismatch == type_mismatch, id
      assert constraint.valid == not (pattern_mismatch or type_mismatch), id
    end

    states =
      initial(
        "<input id=separator pattern=a value='a&#x2028;'><input id=legacy_invalid pattern='[()]' value=x><input id=multiple_bad type=email multiple pattern='a@b' value='a@b,c@d'><input id=disabled pattern=a value=b disabled><input id=readonly pattern=a value=b readonly>"
      )

    assert states["separator"].constraint_validation.checks.pattern_mismatch
    refute states["legacy_invalid"].constraint_validation.checks.pattern_mismatch
    assert states["multiple_bad"].constraint_validation.checks.pattern_mismatch

    for id <- ["disabled", "readonly"] do
      assert states[id].constraint_validation.checks.pattern_mismatch
      refute states[id].constraint_validation.will_validate
      refute states[id].constraint_validation.valid
    end
  end

  test "initial required state distinguishes candidacy and type-specific mutability" do
    for {attrs, candidate, missing} <- [
          {"required", true, true},
          {"required disabled", false, false},
          {"required readonly", false, false},
          {"required hidden", true, true},
          {"type=hidden", false, false},
          {"type=file required", true, true},
          {"type=file required disabled", false, true},
          {"type=checkbox required", true, true},
          {"type=checkbox required disabled", false, true},
          {"type=checkbox required checked", true, false},
          {"type=radio required", true, false},
          {"type=submit", true, false},
          {"type=reset", false, false},
          {"type=image", false, false}
        ] do
      state = initial("<input id=control #{attrs}>") |> Map.fetch!("control")
      assert state.constraint_validation.will_validate == candidate, attrs
      assert state.constraint_validation.checks.value_missing == missing, attrs
    end
  end

  test "radio requirement aggregates every peer but stays within owner and source scope" do
    states =
      initial(
        "<input id=a type=radio name=g required><input id=b type=radio name=g><input id=c type=radio name=h required disabled><input id=d type=radio name=h><form id=other><input id=e type=radio name=g checked></form>"
      )

    for id <- ~w(a b c d), do: assert(states[id].constraint_validation.checks.value_missing)
    refute states["c"].constraint_validation.will_validate
    assert states["d"].constraint_validation.will_validate
    refute states["e"].constraint_validation.checks.value_missing

    states =
      initial("<input id=a type=radio name=g required><input id=b type=radio name=g checked>")

    for id <- ~w(a b), do: refute(states[id].constraint_validation.checks.value_missing)
  end

  test "select placeholder label option differs from later and optgroup empty values" do
    for {html, missing} <- [
          {"<select required><option value=''>Prompt</option><option value=x>X</option></select>",
           true},
          {"<select required><option value=x>X</option><option value='' selected>Empty</option></select>",
           false},
          {"<select required><optgroup label=Group><option value=''>Empty</option></optgroup></select>",
           false},
          {"<select required disabled><option value=''>Prompt</option></select>", true},
          {"<select required multiple><option value=x>X</option></select>", true},
          {"<select required size=2><option value=''>Empty</option></select>", true}
        ] do
      assert {:ok, dom} = HtmlParser.parse_detailed(html)
      [node] = HtmlFormValidator.prepare_initial_nodes(dom.children)
      assert node._initial_form_state.constraint_validation.checks.value_missing == missing, html
    end
  end

  test "logical unassigned radio selection satisfies a projected required peer" do
    assert {:ok, dom} =
             HtmlParser.parse_detailed(
               "<div><template shadowrootmode=open><slot name=visible></slot></template><input id=visible slot=visible type=radio name=g required><input id=unassigned type=radio name=g checked></div>"
             )

    assert {:ok, composition} = NativeElixirPdfUtilities.HtmlToPdf.HtmlComposition.compose(dom)

    nodes =
      HtmlFormValidator.prepare_composed_initial_nodes(
        dom,
        composition.roots,
        composition.composed.children,
        %{}
      )

    [host] = nodes
    [slot] = host.children
    [visible] = slot.children
    refute visible._initial_form_state.checked
    refute visible._initial_form_state.constraint_validation.checks.value_missing
    assert visible._initial_form_state.constraint_validation.valid
  end

  test "numeric bounds and calendar steps stay visible state even when barred from validation" do
    states =
      initial(
        "<input id=number type=number value=3 min=5 step=2 readonly><input id=step type=number value=.5 min=0><input id=precise type=number value=.30000000000000004 min=0 step=.1><input id=negative type=number value=.2 min=-.2 step=.3><input id=bad type=number value=bad required><input id=date type=date value=2026-10-13 min=2026-10-12 step=2><input id=time type=time value=12:00 min=22:00 max=02:00>"
      )

    refute states["number"].constraint_validation.will_validate
    assert states["number"].constraint_validation.checks.range_underflow
    assert states["step"].constraint_validation.checks.step_mismatch
    refute states["precise"].constraint_validation.checks.step_mismatch
    assert states["negative"].constraint_validation.checks.step_mismatch
    assert states["bad"].constraint_validation.checks.value_missing
    refute states["bad"].constraint_validation.checks.bad_input
    assert states["date"].constraint_validation.checks.step_mismatch
    assert states["time"].constraint_validation.checks.range_underflow
    assert states["time"].constraint_validation.checks.range_overflow
  end

  test "datalist descendants and fieldset legends retain separate barred and disabled state" do
    states =
      initial(
        "<datalist><input id=inside required></datalist><fieldset disabled><legend><input id=legend required></legend><input id=disabled required></fieldset><textarea id=long maxlength=1 minlength=5>Authored initial text</textarea>"
      )

    refute states["inside"].constraint_validation.will_validate
    assert states["inside"].constraint_validation.checks.value_missing
    assert states["legend"].constraint_validation.will_validate
    assert states["legend"].constraint_validation.checks.value_missing
    refute states["disabled"].constraint_validation.will_validate
    refute states["disabled"].constraint_validation.checks.value_missing
    refute states["long"].constraint_validation.checks.too_long
    refute states["long"].constraint_validation.checks.too_short
  end

  test "public writer rejects malformed or contradictory transported constraint state" do
    alias NativeElixirPdfUtilities.HtmlToPdf.{Layout, Pagination, PdfWriter, Style}
    assert {:ok, dom} = HtmlParser.parse_detailed("<input required>")
    assert {:ok, styled} = Style.compute_detailed(dom, forms: :static)
    assert {:ok, layout} = Layout.layout(styled)
    assert {:ok, pages} = Pagination.paginate(layout)
    [page] = pages
    index = Enum.find_index(page.boxes, &Map.has_key?(&1, :form_control))
    box = Enum.at(page.boxes, index)
    constraint = box.form_control.initial_state.constraint_validation
    refute constraint.valid
    assert constraint.checks.value_missing

    for forged <- [
          nil,
          %{constraint | valid: true},
          %{constraint | will_validate: "true"},
          %{constraint | checks: Map.delete(constraint.checks, :bad_input)},
          %{constraint | checks: Map.put(constraint.checks, :bad_input, 1)}
        ] do
      box = put_in(box.form_control.initial_state.constraint_validation, forged)
      page = %{page | boxes: List.replace_at(page.boxes, index, box)}
      assert {:error, {:invalid_form, diagnostic}} = PdfWriter.render([page], forms: :static)
      assert diagnostic.stage == :forms and diagnostic.operation == :write_pdf
      assert diagnostic.module == HtmlFormValidator and diagnostic.source == "initial_state"
      assert diagnostic.message =~ "constraint flags"
    end
  end

  test "public pattern exhaustion is actionable rather than an invented mismatch" do
    previous = NativeElixirPdfUtilities.Limits.effective()
    on_exit(fn -> NativeElixirPdfUtilities.Limits.install(previous) end)
    NativeElixirPdfUtilities.Limits.install(%{previous | max_html_pattern_compile_work: 1})

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             NativeElixirPdfUtilities.HtmlToPdf.render("<input pattern='[a-z]+' value=abc>",
               forms: :static
             )

    assert diagnostic.stage == :html and diagnostic.operation == :initial_form_value
    assert diagnostic.module == NativeElixirPdfUtilities.Validators.HtmlFormValueValidator
    assert diagnostic.source == "input/pattern"
    assert diagnostic.message =~ "max_html_pattern_compile_work"
  end

  test "form ownership and fieldset ancestry aggregate only candidate failures" do
    states =
      initial(
        "<form id=owner></form><form id=other><fieldset id=group><input id=external form=owner required></fieldset><input type=email value=bad readonly></form><input form=missing required><fieldset id=empty></fieldset><form id=none></form>"
      )

    refute states["owner"].aggregate_valid
    assert states["other"].aggregate_valid
    refute states["group"].aggregate_valid
    assert states["group"].constraint_validation.valid
    refute states["group"].constraint_validation.will_validate

    assert Enum.all?(states["group"].constraint_validation.checks, fn {_, flag} ->
             flag == false
           end)

    assert states["empty"].aggregate_valid and states["none"].aggregate_valid
    assert states["external"].owner != nil

    for {legend, valid} <- [
          {"<legend><input required></legend>", false},
          {"<legend>First</legend><legend><input required></legend>", true},
          {"<legend><input required disabled></legend>", true}
        ] do
      states =
        initial("<form id=f><fieldset id=g disabled>#{legend}<input required></fieldset></form>")

      assert states["f"].aggregate_valid == valid
      assert states["g"].aggregate_valid == valid
      assert states["g"].constraint_validation.valid
    end
  end

  test "validity selectors delegate candidacy and aggregate semantics while source attributes remain authored" do
    alias NativeElixirPdfUtilities.HtmlToPdf.Style
    alias NativeElixirPdfUtilities.Validators.{HtmlConstraintValidator, HtmlValidator}

    assert {:ok, dom} =
             HtmlParser.parse_detailed(
               "<style>:valid{color:green}:invalid{color:red}</style><form><input required value=Ready></form><input id=invalid required><input id=barred type=email readonly value=bad><fieldset><input required></fieldset><output id=result>Ready</output>"
             )

    assert {:ok, %{children: children}} = Style.compute_detailed(dom, forms: :static)
    nodes = Enum.filter(children, &(Map.get(&1, :tag) != "style"))
    [form, invalid, barred, fieldset, output] = nodes

    for node <- [form, hd(form.children)],
        do: assert_in_delta(elem(node.style.color, 1), 128 / 255, 1.0e-9)

    for node <- [invalid, fieldset], do: assert(node.style.color == {1.0, 0.0, 0.0})

    for node <- [barred, output] do
      refute HtmlValidator.control_state_matches?(:valid, node)
      refute HtmlValidator.control_state_matches?(:invalid, node)
    end

    assert barred.attributes["value"] == "bad"
    prepared_invalid = %{tag: "input", initial_state: initial("<input id=x required>")["x"]}
    assert HtmlConstraintValidator.matches_initial_state?(:invalid, prepared_invalid)

    refute HtmlConstraintValidator.matches_initial_state?(
             :valid,
             Map.put(prepared_invalid, :namespace, :svg)
           )

    refute HtmlConstraintValidator.matches_initial_state?(:invalid, %{tag: "div"})

    refute HtmlConstraintValidator.matches_initial_state?(:valid, %{
             tag: "input",
             initial_state: %{}
           })

    assert HtmlValidator.parse_css_simple_pseudo_class(":VaLiD") == {:ok, :valid, ""}
    assert HtmlValidator.parse_css_simple_pseudo_class(":invalid") == {:ok, :invalid, ""}
    assert HtmlConstraintValidator.valid_transported_state?(initial("<form id=f></form>")["f"])

    refute HtmlConstraintValidator.valid_transported_state?(%{
             initial("<form id=f></form>")["f"]
             | aggregate_valid: nil
           })
  end

  test "authored initial text lengths never invent user edited minlength or maxlength failures" do
    for type <- ~w(text search tel url email password), value <- ["x", "abcdef"] do
      source = "<input id=control type=#{type} minlength=3 maxlength=4 value='#{value}'>"
      state = initial(source)["control"]
      assert state.value == value
      refute state.constraint_validation.checks.too_long
      refute state.constraint_validation.checks.too_short
      assert {:ok, _} = NativeElixirPdfUtilities.HtmlToPdf.render(source, forms: :static)
    end
  end

  defp initial(html) do
    {:ok, dom} = HtmlParser.parse_detailed(html)
    nodes = HtmlFormValidator.prepare_initial_nodes(dom.children)

    collect = fn collect, nodes ->
      Enum.flat_map(nodes, fn node ->
        if Map.get(node, :type) == :element,
          do: [node | collect.(collect, node.children)],
          else: []
      end)
    end

    collect.(collect, nodes)
    |> Enum.filter(
      &(get_in(&1, [:attributes, "id"]) != nil and Map.has_key?(&1, :_initial_form_state))
    )
    |> Map.new(&{&1.attributes["id"], &1._initial_form_state})
  end
end
