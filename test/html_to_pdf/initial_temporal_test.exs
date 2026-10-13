defmodule NativeElixirPdfUtilities.HtmlToPdf.InitialTemporalTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.{Forms, HtmlToPdf, Limits}
  alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, PdfWriter, Style}

  alias NativeElixirPdfUtilities.Validators.{
    HtmlFormValidator,
    HtmlTemporalValidator,
    HtmlValidator
  }

  @types ~w(date month week time datetime-local)

  test "temporal grammar validates calendar dates, ISO weeks, exact time fields and local datetime normalization" do
    for {kind, source, expected, number} <- [
          {"date", "1970-01-01", "1970-01-01", 0},
          {"date", "1969-12-31", "1969-12-31", -86_400_000},
          {"date", "2000-02-29", "2000-02-29", 951_782_400_000},
          {"date", "0001-01-01", "0001-01-01", -62_135_596_800_000},
          {"month", "1970-01", "1970-01", 0},
          {"month", "1969-12", "1969-12", -1},
          {"month", "2026-10", "2026-10", 681},
          {"week", "1970-W01", "1970-W01", -259_200_000},
          {"week", "2020-W53", "2020-W53", 1_609_113_600_000},
          {"week", "2021-W01", "2021-W01", 1_609_718_400_000},
          {"time", "00:00", "00:00", 0},
          {"time", "23:59:59.999", "23:59:59.999", 86_399_999},
          {"time", "01:02:03.1", "01:02:03.1", 3_723_100},
          {"time", "01:02:00.000", "01:02:00.000", 3_720_000},
          {"datetime-local", "1970-01-01 00:00:00.000", "1970-01-01T00:00", 0},
          {"datetime-local", "1970-01-01T01:02:03.100", "1970-01-01T01:02:03.1", 3_723_100},
          {"datetime-local", "2000-02-29 00:00:00.010", "2000-02-29T00:00:00.01", 951_782_400_010}
        ] do
      assert {:ok, %{value: ^expected, number: ^number}} =
               HtmlTemporalValidator.parse(kind, source)

      state = HtmlTemporalValidator.prepare_initial(%{"type" => kind, "value" => source})
      assert state.value == expected and state.temporal.value == number
      assert HtmlTemporalValidator.valid_initial?(state, %{"type" => kind, "value" => source})
    end

    assert {:ok, %{components: %{year: 10_000}}} =
             HtmlTemporalValidator.parse("date", "10000-01-01")

    assert {:ok, %{components: %{year: 1_000_000_000}}} =
             HtmlTemporalValidator.parse("month", "1000000000-02")

    assert {:ok, %{components: %{year: 2026}}} =
             HtmlTemporalValidator.parse("date", "00002026-10-13")
  end

  test "invalid temporal syntax sanitizes value without changing authored source" do
    cases = %{
      "date" =>
        ~w(0000-01-01 2026-02-29 1900-02-29 2026-00-01 2026-13-01 2026-04-31 2026-1-01 026-01-01 2026-01-00 2026-01-01Z +2026-01-01),
      "month" => ~w(0000-01 2026-00 2026-13 2026-1 026-01 2026-01-01),
      "week" => ~w(0000-W01 2021-W53 2026-W00 2026-W54 2026-W1 2026-w01 026-W01),
      "time" =>
        ~w(24:00 12:60 12:00:60 1:00 01:1 01:00.1 01:00:00.1234 01:00:00. 01:00Z 01:00:00+01:00),
      "datetime-local" => [
        "2026-02-29T12:00",
        "2026-01-01t12:00",
        "2026-01-01  12:00",
        "2026-01-01T24:00",
        "2026-01-01T12:00Z",
        "2026-01-01\n12:00"
      ]
    }

    for {kind, invalids} <- cases, invalid <- invalids ++ ["", " invalid", "1970-01-01 ", "٢"] do
      refute match?({:ok, _}, HtmlTemporalValidator.parse(kind, invalid))
      html = "<input type='#{kind}' value='#{invalid}' style='display:none'>"
      assert {:ok, dom} = HtmlParser.parse_detailed(html)
      [node] = HtmlFormValidator.prepare_initial_nodes(dom.children)
      assert node._initial_form_state.value == ""
      assert node.attributes["value"] == String.replace(invalid, ~r/\r\n?/, "\n")
      assert node._initial_form_state.temporal.value == nil
      assert {:ok, _} = Style.compute_detailed(dom)
    end

    assert :error = HtmlTemporalValidator.parse("date", nil)
    assert :error = HtmlTemporalValidator.parse("unsupported", "1970-01-01")
    refute HtmlTemporalValidator.temporal_type?(nil)
    refute HtmlTemporalValidator.valid_initial?(nil, %{})
    refute HtmlTemporalValidator.valid_initial?(%{}, %{"type" => 1})
  end

  test "bounds and exact step units are renderable state, with min then value then fixed epoch base" do
    for {kind, scale, default, value} <- [
          {"date", 86_400_000, 1, "1970-01-02"},
          {"month", 1, 1, "1970-02"},
          {"week", 604_800_000, 1, "1970-W02"},
          {"time", 1000, 60, "00:00:00.100"},
          {"datetime-local", 1000, 60, "1970-01-01T00:00:00.100"}
        ] do
      attrs = %{"type" => kind, "value" => value}
      state = HtmlTemporalValidator.prepare_initial(attrs)
      assert state.value == if(kind == "datetime-local", do: "1970-01-01T00:00:00.1", else: value)
      assert state.temporal.step == default and state.temporal.step_scale == scale
      assert state.temporal.step_base == state.temporal.value
      refute state.temporal.step_mismatch

      for authored <- ["bad", "0", "-2", " any ", "1e309"] do
        assert HtmlTemporalValidator.prepare_initial(Map.put(attrs, "step", authored)).temporal.step ==
                 default
      end

      assert HtmlTemporalValidator.prepare_initial(Map.put(attrs, "step", "ANY")).temporal.step ==
               :any

      empty = HtmlTemporalValidator.prepare_initial(%{"type" => kind})
      assert empty.value == "" and empty.temporal.min == nil and empty.temporal.max == nil
      assert empty.temporal.step_base == if(kind == "week", do: -259_200_000, else: 0)
      refute empty.temporal.step_mismatch
    end

    for {step, mismatch} <- [
          {".1", false},
          {".03", true},
          {"1e308", true},
          {".00000000000000000001", false}
        ] do
      state =
        HtmlTemporalValidator.prepare_initial(%{
          "type" => "time",
          "value" => "00:00:00.100",
          "min" => "00:00",
          "step" => step
        })

      assert state.temporal.step_base == 0
      assert state.temporal.step_mismatch == mismatch
      assert state.value == "00:00:00.100"
    end

    attrs = %{
      "type" => "date",
      "value" => "1970-01-03",
      "min" => "1970-01-02",
      "max" => "1970-01-02",
      "step" => "2"
    }

    state = HtmlTemporalValidator.prepare_initial(attrs)
    assert state.temporal.min == 86_400_000 and state.temporal.max == 86_400_000
    assert state.temporal.range_overflow and state.temporal.step_mismatch
    refute state.temporal.range_underflow
    assert state.value == "1970-01-03"
    under = HtmlTemporalValidator.prepare_initial(%{attrs | "value" => "1970-01-01"})
    assert under.temporal.range_underflow
    refute under.temporal.range_overflow
    invalid = HtmlTemporalValidator.prepare_initial(%{attrs | "min" => "bad", "max" => "bad"})
    assert invalid.temporal.min == nil and invalid.temporal.max == nil
  end

  test "time reversed ranges span midnight and diagnose the excluded interval as both bounds" do
    for {value, excluded} <- [
          {"23:00", false},
          {"00:00", false},
          {"01:00", false},
          {"12:00", true}
        ] do
      state =
        HtmlTemporalValidator.prepare_initial(%{
          "type" => "time",
          "value" => value,
          "min" => "23:00",
          "max" => "01:00"
        })

      assert state.temporal.reversed_range

      assert state.temporal.range_underflow == excluded and
               state.temporal.range_overflow == excluded

      assert state.value == value
    end

    state =
      HtmlTemporalValidator.prepare_initial(%{
        "type" => "date",
        "value" => "1970-01-02",
        "min" => "1970-01-03",
        "max" => "1970-01-01"
      })

    refute state.temporal.reversed_range
    assert state.temporal.range_underflow and state.temporal.range_overflow

    empty =
      HtmlTemporalValidator.prepare_initial(%{
        "type" => "time",
        "min" => "23:00",
        "max" => "01:00"
      })

    refute empty.temporal.range_underflow or empty.temporal.range_overflow
  end

  test "canonical temporal state retains disabled inheritance, source identities and required/read-only selectors" do
    html =
      "<fieldset disabled><legend><input type=date required style=display:none value=2026-10-13></legend><input type=time readonly required style=display:none value=23:00></fieldset>"

    assert {:ok, dom} = HtmlParser.parse_detailed(html)
    [fieldset] = HtmlFormValidator.prepare_initial_nodes(dom.children)
    [legend, disabled] = fieldset.children
    enabled = hd(legend.children)
    refute enabled._initial_form_state.disabled
    assert disabled._initial_form_state.disabled and disabled._initial_form_state.readonly
    assert enabled._selector_id != disabled._selector_id
    assert HtmlValidator.control_state_matches?(:required, enabled)
    assert HtmlValidator.control_state_matches?(:required, disabled)
    assert HtmlValidator.editing_state_matches?(:read_write, enabled, [legend, fieldset])
    assert HtmlValidator.editing_state_matches?(:read_only, disabled, [fieldset])
    assert {:ok, styled} = Style.compute_detailed(dom)
    assert {:ok, _} = HtmlToPdf.render(html)

    assert hd(hd(styled.children).children).children
           |> hd()
           |> Map.fetch!(:style)
           |> Map.fetch!(:_temporal_state)
           |> Map.fetch!(:kind) == "date"
  end

  test "temporal inapplicability and unsupported editing return public diagnostics" do
    for kind <- @types do
      for attribute <- ~w(checked multiple size placeholder) do
        assert {:error, {:unsupported_html, diagnostic}} =
                 HtmlToPdf.render("<input type=#{kind} #{attribute}=x style=display:none>")

        assert diagnostic.stage == :html and diagnostic.operation == :render and
                 diagnostic.message != ""
      end

      assert {:ok, _} = HtmlToPdf.render("<input type=#{kind} value=bad>", forms: :static)

      assert {:error, {:unsupported_form, diagnostic}} =
               HtmlToPdf.render("<input type=#{kind} value=bad>")

      assert diagnostic.stage == :forms and diagnostic.source == "temporal"
      assert diagnostic.message =~ "Mutable temporal controls"

      assert HtmlValidator.valid_form_element?(
               "input",
               %{
                 "type" => kind,
                 "readonly" => "false",
                 "required" => "false",
                 "min" => "bad",
                 "max" => "bad",
                 "step" => "any"
               },
               []
             )
    end

    assert {:ok, %{}} = HtmlTemporalValidator.prepare_render_style(%{}, %{}, [], [])
  end

  test "direct scalar transport preserves canonical values only for disabled or readonly temporal controls" do
    for {kind, value} <- [
          {"date", "2000-02-29"},
          {"month", "2026-10"},
          {"week", "2020-W53"},
          {"time", "01:02:03.100"},
          {"datetime-local", "1970-01-01 01:02:03.100"}
        ],
        lock <- ~w(disabled readonly) do
      attrs = %{"type" => kind, "value" => value, "name" => kind, lock => ""}

      [node] =
        HtmlFormValidator.prepare_initial_nodes([
          %{type: :element, tag: "input", attributes: attrs, children: []}
        ])

      control = %{
        id: node._selector_id,
        order: node._document_order,
        tag: "input",
        attributes: attrs,
        children: [],
        initial_state: node._initial_form_state
      }

      box = %{
        type: :rect,
        x: 10.0,
        y: 10.0,
        width: 120.0,
        height: 18.0,
        fill_color: {1.0, 1.0, 1.0},
        stroke_color: nil,
        stroke_width: 0.0,
        border_radius: 0.0,
        form_control: control,
        form_owner: control.id
      }

      page = %{size: {200, 200}, boxes: [box]}
      assert {:ok, pdf} = PdfWriter.render([page])
      assert {:ok, [field]} = Forms.fields(pdf)
      assert field.value == node._initial_form_state.value and field.read_only
      assert field.type == :text
      forged = put_in(box.form_control.initial_state.temporal.step_base, 99)

      assert {:error, {:invalid_form, %{source: "initial_state"}}} =
               HtmlFormValidator.prepare([%{page | boxes: [forged]}], forms: :static)

      unlocked = %{attrs | lock => ""} |> Map.delete(lock)
      state = HtmlTemporalValidator.prepare_initial(unlocked)

      unlocked_control = %{
        control
        | attributes: unlocked,
          initial_state: Map.merge(control.initial_state, state) |> Map.put(:disabled, false)
      }

      assert {:error, {:unsupported_form, diagnostic}} =
               HtmlFormValidator.prepare(
                 [%{page | boxes: [%{box | form_control: unlocked_control}]}],
                 forms: :interactive
               )

      assert diagnostic.stage == :forms and diagnostic.source == "temporal" and
               diagnostic.operation == :write_pdf

      assert diagnostic.message =~ "forms: :static"
    end
  end

  test "direct layout cannot consume forged or unimplemented temporal paint state" do
    attrs = %{"type" => "date", "value" => "2026-10-13"}
    state = HtmlTemporalValidator.prepare_initial(attrs)
    control = %{attributes: attrs, initial_state: state}
    node = %{style: %{_temporal_state: state.temporal, _form_control: control}}

    assert {:error, {:invalid_layout, diagnostic}} =
             HtmlTemporalValidator.validate_initial_layout(node)

    assert diagnostic.stage == :layout and diagnostic.operation == :layout and
             diagnostic.source == "temporal"

    assert {:error, {:invalid_layout, %{source: "temporal"}}} =
             HtmlTemporalValidator.validate_initial_layout(
               put_in(node.style._temporal_state.step_base, 99)
             )

    assert {:error, {:invalid_layout, _}} =
             HtmlTemporalValidator.validate_initial_layout(%{
               style: %{_temporal_state: state.temporal}
             })

    assert :ok =
             HtmlTemporalValidator.validate_initial_layout(%{
               style: %{display: :none, _temporal_state: :invalid}
             })

    assert :ok =
             HtmlTemporalValidator.validate_initial_layout(%{
               style: %{_form_control: %{attributes: %{"type" => "text"}}}
             })

    assert :ok = HtmlTemporalValidator.validate_initial_layout(%{})
    assert {:ok, dom} = HtmlParser.parse_detailed("<input type=date style=display:none>")
    assert {:ok, %{children: [node]} = styled} = Style.compute_detailed(dom)
    visible = put_in(node.style.display, :inline_block)

    assert {:error, {:invalid_layout, %{source: "temporal"}}} =
             NativeElixirPdfUtilities.HtmlToPdf.Layout.layout(%{styled | children: [visible]})
  end

  test "existing numeric token budget rejects hostile temporal strings before calendar conversion" do
    previous = Limits.effective()
    on_exit(fn -> Limits.install(previous) end)
    Limits.install(Map.put(previous, :max_html_numeric_token_bytes, 12))

    for {kind, attr, value} <- [
          {"date", "value", "12345678-01-01"},
          {"time", "value", "00:00:00.12345"},
          {"month", "min", "1234567890-01"},
          {"week", "max", "1234567890-W01"},
          {"datetime-local", "value", "1970-01-01T00:00"}
        ] do
      assert {:error, {:resource_limit_exceeded, diagnostic}} =
               HtmlToPdf.render("<input type=#{kind} #{attr}=#{value} style=display:none>")

      assert diagnostic.stage == :html and diagnostic.operation == :render and
               diagnostic.message =~ "max_html_numeric_token_bytes"
    end
  end

  test "temporal state selectors hide the original controls without fields or lost captions" do
    html =
      File.read!(
        Path.expand(
          "../fixtures/html_to_pdf/browser_parity/html_temporal_initial_state.html",
          __DIR__
        )
      )

    assert {:ok, pdf} = HtmlToPdf.render(html, forms: :interactive)
    assert {:ok, []} = Forms.fields(pdf)
    assert {:ok, text} = NativeElixirPdfUtilities.Text.extract(pdf)

    for caption <- [
          "Leap date",
          "Month, readonly",
          "ISO week 53",
          "Time crossing midnight",
          "Local datetime",
          "First legend remains enabled",
          "Fieldset time is disabled",
          "After canonical temporal controls"
        ],
        do: assert(text =~ caption)

    refute text =~ "2000-02-29"
    refute text =~ "2020-W53"
  end
end
