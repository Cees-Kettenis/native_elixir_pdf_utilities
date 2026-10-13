defmodule NativeElixirPdfUtilities.HtmlToPdf.InitialSelectTest do
  use ExUnit.Case
  import Bitwise
  alias NativeElixirPdfUtilities.{HtmlToPdf, Forms, Text}
  alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, Style, Layout, PdfWriter}
  alias NativeElixirPdfUtilities.Pdf.Reader
  alias NativeElixirPdfUtilities.Validators.{HtmlFormValidator, HtmlValidator, FormValidator}

  test "selection identities include disabled and hidden options without implicit multiple selections" do
    html = """
    <select multiple name=choices>
    <option disabled selected value="a" label="Alpha">Wrong label</option>
    <optgroup label="Group" disabled><option selected value=""> Empty label </option></optgroup>
    <option hidden selected value="c">Charlie</option><option value="d">Delta</option>
    </select>
    """

    assert {:ok, dom} = HtmlParser.parse_detailed(html)
    [node] = HtmlFormValidator.prepare_initial_nodes(dom.children)
    state = node._initial_form_state.selection
    assert state.multiple and state.listbox and state.display_size == 4
    assert state.value == "a" and state.values == ["a", "", "c"]
    assert state.selected_indices == [0, 1, 2]
    assert state.selected_ids == Enum.take(Enum.map(state.choices, & &1.id), 3)
    assert Enum.map(state.choices, & &1.label) == ["Alpha", "Empty label", "Charlie", "Delta"]
    assert Enum.map(state.choices, & &1.disabled) == [true, true, false, false]
    assert Map.has_key?(node.attributes, "multiple")

    for {option, index} <- HtmlValidator.select_options(node.children) |> Enum.with_index() do
      assert option._selector_id == Enum.at(state.choices, index).id
      assert HtmlValidator.control_state_matches?(:checked, option) == index < 3

      assert option.attributes ==
               Enum.at(HtmlValidator.select_options(hd(dom.children).children), index).attributes
    end

    assert {:ok, dom} = HtmlParser.parse_detailed("<select multiple><option>A</option></select>")
    [node] = HtmlFormValidator.prepare_initial_nodes(dom.children)
    assert node._initial_form_state.selection.value == ""
    assert node._initial_form_state.selection.values == []
  end

  test "display size follows nonnegative HTML prefix parsing and conditional single fallback" do
    for {size, expected, value} <- [
          {nil, 1, "B"},
          {"bad", 1, "B"},
          {"-2", 1, "B"},
          {"-0", 1, "B"},
          {"0", 0, ""},
          {"1", 1, "B"},
          {" +2tail", 2, ""},
          {"3.9", 3, ""}
        ] do
      attribute = if size, do: " size='#{size}'", else: ""

      assert {:ok, dom} =
               HtmlParser.parse_detailed(
                 "<select#{attribute}><option disabled>A</option><option>B</option></select>"
               )

      [node] = HtmlFormValidator.prepare_initial_nodes(dom.children)
      state = node._initial_form_state.selection
      assert state.display_size == expected
      assert state.value == value
      assert state.listbox == expected > 1

      assert {:ok, _pdf} =
               HtmlToPdf.render(
                 "<select#{attribute} name=size><option disabled>A</option><option>B</option></select>"
               )
    end

    for size <- [nil, "0", "1", "invalid", "2"] do
      attribute = if size, do: " size='#{size}'", else: ""

      assert {:ok, dom} =
               HtmlParser.parse_detailed(
                 "<select multiple#{attribute}><option>A</option></select>"
               )

      [node] = HtmlFormValidator.prepare_initial_nodes(dom.children)
      state = node._initial_form_state.selection
      assert state.value == "" and state.values == []
      assert state.listbox == (size != "1")
    end

    for attribute <- ["", "multiple", "size=3", "size=0"] do
      assert {:ok, dom} =
               HtmlParser.parse_detailed(
                 "<select #{attribute}><option selected>A</option><option disabled selected>B</option></select>"
               )

      [node] = HtmlFormValidator.prepare_initial_nodes(dom.children)

      assert node._initial_form_state.selection.values ==
               if(attribute == "multiple", do: ["A", "B"], else: ["B"])
    end
  end

  test "interactive PDF choices carry complete exports zero-based option indices and the correct flags" do
    html = """
    <select name=single size=3><option>A</option><option>B</option></select>
    <select name=empty size=0><option>A</option></select>
    <select name=drop><option value=''>Blank</option></select>
    <select name=multi multiple size=2><option value=a selected>Alpha</option>
    <optgroup label=Group disabled><option value=b selected hidden>Beta</option></optgroup>
    <option value=c>Charlie</option><option value=d selected>Delta</option></select>
    <select name=none multiple><option>A</option></select>
    <select name=menu multiple size=1><option selected>A</option><option selected>B</option></select>
    """

    for mode <- [:static, :interactive] do
      assert {:ok, pdf} = HtmlToPdf.render(html, forms: mode)
      assert {:ok, context} = Reader.read_validated(pdf)
      assert {:ok, %{fields: fields}} = FormValidator.inspect_document(context)

      if mode == :static do
        assert fields == []
      else
        fields = Map.new(fields, &{&1.name, &1})
        assert fields["single"].value == nil and fields["empty"].value == nil
        assert fields["drop"].value == "" and fields["drop"].dictionary["I"] == [0]
        assert (fields["drop"].flags &&& 131_072) != 0
        assert (fields["single"].flags &&& 131_072) == 0
        assert fields["multi"].value == ["a", "b", "d"]
        assert fields["multi"].dictionary["I"] == [0, 1, 3]
        assert Enum.map(fields["multi"].choices, & &1.value) == ["a", "b", "c", "d"]
        assert (fields["multi"].flags &&& 2_097_152) != 0
        assert (fields["multi"].flags &&& 131_072) == 0
        assert fields["none"].value == [] and fields["none"].dictionary["I"] == []
        assert fields["menu"].value == ["A", "B"]
        assert (fields["menu"].flags &&& 131_072) != 0
        assert {:ok, updated} = Forms.fill(pdf, %{"multi" => ["c", "d"]})

        assert {:ok, %{fields: fields}} =
                 updated
                 |> Reader.read_validated()
                 |> then(fn {:ok, context} -> FormValidator.inspect_document(context) end)

        multi = Enum.find(fields, &(&1.name == "multi"))
        assert multi.value == ["c", "d"] and multi.dictionary["I"] == [2, 3]
      end

      assert {:ok, flat} = Forms.flatten(pdf)
      assert {:ok, []} = Forms.fields(flat)
      assert {:ok, text} = Text.extract(flat)
      assert text =~ "Alpha" and text =~ "2 selected"
    end
  end

  test "listbox sizing and row artwork reuse flow clipping and initial selection scrolling" do
    html = """
    <style>select{display:block;font-family:'DejaVu Sans';font-size:10pt;} option.hidden{display:none}</style>
    <select size=2 name=scroll><option>Before</option><option class=hidden>Hidden</option>
    <optgroup label=Group><option value=c selected label=Current>Wrong</option><option>After</option></optgroup>
    <option>Last</option></select><p>Following</p>
    """

    assert {:ok, dom} = HtmlParser.parse_detailed(html)
    assert {:ok, styled} = Style.compute_detailed(dom)
    select = Enum.find(styled.children, &(&1[:tag] == "select"))
    assert select.style.form_kind == :listbox
    assert is_number(select.style.width) and is_number(select.style.height)
    assert {:ok, layout} = Layout.layout(styled)
    texts = Enum.filter(layout.boxes, &(&1.type == :text))
    assert Enum.any?(texts, &(&1.text == "Current" and is_map(&1[:clip])))
    refute Enum.any?(texts, &(&1.text in ["Before", "Hidden", "Wrong", "Last"]))
    assert Enum.any?(texts, &(&1.text == "Following"))

    for mode <- [:static, :interactive] do
      assert {:ok, pdf} = HtmlToPdf.render(html, forms: mode)
      assert {:ok, _flat} = Forms.flatten(pdf)
    end
  end

  test "Linux menu defaults yield to authored backgrounds and borders" do
    for {css, native?, fill, width} <- [
          {"", true, {1.0, 1.0, 1.0}, 0.75},
          {"padding:2pt", true, {1.0, 1.0, 1.0}, 0.75},
          {"border-radius:3pt", false, {239 / 255, 239 / 255, 239 / 255}, 0.75},
          {"border:2pt solid red", false, {239 / 255, 239 / 255, 239 / 255}, 1.5},
          {"background:#00ff00", false, {0.0, 1.0, 0.0}, 0.75},
          {"border:2pt dashed red;border-radius:3pt", false, {239 / 255, 239 / 255, 239 / 255},
           1.5}
        ] do
      assert {:ok, dom} =
               HtmlParser.parse_detailed("<select style='#{css}'><option>A</option></select>")

      assert {:ok, styled} = Style.compute_detailed(dom)
      assert {:ok, layout} = Layout.layout(styled)
      outer = Enum.find(layout.boxes, &is_map(&1[:form_control]))
      assert outer.role == :native_menu == native?
      assert outer.fill_color == fill and outer.stroke_width == width
      assert {:ok, _} = HtmlToPdf.render("<select style='#{css}'><option>A</option></select>")
    end
  end

  test "select resource and ambiguous interactive export failures are actionable" do
    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             HtmlToPdf.render("<select size=1000000001><option>A</option></select>")

    assert diagnostic.stage == :html and diagnostic.reason == :resource_limit_exceeded
    assert diagnostic.message =~ "max_css_numeric_magnitude"

    html =
      "<select multiple><option selected value=a>A</option><option selected value=a>B</option></select>"

    assert {:ok, _} = HtmlToPdf.render(html, forms: :static)
    assert {:error, {:invalid_form, diagnostic}} = HtmlToPdf.render(html, forms: :interactive)
    assert diagnostic.stage == :forms and diagnostic.message =~ "forms: :static"
  end

  test "direct transported canonical selections reject contradictory identities and values" do
    assert {:ok, dom} =
             HtmlParser.parse_detailed(
               "<select multiple><option selected>A</option><option selected>B</option></select>"
             )

    [select] = HtmlFormValidator.prepare_initial_nodes(dom.children)
    selection = select._initial_form_state.selection

    for changed <- [
          %{selection | values: ["A"]},
          %{selection | value: "B"},
          %{selection | selected_indices: [1, 2]},
          %{selection | selected_ids: []},
          %{selection | id: make_ref()},
          %{selection | multiple: false},
          %{selection | display_size: -1},
          %{selection | display_size: 1_000_000_001},
          %{selection | rows: 1},
          %{selection | listbox: false},
          %{selection | choices: [%{}]},
          %{selection | choices: [Map.put(hd(selection.choices), :label, <<255>>)]},
          %{selection | choices: [Map.put(hd(selection.choices), :index, -1)]}
        ],
        mode <- [:static, :interactive] do
      control = %{
        id: select._selector_id,
        order: [0],
        tag: "select",
        attributes: select.attributes,
        children: select.children,
        initial_state: %{select._initial_form_state | selection: changed}
      }

      box = %{
        type: :rect,
        x: 0,
        y: 0,
        width: 80,
        height: 40,
        fill_color: {1, 1, 1},
        stroke_color: {0, 0, 0},
        stroke_width: 1,
        border_radius: 0,
        form_control: control
      }

      assert {:error, {:invalid_form, diagnostic}} =
               PdfWriter.render([%{size: {200, 200}, boxes: [box]}], forms: mode)

      assert diagnostic.stage == :forms and diagnostic.source == "initial_state"
      assert diagnostic.message =~ "selection state"
    end
  end
end
