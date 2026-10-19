defmodule NativeElixirPdfUtilities.HtmlToPdf.InitialControlTest do
  use ExUnit.Case
  alias NativeElixirPdfUtilities.HtmlToPdf
  alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, Style, Layout, Pagination, Font}
  alias NativeElixirPdfUtilities.Validators.{HtmlFormValidator, FormValidator}
  alias NativeElixirPdfUtilities.Pdf.Reader

  test "textarea initial API values preserve whitespace and ignore authored value attributes" do
    html =
      "<textarea name=notes value=Ignored>\n  First\r\n\tSecond\rLast\n</textarea><textarea value=Ignored></textarea>"

    assert {:ok, dom} = HtmlParser.parse_detailed(html)
    [notes, empty] = HtmlFormValidator.prepare_initial_nodes(dom.children)
    assert notes._initial_form_state.value == "  First\n\tSecond\nLast\n"
    assert notes._initial_form_state.cols == 20 and notes._initial_form_state.rows == 2
    assert notes._initial_form_state.wrap == :soft
    assert empty._initial_form_state.value == ""
    assert empty.attributes["value"] == "Ignored"
    assert {:ok, styled} = Style.compute_detailed(dom)
    [notes, empty] = styled.children
    assert notes.style.white_space == :pre_wrap
    assert notes.style.overflow_wrap == :break_word
    assert hd(notes.children).text == "  First\n\tSecond\nLast\n"
    assert hd(empty.children).text == ""
    assert {:ok, parsed} = HtmlParser.parse_detailed("<textarea>First</textarea>")
    doc = NativeElixirPdfUtilities.HtmlTreeHelpers.tree_content(parsed)
    [node] = doc.children
    node = %{node | children: [%{type: :text, text: "\nDirect\r\n\tTail"}]}
    assert {:ok, %{children: [node]}} = Style.compute_detailed(%{doc | children: [node]})
    assert node.style._form_control.initial_state.value == "\nDirect\n\tTail"
  end

  test "positive control dimensions parse integer prefixes with independent defaults" do
    for {attribute, input, expected} <- [
          {"cols", "0", 20},
          {"cols", "bad", 20},
          {"cols", "-4", 20},
          {"rows", "0", 2},
          {"rows", "bad", 2},
          {"rows", "-4", 2},
          {"cols", " +3tail", 3},
          {"rows", "\t5.9", 5}
        ] do
      assert {:ok, dom} =
               HtmlParser.parse_detailed("<textarea #{attribute}='#{input}'></textarea>")

      [node] = HtmlFormValidator.prepare_initial_nodes(dom.children)
      assert Map.fetch!(node._initial_form_state, String.to_existing_atom(attribute)) == expected
      assert node.attributes[attribute] == input
    end

    for {input, expected} <- [{"", 20}, {"0", 20}, {"-3", 20}, {"bad", 20}, {" +7suffix", 7}] do
      assert {:ok, dom} = HtmlParser.parse_detailed("<input size='#{input}'>")
      [node] = HtmlFormValidator.prepare_initial_nodes(dom.children)
      assert node._initial_form_state.size == expected
      assert node.attributes["size"] == input
    end

    for {wrap, expected} <- [
          {"HARD", :hard},
          {"soft", :soft},
          {"OFF", :off},
          {" hard ", :soft},
          {"invalid", :soft}
        ] do
      assert {:ok, dom} = HtmlParser.parse_detailed("<textarea wrap='#{wrap}'></textarea>")
      [node] = HtmlFormValidator.prepare_initial_nodes(dom.children)
      assert node._initial_form_state.wrap == expected
      assert {:ok, %{children: [styled]}} = Style.compute_detailed(dom)
      assert styled.style.white_space == if(expected == :off, do: :pre, else: :pre_wrap)
    end
  end

  test "placeholder artwork is distinct from the canonical value in static and interactive forms" do
    html = """
    <input name=empty placeholder='Type&#10;here'><input name=populated placeholder=Unused value=Actual>
    <input name=secret type=password placeholder=Password><input name=masked type=password value=Secret placeholder=Unused>
    <textarea name=notes placeholder='First&#10;Second'></textarea>
    <textarea name=filled placeholder=Unused>Authored</textarea>
    """

    assert {:ok, dom} = HtmlParser.parse_detailed(html)

    nodes =
      HtmlFormValidator.prepare_initial_nodes(dom.children) |> Enum.filter(&(&1.type == :element))

    [empty, populated, secret, masked, notes, filled] = nodes

    assert empty._initial_form_state.value == "" and
             empty._initial_form_state.placeholder == "Typehere"

    assert empty._initial_form_state.placeholder_shown
    refute populated._initial_form_state.placeholder_shown
    assert secret._initial_form_state.placeholder_shown
    refute masked._initial_form_state.placeholder_shown
    assert notes._initial_form_state.placeholder == "First\nSecond"
    assert filled._initial_form_state.value == "Authored"
    assert {:ok, styled} = Style.compute_detailed(dom)
    nodes = Enum.filter(styled.children, &(&1.type == :element))
    assert hd(hd(nodes).children).style.color == {117 / 255, 117 / 255, 117 / 255}
    assert hd(Enum.at(nodes, 1).children).style.color == {0, 0, 0}

    for mode <- [:static, :interactive] do
      assert {:ok, pdf} = HtmlToPdf.render(html, forms: mode)
      assert {:ok, flat} = NativeElixirPdfUtilities.Forms.flatten(pdf)
      assert {:ok, text} = NativeElixirPdfUtilities.Text.extract(flat)

      for visible <- ~w(Typehere Actual Password First Second Authored),
          do: assert(text =~ visible)

      refute text =~ "Unused" or text =~ "Secret"
      assert {:ok, context} = Reader.read_validated(pdf)
      assert {:ok, %{fields: fields}} = FormValidator.inspect_document(context)

      if mode == :interactive do
        fields = Map.new(fields, &{&1.name, &1})

        assert fields["empty"].value == "" and fields["secret"].value == "" and
                 fields["notes"].value == ""

        assert fields["masked"].value == "Secret" and fields["filled"].value == "Authored"
      else
        assert fields == []
      end
    end
  end

  test "rows and cols use resolved fonts and retain CSS dimensions and wrapping precedence" do
    for box_sizing <- ["content-box", "border-box"] do
      html =
        "<textarea cols=7 rows=3 style='font-family:DejaVu Sans Mono;font-size:10pt;overflow:hidden;box-sizing:#{box_sizing};padding:2pt;border:1pt solid black'>First\nSecond</textarea>"

      assert {:ok, dom} = HtmlParser.parse_detailed(html)
      assert {:ok, %{children: [node]} = styled} = Style.compute_detailed(dom)

      width =
        Float.ceil(Font.text_width("0", node.style.font_face, node.style.font_size) * 7 / 0.75) *
          0.75

      box_extra =
        node.style.padding.left + node.style.padding.right + node.style.border_widths.left +
          node.style.border_widths.right

      extra = if box_sizing == "border-box", do: box_extra, else: 0
      assert_in_delta node.style.width, width + extra, 0.0001
      assert {:ok, layout} = Layout.layout(styled)
      [control] = Enum.filter(layout.boxes, &is_map(Map.get(&1, :form_control)))
      assert_in_delta control.width, width + box_extra, 0.0001
      assert control.height > 30
    end

    assert {:ok, dom} =
             HtmlParser.parse_detailed(
               "<input size=80 style='width:40pt;height:12pt'><textarea cols=80 rows=9 wrap=off style='width:50pt;height:30pt;white-space:pre-wrap;overflow-wrap:break-word'>Text</textarea>"
             )

    assert {:ok, %{children: [input, textarea]}} = Style.compute_detailed(dom)
    assert input.style.width == 40 and input.style.height == 12
    assert textarea.style.width == 50 and textarea.style.height == 30
    assert textarea.style.white_space == :pre_wrap and textarea.style.overflow_wrap == :break_word
  end

  test "authored automatic dimensions retain the control's intrinsic character size" do
    for tag <- ["input", "textarea"], box <- ["content-box", "border-box"] do
      close = if tag == "textarea", do: ">Text</textarea>", else: ">"

      base =
        "<#{tag} #{if tag == "textarea", do: "cols=9 rows=3", else: "size=9"} style='font-family:DejaVu Sans Mono;font-size:10pt;box-sizing:#{box};overflow:hidden"

      assert {:ok, plain} = HtmlParser.parse_detailed(base <> "'" <> close)
      assert {:ok, auto} = HtmlParser.parse_detailed(base <> ";width:auto;height:auto'" <> close)
      assert {:ok, %{children: [plain]}} = Style.compute_detailed(plain)
      assert {:ok, %{children: [auto]}} = Style.compute_detailed(auto)
      assert auto.style.width == plain.style.width
      if tag == "textarea", do: assert(auto.style.height == plain.style.height)
      refute Map.has_key?(auto.style, :_control_column_count)
    end
  end

  test "clipped initial text moves with its control across pages and retains complete field values" do
    value = Enum.map_join(1..12, "\n", &"Line #{&1}")

    for display <- ["block", "inline-block"], mode <- [:static, :interactive] do
      html =
        "<div style='height:65pt'>Prior</div><p style='margin:0'><textarea name=notes rows=2 style='display:#{display};width:80pt;height:24pt;font-size:10pt;line-height:12pt'>#{value}</textarea></p><p>After</p>"

      assert {:ok, dom} = HtmlParser.parse_detailed(html)
      assert {:ok, styled} = Style.compute_detailed(dom)
      assert {:ok, layout} = Layout.layout(styled, page_size: {160, 100}, margin: 5)
      assert {:ok, pages} = Pagination.paginate(layout)
      assert length(pages) >= 2

      texts =
        for page <- pages, box <- page.boxes, box.type == :text, Map.has_key?(box, :clip), do: box

      assert texts != []
      assert Enum.all?(texts, &(Map.get(&1, :out_of_flow) == true))

      assert {:ok, pdf} =
               HtmlToPdf.render(html,
                 forms: mode,
                 page_size: {160, 100},
                 margin: 5,
                 compress_streams: false
               )

      assert {:ok, context} = Reader.read_validated(pdf)
      assert {:ok, %{fields: fields}} = FormValidator.inspect_document(context)

      if mode == :interactive do
        assert [field] = fields
        assert field.value == value
        assert field.widgets != []
      else
        assert fields == []
      end

      assert pdf =~ "re W n"
    end
  end

  test "initial control dimensions and malformed transport retain actionable diagnostics" do
    for html <- [
          "<textarea cols=1000000001></textarea>",
          "<textarea rows=1000000001></textarea>",
          "<input size=1000000001>"
        ] do
      assert {:error, {:resource_limit_exceeded, diagnostic}} = HtmlToPdf.render(html)
      assert diagnostic.stage == :html and diagnostic.operation == :render
      assert diagnostic.message =~ "max_css_numeric_magnitude" and is_binary(diagnostic.source)
    end

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             HtmlToPdf.render("<textarea cols=1000000000 style='font-size:100pt'></textarea>")

    assert diagnostic.stage == :style and diagnostic.operation == :compute
    assert diagnostic.source == "initial control width"

    assert {:error, {:invalid_document, diagnostic}} =
             HtmlFormValidator.validate_initial_control_dimension(-1, :height)

    assert diagnostic.source == "initial control height" and diagnostic.message =~ "nonnegative"
  end

  test "inapplicable checkbox size and radio placeholder preserve physical and field semantics" do
    for {html, reference, kind} <- [
          {"<input type=checkbox size=5>", "<input type=checkbox>", :checkbox},
          {"<input type=radio placeholder=Ignored>", "<input type=radio>", :radio}
        ] do
      assert {:ok, dom} = HtmlParser.parse_detailed(html)
      authored = hd(dom.children)
      effective = HtmlFormValidator.initial_control_attributes(authored)
      refute Map.has_key?(effective, "size")
      refute Map.has_key?(effective, "placeholder")
      assert {:ok, %{children: [styled]}} = Style.compute_detailed(dom)
      assert styled.attributes == authored.attributes
      assert styled.style._form_control.initial_state.checked == false
      refute Map.has_key?(styled.style._form_control.initial_state, :placeholder)

      for mode <- [:static, :interactive] do
        opts = [
          forms: mode,
          asset_resolver: fn request -> flunk("inactive control fetched #{inspect(request)}") end
        ]

        assert {:ok, pdf} = HtmlToPdf.render(html, opts)
        assert {:ok, reference_pdf} = HtmlToPdf.render(reference, opts)
        assert pdf == reference_pdf
        assert {:ok, context} = Reader.read_validated(pdf)
        assert {:ok, %{fields: fields}} = FormValidator.inspect_document(context)

        if mode == :interactive do
          assert [field] = fields
          assert field.type == kind
          assert field.flags == if(kind == :radio, do: 32_768, else: 0)
          assert [widget] = field.widgets
          assert [left, bottom, right, top] = widget.rect
          assert right > left and top > bottom
        else
          assert fields == []
        end
      end
    end
  end

  test "ordinary controls use native inline flow in HTML ancestry and preserve normalized fragment roots" do
    for {tag, source} <- [
          {"input", "<input value=Value>"},
          {"textarea", "<textarea>Value</textarea>"},
          {"select", "<select><option>Value</option></select>"},
          {"button", "<button>Value</button>"}
        ] do
      assert {:ok, fragment} = HtmlParser.parse_detailed(source)
      assert {:ok, %{children: [root]}} = Style.compute_detailed(fragment)
      assert root.style.display == :block
      assert {:ok, dom} = HtmlParser.parse_detailed("<p>Before " <> source <> " After</p>")
      assert {:ok, %{children: [paragraph]}} = Style.compute_detailed(dom)
      control = Enum.find(paragraph.children, &(Map.get(&1, :tag) == tag))
      assert control.style.display == :inline_block
    end

    assert {:ok, dom} =
             HtmlParser.parse_detailed(
               "<p>Before<input value=Value style='display:block'>After</p>"
             )

    assert {:ok, %{children: [paragraph]}} = Style.compute_detailed(dom)
    control = Enum.find(paragraph.children, &(Map.get(&1, :tag) == "input"))
    assert control.style.display == :block
    assert {:ok, hidden} = HtmlParser.parse_detailed("<input type=hidden>")
    assert {:ok, %{children: [control]}} = Style.compute_detailed(hidden)
    assert control.style.display == :none

    for namespace <- [:svg, :mathml] do
      assert HtmlFormValidator.initial_control_display(
               %{tag: "input", namespace: namespace, attributes: %{}},
               :inline_block,
               []
             ) == :inline_block
    end
  end

  test "primitive clips validate text and rectangle transport before emitting scoped artwork" do
    alias NativeElixirPdfUtilities.HtmlToPdf.PdfWriter
    clip = %{x: 5, y: 6, width: 12, height: 14}

    rectangle = %{
      type: :rect,
      x: 0,
      y: 0,
      width: 30,
      height: 30,
      fill_color: {0, 0, 1},
      stroke_color: nil,
      stroke_width: 0,
      border_radius: 0,
      clip: clip
    }

    text = %{
      type: :text,
      x: 0,
      y: 0,
      text: "Clipped",
      font: "Helvetica",
      font_size: 10,
      color: {0, 0, 0},
      clip: clip
    }

    assert {:ok, pdf} =
             PdfWriter.render([%{size: {100, 100}, boxes: [rectangle, text]}],
               compress_streams: false
             )

    assert length(Regex.scan(~r/5 6 12 14 re W n/, pdf)) == 2

    for box <- [rectangle, text], empty <- [%{clip | width: 0}, %{clip | height: 0}] do
      assert {:ok, empty_pdf} =
               PdfWriter.render([%{size: {100, 100}, boxes: [Map.put(box, :clip, empty)]}],
                 compress_streams: false
               )

      assert empty_pdf =~ "#{empty.x} #{empty.y} #{empty.width} #{empty.height} re W n"
    end

    for box <- [rectangle, text],
        malformed <- [
          %{},
          %{clip | width: -1},
          %{clip | height: -1},
          Map.put(clip, :inner_clip, Map.put(clip, :inner_clip, clip)),
          %{clip | x: NativeElixirPdfUtilities.Limits.get(:max_pdf_numeric_magnitude) * 2}
        ] do
      assert {:error, {:invalid_pdf_input, diagnostic}} =
               PdfWriter.render([%{size: {100, 100}, boxes: [Map.put(box, :clip, malformed)]}])

      assert diagnostic.stage == :pdf and diagnostic.operation == :write_pdf
      assert diagnostic.message =~ "clip requires"
    end

    control = %{
      id: make_ref(),
      order: [0],
      tag: "input",
      attributes: %{},
      children: [],
      initial_state: %{owner: nil, checked: false, disabled: false, value: <<255>>}
    }

    for mode <- [:static, :interactive] do
      assert {:error, {:invalid_form, diagnostic}} =
               PdfWriter.render(
                 [%{size: {100, 100}, boxes: [Map.put(rectangle, :form_control, control)]}],
                 forms: mode
               )

      assert diagnostic.stage == :forms and diagnostic.source == "initial_state"
      assert diagnostic.operation == :write_pdf
      assert diagnostic.message =~ "UTF-8 initial text value"
    end
  end
end
