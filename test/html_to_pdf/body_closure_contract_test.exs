defmodule NativeElixirPdfUtilities.HtmlToPdf.BodyClosureContractTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.{Forms, HtmlToPdf, Limits, Text}

  alias NativeElixirPdfUtilities.HtmlToPdf.{
    EmbeddedViewport,
    FormValueBackend,
    HtmlComposition,
    HtmlTree,
    ResourceContext,
    HtmlParser,
    Layout,
    PdfWriter,
    Style
  }

  alias NativeElixirPdfUtilities.Pdf.Reader

  alias NativeElixirPdfUtilities.Validators.{
    EmbeddedFormValidator,
    HtmlFormValidator,
    HtmlFormValueValidator,
    HtmlValidator,
    TemplatePatchValidator,
    WriterValidator
  }

  setup do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    :ok
  end

  test "automatic and initial quotes restore real quote glyphs after an ancestor suppresses them" do
    html =
      "<div style='quotes:none'><q style='quotes:auto'>Auto</q> <q style='quotes:initial'>Initial</q></div>"

    assert {:ok, pdf} = HtmlToPdf.render(html)
    assert {:ok, text} = Text.extract(pdf)
    assert text =~ "“Auto”" and text =~ "“Initial”"
  end

  test "inherited quote keywords keep authored pairs through the actual cascade" do
    for keyword <- ~w(inherit unset revert revert-layer) do
      html = "<div style='quotes: \"[\" \"]\"'><q style='quotes:#{keyword}'>Inherited</q></div>"
      assert {:ok, pdf} = HtmlToPdf.render(html)
      assert {:ok, text} = Text.extract(pdf)
      assert text =~ "[Inherited]"
    end
  end

  test "empty font faces and unrepresentable body margin hints retain canonical defaults" do
    for html <- [
          "<body><font>Actual</font></body>",
          "<body marginwidth=10000000000><font face=''>Actual</font></body>"
        ] do
      assert {:ok, dom} = HtmlParser.parse_detailed(html)
      assert {:ok, tree} = Style.compute_detailed(dom)
      [body] = tree.children
      send(self(), {:default_margin, body.style.margin})
      assert {:ok, pdf} = HtmlToPdf.render(html)
      assert {:ok, text} = Text.extract(pdf)
      assert text =~ "Actual"
    end

    assert_receive {:default_margin, expected}
    assert_receive {:default_margin, ^expected}
  end

  test "short actual curved dash hosts and oversized dotted strokes remain valid finite PDFs" do
    assert {:ok, pdf} =
             HtmlToPdf.render(
               "<div style='box-sizing:border-box;width:2px;height:2px;border:1px dashed #345;border-radius:.4px'></div>",
               compress_streams: false
             )

    assert {:ok, _} = Reader.read_validated(pdf)

    box = %{
      type: :rect,
      x: 2,
      y: 2,
      width: 0.75,
      height: 0.75,
      border_radius: 0.25,
      stroke_width: 3,
      stroke_color: {0.2, 0.3, 0.4},
      fill_color: {0.8, 0.9, 1.0},
      border_styles: %{top: :dotted, right: :dotted, bottom: :dotted, left: :dotted},
      border_widths: %{top: 3, right: 3, bottom: 3, left: 3},
      snap_to_css_pixel_grid: true
    }

    assert {:ok, tiny} =
             PdfWriter.render([%{size: {20, 20}, boxes: [box]}], compress_streams: false)

    assert {:ok, _} = Reader.read_validated(tiny)
    refute tiny =~ "[0 0]"
  end

  test "invalid fallback dependency graphs return actionable CSS errors without substitution" do
    for properties <- [%{"--size" => "var(--missing"}, %{"--size" => "var(--missing, broken"}] do
      assert {:error, {:invalid_css, diagnostic}} =
               HtmlValidator.compute_custom_properties(properties)

      assert diagnostic.stage == :css and diagnostic.reason == :invalid_css
      assert diagnostic.message =~ "variable syntax"
    end

    assert :error =
             HtmlValidator.prepare_css_variable_source(<<255>>, HtmlValidator.new_css_budget())

    assert :error = HtmlValidator.parse_css_variable_references("var(--missing")

    assert {:error, :unsupported_html} =
             HtmlValidator.validate_html_attributes("onclick=run()", "p")

    assert :error = HtmlValidator.parse_css_list_style("list-style", "url()")
    assert :error = HtmlValidator.parse_css_shadow_selector(":host(")
  end

  test "CSS size preparation distinguishes auto margins from signed and unsigned lengths" do
    assert {:ok, :auto} = HtmlValidator.prepare_css_length_source("auto", :margin)

    assert {:ok, %{value: "0"}} =
             HtmlValidator.prepare_css_length_source("0", :margin)

    assert :error = HtmlValidator.prepare_css_length_source("wat", :margin)
    Limits.install(%{Limits.effective() | max_css_work: 1})

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             HtmlValidator.with_render_budget(fn ->
               HtmlValidator.validate_prepared_css_size({:min, [1.0, 2.0]})
             end)

    assert diagnostic.stage == :layout and diagnostic.message =~ "max_css_work"
  end

  test "caller canvas plans reject extra properties and body-only layout keeps actual source background" do
    assert {:ok, dom} = HtmlParser.parse_detailed("<body style='background:#eef'>Actual</body>")
    assert {:ok, tree} = Style.compute_detailed(dom)
    legacy = Map.delete(tree, :document_canvas)
    assert {:ok, layout} = Layout.layout(legacy, page_size: {100, 100}, margin: 0)
    assert Enum.any?(layout.boxes, &(Map.get(&1, :fill_color) != nil))

    plan = %{
      style: %{background_color: {1, 0, 0}, font_size: 10},
      body_background_propagated: false
    }

    assert {:error, {:invalid_document, diagnostic}} =
             Layout.layout(Map.put(tree, :document_canvas, plan),
               page_size: {100, 100},
               margin: 0
             )

    assert diagnostic.operation == :layout_document_canvas and
             diagnostic.message =~ "canonical background"
  end

  test "bad authored attribute order reports the genuine source token" do
    assert {:ok, dom} = HtmlParser.parse_detailed("<p id=actual>Actual</p>")
    [node] = dom.children
    bad = %{node | attribute_order: ["missing"]}

    assert {:error, {:invalid_document, diagnostic}} =
             Style.compute_detailed(%{dom | children: [bad]})

    assert diagnostic.source == "<p id=actual>" and diagnostic.operation == :compute
  end

  test "composition fragments validate real canonical nodes and reject unsupported element kinds" do
    assert {:ok, fragment} = HtmlParser.parse_fragment_tree_detailed("<p>Actual</p>", "div")
    assert :ok = HtmlValidator.validate_composition_document(fragment)
    [node] = fragment.children
    bad = %{node | tag: "unsupported"}

    assert {:error, {_reason, diagnostic}} =
             HtmlValidator.validate_composition_document(%{fragment | children: [bad]})

    assert is_atom(diagnostic.stage) and is_binary(diagnostic.message)
    assert {:error, _} = HtmlValidator.validate_composed_fragment([bad])
  end

  test "layout mode validates callbacks and preserves the enclosing process state" do
    assert {:error, {:invalid_options, diagnostic}} =
             HtmlFormValidator.with_layout_mode(:static, fn _ -> :bad end)

    assert diagnostic.operation == :with_layout_mode and diagnostic.message =~ "zero-argument"
    assert HtmlFormValidator.layout_mode() == :interactive

    assert :static ==
             HtmlFormValidator.with_layout_mode(:static, fn -> HtmlFormValidator.layout_mode() end)

    assert HtmlFormValidator.layout_mode() == :interactive
  end

  test "real native URL resource failures remain attributed to the caller input" do
    Limits.install(%{Limits.effective() | max_html_form_value_bytes: 1})

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             HtmlToPdf.render("<input type=url value='https://example.test'>")

    assert diagnostic.stage == :html and diagnostic.source == "input/url"
    assert diagnostic.message =~ "max_html_form_value_bytes"
  end

  test "actual native replies cannot reset the cumulative compiler budget between requests" do
    {:ok, initial} = HtmlFormValueValidator.prepare(:pattern, "(a)", ["a"])

    {:ok, <<1, compile::unsigned-big-64, _match::unsigned-big-64, _allocated::unsigned-big-64>>} =
      FormValueBackend.run(initial)

    assert compile > 0
    Limits.install(%{Limits.effective() | max_html_pattern_compile_work: compile})

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             HtmlValidator.with_render_budget(fn ->
               {:ok, one} = HtmlFormValueValidator.prepare(:pattern, "(a)", ["a"])
               assert {:ok, :matched} = HtmlFormValueValidator.result(FormValueBackend.run(one))
               {:ok, two} = HtmlFormValueValidator.prepare(:pattern, "(a)", ["a"])
               HtmlFormValueValidator.result(FormValueBackend.run(two))
             end)

    assert diagnostic.operation == :initial_form_value and
             diagnostic.message =~ "max_html_pattern_compile_work"
  end

  test "native fixed capture operand bounds return a public failure independently of configurable ceilings" do
    Limits.install(%{
      Limits.effective()
      | max_html_pattern_bytes: 200_000,
        max_html_pattern_compile_work: 20_000_000
    })

    {:ok, request} =
      HtmlFormValueValidator.prepare(:pattern, String.duplicate("()", 65_536), [""])

    reply = FormValueBackend.run(request)
    assert reply == {:exit, 79}
    assert {:error, {:resource_limit_exceeded, diagnostic}} = HtmlFormValueValidator.result(reply)
    assert diagnostic.message =~ "65,535" and diagnostic.operation == :initial_form_value
  end

  test "RGB function whitespace preserves the actual channel and alpha values" do
    assert HtmlValidator.parse_css_color("rgb(255,\n0,\r\n0)", {0, 0, 0}) ==
             {:ok, {1.0, 0.0, 0.0}}

    assert HtmlValidator.parse_css_color("rgba(0%,\n50%,100%,\n25%)", {0, 0, 0}) ==
             {:ok, {0.0, 0.5, 1.0, 0.25}}

    html = "<div style='width:40px;height:20px;background:rgb(255,\n0,\n0)'></div>"
    assert {:ok, pdf} = HtmlToPdf.render(html, compress_streams: false)
    assert pdf =~ "1 0 0 rg"
    assert {:ok, _} = Reader.read_validated(pdf)
    assert :error = HtmlValidator.parse_css_color("rgb(255,0)", {0, 0, 0})
  end

  test "required template closing tags retain the complete original candidate diagnostic" do
    html = "<body><?marker name='A'>\n<template for='A'><p>Actual</p>"
    assert {:error, {:unsupported_html, diagnostic}} = HtmlParser.parse_detailed(html)
    assert diagnostic.stage == :html and diagnostic.operation == :parse
    assert diagnostic.source == "<template for='A'>"
    assert diagnostic.line == 2 and diagnostic.column == 1
    assert diagnostic.message =~ "no original source has been discarded"
  end

  test "source-string grammar and default style-context contract accept actual caller CSS" do
    grammar = Regex.compile!("\\A(?:" <> HtmlValidator.css_string_pattern() <> ")\\z", "u")
    assert Regex.match?(grammar, ~S("real\41 text"))
    refute Regex.match?(grammar, "\"unterminated")
    assert {:ok, dom} = HtmlParser.parse_detailed("<p>Actual</p>")

    assert {:ok, false} =
             HtmlValidator.validate_source_style_input(
               dom,
               ["p{color:red}"],
               HtmlValidator.new_css_budget(),
               fn _, _, _ -> :ok end
             )

    assert {:error, {:invalid_options, diagnostic}} =
             HtmlValidator.validate_source_style_input(
               dom,
               [],
               HtmlValidator.new_css_budget(),
               fn _ -> :ok end
             )

    assert diagnostic.source == "consumer" and diagnostic.stage == :css
  end

  test "image canvas selection retains authored root ownership and body image hints" do
    png =
      File.read!(
        Path.expand("../fixtures/html_to_pdf/browser_parity/assets/initial-small.png", __DIR__)
      )

    html =
      "<html style='background-image:url(root.png)'><body background=body.png>Actual</body></html>"

    assert {:ok, dom} = HtmlParser.parse_detailed(html)

    assert {:ok, tree} =
             Style.compute_detailed(dom,
               assets: %{"root.png" => {:bytes, png}, "body.png" => {:bytes, png}}
             )

    assert tree.document_canvas.style.background_image != nil
    assert tree.document_canvas.body_background_propagated == false
    assert {:ok, layout} = Layout.layout(tree, page_size: {100, 100}, margin: 0)
    assert :ok = HtmlValidator.validate_measured_child_paint(layout.boxes, hd(tree.children))
    assert {:ok, pdf} = PdfWriter.render([%{size: layout.page_size, boxes: layout.boxes}])
    assert {:ok, text} = Text.extract(pdf)
    assert text =~ "Actual"
  end

  test "div-grouped initial select options retain genuine selected export values" do
    assert {:ok, pdf} =
             HtmlToPdf.render(
               "<select name=grouped><div><option value=a>A</option><option value=b selected>B</option></div></select>"
             )

    assert {:ok, [field]} = Forms.fields(pdf)
    assert field.name == "grouped" and field.value == "b"
    assert Enum.map(field.choices, & &1.value) == ["a", "b"]
  end

  test "Ruby optional-end-tag scope ends at actual SVG integration boundaries" do
    assert {:ok, dom} =
             HtmlParser.parse_detailed(
               "<svg><foreignObject><rb>Base<rt>Annotation</foreignObject></svg>"
             )

    [svg] = dom.children
    [foreign] = svg.children
    [base] = foreign.children
    assert base.tag == "rb" and base.namespace == :html
    assert Enum.any?(base.children, &(Map.get(&1, :tag) == "rt"))
    assert {:error, {:unsupported_html, diagnostic}} = HtmlToPdf.render(dom.source.text)
    assert diagnostic.stage == :layout
    assert diagnostic.operation == :convert
    assert diagnostic.module == NativeElixirPdfUtilities.Validators.SvgConverterValidator
    assert diagnostic.source == "foreignObject"
    assert diagnostic.message =~ "HTML content cannot be omitted"
  end

  test "canonical color sizing and corrupted initial color transport fail actionably" do
    assert {:error, {:unsupported_html, diagnostic}} =
             HtmlToPdf.render("<input type=color style='width:50%'>", forms: :static)

    assert diagnostic.stage == :style and diagnostic.source == "color"
    assert diagnostic.message =~ "fixed dimensions"
    assert {:ok, dom} = HtmlParser.parse_detailed("<input type=color value='#ff0000'>")
    assert {:ok, tree} = Style.compute_detailed(dom, forms: :static)
    [node] = tree.children
    control = node.style._form_control

    corrupted =
      put_in(node.style._form_control, %{
        control
        | attributes: Map.put(control.attributes, "type", "text")
      })

    assert {:error, {:invalid_layout, diagnostic}} =
             Layout.layout(%{tree | children: [corrupted]})

    assert diagnostic.stage == :layout and diagnostic.source == "color"
  end

  test "autocomplete applies to initial hidden and scalar states but never file or action states" do
    for type <- ~w(hidden text email range color date) do
      assert HtmlFormValidator.input_attribute_applies?(type, "autocomplete")
    end

    for type <- ~w(file button reset submit image checkbox radio) do
      refute HtmlFormValidator.input_attribute_applies?(type, "autocomplete")
    end
  end

  test "shadow controls do not inherit the light-tree fieldset disabled state or form owner" do
    html =
      "<form><fieldset disabled><div><template shadowrootmode=open><input name=shadow value=Real></template></div><input name=light value=Light></fieldset></form>"

    assert {:ok, pdf} = HtmlToPdf.render(html)
    assert {:ok, fields} = Forms.fields(pdf)
    assert Enum.find(fields, &(&1.name == "shadow")).read_only == false
    assert Enum.find(fields, &(&1.name == "light")).read_only == true
  end

  test "invalid raw email and readonly textarea contents retain canonical semantic diagnostics" do
    text = %{type: :text, text: "illegal child"}

    for {tag, attrs, source} <- [
          {"input", %{"type" => "email", "multiple" => ""}, "input"},
          {"textarea", %{"readonly" => ""}, "textarea"}
        ] do
      child =
        if tag == "input",
          do: text,
          else: %{type: :element, tag: "b", attributes: %{}, children: [text]}

      dom = %{
        type: :document,
        children: [%{type: :element, tag: tag, attributes: attrs, children: [child]}]
      }

      assert {:error, {:invalid_document, diagnostic}} = Style.compute_detailed(dom)
      assert diagnostic.stage == :style and diagnostic.source == source

      assert diagnostic.message =~
               if(tag == "input", do: "multiple requires", else: "readonly requires")
    end
  end

  test "candidate capture reports unclosed original input through an authentically issued insertion plan" do
    prefix = "<body><?marker name='A'>"
    token = "<template for='A'>"
    original = prefix <> token <> "<p>Actual</p>"
    assert {:ok, dom} = HtmlParser.parse_document_detailed(prefix <> "</body>")
    [html] = dom.children
    [_, body] = html.children
    tree = HtmlTree.from_document(dom)
    root = tree.nodes[tree.root]

    tree = %{
      tree
      | nodes:
          Map.put(tree.nodes, tree.root, %{root | source: %{text: original, line: 1, column: 1}})
    }

    origin = %{
      source: %{text: token, line: 1, column: byte_size(prefix) + 1},
      offset: byte_size(prefix),
      tokenizer_context: "body",
      ancestors: []
    }

    ResourceContext.run([], HtmlValidator.new_image_budget(), fn _ ->
      TemplatePatchValidator.with_parser_invocation(original, fn _ ->
        TemplatePatchValidator.with_insertion_context(
          tree,
          body.node_id,
          "A",
          origin,
          fn handle ->
            TemplatePatchValidator.with_planned_insertion(handle, tree, fn _ ->
              assert {:error, {:unsupported_html, diagnostic}} =
                       HtmlParser.capture_template_candidate(handle, tree)

              assert diagnostic.source == token and diagnostic.column == byte_size(prefix) + 1
              assert diagnostic.message =~ "original input remains retained"
            end)
          end
        )
      end)
    end)
  end

  test "foreign noscript metadata never acquires native head applicability in caller trees" do
    native = %{
      type: :element,
      tag: "meta",
      attributes: %{"name" => "author", "content" => "Actual"},
      children: []
    }

    foreign = %{
      type: :element,
      tag: "noscript",
      namespace: :svg,
      attributes: %{},
      children: [put_in(native.attributes["content"], "Foreign")]
    }

    dom = %{
      type: :document,
      children: [
        %{
          type: :element,
          tag: "html",
          attributes: %{},
          children: [%{type: :element, tag: "head", attributes: %{}, children: [foreign, native]}]
        }
      ]
    }

    assert HtmlValidator.document_metadata_options(dom, []) == [metadata: %{author: "Actual"}]
  end

  test "CSS dependency resource failures retain the canonical originating validator diagnostic" do
    original = Limits.effective()
    Limits.install(%{original | max_css_variable_depth: 0})

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             HtmlValidator.compute_custom_properties(%{
               "--actual" => "var(--missing,var(--other,real))"
             })

    assert diagnostic.stage == :limits and diagnostic.message =~ "max_css_variable_depth"
    Limits.install(%{original | max_html_numeric_token_bytes: 1})

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             HtmlValidator.prepare_css_length_source("calc(1pt)", :length)

    assert diagnostic.operation == :prepare_css_calculation and diagnostic.source == "calc(1pt)"
  end

  test "invalid responsive measurement and SVG reclassification reject corrupted real image transport" do
    png =
      File.read!(
        Path.expand("../fixtures/html_to_pdf/browser_parity/assets/initial-small.png", __DIR__)
      )

    assert {:ok, dom} = HtmlParser.parse_detailed("<img src=actual.png>")
    assert {:ok, tree} = Style.compute_detailed(dom, assets: %{"actual.png" => {:bytes, png}})
    [node] = tree.children
    assert {:ok, layout} = Layout.layout(tree)
    [box] = layout.boxes
    responsive = Map.merge(box, %{type: :responsive_image, responsive_style: node.style})

    assert {:error, {:invalid_layout, diagnostic}} =
             HtmlValidator.validate_measured_child_paint([responsive], node)

    assert diagnostic.operation == :measure_child and diagnostic.source == "img"
    corrupted = put_in(node.style.image, Map.put(node.style.image, :svg_document_image, %{}))
    assert {:error, {_reason, diagnostic}} = Layout.layout(%{tree | children: [corrupted]})

    assert diagnostic.module == NativeElixirPdfUtilities.Validators.SvgDocumentImageValidator and
             is_binary(diagnostic.message)
  end

  test "prepared flat source nodes reset initial form ancestry at real issued tree scope boundaries" do
    html =
      "<form><fieldset disabled><div><template shadowrootmode=open><input name=shadow value=Real></template></div><input name=light value=Light></fieldset></form>"

    assert {:ok, dom} = HtmlParser.parse_detailed(html)
    assert {:ok, composition} = HtmlComposition.compose(dom)
    [form] = HtmlFormValidator.prepare_initial_nodes(composition.composed.children)
    [fieldset] = form.children
    [host, light] = fieldset.children
    [shadow] = host.children

    assert shadow._initial_form_state.owner == nil and
             shadow._initial_form_state.disabled == false

    assert light._initial_form_state.owner == form._selector_id and
             light._initial_form_state.disabled == true
  end

  test "disabled fallback groups propagate through data scripts without inventing suggestions" do
    html =
      "<datalist id=real><select><optgroup label=Blocked disabled><script type='application/json'>{}</script><option value=a>A</option></optgroup><option value=b>B</option></select></datalist>"

    assert {:ok, dom} = HtmlParser.parse_detailed(html)
    [list] = HtmlFormValidator.prepare_initial_nodes(dom.children)

    assert Enum.map(list._initial_form_state.options, &{&1.value, &1.disabled}) == [
             {"a", true},
             {"b", false}
           ]

    assert Enum.map(list._initial_form_state.suggestions, & &1.value) == ["b"]
  end

  test "actual child producer records enforce static recomputation, aggregate limits and repeated ownership" do
    with_child_fields("<input name=first value=Alpha><input name=second value=Beta>", fn surface,
                                                                                         fields ->
      assert {:ok, descriptor} = EmbeddedFormValidator.issue(surface.context, fields)

      group = %{
        type: :drawing_group,
        width: 180,
        height: 100,
        transform: {1, 0, 0, 1, 0, 0},
        boxes: [],
        embedded_widgets: descriptor
      }

      assert {:ok, normalized} =
               WriterValidator.prepare([%{size: {180, 100}, boxes: [group]}], [])

      assert {:ok, merged} = HtmlFormValidator.merge_composition(normalized.pages, [], [])
      assert Enum.map(merged, & &1.name) == ["CHILD_1_first", "CHILD_1_second"]

      assert {:error, {:unsupported_form, diagnostic}} =
               HtmlFormValidator.merge_composition(normalized.pages, [], forms: :static)

      assert diagnostic.message =~ "recompute static child artwork"
      original = Limits.effective()

      for {key, value, message} <- [
            {:max_pdf_form_fields, 1, "shared field"},
            {:max_pdf_form_text_bytes, 7, "child PDF field name"},
            {:max_html_tree_work, 1, "max_html_tree_work"}
          ] do
        Limits.install(Map.put(original, key, value))

        assert {:error, {:resource_limit_exceeded, diagnostic}} =
                 HtmlFormValidator.merge_composition(normalized.pages, [], [])

        assert diagnostic.stage == :forms and diagnostic.message =~ message
        Limits.install(original)
      end

      [page] = normalized.pages
      [start, finish] = page.boxes
      [one, two] = start.embedded_widgets.fields
      invalid = put_in(start.embedded_widgets.fields, [one, %{two | name: one.name}])

      assert {:error, {:invalid_form, diagnostic}} =
               HtmlFormValidator.merge_composition([%{page | boxes: [invalid, finish]}], [], [])

      assert diagnostic.message =~ "duplicate HTML name"
      assert {:ok, independent} = EmbeddedFormValidator.issue(surface.context, fields)

      assert {:ok, doubled} =
               WriterValidator.prepare(
                 [%{size: {180, 100}, boxes: [group, %{group | embedded_widgets: independent}]}],
                 []
               )

      assert {:error, {:unsupported_form, diagnostic}} =
               HtmlFormValidator.merge_composition(doubled.pages, [], [])

      assert diagnostic.message =~ "child control cannot repeat"
    end)
  end

  test "actual appearance text exhaustion retains its shared resource failure" do
    assert {:ok, dom} = HtmlParser.parse_detailed("<input name=actual value='Actual value'>")
    assert {:ok, tree} = Style.compute_detailed(dom)
    assert {:ok, layout} = Layout.layout(tree)

    assert {:ok, _, [field]} =
             HtmlFormValidator.prepare([%{size: layout.page_size, boxes: layout.boxes}], [])

    assert field.name == "actual" and field.value == "Actual value"
    refute Map.has_key?(field, :html_owner)
    refute Map.has_key?(field, :html_control_id)
    Limits.install(%{Limits.effective() | max_appearance_text_bytes: 1})

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             HtmlFormValidator.prepare([%{size: layout.page_size, boxes: layout.boxes}], [])

    assert diagnostic.stage == :forms and diagnostic.message =~ "max_appearance_text_bytes"
  end

  test "already styled image hosts preserve real authored dimensions when preparing skipped artwork" do
    png =
      File.read!(
        Path.expand("../fixtures/html_to_pdf/browser_parity/assets/initial-small.png", __DIR__)
      )

    assert {:ok, dom} =
             HtmlParser.parse_detailed(
               "<input type=image width=48 height=24 src=actual.png style='content-visibility:hidden'>"
             )

    assert {:ok, tree} =
             Style.compute_detailed(dom, forms: :static, assets: %{"actual.png" => {:bytes, png}})

    [original] = dom.children
    [styled] = tree.children
    assert styled.style.image_display == :inline
    skipped = HtmlFormValidator.prepare_skipped_input_style(original, styled.style, [])
    assert skipped.width == 36 and skipped.height == 18
    assert skipped.display == :inline_block
    refute Map.has_key?(skipped, :image)
    refute Map.has_key?(skipped, :_responsive_image)
  end

  test "caller script and head noscript trees receive their original content-model diagnostics" do
    text = %{type: :text, text: "Actual"}
    paragraph = %{type: :element, tag: "p", attributes: %{}, children: [text]}

    script = %{
      type: :element,
      tag: "script",
      attributes: %{"type" => "application/json"},
      children: [paragraph]
    }

    assert {:error, {:invalid_document, diagnostic}} =
             Style.compute_detailed(%{type: :document, children: [script]})

    assert diagnostic.stage == :style and diagnostic.source == "script"
    assert diagnostic.message =~ "text-only contents"
    title = %{type: :element, tag: "title", attributes: %{}, children: [text]}
    noscript = %{type: :element, tag: "noscript", attributes: %{}, children: [title]}
    head = %{type: :element, tag: "head", attributes: %{}, children: [noscript]}
    html = %{type: :element, tag: "html", attributes: %{}, children: [head]}

    assert {:error, {:invalid_document, diagnostic}} =
             Style.compute_detailed(%{type: :document, children: [html]})

    assert diagnostic.stage == :style and diagnostic.source == "noscript"
    assert diagnostic.message =~ "head contents accept only supported style and meta"
  end

  defp with_child_fields(html, callback) do
    ResourceContext.run([], HtmlValidator.new_image_budget(), fn parent ->
      {:ok, prepared} = EmbeddedViewport.prepare_iframe_document({:srcdoc, html}, parent)
      {:ok, surface} = EmbeddedViewport.style_viewport(prepared, 180, 100)

      fields =
        ResourceContext.with_document(surface.context, fn ->
          {:ok, layout} = Layout.layout_viewport(surface.tree, 180, 100)

          {:ok, _, fields} =
            HtmlFormValidator.prepare_scoped(
              [%{size: {180, 100}, boxes: layout.boxes}],
              [],
              surface.context
            )

          fields
        end)

      callback.(surface, fields)
    end)
  end
end
