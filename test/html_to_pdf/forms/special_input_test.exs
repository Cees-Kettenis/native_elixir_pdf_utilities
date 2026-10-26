alias NativeElixirPdfUtilities.TestSupport.Paths

defmodule NativeElixirPdfUtilities.HtmlToPdf.SpecialInputTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.{Forms, HtmlToPdf, Limits, Text}
  alias NativeElixirPdfUtilities.HtmlToPdf.{ColorArtwork, HtmlParser, Layout, PdfWriter, Style}
  alias NativeElixirPdfUtilities.Pdf.Reader
  alias NativeElixirPdfUtilities.Validators.{FormValidator, HtmlFormValidator, HtmlValidator}
  @image Paths.fixture("html_to_pdf/browser_parity/assets/iframe-bars.png")

  test "opaque color sanitation delegates shared CSS colors and preserves authored source" do
    for {value, canonical} <- [
          {"", "#000000"},
          {"bad", "#000000"},
          {"transparent", "#000000"},
          {"#ABCDEF", "#abcdef"},
          {"#abc", "#aabbcc"},
          {"red", "#ff0000"},
          {"rgb(1,2,3)", "#010203"},
          {"rgba(255,0,0,0.5)", "#ff0000"},
          {"#11223344", "#112233"}
        ] do
      assert {:ok, state} = HtmlFormValidator.prepare_initial_color(%{"value" => value})
      assert state.value == canonical and state.color_input.value == canonical
      assert {:ok, dom} = HtmlParser.parse_detailed("<input type=color id=ink value='#{value}'>")
      assert {:ok, %{children: [node]}} = Style.compute_detailed(dom, forms: :static)
      assert node.style._form_control.attributes["value"] == value
      assert node.style._form_control.initial_state.value == canonical
      [prepared] = HtmlFormValidator.prepare_initial_nodes(dom.children)
      assert prepared.node_id == hd(dom.children).node_id
      assert prepared.document_id == hd(dom.children).document_id
      assert node.children == []
    end
  end

  test "inactive color attributes preserve source without changing the canonical swatch" do
    assert {:ok, expected} =
             HtmlToPdf.render("<input type=color>", forms: :static, compress_streams: false)

    for attrs <- [
          "required",
          "multiple",
          "readonly",
          "src=x"
        ] do
      source = "<input type=color #{attrs}>"
      assert {:ok, %{children: [node]}} = HtmlParser.parse_detailed(source)
      assert node.source.text == source
      assert map_size(node.attributes) == 2
      assert {:ok, canonical} = HtmlFormValidator.prepare_initial_color(node.attributes)
      assert canonical.value == "#000000"

      assert {:ok, pdf} =
               HtmlToPdf.render(source,
                 forms: :static,
                 compress_streams: false,
                 asset_resolver: fn request ->
                   flunk("inactive color resource #{inspect(request)}")
                 end
               )

      assert pdf == expected
    end

    assert {:ok, _} =
             HtmlToPdf.render("<input type=color style='width:5pt;height:5pt'>", forms: :static)
  end

  test "native swatch geometry and disabled fieldset state paint canonical RGB without hex text" do
    assert {:ok, dom} =
             HtmlParser.parse_detailed(
               "<fieldset disabled><input type=color name=ink value=red></fieldset>"
             )

    assert {:ok, %{children: [fieldset]}} = Style.compute_detailed(dom, forms: :static)
    node = hd(fieldset.children)
    assert node.style._color_disabled and node.style._color_state.rgb == {1.0, 0.0, 0.0}
    assert [swatch] = ColorArtwork.boxes(node.style, 10, 20, 37.5, 20.25)
    assert swatch.x == 13.75 and swatch.y == 24.5
    assert swatch.width == 30 and swatch.height == 11.25
    assert swatch.stroke_width == 0.75 and swatch.fill_color == {1.0, 0.0, 0.0}
    assert {:ok, pdf} = HtmlToPdf.render("<input type=color value=red>", forms: :static)
    assert {:error, {:no_extractable_text, _}} = Text.extract(pdf, layout: false)
  end

  test "inline color uses the empty swatch margin-edge baseline" do
    assert {:ok, dom} =
             HtmlParser.parse_detailed(
               "<p style='font-size:10.5pt;line-height:30pt'>Before<input type=color value=red>After</p>"
             )

    assert {:ok, %{children: [paragraph]} = styled} =
             Style.compute_detailed(dom, forms: :static, system_font_discovery: false)

    color = Enum.find(paragraph.children, &(Map.get(&1, :tag) == "input"))
    assert color.style.form_kind == :color and color.children == []
    assert {:ok, layout} = Layout.layout(styled)
    host = Enum.find(layout.boxes, &(get_in(&1, [:form_control, :attributes, "type"]) == "color"))
    before = Enum.find(layout.boxes, &(Map.get(&1, :text) == "Before"))
    assert host.y == before.y - 4.5 and host.width == 37.5 and host.height == 20.25

    assert {:ok, middle_dom} =
             HtmlParser.parse_detailed(
               "<p style='font-family:DejaVu Sans;font-size:10.5pt;line-height:30pt'>Before<input type=color value=red style=vertical-align:middle>After</p>"
             )

    assert {:ok, middle_styled} =
             Style.compute_detailed(middle_dom, forms: :static, system_font_discovery: false)

    assert {:ok, middle_layout} = Layout.layout(middle_styled)

    middle_host =
      Enum.find(
        middle_layout.boxes,
        &(get_in(&1, [:form_control, :attributes, "type"]) == "color")
      )

    before = Enum.find(middle_layout.boxes, &(Map.get(&1, :text) == "Before"))
    assert_in_delta middle_host.y + middle_host.height / 2, before.y + 3.0, 0.000001
  end

  test "color layout rejects forged canonical state and missing authority" do
    assert {:ok, dom} = HtmlParser.parse_detailed("<input type=color value=red>")
    assert {:ok, %{children: [node]} = styled} = Style.compute_detailed(dom, forms: :static)

    for forged <- [
          put_in(node.style._color_state.rgb, {0, 1, 0}),
          %{node | style: Map.delete(node.style, :_form_control)},
          %{node | style: Map.delete(node.style, :_color_state)},
          update_in(node.style._form_control.initial_state, &Map.delete(&1, :value)),
          put_in(node.style._form_control.attributes["value"], nil)
        ] do
      assert {:error, {:invalid_layout, diagnostic}} =
               Layout.layout(%{styled | children: [forged]})

      assert diagnostic.stage == :layout and diagnostic.source == "color"
    end
  end

  test "missing color state with malformed authority returns a public diagnostic" do
    assert {:ok, dom} = HtmlParser.parse_detailed("<input type=color value=red>")
    assert {:ok, %{children: [node]} = styled} = Style.compute_detailed(dom, forms: :static)

    for attributes <- [nil, "color", 1, []] do
      malformed =
        node.style
        |> Map.delete(:_color_state)
        |> Map.delete(:_form_control)
        |> Map.put(:_color_control, %{tag: "input", attributes: attributes})

      assert {:error, {:invalid_layout, diagnostic}} =
               Layout.layout(%{styled | children: [%{node | style: malformed}]})

      assert diagnostic.stage == :layout and diagnostic.operation == :layout
      assert diagnostic.module == HtmlFormValidator and diagnostic.source == "color"
      assert diagnostic.message =~ "canonical"
    end
  end

  test "mutable color actions are diagnosed while disabled scalar transport remains exact" do
    assert {:error, {:unsupported_form, diagnostic}} =
             HtmlToPdf.render("<input type=color name=ink value=red>")

    assert diagnostic.stage == :forms and diagnostic.source == "color"
    assert {:ok, pdf} = HtmlToPdf.render("<input type=color disabled name=ink value='#ABCDEF'>")
    assert {:ok, context} = Reader.read_validated(pdf)
    assert {:ok, %{fields: [field]}} = FormValidator.inspect_document(context)
    assert field.value == "#abcdef" and Bitwise.band(field.flags, 1) == 1
    assert {:ok, _} = Forms.flatten(pdf)
  end

  test "image input uses authorized image sizing while preserving original control identity" do
    bytes = File.read!(@image)

    assert {:ok, dom} =
             HtmlParser.parse_detailed(
               "<input type=image id=send src=tile alt=Send width=80 height=40 style='width:30pt;height:auto'>"
             )

    assert {:ok, %{children: [node]}} =
             Style.compute_detailed(dom, forms: :static, assets: %{"tile" => {:bytes, bytes}})

    assert node.tag == "input"
    [prepared] = HtmlFormValidator.prepare_initial_nodes(dom.children)
    assert prepared.node_id == hd(dom.children).node_id
    assert node.style.display == :image and node.style.image.format == :png
    assert node.style.width == 30 and node.style.height == :auto
    assert node.style.image.width == 24 and node.style.image.height == 6
    assert node.style.image.width_px == 32 and node.style.image.height_px == 8
    assert node.style._image_input_state.image_input == %{src: "tile", alt: "Send"}
    refute Map.has_key?(node.style, :_form_control)

    assert {:ok, pdf} =
             HtmlToPdf.render("<input type=image src=tile alt=Send width=80 height=40>",
               forms: :static,
               assets: %{"tile" => {:bytes, bytes}}
             )

    assert {:ok, context} = Reader.read_validated(pdf)
    assert {:ok, %{fields: []}} = FormValidator.inspect_document(context)
  end

  test "image resolution remains caller authorized and interactive submit actions fail explicitly" do
    callback_owner = self()
    bytes = File.read!(@image)

    assert {:ok, _} =
             HtmlToPdf.render("<input type=image src=tile>",
               forms: :static,
               asset_resolver: fn request ->
                 send(callback_owner, {:asset, request})
                 {:ok, bytes}
               end
             )

    assert_receive {:asset, %{kind: :image, reference: "tile"}}

    assert {:error, {reason, diagnostic}} =
             HtmlToPdf.render("<input type=image src='https://example.invalid/private.png'>",
               forms: :static
             )

    assert reason == diagnostic.reason and diagnostic.message =~ "not fetched"

    assert {:error, {:unsupported_form, %{stage: :forms, source: "input/image"}}} =
             HtmlToPdf.render("<input type=image src=tile>", assets: %{"tile" => {:bytes, bytes}})

    assert {:ok, _} =
             HtmlToPdf.render("<input type=image disabled src=tile>",
               assets: %{"tile" => {:bytes, bytes}}
             )
  end

  test "file state is always empty and never resolves its authored value" do
    html =
      "<input id=upload type=file value='/private/secret.txt' accept='.pdf,image/*' multiple required hidden>"

    assert {:ok, dom} = HtmlParser.parse_detailed(html)
    [node] = HtmlFormValidator.prepare_initial_nodes(dom.children)
    assert node._initial_form_state.value == "" and node._initial_form_state.files == []
    assert node._initial_form_state.multiple and node._initial_form_state.required
    assert node._initial_form_state.accept == ".pdf,image/*"
    assert node.attributes["value"] == "/private/secret.txt"

    assert {:ok, _} =
             HtmlToPdf.render(html,
               asset_resolver: fn request -> flunk("file value resolved #{inspect(request)}") end
             )

    assert {:error, {:unsupported_form, diagnostic}} =
             HtmlToPdf.render("<input type=file value='/private/secret.txt'>")

    assert diagnostic.stage == :forms and diagnostic.source == "input/file"
    assert diagnostic.message =~ "forms: :static"
  end

  test "image and file sources cannot be forged into PDF scalar control metadata" do
    for type <- ["image", "file"] do
      assert {:ok, dom} =
               HtmlParser.parse_detailed(
                 "<input type=#{type} #{if(type == "image", do: "src=tile", else: "")} hidden>"
               )

      [source] = HtmlFormValidator.prepare_initial_nodes(dom.children)

      control = %{
        id: source._selector_id,
        order: [0],
        tag: "input",
        attributes: source.attributes,
        children: [],
        initial_state: source._initial_form_state
      }

      box = %{
        type: :rect,
        x: 10,
        y: 10,
        width: 20,
        height: 20,
        border_radius: 0,
        fill_color: nil,
        stroke_color: nil,
        stroke_width: 0,
        form_control: control
      }

      assert {:error, {:invalid_form, diagnostic}} =
               PdfWriter.render([%{size: {100, 100}, boxes: [box]}], forms: :interactive)

      assert diagnostic.stage == :forms and diagnostic.operation == :write_pdf
      assert diagnostic.source == "initial_state" and diagnostic.module == HtmlFormValidator
    end
  end

  test "rounded image input carries a content clip including unequal edges" do
    bytes = File.read!(@image)

    assert {:ok, dom} =
             HtmlParser.parse_detailed(
               "<input type=image src=tile style='width:60pt;height:30pt;border-radius:6pt;object-fit:cover'>"
             )

    assert {:ok, styled} =
             Style.compute_detailed(dom, forms: :static, assets: %{"tile" => {:bytes, bytes}})

    assert {:ok, layout} = Layout.layout(styled)
    image = Enum.find(layout.boxes, &(&1.type == :image))
    assert image.clip.border_radius == 6 and image.clip.width == 60 and image.clip.height == 30
    assert image.width == 120 and image.height == 30

    for css <- ["border-left:2pt solid black", "padding-left:2pt"] do
      assert {:ok, _} =
               HtmlToPdf.render("<input type=image src=tile style='border-radius:6pt;#{css}'>",
                 forms: :static,
                 assets: %{"tile" => {:bytes, bytes}}
               )
    end
  end

  test "source-only preparation retains special control IDs and empty file state without loading images" do
    assert {:ok, dom} =
             HtmlParser.parse_detailed(
               "<input type=image id=send src='https://example.invalid/unfetched.png' alt=Send><input type=file id=upload value='/private/secret.txt'><input type=color id=ink value=red>"
             )

    parent = self()

    assert {:ok, _} =
             Style.with_source_style_contexts(dom, [], HtmlValidator.new_css_budget(), fn node,
                                                                                          pseudo,
                                                                                          _context ->
               if pseudo == nil and Map.has_key?(node, :_initial_form_state),
                 do: send(parent, {:source_control, node})

               :ok
             end)

    for original <- dom.children do
      assert_receive {:source_control, prepared}

      assert Map.take(prepared, [:node_id, :document_id, :tree_scope_id, :parent_id, :source]) ==
               Map.take(original, [:node_id, :document_id, :tree_scope_id, :parent_id, :source])

      case original.attributes["type"] do
        "file" ->
          assert prepared._initial_form_state.value == "" and
                   prepared._initial_form_state.files == []

        "color" ->
          assert prepared._initial_form_state.value == "#ff0000"

        "image" ->
          assert prepared._initial_form_state.image_input.src == original.attributes["src"]
      end
    end
  end

  test "image decoding and source-byte ceilings retain shared actionable failures" do
    assert {:error, {reason, diagnostic}} =
             HtmlToPdf.render("<input type=image src=bad>",
               forms: :static,
               assets: %{"bad" => {:bytes, "invalid image bytes"}}
             )

    assert reason == diagnostic.reason and diagnostic.stage == :style and diagnostic.message != ""
    previous = Limits.effective()
    on_exit(fn -> Limits.install(previous) end)
    Limits.install(%{previous | max_image_source_bytes: 1})

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             HtmlToPdf.render("<input type=image src=tile>",
               forms: :static,
               assets: %{"tile" => {:bytes, File.read!(@image)}}
             )

    assert diagnostic.message =~ "source"
  end

  test "native color stroke adjustment remains local and artwork counts toward layout ceiling" do
    assert {:ok, pdf} =
             HtmlToPdf.render("<p>Before</p><input type=color value=red><p>After</p>",
               forms: :static,
               compress_streams: false
             )

    assert pdf =~ "/SA true"
    assert {:ok, text} = Text.extract(pdf, layout: false)
    assert text =~ "Before" and text =~ "After"
    previous = Limits.effective()
    on_exit(fn -> Limits.install(previous) end)
    Limits.install(%{previous | max_layout_boxes: 1})

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             HtmlToPdf.render("<input type=color value=red>", forms: :static)

    assert diagnostic.stage == :layout and diagnostic.message =~ "max_layout_boxes"
  end

  test "color preparation and native artwork consume existing budgets" do
    previous = Limits.effective()
    on_exit(fn -> Limits.install(previous) end)
    Limits.install(%{previous | max_html_tree_work: 10})

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             HtmlValidator.with_render_budget(fn ->
               HtmlFormValidator.prepare_initial_color(%{"value" => "long invalid color"})
             end)

    assert diagnostic.message =~ "max_html_tree_work"
  end
end
