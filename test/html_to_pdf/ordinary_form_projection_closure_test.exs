defmodule NativeElixirPdfUtilities.HtmlToPdf.OrdinaryFormProjectionClosureTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.{Forms, Limits, Text}

  alias NativeElixirPdfUtilities.HtmlToPdf.{
    Font,
    HtmlParser,
    Layout,
    Pagination,
    PdfWriter,
    ResourceContext,
    Style
  }

  alias NativeElixirPdfUtilities.Pdf.{FormWriter, IncrementalWriter, Reader}

  alias NativeElixirPdfUtilities.Validators.{
    EmbeddedFormValidator,
    FormAppearanceValidator,
    FormValidator,
    HtmlFormValidator,
    HtmlValidator,
    WriterValidator
  }

  setup do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)

    assert {:ok, tree} =
             HtmlParser.parse_detailed("""
             <body style="margin:0">
             <input name="editor" value="Original" style="display:block;font-family:DejaVu Sans">
             <input type="radio" name="pick" value="a" checked>
             <input type="radio" name="pick" value="b">
             </body>
             """)

    assert {:ok, styled} = Style.compute_detailed(tree)
    assert {:ok, layout} = Layout.layout(styled, page_size: {200, 120}, margin: 10)
    assert {:ok, pages} = Pagination.paginate(layout)
    assert {:ok, prepared} = WriterValidator.prepare(pages, [])
    assert {:ok, cleaned, fields} = HtmlFormValidator.prepare(prepared.pages, forms: :interactive)
    assert {:ok, background} = PdfWriter.render(cleaned, forms: :static)
    assert {:ok, context} = Reader.read_validated(background)
    {:ok, fields: fields, context: context}
  end

  test "unscoped prepared text uses a local frame for portable original-font filling", %{
    fields: fields,
    context: context
  } do
    field = Enum.find(fields, &(&1.name == "editor"))
    refute Map.has_key?(field.appearance, :paint_frame)
    assert field.source_editor.origin.x == 0 and field.source_editor.origin.y == 0

    assert {field.source_editor.origin.width, field.source_editor.origin.height} ==
             field.source_editor.size

    assert {:ok, appearance} =
             FormAppearanceValidator.appearance(field, field.source_editor, "Changed ffi")

    refute Map.has_key?(appearance, :paint_frame)
    assert {:ok, pdf} = FormWriter.create(context, fields)
    assert {:ok, initial} = Forms.fields(pdf)
    assert {:ok, filled} = Forms.fill(pdf, %{"editor" => "Changed ffi"})
    assert {:ok, current} = Forms.fields(filled)
    assert Enum.map(initial, & &1.widgets) == Enum.map(current, & &1.widgets)
    assert {:ok, flat} = Forms.flatten(filled)
    assert {:ok, text} = Text.extract(flat, layout: false)
    assert text =~ "Changed ffi"
    output = ".quality/orchestration/ordinary-forms-v5/local-frame"
    File.mkdir_p!(output)
    File.write!(Path.join(output, "filled.pdf"), filled)
    File.write!(Path.join(output, "flat.pdf"), flat)
  end

  test "real radio AP states survive affine then axis composition", %{fields: fields} do
    ResourceContext.run([], HtmlValidator.new_image_budget(), fn context ->
      assert {:ok, source} = EmbeddedFormValidator.issue(context, fields)

      for {matrix, viewport} <- [
            {nil, {0, 0, 180, 240}},
            {{1, 0, 0, 0, 0, 0}, {0, 0, 180, 240}},
            {{1, 0, 0, 1, 0, 0}, {0, 0, -1, 240}},
            {{1, 0, 0, 1, 0, 0}, :invalid}
          ] do
        assert {:error, {:invalid_form, diagnostic}} =
                 EmbeddedFormValidator.compose_affine(source, matrix, viewport, nil)

        assert diagnostic.stage == :forms
        assert diagnostic.message =~ "child field affine placement"
      end

      assert {:ok, affine} =
               EmbeddedFormValidator.compose_affine(
                 source,
                 {0, 1, -1, 0, 140, 10},
                 {0, 0, 180, 240},
                 nil
               )

      radios = Enum.filter(affine, &(&1.type == :radio))
      assert length(radios) == 2
      assert Enum.all?(radios, &(length(&1.appearance_states) == 2))
      assert {:ok, intermediate} = EmbeddedFormValidator.issue(context, affine)
      assert {:ok, final} = EmbeddedFormValidator.compose(intermediate, {1, 1, 0, 0, nil, nil})
      assert Enum.map(final, & &1.value) == Enum.map(fields, & &1.value)

      assert {:ok, base} = PdfWriter.render([%{size: {180, 240}, boxes: []}])
      assert {:ok, reader} = Reader.read_validated(base)
      assert {:ok, pdf} = FormWriter.create(reader, final)
      assert {:ok, changed} = Forms.fill(pdf, %{"pick" => "b"})

      assert {:ok, [%{name: "editor"}, %{name: "pick", value: "b", widgets: [_, _]}]} =
               Forms.fields(changed)

      assert {:ok, flat} = Forms.flatten(changed)
      assert {:ok, []} = Forms.fields(flat)
    end)
  end

  test "source paint cleanup follows real nested native appearance groups", %{fields: fields} do
    field = Enum.find(fields, &(&1.name == "editor"))
    {width, height} = field.appearance.size
    assert {:ok, artwork} = EmbeddedFormValidator.source_paint(field.appearance.boxes)

    group = %{
      type: :drawing_group,
      width: width,
      height: height,
      transform: {1, 0, 0, 1, 0, 0},
      boxes: artwork
    }

    assert {:ok, [cleaned]} = EmbeddedFormValidator.source_paint([group])
    assert Enum.all?(cleaned.boxes, &(not Map.has_key?(&1, :form_control)))
    assert Enum.any?(cleaned.boxes, &(&1.type == :text and &1.text == "Original"))
    leaf = Enum.find(cleaned.boxes, &(&1.type == :text))
    assert {:ok, ^leaf} = EmbeddedFormValidator.issue_drawing_slice(leaf)
    assert {:ok, pdf} = PdfWriter.render([%{size: {width, height}, boxes: [cleaned]}])
    assert {:ok, text} = Text.extract(pdf, layout: false)
    assert text =~ "Original"
  end

  test "static native AP placement matches the validated live widget placement", %{
    context: context,
    fields: fields
  } do
    assert {:ok, pdf} = FormWriter.create(context, fields)
    assert {:ok, reader} = Reader.read_validated(pdf)
    assert {:ok, form} = FormValidator.inspect_document(reader)
    field = Enum.find(form.fields, &(&1.name == "editor"))
    [widget] = field.widgets
    page = Enum.at(reader.pages, widget.page - 1)

    static = %{
      ref: widget.ref,
      dictionary: widget.dictionary,
      page: page,
      page_index: widget.page,
      rect: widget.rect,
      ap: widget.ap,
      normal: widget.normal,
      flags: Map.get(widget.dictionary, "F", 0),
      annots: page.dictionary["Annots"]
    }

    assert {:ok, projected} = FormValidator.prepare_static_placement(reader, static)
    assert {:ok, [live]} = FormValidator.prepare_flatten(reader, [field], form)
    assert projected.stream == live.stream and projected.matrix == live.matrix
    assert projected.widget.rect == widget.rect
  end

  test "external password AP masking rejects actual rendered work exhaustion", %{
    fields: fields,
    context: context
  } do
    assert {:ok, pdf} = FormWriter.create(context, fields)
    assert {:ok, reader} = Reader.read_validated(pdf)
    assert {:ok, %{fields: [field | _]}} = FormValidator.inspect_document(reader)
    [widget] = field.widgets
    {:ref, {id, generation}} = field.ref
    {:ref, {widget_id, widget_generation}} = widget.ref

    assert {:ok, external} =
             IncrementalWriter.write(reader, [
               {id, generation, {:value, Map.put(field.dictionary, "Ff", 8192)}},
               {widget_id, widget_generation,
                {:value, Map.delete(widget.dictionary, "NEPUSourceEditor")}}
             ])

    assert :ok = Limits.install(%{Limits.effective() | max_rendered_text_bytes: 2})

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             Forms.fill(external, %{"editor" => "Changed"})

    assert diagnostic.source == "editor" and diagnostic.operation == :fill
    assert diagnostic.message =~ "password display exceeds max_rendered_text_bytes"
  end

  test "grouped native glyph button states extract their actual selected characters", %{
    fields: fields
  } do
    editor = Enum.find(fields, &(&1.type == :text)).source_editor
    assert {:ok, selected} = FormAppearanceValidator.validate(editor)
    face = hd(selected.loaded_fonts)

    text = %{
      type: :text,
      text: "○",
      font: Font.pdf_name(face),
      font_face: face,
      font_size: 10,
      x: 2,
      y: 4,
      color: {0, 0, 0}
    }

    group = %{type: :glyph_group, actual_text: "○", boxes: [text]}
    appearance = %{size: {16, 16}, boxes: [group]}
    radio = Enum.find(fields, &(&1.type == :radio))
    [off, on] = FormWriter.appearance_frames(%{radio | appearance: appearance})

    for {frame, expected} <- [{off, "○"}, {on, "●"}] do
      assert {:ok, pdf} = PdfWriter.render([frame])
      assert {:ok, rendered} = Text.extract(pdf, layout: false)
      assert rendered == expected
    end

    field = Enum.find(fields, &(&1.type == :text))
    [_, background] = FormWriter.appearance_frames(%{field | appearance: appearance})
    assert background.boxes == []
  end
end
