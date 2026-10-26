defmodule NativeElixirPdfUtilities.HtmlToPdf.FormAppearanceValidationTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.{HtmlToPdf, Limits, Text}
  alias NativeElixirPdfUtilities.Pdf.Reader
  alias NativeElixirPdfUtilities.HtmlToPdf.{EmbeddedViewport, Layout, ResourceContext}

  alias NativeElixirPdfUtilities.Validators.{
    FormAppearanceValidator,
    FormValidator,
    HtmlFormValidator,
    HtmlValidator
  }

  setup do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)

    assert {:ok, pdf} =
             HtmlToPdf.render("<input name='editor' value='Initial' style='border-radius:3px'>",
               system_font_discovery: false
             )

    assert {:ok, context} = Reader.read_validated(pdf)
    assert {:ok, %{fields: [field]}} = FormValidator.inspect_document(context)
    [widget] = field.widgets

    {:ok,
     document: context.document,
     field: field,
     editor: widget.source_editor,
     wire: widget.dictionary["NEPUSourceEditor"]}
  end

  test "persisted templates reject malformed fonts, unknown instructions and invalid styles", %{
    document: document,
    wire: wire
  } do
    [font | _] = wire["Fonts"]

    for altered <- [
          :invalid,
          Map.put(wire, "Unexpected", true),
          put_in(wire, ["Style", "Unexpected"], {:name, "normal"}),
          put_in(wire, ["Style", "white_space"], {:name, "unsupported"}),
          put_in(wire, ["Style", "white_space"], {:string, "normal"}),
          Map.put(wire, "Fonts", [Map.put(font, "Unexpected", true)]),
          Map.put(wire, "Fonts", [Map.put(font, "Style", {:name, "unsupported"})]),
          Map.put(wire, "Fonts", [Map.put(font, "Family", {:string, 42})]),
          Map.put(wire, "Fonts", [Map.put(font, "Data", {:ref, {99_999, 0}})])
        ] do
      assert {:error, {reason, diagnostic}} = FormAppearanceValidator.prepare(document, altered)
      assert diagnostic.reason == reason
      assert is_atom(diagnostic.stage) and is_binary(diagnostic.message)
      assert diagnostic.message != ""
    end
  end

  test "persisted geometry remains closed and bounded through every projection", %{
    document: document,
    wire: wire
  } do
    projection = %{
      "Matrix" => [1, 0, 0, 1, 0, 0],
      "Bounds" => [0, 0, 100, 30],
      "Polygons" => [[0, 0, 100, 0, 100, 30, 0, 30]]
    }

    for altered <- [
          Map.put(wire, "Clip", nil),
          put_in(wire, ["Clip", "Unexpected"], 1),
          put_in(wire, ["Clip", "Width"], -1),
          put_in(wire, ["Clip", "Inner"], Map.put(wire["Clip"], "Inner", wire["Clip"])),
          Map.put(wire, "Projections", [%{}]),
          Map.put(wire, "Projections", [Map.put(projection, "Unexpected", 1)]),
          Map.put(wire, "Projections", [Map.put(projection, "Polygons", [[0, 0, 1]])]),
          Map.put(wire, "Projections", [Map.put(projection, "Matrix", [1, 0, 0, 1, "bad", 0])]),
          Map.put(wire, "Projections", [Map.put(projection, "Bounds", [0, 0, 0, 30])]),
          Map.put(wire, "Projections", [Map.put(projection, "Polygons", [[0, 0, "bad", 0, 0, 1]])])
        ] do
      assert {:error, {reason, diagnostic}} = FormAppearanceValidator.prepare(document, altered)
      assert diagnostic.reason == reason and is_binary(diagnostic.message)
    end

    assert {:ok, rounded} =
             FormAppearanceValidator.prepare(document, put_in(wire, ["Clip", "Radius"], 2))

    assert rounded.clip.border_radius == 2
    assert :ok = Limits.install(%{Limits.effective() | max_pdf_form_depth: 0})

    assert {:error, {:unsupported_form, diagnostic}} =
             FormAppearanceValidator.prepare(document, Map.put(wire, "Projections", [projection]))

    assert diagnostic.message =~ "resource shape or depth"
  end

  test "projected text retains source font and rounded inner clip after serialization", %{
    field: field,
    editor: editor
  } do
    clip = Map.put(editor.clip, :border_radius, 2)
    editor = %{editor | clip: Map.put(clip, :inner_clip, editor.clip)}
    field = Map.put(field, :source_editor, editor)
    polygon = [{0, 0}, {100, 0}, {100, 30}, {0, 30}]

    projected =
      FormAppearanceValidator.project_field(field, {1, 0, 0, 1, 0, 0}, [polygon], {0, 0, 100, 30})

    assert FormAppearanceValidator.project_field(%{}, nil, [], nil) == %{}

    assert {:ok, [encoded], objects, next} =
             FormAppearanceValidator.encode_controls([projected], 10)

    assert next > 10 and objects != []
    assert encoded.source_editor_pdf["Clip"]["Inner"]["Width"] == editor.clip.width
    assert length(encoded.source_editor_pdf["Projections"]) == 1

    assert {:ok, appearance} =
             FormAppearanceValidator.appearance(projected, projected.source_editor, "Updated ffi")

    assert {:ok, pdf} = NativeElixirPdfUtilities.HtmlToPdf.PdfWriter.render([appearance])
    assert {:ok, text} = Text.extract(pdf, layout: false)
    assert text =~ "Updated ffi"
  end

  test "projection coordinates consume preparation entries before pair allocation", %{
    document: document,
    wire: wire
  } do
    projection = %{
      "Matrix" => [1, 0, 0, 1, 0, 0],
      "Bounds" => [0, 0, 100, 30],
      "Polygons" => [[0, 0, 100, 0, 100, 30, 0, 30]]
    }

    wire = Map.put(wire, "Projections", [projection])
    limits = Limits.effective()
    Limits.install(%{limits | max_pdf_form_preparation_entries: 7})
    assert {:ok, _} = FormAppearanceValidator.prepare(document, wire)
    Limits.install(%{limits | max_pdf_form_preparation_entries: 3})

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             FormAppearanceValidator.prepare(document, wire)

    assert diagnostic.stage == :forms and diagnostic.source == "Polygons"
  end

  test "resolved scalar painter denies foreign nodes and malformed geometry", %{editor: editor} do
    for invalid <- [
          nil,
          %{editor | size: {0, 30}},
          %{editor | fonts: []},
          %{editor | style: Map.put(editor.style, :font_size, 0)},
          %{editor | style: Map.put(editor.style, :white_space, :unsupported)},
          %{editor | projections: [%{}]},
          %{
            editor
            | projections: [
                %{
                  matrix: {1, 0, 0, 1, 0, 0},
                  bounds: {0, 0, 100, 30},
                  polygons: [[nil, nil, nil]]
                }
              ]
          }
        ] do
      assert {:error, {reason, diagnostic}} = FormAppearanceValidator.validate(invalid)
      assert diagnostic.reason == reason and is_atom(diagnostic.stage)
    end

    assert {:error, {_, diagnostic}} =
             FormAppearanceValidator.prepare_paint([%{type: :element}], editor.style, editor)

    assert diagnostic.message =~ "resolved scalar text"

    assert {:error, {_, _}} = FormAppearanceValidator.prepare_paint([], editor.style, nil)

    assert {:error, {_, _}} =
             FormAppearanceValidator.encode_controls(
               [%{source_editor: %{editor | fonts: []}}],
               10
             )
  end

  test "unsupported physical projection fails at the AP composition boundary", %{
    editor: editor,
    field: field
  } do
    singular = %{
      editor
      | projections: [
          %{
            matrix: {1, 0, 0, 0, 0, 0},
            bounds: {0, 0, 100, 30},
            polygons: [[{0, 0}, {100, 0}, {100, 30}, {0, 30}]]
          }
        ]
    }

    assert {:error, {_, diagnostic}} =
             FormAppearanceValidator.appearance(field, singular, "Updated")

    assert diagnostic.source == "editor" and diagnostic.operation == :write_pdf

    defaulted = %{
      editor
      | style:
          Map.drop(editor.style, [
            :white_space,
            :text_align,
            :word_break,
            :overflow_wrap,
            :line_break
          ])
    }

    assert {:ok, [encoded], _, _} =
             FormAppearanceValidator.encode_controls([%{source_editor: defaulted}], 10)

    assert encoded.source_editor_pdf["Style"]["text_align"] == {:name, "left"}
    assert encoded.source_editor_pdf["Style"]["line_break"] == {:name, "auto"}
  end

  test "source capture follows grouped appearance leaves and a number editor gutter" do
    ResourceContext.run([], HtmlValidator.new_image_budget(), fn parent ->
      assert {:ok, prepared} =
               EmbeddedViewport.prepare_iframe_document(
                 {:srcdoc,
                  "<body style='margin:0'><input name='editor' value='Grouped'><input type='number' name='number' value='2'></body>"},
                 parent
               )

      assert {:ok, surface} = EmbeddedViewport.style_viewport(prepared, 200, 80)
      assert {:ok, layout} = Layout.layout_viewport(surface.tree, 200, 80)

      assert {:ok, _, fields} =
               HtmlFormValidator.prepare_scoped(
                 [%{size: {200, 80}, boxes: layout.boxes}],
                 [],
                 surface.context
               )

      field = Enum.find(fields, &(&1.name == "editor"))
      control = Enum.find_value(layout.boxes, &Map.get(&1, :form_control))
      {width, height} = field.appearance.size

      group = %{
        type: :drawing_group,
        width: width,
        height: height,
        transform: {1, 0, 0, 1, 0, 0},
        boxes: field.appearance.boxes
      }

      grouped = put_in(field, [:appearance, :boxes], [group])
      assert captured = FormAppearanceValidator.capture(grouped, control)
      assert captured.fonts == field.source_editor.fonts
      assert captured.clip == field.source_editor.clip
      assert Enum.find(fields, &(&1.name == "number")).source_editor != nil
    end)
  end

  test "source text work exhaustion includes the field and operation", %{
    editor: editor,
    field: field
  } do
    assert :ok = Limits.install(%{Limits.effective() | max_layout_text_work: 0})

    HtmlValidator.with_render_budget(fn ->
      assert {:error, {:resource_limit_exceeded, diagnostic}} =
               FormAppearanceValidator.appearance(field, editor, "Updated")

      assert diagnostic.source == "editor"
      assert diagnostic.operation == :prepare_form_appearance
      assert diagnostic.message =~ "max_layout_text_work"
    end)
  end
end
