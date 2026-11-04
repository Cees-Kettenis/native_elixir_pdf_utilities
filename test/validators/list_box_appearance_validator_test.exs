defmodule NativeElixirPdfUtilities.Validators.ListBoxAppearanceValidatorTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.{HtmlToPdf, Limits}
  alias NativeElixirPdfUtilities.Pdf.Reader

  alias NativeElixirPdfUtilities.Validators.{
    FormAppearanceValidator,
    FormInspectionValidator,
    FormValidator,
    ListBoxAppearanceValidator
  }

  setup do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)

    assert {:ok, pdf} =
             HtmlToPdf.render(
               "<select name=choice size=3 style='font-family:DejaVu Sans'><option value=a selected>Alpha</option><option value=b>Beta</option></select>",
               system_font_discovery: false
             )

    assert {:ok, context} = Reader.read_validated(pdf)
    assert {:ok, %{fields: [field]}} = FormValidator.inspect_document(context)
    [widget] = field.widgets

    %{
      document: context.document,
      field: field,
      editor: widget.source_editor,
      wire: widget.dictionary["NEPUSourceEditor"],
      limits: limits
    }
  end

  test "persisted rows reject unknown keys invalid indices numeric geometry and font facts", %{
    document: document,
    wire: wire
  } do
    [row | _] = wire["ListBox"]

    for rows <- [
          42,
          [%{}],
          [Map.put(row, "Extra", true)],
          [Map.put(row, "Background", 42)],
          [Map.put(row, "Selection", [2, 0, 0])],
          [Map.put(row, "Index", -1)],
          [Map.put(row, "Geometry", [0, 0, 1, 1, 0, 0, 0, 1, 0])],
          [Map.put(row, "Geometry", [0, 0, 1, 1, 0, 0, 1, 1, 1.0e300])],
          [Map.put(row, "Geometry", [0, 0, 1, 1, 0, 0, 1, 1, :bad])],
          [Map.put(row, "Color", [2, 0, 0])],
          [Map.put(row, "Label", {:string, <<255>>})],
          [Map.put(row, "Family", {:string, <<255>>})],
          [Map.put(row, "Weight", :bad)],
          [Map.put(row, "Weight", 42)],
          [Map.put(row, "Style", {:name, "unsupported"})]
        ] do
      assert {:error, {reason, diagnostic}} =
               FormAppearanceValidator.prepare(document, Map.put(wire, "ListBox", rows))

      assert diagnostic.reason == reason and diagnostic.stage == :forms
    end
  end

  test "canonical rows use the same validator and admit their aggregate bounds", %{
    editor: editor,
    wire: wire,
    document: document,
    limits: limits
  } do
    [row | _] = editor.listbox.rows

    for model <- [
          :bad,
          %{rows: :bad},
          %{rows: [Map.delete(row, :height)]},
          %{rows: [%{row | color: :bad}]},
          %{rows: [%{row | color: {2, 0, 0}}]}
        ] do
      assert {:error, {:unsupported_form, _}} = ListBoxAppearanceValidator.validate(model)
    end

    Limits.install(%{limits | max_pdf_form_preparation_entries: 1})

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             FormAppearanceValidator.prepare(document, wire)

    assert diagnostic.message =~ "max_pdf_form_preparation_entries"
    Limits.install(%{limits | max_pdf_form_metadata_bytes: 1})

    assert {:error, {:resource_limit_exceeded, _}} =
             ListBoxAppearanceValidator.validate(editor.listbox)

    FormInspectionValidator.with_scope(document, fn ->
      assert {:ok, nil} = ListBoxAppearanceValidator.decode(nil)
    end)
  end

  test "source painting rejects missing fonts and inconsistent row indices", %{
    editor: editor,
    field: field
  } do
    [row | _] = editor.listbox.rows

    for changed <- [
          %{field | dictionary: Map.put(field.dictionary, "TI", -1)},
          %{field | choices: []}
        ] do
      assert {:error, {:unsupported_form, _}} =
               ListBoxAppearanceValidator.appearance(changed, editor, "a")
    end

    invalid_font = %{editor | listbox: %{rows: [%{row | family: "Missing"}]}}

    assert {:error, {:unsupported_form, _}} =
             ListBoxAppearanceValidator.appearance(field, invalid_font, "a")

    unsupported = %{
      field
      | choices: [%{value: "a", label: <<0x10FFFF::utf8>>}, Enum.at(field.choices, 1)]
    }

    assert {:error, _} = ListBoxAppearanceValidator.appearance(unsupported, editor, "a")
  end

  test "source list box frames preserve the top option through native affine projection", %{
    field: field,
    editor: editor
  } do
    projected =
      FormAppearanceValidator.project_field(
        Map.put(field, :source_editor, editor),
        {0.8, 0, 0, 0.8, 10, 10},
        [[{0, 0}, {900, 0}, {900, 900}, {0, 900}]],
        {0, 0, 900, 900}
      )

    assert {:ok, frame} = FormAppearanceValidator.appearance(field, projected.source_editor, "b")
    assert is_integer(frame.choice_top_index)
    assert {:ok, pdf} = NativeElixirPdfUtilities.HtmlToPdf.PdfWriter.render([frame])
    assert {:ok, text} = NativeElixirPdfUtilities.Text.extract(pdf, layout: false)
    assert text =~ "Alpha" and text =~ "Beta"
  end

  test "projected list box failures retain the shared resource diagnostic", %{
    field: field,
    editor: editor,
    limits: limits
  } do
    projected =
      FormAppearanceValidator.project_field(
        Map.put(field, :source_editor, editor),
        {1, 0, 0, 1, 0, 0},
        [[{0, 0}, {900, 0}, {900, 900}, {0, 900}]],
        {0, 0, 900, 900}
      )

    Limits.install(%{limits | max_pdf_xref_revisions: 1})

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             FormAppearanceValidator.appearance(field, projected.source_editor, "b")

    assert diagnostic.message =~ "revision"
  end
end
