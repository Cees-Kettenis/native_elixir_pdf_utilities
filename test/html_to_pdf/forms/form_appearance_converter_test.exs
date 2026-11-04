defmodule NativeElixirPdfUtilities.HtmlToPdf.FormAppearanceConverterTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.{Forms, HtmlToPdf, Limits, Text}
  alias NativeElixirPdfUtilities.Pdf.{IncrementalWriter, Reader}
  alias NativeElixirPdfUtilities.Validators.{FormAppearanceValidator, FormValidator}

  setup do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    :ok
  end

  test "ordinary editor font and style survive the existing portable template wire" do
    bytes = File.read!("priv/fonts/dejavu/DejaVuSansMono.ttf")

    html =
      "<input name='editor' value='Initial' style='font-family:OriginalEditor;font-size:14px;line-height:20px;color:#123456;width:130px;height:30px;padding:2px 3px'>"

    assert {:ok, pdf} =
             HtmlToPdf.render(html,
               fonts: [%{family: "OriginalEditor", data: bytes}],
               system_font_discovery: false
             )

    assert {:ok, context} = Reader.read_validated(pdf)
    assert {:ok, %{fields: [field]}} = FormValidator.inspect_document(context)
    [widget] = field.widgets
    assert widget.dictionary["NEPUSourceEditor"]["Type"] == {:name, "NEPUSourceEditor"}
    assert widget.source_editor.fonts |> Enum.any?(&(&1.data == bytes))
    assert widget.source_editor.style.font_size == 10.5
    assert widget.source_editor.style.color == {18 / 255, 52 / 255, 86 / 255}

    refute Enum.any?(context.document.objects, fn {_ref, object} ->
             is_map(object.value) and
               Enum.any?(
                 ~w(NEPUEffectProgram NEPUEffectRegion NEPUSvgEditLeaf),
                 &Map.has_key?(object.value, &1)
               )
           end)

    assert {:ok, filled} = Forms.fill(pdf, %{"editor" => "ffiChanged"}, appearance: :source)
    artifact = "tmp/test_artifacts/form_appearance_converter/current-ordinary-physical"
    File.mkdir_p!(artifact)
    File.write!(Path.join(artifact, "initial.pdf"), pdf)
    File.write!(Path.join(artifact, "filled.pdf"), filled)
    assert {:ok, [%{value: "ffiChanged", widgets: widgets}]} = Forms.fields(filled)
    assert widgets == [%{page: widget.page, rect: widget.rect}]
    assert {:ok, flat} = Forms.flatten(filled)
    File.write!(Path.join(artifact, "flat.pdf"), flat)
    assert {:ok, text} = Text.extract(flat, layout: false)
    assert text =~ "ffiChanged"
    assert {:ok, filled_context} = Reader.read_validated(filled)
    assert {:ok, %{fields: [filled_field]}} = FormValidator.inspect_document(filled_context)
    assert hd(filled_field.widgets).source_editor.fonts |> Enum.any?(&(&1.data == bytes))
    {:ref, ref} = hd(filled_field.widgets).ap["N"]
    appearance = filled_context.document.objects[ref].value

    assert {:ok, resources} =
             NativeElixirPdfUtilities.Validators.PdfValidator.dictionary(
               filled_context.document,
               appearance["Resources"]
             )

    assert {:ok, fonts} =
             NativeElixirPdfUtilities.Validators.PdfValidator.dictionary(
               filled_context.document,
               resources["Font"]
             )

    assert Enum.any?(fonts, fn {_name, ref} ->
             assert {:ok, dictionary} =
                      NativeElixirPdfUtilities.Validators.PdfValidator.dictionary(
                        filled_context.document,
                        ref
                      )

             case dictionary["BaseFont"] do
               {:name, name} -> name =~ "OriginalEditor"
               _ -> false
             end
           end)
  end

  test "ordinary radio partial flatten leaves its peer scalar and other radio group editable" do
    html =
      "<input name='editor' value='Initial'><input type='radio' name='one' value='a' checked><input type='radio' name='one' value='b'><input type='radio' name='two' value='x' checked><input type='radio' name='two' value='y'>"

    assert {:ok, pdf} = HtmlToPdf.render(html, system_font_discovery: false)
    assert {:ok, toggled} = Forms.fill(pdf, %{"one" => "b"})
    assert {:ok, partial} = Forms.flatten(toggled, fields: ["one"])
    assert {:ok, changed} = Forms.fill(partial, %{"editor" => "Updated", "two" => "y"})
    assert {:ok, fields} = Forms.fields(changed)
    assert Map.new(fields, &{&1.name, &1.value}) == %{"editor" => "Updated", "two" => "y"}
    assert {:ok, flat} = Forms.flatten(changed)
    assert {:ok, []} = Forms.fields(flat)
    assert {:ok, text} = Text.extract(flat, layout: false)
    assert text =~ "Updated"
  end

  test "readonly fields and canonical source-template failure diagnostics remain intact" do
    assert {:ok, pdf} =
             HtmlToPdf.render("<input name='locked' readonly value='Original'>",
               system_font_discovery: false
             )

    assert {:error, {:read_only_form_field, diagnostic}} =
             Forms.fill(pdf, %{"locked" => "Changed"})

    assert diagnostic.stage == :forms and diagnostic.operation == :fill and
             diagnostic.source == "locked"

    assert {:ok, context} = Reader.read_validated(pdf)
    assert {:ok, %{fields: [field]}} = FormValidator.inspect_document(context)
    [widget] = field.widgets
    editor = widget.dictionary["NEPUSourceEditor"]
    {:ref, {id, gen}} = widget.ref

    assert {:ok, corrupted} =
             IncrementalWriter.write(context, [
               {id, gen,
                {:value,
                 Map.put(
                   widget.dictionary,
                   "NEPUSourceEditor",
                   Map.put(editor, "X", {:string, "invalid"})
                 )}}
             ])

    assert {:error, {_, rejected}} = Forms.fields(corrupted)
    assert rejected.operation == :fields
    assert is_binary(rejected.message) and rejected.message != ""
  end

  test "templates preserve password masking and choice export labels through ordinary forms" do
    html =
      "<input name='password' type='password' value='old'><select name='pick'><option value='a'>Alpha</option><option value='b'>Beta</option></select>"

    assert {:ok, pdf} = HtmlToPdf.render(html, system_font_discovery: false)

    assert {:ok, filled} =
             Forms.fill(pdf, %{"password" => "secret", "pick" => "b"}, appearance: :source)

    assert {:ok, values} = Forms.fields(filled)
    assert Map.new(values, &{&1.name, &1.value}) == %{"password" => "secret", "pick" => "b"}
    assert {:ok, flat} = Forms.flatten(filled)
    assert {:ok, text} = Text.extract(flat, layout: false)
    assert text =~ "Beta" and text =~ "••••••"
    refute text =~ "secret"
  end

  test "new characters outside selected embedded fonts return an actionable diagnostic" do
    assert {:ok, pdf} =
             HtmlToPdf.render("<input name='editor' value='ASCII'>", system_font_discovery: false)

    assert {:error, {_, diagnostic}} = Forms.fill(pdf, %{"editor" => <<0x10FFFF::utf8>>})
    assert diagnostic.source == "editor" and diagnostic.operation == :fill
    assert is_binary(diagnostic.message) and diagnostic.message != ""
  end

  test "ordinary template grammar denies malformed resource and projection data" do
    assert {:ok, pdf} =
             HtmlToPdf.render("<input name='editor' value='Initial'>",
               system_font_discovery: false
             )

    assert {:ok, context} = Reader.read_validated(pdf)
    assert {:ok, %{fields: [field]}} = FormValidator.inspect_document(context)
    template = hd(field.widgets).source_editor
    assert {:ok, _} = FormAppearanceValidator.validate(template)
    assert {:error, {_, diagnostic}} = FormAppearanceValidator.validate(%{template | fonts: []})
    assert is_binary(diagnostic.message)

    assert {:error, {_, diagnostic}} =
             FormAppearanceValidator.validate(%{
               template
               | projections: [
                   %{
                     matrix: {1, 0, 0, 1, 0, 0},
                     bounds: {0, 0, 10, 10},
                     polygons: [[{0, 0}, {1, 0}]]
                   }
                 ]
             })

    assert diagnostic.stage == :forms
    assert {:ok, nil} = FormAppearanceValidator.prepare(context.document, nil)
  end

  test "embedded appearance font resource denial remains a public diagnostic" do
    assert {:ok, pdf} =
             HtmlToPdf.render("<input name='editor' value='Initial'>",
               system_font_discovery: false
             )

    assert :ok = Limits.install(%{Limits.effective() | max_aggregate_font_source_bytes: 1})
    assert {:error, {:resource_limit_exceeded, diagnostic}} = Forms.fields(pdf)
    assert diagnostic.operation == :fields
    assert diagnostic.message =~ "max_aggregate_font_source_bytes"
  end

  test "legacy custom effect PDFs remain inspectable but are never executed" do
    pdf = File.read!("test/fixtures/pdf_forms/legacy-svg-radio-appearances.pdf")
    assert {:ok, [%{name: "r", type: :radio, value: "a", widgets: widgets}]} = Forms.fields(pdf)
    assert length(widgets) == 2

    for result <- [Forms.fill(pdf, %{"r" => "b"}), Forms.flatten(pdf)] do
      assert {:error, {:unsupported_form, diagnostic}} = result
      assert diagnostic.stage == :forms and diagnostic.source == "r"
      assert diagnostic.message =~ "custom SVG appearance programs"
      assert diagnostic.operation in [:fill, :flatten]
    end
  end

  test "shared source fonts keep one physical resource and admit exact field object capacity" do
    html =
      "<input type='radio' name='pick' value='a' checked><input type='radio' name='pick' value='b'><input name='editor' value='Initial'>"

    assert {:ok, pdf} = HtmlToPdf.render(html, system_font_discovery: false)
    assert {:ok, context} = Reader.read_validated(pdf)
    required = context.document.trailer["Size"]
    assert :ok = Limits.install(%{Limits.effective() | max_pdf_objects: required})
    assert {:ok, exact} = HtmlToPdf.render(html, system_font_discovery: false)
    assert {:ok, [%{name: "pick", widgets: [_, _]}, %{name: "editor"}]} = Forms.fields(exact)
    assert :ok = Limits.install(%{Limits.effective() | max_pdf_objects: required - 1})

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             HtmlToPdf.render(html, system_font_discovery: false)

    assert diagnostic.stage == :forms
    assert diagnostic.message =~ "object count cannot accommodate form appearances"
  end
end
