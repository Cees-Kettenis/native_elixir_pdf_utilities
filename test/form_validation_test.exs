defmodule NativeElixirPdfUtilities.FormValidationTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.{Forms, Limits, HtmlToPdf}
  alias NativeElixirPdfUtilities.HtmlToPdf.PdfWriter
  alias NativeElixirPdfUtilities.Pdf.{Reader, IncrementalWriter}
  alias NativeElixirPdfUtilities.Validators.{FormValidator, HtmlFormValidator}

  setup do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    :ok
  end

  test "malformed form trees and unresolved entries produce diagnostics" do
    for {field, widget, form} <- [
          {%{}, %{}, %{"Fields" => 1}},
          {%{}, %{}, %{"Fields" => [{:ref, {6, 0}}, {:ref, {6, 0}}]}},
          {%{"Kids" => 1}, %{}, %{}},
          {%{"Kids" => [{:ref, {999, 0}}]}, %{}, %{}},
          {%{"Kids" => [{:ref, {7, 0}}, {:ref, {7, 0}}]}, %{}, %{}},
          {%{"T" => {:string, ""}}, %{}, %{}},
          {%{"Ff" => -1}, %{}, %{}},
          {%{"T" => 123}, %{}, %{}},
          {%{"T" => {:hex, <<254, 255, 1>>}}, %{}, %{}},
          {%{"Opt" => 42}, %{}, %{}},
          {%{"Opt" => {:ref, {999, 0}}}, %{}, %{}},
          {%{"Opt" => [1]}, %{}, %{}},
          {%{"V" => [1]}, %{}, %{}},
          {%{}, %{"Rect" => [0, 0, 0, 0]}, %{}},
          {%{}, %{"Rect" => 1}, %{}},
          {%{}, %{"AP" => {:ref, {999, 0}}}, %{}}
        ] do
      assert {:error, {_, %{module: Forms, operation: :fields, message: message}}} =
               Forms.fields(external_pdf(field, widget, form))

      assert is_binary(message)
    end

    pdf = external_pdf()

    for annots <- [1, {:ref, {999, 0}}, []] do
      pdf = patch_page(pdf, %{"Annots" => annots})
      assert {:error, _} = Forms.fields(pdf)
    end

    {:ok, context} = Reader.read_validated(pdf)
    bad = %{context | catalog: Map.put(context.catalog, "AcroForm", {:ref, {999, 0}})}
    assert {:error, _} = FormValidator.inspect_document(bad)
  end

  test "resolves indirect text, button states and choice array values" do
    for {field, value, expected} <- [
          {%{}, {:string, "Indirect text"}, "Indirect text"},
          {%{}, {:hex, <<254, 255, 0, 65>>}, "A"},
          {%{"FT" => {:name, "Btn"}}, {:name, "Off"}, false},
          {%{"FT" => {:name, "Btn"}}, {:name, "Yes"}, true},
          {%{"FT" => {:name, "Btn"}, "Ff" => 32_768}, {:name, "Yes"}, "Yes"},
          {%{"FT" => {:name, "Ch"}}, [{:ref, {11, 0}}], ["Choice"]}
        ] do
      pdf = external_pdf(Map.put(field, "V", {:ref, {9, 0}}))
      {:ok, context} = Reader.read_validated(pdf)

      {:ok, pdf} =
        IncrementalWriter.write(context, [
          {9, 0, {:value, {:ref, {10, 0}}}},
          {10, 0, {:value, value}},
          {11, 0, {:value, {:string, "Choice"}}}
        ])

      assert {:ok, [%{value: ^expected}]} = Forms.fields(pdf)
    end
  end

  test "inspects indirect dictionaries for unsupported fields without allowing modification" do
    value = %{"CustomValue" => {:string, "retained"}}
    pdf = external_pdf(%{"FT" => {:name, "Custom"}, "V" => {:ref, {9, 0}}})
    {:ok, context} = Reader.read_validated(pdf)
    {:ok, pdf} = IncrementalWriter.write(context, [{9, 0, {:value, value}}])

    assert {:ok, [%{type: :unsupported, value: ^value}]} = Forms.fields(pdf)
    assert {:error, {:unsupported_form, _}} = Forms.fill(pdf, %{"field" => "changed"})
  end

  test "missing and cyclic field values return resolution diagnostics" do
    for {objects, fragment} <- [
          {[], "missing"},
          {[{9, 0, {:value, {:ref, {10, 0}}}}, {10, 0, {:value, {:ref, {9, 0}}}}], "cycle"}
        ],
        field <- [
          %{"V" => {:ref, {9, 0}}},
          %{"FT" => {:name, "Btn"}, "V" => {:ref, {9, 0}}},
          %{"FT" => {:name, "Ch"}, "V" => [{:ref, {9, 0}}]}
        ] do
      {:ok, context} = Reader.read_validated(external_pdf(field))
      {:ok, pdf} = IncrementalWriter.write(context, objects)

      assert {:error,
              {:invalid_pdf_input,
               %{
                 stage: :resolution,
                 reason: :invalid_pdf_input,
                 module: Forms,
                 operation: :fields,
                 message: message
               }}} = Forms.fields(pdf)

      assert message =~ fragment
    end
  end

  test "rejects resolved values that do not match the field type" do
    for {type, value} <- [
          {"Tx", {:name, "Off"}},
          {"Btn", {:string, "Off"}},
          {"Ch", %{}},
          {"Ch", [1]}
        ] do
      pdf = external_pdf(%{"FT" => {:name, type}, "V" => {:ref, {9, 0}}})
      {:ok, context} = Reader.read_validated(pdf)
      {:ok, pdf} = IncrementalWriter.write(context, [{9, 0, {:value, value}}])

      assert {:error,
              {:invalid_form,
               %{stage: :forms, reason: :invalid_form, operation: :fields, message: message}}} =
               Forms.fields(pdf)

      assert is_binary(message) and message != ""
    end
  end

  test "supports combined field widgets and partial nested flattening" do
    pdf =
      external_pdf(%{
        "Subtype" => {:name, "Widget"},
        "Kids" => [],
        "Rect" => [10, 10, 110, 30],
        "AP" => %{"N" => {:ref, {8, 0}}}
      })

    pdf = patch_page(pdf, %{"Annots" => [{:ref, {6, 0}}]})
    assert {:ok, [%{name: "field"}]} = Forms.fields(pdf)
    assert {:ok, updated} = Forms.fill(pdf, %{"field" => "Combined"})
    assert {:ok, [%{value: "Combined"}]} = Forms.fields(updated)
    {:ok, source} = HtmlToPdf.render("<div><input name=\"one\"><input name=\"two\"></div>")
    {:ok, context} = Reader.read_validated(source)
    {:ok, form} = Reader.dictionary(context.document, context.catalog["AcroForm"])
    parent = context.document.trailer["Size"]
    {id, gen} = context.catalog_ref
    catalog = Map.put(context.catalog, "AcroForm", Map.put(form, "Fields", [{:ref, {parent, 0}}]))

    {:ok, nested} =
      IncrementalWriter.write(context, [
        {parent, 0, {:value, %{"T" => {:string, "group"}, "Kids" => form["Fields"]}}},
        {id, gen, {:value, catalog}}
      ])

    assert {:ok, flat} = Forms.flatten(nested, fields: ["group.one"])
    assert {:ok, [%{name: "group.two"}]} = Forms.fields(flat)
  end

  test "rejects unsupported field semantics and signature changes" do
    for field <- [
          %{"FT" => {:name, "Btn"}, "Ff" => 65536},
          %{"FT" => {:name, "Sig"}, "V" => nil},
          %{"FT" => {:name, "Unknown"}},
          %{"Ff" => 8192},
          %{"Ff" => 16_777_216},
          %{"Ff" => 33_554_432},
          %{"FT" => {:name, "Ch"}, "Ff" => 262_144},
          %{"Kids" => []},
          %{"AA" => %{}}
        ] do
      assert {:error, {:unsupported_form, %{operation: :fill}}} =
               Forms.fill(external_pdf(field), %{"field" => "x"})
    end

    for widget <- [%{"AA" => %{}}, %{"MK" => %{"R" => 90}}] do
      assert {:error, {:unsupported_form, _}} =
               Forms.fill(external_pdf(%{}, widget), %{"field" => "x"})
    end

    for value <- [%{"Type" => {:name, "Sig"}}, {:ref, {8, 0}}] do
      pdf = external_pdf(%{"FT" => {:name, "Sig"}, "V" => value})
      assert {:ok, [%{type: :signature, value: ^value}]} = Forms.fields(pdf)
      assert {:error, {:unsupported_form, _}} = Forms.fill(pdf, %{})
    end
  end

  test "validates values, max length, choice selection and glyph support" do
    assert {:error, {:invalid_form, _}} =
             Forms.fill(external_pdf(%{"MaxLen" => -1}), %{"field" => "x"})

    assert {:error, {:invalid_form_value, _}} =
             Forms.fill(external_pdf(%{"MaxLen" => 1}), %{"field" => "xx"})

    assert {:ok, _} = Forms.fill(external_pdf(%{"MaxLen" => 1}), %{"field" => "x"})
    assert {:error, {:invalid_form_value, _}} = Forms.fill(external_pdf(), %{"field" => false})
    assert {:error, {:unsupported_form, _}} = Forms.fill(external_pdf(), %{"field" => <<0>>})

    assert {:error, {:unsupported_form, _}} =
             Forms.fill(external_pdf(%{}, %{"Rect" => [0, 0, 1, 1]}), %{"field" => "x"})

    field = %{"FT" => {:name, "Ch"}, "Opt" => [{:string, "a"}, {:string, "b"}], "V" => nil}
    assert {:ok, _} = Forms.fill(external_pdf(field), %{"field" => nil})

    assert {:error, {:invalid_form_value, _}} =
             Forms.fill(external_pdf(field), %{"field" => ["a"]})

    assert {:error, {:invalid_form_value, _}} = Forms.fill(external_pdf(field), %{"field" => "c"})
    multi = external_pdf(Map.merge(field, %{"Ff" => 2_097_152, "V" => [{:string, "a"}]}))
    assert {:ok, [%{value: ["a"]}]} = Forms.fields(multi)
    assert {:ok, multi} = Forms.fill(multi, %{"field" => ["a", "b"]})
    assert {:ok, [%{value: ["a", "b"]}]} = Forms.fields(multi)
    assert {:ok, _} = Forms.fill(multi, %{"field" => nil})
    {:ok, radio} = HtmlToPdf.render("<div><input type=\"radio\" name=\"a\" value=\"x\"></div>")
    assert {:error, {:invalid_form_value, _}} = Forms.fill(radio, %{"a" => "bad"})
    assert {:ok, _} = Forms.flatten(radio)
  end

  test "flatten removes hidden widgets without requiring an appearance" do
    for flags <- [2, 32, 34, 36] do
      pdf = external_pdf(%{}, %{"F" => flags, "AP" => %{}})
      assert {:ok, flat} = Forms.flatten(pdf)
      assert {:ok, []} = Forms.fields(flat)
      assert {:ok, context} = Reader.read_validated(flat)
      assert hd(context.pages).dictionary["Annots"] == []
      refute Map.has_key?(context.catalog, "AcroForm")
    end
  end

  test "invalid annotation flags return actionable diagnostics" do
    for flags <- [-1, 1.5, {:name, "Hidden"}, nil] do
      assert {:error,
              {:invalid_form,
               %{stage: :forms, operation: :flatten, module: Forms, message: message}}} =
               Forms.flatten(external_pdf(%{}, %{"F" => flags}))

      assert message =~ "non-negative integer F flags"
    end
  end

  test "flatten validates appearances and handles missing AS as Off" do
    assert {:error, _} = Forms.flatten(external_pdf(%{}, %{"AP" => %{}}))
    pdf = external_pdf()
    {:ok, context} = Reader.read_validated(pdf)

    {:ok, bad} =
      IncrementalWriter.write(context, [
        {8, 0,
         {:stream,
          %{"Type" => {:name, "XObject"}, "Subtype" => {:name, "Form"}, "BBox" => [0, 0, 0, 0]},
          ""}}
      ])

    assert {:error, {:unsupported_form, _}} = Forms.flatten(bad)

    pdf =
      external_pdf(%{"FT" => {:name, "Btn"}, "V" => {:name, "Off"}}, %{
        "AP" => %{"N" => %{"Off" => {:ref, {8, 0}}, "Yes" => {:ref, {8, 0}}}}
      })

    assert {:ok, _} = Forms.flatten(pdf)
    {:ok, context} = Reader.read_validated(pdf)
    {id, gen} = hd(context.pages).ref

    {:ok, rotated} =
      IncrementalWriter.write(context, [
        {id, gen, {:value, Map.put(hd(context.pages).dictionary, "Rotate", 90)}}
      ])

    assert {:ok, _} = Forms.flatten(rotated)
  end

  test "form limits and HTML option diagnostics" do
    pdf = external_pdf()
    Limits.install(Map.put(Limits.effective(), :max_pdf_objects, 20))
    assert {:error, {:resource_limit_exceeded, _}} = Forms.fill(pdf, %{"field" => "x"})
    Limits.install(Map.put(Limits.defaults(), :max_pdf_form_fields, 1))
    assert {:error, _} = Forms.fields(pdf)
    Limits.install(Map.put(Limits.defaults(), :max_pdf_form_text_bytes, 1))

    assert {:error, {:resource_limit_exceeded, _}} =
             HtmlToPdf.render("<div><input name=\"long\"></div>")

    assert {:error, {:invalid_options, _}} = HtmlFormValidator.prepare([], forms: :bad)

    :sys.replace_state(NativeElixirPdfUtilities.HtmlToPdf.FontCache, fn state ->
      :ets.delete_all_objects(state.table)
      state
    end)

    Limits.install(Map.put(Limits.defaults(), :max_font_cmap_work, 1))
    assert {:error, _} = Forms.fill(pdf, %{"field" => "x"})
  end

  test "button updates preserve artwork and reject unusable states" do
    {:ok, generated} = HtmlToPdf.render("<div><input type='checkbox' name='agree'></div>")
    {:ok, context} = Reader.read_validated(generated)
    {:ok, form} = FormValidator.inspect_document(context)
    original = hd(hd(form.fields).widgets).ap
    assert {:ok, filled} = Forms.fill(generated, %{"agree" => true})
    {:ok, context} = Reader.read_validated(filled)
    {:ok, form} = FormValidator.inspect_document(context)
    assert hd(hd(form.fields).widgets).ap == original
    assert {:ok, unchecked} = Forms.fill(filled, %{"agree" => false})
    assert {:ok, [%{value: false}]} = Forms.fields(unchecked)

    for normal <- [%{}, %{"Yes" => {:ref, {8, 0}}}] do
      assert {:error, {:unsupported_form, %{source: "field", operation: :fill}}} =
               Forms.fill(external_pdf(%{"FT" => {:name, "Btn"}}, %{"AP" => %{"N" => normal}}), %{
                 "field" => true
               })
    end

    pdf =
      external_pdf(%{"FT" => {:name, "Btn"}}, %{
        "AP" => %{"N" => %{"Off" => {:ref, {8, 0}}, "Yes" => {:ref, {999, 0}}}}
      })

    assert {:error, {_, %{source: "field", operation: :fill}}} =
             Forms.fill(pdf, %{"field" => true})

    pdf =
      external_pdf(
        %{"FT" => {:name, "Btn"}, "Kids" => [{:ref, {7, 0}}, {:ref, {9, 0}}]},
        %{"AP" => %{"N" => %{"Off" => {:ref, {8, 0}}, "Yes" => {:ref, {8, 0}}}}}
      )

    {:ok, context} = Reader.read_validated(pdf)
    {:ok, widget} = Reader.dictionary(context.document, {:ref, {7, 0}})

    {:ok, pdf} =
      IncrementalWriter.write(context, [
        {9, 0,
         {:value,
          Map.put(widget, "AP", %{"N" => %{"Off" => {:ref, {8, 0}}, "Other" => {:ref, {8, 0}}}})}}
      ])

    pdf = patch_page(pdf, %{"Annots" => [{:ref, {7, 0}}, {:ref, {9, 0}}]})

    assert {:error, {:unsupported_form, %{message: message}}} =
             Forms.fill(pdf, %{"field" => true})

    assert message =~ "same export state"
  end

  test "resolves indirect Kids when pruning and bounds tree depth" do
    pdf = external_pdf()
    {:ok, context} = Reader.read_validated(pdf)
    {:ok, field} = Reader.dictionary(context.document, {:ref, {6, 0}})

    {:ok, pdf} =
      IncrementalWriter.write(context, [
        {5, 0, {:value, %{"Fields" => [{:ref, {10, 0}}]}}},
        {6, 0, {:value, Map.put(field, "Kids", {:ref, {9, 0}})}},
        {9, 0, {:value, [{:ref, {7, 0}}]}},
        {10, 0, {:value, %{"T" => {:string, "parent"}, "Kids" => {:ref, {11, 0}}}}},
        {11, 0, {:value, [{:ref, {6, 0}}]}}
      ])

    assert {:ok, flattened} = Forms.flatten(pdf)
    assert {:ok, []} = Forms.fields(flattened)
    {:ok, context} = Reader.read_validated(pdf)

    {:ok, deep} =
      IncrementalWriter.write(context, [
        {5, 0, {:value, %{"Fields" => [{:ref, {12, 0}}]}}},
        {12, 0, {:value, %{"T" => {:string, "root"}, "Kids" => [{:ref, {10, 0}}]}}}
      ])

    Limits.install(Map.put(Limits.defaults(), :max_pdf_form_depth, 1))
    assert {:error, {:resource_limit_exceeded, %{message: message}}} = Forms.fields(deep)
    assert message =~ "max_pdf_form_depth"
  end

  test "validates field alignment and applies left, center and right placement" do
    for alignment <- [0, 1, 2] do
      pdf = external_pdf(%{"Q" => alignment})
      assert {:ok, filled} = Forms.fill(pdf, %{"field" => "X"})
      assert {:ok, [%{value: "X"}]} = Forms.fields(filled)
    end

    assert {:error, {:invalid_form, %{message: message}}} =
             Forms.fields(external_pdf(%{"Q" => 3}))

    assert message =~ "alignment"
    pdf = external_pdf()
    Limits.install(Map.put(Limits.defaults(), :max_pdf_form_text_bytes, 2))
    assert {:error, {:resource_limit_exceeded, _}} = Forms.fields(pdf)
  end

  test "HTML fields reject reserved radio states, duplicate choices and repeated furniture" do
    for html <- [
          "<input type='radio' value='Off'>",
          "<input type='radio' value=''>",
          "<select><option value='a'>A</option><option value='a'>B</option></select>"
        ] do
      assert {:error, {:invalid_form, %{stage: :forms}}} = HtmlToPdf.render("<div>#{html}</div>")
    end

    control = %{id: make_ref(), order: [0], tag: "input", attributes: %{}, children: []}
    page = %{size: {200, 200}, boxes: [%{form_control: control}]}

    assert {:error, {:unsupported_form, %{message: message}}} =
             HtmlFormValidator.prepare([page, page], [])

    assert message =~ "repeat"

    assert {:ok, pdf} =
             HtmlToPdf.render("<p>No control</p><div style='break-before:page'><input></div>")

    assert {:ok, [%{name: "TEXT_2_1"}]} = Forms.fields(pdf)
  end

  test "fills repeated widgets through inherited fields and preserves unrelated annotations" do
    pdf = external_pdf()
    {:ok, context} = Reader.read_validated(pdf)
    {:ok, field} = Reader.dictionary(context.document, {:ref, {6, 0}})
    {:ok, widget} = Reader.dictionary(context.document, {:ref, {7, 0}})

    field =
      field
      |> Map.delete("FT")
      |> Map.delete("V")
      |> Map.put("Kids", [{:ref, {7, 0}}, {:ref, {9, 0}}])

    {:ok, pdf} =
      IncrementalWriter.write(context, [
        {5, 0, {:value, %{"Fields" => [{:ref, {11, 0}}]}}},
        {6, 0, {:value, field}},
        {9, 0, {:value, Map.put(widget, "Rect", [10, 50, 110, 70])}},
        {10, 0,
         {:value,
          %{
            "Type" => {:name, "Annot"},
            "Subtype" => {:name, "Text"},
            "Rect" => [150, 10, 170, 30],
            "Contents" => {:string, "Keep me"}
          }}},
        {11, 0,
         {:value,
          %{
            "T" => {:string, "group"},
            "FT" => {:name, "Tx"},
            "V" => {:ref, {12, 0}},
            "Q" => 1,
            "Kids" => [{:ref, {6, 0}}]
          }}},
        {12, 0, {:value, {:string, "Inherited"}}}
      ])

    pdf = patch_page(pdf, %{"Annots" => [{:ref, {7, 0}}, {:ref, {9, 0}}, {:ref, {10, 0}}]})

    assert {:ok, [%{name: "group.field", value: "Inherited", widgets: [_, _]}]} =
             Forms.fields(pdf)

    assert {:ok, filled} = Forms.fill(pdf, %{"group.field" => "Repeated"})
    {:ok, context} = Reader.read_validated(filled)
    {:ok, form} = FormValidator.inspect_document(context)
    [first, second] = hd(form.fields).widgets
    refute first.ap["N"] == second.ap["N"]
    assert {:ok, flat} = Forms.flatten(filled)
    assert {:ok, []} = Forms.fields(flat)
    assert {:ok, text} = NativeElixirPdfUtilities.Text.extract(flat)
    assert length(Regex.scan(~r/Repeated/, text)) == 2
    {:ok, context} = Reader.read_validated(flat)
    assert hd(context.pages).dictionary["Annots"] == [{:ref, {10, 0}}]
  end

  test "flattened appearance matrices keep small nonzero scales in valid PDF syntax" do
    {:ok, context} = Reader.read_validated(external_pdf())

    for factor <- [1, 100_000_000] do
      {:ok, pdf} =
        IncrementalWriter.write(context, [
          {8, 0,
           {:stream,
            %{
              "Type" => {:name, "XObject"},
              "Subtype" => {:name, "Form"},
              "BBox" => [0, 0, 100_000_000, 100_000_000],
              "Matrix" => [factor, 0, 0, factor, 0, 0],
              "Resources" => %{}
            }, "0 0 100000000 100000000 re f"}}
        ])

      assert {:ok, flat} = Forms.flatten(pdf)
      assert {:ok, flattened} = Reader.read_validated(flat)

      assert {:ok, contents} =
               NativeElixirPdfUtilities.Validators.PdfValidator.content_references(
                 flattened.document,
                 hd(flattened.pages).dictionary
               )

      assert {:ok, invocation} = Reader.decoded_stream(flattened.document, List.last(contents))

      assert [
               {:op, "Q"},
               {:op, "q"},
               {:real, sx},
               {:int, 0},
               {:int, 0},
               {:real, sy},
               {:real, 10.0},
               {:real, 10.0},
               {:op, "cm"},
               {:name, _},
               {:op, "Do"},
               {:op, "Q"}
             ] =
               invocation
               |> NativeElixirPdfUtilities.Tokenizer.new()
               |> NativeElixirPdfUtilities.Tokenizer.tokenize_all()

      assert sx == 100 / (100_000_000 * factor)
      assert sy == 20 / (100_000_000 * factor)
    end
  end

  defp external_pdf(field \\ %{}, widget \\ %{}, form \\ %{}) do
    {:ok, pdf} = PdfWriter.render([%{size: {200.0, 200.0}, boxes: []}])
    {:ok, context} = Reader.read_validated(pdf)
    page = hd(context.pages)
    {page_id, page_gen} = page.ref
    {catalog_id, catalog_gen} = context.catalog_ref

    objects = [
      {5, 0, {:value, Map.merge(%{"Fields" => [{:ref, {6, 0}}]}, form)}},
      {6, 0,
       {:value,
        Map.merge(
          %{
            "FT" => {:name, "Tx"},
            "T" => {:string, "field"},
            "V" => if(field["FT"] == {:name, "Btn"}, do: {:name, "Off"}, else: {:string, "Old"}),
            "Kids" => [{:ref, {7, 0}}]
          },
          field
        )}},
      {7, 0,
       {:value,
        Map.merge(
          %{
            "Type" => {:name, "Annot"},
            "Subtype" => {:name, "Widget"},
            "Parent" => {:ref, {6, 0}},
            "Rect" => [10, 10, 110, 30],
            "AP" => %{"N" => {:ref, {8, 0}}}
          },
          widget
        )}},
      {8, 0,
       {:stream,
        %{
          "Type" => {:name, "XObject"},
          "Subtype" => {:name, "Form"},
          "BBox" => [0, 0, 100, 20],
          "Resources" => %{}
        }, ""}},
      {page_id, page_gen, {:value, Map.put(page.dictionary, "Annots", [{:ref, {7, 0}}])}},
      {catalog_id, catalog_gen, {:value, Map.put(context.catalog, "AcroForm", {:ref, {5, 0}})}}
    ]

    {:ok, pdf} = IncrementalWriter.write(context, objects)
    pdf
  end

  defp patch_page(pdf, patch) do
    {:ok, context} = Reader.read_validated(pdf)
    page = hd(context.pages)
    {id, gen} = page.ref

    {:ok, pdf} =
      IncrementalWriter.write(context, [{id, gen, {:value, Map.merge(page.dictionary, patch)}}])

    pdf
  end
end
