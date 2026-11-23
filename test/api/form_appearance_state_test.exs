defmodule NativeElixirPdfUtilities.FormAppearanceStateTest do
  use ExUnit.Case, async: true

  alias NativeElixirPdfUtilities.{Forms, HtmlToPdf, Text}
  alias NativeElixirPdfUtilities.Pdf.{IncrementalWriter, Reader}
  alias NativeElixirPdfUtilities.Validators.FormValidator

  test "flatten preserves direct and referenced checkbox artwork" do
    for {checked, value, artwork} <- [{"checked", true, "☒"}, {"", false, "☐"}] do
      assert {:ok, pdf} =
               HtmlToPdf.render("<input type=checkbox name=agree #{checked}>",
                 system_font_discovery: false
               )

      assert {:ok, context} = Reader.read_validated(pdf)
      assert {:ok, %{fields: [%{widgets: [widget]}]}} = FormValidator.inspect_document(context)
      {:ref, {id, generation}} = widget.ref
      next = context.document.trailer["Size"]

      for depth <- 0..2 do
        {state, references} =
          case depth do
            0 ->
              {widget.dictionary["AS"], []}

            1 ->
              {{:ref, {next, 0}}, [{next, 0, {:value, widget.dictionary["AS"]}}]}

            2 ->
              {{:ref, {next, 0}},
               [
                 {next, 0, {:value, {:ref, {next + 1, 0}}}},
                 {next + 1, 0, {:value, widget.dictionary["AS"]}}
               ]}
          end

        assert {:ok, input} =
                 IncrementalWriter.write(context, [
                   {id, generation, {:value, Map.put(widget.dictionary, "AS", state)}}
                   | references
                 ])

        assert {:ok, [%{value: ^value}]} = Forms.fields(input)
        assert {:ok, flattened} = Forms.flatten(input)
        assert {:ok, []} = Forms.fields(flattened)
        assert {:ok, ^artwork} = Text.extract(flattened, layout: false)
      end
    end
  end

  test "flattening a layer-controlled widget preserves the original form through a diagnostic" do
    assert {:ok, pdf} = HtmlToPdf.render("<input name=customer value=Ada>")
    assert {:ok, context} = Reader.read_validated(pdf)
    assert {:ok, %{fields: [%{widgets: [widget]}]}} = FormValidator.inspect_document(context)
    {:ref, {id, generation}} = widget.ref
    group = context.document.trailer["Size"]
    member = {:ref, {group, 0}}
    {catalog_id, catalog_gen} = context.catalog_ref
    layers = %{"OCGs" => [member], "D" => %{"OFF" => [member]}}

    assert {:ok, source} =
             IncrementalWriter.write(context, [
               {group, 0, {:value, %{"Type" => {:name, "OCG"}, "Name" => {:string, "Details"}}}},
               {catalog_id, catalog_gen,
                {:value, Map.put(context.catalog, "OCProperties", layers)}},
               {id, generation, {:value, Map.put(widget.dictionary, "OC", member)}}
             ])

    for {layer_value, expected} <- [{nil, :ok}, {member, :invalid_pdf_input}] do
      patch = [{id, generation, {:value, Map.put(widget.dictionary, "OC", member)}}]
      entries = if layer_value == nil, do: [{group, 0, {:value, nil}} | patch], else: patch
      assert {:ok, modified} = IncrementalWriter.write(context, entries)

      case expected do
        :ok ->
          assert {:ok, flat} = Forms.flatten(modified)
          assert {:ok, []} = Forms.fields(flat)

        reason ->
          assert {:error, {^reason, %{stage: :resolution}}} = Forms.flatten(modified)
      end
    end

    assert {:ok, [%{value: "Ada"}]} = Forms.fields(source)
    assert {:error, {:unsupported_form, %{operation: :flatten}}} = Forms.flatten(source)
    assert {:ok, _} = Forms.flatten(pdf)
  end

  test "missing cyclic and incorrectly typed appearance states return actionable diagnostics" do
    assert {:ok, pdf} =
             HtmlToPdf.render("<input type=checkbox name=agree checked>",
               system_font_discovery: false
             )

    assert {:ok, context} = Reader.read_validated(pdf)
    assert {:ok, %{fields: [%{widgets: [widget]}]}} = FormValidator.inspect_document(context)
    {:ref, {id, generation}} = widget.ref
    next = context.document.trailer["Size"]

    for {state, references, reason, stage, fragment} <- [
          {{:ref, {next, 0}}, [], :invalid_pdf_input, :resolution, "missing"},
          {{:ref, {next, 0}}, [{next, 0, {:value, {:ref, {next, 0}}}}], :invalid_pdf_input,
           :resolution, "cycle"},
          {123, [], :invalid_form, :forms, "AS"},
          {{:ref, {next, 0}}, [{next, 0, {:value, {:string, "Yes"}}}], :invalid_form, :forms,
           "AS"}
        ] do
      assert {:ok, input} =
               IncrementalWriter.write(context, [
                 {id, generation, {:value, Map.put(widget.dictionary, "AS", state)}}
                 | references
               ])

      for {operation, result} <- [
            {:fields, Forms.fields(input)},
            {:fill, Forms.fill(input, %{"agree" => true})},
            {:flatten, Forms.flatten(input)}
          ] do
        assert {:error,
                {^reason,
                 %{
                   reason: ^reason,
                   stage: ^stage,
                   module: Forms,
                   operation: ^operation,
                   message: message
                 }}} = result

        assert message =~ fragment
      end
    end
  end
end
