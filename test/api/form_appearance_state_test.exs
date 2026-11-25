defmodule NativeElixirPdfUtilities.FormAppearanceStateTest do
  use ExUnit.Case, async: true

  alias NativeElixirPdfUtilities.{Forms, HtmlToPdf, Text}
  alias NativeElixirPdfUtilities.Pdf.{IncrementalWriter, Reader}
  alias NativeElixirPdfUtilities.Validators.FormValidator

  test "standalone interactive writing carries reader work back to its caller" do
    alias NativeElixirPdfUtilities.HtmlToPdf.{
      FontFallback,
      HtmlParser,
      Layout,
      Pagination,
      PdfWriter,
      Style
    }

    alias NativeElixirPdfUtilities.Validators.ResourceBudgetValidator, as: Budget
    assert {:ok, dom} = HtmlParser.parse_document_detailed("<input name=customer value=Ada>")
    assert {:ok, styled} = Style.compute_detailed(dom)
    assert {:ok, styled} = FontFallback.resolve(styled, :replace)
    assert {:ok, layout} = Layout.layout(styled)
    assert {:ok, pages} = Pagination.paginate(layout)
    write = fn -> PdfWriter.render(pages) end

    measure = fn run ->
      Budget.with_budget(fn ->
        assert {:ok, pdf} = run.()
        {pdf, Budget.snapshot()}
      end)
    end

    {pdf, standalone} = measure.(write)

    {_, inside_worker} =
      measure.(fn ->
        NativeElixirPdfUtilities.Operation.run_with_shared_budget(PdfWriter, :write_pdf, write)
      end)

    assert Map.take(standalone, [:max_work, :max_entries]) ==
             Map.take(inside_worker, [:max_work, :max_entries])

    assert {:ok, [%{value: "Ada"}]} = Forms.fields(pdf)
  end

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

  test "flattening preserves indirect layer identity and diagnoses unresolved or invalid membership" do
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
    assert {:ok, flat} = Forms.flatten(source)
    assert {:ok, []} = Forms.fields(flat)
    assert {:ok, written} = Reader.read_validated(flat)
    assert written.catalog["OCProperties"] == layers
    assert hd(written.pages).resources["Properties"]["NEPUForm1"] == member

    for invalid <- [7, %{"Type" => {:name, "OCG"}}, %{"Type" => {:name, "Unknown"}}] do
      assert {:ok, input} =
               IncrementalWriter.write(context, [
                 {id, generation, {:value, Map.put(widget.dictionary, "OC", invalid)}}
               ])

      assert {:error, {:unsupported_form, %{operation: :flatten}}} = Forms.flatten(input)
    end

    assert {:ok, _} = Forms.flatten(pdf)
  end

  @tag :tmp_dir
  test "selected and filled widgets keep layer policies, page properties and other editable fields",
       %{tmp_dir: dir} do
    assert {:ok, pdf} =
             HtmlToPdf.render(
               "<input name=price value=100><p>Footer</p><input name=notes value=Keep>",
               page_size: {220, 160},
               margin: 5
             )

    assert {:ok, ctx} = Reader.read_validated(pdf)
    assert {:ok, form} = FormValidator.inspect_document(ctx)
    field = Enum.find(form.fields, &(&1.name == "price"))
    [widget] = field.widgets
    {:ref, {id, gen}} = widget.ref
    [page] = ctx.pages
    {page_id, page_gen} = page.ref
    {cat_id, cat_gen} = ctx.catalog_ref
    next = ctx.document.trailer["Size"]
    group = {:ref, {next, 0}}
    policy = %{"Type" => {:name, "OCMD"}, "OCGs" => [group], "P" => {:name, "AnyOff"}}
    properties = %{"NEPUForm1" => group}
    {:ok, resources} = Reader.resolve(ctx.document, page.resources)

    page_dict =
      Map.put(
        page.dictionary,
        "Resources",
        Map.put(resources, "Properties", {:ref, {next + 2, 0}})
      )

    raster = fn bytes ->
      NativeElixirPdfUtilities.TestSupport.PdfVisualCompare.rasterize_pdf!(bytes,
        artifact_dir: Path.join(dir, "layer")
      )
    end

    for membership <- [group, policy, {:ref, {next + 1, 0}}] do
      images =
        for initial <- ["ON", "OFF"] do
          layers = %{
            "OCGs" => [group],
            "D" => %{"BaseState" => {:name, initial}, "Order" => [group]}
          }

          assert {:ok, source} =
                   IncrementalWriter.write(ctx, [
                     {next, 0,
                      {:value, %{"Type" => {:name, "OCG"}, "Name" => {:string, "Prices"}}}},
                     {next + 1, 0, {:value, policy}},
                     {next + 2, 0, {:value, properties}},
                     {id, gen, {:value, Map.put(widget.dictionary, "OC", membership)}},
                     {page_id, page_gen, {:value, page_dict}},
                     {cat_id, cat_gen, {:value, Map.put(ctx.catalog, "OCProperties", layers)}}
                   ])

          assert {:ok, filled} = Forms.fill(source, %{"price" => "250"})
          assert {:ok, partial} = Forms.flatten(source, fields: ["price"])
          assert {:ok, updated} = Forms.fill(source, %{"price" => "250"}, flatten: true)

          for {original, output} <- [{source, partial}, {filled, updated}] do
            assert {:ok, [%{name: "notes", value: "Keep"}]} = Forms.fields(output)
            assert {:ok, _} = Forms.fill(output, %{"notes" => "Still editable"})
            assert {:ok, result} = Reader.read_validated(output)
            assert result.catalog["OCProperties"] == layers

            assert hd(result.pages).resources["Properties"] ==
                     Map.put(properties, "NEPUForm2", membership)

            assert raster.(output) == raster.(original)
          end

          raster.(updated)
        end

      assert [on, off] = images
      refute on == off
    end
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
