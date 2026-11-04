defmodule NativeElixirPdfUtilities.IndirectStructuralTypesTest do
  use ExUnit.Case, async: false

  alias NativeElixirPdfUtilities.{Info, Limits, Merge, Split, Stamp, Text, Transform}
  alias NativeElixirPdfUtilities.Pdf.{IncrementalWriter, Reader}
  alias NativeElixirPdfUtilities.TestSupport.Paths

  setup do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    pdf = File.read!(Paths.fixture("pdf_transforms/nested-multi-page-report.pdf"))
    assert {:ok, context} = Reader.read_validated(pdf)
    %{pdf: pdf, context: context, limits: limits}
  end

  test "indirect Catalog Pages and Page names retain public read and rebuild behavior", %{
    pdf: direct,
    context: context
  } do
    # ISO 32000-1 section 7.3.10 and Tables 28-30 permit indirect Type names.
    for types <- [["Catalog"], ["Pages"], ["Page"], ["Catalog", "Pages", "Page"]] do
      indirect = indirect_types(context, types)
      assert {:ok, document} = Reader.read(indirect)
      assert document.pages == context.document.pages
      assert Info.page_count(indirect) == Info.page_count(direct)
      assert Info.page_sizes(indirect) == Info.page_sizes(direct)
      assert Text.extract(indirect) == Text.extract(direct)

      for operation <- [
            &Merge.merge([&1, &1]),
            &Transform.pick_pages(&1, [3, 1]),
            &Transform.delete_pages(&1, [2]),
            &Transform.rotate_pages(&1, 90),
            &Info.put(&1, title: "Updated"),
            &Stamp.text(&1, "Reviewed")
          ] do
        assert {:ok, expected} = operation.(direct)
        assert {:ok, actual} = operation.(indirect)
        assert Info.page_count(actual) == Info.page_count(expected)
        assert Info.page_sizes(actual) == Info.page_sizes(expected)
        assert Text.extract(actual) == Text.extract(expected)
      end

      assert {:ok, expected} = Split.by_page(direct)
      assert {:ok, actual} = Split.by_page(indirect)
      assert Enum.map(actual, &Text.extract/1) == Enum.map(expected, &Text.extract/1)
    end
  end

  test "validation and incremental updates preserve original indirect Type values", %{
    context: context
  } do
    pdf = indirect_types(context, ["Catalog", "Pages", "Page"])
    assert {:ok, parsed} = Reader.read_validated(pdf)
    assert {:ref, _} = parsed.catalog["Type"]
    assert Enum.all?(parsed.pages, &match?({:ref, _}, &1.dictionary["Type"]))
    assert {:ok, updated} = Info.put(pdf, title: "Preserved")
    assert {:ok, after_update} = Reader.read_validated(updated)
    assert after_update.catalog["Type"] == parsed.catalog["Type"]

    for {before_page, after_page} <- Enum.zip(parsed.pages, after_update.pages) do
      assert before_page.dictionary["Type"] == after_page.dictionary["Type"]
    end
  end

  test "indirect page types preserve indirect link remapping and removed-target omission", %{
    context: context
  } do
    [first, second, _third] = context.pages
    next = context.document.trailer["Size"]

    annotation = %{
      "Subtype" => {:ref, {next, 0}},
      "Rect" => [0, 0, 10, 10],
      "A" => %{"S" => {:ref, {next + 1, 0}}, "D" => {:name, "Target"}}
    }

    catalog =
      Map.put(context.catalog, "Dests", %{"Target" => [{:ref, second.ref}, {:name, "Fit"}]})

    {catalog_id, catalog_gen} = context.catalog_ref
    {page_id, page_gen} = first.ref

    assert {:ok, linked} =
             IncrementalWriter.write(context, [
               {catalog_id, catalog_gen, {:value, catalog}},
               {page_id, page_gen, {:value, Map.put(first.dictionary, "Annots", [annotation])}},
               {next, 0, {:value, {:name, "Link"}}},
               {next + 1, 0, {:value, {:name, "GoTo"}}}
             ])

    assert {:ok, linked_context} = Reader.read_validated(linked)
    pdf = indirect_types(linked_context, ["Catalog", "Pages", "Page"])

    for result <- [Merge.merge([pdf]), Transform.pick_pages(pdf, [1, 2])] do
      assert {:ok, output} = result
      assert {:ok, rebuilt} = Reader.read_validated(output)
      [page, target | _] = rebuilt.pages
      assert [link] = page.dictionary["Annots"]
      assert {:ok, link} = Reader.dictionary(rebuilt.document, link)
      assert {:ok, action} = Reader.dictionary(rebuilt.document, link["A"])
      assert [{:ref, target_ref}, {:name, "Fit"}] = action["D"]
      assert target_ref == target.ref
    end

    assert {:ok, output} = Transform.pick_pages(pdf, [1])
    assert {:ok, %{pages: [page]}} = Reader.read_validated(output)
    refute Map.has_key?(page.dictionary, "Annots")
  end

  test "selection rejects dependencies on indirect Page objects outside the page tree", %{
    context: context
  } do
    [first | _] = context.pages
    next = context.document.trailer["Size"]
    {id, generation} = first.ref

    for type <- [{:name, "Page"}, {:ref, {next + 1, 0}}] do
      assert {:ok, pdf} =
               IncrementalWriter.write(context, [
                 {id, generation,
                  {:value, Map.put(first.dictionary, "Extra", {:ref, {next, 0}})}},
                 {next, 0, {:value, %{"Type" => type}}},
                 {next + 1, 0, {:value, {:name, "Page"}}}
               ])

      assert {:error, {:unsupported_pdf_feature, diagnostic}} = Transform.pick_pages(pdf, [1])
      assert diagnostic.stage == :page_dependencies
      assert diagnostic.module == Transform
      assert diagnostic.operation == :pick_pages
      assert diagnostic.message =~ "not selected"
    end
  end

  test "invalid resolved types and unresolved chains retain actionable public diagnostics", %{
    context: context
  } do
    next = context.document.trailer["Size"]
    [page | _] = context.pages

    for ref <- [context.catalog_ref, context.page_tree_ref, page.ref] do
      {id, generation} = ref
      dictionary = context.document.objects[ref].value
      changed = {id, generation, {:value, Map.put(dictionary, "Type", {:ref, {next, 0}})}}

      for {objects, stage, message} <- [
            {[], :resolution, "missing"},
            {[{next, 0, {:value, {:ref, {next, 0}}}}], :resolution, "cycle"},
            {[{next, 0, {:value, 42}}], :page_tree, "malformed"},
            {[{next, 0, {:value, nil}}], :page_tree, "malformed"}
          ] do
        assert {:ok, pdf} = IncrementalWriter.write(context, [changed | objects])

        for {module, operation, run} <- [
              {Reader, :read, &Reader.read/1},
              {Info, :page_count, &Info.page_count/1},
              {Merge, :merge, &Merge.merge([&1])},
              {Transform, :pick_pages, &Transform.pick_pages(&1, [1])}
            ] do
          assert {:error, {:invalid_pdf_input, diagnostic}} = run.(pdf)
          assert diagnostic.stage == stage
          assert diagnostic.reason == :invalid_pdf_input
          assert diagnostic.module == module
          assert diagnostic.operation == operation

          expected =
            if stage == :page_tree and ref != context.catalog_ref,
              do: "invalid Type",
              else: message

          assert diagnostic.message =~ expected
        end
      end
    end
  end

  test "type reference chains use the existing bounded resolver", %{
    pdf: direct,
    context: context,
    limits: limits
  } do
    for types <- [["Catalog"], ["Pages"], ["Page"]] do
      Limits.install(limits)
      indirect = indirect_types(context, types)
      Limits.install(%{limits | max_pdf_reference_chain_depth: 1})
      assert {:ok, _} = Reader.read(direct)
      assert {:error, {:resource_limit_exceeded, diagnostic}} = Reader.read(indirect)
      assert diagnostic.stage == :limits
      assert diagnostic.module == Reader
      assert diagnostic.operation == :read
      assert diagnostic.message =~ "reference chain depth"
    end
  end

  defp indirect_types(context, types) do
    next = context.document.trailer["Size"]
    aliases = Map.new(Enum.with_index(types), fn {type, index} -> {type, next + index * 2} end)

    names =
      Enum.flat_map(aliases, fn {type, id} ->
        [{id, 0, {:value, {:ref, {id + 1, 0}}}}, {id + 1, 0, {:value, {:name, type}}}]
      end)

    objects =
      Enum.flat_map(context.document.objects, fn {{id, generation}, object} ->
        case object.value do
          %{"Type" => {:name, type}} = dictionary when is_map_key(aliases, type) ->
            [{id, generation, {:value, Map.put(dictionary, "Type", {:ref, {aliases[type], 0}})}}]

          _ ->
            []
        end
      end)

    assert {:ok, pdf} = IncrementalWriter.write(context, objects ++ names)
    pdf
  end
end
