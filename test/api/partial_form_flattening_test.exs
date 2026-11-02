defmodule NativeElixirPdfUtilities.PartialFormFlatteningTest do
  use ExUnit.Case, async: true

  alias NativeElixirPdfUtilities.{Forms, HtmlToPdf}
  alias NativeElixirPdfUtilities.Pdf.{IncrementalWriter, Reader}

  test "partial flattening preserves wide root and nested sibling order" do
    assert {:ok, pdf} = HtmlToPdf.render("<p>Form</p>", system_font_discovery: false)
    assert {:ok, context} = Reader.read_validated(pdf)
    first = context.document.trailer["Size"]

    leaves =
      Enum.map(0..999, fn index ->
        {first + index, 0,
         {:value,
          %{
            "T" => {:string, "field#{index}"},
            "FT" => {:name, "Tx"},
            "V" => {:string, "kept"},
            "Subtype" => {:name, "Widget"},
            "Rect" => [0, 0, 20, 20],
            "F" => 2
          }}}
      end)

    references = Enum.map(0..999, &{:ref, {first + &1, 0}})
    {catalog_id, catalog_generation} = context.catalog_ref
    [page] = context.pages
    {page_id, page_generation} = page.ref

    page_patch =
      {page_id, page_generation, {:value, Map.put(page.dictionary, "Annots", references)}}

    for nested? <- [false, true] do
      {roots, parents, prefix} =
        if nested? do
          children = Enum.slice(references, 1, 998)

          parents = [
            {first + 1000, 0, {:value, %{"T" => {:string, "group"}, "Kids" => children}}},
            {first + 1001, 0, {:value, %{"T" => {:string, "empty"}, "Kids" => [hd(references)]}}}
          ]

          {[{:ref, {first + 1001, 0}}, {:ref, {first + 1000, 0}}, List.last(references)], parents,
           "group."}
        else
          {references, [], ""}
        end

      catalog = Map.put(context.catalog, "AcroForm", %{"Fields" => roots})

      assert {:ok, source} =
               IncrementalWriter.write(
                 context,
                 leaves ++
                   parents ++ [page_patch, {catalog_id, catalog_generation, {:value, catalog}}]
               )

      assert {:ok, original} = Forms.fields(source)

      selected = [if(nested?, do: "empty.field0", else: "field0"), prefix <> "field500"]
      assert {:ok, flattened} = Forms.flatten(source, fields: selected)
      assert {:ok, remaining} = Forms.fields(flattened)

      assert Enum.map(remaining, &{&1.name, &1.value}) ==
               original
               |> Enum.reject(&(&1.name in selected))
               |> Enum.map(&{&1.name, &1.value})

      assert {:ok, flattened_context} = Reader.read_validated(flattened)

      assert {:ok, form} =
               Reader.dictionary(
                 flattened_context.document,
                 flattened_context.catalog["AcroForm"]
               )

      if nested? do
        assert form["Fields"] == [{:ref, {first + 1000, 0}}, List.last(references)]

        assert {:ok, group} =
                 Reader.dictionary(flattened_context.document, {:ref, {first + 1000, 0}})

        assert group["Kids"] ==
                 Enum.slice(references, 1, 998) |> List.delete({:ref, {first + 500, 0}})
      else
        assert form["Fields"] ==
                 references
                 |> List.delete(hd(references))
                 |> List.delete({:ref, {first + 500, 0}})
      end

      assert {:ok, fully_flattened} = Forms.flatten(flattened)
      assert {:ok, []} = Forms.fields(fully_flattened)
    end
  end
end
