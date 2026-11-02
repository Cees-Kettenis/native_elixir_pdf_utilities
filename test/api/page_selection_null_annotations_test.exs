defmodule NativeElixirPdfUtilities.PageSelectionNullAnnotationsTest do
  use ExUnit.Case, async: true

  alias NativeElixirPdfUtilities.{HtmlToPdf, Split, Text, Transform}
  alias NativeElixirPdfUtilities.Pdf.{IncrementalWriter, Reader}

  setup do
    assert {:ok, base} = HtmlToPdf.render("<p>Preserved</p>", system_font_discovery: false)
    assert {:ok, context} = Reader.read_validated(base)
    %{context: context}
  end

  test "selection and splitting treat omitted null and empty arrays identically", %{
    context: context
  } do
    next = context.document.trailer["Size"]

    for {value, objects} <- [
          {:omitted, []},
          {nil, []},
          {[], []},
          {{:ref, {next, 0}}, [{next, 0, {:value, nil}}]},
          {{:ref, {next, 0}}, [{next, 0, {:value, []}}]}
        ] do
      candidate = annotation_pdf(context, value, objects)
      assert {:ok, selected} = Transform.pick_pages(candidate, [1])
      assert {:ok, [split]} = Split.by_page(candidate)

      for output <- [selected, split] do
        assert {:ok, "Preserved"} = Text.extract(output, layout: false)
        assert {:ok, rebuilt} = Reader.read_validated(output)
        assert [page] = rebuilt.pages
        refute Map.has_key?(page.dictionary, "Annots")
      end
    end
  end

  test "resolved non-array annotations remain invalid", %{context: context} do
    next = context.document.trailer["Size"]

    for {value, objects} <- [{42, []}, {{:ref, {next, 0}}, [{next, 0, {:value, 42}}]}] do
      candidate = annotation_pdf(context, value, objects)

      for {operation, module, result} <- [
            {:pick_pages, Transform, Transform.pick_pages(candidate, [1])},
            {:split_by_page, Split, Split.by_page(candidate)}
          ] do
        assert {:error, {:invalid_pdf_input, diagnostic}} = result
        assert diagnostic.stage == :annotations
        assert diagnostic.reason == :invalid_pdf_input
        assert diagnostic.operation == operation
        assert diagnostic.module == module
        assert diagnostic.message =~ "Annots"
      end
    end
  end

  test "missing and cyclic annotation references fail with actionable diagnostics", %{
    context: context
  } do
    next = context.document.trailer["Size"]

    for objects <- [[], [{next, 0, {:value, {:ref, {next, 0}}}}]] do
      candidate = annotation_pdf(context, {:ref, {next, 0}}, objects)

      for {operation, module, result} <- [
            {:pick_pages, Transform, Transform.pick_pages(candidate, [1])},
            {:split_by_page, Split, Split.by_page(candidate)}
          ] do
        assert {:error, {:invalid_pdf_input, diagnostic}} = result
        assert diagnostic.reason == :invalid_pdf_input
        assert diagnostic.operation == operation
        assert diagnostic.module == module
        assert is_atom(diagnostic.stage)
        assert diagnostic.message != ""
      end
    end
  end

  defp annotation_pdf(context, value, objects) do
    [page] = context.pages
    {id, generation} = page.ref

    dictionary =
      if value == :omitted,
        do: Map.delete(page.dictionary, "Annots"),
        else: Map.put(page.dictionary, "Annots", value)

    assert {:ok, pdf} =
             IncrementalWriter.write(context, [{id, generation, {:value, dictionary}} | objects])

    pdf
  end
end
