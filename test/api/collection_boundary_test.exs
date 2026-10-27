defmodule NativeElixirPdfUtilities.CollectionBoundaryTest do
  use ExUnit.Case, async: false

  alias NativeElixirPdfUtilities.{Attachments, Limits, Merge, Outlines, Split, Transform}
  alias NativeElixirPdfUtilities.HtmlToPdf.PdfWriter

  setup do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    assert {:ok, pdf} = PdfWriter.render(List.duplicate(%{size: {100, 100}, boxes: []}, 2))
    %{pdf: pdf}
  end

  test "improper caller collections and nested tails return public diagnostics", %{pdf: pdf} do
    for {module, operation, run} <- [
          {Merge, :merge, fn -> Merge.merge([pdf | :bad]) end},
          {Transform, :pick_pages, fn -> Transform.pick_pages(pdf, [1 | :bad]) end},
          {Transform, :delete_pages, fn -> Transform.delete_pages(pdf, [1 | :bad]) end},
          {Transform, :rotate_pages,
           fn -> Transform.rotate_pages(pdf, 90, pages: [1 | :bad]) end},
          {Split, :split_by_ranges, fn -> Split.by_ranges(pdf, [1..1 | :bad]) end},
          {Outlines, :put_outlines, fn -> Outlines.put(pdf, [{"Page", 1} | :bad]) end},
          {Outlines, :put_outlines,
           fn -> Outlines.put(pdf, [{"Parent", 1, [{"Child", 2} | :bad]}]) end},
          {Attachments, :embed,
           fn -> Attachments.embed(pdf, [%{filename: "a", bytes: "a"} | :bad]) end}
        ] do
      assert {:error, {reason, diagnostic}} = run.()
      refute reason == :operation_worker_failed
      assert diagnostic.reason == reason and diagnostic.module == module
      assert diagnostic.operation == operation
      assert is_atom(diagnostic.stage) and is_binary(diagnostic.message)
    end
  end

  test "collection ceilings apply before preparing oversized requests", %{pdf: pdf} do
    limits = Limits.effective()

    for {key, run} <- [
          {:max_merge_inputs, fn -> Merge.merge([pdf, pdf]) end},
          {:max_split_outputs, fn -> Split.by_ranges(pdf, [1..1, 2..2]) end},
          {:max_pdf_attachments,
           fn ->
             Attachments.embed(pdf, [
               %{filename: "a", bytes: "a"},
               %{filename: "b", bytes: "b"}
             ])
           end},
          {:max_pdf_outline_items, fn -> Outlines.put(pdf, [{"Parent", 1, [{"Child", 2}]}]) end}
        ] do
      Limits.install(Map.put(limits, key, 1))
      assert {:error, {:resource_limit_exceeded, diagnostic}} = run.()
      assert is_binary(diagnostic.message) and is_atom(diagnostic.operation)
    end

    Limits.install(%{limits | max_pdf_outline_items: 2})

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             Outlines.put(pdf, [{"Parent", 1, [{"Child", 2}]}, {"Sibling", 1}])

    assert diagnostic.operation == :put_outlines and diagnostic.message =~ "item count"
    Limits.install(limits)
    assert {:ok, single} = PdfWriter.render([%{size: {100, 100}, boxes: []}])
    Limits.install(%{limits | max_pdf_pages: 1})
    assert {:error, {:resource_limit_exceeded, diagnostic}} = Transform.pick_pages(single, [1, 1])
    assert diagnostic.operation == :pick_pages and diagnostic.message =~ "max_pdf_pages"
  end
end
