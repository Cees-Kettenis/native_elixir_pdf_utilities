defmodule NativeElixirPdfUtilities.CollectionBoundaryTest do
  use ExUnit.Case, async: false

  alias NativeElixirPdfUtilities.{
    Attachments,
    Forms,
    HtmlToPdf,
    Info,
    Limits,
    Merge,
    Outlines,
    Split,
    Transform
  }

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
          {:max_entries, fn -> Merge.merge([pdf, pdf]) end},
          {:max_entries, fn -> Split.by_ranges(pdf, [1..1, 2..2]) end},
          {:max_entries,
           fn ->
             Attachments.embed(pdf, [
               %{filename: "a", bytes: "a"},
               %{filename: "b", bytes: "b"}
             ])
           end},
          {:max_entries, fn -> Outlines.put(pdf, [{"Parent", 1, [{"Child", 2}]}]) end}
        ] do
      Limits.install(Map.put(limits, key, 1))
      assert {:error, {:resource_limit_exceeded, diagnostic}} = run.()
      assert is_binary(diagnostic.message) and is_atom(diagnostic.operation)
    end

    Limits.install(%{limits | max_entries: 2})

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             Outlines.put(pdf, [{"Parent", 1, [{"Child", 2}]}, {"Sibling", 1}])

    assert diagnostic.operation == :put_outlines and diagnostic.message =~ "max_entries"
    Limits.install(limits)
    assert {:ok, single} = PdfWriter.render([%{size: {100, 100}, boxes: []}])
    Limits.install(%{limits | max_pages: 1})
    assert {:error, {:resource_limit_exceeded, diagnostic}} = Transform.pick_pages(single, [1, 1])
    assert diagnostic.operation == :pick_pages and diagnostic.message =~ "max_pages"
  end

  test "nested field names, keywords and choice values return public diagnostics" do
    assert {:ok, pdf} =
             HtmlToPdf.render(
               "<input name='x'><select name='c' multiple><option value='a'>A</option><option value='b'>B</option></select>",
               system_font_discovery: false
             )

    for tail <- [:bad, nil, %{}],
        {module, operation, reason, run} <- [
          {Forms, :flatten, :invalid_options, fn -> Forms.flatten(pdf, fields: ["x" | tail]) end},
          {Info, :put_info, :invalid_pdf_input,
           fn -> Info.put(pdf, keywords: ["valid" | tail]) end},
          {Forms, :fill, :invalid_form_value, fn -> Forms.fill(pdf, %{"c" => ["a" | tail]}) end}
        ] do
      assert {:error, {^reason, diagnostic}} = run.()
      assert diagnostic.reason == reason and diagnostic.module == module
      assert diagnostic.operation == operation
      assert is_atom(diagnostic.stage) and is_binary(diagnostic.message)
      assert diagnostic.message != ""
    end

    assert {:ok, ^pdf} = Forms.flatten(pdf, fields: [])
    assert {:ok, filled} = Forms.fill(pdf, %{"c" => ["a", "b"]})
    assert {:ok, fields} = Forms.fields(filled)
    assert Enum.find(fields, &(&1.name == "c")).value == ["a", "b"]
  end

  test "nested lists respect field counts and the minimum keyword separator bytes", %{pdf: pdf} do
    original = Limits.effective()
    Limits.install(%{original | max_entries: 1})

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             Forms.flatten(pdf, fields: ["a", "b"])

    assert diagnostic.operation == :flatten and diagnostic.module == Forms
    assert diagnostic.stage == :forms and diagnostic.reason == :resource_limit_exceeded
    assert diagnostic.message =~ "max_entries"

    Limits.install(%{original | max_decoded_bytes: 4})
    assert {:ok, updated} = Info.put(pdf, keywords: ["", "", ""])
    assert {:ok, %{keywords: ", , "}} = Info.get(updated)

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             Info.put(pdf, keywords: ["", "", "", ""])

    assert diagnostic.operation == :put_info and diagnostic.module == Info
    assert diagnostic.stage == :limits and diagnostic.reason == :resource_limit_exceeded
    assert diagnostic.message =~ "keywords" and diagnostic.message =~ "byte limit"
  end
end
