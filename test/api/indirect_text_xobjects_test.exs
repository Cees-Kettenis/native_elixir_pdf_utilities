defmodule NativeElixirPdfUtilities.IndirectTextXobjectsTest do
  use ExUnit.Case, async: true

  alias NativeElixirPdfUtilities.{HtmlToPdf, Stamp, Text}
  alias NativeElixirPdfUtilities.Pdf.{IncrementalWriter, Reader}

  test "stamped text retains string and positioned extraction through indirect Form subtypes" do
    assert {:ok, base} = HtmlToPdf.render("<p>Original</p>", system_font_discovery: false)
    assert {:ok, pdf} = Stamp.text(base, "APPROVED", system_font_discovery: false)
    assert {:ok, context} = Reader.read_validated(pdf)
    indirect = indirect_subtypes(context)

    assert {:ok, expected} = Text.extract(pdf, layout: false)
    assert expected =~ "APPROVED"
    assert Text.extract(indirect, layout: false) == {:ok, expected}
    assert Text.extract_spans(indirect) == Text.extract_spans(pdf)
  end

  test "nested Forms and image replacement text retain direct and indirect semantics" do
    assert {:ok, base} = HtmlToPdf.render("<p>Original</p>", system_font_discovery: false)
    assert {:ok, context} = Reader.read_validated(base)
    [page] = context.pages
    {page_id, generation} = page.ref
    next = context.document.trailer["Size"]
    assert {:ok, resources} = Reader.dictionary(context.document, page.dictionary["Resources"])
    resources = Map.put(resources, "XObject", %{"Outer" => {:ref, {next, 0}}})
    contents = List.wrap(page.dictionary["Contents"]) ++ [{:ref, {next + 3, 0}}]

    page_dictionary =
      page.dictionary |> Map.put("Resources", resources) |> Map.put("Contents", contents)

    assert {:ok, pdf} =
             IncrementalWriter.write(context, [
               {page_id, generation, {:value, page_dictionary}},
               {next, 0,
                {:stream,
                 %{
                   "Type" => {:name, "XObject"},
                   "Subtype" => {:name, "Form"},
                   "BBox" => [0, 0, 100, 100],
                   "Resources" => %{"XObject" => %{"Inner" => {:ref, {next + 1, 0}}}}
                 }, "/Inner Do"}},
               {next + 1, 0,
                {:stream,
                 %{
                   "Type" => {:name, "XObject"},
                   "Subtype" => {:name, "Form"},
                   "BBox" => [0, 0, 100, 100],
                   "Resources" => %{"XObject" => %{"Picture" => {:ref, {next + 2, 0}}}}
                 }, "q 20 0 0 10 0 0 cm /Span << /ActualText (Picture) >> BDC /Picture Do EMC Q"}},
               {next + 2, 0,
                {:stream,
                 %{
                   "Type" => {:name, "XObject"},
                   "Subtype" => {:name, "Image"},
                   "Width" => 1,
                   "Height" => 1,
                   "ColorSpace" => {:name, "DeviceRGB"},
                   "BitsPerComponent" => 8
                 }, <<255, 0, 0>>}},
               {next + 3, 0, {:stream, %{}, "/Outer Do"}}
             ])

    assert {:ok, context} = Reader.read_validated(pdf)
    indirect = indirect_subtypes(context)
    assert {:ok, expected} = Text.extract(pdf, layout: false)
    assert expected =~ "Picture"
    assert Text.extract(indirect, layout: false) == {:ok, expected}
    assert {:ok, %{pages: [%{spans: spans}]}} = Text.extract_spans(indirect, actual_text: true)
    assert Enum.any?(spans, &(&1.text == "Picture" and &1.image_resource == "Picture"))

    assert Text.extract_spans(indirect, actual_text: true) ==
             Text.extract_spans(pdf, actual_text: true)
  end

  test "missing and cyclic subtype references return actionable extraction diagnostics" do
    assert {:ok, base} = HtmlToPdf.render("<p>Original</p>", system_font_discovery: false)
    assert {:ok, pdf} = Stamp.text(base, "APPROVED", system_font_discovery: false)
    assert {:ok, context} = Reader.read_validated(pdf)

    {{id, generation}, object} =
      Enum.find(context.document.objects, fn {_ref, object} ->
        is_map(object.value) and object.value["Subtype"] == {:name, "Form"}
      end)

    next = context.document.trailer["Size"]

    for extra <- [[], [{next, 0, {:value, {:ref, {next, 0}}}}]] do
      assert {:ok, candidate} =
               IncrementalWriter.write(context, [
                 {id, generation,
                  {:stream, Map.put(object.value, "Subtype", {:ref, {next, 0}}), object.stream}}
                 | extra
               ])

      for {operation, result} <- [
            extract: Text.extract(candidate),
            extract_spans: Text.extract_spans(candidate)
          ] do
        assert {:error, {:invalid_pdf_input, diagnostic}} = result
        assert diagnostic.stage == :resolution
        assert diagnostic.reason == :invalid_pdf_input
        assert diagnostic.operation == operation
        assert diagnostic.module == Text
        assert diagnostic.message != ""
      end
    end
  end

  defp indirect_subtypes(context) do
    next = context.document.trailer["Size"]

    objects =
      context.document.objects
      |> Enum.filter(fn {_ref, object} ->
        is_map(object.value) and object.value["Subtype"] in [{:name, "Form"}, {:name, "Image"}]
      end)

    revisions =
      Enum.with_index(objects)
      |> Enum.flat_map(fn {{{id, generation}, object}, index} ->
        [
          {id, generation,
           {:stream, Map.put(object.value, "Subtype", {:ref, {next + index, 0}}), object.stream}},
          {next + index, 0, {:value, object.value["Subtype"]}}
        ]
      end)

    assert {:ok, pdf} = IncrementalWriter.write(context, revisions)
    pdf
  end
end
