defmodule NativeElixirPdfUtilities.Pdf.ObjectMappingTest do
  use ExUnit.Case, async: true

  alias NativeElixirPdfUtilities.HtmlToPdf
  alias NativeElixirPdfUtilities.Pdf.{ObjectMapping, Reader}

  test "resource traversal and remapping retain the references of a real interactive page" do
    assert {:ok, pdf} =
             HtmlToPdf.render(
               "<a href='https://example.test/resource'>Owned link</a><input name='note' value='Original'>",
               system_font_discovery: false
             )

    assert {:ok, document} = Reader.read(pdf)
    [page] = document.pages
    assert {:ok, dictionary} = Reader.dictionary(document, {:ref, page.ref})
    refs = ObjectMapping.references(dictionary)
    assert refs != []
    assert Enum.all?(refs, &Map.has_key?(document.objects, &1))
    assert {:ref, contents} = dictionary["Contents"]
    assert contents in refs
    for {:ref, annotation} <- dictionary["Annots"], do: assert(annotation in refs)
    assert ObjectMapping.references(dictionary["MediaBox"]) == []

    mapping = Map.new(refs, fn {id, generation} -> {{id, generation}, id + 100} end)
    remapped = ObjectMapping.remap(dictionary, mapping)
    assert remapped["MediaBox"] == dictionary["MediaBox"]
    assert remapped["Contents"] == {:ref, {elem(contents, 0) + 100, elem(contents, 1)}}

    assert Enum.sort(ObjectMapping.references(remapped)) ==
             Enum.sort(Enum.map(refs, fn {id, generation} -> {id + 100, generation} end))
  end
end
