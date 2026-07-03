defmodule NativeElixirPdfUtilities.MergeTest do
  use ExUnit.Case

  alias NativeElixirPdfUtilities.Merge

  defp merge_pdf(objects) do
    body =
      objects
      |> Enum.map(fn {id, source} -> "#{id} 0 obj #{source} endobj\n" end)
      |> Enum.join("")

    "%PDF-1.7\n" <> body <> "%%EOF\n"
  end

  test "rejects an empty input list" do
    assert_raise ArgumentError, "merge/1 expects at least one PDF binary", fn ->
      Merge.merge([])
    end
  end

  test "renumbers pages and injects inherited page attributes" do
    content = """
    BT
    1 0 0 1 0 0 Tm
    (Hi) Tj
    ET
    """

    pdf =
      merge_pdf([
        {1, "<< /Type /Catalog /Pages 2 0 R >>"},
        {2,
         "<< /Type /Pages /Kids [ 3 0 R ] /Count 1 /Resources << /Font << /F1 4 0 R >> >> /MediaBox [ 0 0 612 792 ] >>"},
        {3, "<< [] /Type /Page /Parent 2 0 R /Contents 10 0 R /Annots [] >>"},
        {4, "<< /Type /Font /Subtype /Type1 /BaseFont /Helvetica >>"},
        {10, "<< /Length #{byte_size(content)} >>\nstream\n#{content}endstream"}
      ])

    assert {:ok, merged} = Merge.merge([pdf, pdf])

    assert merged =~ "%PDF-1.7"
    assert merged =~ "/Type /Pages /Kids [ 6 0 R 17 0 R ] /Count 2"
    assert merged =~ "/Parent 1 0 R"
    assert merged =~ "/Resources << /Font << /F1 7 0 R >> >>"
    assert merged =~ "/MediaBox [ 0 0 612 792 ]"
    assert merged =~ "xref\n0 25\n"
    assert merged =~ " 00000 f"
  end

  test "handles sparse and unusual object bodies without changing stream bytes" do
    pdf =
      merge_pdf([
        {1, "<< /Type /Catalog /Pages 99 0 R >>"},
        {3, "/Type /Page /MediaBox [ 1 2 nope 4 ] /Resources [] /Contents 10 0 R"},
        {10,
         """
         << /Length 6 /Flag true /Other false /Nothing null /Real 1.25 /One 1.0 /Name /AName /Hex <0F> /Literal (a\\n\\r\\t\\b\\f\\(\\)\\\\\\001) /Ref 3 0 R >>
         stream
         abc123
         endstream
         """},
        {12, "xref trailer startxref R true false null cm"}
      ])

    assert {:ok, merged} = Merge.merge([pdf])

    assert merged =~ "/MediaBox [ 0 0 595 842 ]"
    assert merged =~ "/Flag true /Other false /Nothing null"
    assert merged =~ "/Real 1.25 /One 1"
    assert merged =~ "/Hex <0F>"
    assert merged =~ "/Literal (a\\n\\r\\t\\b\\f\\("
    assert merged =~ "\nstream\nabc123\nendstream"
    assert merged =~ "cm"
  end

  test "tolerates inputs without pages or catalogs" do
    no_catalog =
      "xref\n" <> merge_pdf([{4, "<< /Type /NotPage /Value [ << /Nested true >> ] >>"}])

    no_root_pages = merge_pdf([{1, "<< /Type /Catalog >>"}, {2, "<< /Type /NotPage >>"}])

    assert {:ok, merged} = Merge.merge([no_catalog, no_root_pages])
    assert merged =~ "/Type /Pages /Kids [  ] /Count 0"
  end

  test "covers page defaults without inherited resources" do
    pdf =
      merge_pdf([
        {1, "<< /Type /Catalog /Pages 2 0 R >>"},
        {2, "<< /Type /Pages /Kids [ 3 0 R ] /Count 1 /Resources >>"},
        {3, "<< /Type /Page /Resources [] /MediaBox [ 0 0 200 300 ] >>"}
      ])

    assert {:ok, merged} = Merge.merge([pdf])

    assert merged =~ "/MediaBox [ 0 0 200 300 ]"
    assert merged =~ "/Resources [ ]"
  end

  test "handles nested page dictionaries and malformed inherited values" do
    nested_page_pdf =
      merge_pdf([
        {1, "<< /Type /Catalog /Pages 2 0 R >>"},
        {2, "<< /Type /Pages /Kids [ 3 0 R ] /Count 1 >>"},
        {3,
         "<< [] /Type /Page /Resources << /ProcSet [ /PDF ] /Font << /F1 4 0 R >> >> /MediaBox [ 0.5 0 200.25 300 ] /Contents 5 0 R /AltParent 3 0 R >>"},
        {4, "<< /Type /Font >>"},
        {5, "<< /Length 2 >> stream\nHi\nendstream"}
      ])

    malformed_root_pdf =
      "%PDF-1.7\n" <>
        "1 0 obj << /Type /Catalog /Other /Thing /More /Stuff >> endobj\n" <>
        "2 0 obj << /Type /Pages /Kids [ 3 0 R ] /Count 1 /Resources endobj\n" <>
        "3 0 obj << /Type /Page /MediaBox /Bad >> endobj\n" <>
        "%%EOF\n"

    unterminated_inherited_pdf =
      "%PDF-1.7\n" <>
        "1 0 obj << /Type /Catalog /Pages 2 0 R >> endobj\n" <>
        "2 0 obj << /Type /Pages /Kids [ 3 0 R ] /Count 1 /Resources << /Font << /F1 4 0 R >> endobj\n" <>
        "3 0 obj << /Type /Page >> endobj\n" <>
        "4 0 obj << /Type /Font >> endobj\n" <>
        "%%EOF\n"

    empty_inherited_pdf =
      "%PDF-1.7\n" <>
        "1 0 obj << /Type /Catalog /Pages 2 0 R >> endobj\n" <>
        "2 0 obj << /Type /Pages /Kids [ 3 0 R ] /Count 1 /Resources endobj\n" <>
        "3 0 obj << /Type /Page >> endobj\n" <>
        "%%EOF\n"

    assert {:ok, merged} =
             Merge.merge([
               nested_page_pdf,
               malformed_root_pdf,
               unterminated_inherited_pdf,
               empty_inherited_pdf
             ])

    assert merged =~ "/MediaBox [ 0.5 0 200.25 300 ]"
    assert merged =~ "/Resources << /ProcSet [ /PDF ] /Font << /F1"
    assert merged =~ "/AltParent 6 0 R"
    assert merged =~ "/MediaBox [ 0 0 595 842 ]"
  end
end
