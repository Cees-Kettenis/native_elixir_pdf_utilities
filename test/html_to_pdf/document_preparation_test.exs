defmodule NativeElixirPdfUtilities.HtmlToPdf.DocumentPreparationTest do
  use ExUnit.Case, async: true

  alias NativeElixirPdfUtilities.{HtmlToPdf, Text}
  alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, PdfWriter, ResourceContext}
  alias NativeElixirPdfUtilities.Validators.HtmlValidator

  test "preparation executes actual fonts, pagination and document options before PDF writing" do
    html =
      "<title>Prepared title</title><h1 id='target'>Heading</h1><p><a href='#target'>Jump</a></p>"

    assert {:ok, dom} = HtmlParser.parse_document_detailed(html)
    budget = HtmlValidator.new_image_budget()

    ResourceContext.run([], budget, fn _context ->
      assert {:ok, prepared} =
               HtmlToPdf.prepare_document(dom, [outlines: :headings, margin: 24], budget)

      assert length(prepared.pages) == 1
      assert prepared.layout_tree.boxes != []
      assert Keyword.get(prepared.options, :metadata)[:title] == "Prepared title"
      assert [_heading] = Keyword.fetch!(prepared.options, :outlines)
      assert {:ok, pdf} = PdfWriter.render(prepared.pages, prepared.options)
      assert {:ok, text} = Text.extract(pdf)
      assert text =~ "Heading"
      assert text =~ "Jump"
    end)
  end

  test "preparation rejects a different image budget within an issued resource scope" do
    assert {:ok, dom} = HtmlParser.parse_document_detailed("<p>Body</p>")
    budget = HtmlValidator.new_image_budget()

    ResourceContext.run([], budget, fn _context ->
      assert {:error,
              {:invalid_options,
               %{stage: :options, reason: :invalid_options, source: "image_budget"} = diagnostic}} =
               HtmlToPdf.prepare_document(dom, [], HtmlValidator.new_image_budget())

      assert diagnostic.message =~ "current issued image budget"
      assert diagnostic.operation == :prepare_document
    end)
  end

  test "preparation requires a live resource scope and rejects caller source ownership" do
    assert {:ok, dom} = HtmlParser.parse_document_detailed("<p>Body</p>")
    budget = HtmlValidator.new_image_budget()

    assert {:error, {:invalid_options, %{stage: :resource, message: message}}} =
             HtmlToPdf.prepare_document(dom, [], budget)

    assert message =~ "scoped context"

    ResourceContext.run([], budget, fn _context ->
      assert {:error, {:invalid_options, %{stage: :options, message: message}}} =
               HtmlToPdf.prepare_document(dom, [__template_source_owner__: make_ref()], budget)

      assert message =~ "__template_source_owner__"
    end)
  end

  test "preparation delegates malformed document rejection to the document validator" do
    budget = HtmlValidator.new_image_budget()

    ResourceContext.run([], budget, fn _context ->
      assert {:error, {:invalid_document, %{stage: :style, message: message}}} =
               HtmlToPdf.prepare_document(%{children: []}, [], budget)

      assert message =~ "parsed HTML document"
    end)
  end
end
