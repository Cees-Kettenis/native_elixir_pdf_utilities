alias NativeElixirPdfUtilities.TestSupport.Paths

Code.require_file(Paths.support("svg_public_workflow.ex"))
Code.require_file(Paths.support("png_fixture.ex"))

defmodule NativeElixirPdfUtilities.HtmlToPdf.SvgDocumentSourcePrintTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.{Limits}
  alias NativeElixirPdfUtilities.HtmlToPdf.{ResourceContext, PdfWriter}
  alias NativeElixirPdfUtilities.Pdf.Reader
  alias NativeElixirPdfUtilities.Validators.HtmlValidator

  setup do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    :ok
  end

  test "document glyphs enter the source print canvas while links retain their point bounds" do
    font = File.read!("priv/fonts/dejavu/DejaVuSans.ttf")

    source =
      "<svg width='160' height='60'><text x='8' y='28' font-size='20' font-family='DejaVu Sans'>Vector Ω café</text></svg>"

    ResourceContext.run([], HtmlValidator.new_image_budget(), fn context ->
      assert {:ok, doc} =
               NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.document(
                 source,
                 context.image_budget,
                 "glyph.svg",
                 :object,
                 [%{data: font}]
               )

      assert {:ok, image} =
               NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.compose_document(
                 doc,
                 120,
                 45
               )

      box = %{
        type: :image,
        image: image,
        x: 6,
        y: 9,
        width: 120,
        height: 45,
        link_url: "https://example.com/document"
      }

      assert {:ok, pdf} =
               PdfWriter.render([%{size: {390, 60}, boxes: [box]}], compress_streams: false)

      assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf)

      assert {:error, {:no_extractable_text, _}} =
               NativeElixirPdfUtilities.Text.extract(pdf, layout: false)

      assert {:ok, read} = Reader.read_validated(pdf)

      assert Enum.any?(Map.values(read.document.objects), fn object ->
               object.value["Subtype"] == {:name, "Link"} and
                 get_in(object.value, ["A", "URI"]) == {:string, "https://example.com/document"}
             end)
    end)
  end
end
