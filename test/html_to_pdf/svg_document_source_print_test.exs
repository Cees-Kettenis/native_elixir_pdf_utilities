Code.require_file("../support/svg_public_workflow.ex", __DIR__)
Code.require_file("../support/png_fixture.ex", __DIR__)

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

  test "real document glyphs enter the source print canvas while links retain their point bounds" do
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

  test "nested document source paint uses the actual local plane height" do
    ResourceContext.run([], HtmlValidator.new_image_budget(), fn context ->
      assert {:ok, doc} =
               NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.document(
                 "<svg width='160' height='60'><rect width='160' height='60' fill='red'/></svg>",
                 context.image_budget
               )

      assert {:ok, image} =
               NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.compose_document(
                 doc,
                 120,
                 45
               )

      box = %{type: :image, image: image, x: 0, y: 0, width: 120, height: 45}

      group = %{
        type: :drawing_group,
        width: 120,
        height: 45,
        transform: {1, 0.25, 0.5, 1, 6, 9},
        clip: nil,
        boxes: [box]
      }

      assert {:ok, pdf} =
               PdfWriter.render([%{size: {390, 60}, boxes: [group]}], compress_streams: false)

      assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf)
    end)
  end

  test "actual source print coordinates enforce the existing PDF numeric ceiling" do
    ResourceContext.run([], HtmlValidator.new_image_budget(), fn context ->
      assert {:ok, doc} =
               NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.document(
                 "<svg width='160' height='60'><rect width='160' height='60'/></svg>",
                 context.image_budget
               )

      assert {:ok, image} =
               NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.compose_document(
                 doc,
                 120,
                 45
               )

      Limits.install(Map.put(Limits.effective(), :max_pdf_numeric_magnitude, 1000))
      box = %{type: :image, image: image, x: 261.75, y: 9, width: 120, height: 45}

      assert {:ok, pdf} = PdfWriter.render([%{size: {390, 60}, boxes: [box]}])
      assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf)
    end)
  end
end
