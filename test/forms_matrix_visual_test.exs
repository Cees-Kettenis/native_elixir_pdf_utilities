defmodule NativeElixirPdfUtilities.FormsMatrixVisualTest do
  use ExUnit.Case, async: false

  alias NativeElixirPdfUtilities.{Forms, HtmlToPdf}
  alias NativeElixirPdfUtilities.Pdf.{IncrementalWriter, Reader}
  alias NativeElixirPdfUtilities.Validators.FormValidator
  alias NativeElixirPdfUtilities.TestSupport.PdfVisualCompare

  @moduletag :browser_parity

  test "flattening keeps artwork with very small placement scales" do
    {:ok, pdf} = HtmlToPdf.render("<div><input name='field'></div>")
    {:ok, context} = Reader.read_validated(pdf)
    {:ok, form} = FormValidator.inspect_document(context)
    widget = hd(hd(form.fields).widgets)
    {:ref, {id, generation}} = widget.ap["N"]

    for factor <- [1, 100_000_000] do
      {:ok, source} =
        IncrementalWriter.write(context, [
          {id, generation,
           {:stream,
            %{
              "Type" => {:name, "XObject"},
              "Subtype" => {:name, "Form"},
              "BBox" => [0, 0, 100_000_000, 100_000_000],
              "Matrix" => [factor, 0, 0, factor, 0, 0],
              "Resources" => %{}
            }, "1 0 0 rg 0 0 100000000 100000000 re f"}}
        ])

      assert {:ok, flattened} = Forms.flatten(source)

      for stats <-
            PdfVisualCompare.pdf_visual_stats!(source, flattened,
              artifact_dir: "tmp/forms_visual/small_matrix_#{factor}"
            ) do
        assert stats.changed_pixels == 0
      end
    end
  end
end
