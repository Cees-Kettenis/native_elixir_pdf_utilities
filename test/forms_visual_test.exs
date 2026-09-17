defmodule NativeElixirPdfUtilities.FormsVisualTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.{Forms, HtmlToPdf}
  alias NativeElixirPdfUtilities.TestSupport.PdfVisualCompare

  @moduletag :browser_parity

  test "flattening preserves hidden widget visibility" do
    {:ok, pdf} =
      HtmlToPdf.render("""
      <div><input name="hidden" value="Secret"></div>
      <div><input type="radio" name="choice" value="shown" checked>
      <input type="radio" name="choice" value="hidden"></div>
      """)

    {:ok, context} = NativeElixirPdfUtilities.Pdf.Reader.read_validated(pdf)
    {:ok, form} = NativeElixirPdfUtilities.Validators.FormValidator.inspect_document(context)

    for flags <- [2, 32, 36] do
      patches =
        for field <- form.fields,
            {widget, index} <- Enum.with_index(field.widgets),
            field.name == "hidden" or index == 1 do
          {:ref, {id, gen}} = widget.ref
          {id, gen, {:value, Map.put(widget.dictionary, "F", flags)}}
        end

      {:ok, hidden} = NativeElixirPdfUtilities.Pdf.IncrementalWriter.write(context, patches)
      assert {:ok, flattened} = Forms.flatten(hidden)

      for stats <-
            PdfVisualCompare.pdf_visual_stats!(hidden, flattened,
              artifact_dir: "tmp/forms_visual/hidden-#{flags}"
            ) do
        assert stats.changed_pixels == 0
      end
    end
  end

  test "interactive controls match static artwork and flattening preserves filled appearances" do
    html = """
    <style>
    input, textarea, select { background: #eee; border: 2px solid #123456; }
    </style>
    <div><input name="person" value="Old name"></div>
    <div><input type="checkbox" name="agree"></div>
    <div><input type="radio" name="contact" value="email" checked>
    <input type="radio" name="contact" value="phone"></div>
    <div><textarea name="notes">Original notes</textarea></div>
    <div><select name="country"><option value="NL">Netherlands</option>
    <option value="MY">Malaysia</option></select></div>
    """

    assert {:ok, interactive} = HtmlToPdf.render(html)
    assert {:ok, static} = HtmlToPdf.render(html, forms: :static)

    for stats <-
          PdfVisualCompare.pdf_visual_stats!(static, interactive,
            artifact_dir: "tmp/forms_visual/generated"
          ) do
      assert stats.changed_ratio < 0.002
    end

    assert {:ok, filled} =
             Forms.fill(interactive, %{
               "person" => "Cees Kettenis",
               "agree" => true,
               "contact" => "phone",
               "notes" => "First line\nSecond line",
               "country" => "MY"
             })

    [stats] =
      PdfVisualCompare.assert_pdf_visual_change!(interactive, filled,
        artifact_dir: "tmp/forms_visual/filled"
      )

    assert stats.changed_pixels > 0

    # A rotated page must keep the same widget placement after flattening.
    {:ok, context} = NativeElixirPdfUtilities.Pdf.Reader.read_validated(filled)
    page = hd(context.pages)
    {id, generation} = page.ref

    {:ok, filled} =
      NativeElixirPdfUtilities.Pdf.IncrementalWriter.write(
        context,
        [{id, generation, {:value, Map.put(page.dictionary, "Rotate", 90)}}]
      )

    assert {:ok, flattened} = Forms.flatten(filled)

    for stats <-
          PdfVisualCompare.pdf_visual_stats!(filled, flattened,
            artifact_dir: "tmp/forms_visual/flattened"
          ) do
      assert stats.changed_pixels == 0
    end
  end
end
