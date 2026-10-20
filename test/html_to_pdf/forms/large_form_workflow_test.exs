defmodule NativeElixirPdfUtilities.HtmlToPdf.LargeFormWorkflowTest do
  use ExUnit.Case, async: true

  alias NativeElixirPdfUtilities.{Forms, HtmlToPdf, Limits, Text}
  alias NativeElixirPdfUtilities.Pdf.Reader

  test "native appearances support one thousand editable controls under default page limits" do
    for count <- [1, 100, 1_000] do
      html =
        Enum.map_join(1..count, fn i ->
          "<div><input name='field#{i}' value='Initial#{i}'></div>"
        end)

      assert {:ok, pdf} = HtmlToPdf.render(html, system_font_discovery: false)
      assert {:ok, fields} = Forms.fields(pdf)

      assert Enum.map(fields, &{&1.name, &1.value}) ==
               Enum.map(1..count, &{"field#{&1}", "Initial#{&1}"})

      assert {:ok, context} = Reader.read_validated(pdf)
      assert length(context.pages) < Limits.get(:max_rendered_pages)

      catalogs =
        for {_ref, %{value: %{"Type" => {:name, "Catalog"}}}} <- context.document.objects,
            do: true

      assert length(catalogs) == 1

      assert {:ok, form} =
               NativeElixirPdfUtilities.Validators.FormValidator.inspect_document(context)

      for field <- form.fields do
        assert field.flags == 0 and field.type == :text
        [widget] = field.widgets
        normal = widget.dictionary["AP"]["N"]
        background = widget.dictionary["NEPUBackground"]
        assert {:ok, normal_dictionary} = Reader.dictionary(context.document, normal)
        assert {:ok, background_dictionary} = Reader.dictionary(context.document, background)
        assert normal_dictionary["Type"] == {:name, "XObject"}
        assert normal_dictionary["Subtype"] == {:name, "Form"}
        assert normal_dictionary["BBox"] == background_dictionary["BBox"]
        assert normal != background
      end

      values = Map.new(1..count, &{"field#{&1}", "Updated#{&1}"})
      assert {:ok, filled} = Forms.fill(pdf, values)
      assert {:ok, fields} = Forms.fields(filled)

      assert Enum.map(fields, &{&1.name, &1.value}) ==
               Enum.map(1..count, &{"field#{&1}", "Updated#{&1}"})

      assert {:ok, flat} = Forms.flatten(filled)
      assert {:ok, []} = Forms.fields(flat)
      assert {:ok, text} = Text.extract(flat)
      for i <- 1..count, do: assert(String.contains?(text, "Updated#{i}"))
      refute text =~ "Initial"
    end
  end
end
