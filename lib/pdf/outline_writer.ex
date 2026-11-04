defmodule NativeElixirPdfUtilities.Pdf.OutlineWriter do
  @moduledoc false

  alias NativeElixirPdfUtilities.Diagnostics
  alias NativeElixirPdfUtilities.Pdf.IncrementalWriter
  alias NativeElixirPdfUtilities.Pdf.OutlineBuilder
  alias NativeElixirPdfUtilities.Validators.OutlineValidator
  alias NativeElixirPdfUtilities.Validators.PdfValidator

  @doc false
  @spec write(PdfValidator.context(), [OutlineValidator.item()]) ::
          {:ok, binary()} | {:error, {atom(), Diagnostics.diagnostic()}}
  def write(context, items) do
    case context do
      %{
        catalog: catalog,
        catalog_ref: {catalog_object, catalog_generation},
        pages: pages,
        document: %{
          binary: pdf,
          trailer: trailer,
          xref_offset: previous_xref_offset
        }
      }
      when is_map(catalog) and is_list(pages) and is_binary(pdf) and is_map(trailer) and
             is_integer(previous_xref_offset) ->
        size = Map.fetch!(trailer, "Size")

        page_refs =
          pages |> Enum.with_index(1) |> Map.new(fn {page, number} -> {number, page.ref} end)

        built = OutlineBuilder.build(items, &Map.fetch!(page_refs, &1), size)

        updated_catalog =
          case built.root_ref do
            nil -> Map.delete(catalog, "Outlines")
            root_ref -> Map.put(catalog, "Outlines", {:ref, root_ref})
          end

        objects =
          [{catalog_object, catalog_generation, updated_catalog} | built.objects]

        IncrementalWriter.write(
          context,
          Enum.map(objects, fn {object, generation, value} ->
            {object, generation, {:value, value}}
          end)
        )

      _ ->
        Diagnostics.error(
          :incremental_write,
          :invalid_pdf_input,
          "prepared outline write context is malformed"
        )
    end
  end
end
