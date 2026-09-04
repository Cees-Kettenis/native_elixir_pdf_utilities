defmodule NativeElixirPdfUtilities.Pdf.OutlineWriter do
  @moduledoc false

  alias NativeElixirPdfUtilities.Diagnostics
  alias NativeElixirPdfUtilities.Pdf.InfoCodec
  alias NativeElixirPdfUtilities.Pdf.OutlineBuilder
  alias NativeElixirPdfUtilities.Validators.IncrementalValidator
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

        write_increment(
          pdf,
          trailer,
          previous_xref_offset,
          objects,
          max(size, built.next_id)
        )

      _ ->
        error("prepared outline write context is malformed")
    end
  end

  defp write_increment(pdf, trailer, previous_xref_offset, objects, size) do
    separator = trailing_separator(pdf)
    starting_position = byte_size(pdf) + byte_size(separator)

    result =
      Enum.reduce_while(objects, {:ok, [], [], starting_position}, fn
        {object, generation, dictionary}, {:ok, pieces, entries, position} ->
          case InfoCodec.serialize_value(dictionary) do
            {:ok, body} ->
              piece = [
                Integer.to_string(object),
                " ",
                Integer.to_string(generation),
                " obj\n",
                body,
                "\nendobj\n"
              ]

              {:cont,
               {:ok, [piece | pieces], [{object, generation, position} | entries],
                position + :erlang.iolist_size(piece)}}

            :error ->
              {:halt, error("outline objects cannot be serialized")}
          end
      end)

    case result do
      {:ok, pieces, entries, xref_offset} ->
        pieces = Enum.reverse(pieces)

        with {:ok, trailer_id} <-
               IncrementalValidator.prepare_identifier(Map.get(trailer, "ID"), [pdf, pieces]) do
          incremental_trailer =
            %{
              "Size" => size,
              "Root" => Map.fetch!(trailer, "Root"),
              "Prev" => previous_xref_offset
            }
            |> copy_trailer_entry(trailer, "Info")
            |> maybe_put_identifier(trailer_id)

          case InfoCodec.serialize_value(incremental_trailer) do
            {:ok, trailer_io} ->
              xref_entries =
                entries
                |> Enum.sort_by(fn {object, _generation, _offset} -> object end)
                |> Enum.map(fn {object, generation, offset} ->
                  [
                    Integer.to_string(object),
                    " 1\n",
                    padded(offset, 10),
                    " ",
                    padded(generation, 5),
                    " n \n"
                  ]
                end)

              {:ok,
               IO.iodata_to_binary([
                 pdf,
                 separator,
                 pieces,
                 "xref\n",
                 xref_entries,
                 "trailer\n",
                 trailer_io,
                 "\nstartxref\n",
                 Integer.to_string(xref_offset),
                 "\n%%EOF\n"
               ])}

            :error ->
              error("incremental outline trailer cannot be serialized")
          end
        end

      {:error, _error} = writer_error ->
        writer_error
    end
  end

  defp copy_trailer_entry(updated, source, key) do
    case Map.fetch(source, key) do
      {:ok, value} -> Map.put(updated, key, value)
      :error -> updated
    end
  end

  defp maybe_put_identifier(trailer, identifier) do
    case identifier do
      nil -> trailer
      identifier -> Map.put(trailer, "ID", identifier)
    end
  end

  defp trailing_separator(pdf) do
    case byte_size(pdf) > 0 and :binary.last(pdf) in [?\n, ?\r] do
      true -> ""
      false -> "\n"
    end
  end

  defp padded(integer, length) do
    integer |> Integer.to_string() |> String.pad_leading(length, "0")
  end

  defp error(message) do
    Diagnostics.error(:incremental_write, :invalid_pdf_input, message)
  end
end
