defmodule NativeElixirPdfUtilities.Pdf.IncrementalWriter do
  @moduledoc false

  alias NativeElixirPdfUtilities.Diagnostics
  alias NativeElixirPdfUtilities.Pdf.InfoCodec
  alias NativeElixirPdfUtilities.Validators.IncrementalValidator
  alias NativeElixirPdfUtilities.Validators.PdfValidator

  @type object_body :: {:value, PdfValidator.value()} | {:stream, map(), binary()}
  @type object_entry :: {non_neg_integer(), non_neg_integer(), object_body()}

  @doc false
  @spec write(PdfValidator.context(), [object_entry()]) ::
          {:ok, binary()} | {:error, {atom(), Diagnostics.diagnostic()}}
  def write(context, objects) do
    case context do
      %{
        document: %{
          binary: pdf,
          trailer: trailer,
          xref_offset: previous_xref_offset
        }
      }
      when is_binary(pdf) and is_map(trailer) and is_integer(previous_xref_offset) and
             is_list(objects) ->
        write_increment(pdf, trailer, previous_xref_offset, objects)

      _ ->
        error("prepared incremental write context is malformed")
    end
  end

  defp write_increment(pdf, trailer, previous_xref_offset, objects) do
    separator = trailing_separator(pdf)
    starting_position = byte_size(pdf) + byte_size(separator)

    result =
      Enum.reduce_while(objects, {:ok, [], [], starting_position}, fn
        {object, generation, body}, {:ok, pieces, entries, position}
        when is_integer(object) and object > 0 and is_integer(generation) and generation >= 0 ->
          case serialize_body(body) do
            {:ok, serialized} ->
              piece = [
                Integer.to_string(object),
                " ",
                Integer.to_string(generation),
                " obj\n",
                serialized,
                "\nendobj\n"
              ]

              {:cont,
               {:ok, [piece | pieces], [{object, generation, position} | entries],
                position + :erlang.iolist_size(piece)}}

            :error ->
              {:halt, error("incremental object cannot be serialized")}
          end

        _entry, _output ->
          {:halt, error("prepared incremental objects are malformed")}
      end)

    case result do
      {:ok, pieces, entries, xref_offset} ->
        pieces = Enum.reverse(pieces)

        with :ok <- unique_entries(entries),
             {:ok, trailer_id} <-
               IncrementalValidator.prepare_identifier(Map.get(trailer, "ID"), [pdf, pieces]),
             {:ok, trailer_io} <-
               incremental_trailer(trailer, previous_xref_offset, entries, trailer_id) do
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
        else
          {:error, _error} = write_error -> write_error
        end

      {:error, _error} = write_error ->
        write_error
    end
  end

  defp serialize_body(body) do
    case body do
      {:value, value} ->
        InfoCodec.serialize_value(value)

      {:stream, dictionary, data} when is_map(dictionary) and is_binary(data) ->
        dictionary = Map.put(dictionary, "Length", byte_size(data))

        case InfoCodec.serialize_value(dictionary) do
          {:ok, serialized} -> {:ok, [serialized, "\nstream\n", data, "\nendstream"]}
          :error -> :error
        end

      _ ->
        :error
    end
  end

  defp unique_entries(entries) do
    object_numbers = Enum.map(entries, fn {object, _generation, _offset} -> object end)

    case length(object_numbers) == length(Enum.uniq(object_numbers)) do
      true -> :ok
      false -> error("incremental object numbers must be unique")
    end
  end

  defp incremental_trailer(trailer, previous_xref_offset, entries, trailer_id) do
    maximum_object =
      entries
      |> Enum.map(fn {object, _generation, _offset} -> object end)
      |> Enum.max(fn -> 0 end)

    size = max(Map.get(trailer, "Size", 0), maximum_object + 1)

    updated =
      %{
        "Size" => size,
        "Root" => Map.get(trailer, "Root"),
        "Prev" => previous_xref_offset
      }
      |> copy_trailer_entry(trailer, "Info")
      |> maybe_put_identifier(trailer_id)

    case InfoCodec.serialize_value(updated) do
      {:ok, serialized} -> {:ok, serialized}
      :error -> error("incremental trailer cannot be serialized")
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
    Diagnostics.error(:incremental_write, :invalid_pdf_input, message, module: __MODULE__)
  end
end
