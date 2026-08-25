defmodule NativeElixirPdfUtilities.Pdf.InfoWriter do
  @moduledoc false

  alias NativeElixirPdfUtilities.Diagnostics
  alias NativeElixirPdfUtilities.Limits
  alias NativeElixirPdfUtilities.Pdf.InfoCodec
  alias NativeElixirPdfUtilities.Validators.PdfValidator

  @doc false
  @spec write(PdfValidator.context(), map()) ::
          {:ok, binary()} | {:error, {atom(), Diagnostics.diagnostic()}}
  def write(context, dictionary) do
    case context do
      %{
        document: %{
          binary: pdf,
          trailer: trailer,
          xref_offset: previous_xref_offset
        }
      }
      when is_binary(pdf) and is_map(trailer) and is_integer(previous_xref_offset) and
             is_map(dictionary) ->
        write_increment(pdf, trailer, previous_xref_offset, dictionary)

      _ ->
        error("prepared information write context is malformed")
    end
  end

  defp write_increment(pdf, trailer, previous_xref_offset, dictionary) do
    size = Map.get(trailer, "Size")

    cond do
      not is_integer(size) or size <= 0 ->
        error("active trailer Size is malformed")

      size > Limits.get(:max_pdf_objects) ->
        Diagnostics.error(
          :limits,
          :resource_limit_exceeded,
          "PDF object count cannot accommodate an information update"
        )

      true ->
        with {:ok, dictionary_io} <- InfoCodec.serialize_value(dictionary),
             {:ok, trailer_id} <- updated_identifier(Map.get(trailer, "ID"), pdf, dictionary_io),
             {:ok, root} <- active_root(Map.get(trailer, "Root")) do
          separator = trailing_separator(pdf)
          object_number = size
          object_offset = byte_size(pdf) + byte_size(separator)

          object = [
            Integer.to_string(object_number),
            " 0 obj\n",
            dictionary_io,
            "\nendobj\n"
          ]

          xref_offset = object_offset + :erlang.iolist_size(object)

          incremental_trailer =
            %{
              "Size" => size + 1,
              "Root" => root,
              "Info" => {:ref, {object_number, 0}},
              "Prev" => previous_xref_offset
            }
            |> maybe_put_identifier(trailer_id)

          {:ok, trailer_io} = InfoCodec.serialize_value(incremental_trailer)

          {:ok,
           IO.iodata_to_binary([
             pdf,
             separator,
             object,
             "xref\n",
             Integer.to_string(object_number),
             " 1\n",
             padded_offset(object_offset),
             " 00000 n \n",
             "trailer\n",
             trailer_io,
             "\nstartxref\n",
             Integer.to_string(xref_offset),
             "\n%%EOF\n"
           ])}
        else
          :error -> error("information dictionary cannot be serialized")
          {:error, _} = writer_error -> writer_error
        end
    end
  end

  defp active_root(value) do
    case value do
      {:ref, {object, generation}} = root
      when is_integer(object) and object >= 0 and is_integer(generation) and generation >= 0 ->
        {:ok, root}

      _ ->
        error("active trailer Root is malformed")
    end
  end

  defp updated_identifier(value, pdf, dictionary_io) do
    case value do
      nil ->
        {:ok, nil}

      [first, second] when tuple_size(first) == 2 and tuple_size(second) == 2 ->
        case {pdf_string_value?(first), pdf_string_value?(second)} do
          {true, true} ->
            digest = :crypto.hash(:sha256, [pdf, dictionary_io]) |> binary_part(0, 16)
            {:ok, [first, {:hex, digest}]}

          _ ->
            error("active trailer ID is malformed")
        end

      _ ->
        error("active trailer ID is malformed")
    end
  end

  defp pdf_string_value?(value) do
    case value do
      {kind, bytes} when kind in [:string, :hex] and is_binary(bytes) -> true
      _ -> false
    end
  end

  defp maybe_put_identifier(trailer, identifier) do
    case identifier do
      nil -> trailer
      identifier -> Map.put(trailer, "ID", identifier)
    end
  end

  defp trailing_separator(pdf) do
    case byte_size(pdf) do
      0 ->
        "\n"

      _size ->
        case :binary.last(pdf) in [?\n, ?\r] do
          true -> ""
          false -> "\n"
        end
    end
  end

  defp padded_offset(offset) do
    offset |> Integer.to_string() |> String.pad_leading(10, "0")
  end

  defp error(message) do
    Diagnostics.error(:incremental_write, :invalid_pdf_input, message)
  end
end
