defmodule NativeElixirPdfUtilities.Validators.AttachmentValidator do
  @moduledoc false

  alias NativeElixirPdfUtilities.{Diagnostics, Limits}
  alias NativeElixirPdfUtilities.Pdf.InfoCodec
  alias NativeElixirPdfUtilities.Validators.{MimeValidator, PdfValidator}

  @doc false
  @spec prepare(term(), term()) :: {:ok, [map()]} | {:error, {atom(), map()}}
  def prepare(attachments, opts) do
    cond do
      opts != [] ->
        error("attachment options must be an empty keyword list")

      not is_list(attachments) ->
        error("attachments must be a list of maps")

      length(attachments) > Limits.get(:max_pdf_attachments) ->
        limit("attachment count exceeds max_pdf_attachments")

      true ->
        Enum.reduce_while(attachments, {:ok, [], MapSet.new(), 0}, fn attachment,
                                                                      {:ok, acc, names, total} ->
          with {:ok, prepared} <- prepare_attachment(attachment),
               false <- MapSet.member?(names, prepared.filename),
               total <- total + byte_size(prepared.bytes),
               :ok <- byte_budget(total) do
            {:cont, {:ok, [prepared | acc], MapSet.put(names, prepared.filename), total}}
          else
            true -> {:halt, error("attachment filenames must be unique")}
            {:error, _} = failure -> {:halt, failure}
          end
        end)
        |> case do
          {:ok, prepared, _names, _total} -> {:ok, Enum.reverse(prepared)}
          failure -> failure
        end
    end
  end

  @doc false
  @spec inspect_document(PdfValidator.context()) :: {:ok, map()} | {:error, {atom(), map()}}
  def inspect_document(context) do
    document = context.document

    with {:ok, names} <- PdfValidator.dictionary(document, Map.get(context.catalog, "Names", %{})),
         {:ok, entries, _seen} <-
           name_tree(document, Map.get(names, "EmbeddedFiles"), MapSet.new(), 0),
         true <- length(entries) <= Limits.get(:max_pdf_attachments),
         true <- length(Enum.uniq_by(entries, &elem(&1, 0))) == length(entries) do
      Enum.reduce_while(entries, {:ok, [], 0}, fn {name, value}, {:ok, files, total} ->
        with {:ok, spec} <- PdfValidator.dictionary(document, value),
             {:ok, filename} <- text(Map.get(spec, "UF", Map.get(spec, "F"))),
             {:ok, description} <- text(Map.get(spec, "Desc", {:string, ""})),
             {:ok, ef} <- PdfValidator.dictionary(document, Map.get(spec, "EF")),
             {:ok, stream} <-
               PdfValidator.validate_stream(document, Map.get(ef, "UF", Map.get(ef, "F"))),
             {:ok, params} <-
               PdfValidator.dictionary(document, Map.get(stream.dictionary, "Params", %{})),
             {:ok, mime} <- existing_mime(Map.get(stream.dictionary, "Subtype")),
             size <- Map.get(params, "Size"),
             true <- is_nil(size) or (is_integer(size) and size >= 0),
             :ok <- byte_budget(total + byte_size(stream.stream)) do
          file = %{
            name: name,
            filename: filename,
            description: description,
            mime_type: mime,
            size: size,
            reference: value
          }

          {:cont, {:ok, [file | files], total + byte_size(stream.stream)}}
        else
          false -> {:halt, error("embedded file Size must be a non-negative integer")}
          {:error, _} = failure -> {:halt, failure}
        end
      end)
      |> case do
        {:ok, files, total} ->
          {:ok,
           %{names: names, entries: entries, files: Enum.reverse(files), stored_bytes: total}}

        failure ->
          failure
      end
    else
      false -> error("embedded file names must be unique and within max_pdf_attachments")
      {:error, _} = failure -> failure
    end
  end

  @doc false
  @spec prepare_write(PdfValidator.context(), map(), [map()]) :: :ok | {:error, {atom(), map()}}
  def prepare_write(context, existing, attachments) do
    existing_names = MapSet.new(Enum.flat_map(existing.files, &[&1.name, &1.filename]))

    with :ok <- NativeElixirPdfUtilities.Validators.ModificationValidator.validate(context),
         :ok <-
           byte_budget(
             existing.stored_bytes + Enum.reduce(attachments, 0, &(byte_size(&1.bytes) + &2))
           ) do
      cond do
        Enum.any?(attachments, &MapSet.member?(existing_names, &1.filename)) ->
          error("an attachment with this filename already exists")

        length(existing.files) + length(attachments) > Limits.get(:max_pdf_attachments) ->
          limit("attachment count exceeds max_pdf_attachments")

        context.document.trailer["Size"] + 2 * length(attachments) > Limits.get(:max_pdf_objects) ->
          limit("PDF object count cannot accommodate attachments")

        true ->
          :ok
      end
    end
  end

  defp prepare_attachment(input) do
    case input do
      %{filename: filename, bytes: bytes} when is_binary(filename) and is_binary(bytes) ->
        description = Map.get(input, :description, "")

        cond do
          Enum.any?(Map.keys(input), &(&1 not in [:filename, :bytes, :description, :mime_type])) ->
            error("unknown attachment metadata key")

          not String.valid?(filename) or filename in ["", ".", ".."] or
              String.contains?(filename, ["/", "\\", <<0>>]) ->
            error("filename must be valid UTF-8 without path separators or NUL")

          not is_binary(description) or not String.valid?(description) ->
            error("description must be valid UTF-8")

          byte_size(filename) + byte_size(description) > Limits.get(:max_pdf_info_value_bytes) ->
            limit("attachment metadata exceeds max_pdf_info_value_bytes")

          byte_size(bytes) > Limits.get(:max_pdf_attachment_bytes) ->
            limit("attachment exceeds max_pdf_attachment_bytes")

          true ->
            with {:ok, mime} <- MimeValidator.prepare(bytes, filename, Map.get(input, :mime_type)) do
              {:ok,
               %{filename: filename, bytes: bytes, description: description, mime_type: mime}}
            end
        end

      _ ->
        error("each attachment requires binary bytes and a filename")
    end
  end

  defp name_tree(document, value, seen, depth) do
    cond do
      is_nil(value) ->
        {:ok, [], seen}

      depth > Limits.get(:max_pdf_value_depth) or
          MapSet.size(seen) >= Limits.get(:max_pdf_name_tree_nodes) ->
        limit("embedded file name tree exceeds traversal limits")

      MapSet.member?(seen, value) ->
        error("embedded file name tree contains a cycle or shared node")

      true ->
        seen = MapSet.put(seen, value)

        with {:ok, node} <- PdfValidator.dictionary(document, value),
             {:ok, pairs} <- PdfValidator.resolve(document, Map.get(node, "Names", [])),
             {:ok, kids} <- PdfValidator.resolve(document, Map.get(node, "Kids", [])),
             true <- is_list(pairs) and rem(length(pairs), 2) == 0 and is_list(kids),
             true <- pairs == [] or kids == [],
             true <- div(length(pairs), 2) <= Limits.get(:max_pdf_attachments) do
          Enum.chunk_every(pairs, 2)
          |> Enum.reduce_while({:ok, []}, fn [key, ref], {:ok, entries} ->
            case text(key) do
              {:ok, key} -> {:cont, {:ok, [{key, ref} | entries]}}
              failure -> {:halt, failure}
            end
          end)
          |> case do
            {:ok, entries} ->
              Enum.reduce_while(kids, {:ok, Enum.reverse(entries), seen}, fn child,
                                                                             {:ok, acc, visited} ->
                case name_tree(document, child, visited, depth + 1) do
                  {:ok, more, visited} ->
                    if length(acc) + length(more) <= Limits.get(:max_pdf_attachments),
                      do: {:cont, {:ok, acc ++ more, visited}},
                      else: {:halt, limit("attachment count exceeds max_pdf_attachments")}

                  failure ->
                    {:halt, failure}
                end
              end)

            failure ->
              failure
          end
        else
          false ->
            error("embedded file name tree requires Names pairs or Kids within attachment limits")

          {:error, _} = failure ->
            failure
        end
    end
  end

  defp text(value) do
    case value do
      {kind, bytes} when kind in [:string, :hex] ->
        case InfoCodec.decode_text(bytes) do
          {:ok, text} -> {:ok, text}
          :error -> error("attachment text has invalid encoding")
        end

      _ ->
        error("attachment name and description must be PDF strings")
    end
  end

  defp existing_mime(value) do
    case value do
      nil -> {:ok, nil}
      {:name, mime} -> MimeValidator.validate(mime)
      _ -> error("embedded file Subtype must be a MIME name")
    end
  end

  defp byte_budget(total) do
    if total <= Limits.get(:max_pdf_attachment_total_bytes),
      do: :ok,
      else: limit("attachments exceed max_pdf_attachment_total_bytes")
  end

  defp error(message), do: Diagnostics.error(:attachments, :invalid_attachment, message)
  defp limit(message), do: Diagnostics.error(:limits, :resource_limit_exceeded, message)
end
