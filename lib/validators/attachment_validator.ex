defmodule NativeElixirPdfUtilities.Validators.AttachmentValidator do
  @moduledoc false

  alias NativeElixirPdfUtilities.{Diagnostics, Limits}

  alias NativeElixirPdfUtilities.Validators.{
    InfoValidator,
    MimeValidator,
    PdfValidator,
    ResourceBudgetValidator
  }

  @doc false
  @spec prepare(term(), term()) :: {:ok, [map()]} | {:error, {atom(), map()}}
  def prepare(attachments, opts) do
    count =
      NativeElixirPdfUtilities.Validators.CollectionValidator.bounded_length(
        attachments,
        Limits.get(:max_entries)
      )

    cond do
      opts != [] ->
        error("attachment options must be an empty keyword list")

      count == :invalid ->
        error("attachments must be a list of maps")

      count == :limit_exceeded ->
        limit("attachment count exceeds max_entries")

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
    ResourceBudgetValidator.with_budget(fn -> inspect_files(context) end)
  end

  defp inspect_files(context) do
    document = context.document

    with {:ok, names_value} <-
           PdfValidator.optional_entry(document, context.catalog, "Names", %{}),
         {:ok, names} <- PdfValidator.dictionary(document, names_value),
         {:ok, entries} <- name_tree(document, Map.get(names, "EmbeddedFiles")) do
      Enum.reduce_while(entries, {:ok, [], 0}, fn {name, value}, {:ok, files, total} ->
        with {:ok, spec} <- PdfValidator.dictionary(document, value),
             {:ok, filename_value} <-
               PdfValidator.optional_entry(document, spec, "UF", Map.get(spec, "F")),
             {:ok, filename_value} <- PdfValidator.resolve(document, filename_value),
             {:ok, filename} <- text(filename_value),
             {:ok, description_value} <-
               PdfValidator.optional_entry(document, spec, "Desc", {:string, ""}),
             {:ok, description} <- text(description_value),
             {:ok, ef} <- PdfValidator.dictionary(document, Map.get(spec, "EF")),
             {:ok, stream} <-
               PdfValidator.validate_stream(document, Map.get(ef, "UF", Map.get(ef, "F"))),
             {:ok, params_value} <-
               PdfValidator.optional_entry(document, stream.dictionary, "Params", %{}),
             {:ok, params} <- PdfValidator.dictionary(document, params_value),
             {:ok, subtype} <-
               PdfValidator.resolve(document, Map.get(stream.dictionary, "Subtype")),
             {:ok, mime} <- existing_mime(subtype),
             {:ok, size} <- PdfValidator.resolve(document, Map.get(params, "Size")),
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

        length(existing.files) + length(attachments) > Limits.get(:max_entries) ->
          limit("attachment count exceeds max_entries")

        context.document.trailer["Size"] + 2 * length(attachments) > Limits.get(:max_entries) ->
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

          byte_size(filename) + byte_size(description) > Limits.get(:max_decoded_bytes) ->
            limit("attachment metadata exceeds max_decoded_bytes")

          byte_size(bytes) > Limits.get(:max_input_bytes) ->
            limit("attachment exceeds max_input_bytes")

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

  defp name_tree(document, root) do
    if is_nil(root) do
      {:ok, []}
    else
      state = %{nodes: 0, seen: MapSet.new(), names: MapSet.new(), entries: []}

      with {:ok, state} <- collect_node(document, root, 0, state),
           do: {:ok, Enum.reverse(state.entries)}
    end
  end

  defp collect_node(document, value, depth, state) do
    with :ok <- ResourceBudgetValidator.reserve(:max_work, 1, :limits) do
      cond do
        is_nil(value) ->
          {:ok, state}

        depth > Limits.get(:max_depth) or state.nodes >= Limits.get(:max_entries) ->
          limit("embedded file name tree exceeds traversal limits")

        match?({:ref, _}, value) and MapSet.member?(state.seen, value) ->
          error("embedded file name tree contains a cycle or shared node")

        true ->
          # Indirect references identify PDF objects. Inline dictionaries have no object identity.
          seen = if match?({:ref, _}, value), do: MapSet.put(state.seen, value), else: state.seen
          state = %{state | nodes: state.nodes + 1, seen: seen}

          with {:ok, node} <- PdfValidator.dictionary(document, value),
               {:ok, pairs} <- PdfValidator.resolve(document, Map.get(node, "Names", [])),
               {:ok, kids} <- PdfValidator.resolve(document, Map.get(node, "Kids", [])) do
            case {pairs, kids} do
              {pairs, []} -> collect_pairs(pairs, state)
              {[], kids} -> collect_children(document, kids, depth + 1, state)
              _ -> invalid_tree()
            end
          end
      end
    end
  end

  defp collect_children(document, kids, depth, state) do
    case kids do
      [] ->
        {:ok, state}

      [child | rest] ->
        with {:ok, state} <- collect_node(document, child, depth, state),
             do: collect_children(document, rest, depth, state)

      _ ->
        invalid_tree()
    end
  end

  defp collect_pairs(pairs, state) do
    case pairs do
      [] ->
        {:ok, state}

      [key, ref | rest] ->
        with true <- MapSet.size(state.names) < Limits.get(:max_entries),
             :ok <- ResourceBudgetValidator.reserve(:max_work, 1, :limits),
             {:ok, name} <- text(key),
             false <- MapSet.member?(state.names, name) do
          collect_pairs(rest, %{
            state
            | names: MapSet.put(state.names, name),
              entries: [{name, ref} | state.entries]
          })
        else
          false -> limit("attachment count exceeds max_entries")
          true -> error("embedded file names must be unique")
          failure -> failure
        end

      _ ->
        invalid_tree()
    end
  end

  defp invalid_tree, do: error("embedded file name tree requires Names pairs or Kids")

  defp text(value) do
    case value do
      {kind, bytes} when kind in [:string, :hex] ->
        case InfoValidator.decode_metadata_text(bytes) do
          :error -> error("attachment text has invalid encoding")
          result -> result
        end

      _ ->
        error("attachment name and description must be PDF strings")
    end
  end

  defp existing_mime(value) do
    case value do
      nil ->
        {:ok, nil}

      {:name, mime} ->
        with {:ok, mime} <- MimeValidator.validate(mime),
             :ok <- ResourceBudgetValidator.reserve(:max_decoded_bytes, byte_size(mime), :limits),
             do: {:ok, mime}

      _ ->
        error("embedded file Subtype must be a MIME name")
    end
  end

  defp byte_budget(total) do
    if total <= Limits.get(:max_input_bytes),
      do: :ok,
      else: limit("attachments exceed max_input_bytes")
  end

  defp error(message), do: Diagnostics.error(:attachments, :invalid_attachment, message)
  defp limit(message), do: Diagnostics.error(:limits, :resource_limit_exceeded, message)
end
