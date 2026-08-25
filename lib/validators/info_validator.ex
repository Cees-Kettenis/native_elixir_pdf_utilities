defmodule NativeElixirPdfUtilities.Validators.InfoValidator do
  @moduledoc """
  Validation and normalization for PDF information inspection and updates.

  The validator consumes the shared reader context, resolves the active
  information dictionary, validates supported field values, prepares effective
  page geometry, and produces metadata patches that writers can serialize
  without reinterpreting caller input.
  """

  alias NativeElixirPdfUtilities.Diagnostics
  alias NativeElixirPdfUtilities.Limits
  alias NativeElixirPdfUtilities.Pdf.InfoCodec
  alias NativeElixirPdfUtilities.Validators.PdfValidator

  @field_keys %{
    title: "Title",
    author: "Author",
    subject: "Subject",
    keywords: "Keywords",
    producer: "Producer",
    creation_date: "CreationDate",
    modification_date: "ModDate"
  }

  @text_fields [:title, :author, :subject, :keywords, :producer]
  @date_fields [:creation_date, :modification_date]

  @typedoc "Normalized common PDF information fields."
  @type info :: %{
          required(:title) => String.t() | nil,
          required(:author) => String.t() | nil,
          required(:subject) => String.t() | nil,
          required(:keywords) => String.t() | nil,
          required(:producer) => String.t() | nil,
          required(:creation_date) => NaiveDateTime.t() | nil,
          required(:modification_date) => NaiveDateTime.t() | nil
        }

  @typedoc "One effective page size in PDF points."
  @type page_size :: %{
          required(:page_number) => pos_integer(),
          required(:width) => float(),
          required(:height) => float(),
          required(:unit) => :point,
          required(:rotation) => 0 | 90 | 180 | 270,
          required(:media_box) => %{
            required(:left) => float(),
            required(:bottom) => float(),
            required(:right) => float(),
            required(:top) => float()
          }
        }

  @typedoc "Validated caller patch keyed by PDF information dictionary names."
  @type patch :: %{optional(binary()) => PdfValidator.value() | :remove}

  @doc false
  @spec validate_incremental_object_capacity(term(), term()) ::
          :ok | {:error, {atom(), Diagnostics.diagnostic()}}
  def validate_incremental_object_capacity(trailer, xref) do
    case {trailer, xref} do
      {%{"Size" => size}, xref} when is_map(xref) ->
        max_pdf_objects = Limits.get(:max_pdf_objects)

        cond do
          not is_integer(size) or size <= 0 ->
            error(:incremental_write, "active trailer Size is malformed")

          size > max_pdf_objects or map_size(xref) >= max_pdf_objects ->
            Diagnostics.error(
              :limits,
              :resource_limit_exceeded,
              "PDF object count cannot accommodate an information update"
            )

          true ->
            :ok
        end

      _ ->
        error(:incremental_write, "active cross-reference state is malformed")
    end
  end

  @doc """
  Reads and validates the common fields in a shared PDF context.
  """
  @spec prepare_info(PdfValidator.context()) ::
          {:ok, info()} | {:error, {atom(), Diagnostics.diagnostic()}}
  def prepare_info(context) do
    with {:ok, document, dictionary} <- information_dictionary(context) do
      @field_keys
      |> Enum.reduce_while({:ok, %{}}, fn {field, key}, {:ok, info} ->
        case normalized_existing_value(document, field, Map.get(dictionary, key)) do
          {:ok, value} -> {:cont, {:ok, Map.put(info, field, value)}}
          {:error, _} = info_error -> {:halt, info_error}
        end
      end)
    end
  end

  @doc """
  Resolves and validates effective page sizes from a shared PDF context.
  """
  @spec prepare_page_sizes(PdfValidator.context()) ::
          {:ok, [page_size()]} | {:error, {atom(), Diagnostics.diagnostic()}}
  def prepare_page_sizes(context) do
    case context do
      %{document: document, pages: pages} when is_map(document) and is_list(pages) ->
        pages
        |> Enum.with_index(1)
        |> Enum.reduce_while({:ok, []}, fn {page, page_number}, {:ok, sizes} ->
          case prepare_page_size(document, page, page_number) do
            {:ok, size} -> {:cont, {:ok, [size | sizes]}}
            {:error, _} = page_error -> {:halt, page_error}
          end
        end)
        |> case do
          {:ok, sizes} -> {:ok, Enum.reverse(sizes)}
          {:error, _} = page_error -> page_error
        end

      _ ->
        error(:validation, "shared PDF validation context is malformed")
    end
  end

  @doc """
  Validates and normalizes a caller-provided information patch.
  """
  @spec prepare_patch(term()) ::
          {:ok, patch()} | {:error, {atom(), Diagnostics.diagnostic()}}
  def prepare_patch(input) do
    case normalized_input(input) do
      {:ok, input} -> normalize_patch(input)
      :error -> error(:input, "Info.put/2 expects a map or keyword list of supported fields")
    end
  end

  @doc """
  Applies a prepared patch to a validated existing information dictionary.
  """
  @spec prepare_write(PdfValidator.context(), patch()) ::
          {:ok, map()} | {:error, {atom(), Diagnostics.diagnostic()}}
  def prepare_write(context, patch) do
    with {:ok, document, dictionary} <- information_dictionary(context),
         {:ok, _info} <- prepare_info(context),
         true <- is_map(patch) do
      updated =
        Enum.reduce(patch, dictionary, fn {key, value}, updated ->
          case value do
            :remove -> Map.delete(updated, key)
            value -> Map.put(updated, key, value)
          end
        end)

      case InfoCodec.serialize_value(updated) do
        {:ok, _iodata} -> {:ok, %{document: document, dictionary: updated}}
        :error -> error(:info, "information dictionary contains an unsupported value")
      end
    else
      false -> error(:input, "prepared information patch is malformed")
      {:error, _} = write_error -> write_error
    end
  end

  @doc false
  @spec normalize_new_metadata(term()) ::
          {:ok, %{optional(atom()) => binary()}}
          | {:error, {atom(), Diagnostics.diagnostic()}}
  def normalize_new_metadata(input) do
    with {:ok, patch} <- prepare_patch(input),
         false <- Enum.any?(patch, fn {_key, value} -> value == :remove end) do
      reverse_keys = Map.new(@field_keys, fn {field, key} -> {key, field} end)

      patch
      |> Enum.reduce_while({:ok, %{}}, fn {key, value}, {:ok, metadata} ->
        {_kind, bytes} = value
        {:ok, value} = InfoCodec.decode_text(bytes)
        {:cont, {:ok, Map.put(metadata, Map.fetch!(reverse_keys, key), value)}}
      end)
    else
      _ -> error(:info, "PDF metadata must use supported fields and value types")
    end
  end

  defp information_dictionary(context) do
    case context do
      %{document: %{trailer: trailer} = document} when is_map(trailer) ->
        case Map.get(trailer, "Info") do
          nil ->
            {:ok, document, %{}}

          value ->
            case PdfValidator.dictionary(document, value) do
              {:ok, dictionary} -> {:ok, document, dictionary}
              {:error, _reason} -> error(:info, "PDF Info entry does not resolve to a dictionary")
            end
        end

      _ ->
        error(:validation, "shared PDF validation context is malformed")
    end
  end

  defp normalized_existing_value(document, field, value) do
    case value do
      nil ->
        {:ok, nil}

      value ->
        case PdfValidator.resolve(document, value) do
          {:ok, resolved} -> decode_existing_value(field, resolved)
          {:error, _reason} -> error(:info, "PDF information field #{field} cannot be resolved")
        end
    end
  end

  defp decode_existing_value(field, value) do
    case {field, value} do
      {field, {kind, bytes}} when field in @text_fields and kind in [:string, :hex] ->
        case InfoCodec.decode_text(bytes) do
          {:ok, text} -> {:ok, text}
          :error -> error(:info, "PDF information field #{field} has invalid text encoding")
        end

      {field, {kind, bytes}} when field in @date_fields and kind in [:string, :hex] ->
        with {:ok, date_text} <- InfoCodec.decode_text(bytes),
             {:ok, date_time, _date} <- InfoCodec.parse_date(date_text) do
          {:ok, date_time}
        else
          _ -> error(:info, "PDF information field #{field} has an invalid PDF date")
        end

      _ ->
        error(:info, "PDF information field #{field} has an unsupported value")
    end
  end

  defp prepare_page_size(document, page, page_number) do
    case page do
      %{media_box: media_box, rotate: rotate} ->
        with {:ok, [left, bottom, right, top]} <-
               PdfValidator.number_array(document, media_box, 4),
             true <- right > left and top > bottom,
             {:ok, rotation} <- normalized_rotation(document, rotate) do
          unrotated_width = (right - left) * 1.0
          unrotated_height = (top - bottom) * 1.0

          {width, height} =
            case rotation in [90, 270] do
              true -> {unrotated_height, unrotated_width}
              false -> {unrotated_width, unrotated_height}
            end

          {:ok,
           %{
             page_number: page_number,
             width: width,
             height: height,
             unit: :point,
             rotation: rotation,
             media_box: %{
               left: left * 1.0,
               bottom: bottom * 1.0,
               right: right * 1.0,
               top: top * 1.0
             }
           }}
        else
          _ -> page_error(page_number, "has a malformed effective MediaBox or Rotate value")
        end

      _ ->
        page_error(page_number, "context is malformed")
    end
  end

  defp normalized_rotation(document, value) do
    case PdfValidator.resolve(document, value || 0) do
      {:ok, rotation} when is_integer(rotation) and rem(rotation, 90) == 0 ->
        {:ok, Integer.mod(rotation, 360)}

      _ ->
        :error
    end
  end

  defp normalized_input(input) do
    case input do
      input when is_map(input) ->
        {:ok, input}

      input when is_list(input) ->
        case Keyword.keyword?(input) and
               length(input) == length(Keyword.keys(input) |> Enum.uniq()) do
          true -> {:ok, Map.new(input)}
          false -> :error
        end

      _ ->
        :error
    end
  end

  defp normalize_patch(input) do
    case Enum.all?(Map.keys(input), &Map.has_key?(@field_keys, &1)) do
      true ->
        input
        |> Enum.reduce_while({:ok, %{}, 0}, fn {field, value}, {:ok, patch, total_bytes} ->
          case normalized_patch_value(field, value) do
            {:ok, normalized, value_bytes} ->
              total_bytes = total_bytes + value_bytes

              cond do
                value_bytes > Limits.get(:max_pdf_info_value_bytes) ->
                  {:halt, limit_error("PDF information field #{field} exceeds the byte limit")}

                total_bytes > Limits.get(:max_pdf_info_total_bytes) ->
                  {:halt, limit_error("PDF information values exceed the aggregate byte limit")}

                true ->
                  {:cont,
                   {:ok, Map.put(patch, Map.fetch!(@field_keys, field), normalized), total_bytes}}
              end

            :error ->
              {:halt, error(:info, "PDF information field #{field} has an invalid value")}
          end
        end)
        |> case do
          {:ok, patch, _bytes} -> {:ok, patch}
          {:error, _} = patch_error -> patch_error
        end

      false ->
        error(:input, "Info.put/2 received an unknown information field")
    end
  end

  defp normalized_patch_value(field, value) do
    case value do
      nil ->
        {:ok, :remove, 0}

      value ->
        case field in @text_fields do
          true -> normalized_text_value(field, value)
          false -> normalized_date_value(value)
        end
    end
  end

  defp normalized_text_value(field, value) do
    case {field, value} do
      {:keywords, values} when is_list(values) ->
        case Enum.all?(values, &(is_binary(&1) and String.valid?(&1))) do
          true ->
            joined = Enum.join(values, ", ")
            {:ok, InfoCodec.encode_text(joined), byte_size(joined)}

          false ->
            :error
        end

      {_field, value} when is_binary(value) ->
        case String.valid?(value) do
          true -> {:ok, InfoCodec.encode_text(value), byte_size(value)}
          false -> :error
        end

      _ ->
        :error
    end
  end

  defp normalized_date_value(value) do
    case InfoCodec.normalize_date(value) do
      {:ok, value} -> {:ok, InfoCodec.encode_text(value), byte_size(value)}
      :error -> :error
    end
  end

  defp page_error(page_number, message) do
    Diagnostics.error(
      :page_tree,
      :invalid_pdf_input,
      "page #{page_number} #{message}",
      source: "page #{page_number}"
    )
  end

  defp limit_error(message) do
    Diagnostics.error(:limits, :resource_limit_exceeded, message)
  end

  defp error(stage, message) do
    Diagnostics.error(stage, :invalid_pdf_input, message)
  end
end
