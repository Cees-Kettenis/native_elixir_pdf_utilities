defmodule NativeElixirPdfUtilities.Validators.IncrementalValidator do
  @moduledoc false

  alias NativeElixirPdfUtilities.Limits
  alias NativeElixirPdfUtilities.Diagnostics
  alias NativeElixirPdfUtilities.Validators.PdfValidator

  @doc false
  @spec validate_output_size(non_neg_integer()) ::
          :ok | {:error, {atom(), Diagnostics.diagnostic()}}
  def validate_output_size(bytes) do
    limit = min(Limits.get(:max_pdf_input_bytes), Limits.get(:max_rendered_pdf_bytes))

    if bytes <= limit do
      :ok
    else
      Diagnostics.error(
        :incremental_write,
        :resource_limit_exceeded,
        "incremental PDF output requires #{bytes} bytes, exceeding min(max_pdf_input_bytes, max_rendered_pdf_bytes) (#{limit})"
      )
    end
  end

  @doc false
  @spec prepare_identifier(PdfValidator.value(), iodata()) ::
          {:ok, [PdfValidator.value()] | nil}
          | {:error, {atom(), Diagnostics.diagnostic()}}
  def prepare_identifier(identifier, revision_content) do
    case identifier do
      nil ->
        {:ok, nil}

      [first, second] ->
        case {pdf_string_value?(first), pdf_string_value?(second)} do
          {true, true} ->
            digest = :crypto.hash(:sha256, revision_content) |> binary_part(0, 16)
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

  defp error(message) do
    Diagnostics.error(:incremental_write, :invalid_pdf_input, message, module: __MODULE__)
  end
end
