defmodule NativeElixirPdfUtilities.Attachments do
  @moduledoc """
  Embeds caller-approved files and lists existing PDF attachments.

  Files are stored as supplied. MIME detection is descriptive, not malware
  scanning or content sanitization. Callers must approve attachment content.
  No files are fetched, executed, extracted to disk, or automatically launched.
  """
  alias NativeElixirPdfUtilities.Diagnostics
  alias NativeElixirPdfUtilities.Pdf.{AttachmentWriter, Reader}
  alias NativeElixirPdfUtilities.Validators.AttachmentValidator

  @doc "Lists embedded filenames, descriptions, MIME types and declared byte sizes without extracting files."
  @spec list(binary()) :: {:ok, [map()]} | {:error, {atom(), Diagnostics.diagnostic()}}
  def list(pdf) do
    result =
      with {:ok, context} <- Reader.read_validated(pdf),
           {:ok, existing} <- AttachmentValidator.inspect_document(context) do
        {:ok,
         Enum.map(existing.files, &Map.take(&1, [:filename, :description, :mime_type, :size]))}
      end

    own_error(result, :list)
  end

  @doc """
  Embeds a list of maps containing `:filename` and binary `:bytes`.

  Optional `:description` and `:mime_type` describe the file. Recognized magic
  bytes take precedence over extension inference; clear conflicts return a
  diagnostic. Unknown types use `application/octet-stream`. Duplicate filenames
  are rejected. Options currently must be empty.
  """
  @spec embed(binary(), [map()], keyword()) ::
          {:ok, binary()} | {:error, {atom(), Diagnostics.diagnostic()}}
  def embed(pdf, attachments, opts \\ []) do
    result =
      with {:ok, attachments} <- AttachmentValidator.prepare(attachments, opts),
           {:ok, context} <- Reader.read_validated(pdf),
           {:ok, existing} <- AttachmentValidator.inspect_document(context),
           :ok <- AttachmentValidator.prepare_write(context, existing, attachments) do
        case attachments do
          [] -> {:ok, pdf}
          _ -> AttachmentWriter.write(context, existing, attachments)
        end
      end

    own_error(result, :embed)
  end

  defp own_error(result, operation) do
    case result do
      {:error, {reason, diagnostic}} ->
        {:error, {reason, Map.merge(diagnostic, %{module: __MODULE__, operation: operation})}}

      success ->
        success
    end
  end
end
