defmodule NativeElixirPdfUtilities.Info do
  @moduledoc """
  Reads PDF document information and page geometry and updates common metadata.

  Information updates append an incremental revision. The existing PDF bytes
  and unspecified information dictionary entries remain unchanged. XMP
  metadata is outside this API and is neither read nor updated.
  """

  alias NativeElixirPdfUtilities.Diagnostics
  alias NativeElixirPdfUtilities.Pdf.InfoWriter
  alias NativeElixirPdfUtilities.Pdf.Reader
  alias NativeElixirPdfUtilities.Validators.InfoValidator

  @typedoc "Common fields from the active PDF information dictionary."
  @type info :: InfoValidator.info()

  @typedoc """
  One effective page size.

  Width and height are PDF points. MediaBox coordinates remain in the page's
  default user-space units before applying its UserUnit scale.
  """
  @type page_size :: InfoValidator.page_size()

  @type error_reason :: Reader.error_reason()

  @doc """
  Returns common fields from the active PDF information dictionary.

  Missing fields are returned as `nil`. PDF dates are validated and converted
  to `NaiveDateTime`; timezone suffixes are validated while the written
  wall-clock value is retained.
  """
  @spec get(binary()) ::
          {:ok, info()} | {:error, {error_reason(), Diagnostics.diagnostic()}}
  def get(pdf) do
    with {:ok, context} <- read_context(pdf, :get_info),
         {:ok, info} <- InfoValidator.prepare_info(context) |> own_error(:get_info) do
      {:ok, info}
    end
  end

  @doc """
  Applies a patch to common PDF information fields and returns an updated PDF.

  Omitted fields remain unchanged and `nil` removes a field. Text values must
  be valid UTF-8. Dates accept `Date`, `NaiveDateTime`, `DateTime`, ISO 8601
  strings, and valid PDF date strings.
  """
  @spec put(binary(), map() | keyword()) ::
          {:ok, binary()} | {:error, {error_reason(), Diagnostics.diagnostic()}}
  def put(pdf, fields) do
    with {:ok, patch} <- InfoValidator.prepare_patch(fields) |> own_error(:put_info),
         {:ok, context} <- read_context(pdf, :put_info),
         {:ok, prepared} <- InfoValidator.prepare_write(context, patch) |> own_error(:put_info) do
      case map_size(patch) do
        0 -> {:ok, pdf}
        _ -> InfoWriter.write(context, prepared.dictionary) |> own_error(:put_info)
      end
    end
  end

  @doc """
  Returns the validated number of pages in a PDF.
  """
  @spec page_count(binary()) ::
          {:ok, non_neg_integer()} | {:error, {error_reason(), Diagnostics.diagnostic()}}
  def page_count(pdf) do
    with {:ok, context} <- read_context(pdf, :page_count) do
      {:ok, length(context.pages)}
    end
  end

  @doc """
  Returns each page's effective MediaBox size and normalized rotation.

  Width and height use PDF points, including the page's UserUnit scale, and
  reflect page rotation. MediaBox coordinates remain in default user-space
  units. Page numbers begin at one.
  """
  @spec page_sizes(binary()) ::
          {:ok, [page_size()]} | {:error, {error_reason(), Diagnostics.diagnostic()}}
  def page_sizes(pdf) do
    with {:ok, context} <- read_context(pdf, :page_sizes),
         {:ok, sizes} <- InfoValidator.prepare_page_sizes(context) |> own_error(:page_sizes) do
      {:ok, sizes}
    end
  end

  @doc """
  Returns whether the active PDF trailer declares encryption.

  This validates the header, cross-reference chain, and trailer without trying
  to decrypt or interpret encrypted document objects.
  """
  @spec encrypted?(binary()) ::
          {:ok, boolean()} | {:error, {error_reason(), Diagnostics.diagnostic()}}
  def encrypted?(pdf) do
    case Reader.probe(pdf) |> own_error(:encryption_status) do
      {:ok, probe} -> {:ok, probe.encrypted?}
      {:error, _} = probe_error -> probe_error
    end
  end

  defp read_context(pdf, operation) do
    case Reader.read_validated(pdf) |> own_error(operation) do
      {:ok, context} -> {:ok, context}
      {:error, _} = reader_error -> reader_error
    end
  end

  defp own_error(result, operation) do
    case result do
      {:error, {reason, diagnostic}} ->
        {:error,
         {reason,
          diagnostic
          |> Map.put(:operation, operation)
          |> Map.put(:module, __MODULE__)}}

      result ->
        result
    end
  end
end
