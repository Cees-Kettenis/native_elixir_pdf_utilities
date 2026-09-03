defmodule NativeElixirPdfUtilities.Outlines do
  @moduledoc """
  Reads, writes, and detects PDF document outlines.

  PDF viewers commonly call outline items bookmarks. Exact outline input uses
  one-based page numbers. Automatic detection reuses an existing outline when
  present and otherwise makes a best-effort guess from positioned text and
  relative font sizes.
  """

  alias NativeElixirPdfUtilities.Diagnostics
  alias NativeElixirPdfUtilities.Pdf.OutlineDetector
  alias NativeElixirPdfUtilities.Pdf.OutlineWriter
  alias NativeElixirPdfUtilities.Pdf.Reader
  alias NativeElixirPdfUtilities.Validators.OutlineValidator

  @typedoc "A supported PDF destination view."
  @type view :: OutlineValidator.view()
  @typedoc "A normalized outline item returned by this module."
  @type item :: OutlineValidator.item()
  @typedoc "A concise outline item accepted by `put/2`."
  @type item_input ::
          %{
            required(:title) => String.t(),
            optional(:page) => pos_integer() | nil,
            optional(:view) => view(),
            optional(:open) => boolean(),
            optional(:children) => [item_input()]
          }
          | {String.t(), pos_integer() | nil}
          | {String.t(), pos_integer() | nil, [item_input()]}

  @type error_reason ::
          :encrypted_pdf
          | :invalid_outlines
          | :invalid_pdf_input
          | :no_outline_source
          | :resource_limit_exceeded
          | :unsupported_pdf_feature

  @doc """
  Returns the active PDF outline as a normalized nested list.

  A PDF without an outline returns `{:ok, []}`. Page numbers are one-based.
  Unsupported actions are returned as destinationless items rather than being
  executed or copied.
  """
  @spec get(binary()) ::
          {:ok, [item()]} | {:error, {error_reason(), Diagnostics.diagnostic()}}
  def get(pdf) do
    with {:ok, context} <- Reader.read_validated(pdf),
         {:ok, items} <- OutlineValidator.extract(context) do
      {:ok, items}
    else
      {:error, error} -> owned_error(error, :get_outlines)
    end
  end

  @doc """
  Replaces the active PDF outline and returns an incrementally updated PDF.

  Items may use normalized maps, `{title, page}` tuples, or
  `{title, page, children}` tuples. Passing an empty list removes the active
  outline. A map may set `:view`, `:open`, and `:children`; `:page` may be nil
  for a destinationless grouping item.
  """
  @spec put(binary(), [item_input()]) ::
          {:ok, binary()} | {:error, {error_reason(), Diagnostics.diagnostic()}}
  def put(pdf, items) do
    with {:ok, context} <- Reader.read_validated(pdf),
         {:ok, items} <- OutlineValidator.normalize(items, length(context.pages)),
         :ok <- OutlineValidator.validate_incremental_capacity(context, items),
         {:ok, updated} <- OutlineWriter.write(context, items) do
      {:ok, updated}
    else
      {:error, error} -> owned_error(error, :put_outlines)
    end
  end

  @doc """
  Returns a proposed outline for a PDF.

  Existing outlines are returned unchanged. Otherwise the detector uses
  painted, extractable text and relative font sizes to guess headings. This is
  deliberately best-effort and may require caller adjustment before `put/2`.
  """
  @spec detect(binary()) ::
          {:ok, [item()]} | {:error, {error_reason(), Diagnostics.diagnostic()}}
  def detect(pdf) do
    case get(pdf) do
      {:ok, []} ->
        case OutlineDetector.detect(pdf) do
          {:ok, items} -> {:ok, items}
          {:error, error} -> owned_error(error, :detect_outlines)
        end

      {:ok, items} ->
        {:ok, items}

      {:error, error} ->
        owned_error(error, :detect_outlines)
    end
  end

  @doc """
  Detects an outline and writes it back to the PDF in one operation.

  This is equivalent to passing the result of `detect/1` to `put/2`.
  """
  @spec automatic(binary()) ::
          {:ok, binary()} | {:error, {error_reason(), Diagnostics.diagnostic()}}
  def automatic(pdf) do
    with {:ok, items} <- detect(pdf),
         {:ok, updated} <- put(pdf, items) do
      {:ok, updated}
    else
      {:error, error} -> owned_error(error, :automatic_outlines)
    end
  end

  defp owned_error({reason, diagnostic}, operation) do
    {:error,
     {reason,
      diagnostic
      |> Map.put(:operation, operation)
      |> Map.put(:module, __MODULE__)}}
  end
end
