defmodule NativeElixirPdfUtilities.Transform do
  @moduledoc """
  Rebuilds PDFs with selected, reordered, deleted, or rotated pages.

  Page numbers are one-based and follow the source document's page-tree order.
  Page ranges are inclusive, ascending, and must use a step of one. Transform
  selections reject duplicate pages, including duplicates produced by
  overlapping selectors. Every successful operation writes a new catalog, page
  tree, cross-reference table, and trailer.

  Rebuilding omits unselected page objects and resources used only by those
  pages. It is not secure redaction because retained pages can share resources
  with removed pages. Internal link annotations that target removed pages are
  omitted; links to retained pages and external URI links remain.
  """

  alias NativeElixirPdfUtilities.Diagnostics
  alias NativeElixirPdfUtilities.Pdf.AssemblyWriter
  alias NativeElixirPdfUtilities.Pdf.Reader
  alias NativeElixirPdfUtilities.Validators.TransformValidator

  @type page_selector :: pos_integer() | Range.t()
  @type error_reason ::
          :encrypted_pdf
          | :invalid_options
          | :invalid_page_selection
          | :invalid_pdf_input
          | :invalid_rotation
          | :page_out_of_bounds
          | :resource_limit_exceeded
          | :unsupported_pdf_feature

  @doc """
  Rebuilds a PDF with the selected pages in the requested order.

  Selection entries may be positive page numbers or inclusive, ascending,
  unit-step ranges. Duplicate pages and empty selections are rejected.
  """
  @spec pick_pages(binary(), [page_selector()]) ::
          {:ok, binary()} | {:error, {error_reason(), Diagnostics.diagnostic()}}
  def pick_pages(pdf, selection) do
    with {:ok, context} <- Reader.read_validated(pdf),
         {:ok, input} <- TransformValidator.prepare_pick(context, selection) do
      AssemblyWriter.write([input])
    else
      {:error, error} -> owned_error(error, :pick_pages)
    end
  end

  @doc """
  Rebuilds a PDF without the selected pages.

  Page numbers refer to the original document and are evaluated together.
  Ranges must be inclusive, ascending, and use a step of one. Deleting every
  page is rejected. An empty selection rebuilds the document without removing
  any pages.
  """
  @spec delete_pages(binary(), [page_selector()]) ::
          {:ok, binary()} | {:error, {error_reason(), Diagnostics.diagnostic()}}
  def delete_pages(pdf, selection) do
    with {:ok, context} <- Reader.read_validated(pdf),
         {:ok, input} <- TransformValidator.prepare_delete(context, selection) do
      AssemblyWriter.write([input])
    else
      {:error, error} -> owned_error(error, :delete_pages)
    end
  end

  @doc """
  Rebuilds a PDF after rotating selected pages clockwise.

  Rotation must be an integer multiple of 90 degrees and is added to each
  page's effective existing rotation. The `:pages` option accepts `:all`, which
  is the default, or a list of page numbers and inclusive, ascending, unit-step
  ranges.
  """
  @spec rotate_pages(binary(), integer(), pages: :all | [page_selector()]) ::
          {:ok, binary()} | {:error, {error_reason(), Diagnostics.diagnostic()}}
  def rotate_pages(pdf, degrees, options \\ []) do
    with {:ok, context} <- Reader.read_validated(pdf),
         {:ok, input} <- TransformValidator.prepare_rotation(context, degrees, options) do
      AssemblyWriter.write([input])
    else
      {:error, error} -> owned_error(error, :rotate_pages)
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
