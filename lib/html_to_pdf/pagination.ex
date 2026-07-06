defmodule NativeElixirPdfUtilities.HtmlToPdf.Pagination do
  @moduledoc """
  Pagination stage for the native HTML-to-PDF renderer.

  Later milestones split layout output into deterministic PDF pages with page
  margins, automatic page breaks, and repeated table headers.
  """

  @type page :: term()
  @type render_option :: NativeElixirPdfUtilities.HtmlToPdf.render_option()

  @doc """
  Splits a layout tree into PDF pages.
  """
  @spec paginate(term(), [render_option()]) :: {:ok, [page()]} | {:error, :not_implemented}
  def paginate(layout_tree, opts \\ []) do
    case {layout_tree, opts} do
      {layout_tree, opts} when is_list(opts) ->
        case Application.get_env(:native_elixir_pdf_utilities, __MODULE__) do
          implementation when is_function(implementation, 2) ->
            case implementation.(layout_tree, opts) do
              {:ok, pages} when is_list(pages) -> {:ok, pages}
              _ -> {:error, :not_implemented}
            end

          _ ->
            {:error, :not_implemented}
        end
    end
  end
end
