defmodule NativeElixirPdfUtilities.HtmlToPdf.PdfWriter do
  @moduledoc """
  PDF writer stage for the native HTML-to-PDF renderer.

  This module will become the low-level PDF byte writer used by the HTML
  renderer. The milestone 1 scaffold only defines the rendering boundary.
  """

  @type page :: NativeElixirPdfUtilities.HtmlToPdf.Pagination.page()
  @type render_option :: NativeElixirPdfUtilities.HtmlToPdf.render_option()

  @doc """
  Renders paginated drawing instructions to a PDF binary.
  """
  @spec render([page()], [render_option()]) :: {:ok, binary()} | {:error, :not_implemented}
  def render(pages, opts \\ []) do
    case {pages, opts} do
      {pages, opts} when is_list(pages) and is_list(opts) ->
        case Application.get_env(:native_elixir_pdf_utilities, __MODULE__) do
          implementation when is_function(implementation, 2) ->
            case implementation.(pages, opts) do
              {:ok, pdf_binary} when is_binary(pdf_binary) -> {:ok, pdf_binary}
              _ -> {:error, :not_implemented}
            end

          _ ->
            {:error, :not_implemented}
        end
    end
  end
end
