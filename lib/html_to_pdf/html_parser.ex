defmodule NativeElixirPdfUtilities.HtmlToPdf.HtmlParser do
  @moduledoc """
  Strict HTML parser for the native HTML-to-PDF renderer.

  Later milestones fill this module in with the supported document-oriented HTML
  subset. The scaffold exists so the facade can expose the intended pipeline.
  """

  @type dom_tree :: term()

  @doc """
  Parses an HTML binary into a renderer DOM tree.
  """
  @spec parse(String.t()) :: {:ok, dom_tree()} | {:error, :not_implemented}
  def parse(html) do
    case html do
      html when is_binary(html) ->
        case Application.get_env(:native_elixir_pdf_utilities, __MODULE__) do
          implementation when is_function(implementation, 1) ->
            case implementation.(html) do
              {:ok, dom} -> {:ok, dom}
              _ -> {:error, :not_implemented}
            end

          _ ->
            {:error, :not_implemented}
        end
    end
  end
end
