defmodule NativeElixirPdfUtilities.HtmlToPdf.CssParser do
  @moduledoc """
  CSS parser for the native HTML-to-PDF renderer.

  Later milestones add a strict CSS subset with predictable cascade behavior.
  """

  @type stylesheet :: term()

  @doc """
  Parses CSS source into a renderer stylesheet representation.
  """
  @spec parse(String.t()) :: {:ok, stylesheet()} | {:error, :not_implemented}
  def parse(css) do
    case css do
      css when is_binary(css) ->
        case Application.get_env(:native_elixir_pdf_utilities, __MODULE__) do
          implementation when is_function(implementation, 1) ->
            case implementation.(css) do
              {:ok, stylesheet} -> {:ok, stylesheet}
              _ -> {:error, :not_implemented}
            end

          _ ->
            {:error, :not_implemented}
        end
    end
  end
end
