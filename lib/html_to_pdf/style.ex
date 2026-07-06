defmodule NativeElixirPdfUtilities.HtmlToPdf.Style do
  @moduledoc """
  Style computation for the native HTML-to-PDF renderer.

  This module will apply element defaults, parsed CSS, inline declarations,
  inheritance, specificity, and source order as the feature is implemented.
  """

  @type styled_tree :: term()
  @type render_option :: NativeElixirPdfUtilities.HtmlToPdf.render_option()

  @doc """
  Computes styles for a parsed HTML document tree.
  """
  @spec compute(term(), [render_option()]) :: {:ok, styled_tree()} | {:error, :not_implemented}
  def compute(dom, opts \\ []) do
    case {dom, opts} do
      {dom, opts} when is_list(opts) ->
        case Application.get_env(:native_elixir_pdf_utilities, __MODULE__) do
          implementation when is_function(implementation, 2) ->
            case implementation.(dom, opts) do
              {:ok, styled_tree} -> {:ok, styled_tree}
              _ -> {:error, :not_implemented}
            end

          _ ->
            {:error, :not_implemented}
        end
    end
  end
end
