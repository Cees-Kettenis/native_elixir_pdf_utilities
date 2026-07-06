defmodule NativeElixirPdfUtilities.HtmlToPdf.Layout do
  @moduledoc """
  Layout engine for the native HTML-to-PDF renderer.

  Later milestones add block, inline, list, table, flexbox, and grid layout
  behavior behind this module.
  """

  @type layout_tree :: term()
  @type render_option :: NativeElixirPdfUtilities.HtmlToPdf.render_option()

  @doc """
  Converts a styled document tree into a layout tree.
  """
  @spec layout(term(), [render_option()]) :: {:ok, layout_tree()} | {:error, :not_implemented}
  def layout(styled_tree, opts \\ []) do
    case {styled_tree, opts} do
      {styled_tree, opts} when is_list(opts) ->
        case Application.get_env(:native_elixir_pdf_utilities, __MODULE__) do
          implementation when is_function(implementation, 2) ->
            case implementation.(styled_tree, opts) do
              {:ok, layout_tree} -> {:ok, layout_tree}
              _ -> {:error, :not_implemented}
            end

          _ ->
            {:error, :not_implemented}
        end
    end
  end
end
