defmodule NativeElixirPdfUtilities.HtmlToPdf.Pagination do
  @moduledoc """
  Pagination stage for the native HTML-to-PDF renderer.

  Milestone 2 wraps the one-page layout tree in a page list. Later milestones
  split layout output into deterministic PDF pages with page margins, automatic
  page breaks, and repeated table headers.
  """

  @type page :: %{size: {float(), float()}, boxes: [term()]}
  @type render_option :: NativeElixirPdfUtilities.HtmlToPdf.render_option()

  @doc """
  Splits a layout tree into PDF pages.
  """
  @spec paginate(term(), [render_option()]) :: {:ok, [page()]} | {:error, :invalid_layout}
  def paginate(layout_tree, opts \\ []) do
    case {layout_tree, opts} do
      {%{type: :layout, page_size: page_size, boxes: boxes}, opts}
      when is_list(boxes) and is_list(opts) ->
        {:ok, [%{size: page_size, boxes: boxes}]}

      _ ->
        {:error, :invalid_layout}
    end
  end
end
