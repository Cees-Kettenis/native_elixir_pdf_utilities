defmodule NativeElixirPdfUtilities.HtmlToPdf.Style do
  @moduledoc """
  Style computation for the native HTML-to-PDF renderer.

  This module applies the paragraph defaults used by the milestone 2 vertical
  slice. Later milestones add CSS parsing, inheritance, and cascade behavior.
  """

  @type text_node :: %{type: :text, text: String.t()}
  @type styled_element :: %{
          type: :element,
          tag: String.t(),
          style: map(),
          children: [text_node()]
        }
  @type styled_tree :: %{type: :document, children: [styled_element()]}
  @type render_option :: NativeElixirPdfUtilities.HtmlToPdf.render_option()

  @doc """
  Computes styles for a parsed HTML document tree.
  """
  @spec compute(term(), [render_option()]) :: {:ok, styled_tree()} | {:error, :invalid_document}
  def compute(dom, opts \\ []) do
    case {dom, opts} do
      {%{type: :document, children: [%{type: :element, tag: "p", children: children}]}, opts}
      when is_list(opts) and is_list(children) ->
        font = Keyword.get(opts, :default_font, "Helvetica")

        {:ok,
         %{
           type: :document,
           children: [
             %{
               type: :element,
               tag: "p",
               style: %{
                 display: :block,
                 color: {0, 0, 0},
                 font_family: font,
                 font_size: 12.0,
                 line_height: 14.4,
                 margin_after: 12.0
               },
               children: children
             }
           ]
         }}

      _ ->
        {:error, :invalid_document}
    end
  end
end
