defmodule NativeElixirPdfUtilities.HtmlToPdf do
  @moduledoc """
  Public facade for native HTML/CSS to PDF rendering.

  The renderer is intentionally structured as a small pipeline:

    * parse HTML into a document tree
    * compute styles
    * lay out the styled tree
    * paginate layout boxes
    * write PDF bytes

  The supported surface is a strict, document-oriented HTML/CSS subset. Invalid
  or unsupported input returns an error instead of falling back to browser-like
  guessing. See the README support matrix for the current element, CSS, layout,
  image, and font support.
  """

  alias NativeElixirPdfUtilities.HtmlToPdf.HtmlParser
  alias NativeElixirPdfUtilities.HtmlToPdf.Layout
  alias NativeElixirPdfUtilities.HtmlToPdf.Pagination
  alias NativeElixirPdfUtilities.HtmlToPdf.PdfWriter
  alias NativeElixirPdfUtilities.HtmlToPdf.Style

  @type page_size :: :a4 | atom()
  @type render_option ::
          {:page_size, page_size() | {number(), number()}}
          | {:margin, String.t() | number()}
          | {:base_url, String.t() | nil}
          | {:stylesheets, [String.t()]}
          | {:default_font, String.t()}
          | {:fonts, [map() | keyword() | {String.t(), String.t()}]}
  @type error_reason ::
          :invalid_document
          | :invalid_html
          | :invalid_layout
          | :invalid_margin
          | :invalid_page_size
          | :invalid_path
          | :invalid_pdf_input
          | :not_implemented
          | :unsupported_html
          | File.posix()

  @doc """
  Renders an HTML document to a PDF binary.

  Returns `{:ok, pdf_binary}` when rendering succeeds or `{:error, reason}` when
  parsing, styling, layout, pagination, or PDF writing cannot be completed.

  Supported options include `:page_size`, `:margin`, `:base_url`,
  `:stylesheets`, `:default_font`, and explicit TTF `:fonts`.
  """
  @spec render(String.t(), [render_option()]) :: {:ok, binary()} | {:error, error_reason()}
  def render(html, opts \\ []) do
    with {:ok, dom} <- HtmlParser.parse(html),
         {:ok, styled_tree} <- Style.compute(dom, opts),
         {:ok, layout_tree} <- layout_document(styled_tree, opts),
         {:ok, pages} <- Pagination.paginate(layout_tree, opts),
         {:ok, pdf_binary} <- PdfWriter.render(pages, opts) do
      {:ok, pdf_binary}
    end
  end

  @doc """
  Reads an HTML file, renders it to PDF, and writes the PDF to `output_path`.

  Returns `:ok` after writing the output file or `{:error, reason}` if reading,
  rendering, or writing fails. Rendering options are the same as `render/2`.
  """
  @spec render_file(String.t(), String.t(), [render_option()]) ::
          :ok | {:error, error_reason()}
  def render_file(input_path, output_path, opts \\ []) do
    case {input_path, output_path} do
      {input_path, output_path} when is_binary(input_path) and is_binary(output_path) ->
        with {:ok, html} <- File.read(input_path),
             {:ok, pdf_binary} <- render(html, opts),
             :ok <- File.write(output_path, pdf_binary) do
          :ok
        end

      _ ->
        {:error, :invalid_path}
    end
  end

  @spec layout_document(term(), [render_option()]) ::
          {:ok, term()} | {:error, :invalid_layout | :invalid_margin | :invalid_page_size}
  defp layout_document(styled_tree, opts) do
    apply(Layout, :layout, [styled_tree, opts])
  end
end
