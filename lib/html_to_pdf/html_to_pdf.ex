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

  alias NativeElixirPdfUtilities.HtmlToPdf.CssParser
  alias NativeElixirPdfUtilities.HtmlToPdf.HtmlParser
  alias NativeElixirPdfUtilities.HtmlToPdf.Layout
  alias NativeElixirPdfUtilities.HtmlToPdf.Pagination
  alias NativeElixirPdfUtilities.HtmlToPdf.PdfWriter
  alias NativeElixirPdfUtilities.HtmlToPdf.Style
  alias NativeElixirPdfUtilities.Diagnostics

  @type page_size :: :a4 | :letter | {number(), number()}
  @type render_option ::
          {:page_size, page_size() | {number(), number()}}
          | {:margin, String.t() | number()}
          | {:base_url, String.t() | nil}
          | {:stylesheets, [String.t()]}
          | {:default_font, String.t()}
          | {:fonts, [map() | keyword() | {String.t(), String.t()}]}
  @type error_reason ::
          :invalid_document
          | :invalid_css
          | :invalid_html
          | :invalid_layout
          | :invalid_margin
          | :invalid_options
          | :invalid_page_size
          | :invalid_path
          | :invalid_pdf_input
          | :not_implemented
          | :unsupported_html
          | File.posix()
  @type error_detail :: Diagnostics.diagnostic()
  @type detailed_error_reason :: {error_reason(), error_detail()}

  @doc """
  Renders an HTML document to a PDF binary.

  Returns `{:ok, pdf_binary}` when rendering succeeds or
  `{:error, {reason, diagnostic}}` when
  parsing, styling, layout, pagination, or PDF writing cannot be completed.
  Rendering failures include a broad reason and diagnostic detail, for example
  `{:error, {:invalid_css, %{message: "...", line: 18, source: "..."}}}`.

  Supported options include `:page_size`, `:margin`, `:base_url`,
  `:stylesheets`, `:default_font`, and explicit TTF `:fonts`.

  `:page_size` accepts `:a4`, `:letter`, or a positive `{width, height}` tuple.
  Tuple values up to `20 x 20` are interpreted as inches for compatibility with
  ChromicPDF-style custom label sizes; larger tuples are interpreted as PDF
  points.
  """
  @spec render(String.t(), [render_option()]) ::
          {:ok, binary()} | {:error, detailed_error_reason()}
  def render(html, opts \\ []) do
    case do_render(html, opts) do
      {:ok, pdf_binary} ->
        {:ok, pdf_binary}

      {:error, {reason, detail}} ->
        {:error,
         {reason, Diagnostics.with_context(detail, operation: :render, module: __MODULE__)}}
    end
  end

  @doc """
  Reads an HTML file, renders it to PDF, and writes the PDF to `output_path`.

  Returns `:ok` after writing the output file or `{:error, {reason, diagnostic}}` if reading,
  rendering, or writing fails. Rendering options are the same as `render/2`.
  """
  @spec render_file(String.t(), String.t(), [render_option()]) ::
          :ok | {:error, detailed_error_reason()}
  def render_file(input_path, output_path, opts \\ []) do
    case {input_path, output_path} do
      {input_path, output_path} when is_binary(input_path) and is_binary(output_path) ->
        case File.read(input_path) do
          {:ok, html} ->
            case render(html, opts) do
              {:ok, pdf_binary} ->
                case File.write(output_path, pdf_binary) do
                  :ok ->
                    :ok

                  {:error, reason} ->
                    file_error(reason, :write, output_path)
                end

              {:error, {reason, detail}} ->
                {:error,
                 {reason,
                  Diagnostics.with_context(detail,
                    operation: :render_file,
                    module: __MODULE__,
                    source: input_path
                  )}}
            end

          {:error, reason} ->
            file_error(reason, :read, input_path)
        end

      _ ->
        Diagnostics.error(:file, :invalid_path, "input and output paths must be strings",
          operation: :render_file,
          module: __MODULE__
        )
    end
  end

  defp do_render(html, opts) do
    with {:ok, dom} <- HtmlParser.parse_detailed(html),
         {:ok, effective_opts} <- effective_render_options_detailed(dom, opts),
         {:ok, styled_tree} <- Style.compute_detailed(dom, effective_opts),
         {:ok, layout_tree} <- layout_document(styled_tree, effective_opts),
         {:ok, pages} <- Pagination.paginate(layout_tree, effective_opts),
         {:ok, pdf_binary} <- PdfWriter.render(pages, effective_opts) do
      {:ok, pdf_binary}
    end
  end

  defp layout_document(styled_tree, opts) do
    case apply(Layout, :layout, [styled_tree, opts]) do
      {:ok, layout_tree} ->
        {:ok, layout_tree}

      {:error, reason} ->
        Diagnostics.error(:layout, reason, layout_message(reason))
    end
  end

  defp effective_render_options_detailed(dom, opts) do
    case Keyword.keyword?(opts) do
      true ->
        with {:ok, page_options} <-
               dom |> stylesheet_sources() |> Enum.join("\n") |> CssParser.page_options() do
          {:ok, Keyword.merge(page_options, opts)}
        end

      false ->
        Diagnostics.error(:options, :invalid_options, "render options must be a keyword list")
    end
  end

  defp layout_message(reason) do
    "layout failed: #{reason}"
  end

  defp file_error(reason, operation, source) do
    Diagnostics.error(:file, reason, "file #{operation} failed: #{reason}",
      operation: operation,
      module: __MODULE__,
      source: source
    )
  end

  defp stylesheet_sources(node) do
    stylesheet_sources(node, false)
  end

  defp stylesheet_sources(node, in_style?) do
    case node do
      %{type: :document, children: children} ->
        Enum.flat_map(children, &stylesheet_sources(&1, in_style?))

      %{type: :element, tag: "style", children: children} ->
        Enum.flat_map(children, &stylesheet_sources(&1, true))

      %{type: :element, children: children} ->
        Enum.flat_map(children, &stylesheet_sources(&1, in_style?))

      %{type: :text, text: text} when is_binary(text) ->
        case in_style? do
          true -> [text]
          false -> []
        end
    end
  end
end
