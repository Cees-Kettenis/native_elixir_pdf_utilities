defmodule NativeElixirPdfUtilities.HtmlToPdf do
  @moduledoc """
  Public facade for native HTML/CSS to PDF rendering.

  The renderer is intentionally structured as a small pipeline:

    * parse HTML into a document tree
    * compute styles
    * resolve every text grapheme to an available font
    * lay out the styled tree
    * paginate layout boxes
    * write PDF bytes

  The supported surface is a strict, document-oriented HTML/CSS subset.
  Malformed structure and unsupported features return errors instead of using
  browser-like guessing. Unsupported text graphemes are visibly replaced by
  default. See the README support matrix for the current element, CSS, layout,
  image, and font support.
  """

  alias NativeElixirPdfUtilities.HtmlToPdf.CssParser
  alias NativeElixirPdfUtilities.HtmlToPdf.Font
  alias NativeElixirPdfUtilities.HtmlToPdf.FontFallback
  alias NativeElixirPdfUtilities.HtmlToPdf.HtmlParser
  alias NativeElixirPdfUtilities.HtmlToPdf.Layout
  alias NativeElixirPdfUtilities.HtmlToPdf.PageFurniture
  alias NativeElixirPdfUtilities.HtmlToPdf.PageGeometry
  alias NativeElixirPdfUtilities.HtmlToPdf.Pagination
  alias NativeElixirPdfUtilities.HtmlToPdf.PdfWriter
  alias NativeElixirPdfUtilities.HtmlToPdf.Style
  alias NativeElixirPdfUtilities.Diagnostics
  alias NativeElixirPdfUtilities.Validators.HtmlValidator

  @type page_size :: PageGeometry.page_size_input()
  @type page_margin :: PageGeometry.margin_input()
  @type pdf_metadata ::
          keyword()
          | %{
              optional(:title) => String.t(),
              optional(:author) => String.t(),
              optional(:subject) => String.t(),
              optional(:keywords) => String.t() | [String.t()],
              optional(:creation_date) =>
                Date.t() | NaiveDateTime.t() | DateTime.t() | String.t(),
              optional(:modification_date) =>
                Date.t() | NaiveDateTime.t() | DateTime.t() | String.t()
            }
  @type page_furniture_template :: String.t() | false | nil
  @type page_furniture_variants ::
          String.t()
          | [
              default: page_furniture_template(),
              first: page_furniture_template(),
              odd: page_furniture_template(),
              even: page_furniture_template()
            ]
          | %{
              optional(:default) => page_furniture_template(),
              optional(:first) => page_furniture_template(),
              optional(:odd) => page_furniture_template(),
              optional(:even) => page_furniture_template()
            }
  @type page_furniture ::
          [
            header: page_furniture_variants(),
            footer: page_furniture_variants()
          ]
          | %{
              optional(:header) => page_furniture_variants(),
              optional(:footer) => page_furniture_variants()
            }
  @typedoc "An explicitly tagged inline stylesheet or local stylesheet file."
  @type stylesheet_source :: {:css, String.t()} | {:file, String.t()}
  @type unsupported_glyphs :: :replace | :error
  @type render_option ::
          {:page_size, page_size()}
          | {:margin, page_margin()}
          | {:base_url, String.t() | nil}
          | {:stylesheets, [stylesheet_source()]}
          | {:default_font, String.t() | [String.t()]}
          | {:fonts, [map() | keyword() | {String.t(), String.t()}]}
          | {:metadata, pdf_metadata()}
          | {:page_furniture, page_furniture() | false | nil}
          | {:unsupported_glyphs, unsupported_glyphs()}
  @type error_reason ::
          :invalid_document
          | :invalid_css
          | :invalid_encoding
          | :invalid_html
          | :invalid_layout
          | :invalid_margin
          | :invalid_options
          | :invalid_page_size
          | :invalid_path
          | :invalid_pdf_input
          | :not_implemented
          | :resource_limit_exceeded
          | :unsupported_glyph
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
  `:stylesheets`, `:default_font`, explicit local `:fonts`, PDF `:metadata`,
  opt-in `:page_furniture` headers and footers, and `:unsupported_glyphs`.
  Metadata supports title, author, subject, keywords, creation date, and
  modification date. An HTML `<title>` supplies the PDF title when
  `metadata[:title]` is not set.

  Unsupported graphemes are replaced visibly with U+FFFD by default. Set
  `unsupported_glyphs: :error` to return an `:unsupported_glyph` diagnostic
  instead.

  Page furniture accepts `:header` and `:footer` HTML templates. Each can be a
  string used on every page or variants named `:default`, `:first`, `:odd`, and
  `:even`. A variant set to `false` or `nil` is omitted. The `:first` variant
  has precedence on page one, followed by the matching odd/even variant and
  then `:default`. Templates can contain `{{page}}` and `{{pages}}` tokens.
  Furniture is disabled when `:page_furniture` is omitted, `nil`, or `false`.
  Enabled furniture must fit inside the page margin.

  `:page_size` accepts the CSS named sizes `:a5`, `:a4`, `:a3`, `:b5`, `:b4`,
  `:jis_b5`, `:jis_b4`, `:letter`, `:legal`, and `:ledger`, optionally paired
  with `:portrait` or `:landscape`, or a positive `{width, height}` tuple. Tuple
  values up to `20 x 20` are interpreted as inches for compatibility with
  ChromicPDF-style custom label sizes; larger tuples are interpreted as PDF
  points. CSS two-length strings retain their declared units.

  `:margin` accepts a nonnegative point number, a CSS string containing one to
  four absolute lengths, or a map with `:top`, `:right`, `:bottom`, and `:left`
  values. Explicit renderer `:page_size` and `:margin` options override
  stylesheet `@page` defaults.

  `:stylesheets` accepts a list of `{:css, css}` and `{:file, path}` tuples.
  The explicit tag determines whether content is parsed directly or read from
  the local filesystem; bare strings are rejected.

  `:base_url` is also the authorization root for document-selected local image
  and `@font-face` paths. Relative paths and absolute paths beneath that root
  are accepted; traversal outside it and symlink components are rejected.
  Explicit `:fonts` and `{:file, stylesheet}` paths are trusted caller
  configuration and are not reclassified as document-selected resources.
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
    case HtmlValidator.validate_paths(input_path, output_path) do
      {:ok, %{input_path: input_path, output_path: output_path}} ->
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

      {:error, {reason, diagnostic}} ->
        {:error,
         {reason,
          Diagnostics.with_context(diagnostic, operation: :render_file, module: __MODULE__)}}
    end
  end

  defp do_render(html, opts) do
    font_options = Font.normalize_options(opts)

    case HtmlValidator.validate_render_request(html, opts, font_options) do
      :ok ->
        {:ok, opts} = font_options

        with {:ok, dom} <- HtmlParser.parse_detailed(html),
             {:ok, effective_opts} <- effective_render_options_detailed(dom, opts),
             {:ok, styled_tree} <- Style.compute_detailed(dom, effective_opts),
             {:ok, styled_tree} <-
               FontFallback.resolve(
                 styled_tree,
                 Keyword.get(effective_opts, :unsupported_glyphs, :replace)
               ),
             {:ok, layout_tree} <- layout_document(styled_tree, effective_opts),
             {:ok, pages} <- Pagination.paginate(layout_tree, effective_opts),
             {:ok, pages} <- PageFurniture.decorate(pages, layout_tree, effective_opts),
             {:ok, pdf_binary} <- PdfWriter.render(pages, effective_opts) do
          {:ok, pdf_binary}
        end

      {:error, {_reason, _diagnostic}} = error ->
        error
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
    with {:ok, stylesheet_entries} <- Style.load_stylesheets(dom, opts),
         {:ok, page_options} <- page_options_from_stylesheets(stylesheet_entries) do
      effective_opts = Keyword.merge(page_options, opts)
      {:ok, metadata_options(dom, effective_opts)}
    else
      {:error, :invalid_document} ->
        Diagnostics.error(
          :style,
          :invalid_document,
          "configured stylesheet file could not be read"
        )

      {:error, {_reason, _diagnostic}} = error ->
        error
    end
  end

  defp page_options_from_stylesheets(entries) do
    Enum.reduce_while(entries, {:ok, []}, fn entry, {:ok, acc} ->
      case CssParser.page_options(entry.css) do
        {:ok, page_options} ->
          {:cont, {:ok, PageGeometry.merge_page_options(acc, page_options)}}

        {:error, :invalid_css} ->
          {:error, error} = CssParser.parse_detailed(entry.css)
          {:halt, {:error, error}}
      end
    end)
  end

  defp metadata_options(dom, opts) do
    case document_title(dom) do
      nil ->
        opts

      title ->
        case Keyword.fetch(opts, :metadata) do
          :error ->
            Keyword.put(opts, :metadata, title: title)

          {:ok, metadata} when is_map(metadata) ->
            Keyword.put(opts, :metadata, Map.put_new(metadata, :title, title))

          {:ok, metadata} when is_list(metadata) ->
            case Keyword.keyword?(metadata) do
              true -> Keyword.put(opts, :metadata, Keyword.put_new(metadata, :title, title))
              false -> opts
            end

          {:ok, _metadata} ->
            opts
        end
    end
  end

  defp document_title(node) do
    case node do
      %{type: :element, tag: "title", children: children} ->
        title =
          children
          |> Enum.map_join("", & &1.text)
          |> String.trim()

        if title == "", do: nil, else: title

      %{children: children} when is_list(children) ->
        Enum.find_value(children, &document_title/1)

      _ ->
        nil
    end
  end

  defp layout_message(reason) do
    case reason do
      :invalid_margin ->
        "layout failed: margin must be non-negative and leave a positive printable area"

      reason ->
        "layout failed: #{reason}"
    end
  end

  defp file_error(reason, operation, source) do
    Diagnostics.error(:file, reason, "file #{operation} failed: #{reason}",
      operation: operation,
      module: __MODULE__,
      source: source
    )
  end
end
