defmodule NativeElixirPdfUtilities.HtmlToPdf do
  @moduledoc """
  Public facade for native HTML/CSS to PDF rendering.

  Supported controls generate interactive AcroForm fields by default. Set
  `forms: :static` for static artwork. Names use HTML `name` or a generated
  `TYPE_PAGENR_ELEMENT` key; see the [forms guide](pdf-forms.html).

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
  default. See the
  [HTML-to-PDF compatibility guide](html-to-pdf-compatibility.html) for the
  current element, CSS, layout, image, and font support.
  """

  alias NativeElixirPdfUtilities.HtmlToPdf.ResourceContext
  alias NativeElixirPdfUtilities.Validators.ResourceValidator
  alias NativeElixirPdfUtilities.HtmlToPdf.RenderCache
  alias NativeElixirPdfUtilities.HtmlToPdf.Font
  alias NativeElixirPdfUtilities.HtmlToPdf.FontFallback
  alias NativeElixirPdfUtilities.HtmlToPdf.Layout
  alias NativeElixirPdfUtilities.HtmlToPdf.PageFurniture
  alias NativeElixirPdfUtilities.HtmlToPdf.PageGeometry
  alias NativeElixirPdfUtilities.HtmlToPdf.Pagination
  alias NativeElixirPdfUtilities.HtmlToPdf.PdfWriter
  alias NativeElixirPdfUtilities.HtmlToPdf.Style
  alias NativeElixirPdfUtilities.Diagnostics
  alias NativeElixirPdfUtilities.Pdf.OutlineDetector
  alias NativeElixirPdfUtilities.Validators.FontValidator
  alias NativeElixirPdfUtilities.Validators.PrintMediaValidator
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
              optional(:producer) => String.t(),
              optional(:creation_date) =>
                Date.t() | NaiveDateTime.t() | DateTime.t() | String.t(),
              optional(:modification_date) =>
                Date.t() | NaiveDateTime.t() | DateTime.t() | String.t()
            }
  @type page_furniture_template :: String.t() | false | nil
  @type page_furniture_variants ::
          page_furniture_template()
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
  @typedoc "Caller-approved bytes or a trusted local asset file."
  @type asset_source :: {:bytes, binary()} | {:file, String.t()}
  @typedoc "Context supplied to a caller-provided asset resolver."
  @type asset_request :: %{
          required(:reference) => String.t(),
          required(:kind) => :image | :background_image | :font
        }
  @typedoc "A callback that supplies bytes without giving the renderer network access."
  @type asset_resolver ::
          (asset_request() -> {:ok, binary()} | :not_found | {:error, term()})
  @type unsupported_glyphs :: :replace | :error
  @type outlines :: :headings | [NativeElixirPdfUtilities.Outlines.item_input()] | false | nil
  @type render_option ::
          {:forms, :interactive | :static}
          | {:print_viewport, {number(), number()}}
          | {:image_selection_density, number()}
          | {:page_size, page_size()}
          | {:margin, page_margin()}
          | {:base_url, String.t() | nil}
          | {:assets, %{optional(String.t()) => asset_source()}}
          | {:asset_resolver, asset_resolver() | nil}
          | {:stylesheets, [stylesheet_source()]}
          | {:print_viewport, {number(), number()}}
          | {:image_selection_density, number()}
          | {:default_font, String.t() | [String.t()]}
          | {:fonts, [map() | keyword() | {String.t(), String.t()}]}
          | {:system_font_discovery, boolean()}
          | {:metadata, pdf_metadata()}
          | {:outlines, outlines()}
          | {:page_furniture, page_furniture() | false | nil}
          | {:unsupported_glyphs, unsupported_glyphs()}
          | {:compress_streams, boolean()}
          | {:subset_fonts, boolean()}
  @type error_reason ::
          :invalid_document
          | :invalid_css
          | :invalid_encoding
          | :invalid_form
          | :invalid_html
          | :invalid_layout
          | :invalid_margin
          | :invalid_options
          | :invalid_outlines
          | :invalid_page_size
          | :invalid_path
          | :invalid_pdf_input
          | :not_implemented
          | :resource_denied
          | :resource_cycle
          | :resource_not_found
          | :resource_limit_exceeded
          | :unsupported_form
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
  `:stylesheets`, `:default_font`, explicit local `:fonts`,
  `:system_font_discovery`, PDF `:metadata`, opt-in `:page_furniture` headers
  and footers, PDF `:outlines`, `:unsupported_glyphs`, `:compress_streams`, and
  `:subset_fonts`. Responsive selection accepts `:print_viewport` as a positive
  `{width, height}` in CSS pixels and `:image_selection_density` as a positive
  number, defaulting to the effective page area and density 1. Pictures compose
  inline by default and accept authored `display:block` for block placement.
  Stream compression and TrueType font subsetting are enabled by default to keep
  PDFs small. Set `compress_streams: false` or `subset_fonts: false` to opt out.
  Subsetting retains the original glyph IDs for PDF character mapping and keeps
  the full font when its license forbids subsetting.
  Metadata supports title, author, subject, keywords, producer, creation date,
  and modification date. An HTML `<title>` supplies the PDF title when
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

  Set `:outlines` to `:headings` to turn visible `h1` through `h6` elements
  into nested PDF bookmarks using their final positions after pagination. An
  exact outline list accepted by `NativeElixirPdfUtilities.Outlines.put/2` may
  also be supplied. Outlines are omitted by default.

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

  Initial local and `srcdoc` iframe documents render in isolated, clipped
  viewports. `srcdoc` takes precedence; absent, empty and `about:blank` sources
  initialize a blank document. Child documents share cumulative rendering
  budgets and caches. Their resources use local authorization before every
  read or cache lookup, and never invoke the asset resolver. `forms: :static`
  preserves child control artwork; interactive child widgets return an
  `:unsupported_form` diagnostic pending transformed widget support. Child HTML
  overflow selects classic scrollbars and reflows within their reduced client
  area. The PDF captures the initial scroll position. Cyclic percentage sizing
  that cannot settle on a client area returns an actionable `:not_implemented`
  diagnostic. Author `html` or `body` with `overflow:hidden` for a static clipped
  viewport.

  `:base_url` is also the authorization root for local iframe documents and
  document-selected local image and `@font-face` paths. Relative paths and absolute paths beneath that root
  are accepted; traversal outside it and symlink components are rejected.
  Explicit `:fonts` and `{:file, stylesheet}` paths are trusted caller
  configuration at the top level. Child documents authorize explicit font and
  stylesheet file paths beneath the same immutable root; supplied inline bytes
  remain permitted.
  Stylesheet media queries use the requested page box in CSS pixels, including
  margins, before authored @page size rules. Set
  `:print_viewport` to an explicit positive `{width, height}` or
  `:image_selection_density` to a positive deterministic density. Child iframe
  styles use their own media viewport including scrollbars. Client dimensions
  govern their layout.

  """
  @spec render(String.t(), [render_option()]) ::
          {:ok, binary()} | {:error, detailed_error_reason()}
  def render(html, opts \\ []) do
    case HtmlValidator.with_render_budget(fn ->
           RenderCache.with_stylesheets(fn ->
             FontValidator.with_budget(fn -> do_render(html, opts) end)
           end)
         end) do
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
        case NativeElixirPdfUtilities.FileReader.read(
               input_path,
               NativeElixirPdfUtilities.Limits.get(:max_html_source_bytes)
             ) do
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

          {:error, {reason, diagnostic}} ->
            {:error,
             {reason,
              Diagnostics.with_context(diagnostic,
                operation: :render_file,
                module: __MODULE__,
                source: input_path
              )}}

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
        image_budget = HtmlValidator.new_image_budget()

        ResourceContext.run(
          ResourceValidator.renderer_context_options(opts),
          image_budget,
          fn _context ->
            with {:ok, prepared} <-
                   NativeElixirPdfUtilities.Validators.TemplateExecutionValidator.prepare_source(
                     html,
                     opts,
                     image_budget
                   ),
                 {:ok, pdf_binary} <- PdfWriter.render(prepared.pages, prepared.options) do
              {:ok, pdf_binary}
            end
          end
        )

      {:error, {_reason, _diagnostic}} = error ->
        error
    end
  end

  @doc false
  @spec prepare_document(term(), term(), HtmlValidator.image_budget()) ::
          {:ok, map()} | {:error, detailed_error_reason()}
  def prepare_document(dom, opts, image_budget) do
    font_options = Font.normalize_options(opts)

    with :ok <- HtmlValidator.validate_document_render_options(opts, font_options),
         :ok <- ResourceValidator.validate_preparation_budget(image_budget),
         {:ok, opts} <- font_options,
         {:ok, effective_opts} <- effective_render_options_detailed(dom, opts),
         {:ok, styled_tree} <- Style.compute_detailed(dom, effective_opts, image_budget),
         :ok <- HtmlValidator.release_style_input(dom),
         {:ok, styled_tree} <-
           FontFallback.resolve(
             styled_tree,
             Keyword.get(effective_opts, :unsupported_glyphs, :replace)
           ),
         {:ok, layout_tree} <-
           Font.with_measurement_cache(fn -> layout_document(styled_tree, effective_opts) end),
         {:ok, layout_tree} <- Layout.resolve_images(layout_tree),
         {:ok, pages} <- Pagination.paginate(layout_tree, effective_opts),
         effective_opts <- prepared_outline_options(pages, effective_opts),
         {:ok, pages} <- PageFurniture.decorate(pages, layout_tree, effective_opts, image_budget) do
      {:ok,
       %{
         styled_tree: styled_tree,
         layout_tree: layout_tree,
         pages: pages,
         options: effective_opts
       }}
    end
  end

  defp prepared_outline_options(pages, opts) do
    outlines =
      case Keyword.get(opts, :outlines) do
        :headings -> OutlineDetector.from_paginated_headings(pages)
        false -> []
        nil -> []
        outlines -> outlines
      end

    Keyword.put(opts, :outlines, outlines)
  end

  defp layout_document(styled_tree, opts) do
    case apply(Layout, :layout, [Map.put(styled_tree, :_css_pixel_viewport, true), opts]) do
      {:ok, layout_tree} ->
        {:ok, layout_tree}

      {:error, {_reason, diagnostic}} = error when is_map(diagnostic) ->
        error

      {:error, reason} ->
        Diagnostics.error(:layout, reason, layout_message(reason))
    end
  end

  defp effective_render_options_detailed(dom, opts) do
    with {:ok, media_context} <- PrintMediaValidator.renderer_context(opts),
         {:ok, stylesheet_entries} <- Style.prepare_stylesheets(dom, opts) do
      page_options =
        Enum.reduce(stylesheet_entries, [], fn entry, acc ->
          PageGeometry.merge_page_options(acc, entry.prepared.page_options)
        end)

      effective_opts = Keyword.merge(page_options, opts)

      effective_opts =
        Keyword.put_new(
          effective_opts,
          :print_viewport,
          {media_context.width, media_context.height}
        )

      {:ok, HtmlValidator.document_metadata_options(dom, effective_opts)}
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
