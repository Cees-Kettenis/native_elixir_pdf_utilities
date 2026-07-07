<p align="center">
  <img src="assets/readme-banner.svg" alt="Native Elixir PDF Utilities" />
</p>

<p align="center">
  <a href="https://hex.pm/packages/native_elixir_pdf_utilities"><img src="https://img.shields.io/hexpm/v/native_elixir_pdf_utilities.svg" alt="Hex.pm" /></a> <a href="https://native-elixir-pdf-utilities.hexdocs.pm/api-reference.html"><img src="https://img.shields.io/badge/hex-docs-blue.svg" alt="HexDocs" /></a> <a href="LICENSE"><img src="https://img.shields.io/hexpm/l/native_elixir_pdf_utilities.svg" alt="License" /></a> <img src="https://img.shields.io/badge/elixir-~%3E%201.18-4B275F.svg" alt="Elixir ~> 1.18" />
</p>

# Native Elixir PDF Utilities

Native Elixir PDF Utilities is a small library for developers who need practical PDF building blocks without command line tools.

PDFs are useful, awkward, and full of edge cases. This project focuses on the common structural work that Elixir applications often need: reading PDF bytes, understanding the object stream, extracting embedded text when it is available, and combining documents in a predictable way.

The goal is not to be a full PDF engine overnight. It is a steadily improving toolkit, handled by an excited developer who wants this to become a dependable native Elixir option for day-to-day PDF utility work.

## Package and Docs

- Package: https://hex.pm/packages/native_elixir_pdf_utilities
- API docs: https://native-elixir-pdf-utilities.hexdocs.pm/api-reference.html

## What It Does

1. Tokenizer - turns classic PDF byte streams into structured Elixir tokens.
2. Merger - combines multiple PDF binaries into a fresh PDF with rewritten object references.
3. Reader - extracts embedded text from PDFs when the document contains readable text data.
4. HTML to PDF - renders a strict, document-oriented HTML/CSS subset to native PDF bytes without Chromium, wkhtmltopdf, Node, Rust, Python, OS packages, or SaaS calls.

## Native HTML/CSS to PDF

`NativeElixirPdfUtilities.HtmlToPdf` is a native document renderer. It is designed for predictable server-side PDFs such as reports, invoices, labels, statements, and simple generated documents. It is not a browser engine and does not claim browser compatibility.

```elixir
alias NativeElixirPdfUtilities.HtmlToPdf

{:ok, pdf} =
  HtmlToPdf.render("""
  <style>
    h1 { color: #336699; }
    p { border: 1pt solid #336699; padding: 6pt; }
  </style>
  <h1>Invoice</h1>
  <p>Hello <strong>customer</strong></p>
  <ul><li><a href="https://example.com">View account</a></li></ul>
  """)
```

Write an HTML file directly to a PDF path:

```elixir
:ok =
  HtmlToPdf.render_file(
    "invoice.html",
    "invoice.pdf",
    page_size: :a4,
    margin: "20mm",
    stylesheets: ["assets/invoice.css"],
    base_url: "assets"
  )
```

Embed caller-supplied TTF fonts explicitly:

```elixir
{:ok, pdf} =
  HtmlToPdf.render(
    ~s(<p style="font-family: 'Report Sans', Helvetica">Café</p>),
    fonts: [
      %{family: "Report Sans", path: "priv/fonts/report-sans.ttf"}
    ]
  )
```

Render local or data URI images:

```elixir
{:ok, pdf} =
  HtmlToPdf.render(
    ~s(<img src="logo.png" style="width: 96pt"><p>Quarterly report</p>),
    base_url: "priv/static"
  )
```

### HtmlToPdf Options

| Option | Supported values | Notes |
| --- | --- | --- |
| `:page_size` | `:a4` or `{width, height}` in points | Custom page sizes must be positive numbers. |
| `:margin` | Number of points or CSS length string | Examples: `24`, `"20mm"`, `"0.5in"`. |
| `:base_url` | Local path or `file://` URL | Used for relative image paths. Remote HTTP fetching is not supported. |
| `:stylesheets` | CSS strings or local CSS file paths | Configured stylesheets load before embedded `<style>` tags. |
| `:default_font` | Font family or fallback list | Defaults to `"Helvetica"`. |
| `:fonts` | `%{family: ..., path: ...}` maps, keyword lists, or `{family, path}` tuples | TTF only. `:weight` and `:style` are optional. System font discovery is intentionally not required. |

### HTML Support Matrix

| Area | Supported |
| --- | --- |
| Document wrappers | `doctype html`, `html`, `head`, `body`, `style`, `meta`, `title` |
| Blocks | `div`, `p`, `h1` through `h6` |
| Inline text | `span`, `strong`, `b`, `em`, `i`, `a`, `br`; common named and numeric HTML entities are decoded |
| Lists | `ul`, `ol`, `li` |
| Tables | `table`, `caption`, `thead`, `tbody`, `tfoot`, `tr`, `th`, `td` |
| Images | Strict `img` with required `src` |
| Attributes | `id`, `class`, `style`, `lang` on `html`, metadata attributes on `meta`, `href` on links, `src` on images, `colspan`/`rowspan` on cells |
| Links | `https://`, `http://`, and `mailto:` URI annotations |

Unsupported HTML returns `{:error, :unsupported_html}` or `{:error, :invalid_document}` instead of being silently approximated.

### CSS Support Matrix

| Area | Supported |
| --- | --- |
| Selectors | Element, `.class`, `#id`, `element.class`, descendant, direct child, comma groups, `:root`, `:first-child` |
| Cascade | Specificity, source order, inline style priority, `!important`, inheritance for text styles, and CSS custom properties via `var(--name)` |
| Units | `pt`, `px`, `rem`, `mm`, `cm`, `in`, percentages for `width`/`height`/`min-height`, and unitless `0` |
| Display | `block`, `inline`, `none`, `flex`, `inline-flex`, `grid`, `inline-grid` |
| Box model | `width`, `height`, `min-height`, `aspect-ratio`, `margin`, negative margins, `padding`, side-specific margin/padding, `border`, side-specific `border-*`, `border-width`, `border-color`, `border-radius`, `border-collapse`, `background-color` |
| Text | `color`, `font-family`, `font-size`, `font-weight`, `font-style`, `line-height`, `text-align`, `vertical-align`, `line-break`; `#RRGGBBAA` colors are accepted with alpha ignored |
| Page breaks | `break-before`, `break-after`, `page-break-before`, `page-break-after` with `auto`, `page`, or `always` |
| Flexbox subset | `flex-direction`, `flex-wrap`, `gap`, `row-gap`, `column-gap`, `justify-content`, `align-items`, `align-self`, `order`, `flex-grow`, `flex-shrink`, `flex-basis`, `flex` |
| Grid subset | `grid-template-columns`, `grid-template-rows`, `grid-auto-columns`, `grid-auto-rows`, `grid-column`, `grid-column-start`, `grid-column-end`, `grid-row`, `grid-row-start`, `grid-row-end`, `grid-area`, `gap`, `row-gap`, `column-gap`, `justify-items`, `align-items`, `justify-content`, `align-content` |

Unsupported CSS properties or invalid values return `{:error, :invalid_document}`. The renderer does not ignore unknown declarations.

### Layout Details

Block, list, table, flexbox, and grid layout are deterministic and intentionally narrower than browser layout. Tables use deterministic column sizing based on declared widths, available table width, and intrinsic unbreakable content, with support for collapsed borders, cell backgrounds, `colspan`, repeated headers, and missing trailing cells in shorter rows. Flexbox and grid support document-oriented single-line text and image items, not the full browser algorithms.

Pagination supports automatic page breaks, manual page breaks, page margins, basic keep-together behavior for emitted flow units, and repeated table headers when table bodies continue across pages.

Images support PNG, JPEG, and SVG data URIs, plus PNG/JPEG from absolute local paths and `base_url`-relative paths. SVG data URIs are rasterized to PNG with the lightweight `resvg` NIF using local in-process rendering; remote URLs and unsafe relative paths are rejected.

Fonts support built-in PDF fonts (`Helvetica`, `Courier`, `Times-Roman` and their bold/italic variants) plus explicit TTF embedding. Embedded fonts use TTF glyph widths, Type0/CID PDF resources, and basic Unicode mapping. Complex shaping for Arabic, Indic scripts, Thai, emoji sequences, and other advanced typography is not supported.

### Unsupported Features

These features are intentionally outside the current renderer boundary:

- JavaScript and runtime DOM behavior.
- `script`, `canvas`, `video`, `audio`, `iframe`, and interactive form behavior.
- Remote asset fetching.
- CSS floats, absolute/fixed positioning, transforms, animations, media queries, pseudo-elements, and pseudo-classes.
- Full browser-compatible table, flexbox, and grid algorithms.
- Complex text shaping and bidirectional layout.

### Validation Expectations

Automated tests cover parsing, CSS cascade, layout dimensions, pagination, PDF object output, images, fonts, links, and end-to-end rendering. Human visual validation is still required before accepting broad layout changes because PDF layout regressions can be visually obvious while remaining structurally valid.

## Installation

```elixir
def deps do
  [
    {:native_elixir_pdf_utilities, "~> 0.2.0"}
  ]
end
```

## Development

Run the project checks before contributing:

```bash
mise exec -- mix test
MIX_ENV=test mise exec -- mix dialyzer
mise exec -- mix format
```

For HTML/CSS to PDF changes, also open representative rendered PDFs and compare layout visually across single-page, multi-page, table, flexbox, grid, image, and embedded-font examples.

## License

MIT. See `LICENSE`.
