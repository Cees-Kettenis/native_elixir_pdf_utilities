# Create PDFs from HTML

Use `NativeElixirPdfUtilities.HtmlToPdf` to render HTML into PDF bytes or a file.
The examples below reuse the alias from the first example. Supply your own
files where paths are shown. See [HTML and CSS support](html-to-pdf-compatibility.md)
for supported features and options.

## Render HTML

`render/2` returns PDF bytes:

```elixir
alias NativeElixirPdfUtilities.HtmlToPdf

html = "<h1>Invoice</h1><p>Amount due: 200.00</p>"

{:ok, pdf} = HtmlToPdf.render(html, page_size: :a4, margin: "18mm")
File.write!("/tmp/invoice.pdf", pdf)
```

### Add a styled table

```elixir
html = """
<style>
  table { width: 100%; border-collapse: collapse; }
  th, td { border: 1pt solid #cccccc; padding: 5pt; text-align: left; }
</style>
<table>
  <thead><tr><th>Item</th><th>Amount</th></tr></thead>
  <tbody><tr><td>Printing</td><td>200.00</td></tr></tbody>
</table>
"""

{:ok, pdf} = HtmlToPdf.render(html, margin: "18mm")
```

## Render a file

`render_file/3` reads the HTML and writes the PDF:

```elixir
:ok =
  HtmlToPdf.render_file(
    "priv/static/templates/invoice.html",
    "/tmp/invoice.pdf",
    page_size: :a4,
    margin: "18mm",
    stylesheets: [{:file, "priv/static/templates/invoice.css"}],
    base_url: "priv/static"
  )
```

Use `{:css, css}` for inline CSS and `{:file, path}` for a stylesheet file.
The renderer does not infer `:base_url` from the input file's directory. Reading
and writing errors use the [diagnostic contract](diagnostics.md).

## Add headers, footers, and page numbers

Use `:page_furniture` for header and footer templates. Leave enough page
margin for them:

```elixir
{:ok, pdf} =
  HtmlToPdf.render(
    "<h1>Account statement</h1><p>Statement content...</p>",
    margin: "18mm",
    page_furniture: [
      header: [
        default: "<div style=\"font-size: 8pt\">Account statement</div>",
        first: false
      ],
      footer:
        "<div style=\"font-size: 8pt; text-align: right\">" <>
          "Page {{page}} of {{pages}}</div>"
    ]
  )
```

Use `:first`, `:odd`, `:even`, and `:default` variants when pages need
different headers or footers. A `false` or `nil` variant disables that template on the
matching page. To number an existing PDF, use [Stamp.page_numbers/2](https://github.com/Cees-Kettenis/native_elixir_pdf_utilities/blob/main/docs/pdf-stamping.md#page-numbers).

## Load local images

Set `:base_url` when the document refers to local images or fonts. Referenced
paths must stay beneath that directory.

```elixir
{:ok, pdf} =
  HtmlToPdf.render(
    """
    <div style="width: 90mm; border: 1pt solid #111; padding: 4mm">
      <img src="images/product.png" alt="Product" style="width: 22mm">
      <p>001764 - DEFAULT</p>
    </div>
    """,
    base_url: "priv/static"
  )
```

Map document references to bytes to avoid filesystem reads for those assets,
or to trusted files to authorize specific paths:

```elixir
HtmlToPdf.render(html,
  assets: %{
    "product-image" => {:bytes, png_bytes},
    "report-font" => {:file, "priv/fonts/report-sans.ttf"}
  }
)
```

The HTML or CSS can use those exact references in `src` or `url(...)`.
For caller-managed asset loading, supply a resolver:

```elixir
resolver = fn
  %{reference: "company-logo", kind: :image} -> {:ok, png_bytes}
  _request -> :not_found
end

{:ok, pdf} =
  HtmlToPdf.render(~s(<img src="company-logo" style="width: 30mm">),
    asset_resolver: resolver
  )
```

See [Local files and assets](html-to-pdf-compatibility.md#assets-and-local-files)
for the path and callback rules.

## Register a font

Pass a static TrueType font when output must use the same face on every host:

```elixir
{:ok, pdf} =
  HtmlToPdf.render(
    ~s(<p style="font-family: 'Report Sans'">Café</p>),
    fonts: [%{family: "Report Sans", path: "priv/fonts/report-sans.ttf"}],
    system_font_discovery: false
  )
```

Use `data: ttf_bytes` instead of `path:` for an in-memory font. Font fallback
and embedding restrictions are covered in [Fonts and text](html-to-pdf-compatibility.md#fonts-and-text).

A document can also load a font beneath `:base_url` with `@font-face`:

```elixir
{:ok, pdf} =
  HtmlToPdf.render(
    """
    <style>
      @font-face {
        font-family: "Report Sans";
        src: url("fonts/report-sans.ttf") format("truetype");
      }
      body { font-family: "Report Sans"; }
    </style>
    <p>Café</p>
    """,
    base_url: "priv/static"
  )
```

## Set PDF metadata

```elixir
{:ok, pdf} =
  HtmlToPdf.render(
    "<title>Monthly statement</title><p>Statement content</p>",
    metadata: [
      author: "Finance Operations",
      subject: "Customer statement",
      keywords: ["statement", "monthly"],
      creation_date: Date.utc_today()
    ]
  )
```

The first non-empty HTML `<title>` becomes the PDF title unless
`metadata[:title]` is set. To change metadata in an existing PDF, use
[Info.put/2](pdf-information.md#updating-information).

## Create bookmarks from headings

Visible HTML headings can become nested items in the PDF viewer's bookmarks
panel:

```elixir
{:ok, pdf} =
  HtmlToPdf.render(
    """
    <h1>Annual report</h1>
    <h2>Financial results</h2>
    <h3>Revenue</h3>
    <h3>Expenses</h3>
    """,
    outlines: :headings
  )
```

See [PDF outlines and bookmarks](pdf-outlines.md) for exact outline input,
automatic detection in existing PDFs, and preservation behavior.

## Render static form values

Form controls become visible, non-editable PDF content:

```elixir
html = """
<div>
  <input type="text" value="Amira Tan">
  <input type="checkbox" checked>
  <select><option selected>Approved</option></select>
  <textarea>Documents verified</textarea>
  <button type="button">Record application</button>
</div>
"""

{:ok, pdf} = HtmlToPdf.render(html)
```

## Handle an error

Rendering uses the library's shared diagnostic result:

```elixir
require Logger

case HtmlToPdf.render(html) do
  {:ok, pdf} ->
    File.write!("/tmp/document.pdf", pdf)

  {:error, {reason, diagnostic}} ->
    Logger.warning(
      "PDF render failed with #{reason}: #{diagnostic.message}"
    )
end
```

Use the reason for program flow and the diagnostic for logs or template fixes.
See [Diagnostics](diagnostics.md) for the full contract.
