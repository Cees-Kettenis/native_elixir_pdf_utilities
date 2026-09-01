# HTML to PDF examples

These examples show common `NativeElixirPdfUtilities.HtmlToPdf` workflows. See
[HTML to PDF compatibility](html-to-pdf-compatibility.md) for the supported
HTML, CSS, options, and known limits.

## Render HTML

`render/2` returns PDF bytes:

```elixir
alias NativeElixirPdfUtilities.HtmlToPdf

html = """
<style>
  @page { size: A4; margin: 18mm; }
  body { font-family: "DejaVu Sans"; font-size: 10pt; }
  h1 { border-bottom: 2pt solid #22344a; padding-bottom: 6pt; }
  table { width: 100%; border-collapse: collapse; }
  th, td { border: 1pt solid #d3d3d3; padding: 5pt; }
  th { background-color: #eeeeee; text-align: left; }
  .amount { text-align: right; }
</style>

<h1>Invoice INV-0001</h1>
<table>
  <thead>
    <tr><th>Item</th><th>Description</th><th class="amount">Amount</th></tr>
  </thead>
  <tbody>
    <tr><td>PO-1</td><td>Cutting and sewing</td><td class="amount">120.00</td></tr>
    <tr><td>PO-2</td><td>Finishing</td><td class="amount">80.00</td></tr>
  </tbody>
</table>
"""

{:ok, pdf} = HtmlToPdf.render(html)
File.write!("/tmp/invoice.pdf", pdf)
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

## Add headers, footers, and page numbers

Page furniture is opt-in. Reserve page margins large enough for it:

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
different furniture.

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

To supply assets without filesystem access, map document references to bytes
or trusted files:

```elixir
HtmlToPdf.render(html,
  assets: %{
    "product-image" => {:bytes, png_bytes},
    "report-font" => {:file, "priv/fonts/report-sans.ttf"}
  }
)
```

The HTML or CSS can use those exact references in `src` or `url(...)`.

## Register a font

Pass a static TrueType font when output must use the same face on every host:

```elixir
{:ok, pdf} =
  HtmlToPdf.render(
    ~s(<p style="font-family: 'Report Sans'">Café</p>),
    fonts: [%{family: "Report Sans", path: "priv/fonts/report-sans.ttf"}]
  )
```

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
`metadata[:title]` is set.

## Render static form values

Form controls become visible, non-editable PDF content:

```elixir
html = """
<input type="text" value="Amira Tan">
<input type="checkbox" checked>
<select><option selected>Approved</option></select>
<textarea>Documents verified</textarea>
<button type="button">Record application</button>
"""

{:ok, pdf} = HtmlToPdf.render(html)
```

## Handle an error

Rendering uses the library's shared diagnostic result:

```elixir
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
