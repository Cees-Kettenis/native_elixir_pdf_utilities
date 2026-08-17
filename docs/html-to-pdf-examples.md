# HTML to PDF Examples

`NativeElixirPdfUtilities.HtmlToPdf` renders a strict, document-oriented HTML/CSS subset to native PDF bytes. These examples show the intended calling style for reports, print templates, labels, and operational documents.

For the exact compatibility surface, see [HTML to PDF Compatibility](html-to-pdf-compatibility.md).

## Basic Render

```elixir
alias NativeElixirPdfUtilities.HtmlToPdf

{:ok, pdf} =
  HtmlToPdf.render("""
  <style>
    @page { size: A4; margin: 18mm; }

    body {
      font-family: Helvetica;
      font-size: 10pt;
      color: #142033;
    }

    .document-title {
      font-size: 18pt;
      font-weight: bold;
      margin-bottom: 10pt;
      border-bottom: 2pt solid #22344a;
      padding-bottom: 6pt;
    }

    .summary {
      display: grid;
      grid-template-columns: 1fr 1fr 1fr;
      gap: 8pt;
      margin-bottom: 12pt;
    }

    .summary-box {
      border: 1pt solid #cfd8e3;
      padding: 7pt;
      background-color: #f7f9fc;
    }

    table {
      width: 100%;
      border-collapse: collapse;
    }

    th {
      background-color: #eeeeee;
      font-weight: bold;
      text-align: left;
    }

    th, td {
      border: 1pt solid #d3d3d3;
      padding: 5pt;
      vertical-align: top;
    }

    .amount {
      text-align: right;
    }
  </style>

  <section>
    <h1 class="document-title">Invoice INV-0001</h1>

    <div class="summary">
      <div class="summary-box"><strong>Customer</strong><br>South Island Garment</div>
      <div class="summary-box"><strong>Date</strong><br>08/07/2026</div>
      <div class="summary-box"><strong>Status</strong><br>Ready</div>
    </div>

    <table>
      <thead>
        <tr><th>Item</th><th>Description</th><th class="amount">Amount</th></tr>
      </thead>
      <tbody>
        <tr><td>PO-1</td><td>Cutting and sewing service</td><td class="amount">120.00</td></tr>
        <tr><td>PO-2</td><td>Finishing service</td><td class="amount">80.00</td></tr>
      </tbody>
    </table>
  </section>
  """)
```

## Render a File

Use `render_file/3` when the HTML already lives on disk and the result should be written directly.

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

Configured stylesheets are loaded before embedded `<style>` tags. This lets shared print CSS define defaults while the template keeps document-specific overrides close to the markup.
Use `{:css, css}` for inline configured CSS and `{:file, path}` for a local
stylesheet. Bare strings are rejected so the renderer never has to guess
whether a value is CSS or a filesystem path.

## Running Headers, Footers, and Page Numbers

Page furniture is disabled unless `:page_furniture` is supplied. Reserve enough
page margin for the visible header and footer:

```elixir
{:ok, pdf} =
  HtmlToPdf.render(
    """
    <h1>Account statement</h1>
    <p>Statement content...</p>
    """,
    page_size: :a4,
    margin: "18mm",
    page_furniture: [
      header: [
        default: "<div style=\"font-size: 8pt\">Account statement</div>",
        first: false
      ],
      footer:
        "<div style=\"font-size: 8pt; text-align: right\">Page {{page}} of {{pages}}</div>"
    ]
  )
```

The example omits the header on the first page and repeats it afterward. For
first-page-only furniture, use `default: false` and provide `first: template`.
For distinct facing-page designs, provide `odd:` and `even:` templates.
`:first` takes precedence on page one, then odd/even, then `:default`.

Templates support the normal renderer HTML/CSS subset. Configured
`:stylesheets`, `:fonts`, and `:base_url` options are available while rendering
them. Main-document embedded styles are separate from page-furniture
templates, so place shared rules in `:stylesheets` or inline them in the
template.

## Images

Local PNG/JPEG paths must resolve beneath `:base_url`, which acts as the
document-resource authorization root. Relative paths and absolute paths inside
that root are accepted; traversal and symlink components are rejected. SVG
data URIs are accepted and rasterized locally, subject to a 5 MB source limit,
an 8,192 pixel per-axis limit, and a 16,777,216 total-pixel raster limit.

```elixir
{:ok, pdf} =
  HtmlToPdf.render(
    """
    <style>
      .label {
        width: 90mm;
        height: 38mm;
        padding: 4mm;
        border: 1pt solid #111111;
      }

      .qr {
        width: 22mm;
        height: 22mm;
        margin-bottom: 3mm;
      }
    </style>

    <div class="label">
      <img class="qr" src="qr/stock-in-001.png" alt="Stock QR">
      <strong>Product</strong><br>
      001764 - DEFAULT
    </div>
    """,
    page_size: {90 / 25.4, 38 / 25.4},
    margin: 0,
    base_url: "priv/static"
  )
```

Remote asset fetching is intentionally not supported. The renderer should be deterministic on the server and should not depend on network availability during PDF generation.

## Fonts

Built-in PDF fonts are available without setup. For Unicode-heavy documents, pass explicit TrueType fonts or declare a local font in CSS.

```elixir
{:ok, pdf} =
  HtmlToPdf.render(
    ~s(<p style="font-family: 'Report Sans', Helvetica">Café</p>),
    fonts: [
      %{family: "Report Sans", path: "priv/fonts/report-sans.ttf"}
    ]
  )
```

Explicit font registration avoids relying on OS font discovery. That makes production output easier to reproduce across containers and hosts.

CSS declarations use the same registry and can resolve local URLs beneath
`:base_url`:

```elixir
{:ok, pdf} =
  HtmlToPdf.render(
    """
    <style>
      @font-face {
        font-family: "Report Sans";
        src: url("fonts/report-sans.ttf") format("truetype");
        font-weight: 400;
        font-style: normal;
      }
      @media print { body { font-family: "Report Sans", sans-serif; } }
    </style>
    <p>Café</p>
    """,
    base_url: "priv/static"
  )
```

Document-selected font URLs must remain beneath `:base_url` and cannot traverse
symlinks. WOFF/WOFF2 and CFF-flavored OpenType fonts are unsupported; convert
them to TTF for predictable embedding.

## PDF Metadata

Set common PDF document information under `:metadata`. Calendar structs and ISO 8601 strings are accepted for dates.

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

When `:metadata` does not contain `:title`, the renderer uses the first non-empty HTML `<title>`. An explicit metadata title always wins.

## Static Form Records

Supported form controls render as visible, non-editable PDF content. They do not create PDF form fields or widget annotations.

```elixir
html = """
<div class="application">
  <input type="text" value="Amira Tan">
  <input type="checkbox" checked>
  <input type="radio">
  <select>
    <option>Pending</option>
    <option selected>Approved</option>
  </select>
  <textarea>Documents verified
Signature required</textarea>
  <button type="button">Record application</button>
</div>
"""

{:ok, pdf} = NativeElixirPdfUtilities.HtmlToPdf.render(html)
```

Text inputs display their `value`. Selects display the selected option, or the first option when no `selected` attribute is present. Textarea child text takes precedence over its optional `value` fallback. `checked`, `selected`, and `disabled` may use normal valueless HTML syntax. Disabled controls have no automatic visual treatment; use a class or an attribute selector such as `input[disabled]` when a disabled-looking print style is required.

## Styling Choices

The renderer is intentionally strict. Unsupported CSS does not get ignored because silent fallback can create PDFs that look valid but are missing important layout or print information.

Preferred template patterns:

- Use explicit `@page` size and margins for print templates.
- Use tables for tabular financial or item data.
- Use grid or flex for predictable document header and card layouts.
- Use explicit `width`, `height`, `min-height`, and padding where exact print dimensions matter.
- Keep images local or use data URIs.
- Prefer simple selectors and document-oriented CSS over browser app CSS.

Useful print CSS:

```css
@page {
  size: A4 landscape;
  margin: 12mm 10mm 15mm;
  margin-left: 18mm;
}

.sheet {
  width: 100%;
  min-height: 190mm;
  font-size: 8pt;
}

.header {
  display: flex;
  justify-content: space-between;
  align-items: flex-start;
  border-bottom: 2pt solid #22344a;
  padding-bottom: 6pt;
  margin-bottom: 8pt;
}

.items {
  width: 100%;
  border-collapse: collapse;
}

.items th,
.items td {
  border: 1pt solid #d0d0d0;
  padding: 4pt;
}
```

## Error Handling

`render/2` returns a broad error reason plus a diagnostic detail map when rendering input is invalid.

```elixir
case HtmlToPdf.render(html, page_size: :a4) do
  {:ok, pdf} ->
    File.write!("/tmp/document.pdf", pdf)

  {:error, {reason, detail}} ->
    Logger.warning("""
    PDF render failed
    reason=#{inspect(reason)}
    stage=#{inspect(detail.stage)}
    message=#{detail.message}
    source=#{Map.get(detail, :source, "")}
    """)

    {:error, reason}

  {:error, reason} ->
    {:error, reason}
end
```

Example CSS failure:

```elixir
{:error,
 {:invalid_css,
  %{
    stage: :css,
    reason: :invalid_css,
    message: ~s(line 1: declaration "display: table-row-group" is invalid or unsupported),
    line: 1,
    column: 1,
    source: "display: table-row-group"
  }}} =
  HtmlToPdf.render(~s(<p style="display: table-row-group">Bad</p>))
```

Example HTML failure:

```elixir
{:error,
 {:unsupported_html,
  %{
    stage: :html,
    reason: :unsupported_html,
    message: ~s(line 2: HTML tag "<script>" is unsupported),
    line: 2,
    column: 1,
    source: "<script>"
  }}} =
  HtmlToPdf.render("""
  <p>Before</p>
  <script>alert("bad")</script>
  """)
```

The broad reason is intended for program flow. The detail map is intended for logs, UI feedback, and fixing templates.
