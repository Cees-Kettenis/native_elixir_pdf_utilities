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
    stylesheets: ["priv/static/templates/invoice.css"],
    base_url: "priv/static"
  )
```

Configured stylesheets are loaded before embedded `<style>` tags. This lets shared print CSS define defaults while the template keeps document-specific overrides close to the markup.

## Images

Local PNG/JPEG paths can be absolute or relative to `:base_url`. SVG data URIs are accepted and rasterized locally.

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

Built-in PDF fonts are available without setup. For Unicode-heavy documents, pass explicit TTF fonts.

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
@page { size: A4 landscape; margin: 7mm; }

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
