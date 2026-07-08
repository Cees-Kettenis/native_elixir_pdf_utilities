# Roadmap

This roadmap lists practical improvements that would make Native Elixir PDF
Utilities more useful for real Elixir applications. The project should keep its
current identity: a native, predictable PDF toolkit for app-generated documents,
not a full browser engine.

## Direction

The highest-value path is to improve common document workflows:

- render existing invoice, label, report, and statement templates with fewer edits
- support predictable print-oriented layout features
- add PDF manipulation tools that applications commonly need after rendering
- keep unsafe or network-dependent behavior explicit and opt-in

## HTML to PDF Rendering

### Print Styles

Support `@media print` blocks and ignore screen-only rules. Many existing
templates already include print CSS, so this would reduce the amount of template
rewriting callers need to do.

Example:

```css
@media print {
  .screen-only {
    display: none;
  }
}
```

### CSS Font Loading

Add `@font-face` support so templates can declare fonts in CSS instead of only
through render options.

Example:

```css
@font-face {
  font-family: Inter;
  src: url("./Inter-Regular.ttf");
  font-weight: 400;
}
```

This should reuse the existing font registry and keep local file loading rules
strict.

### Running Headers, Footers, and Page Numbers

Add first-class support for repeated page furniture. This is one of the most
useful features for reports, invoices, statements, contracts, and exports.

Possible API:

```elixir
NativeElixirPdfUtilities.HtmlToPdf.render(html,
  header_html: "...",
  footer_html: "...",
  page_numbers: true
)
```

Useful follow-up features:

- current page number
- total page count
- first-page-only or except-first-page header/footer options
- different odd/even page headers

### Generated Content

Support common pseudo-elements:

- `::before`
- `::after`
- `content`
- `attr(...)`

Example:

```css
.required::after {
  content: "*";
}

a::after {
  content: " (" attr(href) ")";
}
```

### More Selectors

Add selectors commonly found in document templates:

- `[attr]`
- `[attr=value]`
- `:not(...)`
- `:nth-child(odd)`
- `:nth-child(even)`
- `:first-of-type`
- `:last-of-type`

These should stay strict. Unsupported selectors should continue returning clear
diagnostics instead of being silently ignored.

### Positioning

Add a limited positioning model for document use cases.

Start with:

- `position: absolute`
- `top`
- `right`
- `bottom`
- `left`
- `z-index`

Then consider:

- `position: fixed`

This would help labels, badges, watermarks, forms, and page furniture. Fixed
positioning is especially useful for repeated headers and footers.

### Image Sizing

Support browser-like image fitting:

- `object-fit: contain`
- `object-fit: cover`
- `object-position`

This would make product images, logos, and thumbnails easier to use in fixed
size boxes.

### Background Images

Support local background images for branded PDFs, certificates, letterheads, and
forms.

Useful CSS properties:

- `background-image`
- `background-size`
- `background-position`
- `background-repeat`

Remote background images should follow the same safety model as normal images.

### Opt-In Remote Assets

Keep remote asset loading disabled by default. Add an explicit opt-in API so
callers control security, timeouts, authentication, and caching.

Possible API:

```elixir
NativeElixirPdfUtilities.HtmlToPdf.render(html,
  allow_remote_assets: true,
  asset_fetcher: &MyApp.fetch_asset/1
)
```

The renderer should avoid owning HTTP policy directly when possible. A caller
provided fetcher gives applications control over allowed hosts, headers,
timeouts, and caching.

## Typography

Text rendering is one of the hardest parts of browser-like output. Improve it in
stages:

1. Better Unicode line breaking.
2. Soft hyphen support.
3. Optional hyphenation dictionaries.
4. `direction: rtl`.
5. Basic bidirectional text layout.
6. Optional complex shaping for Arabic, Indic scripts, Thai, emoji sequences,
   and other advanced typography.

The first few steps would improve many documents without requiring a large text
shaping engine immediately.

## PDF Utility Features

These features are useful outside the HTML renderer and would make the package a
broader PDF toolkit.

### PDF Information

Expose common document information:

```elixir
NativeElixirPdfUtilities.Info.page_count(pdf)
NativeElixirPdfUtilities.Info.metadata(pdf)
```

Useful fields:

- page count
- page sizes
- title
- author
- producer
- creation date
- modification date
- encryption status

### Split PDFs

Split a PDF into ranges or individual pages.

Example:

```elixir
NativeElixirPdfUtilities.Split.pages(pdf, ranges: [1..3, 7])
```

### Reorder and Delete Pages

Allow callers to pick pages in a new order.

Example:

```elixir
NativeElixirPdfUtilities.Transform.pick_pages(pdf, [3, 1, 2])
```

This is useful for building packets, removing cover pages, or extracting
selected documents from a merged PDF.

### Rotate Pages

Rotate selected pages.

Example:

```elixir
NativeElixirPdfUtilities.Transform.rotate(pdf,
  pages: :all,
  degrees: 90
)
```

### Stamp and Watermark

Add text or PDF overlays to existing documents.

Examples:

```elixir
NativeElixirPdfUtilities.Stamp.text(pdf, "CONFIDENTIAL")
NativeElixirPdfUtilities.Stamp.pdf(pdf, overlay_pdf)
```

This is useful for drafts, approvals, internal documents, and branded output.

### Add Page Numbers to Existing PDFs

Add page numbers after rendering or merging. This should support position,
format, font, size, color, and page ranges.

Example:

```elixir
NativeElixirPdfUtilities.Stamp.page_numbers(pdf,
  format: "Page {page} of {total}",
  position: :bottom_center
)
```

### Form Filling

Support filling AcroForm fields and optionally flattening the result.

Example:

```elixir
NativeElixirPdfUtilities.Forms.fill(pdf, %{
  "customer_name" => "Acme Inc."
})

NativeElixirPdfUtilities.Forms.flatten(pdf)
```

This would be valuable for generated government forms, applications, contracts,
and operational documents.

### Attachments

Embed files inside a PDF. This is useful for invoices with XML or JSON sidecar
data, audit packages, and document bundles.

Example:

```elixir
NativeElixirPdfUtilities.Attachments.add(pdf,
  filename: "invoice.xml",
  content: xml
)
```

### Compression and Optimization

Add basic output optimization:

- compress uncompressed streams
- deduplicate repeated images
- remove unreachable objects when possible
- optionally subset embedded fonts

This should be conservative and should not change visual output.

### Archive-Friendly Output

Consider an archive-friendly mode before attempting full PDF/A compliance.

Useful steps:

- embed required fonts
- avoid unsupported transparency where possible
- include document metadata
- expose validation diagnostics

Full PDF/A support is a larger project, but an explicit archive-friendly mode
would still be useful to callers.

## Suggested Priority

1. `@font-face`
2. running headers, footers, and page numbers
3. `@media print`
4. `position: absolute`
5. background images
6. PDF page count and metadata
7. split, reorder, and delete pages
8. watermark and stamp existing PDFs
9. add page numbers to existing PDFs
10. form filling

This sequence improves common user-facing document workflows first, then grows
the package into a stronger general-purpose PDF utility library.

## Versioning Path

The project follows a SemVer-style release policy once the public API is stable:

- `1.0.0` is the first stable release and defines the supported public API.
- `1.1.0` is a backwards-compatible minor release with new capabilities or
  improvements.
- `1.1.1` is a patch release for bug fixes.
- `2.0.0` is required after `1.0.0` when a release intentionally breaks the
  public API.

Before `1.0.0`, the package remains in initial development. Breaking public API
changes can still happen in `0.x` releases, but they should be called out
clearly in the changelog.

Feature completion alone does not decide `1.0.0`; API stability does. A stable
release is reasonable once the core rendering, merge, text extraction, and
tokenizer APIs feel settled, the supported HTML/CSS surface is documented,
diagnostics and error shapes are unlikely to change casually, and realistic
fixtures cover the workflows the project promises.

A practical path is:

1. `0.4.0` - finish must-have renderer improvements such as CSS font loading,
   running headers and footers, page numbers, and print CSS polish.
2. `0.5.0` to `0.8.0` - continue roadmap features, document behavior, and clean
   up naming, options, return values, and diagnostics.
3. `0.9.0` - release candidate and API freeze, except for bug fixes and final
   documentation corrections.
4. `1.0.0` - stable public API that users can depend on.
