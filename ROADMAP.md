# Roadmap

Native Elixir PDF Utilities renders application documents and edits existing
PDFs. After 1.0, we want to make it easier to use existing HTML and CSS, work
with PDFs from other applications, and build document workflows in Elixir.

The versions below are planned targets. Scope and version numbers may change
as the work develops. These milestones describe future work; the
[user guide](docs/README.md) covers what is available today, and
[CHANGELOG.md](CHANGELOG.md) lists published releases.

Version 1.x will preserve the documented public API and supported workflows.
Minor releases add compatible features, patch releases fix bugs, and intentional
breaking changes require a new major version.

## 1.1.0: Near-complete HTML compatibility

Support nearly all HTML features that have a useful representation in a PDF.
Follow the HTML specification's parsing rules, including optional closing tags,
valid attribute formats, character references, and document structure. Expand
support for document elements, links, images, embedded content, and forms.

The goal is to render existing static HTML documents with little need to rewrite
them for this library. Publish a coverage guide so users can see which parts of
the HTML specification work and which still need attention. Applications will
continue to control remote asset fetching through the existing asset interfaces.

## 1.2.0: Near-complete CSS compatibility

Support nearly all CSS features relevant to printed documents. This includes
selectors, the cascade, units and calculations, flex and grid layouts, tables,
floats, positioning, backgrounds, borders, and transforms. Improve print styling
with page rules, headers and footers, counters, and predictable page breaks.

Typography should work across languages, including right-to-left text, scripts
that require shaping, Unicode line breaking, hyphenation, and emoji sequences.
The goal is to reuse ordinary stylesheets and produce consistent PDFs for
complex documents. Document coverage and compare supported rendering against
Chromium as these capabilities grow.

## 1.3.0: PDF specification compatibility and conformance

Aim for near-complete support for the document features in the PDF specification
versions we support. Broaden reading and writing support for fonts, images,
colour, transparency, annotations, metadata, embedded files, accessibility
structure, and document security.

Generated and edited PDFs should meet the applicable specification requirements
and work consistently in independent PDF viewers and tools. Preserve document
features such as form fields, bookmarks, page labels, and attachments when an
operation should keep them. Publish the supported PDF versions and feature
coverage, with conformance checks and fixtures from other PDF producers.

## 1.4.0: Document comparison and change reports

Compare two PDFs and explain what changed. Detect changes to text, images,
layout, page order, and form values. Produce a visual comparison for people to
review and a structured report that applications can process.

Use this to review a revised purchase order, identify changes between contract
versions, or check whether a template update changed the generated document.
Help users distinguish meaningful changes from differences in metadata or PDF
encoding that leave the document looking the same.

## 1.5.0: PDF rendering and previews

Render existing PDF pages as images so applications can display previews,
thumbnails, and selected page regions. Provide control over resolution and page
selection, with an easy way to create a sheet of thumbnails for a whole document.

The goal is to let an Elixir application show users what a PDF looks like without
opening a separate viewer. Applications can also use these page images for
document comparisons and image-based processing of scanned documents.

## 1.6.0: Complete form authoring and editing

Turn an existing PDF into a fillable form. Create, move, resize, rename, and
remove fields, and give applications control over field types, defaults, and
appearance. Help users place fields by page coordinates and identify likely
field locations in existing documents.

Improve the existing filling and flattening workflows so text fits predictably
and fields retain suitable fonts and styling. A user should be able to take a
supplier's form, make it fillable, populate it with application data, and produce
a finished document through the same library.

## 1.7.0: OCR and searchable scanned PDFs

Recognise text in scanned PDFs and image-based pages. Return the recognised text
with its position on the page, and add a searchable text layer while preserving
the original scan's appearance. Support language selection and report recognition
confidence where the OCR engine provides it.

This should make scanned invoices, receipts, and archived documents searchable
and allow users to copy their text. Applications can also use the results for
document indexing and data extraction. Document any OCR engine requirements and
the effect of scan quality on the results.

## 1.8.0: Digital signing and signature verification

Sign PDFs with certificates and verify signatures on PDFs received from others.
Use standard PDF signatures that independent tools, including Acrobat, can
verify. Support timestamps and explain whether the signed content is intact,
whether later changes occurred, and whether the signer is trusted.

Support bring your own key. Callers provide their own private key and matching
certificate chain, or use a signing callback so a hardware device or external
key service can keep the private key. Make it possible to test signing with
locally generated certificates and independently verify the results. Recipients
can trust a certificate they have checked themselves or use an established
certificate provider.

## 1.9.0: Secure redaction and document sanitisation

Permanently remove sensitive text, images, and selected page regions, including
the underlying data that extraction tools could read. Let applications remove
hidden information such as metadata, attachments, comments, and earlier
document revisions.

Write a clean PDF containing only the content that should remain, and verify
the result with independent inspection and extraction tools. Use this to prepare
documents for sharing when they contain personal information, internal notes,
or confidential pricing.

## 1.10.0: Structured PDF data extraction

Extract document content as useful structures, including paragraphs, headings,
tables, and reading order. Let applications describe the fields or document
templates they want to recognise, then return records with source page numbers
and positions for review.

The goal is to turn an invoice or purchase order into application data, including
supplier details, totals, and line items. Build on text extraction and OCR so
the workflow can handle both generated PDFs and scanned documents. Report
uncertain results so applications can ask for review before accepting the data.
