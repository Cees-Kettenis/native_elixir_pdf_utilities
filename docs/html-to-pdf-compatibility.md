# HTML and CSS support

Use the renderer for reports, invoices, statements, forms, and labels. Start
with [a working example](html-to-pdf-examples.md#render-html), then choose the
features you need below. Unsupported HTML tags and CSS properties return an
[error](diagnostics.md). Some accepted `@page` declarations have no rendering
effect; see [page declarations without an effect](#page-declarations-without-an-effect).

## HTML support

The renderer prints the initial static document. It does not run JavaScript,
fetch remote resources, or simulate clicks, scrolling, submission or navigation.

| Content | Supported elements and behavior |
| --- | --- |
| Document | `html`, `head`, `body`, `title`, supported `meta`, `style`, and an HTML doctype |
| Sections and text | Headings, paragraphs, ordinary section containers, preformatted text, inline emphasis, quotations and character references |
| Lists | Ordered, unordered and description lists, including nested lists and authored numbering |
| Tables | Captions, column groups, row groups, spanning cells and repeated headings. See [tables](#tables). |
| Images | PNG, JPEG and SVG; supported `picture`, `srcset` and `sizes` source selection. See [images](#images-and-backgrounds). |
| Links | Supported URI and document-fragment destinations, including static image maps |
| Forms | Initial control artwork and supported editable PDF fields. See [forms](#forms). |
| Disclosure content | Initial open/closed `details` and nonmodal `dialog`, plus authored static popover artwork |
| Composition | Supported declarative shadow roots, slots and initial selected content; unused template contents stay inert |
| Embedded content | Authorized local HTML, plaintext and supported image child documents in isolated viewports |
| Math | Supported MathML through native layout and PDF text; unsupported structures return diagnostics |
| Custom containers | Valid unregistered HTML custom names with ordinary inline defaults and supported transparent contents |

Use `id`, `class` and `style` for styling. Supported attributes retain their
ordinary initial-state meaning. Boolean attributes depend on presence, so
`disabled="false"` still disables a control and `open="false"` still opens details.

HTML must be UTF-8. A leading BOM and supported HTML doctypes are accepted.
`html`, `head` and `body` wrappers and standard optional paragraph, list, option
and table end tags may be omitted. Duplicate attributes, invalid structure and
unsupported executable content return [diagnostics](diagnostics.md).
Comments paint nothing. Plain text and supported inline content may appear
directly in the body. An empty document produces one blank page.

### Text, lists and metadata

Preformatted text preserves spaces, line breaks and default eight-space tab
stops. `pre`, `listing`, `code`, `samp`, `kbd` and `tt` use the bundled monospace
family by default. Author CSS can change the font.

Ordered lists support `start`, `reversed`, per-item `value` and decimal,
alphabetic or Roman markers. Nested unordered lists use disc, circle and square
markers. Supported CSS marker types include decimal-leading-zero and `none`.
Inside markers wrap with the text; outside markers retain item padding.

Underline, overline and line-through support solid strokes with authored color
and thickness. Decorations follow wrapping, relative positioning and pagination.
Interrupted inline fragments preserve backgrounds, borders, padding and links
around nested blocks. Inline blocks use their last normal-flow baseline; empty
or clipped inline blocks use the bottom margin edge. Supported vertical alignment
includes top, middle, bottom and numeric offsets.

The first nonempty HTML title supplies the PDF title. Head author, description
and keyword metadata supply corresponding PDF information unless caller metadata
overrides them. Shadow, child-document and unused-template metadata do not replace
the enclosing document's metadata. See [metadata options](html-to-pdf-examples.md#set-pdf-metadata).

`lang` and supported Content-Language metadata affect language inheritance and
`:lang()` matching. Directional layout and complex-script shaping remain outside
the supported text profile.

### Visibility and composition

`display:none` suppresses painting. The global `hidden` attribute supplies a
lower-priority display default that author CSS can override. `hidden="until-found"`
and supported `content-visibility` rules preserve the initial box while suppressing
its contents. `inert` preserves artwork and extractable text while suppressing
active PDF fields and links.

Closed details, closed dialogs, unused templates and initial hidden popovers
omit their hidden artwork and fields. Hidden markup still undergoes input
validation. Executable scripts and event-handler attributes remain unsupported,
including in hidden content. Non-executable script data blocks retain literal
text and default to hidden. Canvas renders supported authored fallback content.

Declarative shadow roots and slots provide initial static composition. Document
styles and shadow styles retain their own scope. Initial selected-content copies
follow the selected option. Registration, custom-element upgrades, scripts and
later DOM updates are not performed.

### Embedded documents and SVG

Use `srcdoc` or authorized local HTML for supported iframe child documents.
`object`, `embed` and bounded zero-border framesets support their admitted static
child-document profiles. Child documents share resource limits and remain clipped
to their viewport. Unsupported MIME types, resource failures and unsupported
child behavior return diagnostics. Use `forms: :static` for child control artwork.
Embedded PDF rendering and general browser frameset behavior are unsupported.

SVG artwork uses Resvg and is embedded as PNG. SVG text is rasterized and internal
SVG links are inert; an ordinary HTML link around the image remains active.
`foreignObject`, non-scaling strokes and SVG2 inline `textPath` path data return
`unsupported_html`. Use a same-document path reference for supported `textPath`
content. Other SVG features may differ from browser rendering. See
[Images](image-processing.md) for image behavior and restrictions.

### Forms

Text-entry controls, textareas, checkboxes, radios and supported single/multiple
selects create editable AcroForm fields by default. Buttons produce static artwork.
Hidden inputs produce neither artwork nor fields. Set `forms: :static` when you
want initial artwork without editable fields.

Initial values follow the supported input state's sanitation rules. Invalid
initial values may still render and participate in `:valid` or `:invalid` matching.
HTML `pattern`, required, bounds and step constraints describe initial HTML state;
filling an existing PDF uses the PDF form API.

Number inputs use PDF text fields. Range, temporal, color, file and image controls
have bounded initial-state artwork and interactive restrictions. Mutable sliders
and temporal pickers are unsupported. Temporal artwork uses the English/en-US or
unspecified-locale profile. Unsupported control modes return a diagnostic; use
static mode when appropriate.

Readonly and disabled controls preserve their applicable field restrictions.
Names must satisfy PDF field-name rules, and separate controls must not accidentally
share a name. Radio groups follow form ownership and effective initial checkedness.
See [PDF forms](pdf-forms.md) for naming, values, filling, flattening and limitations.

## CSS support

| I need | Supported CSS |
| --- | --- |
| Select elements | Tag, class, ID, universal, attribute presence, equality, token/dash/prefix/suffix/substring operators and case flags, descendant and child selectors; comma groups; `:lang()` with range lists, escapes and wildcard subtags; simple `:not()`; `:root`; first/last child and type selectors; integer An+B for `:nth-child`, `:nth-last-child`, `:nth-of-type`, `:nth-last-of-type`; `:only-child` and `:only-of-type` |
| Reuse styles | The cascade, `!important`, inherited text styles, custom properties, `var()`, `currentColor` |
| Arrange content | Block, inline, inline-block, flex, and grid layouts; relative and absolute positioning; `z-index` |
| Set sizes and spacing | Width/height, min/max sizes, `min()`, aspect ratio, box sizing, margins, padding, gaps |
| Style text | Font family/size/weight/style, line height, alignment, text transform, letter spacing, line and word breaking, `white-space: normal`, `pre-line`, `nowrap`, `pre`, `pre-wrap`, or `break-spaces` |
| Decorate text | `text-decoration`, `text-decoration-line`, `text-decoration-color`, `text-decoration-thickness: auto/from-font/length/percentage`, and solid `text-decoration-style` |
| Style lists | `list-style-type`, `list-style-position: inside/outside`, and the supported type/position forms of `list-style` |
| Decorate boxes | Colors, borders, rounded corners, background images, sizing, positioning, and repetition |
| Generate text | `::before`, `::after`, quoted `content`, `attr()`, `counter()`, counter reset and increment |
| Control pages | Bare `@page` size/margins, print media rules, page breaks, and best-effort `break-inside: avoid` |

Attribute selectors support the applicable HTML case rules and explicit `i`
or `s` flags. Supported initial control selectors include enabled/disabled,
checked, required/optional and valid/invalid states. Positional selectors count
authored element siblings, including hidden elements. Namespace selector syntax
and the An+B `of` selector-list syntax remain unsupported.

Body lengths support `pt`, `px`, `mm`, `cm`, `in`, `rem`, supported percentages,
and unitless `0`. Leading decimals such as `.5mm` are accepted. Letter spacing
also accepts `em`. Flex and grid support gaps, alignment, ordering, and
track/item sizing, but not every browser layout rule.

`white-space: nowrap` collapses whitespace and prevents automatic wrapping.
Explicit `<br>` breaks still apply; long lines can overflow their container.
`break-spaces` retains ordinary Unicode line opportunities, including CJK and
word hyphens, and allows a break after each preserved space, tab or other
Unicode space separator except no-break space. Spaces keep their measured
width at line ends. These opportunities and styled inline boundaries share
the same Unicode line breaking validator.

## Render options

Pass options to `HtmlToPdf.render(html, options)` or `render_file/3`.

| Option | Use it to | Default | Supported values |
| --- | --- | --- | --- |
| `:page_size` | Set page dimensions | `:a4` | Named size, orientation tuple, size string, or numeric tuple. See [page sizes](#page-size-and-margins). |
| `:margin` | Set page margins | `0` | Nonnegative point number, CSS string, or side map. See [margin formats](#page-size-and-margins). |
| `:stylesheets` | Add CSS | `[]` | List of `{:css, css}` or `{:file, path}` entries |
| `:base_url` | Allow local images/fonts | None | Directory path such as `"priv/static"`. See [asset sources](#assets-and-local-files). |
| `:assets` | Map asset references | `%{}` | Map of references to `{:bytes, binary}` or `{:file, path}` |
| `:asset_resolver` | Resolve asset references | None | One-argument function or `nil`. See the [callback contract](#assets-and-local-files). |
| `:default_font` | Choose text fonts | `"DejaVu Sans"` | Family name string or a list of family names in fallback order |
| `:fonts` | Register TrueType fonts | `[]` | List of font definitions with `:family` and `:path` or `:data`. See [font registration](html-to-pdf-examples.md#register-a-font). |
| `:system_font_discovery` | Look up installed fonts | `true` | `true` or `false` |
| `:compress_streams` | Compress eligible PDF streams when smaller | `true` | Boolean; set `false` to favor render speed over file size |
| `:subset_fonts` | Omit unused TrueType outlines | `true` | Boolean; set `false` to embed full fonts |
| `:unsupported_glyphs` | Handle missing characters | `:replace` | `:replace` or `:error` |
| `:forms` | Create AcroForm fields | `:interactive` | `:interactive` or `:static`; see [PDF forms](pdf-forms.md). |
| `:metadata` | Set document information | HTML title when present | Metadata fields such as `:title` and `:author`. See [metadata options](html-to-pdf-examples.md#set-pdf-metadata). |
| `:outlines` | Add PDF bookmarks | Disabled | `:headings`, bookmark list, `false`, or `nil`. See [bookmark examples](html-to-pdf-examples.md#create-bookmarks-from-headings). |
| `:page_furniture` | Add headers and footers | Disabled | Keyword list or map of templates. See [template options](html-to-pdf-examples.md#add-headers-footers-and-page-numbers). |

### Page size and margins

| Page-size format | Supported values or example |
| --- | --- |
| Named size | `:a5`, `:a4`, `:a3`, `:b5`, `:b4`, `:jis_b5`, `:jis_b4`, `:letter`, `:legal`, `:ledger` |
| Named size with orientation | `{:a4, :landscape}` or `{:a4, :portrait}` |
| Size string | `"a4 landscape"` or custom dimensions such as `"100mm 60mm"` |
| Numeric `{width, height}` | `{8.5, 11}` uses inches; `{612, 792}` uses points. Both values use inches when both are at most 20; otherwise both use points. |

Prefer size strings with explicit units for custom dimensions.

| Margin format | Example | Meaning |
| --- | --- | --- |
| Point number | `36` | 36 points on every side |
| CSS string with one value | `"12mm"` | Same margin on every side |
| CSS string with two values | `"18mm 12mm"` | Top/bottom, then left/right |
| CSS string with three values | `"18mm 12mm 10mm"` | Top, left/right, bottom |
| CSS string with four values | `"18mm 12mm 10mm 8mm"` | Top, right, bottom, left |
| Side map | `%{top: 36, bottom: "12mm"}` | Set individual sides; omitted sides are zero |

Page-size and margin strings accept `pt`, `px`, `mm`, `cm`, `in`, `q`, and `pc`.
Explicit `:page_size` and `:margin` options override `@page`.

### Page declarations without an effect

Only `size` and page-margin declarations that can be normalized to the formats
above change the output. The parser also accepts these page-context declarations
without applying them:

| Accepted declaration | What is not applied |
| --- | --- |
| `page-orientation`, `marks`, `bleed` | Page rotation, printer marks, and bleed settings |
| Other accepted page-context properties, such as backgrounds, borders, fonts, counters, padding, and width/height | Page decoration, typography, counters, and box layout specified inside `@page` |
| Page margins using percentages, relative units such as `em` or `vw`, negative lengths, or `auto` | Margin values that cannot become nonnegative absolute point lengths |
| `size: auto`, a single-length `size`, relative page sizes, CSS-wide keywords, and functions such as `calc()` or `var()` in page size/margins | Values that cannot be normalized to a supported page size or margin |

These declarations leave earlier applicable page settings, explicit render
options, or library defaults in effect. For example, this rule is accepted but
does not rotate the page or apply a percentage margin:

```css
@page { page-orientation: rotate-left; margin: 10%; }
```

Use `size: A4 landscape` to change page orientation and an absolute margin such
as `12mm`, or pass explicit `:page_size` and `:margin` options. Successful
rendering alone does not confirm that every `@page` declaration was applied.
Malformed declarations, unknown page properties, and named or selector-based
page rules still return errors.

## Tables

| Feature | Support and limits |
| --- | --- |
| Column sizing | Automatic sizing or `table-layout: fixed` |
| Spanning cells | `colspan` and `rowspan`; a `rowspan` ends at its row-group boundary |
| Nested tables | Supported inside cells |
| Borders | Separate or collapsed borders; collapsed tables retain the requested bottom border at page breaks |
| Headers across pages | Rows in `<thead>` repeat when the table continues on another page |
| Keeping row groups together | Set `break-inside: avoid` on each `<tbody>` that should stay together; groups too tall for a page split between rows |

### Table page breaks

Table width/height, spacing, cell padding, background color and supported
alignment hints have lower priority than authored CSS. Captions support top and
bottom placement. Row and group background layers preserve separated gaps and
colspan coverage. Baseline cells use measured flow baselines. Descendants with
percentage heights, including percentage calculations, resolve their baseline
after row, cell and spanning allocation. Legacy border/frame/rules hints enter
below authored CSS. Collapsed edges
resolve hidden suppression, width, style, origin and positional conflicts across
cells and spans. All-none grids retain their geometry and background layers
without computing paint edges. Ragged collapsed grids retain empty-slot border geometry. CSS table formatting
creates anonymous rows and cells around admitted visible content without changing
authored selector matching.

Fitting inline-block tables remain together. Oversized atomic table lines move
to a fresh page and slice their continuous print geometry across pages. Captions,
rowspans and links retain source placement; this monolithic path does not repeat
headers. A fixed-height atomic owner keeps its allocated flow height. Visible
table overflow continues across pages beside following flow; hidden overflow
stays clipped to the owner. Ordinary block tables retain their existing row
fragmentation and repeated-header behavior. This follows the permitted monolithic slicing policy
in [CSS Fragmentation 3](https://www.w3.org/TR/css-break-3/#unforced-breaks).

Page breaks use measured content height. The printable area is the page height minus its top and bottom margins.

- `<thead>` repeats when a table continues onto another page and
  stays with the first body row when both fit. Body rows do not repeat.
- Set `break-inside: avoid` on each `<tbody>` to keep its rows
  together. A group that fits on a fresh page with its heading moves there if
  needed. Larger groups split between rows. Without this CSS, groups can split even when they would fit on a fresh page.
- With `border-collapse: collapse`, each page's final cells retain
  their configured bottom border. `border-bottom: none` remains absent.

A single row that cannot fit on a fresh page with its heading may overflow;
this behavior does not split individual rows. Divide its content into multiple
rows or reduce its height before rendering.

See the [grouped table example](html-to-pdf-examples.md#paginate-a-table-with-row-groups).

## Images and backgrounds

| Format | Supported content |
| --- | --- |
| JPEG | Grayscale, RGB, and CMYK |
| PNG | Greyscale, RGB, indexed palettes, greyscale with alpha, and RGBA at all legal static PNG sample depths, including Adam7 interlacing. Color and alpha retain 16-bit precision. Chunk CRCs, ordering, palettes, transparency, and scanlines are validated. |
| SVG | Self-contained SVG through Resvg, with bounded source size, raster dimensions, nodes, nesting, path data, filters, and references. DTD/entity declarations and escaped CSS resource syntax are rejected; resources require explicit authorization. |

| Image styling | Supported values or behavior |
| --- | --- |
| `object-fit` | `fill`, `contain`, `cover`, `none`, or `scale-down` |
| `object-position` | Position an image within its box |
| `background-size` | Explicit sizes, `cover`, or `contain` |
| `background-position` | Position a background within its box |
| `background-repeat` | `repeat`, `repeat-x`, `repeat-y`, or `no-repeat` |

Responsive image selection uses supported `picture` sources, `srcset` candidates
and `sizes`. Static printing resolves visible lazy-auto images using their final
layout width. It does not simulate browser loading delays or scrolling.

### Assets and local files

| Source | How to supply it | Rules |
| --- | --- | --- |
| Local directory | `base_url: "priv/static"` with `src="images/logo.png"` | References are checked for traversal and symlinks beneath the directory. `render_file/3` does not infer this directory. |
| Explicit asset map | `assets: %{"logo.png" => {:bytes, bytes}}` or a `{:file, path}` value | Mappings take precedence; mapped files do not require `:base_url`. |
| Resolver callback | `asset_resolver: fn request -> ... end` | Used when a local reference fails path checks or cannot be read. Resource-limit failures do not fall back. |
| Data URI | Put a supported data URI in the image reference | Image format restrictions still apply. |

The resolver receives `%{reference: reference, kind: kind}`, where `kind` is
`:image`, `:background_image`, or `:font`.

| Resolver return | Meaning |
| --- | --- |
| `{:ok, bytes}` | Use the supplied asset bytes |
| `:not_found` | The resolver could not find the asset |
| `{:error, reason}` | Asset resolution failed |

The renderer never downloads remote assets itself. Caller-configured font
paths and stylesheet files do not require `:base_url`.
Local image and font reads through `:base_url` work on Windows, macOS, and Linux. Paths are checked for traversal and existing symlinks before
bounded file reads. These checks cannot prevent another process from changing
a path during a read. Use asset directories that untrusted users cannot modify,
or supply approved bytes with `:assets` or `:asset_resolver`.
Explicit file mappings, configured font paths, and stylesheet paths also use
bounded reads and trust the caller's pathname.

See [image and asset examples](html-to-pdf-examples.md#load-local-images).

## Fonts and text

Choose a font with CSS `font-family` or the `:default_font` render option.

| Font source | How to use it |
| --- | --- |
| Bundled | DejaVu Sans is included and used by default. |
| Installed on the rendering machine | System-font discovery is enabled by default. Use an installed family by name, such as `font-family: "Liberation Sans"` or `default_font: "Liberation Sans"`, if that family is installed. No `:fonts` registration is needed. |
| Supplied by your application | Register a static TrueType font with `:family` and either `:path` or `:data`. Optional `:weight` and `:style` default to `400` and `:normal`. |

Installed fonts must meet the same [font-format and embedding requirements](#known-limits)
as supplied fonts. For consistent font selection across machines, supply your
fonts and set `system_font_discovery: false`.
See [Register a font](html-to-pdf-examples.md#register-a-font).

Bundled DejaVu Sans Mono faces and STIX Two Math Regular load on demand.
Use `font-family: "STIX Two Math"` or `font-family: math` to select the pinned
math font for text. Explicitly supplied faces take precedence. TrueType Unicode
format-4 and format-12 character maps support supplementary characters when the
selected font contains them. MathML support remains limited to the admitted
markup and layout profile; selecting a math font does not add unsupported elements.

Missing characters use available fallback fonts, then U+FFFD replacement.
Set `unsupported_glyphs: :error` to fail instead of replacing them.

The renderer subsets TrueType fonts and compresses eligible PDF streams by default.
Subsetting keeps glyph IDs unchanged and includes the outlines needed by composite
glyphs. Fonts whose license forbids subsetting remain fully embedded. Set
`subset_fonts: false` or `compress_streams: false` when render time matters more
than output size.

## Pagination

Pages break automatically or at supported CSS page breaks. Use
`:page_furniture` for running headers, footers, and page numbers. Reserve enough
margin for the templates; they do not push body content out of the way.

Paragraph line groups honor inherited positive-integer `orphans` and `widows`,
each defaulting to two. Root auto-height body bottom padding participates in
pagination; embedded child bodies retain their isolated viewport extent.

Templates can vary on the first, odd, and even pages. A matching `false` or
`nil` variant hides that template. See [headers and footers](html-to-pdf-examples.md#add-headers-footers-and-page-numbers).
For PDF viewer bookmarks, see [outlines from headings](html-to-pdf-examples.md#create-bookmarks-from-headings).

## Known limits

| Not supported | What to use instead |
| --- | --- |
| JavaScript, canvas drawing APIs, video, audio, plugin embeds and general framesets | Static HTML, supported images, authorized local HTML child viewports and bounded zero-border framesets |
| XFA, JavaScript form actions | Use supported AcroForm fields or static artwork |
| Floats, fixed positioning, transforms, animations | Block, flex, grid, relative, or absolute layout |
| Named pages and CSS page-margin boxes | Page options and header/footer templates |
| CSS `counter(page)` or `counter(pages)` | `{{page}}` and `{{pages}}` in header/footer templates |
| Animated PNG playback | The PNG default image is rendered; animation frames are not played. |
| WOFF/WOFF2, variable fonts, CFF OpenType | Static TrueType fonts that permit embedding |
| Complex shaping or bidirectional text | A renderer with shaping support for Arabic, Indic scripts, Thai, and complex emoji sequences |

The renderer supports a document-oriented subset rather than complete browser
compatibility. Flex/grid sizing, intrinsic sizing, complex inline content and
MathML have additional limits. Unsupported combinations return diagnostics.
Accepted SVG artwork and native control themes may differ from browser output.
Use [examples](html-to-pdf-examples.md) as a starting point and check your output
with the fonts and assets your application will supply.

See [Resource limits](resource-limits.md) for configurable size and processing
limits, and [Diagnostics](diagnostics.md) for handling failures.
