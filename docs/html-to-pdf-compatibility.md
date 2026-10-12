# HTML and CSS support

Use the renderer for reports, invoices, statements, forms, and labels. Start
with [a working example](html-to-pdf-examples.md#render-html), then choose the
features you need below. Unsupported HTML tags and CSS properties return an
[error](diagnostics.md). Some accepted `@page` declarations have no rendering
effect; see [page declarations without an effect](#page-declarations-without-an-effect).

## HTML support

| Content | Supported elements |
| --- | --- |
| Document | `html`, `head`, `body`, `title`, `meta`, `style`, and an HTML doctype |
| Headings and sections | `h1` through `h6`, `p`, `div`, `section`, `article`, `aside`, `header`, `footer`, `main`, `nav`, `address`, `blockquote`, `figure`, `figcaption`, `hgroup`, `search`, `center`, `pre`, `listing` |
| Inline text | `span`, `strong`, `b`, `em`, `i`, `u`, `s`, `a`, `br`, `abbr`, `cite`, `dfn`, `data`, `time`, `code`, `samp`, `kbd`, `tt`, `var`, `big`, `small`, and character references such as `&amp;` |
| Lists | `ul`, `ol`, `li`, `dl`, `dt`, `dd` |
| Tables | `table`, `caption`, `colgroup`, `col`, `thead`, `tbody`, `tfoot`, `tr`, `th`, `td` |
| Images | `img` with a `src` |
| Custom containers | Valid unregistered HTML custom names with inline defaults and supported transparent contents |
| Form controls | Text, search, telephone, URL, email, password, checkbox, radio, hidden, submit, reset, and button `input`; `select`, `option`, `textarea`, `button`, ordinary `form` and `label` containers |

Preformatted text retains spaces, default eight-space tab stops, and line breaks.
`pre` and `listing` discard one immediately following newline and default to bundled DejaVu
Sans Mono. `code`, `samp`, `kbd`, and `tt` use the same family. Fonts load only
when selected, and author CSS can override these defaults. Description items
accept their standard omitted end tags; list items can contain blocks and nested
lists. Nested unordered lists use circle and square markers. Figure captions
must appear first or last. `data[value]`, `time[datetime]`, and `blockquote[cite]`
retain semantic values without inventing printed text. Directional layout remains
a 1.2.0 dependency.

Ordered lists support `start`, `reversed`, per-item `value`, and the `1`, `a`,
`A`, `i`, and `I` marker hints. Unordered lists and items support their legacy
marker hints. Numbering excludes items hidden with `display: none` or changed
to `display: block`. Author CSS overrides marker hints. Supported marker types
are decimal, decimal-leading-zero, lower/upper alpha and Latin aliases,
lower/upper Roman, disc, circle, square, and none. Roman values outside 1..3999
and nonpositive alphabetic values use decimal representation. Marker text
receives font fallback before layout. Inside markers participate in wrapping;
outside markers retain the item's authored padding and box styling.
The legacy `compact` attribute is accepted without an appearance change.
Custom counter styles and marker images remain unsupported.

`u` and `s` supply underline and line-through defaults. CSS can add solid
underline, overline, and line-through decorations, combine lines, set their
color and thickness, or remove the element's own decoration. An ancestor's
line retains its originating color and font metrics through ordinary descendants;
inline-block content and absolutely positioned descendants start their own
propagation scope. Lines wrap and paginate with the text and move with relative
positioning. Automatic placement follows the pinned Chromium UA rules using the
selected font's ascent. `from-font` thickness uses its OpenType underline metrics
or the automatic fallback. Strokes have a one-CSS-pixel minimum, including an
authored zero thickness. Native underlines remain continuous across glyph ink,
which [CSS Text Decoration Level 3 permits](https://www.w3.org/TR/css-text-decor-3/#line-decoration).
Double, dotted, dashed, and wavy decorations, underline offsets, and ruby remain
further work and return diagnostics when requested.

The first nonempty `title` supplies the PDF title after ASCII whitespace
collapse. Head `meta[name=author]` values combine in source order as PDF author
text. The first description becomes the PDF subject. Keywords combine across
head meta elements with comma-token trimming and duplicate removal. Explicit
caller metadata takes precedence. Other supported metadata attributes remain
selector-visible and add no printed text. UTF-8 charset declarations are
accepted; other declared charsets return a diagnostic under the UTF-8 input
contract. `http-equiv` refresh, CSP enforcement/reporting, and cookie requests
return source diagnostics even with `display:none`. Advisory metadata such as
viewport, social properties, and `X-UA-Compatible` is retained without browser
navigation, policy, or session behavior.

`lang` is accepted on supported HTML elements and retains authored values.
CSS `:lang()` uses the nearest language attribute; an empty value stops inheritance.
Unknown and padded identifiers stay distinguishable, and `und` is a tagged language.
Literal HTML `xml:lang` is retained for attribute selectors and has no language
effect. Language selectors accept escaped identifiers, quoted strings, range
lists and wildcard subtags, with canonical aliases and RFC extended filtering.
An empty range matches untagged content; `"*"` does not. Language computation
uses the existing CSS work budget and a cache scoped to each style computation.
Foreign-content and shadow/slot language integration remain further work.

Content-Language pragmas use the last successfully processed value in document
order. Missing, empty and comma-containing values are ignored; otherwise the first
ASCII whitespace token supplies the default. Inert template contents do not set
that default. The current renderer has no HTTP-header language input.

Supported attributes accept empty, unquoted, single-quoted, and double-quoted
syntax. Unquoted values must be nonempty and cannot contain ASCII whitespace,
quotes, backticks, `=`, `<`, or `>`. A slash directly after an unquoted value
belongs to that value, so use `<img src=logo.png />` when writing a trailing
slash. Duplicate attribute names are rejected regardless of case or quoting.
HTML comments are accepted between elements and inside ordinary content and
paint nothing. Malformed or unterminated comments return source diagnostics.
Comment delimiters inside `style`, `title`, and `textarea` remain text.

Paragraphs, list items, options, captions, column groups, and table sections,
rows, and cells accept their valid optional end tags. `html`, `head`, and `body`
tags can be omitted. The renderer constructs their ancestry and implicit table
groups for CSS selectors. Required closing tags remain required. Plain text,
inline elements, and supported controls can appear directly in the body.
Empty documents render one blank page; empty table structures are accepted.
HTML input is UTF-8. A leading UTF-8 BOM is ignored. An HTML doctype and
`SYSTEM "about:legacy-compat"` are accepted before content; legacy quirks modes
and XML declarations remain unsupported.

`HtmlParser.parse/1` and `parse_detailed/1` preserve the existing tree format.
`parse_document_detailed/1` constructs document and table groups, as used by
`HtmlToPdf.render/2`. `parse_fragment/2` and `parse_fragment_detailed/2` accept a
supported non-void context such as `"tr"`, `"select"`, or `"p"` and return a tree
without its context wrapper. Fragment failures include shared diagnostics.
Raw-text and RCDATA contexts consume the full fragment as text.

The parser also retains SVG and MathML namespaces, adjusted foreign names and
attributes, HTML integration points, and literal foreign CDATA. Foreign elements
include `namespace` and `attribute_namespaces` fields. Use `"svg"` or `"math"`
for a root fragment context, or a map such as
`%{namespace: :mathml, tag: "annotation-xml", attributes: %{"encoding" => "text/html"}}`
for an integration point. This is parser support. SVG and MathML layout remains
under implementation, and rendering these trees returns an actionable diagnostic.

Hidden inputs retain their supported attributes in the parsed tree and produce
no printed content, spacing, generated content, or AcroForm field. Their type
is case-insensitive. Author CSS cannot make them visible, even with `!important`.
Hidden input backgrounds do not request assets. Attribute and CSS validation
still apply. This does not implement form submission or additional input types.

Input type keywords use ASCII case matching. Missing, empty, unknown, obsolete or
untrimmed values select Text, so `type="hidden "` remains visible and
`type="checKbox"` creates a text field. The original type string stays available to
CSS. Recognized states whose rendering is unfinished still return diagnostics;
these include dates, number, range, color, file and image inputs.
Defaulted Text uses the same sanitation, readonly rules, values and generated names
as explicit text inputs. Caller-supplied input trees use the same validator.

Ordinary `form` containers render as blocks and `label` containers render inline.
Submission metadata is retained without running actions. Supported controls use
the nearest ancestor form unless a `form` attribute explicitly names an ID.
The first element with that ID must be an HTML form; unresolved references and
IDs belonging to another element leave the control unowned. Labels resolve their
explicit `for` target or first supported labelable descendant. Hidden inputs
are not labelable. Nested forms and nested labels return parser diagnostics.
Complete label content rules and broader form-associated elements remain open.

Radio groups use actual form ownership and a nonempty authored name in the
ordinary HTML tree. The last authored checked radio wins. Empty or absent names
remain independent. Artwork and editable field values use that effective state;
CSS attribute selectors still see the authored `checked` attributes. Independent
owner groups sharing a PDF field name return `:unsupported_form`; use unique
names or `forms: :static`. Broader selection modes, fieldsets,
legends, optgroups, constraint states and shadow-tree integration remain open.

Ordinary single-select initial state retains the last authored selection or
defaults to the first enabled option. Explicit disabled selections remain
selected. Empty selects and all-disabled selects without a selected option
render with no selection; editable PDF choice fields omit their initial value.
Empty option values remain empty strings. Initial `:checked` matches canonical
checkbox/radio or option state; authored attributes remain unchanged. Ordinary optgroups support label metadata,
option order, omitted end tags and inherited disabled state. Nonempty option
label attributes supply display labels independently of values. Text-derived
labels and values trim and collapse ASCII whitespace while preserving NBSP;
explicit values remain intact. Empty dropdowns retain a normal font line without
an authored height. Native dropdowns reset CSS line-height to the font's normal
line height. Multiple/listbox sizing, rich/custom controls, interactive disabled
option policy, constraints and shadow integration remain open.

Supported text-entry, checkbox, radio and selection controls create editable AcroForm fields by default. Use
`forms: :static` for static artwork. See [field naming and filling](pdf-forms.md). Links support `http://`, `https://`, and `mailto:` destinations.

Search inputs use the text-field drawing and AcroForm path, with editable PDF
text values and `TEXT` generated names. Their native browser theme, search
activation, constraints and additional attributes remain incomplete.

Text, search and telephone inputs remove CR and LF from their initial value in both form modes, including
when the type is omitted. Parsed attributes and CSS selectors retain the authored
value. Other characters remain in the control value. Textarea line breaks remain
multiline content. Empty text-entry inputs retain one line of intrinsic height.

Text/search/telephone/URL/email inputs and textareas accept `readonly` by boolean presence, including
`readonly="false"`. Interactive fields expose `read_only: true` and reject filling
with `:read_only_form_field`; flattening remains available. Readonly adds no
disabled appearance. Static artwork stays identical unless author CSS selects the
attribute. Combined `readonly` and `disabled` set the read-only bit once and keep
the textarea multiline bit. Other input types and control hosts reject `readonly`.
Constraints, ownership and complete native widget themes remain unfinished.

Telephone, URL and single-email inputs use the same text-field drawing, editable
AcroForm values and `TEXT` generated names. URL/email values strip CR/LF and then
trim leading/trailing ASCII whitespace. Non-ASCII whitespace and internal spaces
remain intact. Telephone spacing and punctuation stay unchanged apart from CR/LF.
Invalid URL/email initial values still render. Constraint selector states, IDN display conversion, further attributes and ownership
remain unfinished; unsupported attributes return diagnostics.

Email inputs also accept boolean `multiple`, including `multiple="false"`.
Initial comma-separated tokens trim ASCII edge whitespace and rejoin in order;
empty tokens remain. This rule preserves internal CR/LF inside invalid tokens,
unlike single-email sanitation. The PDF field stays one text field whose value
is a string. Invalid initial list tokens still render; complete constraint state
and IDN display conversion remain unfinished. Other supported input types and
HTML listboxes still reject `multiple`.

Password inputs remove initial CR/LF without trimming and display one bullet per
grapheme. Original attributes remain visible to CSS. Interactive output uses a
Text field with the Password flag and retains the sanitized value; readonly and
disabled still prevent filling. Filling generates masked artwork, and flattening
uses current appearances. PDF field values and earlier revisions remain stored
data. Masking and flattening do not redact them. Additional password attributes,
constraints, ownership and complete native appearance remain unfinished.

Submit, reset and button inputs render static button artwork in both form modes.
Their authored value supplies the label, including empty values. Missing submit
and reset labels use English `Submit` and `Reset`; missing button labels are empty.
Labels retain spaces and line breaks, and author CSS can override their defaults.
Original attributes remain visible to selectors. Disabled presence remains available to
author selectors. Buttons create no PDF fields, reserve no names and
consume no field appearance budget. Empty input buttons retain one line; an empty
`<button>` prints no label or submission value. Submission and reset actions remain
excluded. Broader ownership, additional attributes, complete native sizing and long-label
clipping are unfinished.

Use `id`, `class`, and `style` to apply CSS. Supported elements also accept
relevant image, link, table-span, and form-value attributes.

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

HTML attribute-value equality follows the Standard's ASCII case rules. For example,
`input[type="SEARCH"]` matches lowercase and mixed-case search types. Values of
`id`, `class`, `name`, `value`, `role`, `title` and data attributes remain exact.
The original attributes remain available to selectors and `attr()`.
`i`/`I` forces ASCII-insensitive comparison; `s`/`S` forces exact comparison.
Token `~=`, dash `|=`, prefix `^=`, suffix `$=` and substring `*=` operators
follow the same case rules. Empty token/prefix/suffix/substring queries never match;
tokens use CSS ASCII whitespace. Quoted values and plain identifiers are accepted.
Unquoted numbers and malformed flags/operators return CSS diagnostics. Unicode
and escaped type, class, ID, attribute and supported pseudo names share canonical
CSS identifier grammar. Hexadecimal escapes consume up to six digits plus optional
terminating whitespace; invalid scalar escapes become U+FFFD. Escaped punctuation
stays within its identifier. Quoted attribute values support escapes and continued
lines. Type/attribute names fold ASCII only; class and ID values retain Unicode
and case, and HTML class lists split ASCII whitespace only. Full CSS preprocessing,
remaining declaration grammar, namespace matching and quirks remain unfinished.

`:enabled` and `:disabled` match initial state on supported input, button, select,
textarea and option hosts. Boolean `disabled="false"` still selects disabled;
readonly and ARIA attributes alone leave controls enabled. Ordinary elements and
foreign nodes match neither state. These selectors compose with `:not`, attributes,
classes, positional selectors and combinators. Simple positional/control pseudo-class
names use ASCII case matching. Fieldsets, optgroups, inherited disabling and
form-associated custom controls remain unfinished, along with broader control states.

Image `width` and `height` attributes provide initial pixel or percentage hints,
below author CSS. Their HTML prefix rules ignore invalid initial values and allow
trailing text after a numeric prefix. CSS `width:auto` and `height:auto` restore
automatic sizing from the loaded image's natural ratio. Zero extents retain layout
space without image painting. Every image still requires an authorized, valid asset;
zero sizes do not bypass failures. Numeric excesses use existing limits and return
style diagnostics with the offending attribute. Responsive source selection and
complete alternative-text behavior remain unfinished.

Body lengths support `pt`, `px`, `mm`, `cm`, `in`, `rem`, supported percentages,
and unitless `0`. Leading decimals such as `.5mm` are accepted. Letter spacing
also accepts `em`. Flex and grid support gaps, alignment, ordering, and
track/item sizing, but not every browser layout rule.

`white-space: nowrap` collapses whitespace and prevents automatic wrapping.
Explicit `<br>` breaks still apply; long lines can overflow their container.

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
| SVG | Self-contained SVG through Resvg, with bounded source size, raster dimensions, nodes, nesting, path data, filters, and references. DTD/entity declarations, image elements, external references, and escaped CSS resource syntax are rejected. |

| Image styling | Supported values or behavior |
| --- | --- |
| `object-fit` | `fill`, `contain`, or `cover` |
| `object-position` | Position an image within its box |
| `background-size` | Explicit sizes, `cover`, or `contain` |
| `background-position` | Position a background within its box |
| `background-repeat` | `repeat`, `repeat-x`, `repeat-y`, or `no-repeat` |

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
selected font contains them. Font availability does not yet establish MathML
layout support; see the [MathML checklist](html-to-pdf-mathml-1.1.0-checklist.md).

Advanced callers can read selected-font OpenType MATH data with
`Font.math_metrics/1`. Metrics remain in font design units and include
`units_per_em`. Missing or malformed tables return shared diagnostics.
Parsing occurs only on request and uses the cumulative `max_font_math_work`
budget. Results include selected default-math `ssty`, `flac`, and `dtls`
substitution lookups and their single/alternate kinds. `Font.glyph_bounds/1` supplies glyph ink bounds from the
selected TrueType outlines. These APIs supply font data; MathML markup
rendering is still under implementation.

Low-level `PdfWriter.render/2` callers may provide `:glyph_ids` on embedded-font
text boxes, one positive glyph ID per Unicode scalar in `:text`. Explicit runs
use the selected glyph widths, bypass Latin ligatures and automatic kerning,
and preserve the original Unicode in ToUnicode mappings. Validators check
selected outlines and widths, font budgets, and the fixed 65,535 CID bound.
Ordinary text retains its existing shaping. Empty text paints no operators.

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

Templates can vary on the first, odd, and even pages. A matching `false` or
`nil` variant hides that template. See [headers and footers](html-to-pdf-examples.md#add-headers-footers-and-page-numbers).
For PDF viewer bookmarks, see [outlines from headings](html-to-pdf-examples.md#create-bookmarks-from-headings).

## Known limits

| Not supported | What to use instead |
| --- | --- |
| JavaScript, canvas, video, audio, iframe | Static HTML and supported images |
| XFA, JavaScript form actions | Use supported AcroForm fields or static artwork |
| Floats, fixed positioning, transforms, animations | Block, flex, grid, relative, or absolute layout |
| Named pages and CSS page-margin boxes | Page options and header/footer templates |
| CSS `counter(page)` or `counter(pages)` | `{{page}}` and `{{pages}}` in header/footer templates |
| Animated PNG playback | The PNG default image is rendered; animation frames are not played. |
| WOFF/WOFF2, variable fonts, CFF OpenType | Static TrueType fonts that permit embedding |
| Complex shaping or bidirectional text | A renderer with shaping support for Arabic, Indic scripts, Thai, and complex emoji sequences |

See [Resource limits](resource-limits.md) for configurable size limits and
[Browser rendering](html-to-pdf-browser-parity-coverage.md) for visual tolerances.

### JPEG validation

The renderer accepts 8-bit baseline and progressive JPEGs. Before embedding an
asset, `JpegValidator` checks segment bounds, quantization and Huffman tables,
scan component selectors and parameters, progressive scan order, restart marker
order, and end-of-image termination. Every scan must contain entropy-coded
bytes, and every frame component must have an initial scan. Header-only images,
empty scans, and truncated marker streams return an `:invalid_document`
diagnostic at the `:style` stage through `HtmlToPdf.render/2`.

These checks validate the encoded structure without decoding the Huffman-coded
pixels. They do not prove that the entropy data supplies every declared pixel.
PDF readers decode the original JPEG bytes through the DCTDecode filter.

### JPEG color conventions

JPEG metadata preserves grayscale, RGB, YCbCr, ordinary CMYK, Adobe CMYK, and
Adobe YCCK separately. Frame components and Adobe markers must be consistent.
Unsupported component identifiers without an Adobe marker, conflicting markers,
and repeated frames fail with a diagnostic. Ordinary CMYK keeps its sample
polarity; Adobe CMYK and YCCK use inverted samples. The PDF writer emits explicit
DCT color-transform parameters for the supported conventions.

The ordinary CMYK fixture contains standard non-inverted CMYK samples and no
Adobe marker. It is independently rasterized alongside Adobe CMYK and YCCK.
Some browser JPEG decoders assume inverted samples even without the Adobe
marker, so browser decoding alone is not an oracle for ordinary CMYK polarity.

The mapping follows the Adobe PDF reference, section 3.3, DCTDecode filter
parameters: https://opensource.adobe.com/dc-acrobat-sdk-docs/pdfstandards/pdfreference1.7old.pdf

See [PNG and SVG images](image-processing.md) for output behavior and image errors.


Non-executable HTML script data blocks preserve literal text, including character
references and markup-looking strings. The renderer does not validate or evaluate
the data format, parse its contents as HTML/CSS, or resolve its src attribute.
Script type uses exact ASCII JavaScript MIME essences and legacy language defaults.
Classic, module, import-map and speculation-rule scripts return source diagnostics,
including hidden scripts. Foreign scripts retain their existing rejection.

Script boxes default to display:none. Author CSS can expose literal data in
ordinary supported body contexts; leading metadata scripts join the constructed
head. Empty contents and escaped/double-escaped closing boundaries are supported.
Data scripts and ordinary templates remain actual siblings in supported
list/description/table/select/button parents without becoming terms, rows, cells,
columns or choices. Implicit row groups retain both and column groups retain
templates. Visible script/template boxes in table structures diagnose pending
anonymous formatting. Complete insertion modes and active composition remain unfinished. Unsupported executable content also rejects when
callers supply a DOM directly to style computation.


Ordinary HTML templates retain their unused contents in a separate document
fragment, with empty actual children. Nested templates, script data and foreign
namespaces remain in the parser tree. Unused contents do not contribute styles,
fonts, images, fields, metadata or visible text. Template boxes default hidden;
author CSS may display an empty box, while selectors still count the template
element among its siblings. Head templates stay in metadata placement.

Input policy and shared node/depth limits also apply to unused fragments.
Executable scripts and event attributes diagnose, including supplied DOMs.
Supplied template DOMs require empty children and a document_fragment content
map. Direct DOM text requires valid UTF-8. Template for/shadowroot attributes
currently diagnose because active composition is not implemented. Full template
insertion rules, remaining specialized contexts and owner-document semantics remain
unfinished. Unsupported static markup within templates still diagnoses.


Positional selectors count actual element siblings, including hidden templates,
with forward/reverse child and namespace/tag type positions. Supported compound
negations may contain these functions. Coefficients and offsets can be signed,
zero or negative; n starts at zero. Namespace selector syntax,
Level 4 of-selector-list filtering, deeper functional nesting and positional
matching for parentless/fragment roots remain unfinished.


SVG image assets reject namespace-qualified or ASCII-case-variant event handlers,
foreign scripts and executable XHTML scripts before conversion. Genuine unqualified
XHTML type attributes classify non-executable HTML data; qualified XML attributes
do not replace them. Shared validators own this policy, including inactive caller
DOMs. Full inline SVG composition and foreignObject/font painting remain unfinished.


With the static scripting-disabled model, noscript prints supported active inline
fallback and author display:block fallback. Head noscript can contain supported
style/meta elements; its styles and metadata apply normally. Resources and form
controls inside active fallback follow ordinary policy and remain editable.
Unused template fallback contributes no styles, metadata, fields or asset loads.
Default inline noscript and neutral inline ancestors support nested in-flow
blocks with anonymous text groups. Percentage widths, sibling and parent boundary
margin collapse, table-cell flow and atomic inline-block contents retain ordinary
layout. Nested noscript and invalid head/transparent contents diagnose. Interrupted
inline ancestors with backgrounds, box edges, links, positioning, clipping or
vertical alignment diagnose while complete fragment painting remains unfinished.
Replaced-inline images also diagnose. HTML-discovered link processing remains
excluded. Remaining insertion/select contexts and full anonymous inline/block
and table formatting remain unfinished.


Unregistered custom HTML containers preserve inline inheritance, nested supported
flow/phrasing and harmless attributes. Names fold ASCII only, preserve Unicode and
valid punctuation, and exclude the eight reserved custom names. CSS escapes can
select punctuation names. A trailing slash does not close a nonvoid custom tag.
Transparent contents obey the nearest ordinary ancestor's content model. A bare
custom fragment context uses document-flow fallback. Supplied DOMs share name,
namespace, structure and event validation. Active inline SVG/MathML painting still
diagnoses; HTML integration points preserve their separate namespace.

The global `is` attribute remains selector-visible metadata. Builtin controls keep
ordinary behavior. Custom attributes named `src`, `href`, `disabled`, `readonly`
or `multiple` create no resources, links or controls. Registration, upgrades,
callbacks and form-associated custom internals remain excluded.

The global `hidden` attribute defaults to `display:none` below author CSS. Empty
and invalid values, including `false`, mean Hidden. Author display overrides may
reveal content and still receive normal resource failures. Hidden descendants
create no fields or resource loads, while stylesheet/metadata processing and
actual sibling positions remain. An image's own `src` retains strict resource
validation even when that image is hidden. Exact ASCII-insensitive `until-found` diagnoses
pending initial content-visibility/containment painting; whitespace is not trimmed.
`type=hidden` input keeps its separate UA `!important` rule.
Directional layout remains a 1.2.0 dependency.

`details` renders as a block. Its first direct `summary` is the legend, with an
inside disclosure marker. A missing summary supplies an English "Details"
legend using inherited typography. Additional summaries remain ordinary blocks.
The boolean `open` attribute shows the body even when its value is `false`.
Closed bodies omit text, images and fields, but still undergo input validation.
Author styles can change the summary marker, position or display without exposing
a closed body. Positional selectors retain the original authored DOM positions.
Open bodies use normal wrapping and pagination. Named groups are case-sensitive;
a nonempty name allows at most one initially open member and cannot nest within
another member of that same group. Violations return actionable diagnostics.
Template fragments have independent groups. Browser toggling is outside the static
profile. `::details-content`, directional marker layout and complete shadow
composition remain open. Markers use embedded font symbols with browser-sized
advances; Chromium uses geometric artwork, so small shape differences remain.

The global `popover` attribute retains its authored value. Empty means Auto,
ASCII-insensitive `auto`, `hint` and `manual` select their named states, missing
means None, and invalid values mean Manual without trimming whitespace. All
initial popovers are closed in this scripting-disabled profile. `:popover-open`
never matches; its negation does. Closed popovers omit paint, fields and unused
image/background dependencies while retaining DOM sibling positions and initial
radio-group participation. Their contents still undergo HTML validation.
Author display overrides receive an actionable diagnostic because native fixed
positioning and fit-content geometry remain unfinished. The display default
participates below author CSS; it is not an important rule. Activation, top-layer
composition, dialog integration and complete popover geometry remain open.

Actual HTML/foreign `href`, `src` and `xlink:href` references reject `javascript:` and `vbscript:`
schemes before hidden content can skip painting. Matching is ASCII-insensitive
and accounts for leading URL controls/spaces and embedded tabs/newlines. CSS
background URL validation uses shared CSS escape decoding before the same check.
Unregistered custom href/src attributes remain metadata and create no URLs.
Ordinary local-resource and link authorization still apply to visible content.

The global boolean `inert` attribute preserves initial artwork and sanitized
control values while suppressing active fields and link regions through ordinary
descendants. A value of `false` still means the attribute is present. Actual
siblings, enabled/disabled selectors and visible resource authorization remain.
Supplied internal state is cleared and recomputed from the attributes. Applicable
flat-tree, shadow and dialog composition remain unfinished. PDF text remains
searchable and extractable.

`mark` defaults to black text on yellow; `ins` underlines; `del` and legacy
`strike` use line-through. Author CSS can override the defaults. Transparent
`ins`/`del` contents follow the nearest ordinary ancestor's constraints. Their
`cite`/`datetime` values remain selector-visible metadata without resource loading
or link creation. Edit date parsing/association remains unfinished.
Inline color backgrounds preserve ancestor and child painting order, wrapping,
font metrics, relative positioning and page breaks. Inline borders, padding,
image backgrounds and decorated fragments interrupted by blocks remain open.


Supported fresh unvisited HTTP/HTTPS/mailto anchors default to #0000EE and underline below author CSS. Anchors without href inherit ordinary color/decorations. Inert links preserve their artwork while omitting active regions; hidden links paint nothing. No destination is fetched. Browser visited/activation state is excluded; remaining anchor construction, attributes and base/relative/fragment destinations stay open.


Styled inline boundaries preserve complete unbroken words. Supported line-break, overflow-wrap and word-break values remain independent; emergency wrapping follows ordinary opportunities and preserves extended grapheme clusters across styles. Invalid keywords and malformed advanced wrapping values return shared diagnostics. Void wbr opportunities and legacy nobr defaults are supported, including styled-word min-content grouping. Full Unicode/language breaking, shaping, broader global/parser integration and general table sizing remain open.


Optional breaks retain parent line metrics and explicit opportunities across styled words, nowrap and pre text. A leading break can create an empty line before an oversized word. Authored display:none hides a marker; other own box styles, pseudo content, counter effects and background resources create no paint or asset reads. Empty flex/grid marker items retain their real gaps or slots. Nonpainting markers retain pagination bounds and count toward cumulative writer limits before normalization. Broader automatic tables and the existing synthetic-table pagination disagreement remain incomplete.


HTML headingoffset and boolean headingreset compute automatic bookmark levels through inclusive HTML ancestors, capped at nine. Invalid and negative offsets contribute zero. These attributes preserve artwork and initial fields. Inline headings retain a single bookmark across wrapping, distinct equal titles remain separate, and hidden descendants do not enter titles. Caller outlines retain precedence. Shadow-host and foreign-document integration remain partial. The mixed-inline baseline disagreement recorded for heading offsets is resolved by the baseline batch below.


Inline blocks align to their last normal-flow line. Empty content and overflow:hidden use the bottom margin edge. Declared height, margins, borders and padding affect line extents while relative offsets and absolute descendants leave the flow baseline intact. Multiline automatic widths use the widest forced line. Empty form controls retain their intrinsic line struts. Other vertical-align values, full shrink-to-fit, writing modes and inline-flex/grid baseline integration remain partial.


Native form appearance objects preserve the existing artwork and active PDF resource graph while avoiding unused temporary page objects. The default limits support the tested 1,000-control interactive document; this does not complete the remaining control semantics or native-theme work.

All 148 opaque CSS named colors are supported in text, backgrounds, borders and
text decorations. Keywords use ASCII case folding and whitespace. Color parsing
and CSS declaration normalization delegate to the shared validators. System
colors and broader modern color functions remain unfinished.

Horizontal rules render with gray inset borders, half-em vertical margins and
automatic horizontal margins. Legacy width, size, align, color and noshade hints
apply below author CSS. Legacy HTML color parsing remains separate from CSS color
syntax and shares the named-color palette. Rules inside selects are separators,
not options. Automatic horizontal margins work in ordinary block layout;
unfinished flex, table and intrinsic-sizing contexts return diagnostics.

Structural first/last/only and forward/reverse An+B child/type selectors match
document and fragment roots using their inclusive element siblings. Text and
comments do not count; authored style and template elements do. Virtual fragment
default-body styling does not create source siblings or element ancestors.
Positions use the existing render-scoped cache and shared CSS-work budget.
Namespace grammar, selector-list negation and the An+B of-selector syntax remain open.

Atomic inline dimensions and their containing fixed-height lines retain 1/64
CSS-pixel precision. Empty or non-visible-overflow atomic baselines preserve
fractional bottom-edge depth. Broader block, flex and grid precision remains open.

Boolean `required` attributes are supported on the existing text-like,
checkbox/radio, select and textarea controls. Initial `:required` and `:optional`
matching follows applicability and attribute presence, including disabled and
readonly controls. Hidden and button input states match neither selector.
Radio selectors retain each button's authored requirement attribute, while
editable widgets in one PDF radio field share the owner-scoped group requirement.
Readonly and disabled controls do not acquire a PDF Required flag. Empty required
values remain renderable. Remaining input types, complete initial validity and
submission behavior are separate unfinished requirements.
