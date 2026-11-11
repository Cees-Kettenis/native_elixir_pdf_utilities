# HTML and CSS support

Render static reports, invoices, forms and labels. Start with the
[examples](html-to-pdf-examples.md). Unsupported features return
[diagnostics](diagnostics.md).

## HTML support

| Content | Supported |
| --- | --- |
| Text | Headings, paragraphs, sections, inline formatting, quotations and preformatted text |
| Lists | Ordered, unordered and description lists, including nesting and numbering |
| Tables | Captions, column/row groups, spanning cells and repeated headings |
| Images | PNG, JPEG, SVG and responsive `picture`/`srcset`/`sizes` selection |
| Links | URI and same-document fragment destinations, including static image maps |
| Forms | Initial artwork and supported editable PDF fields |
| Embedded documents | Authorized local HTML, plaintext and supported images; iframe `srcdoc` |
| Math | Native MathML equations |

HTML must be UTF-8. Close nonvoid elements by default; `html_recovery: true`
accepts supported omitted closing tags and nonvoid trailing slashes. JavaScript,
event handlers and linked stylesheets are excluded. Supply CSS through `:stylesheets`.

Rendering checks the supported subset, not complete HTML validity.

### Visibility and composition

Supports initial disclosure states, hidden/inert content, declarative shadow
roots, slots and named template replacement. Scripts and later DOM updates do
not run. Hidden or replaced source content still requires valid, authorized resources.

### Forms

Text inputs, textareas, checkboxes, radios and supported selects become editable
fields. Buttons stay artwork; hidden inputs produce no output. Use `forms: :static`
for artwork only. Other input types have restrictions; mutable sliders and temporal
pickers are unsupported. See [PDF forms](pdf-forms.md) for names, values and filling.

Supported child-document controls remain editable but cannot repeat across pages
or page furniture. Unsupported widget transforms return diagnostics.

## CSS support

| Area | Supported |
| --- | --- |
| Selectors | Namespaces, tags, classes, IDs, attributes, descendant/child/sibling combinators, `:is()`/`:where()`/`:not()`/`:has()`, filtered positions, `:lang()`/`:dir()` and initial control/link/disclosure states |
| Styles | Cascade, inheritance, CSS-wide keywords, `all`, `!important`, layers, nesting, `@scope`, custom properties, `@property`, `var()` and `currentColor` |
| Layout | Block, inline, inline-block/table, flex, grid, flow-root, contents, logical box axes, sizing, margins, padding and gaps |
| Text | Fonts, alignment, spacing, wrapping, whitespace, list markers and solid text decorations |
| Boxes | Colors, borders, rounded corners and background images |
| Generated content | `::before`, `::after`, strings, `attr()`, `counter()` and counter reset/increment/set controls |
| Printing | Print media queries, static `@supports`, bare `@page` size/margins and page breaks |

Shadow styles support `:host()`, `:host-context()`, `::slotted()` and explicit
`::part()` exports. Matching uses authored tree relationships across slot composition.
Malformed declarations and invalid selector rules are discarded; unknown at-rules
and `@import` are ignored. Valid declarations still receive strict value and resource
checks. Direction selectors resolve initial HTML direction; right-to-left layout
and painting remain gated. New pseudo-elements such as `::first-line`, `::first-letter`, `::marker`,
`::placeholder` and control parts have selector ownership, but return
`:not_implemented` when selected until their layout and painting are available.

Support is a subset of browser CSS. Lengths include physical units, selected-font
`em`, `ex`, `ch`, `cap`, `ic`, `lh` and their root-relative forms. Physical and
logical viewport units, including small, large and dynamic forms, use the same
static print viewport. Child documents use their own viewport.

Shared typed arithmetic supports calculated dimensions, box edges, text metrics,
positions and transforms. Font units compute on the declaring element; percentages
retain the consuming box's basis. Intrinsic dimensions support `min-content`,
`max-content`, `fit-content` and static `calc-size()` calculations.

Typed `attr()` supports raw strings, numeric units, `type()` and fallbacks.
Attribute substitutions cannot supply resource URLs. `env()` supplies zero safe-area
insets and a preferred text scale of one; unknown values require a fallback.
Print conditions use 8-bit color, light colors and unchanged accessibility
preferences. Font support queries report the formats and features the renderer
can actually use. Imports and live device state remain excluded.

Ordinary HTML text paints Unicode line/paragraph separators as spaces, matching Chromium.
Ordinary newlines follow `white-space`.

### Cascade and custom properties

Named, anonymous and nested `@layer` rules share order across stylesheets.
Important declarations reverse that order. `revert-layer` restores the earlier
layer; `revert` restores the element's user-agent and inherited styling.
Shorthands reset omitted components and preserve later longhand overrides.
CSS nesting supports `&`, selector lists and interleaved declarations.
`@scope` supports explicit or owning-style roots, limits and scope proximity,
including the existing shadow and slot boundaries.

Custom properties preserve token boundaries and compute before inheritance.
Missing variables, cycles and invalid substituted values use CSS defaulting for
the affected property. Resource permissions and rendering limits still apply.
Registered `@property` declarations support typed scalars, alternatives and lists,
with their declared defaults and inheritance. Relative values use the declaring
element's metrics. Invalid registrations are ignored. Registered gradient images
require the CSS image implementation and return `:not_implemented`; source-only
contexts cannot invent missing font or viewport metrics.

### Positioning and transforms

Static, relative, absolute, fixed and printed sticky positioning are supported.
Fixed artwork repeats on each page. Native controls appear on their first page only.
A transformed containing block keeps its fixed
children local. Sticky uses the initial print scrollport before pagination and
retains its normal flow allocation. No scrolling is simulated. Transforms move
artwork without reflow. Isolation, layout/paint containment and positioned stacking
contexts preserve paint order. Full containment, floats, 3D transforms, perspective
and animations are unsupported. Child-document form repetition restrictions still apply.

Logical sizes, edges and insets follow `writing-mode` and `direction` during the
cascade. These box axes do not provide vertical glyph orientation or full bidi
text layout. Aspect ratios use the selected content or border box, and definite
ratio-derived heights resolve descendant percentages. Flow roots contain margins;
ordinary blocks collapse adjoining positive and negative margin sets.

`visibility:hidden` preserves allocation and allows visible descendants;
`force-hidden` suppresses the entire subtree. Hidden artwork creates no links or
fields. Collapsed table tracks retain intrinsic sizing before removal; collapsed
flex items retain a cross-axis strut. CSS table and ruby roles gain anonymous owners.

Overflow axes and clip margins keep paint and annotation coordinates together.
Text ellipses respect shaped clusters and painted positions. Static `line-clamp`
uses the 21 March 2023 Overflow 4 grammar, including `max-lines`, `block-ellipsis`
and `continue:discard`. Ordinary descendants share the line budget; independent
formatting contexts keep their own line counts. `max-lines` alone does not force
page breaks. These features do not add interactive scrolling.

## Render options

Pass options to `HtmlToPdf.render/2` or `render_file/3`.

| Option | Default | Use |
| --- | --- | --- |
| `:page_size` | `:a4` | Named size, orientation tuple, dimension string or numeric tuple |
| `:margin` | `0` | Points, CSS string or side map |
| `:html_recovery` | `false` | Enable supported HTML recovery |
| `:stylesheets` | `[]` | List of `{:css, css}` or `{:file, path}` |
| `:base_url` | None | URL resolution base; a local directory also authorizes assets beneath it |
| `:assets` | `%{}` | Map references to `{:bytes, bytes}` or `{:file, path}` |
| `:asset_resolver` | None | Callback supplying approved asset bytes |
| `:print_viewport` | Page-derived | Positive `{width, height}` in CSS pixels |
| `:image_selection_density` | `1` | Positive responsive-image density |
| `:default_font` | `"DejaVu Sans"` | Family name or fallback list |
| `:fonts` | `[]` | Font definitions with `:family` and `:path` or `:data` |
| `:system_font_discovery` | `true` | Look up installed fonts |
| `:unsupported_glyphs` | `:replace` | Replace missing characters or use `:error` |
| `:compress_streams` | `true` | Compress eligible PDF streams |
| `:subset_fonts` | `true` | Embed only needed TrueType or CFF outlines when permitted |
| `:forms` | `:interactive` | Editable fields or `:static` artwork |
| `:metadata` | HTML title | Document information such as title and author |
| `:outlines` | Disabled | `:headings` or an explicit bookmark list |
| `:page_furniture` | Disabled | Header/footer templates and page-number tokens |

### Page size and margins

| Setting | Examples and limits |
| --- | --- |
| Page size | `:a4`, `:letter`, `{:a4, :landscape}` or `"100mm 60mm"` |
| Numeric sizes | Both values ≤ 20: inches; otherwise points |
| Margins | `36`, `"18mm 12mm"` or `%{top: 36, bottom: "12mm"}` |
| `@page` | Supported size and absolute margins only; explicit options take priority |
| Ignored page settings | Rotation, marks, bleed, decoration and unsupported size/margin values |

## Tables

| Feature | Support |
| --- | --- |
| Layout | Automatic/fixed sizing, nested tables, separate/collapsed borders |
| Spans | `colspan` and `rowspan`; rowspans stop at row-group boundaries |

Automatic tables can exceed their CSS `width` when cell contents need more space.
The renderer preserves text size and does not shrink the page to fit; content
beyond the page edge is clipped. Use a wider page or reduce content widths.
Compared with v1.0.0, columns now preserve their minimum content widths instead
of squeezing them and overlapping text. Existing narrow-page templates may
therefore wrap differently or extend farther beyond the page.

### Table page breaks

| Content | Behavior |
| --- | --- |
| `<thead>` | Repeats in ordinary tables |
| `<tbody>` | `break-inside: avoid` keeps fitting groups together; larger groups split between rows |
| Individual rows | Never split; oversized rows may overflow |
| Oversized inline-block tables | Slice across pages without repeating headings |

[Grouped table example](html-to-pdf-examples.md#paginate-a-table-with-row-groups).

## Images and backgrounds

| Feature | Support |
| --- | --- |
| Formats | PNG, JPEG and rasterized SVG; [image restrictions](image-processing.md) |
| SVG text and links | Text becomes pixels; internal links are inactive |
| Image styling | `object-fit`, `object-position` |
| Backgrounds | Sizing, positioning and repetition |

### Assets and local files

| Setting | Behavior |
| --- | --- |
| Resolution order | `:assets` → authorized local files → `:asset_resolver`; no fallback on resource limits |
| Local files | Require a directory `:base_url`; `render_file/3` does not infer it |
| Path checks | Traversal and symlinks checked; use trusted directories or approved bytes |
| Explicit file paths | Caller-configured asset mappings, fonts and stylesheets are trusted |
| Other sources | Supported data URIs; remote resources are never fetched |
| Resolver request | `%{reference: reference, kind: kind}`; kind is `:image`, `:background_image` or `:font` |
| Resolver return | `{:ok, bytes}`, `:not_found` or `{:error, reason}` |
| Callback execution | Disposable operation worker; shares [operation limits](resource-limits.md#operation-time-and-memory) |

[Asset examples](html-to-pdf-examples.md#load-local-images).

### Resolver process context

When upgrading from v1.0.0, update callbacks that depend on the calling process.
`self()` identifies the worker, caller process dictionary entries are not
inherited, and private ETS tables remain accessible only to their owner.

Read needed values in the caller before creating the callback, then capture
those values in its closure. Capturing a private ETS table identifier does not
grant the worker access. Capture the caller PID before rendering when a callback
needs to send it messages.

The [logo resolver example](html-to-pdf-examples.md#load-local-images) reads
`logo_png` before rendering and captures the bytes. For caller-stored images,
replace that read with `Process.get(:logo_png)` or a caller-side ETS lookup.

## Fonts and text

| Setting | Support |
| --- | --- |
| Default | DejaVu Sans; installed-font discovery enabled |
| Consistent output | Supply fonts and set `system_font_discovery: false` |
| Custom fonts | Embeddable static TrueType and CFF OpenType, WOFF 1 and selected collection faces; [registration example](html-to-pdf-examples.md#register-a-font) |
| Face selection | Ordered families; CSS directional weight and width matching, weight/width ranges, italic/oblique descriptors and Unicode ranges. Supply `collection_index` to select a collection face, starting at zero |
| Missing characters | Fallback fonts, then replacement; use `unsupported_glyphs: :error` to fail |

CFF 1 fonts preserve source charstrings, hints and local/global subroutine ownership
when converted to CID-keyed PDF fonts. Each document character code keeps its own
Unicode extraction map, including aliases sharing one glyph. Legacy `seac`
composition and nondeterministic Type 2 `random` programs currently return an
unsupported-font diagnostic rather than approximate artwork. CFF2 remains excluded.

## MathML in HTML

| Feature | Support |
| --- | --- |
| Equations | Tokens, operators, fractions, radicals, scripts, limits, accents and matrices |
| Fonts | Bundled STIX Two Math; custom fonts need required MATH metrics |
| Page breaks | Equations stay together; oversized equations return a diagnostic |

[MathML examples](html-to-pdf-examples.md#mathml-equations).

## Pagination

| Feature | Support |
| --- | --- |
| Breaks | Automatic or supported CSS page breaks |
| Headers/footers | First/odd/even variants; reserve space in page margins |
| Page numbers | `{{page}}` and `{{pages}}` in templates |

[Header and footer examples](html-to-pdf-examples.md#add-headers-footers-and-page-numbers).

## Known limits

| Unsupported | Alternative |
| --- | --- |
| JavaScript, media playback, canvas drawing APIs | Static HTML and images |
| Embedded PDFs, XFA, JavaScript form actions | Supported HTML and PDF forms |
| Named pages, page-margin boxes, CSS page counters | Page options and header/footer templates |
| WOFF2, variable fonts | Embeddable static TrueType and CFF OpenType fonts |
| Complex shaping, bidirectional text, vertical writing | A renderer supporting those scripts and layouts |

Embedded content and framesets have restricted static support. SVG, control
styling and some layouts can differ from browsers. Check your generated PDF.

[Resource limits](resource-limits.md) · [Diagnostics](diagnostics.md).
