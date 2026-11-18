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
| Text | Fonts, alignment, spacing, wrapping, whitespace, list markers, text decorations, shadows and emphasis |
| Boxes | Colors, borders, rounded corners and background images |
| Generated content | `::before`, `::after`, `::marker`, replacement content, images, quotes, attribute fallbacks, nested counters and visual `@counter-style` definitions |
| Printing | Print media queries, static `@supports`, named pages, margin boxes, cross-references, footnotes and page/column floats |

Shadow styles support `:host()`, `:host-context()`, `::slotted()` and explicit
`::part()` exports. Matching uses authored tree relationships across slot composition.
Malformed declarations and invalid selector rules are discarded; unknown at-rules
and `@import` are ignored. Valid declarations still receive strict value and resource
checks. Direction selectors resolve initial HTML direction; right-to-left layout
and painting remain gated. Some pseudo-elements, including `::first-line`,
`::placeholder` and unimplemented control parts, have selector ownership and
return `:not_implemented` when selected.

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
contexts preserve paint order. Static 3D projection and flattening are described
under compositing below. Full containment and animations remain unsupported. Child-document form repetition restrictions still apply.

Logical sizes, edges and insets follow `writing-mode` and `direction` during the
cascade. Vertical and sideways text use the glyph orientation and font metrics
described under typography below. Aspect ratios use the selected content or border box, and definite
ratio-derived heights resolve descendant percentages. Flow roots contain margins;
ordinary blocks collapse adjoining positive and negative margin sets.

Flex layout supports intrinsic and percentage bases, automatic content minima,
min/max freezing, reverse wrapping and shared first/last baseline alignment.
Flex and grid accept `place-*` shorthands, safe/unsafe positions and distributed
alignment. Grid supports named lines and areas, template shorthands, intrinsic
and fit-content tracks, auto-fill/auto-fit, dense placement and subgrid tracks,
names and gaps. Orthogonal flex/grid items retain their own writing plane;
subgrids map inherited tracks and intrinsic contributions across those axes.

Floats support physical and inline edges, clearance and formatting-context
avoidance. `shape-outside` accepts circles, ellipses, rounded insets, polygons,
reference boxes and images, with margins and alpha thresholds. Shape images use
the ordinary image permissions and resource budgets. Selected `shape-inside`
behavior supports basic shapes in boxes with a definite height; complete
exclusions, multiple inside intervals and shape padding remain unsupported.

Multi-column blocks support column counts and preferred widths, normal or
explicit gaps, rules, spanning blocks, column breaks and bounded balancing.
Spanners can cross normal block ancestors; separate formatting contexts and
fixed-position containing blocks stop that propagation.
Definite-height blocks can overflow into additional horizontal columns.
Automatic-height blocks use the remaining first-page space and continue in
column sets on later pages. Flex/grid
continuations retain whole lines and fitting item rows; nested columns use the
final outer fragmentainer height when assigning their text.

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
| `@page` | Named pages, `:first`, `:left`, `:right` and `:blank` selectors, page-specific geometry and decoration |
| Print output | `page-orientation` rotation, crop and cross marks, bleed, and PDF trim and bleed boxes |

Explicit `:page_size` and `:margin` options take priority over authored geometry.
The default print viewport still comes from the initial page geometry.

### Paged publishing

| Feature | Supported behavior |
| --- | --- |
| Running content | All 16 page-margin boxes, `counter(page)`, `counter(pages)`, `string-set` and `position: running(name)`. The draft `running: name` spelling also keeps a copy in body flow. |
| References | `target-counter()`, `target-text()` and `leader()` for same-document references and tables of contents |
| Breaks | Forced and avoided page/column break controls, recto/verso pages, and selected `box-decoration-break` and `margin-break` behavior |
| Footnotes | `float: footnote`, `::footnote-call`, `::footnote-marker`, `@footnote`, keep policies and continuation across pages |
| Floats | Static page and column placement, deferral, offsets and continuation |
| Bookmarks | `bookmark-level`, `bookmark-label` and `bookmark-state`. Explicit `:outlines` options select the API outline behavior. |

Reference targets need an ID in the same document. Running content keeps the
source element's resource permissions. Pagination returns a diagnostic when it
repeats a state, exhausts a resource budget or cannot advance through an
oversized allocation across different page widths. Footnote and float
continuations use one page size and margin configuration. Counter styles are visual.
Spoken counter styles and live browser state are outside this static rendering
subset.

## Tables

| Feature | Support |
| --- | --- |
| Layout | Automatic/fixed sizing, nested tables, separate/collapsed borders |
| Spans | `colspan` and `rowspan`; rowspans stop at row-group boundaries |
| Empty cells | `empty-cells: show/hide` in separate-border tables; ignored with collapsed borders |

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
| Individual rows | Continue at whole-line or atomic boundaries when they cannot fit; spanning cells retain their source geometry |
| Oversized inline-block tables | Slice across pages without repeating headings |

[Grouped table example](html-to-pdf-examples.md#paginate-a-table-with-row-groups).

## Images and backgrounds

| Feature | Support |
| --- | --- |
| Formats | PNG, JPEG and rasterized SVG; [image restrictions](image-processing.md) |
| SVG text and links | Text becomes pixels; internal links are inactive |
| Image styling | `object-fit`, `object-position`, `image-orientation`, `image-resolution` and `image-rendering` |
| CSS images | URLs, linear/radial/conic gradients and their repeating forms, `image-set()` and static `image()` sources with color fallback |
| Backgrounds | Ordered layers, shorthand resets, independent sizes/positions/repetition, origin/clip boxes and fixed page attachment |

`image-set()` uses `:image_selection_density`, shared with responsive HTML images.
Only the selected supported source is loaded. Static `image()` supports direction
and a color fallback for an unavailable or undecodable image. Permission denials
and resource-limit failures retain their diagnostics rather than using a fallback.
Asset permissions and resolver behavior are the same as for ordinary images.

Raster orientation uses Exif metadata by default. `image-orientation:none` retains
the encoded orientation. `image-resolution:from-image` opts into Exif/JFIF or PNG
resolution metadata; an explicit resolution and optional `snap` are also supported.
Malformed optional metadata is ignored. SVG keeps its own viewport sizing.
Rendering quality maps to the PDF image interpolation hint: `smooth` and
`high-quality` enable interpolation, while `crisp-edges` and `pixelated` disable
it. The PDF viewer controls the final sampling.

Background layers cycle shorter longhand lists independently. `round` and `space`
work on each axis. `scroll` and `local` share the unscrolled static geometry;
`fixed` uses the final PDF page viewport and retains each fragment's clip.
Explicit background sizes avoid differences between browser print viewports and
implicit gradient canvases. Root/body canvas propagation is separate from
fragmented element painting. Generated images and repeated tiles use the existing
shared image, raster, work and entry budgets.

### Colors and box painting

| Feature | Support |
| --- | --- |
| Color spaces | sRGB, linear sRGB, Display P3, A98 RGB, ProPhoto RGB, Rec. 2020, XYZ D50/D65, Lab/LCH, Oklab/Oklch, HSL and HWB |
| Color functions | Relative colors, missing components, `color-mix()`, `light-dark()` and deterministic system colors |
| Interpolation | Premultiplied alpha, selected color spaces, hue paths, gradient stop fixup, double positions and transition hints |
| Corners | Physical and logical corner longhands, elliptical pairs, percentages and proportional overlap reduction |
| Borders | Border images with independent slices, widths, outsets, edge repetition and optional center fill |
| Additional paint | Outlines and offsets, ordered outer/inset box shadows, inherited `-webkit-text-fill-color` and text stroke |

Conversions and mixing retain extended channels until the paint boundary. Output
uses component-clipped sRGB in PDF DeviceRGB; this is not an ICC workflow or a
PDF-wide conformance claim. Gradients use the legacy sRGB default unless an
interpolation space is specified with `in`.

The print environment prefers light when both color schemes are offered.
`color-scheme:dark` selects the dark system/control palette. Authored colors take
precedence over control defaults. Forced colors are inactive, and both `economy`
and `exact` print adjustment retain authored paint in generated PDFs.

Border images stretch their admitted source slices without changing layout
dimensions. SVG sources remain converter-backed images; percentage slices avoid
dependence on their raster sampling density. HTML text fill/stroke keeps PDF text
operators and logical extraction. SVG text retains the converter's existing
image/text ownership rules.

### Compositing, effects and static transforms

| Feature | Support |
| --- | --- |
| Compositing | Element/group opacity, the sixteen standard blend modes, background layer blending and isolation |
| Filters | Static blur, drop shadow and color functions; owned same-document SVG filter graphs; backdrop filtering |
| Clipping and masking | Basic shapes and SVG path clips, layered alpha/luminance image masks, mask composites and nine-part mask borders |
| Affine transforms | Transform boxes and origins; individual translate, rotate and scale before the transform list |
| Projective transforms | Static 4x4 matrices, perspective and origins, preserve-3d, flattening boundaries, hidden backfaces and intersecting paint planes |
| Offset paths | Static path strings, basic shapes, geometry boxes and rays; distance, position, anchor, rotation and the offset shorthand |

Group opacity applies once to overlapping descendants. Ordinary affine artwork
keeps native PDF instructions. Filters, masks and projective scenes use bounded
raster artwork at the shared SVG sampling scale, with logical text and separate
link geometry retained. Clipped link regions follow source contours, including
perspective projection. Open offset paths clamp distance at their endpoints;
closed paths wrap, and rays use their specified containment rules.

Rasterized effects, projective scenes and CSS opacity/blend groups require
`forms: :static` for controls. Editable PDF widgets repaint outside the page's
CSS compositing groups, so those combinations return an actionable diagnostic.
Text in these effects requires selected embedded glyph outlines. Complex source
border styles and chained referenced filter graphs return diagnostics; use solid
borders or separate nested effect elements. Filter references stay within their
selected document/tree scope and reuse already authorized image resources.
Mask URLs load authorized images; SVG `<mask>` references are not accepted.
Offset path strings and basic shapes are supported; SVG URL offset-path
references are not currently accepted. Extreme intermediate scenes that cross
the eye plane without a finite flattening canvas return a projection diagnostic.
No animation or scripting is sampled.

Selected static `-webkit-` background, corner, shadow, box-sizing, flex, column,
appearance, transform, print-adjustment and text spellings share their standard
validators and cascade targets. Legacy box orient/align/pack/flex/order spellings
map to the corresponding flex properties. Animation and interaction aliases are
not included.

SVG geometry, fills, strokes, dash patterns, markers, paint order and rendering
hints pass through the existing SVG converter. SVG-only presentation properties
do not become HTML box properties.

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
| Callback execution | Disposable operation worker; shares [operation limits](resource-limits.md#operation-allowance) |

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
| Custom fonts | Embeddable TrueType, CFF/CFF2 OpenType, WOFF/WOFF2 and selected collection faces; [registration example](html-to-pdf-examples.md#register-a-font) |
| Face selection | Ordered families; CSS directional weight and width matching, weight/width ranges, italic/oblique descriptors and Unicode ranges. Supply `collection_index` to select a collection face, starting at zero |
| Missing characters | Fallback fonts, then replacement; use `unsupported_glyphs: :error` to fail |

CFF 1 fonts preserve source charstrings, hints and local/global subroutine ownership
when converted to CID-keyed PDF fonts. Each document character code keeps its own
Unicode extraction map, including aliases sharing one glyph. Legacy `seac`
composition is evaluated before embedding. Type 2 `random` uses a reproducible
per-glyph sequence, seeded from `initialRandomSeed` and the glyph ID; its evaluated
outline is frozen in the PDF. Large evaluated programs use bounded subroutines.

Variable fonts become static instances before layout and embedding. Axis values
are clamped to their declared ranges; named instances, `avar` mappings, TrueType
`gvar` outlines, CFF2 blends and horizontal, vertical and global variation metrics
use the selected coordinates. CSS weight, width, style, optical sizing and explicit
variation settings participate in selection. Font configuration examples are in
[variable fonts and controls](html-to-pdf-examples.md#variable-fonts-and-controls).

| Control | Support |
| --- | --- |
| Features and language | Feature/variation settings, kerning, variant subproperties, language overrides and named `@font-feature-values` definitions share one selection plan |
| Execution | GSUB substitutions, ligatures and contextual/chaining/reverse lookups; GPOS pair, cursive, mark and contextual positioning. Script/language systems, variation-selected features and kerning controls participate in shaping |
| Synthesis | Permitted synthetic bold, oblique, caps and script-position glyphs; `font-synthesis` and its subproperties can disable synthesis |
| Font metrics | `size-adjust`, ascent/descent/line-gap overrides and `font-size-adjust`, including a primary-face `from-font` reference for fallback |
| Color bitmaps | `sbix` PNG/JPEG, duplicate glyphs and density selection; applicable CBDT/CBLC PNG and 32-bit BGRA records with strike metrics |
| Color vectors | COLR v0 layers; bounded COLR v1 paint graphs, gradients, transforms and composition; CPAL light/dark and named `@font-palette-values` overrides |
| SVG glyphs | Static plain/gzip SVG, selected glyph IDs, palette/current-color substitution and embedded PNG/JPEG. No animation or external file/network resources |

Color artwork retains logical Unicode and an invisible selected text run for PDF
selection geometry. Font embedding restrictions still apply. TIFF and
other unimplemented bitmap encodings return a diagnostic. Emoji presentation,
variation selectors and supported ZWJ sequences select a covering font for the
complete cluster. Missing sequences follow the configured unsupported-glyph policy.

Paragraph bidi resolution supports HTML `dir`, `bdi`, `bdo` and CSS `direction`
and `unicode-bidi`. Lines retain logical extraction order while glyphs and links
use visual positions. OpenType shaping includes Arabic joining, Syriac contextual
forms and the supported Indic syllable stages. A selected font must contain the
required glyphs and layout tables.

Extended grapheme segmentation, bidi properties and line breaking use Unicode
17.0.0 on every supported runtime. Line-breaking rules share the shaped cluster
boundaries used by font fallback and painting. `word-break`, `overflow-wrap` and `line-break` select the
admitted opportunities. Automatic hyphenation uses caller-supplied language
patterns or exceptions; no dictionaries are downloaded or bundled. The
`hyphenate-limit-*` controls bound word fragments, successive lines and page ends.

Whitespace supports the legacy `white-space` values and independent collapse,
wrap and trim controls. `tab-size` accepts nonnegative numbers, fractional stops
and lengths. Stops use the containing block's space measure, including after
wrapping. Preserved trailing spaces participate in fitting or hang according to
the selected whitespace mode. Chinese and Japanese segment breaks between CJK
text do not create word gaps; Korean text retains them.

`word-spacing` adds signed lengths at word separators. `text-indent` accepts
lengths, percentages, `hanging` and `each-line`. Logical `start`/`end` alignment,
`text-align-last` and `text-justify` share the fitted line geometry. Justification
expands complete shaping clusters or word separators and preserves cursive joins.
Korean automatic justification uses word separators. `hanging-punctuation`
excludes eligible edge punctuation from fitting and alignment.

`text-wrap` and `text-wrap-style` support static `auto`, `stable`, `balance` and
`pretty` behavior. Balanced wrapping keeps the ordinary line count and searches
for a smaller fitting measure. Pretty wrapping uses that measure when the final
line would contain a single word. Alignment and percentage indents continue to
use the containing block's full width.

`writing-mode` supports horizontal, vertical and sideways directions.
`text-orientation` selects mixed, upright or sideways glyphs. Use
`text-combine-upright: all` or `digits 2`, `digits 3`, `digits 4` to fit selected
text into one vertical em box. Font `vert` and `vrt2` features supply vertical
alternates when available. Orthogonal inline flex and grid containers resolve
percentage, intrinsic and calculated inline sizes through the shared sizing rules.

Ruby supports `ruby-align`, `ruby-position`, `ruby-merge` and `ruby-overhang`.
Initial letters support `initial-letter`, `initial-letter-align` and
`initial-letter-wrap`, including `::first-letter` selections with surrounding
punctuation. Their source text and links remain part of the paragraph.

Text decorations support solid, dotted, dashed, double and wavy lines, selected
thickness and color, `text-underline-offset`, `text-underline-position` and
`text-decoration-skip-ink: auto | none | all`. `text-shadow` accepts ordered
colors, signed offsets and nonnegative blur radii. Blurred shadows use selected
glyph outlines and the existing SVG converter. `text-emphasis` supports filled
and open marks, custom strings, color and over/under or physical side placement.
Marks skip whitespace and eligible punctuation and reserve line space.

Inline alignment supports the `vertical-align` shorthand and its
`alignment-baseline`, `baseline-shift` and `baseline-source` longhands.
Percentage shifts use the element's line height; `baseline-source` selects an
inline block's first or last baseline. `dominant-baseline` is inherited.
Font BASE tables supply script-specific horizontal and vertical alignment
metrics when present. Mathematical alignment uses the selected font's MATH
axis or minus-sign bounds, with synthesized metrics for missing baselines.

Advanced styled-tree callers using built-in PDF faces must select an embedded
font for blurred text shadows and emphasis, which require glyph outlines and
font metrics. Missing information returns an actionable font diagnostic.

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

Embedded content and framesets have restricted static support. SVG, control
styling and some layouts can differ from browsers. Check your generated PDF.

[Resource limits](resource-limits.md) · [Diagnostics](diagnostics.md).
