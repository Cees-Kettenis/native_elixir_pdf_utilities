# Authored dialogs, quotations, and legacy hints

The native renderer supports closed and authored-open nonmodal `dialog`
elements. `open` is a boolean attribute, so `open="false"` still opens the
initial dialog. Closed dialogs omit descendant painting and resource requests.
Author CSS can override the initial display and position defaults. The renderer
does not infer script-created modal state or add a backdrop.

Dialogs use the HTML user-agent defaults for their border, white background,
one-em padding, automatic inline margins, absolute position, and fit-content
width. Their static vertical position follows the authored flow. Explicit
insets and dimensions use the existing positioned layout machinery. Later
positioned flow content can cover an earlier nonmodal dialog.

`q` generates opening and closing quotes through its `::before` and `::after`
pseudo-elements. Author content, display, color, and quote declarations override
those defaults. CSS `quotes` accepts `auto`, `none`, inherited values, or complete
pairs of CSS strings. `open-quote`, `close-quote`, `no-open-quote`, and
`no-close-quote` share document-wide nesting state. A closing quote at depth zero
produces no text. Deeper nesting reuses the last configured pair. Hidden
pseudo-elements do not change nesting depth.

Automatic pairs follow the element's inherited language and a pinned ICU 78
CLDR table. The table contains 906 locale entries, with longest locale-prefix
fallback and Chromium's default English pairs for unknown locales. Its source,
regeneration command, input checksum, and data license are in
`scripts/quotation-data`. Fonts still determine whether each generated character
can be painted, using the renderer's existing unsupported-glyph policy.

`font` supports legacy `color`, `face`, and `size` presentation hints. The seven
HTML sizes are fixed at 10, 13, 16, 18, 24, 32, and 48 CSS pixels. Relative sizes
use the HTML base size of three. Shared HTML parsing handles numeric prefixes,
clamping and legacy colors. Author CSS overrides the hints.

`body` supports legacy text, background, unvisited link colors, background-image
references and margin attributes. Background references use the normal asset
resolver and active-URL validation. `alink` and `vlink` are preserved; the static
renderer creates no active or visited state. `acronym` uses ordinary inline
content. Titled `abbr` and `acronym` elements paint dotted underlines without
inventing tooltip text. Author text-decoration declarations can suppress them
or select solid, dotted, or dashed strokes. These strokes reuse the PDF border
writer's existing pattern geometry.

Removed `basefont` and `bgsound` elements preserve their attributes but add no
font state, background sound, or resource requests to printed output. `blink`
paints ordinary static inline text. Removed `keygen` elements preserve their
initial authored metadata and generate no interactive form field. Menu and
obsolete directory lists are handled by list layout.

Focused browser fixtures compare the original dialog, quote, font/body,
abbreviation-title and removed-element HTML
against Chromium without changing the source or comparison thresholds.
