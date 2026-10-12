# Supported browser rendering

The HTML renderer supports a document-oriented subset of browser rendering for
reports, invoices, statements, forms, and labels. Use the overview below to
choose a layout, then follow the linked reference for supported values and
restrictions.

## Supported behavior

| Area                                                                         | What you can use                                                                                              |
| ---------------------------------------------------------------------------- | ------------------------------------------------------------------------------------------------------------- |
| [HTML and text](html-to-pdf-compatibility.md#html-support)                    | Headings, paragraphs, inline emphasis, lists, links, character references, and supported form controls           |
| [CSS](html-to-pdf-compatibility.md#css-support)                               | Supported selectors, the cascade, custom properties, generated content, colors, borders, and spacing          |
| [Sizing and layout](html-to-pdf-compatibility.md#css-support)                 | Block and inline layout, flexbox, grid, size constraints, text wrapping, and relative or absolute positioning |
| [Tables](html-to-pdf-compatibility.md#tables)                                 | Column sizing, spanning cells, nested tables, repeated headers, and separate or collapsed borders             |
| [Images and backgrounds](html-to-pdf-compatibility.md#images-and-backgrounds) | JPEG, supported PNG formats, self-contained SVG, image fitting, and background sizing and repetition          |
| [Pages](html-to-pdf-compatibility.md#pagination)                              | Page sizes and margins, automatic and explicit breaks, running headers and footers, and page numbers          |
| [Fonts](html-to-pdf-compatibility.md#fonts-and-text)                          | Bundled DejaVu Sans, registered TrueType fonts, optional system-font discovery, and glyph fallback            |

Supported controls create interactive AcroForm fields by default; use
`forms: :static` for artwork only. See [PDF forms](pdf-forms.md).
This subset does not include JavaScript or full browser layout and typography. See [Known limits](html-to-pdf-compatibility.md#known-limits)
before adapting a browser template. Unsupported HTML tags and CSS properties
are reported through the [diagnostic contract](diagnostics.md). Some accepted
`@page` declarations are ignored, including `page-orientation`, `marks`, `bleed`,
and margins that cannot be normalized. See
[page declarations without an effect](html-to-pdf-compatibility.md#page-declarations-without-an-effect)
before relying on successful rendering as evidence that page settings were applied.

## Visual parity

We compare rendered documents with Chromium at 72 DPI. Every fixture enforces
a strict ceiling of **less than 1% differing pixels**. A pixel differs when at least one
RGB channel differs by more than 12 out of 255. Each fixture also checks the
page count and its existing average channel-delta limit.

Chromium and the native renderer load the same bundled DejaVu font files. Two
font fixtures also use the same bundled Liberation font files. Each run records
SHA-256 hashes of these files alongside its raster output. The quality matrix
checks the threshold on every supported Elixir runtime. Browser and rasterizer
updates can change the result, so failures must be investigated and corrected
when those tools change. The measured bound applies to these representative
fixtures within the supported HTML/CSS subset; arbitrary documents or different
font files may have a different result.

These percentages describe visual comparisons, not a percentage of browser
features supported. Start with the [rendering examples](html-to-pdf-examples.md)
for working templates.

## Regression coverage

The suite includes invoices, statements, forms, labels, and focused fixtures for
supported layout behavior. Recent cases cover `white-space: nowrap`, leading
decimal lengths, letter spacing and ligatures, collapsed table borders across
pages, and body padding, backgrounds, and grid layout.

`font_supplementary_unicode.html` uses the same pinned STIX Two Math bytes in
both renderers. It checks mathematical alphanumeric characters, ordinary
operators, wrapping, and page boundaries. Public tests check exact Unicode text
extraction with font subsetting enabled and disabled. MathML layout has its own
unfinished checklist.

`math_glyph_rows.html` checks selected glyph/row measurement against equivalent
CSS coordinates calculated independently from STIX. Both painting paths use
the same renamed reference subset. It checks glyph identities, original text,
baselines, and spacing at 30 points. Direct MathML raster-ink differences
remain recorded in the milestone progress file and do not count as completed
MathML rendering.

`math_stretch_glyphs.html` compares natural and larger glyphs, vertical
parenthesis/radical/integral assemblies, and a horizontal brace with repeated
extenders. FontTools calculates the reference positions from the unmodified
source using exact rational arithmetic. Both renderers paint identical renamed
subset bytes. Selected glyph IDs and exact replacement Unicode are checked
alongside the pixels. This is stretch geometry coverage; native MathML element
layout remains unfinished.

`html_attribute_selectors.html` compares equality, ASCII token separators,
dash values, substrings, empty queries, missing attributes, case overrides,
negation and generated content. Chromium 151/152 ignores explicit `s` flags.
The native fixture renderer adds `s`/`S` to three marked data-attribute selectors;
Chromium uses their equivalent default exact comparison with identical artwork.
Direct validator and public tests also prove that `s` overrides HTML defaults.
The original direct-`s` failure remains under `.quality`; thresholds stay fixed.

`html_search_inputs.html` compares authored search controls with CR/LF values,
case variants, original attribute selectors, missing values, disabled state,
table placement and preformatted CSS. These controls use explicit font, padding,
border and sizing declarations. This fixture does not establish complete native
search-widget theme support. Public tests check text field types, values,
generated names, disabled flags and unsupported-attribute diagnostics. Its type
value selector now matches ASCII case variants, while its value selector retains
exact authored matching.

`html_input_values.html` compares CR, LF and CRLF references in text values,
default/case-variant types, original-value selectors, an empty sanitized value,
disabled controls in a table cell, and an authored preformatted text style.
Public tests check identical sanitized artwork and interactive field values.
Layout tests retain the empty control's height in block, flex, grid and inline
block placement.

`html_hidden_inputs.html` compares hidden controls before blocks, within inline
text and table cells, with case variants, empty values, disabled state,
positioning, generated content, and author `display: block !important` overrides.
Public tests separately check extracted content, byte-identical output without
the hidden controls, and the absence of AcroForm fields.

`html_text_decorations.html` compares two pages of underline and strike defaults,
combined lines, ancestor colors, selected-font thickness, wrapping, author
clearing, atomic boundaries, relative positioning, and zero/transparent styles.
Metadata and caller precedence have separate public API tests. The existing
strict per-page pixel thresholds remain unchanged.

`html_list_attributes.html` compares two pages of starting values, reversed
and reset numbering, alphabetic and Roman hints, CSS overrides, inside wrapping,
styled item boxes, hidden items, nested lists, and preformatted newline/tab edges.
Its font files, page geometry, and strict per-page threshold match the other
focused fixtures.
HTML syntax coverage checks comments within text and tables, empty and unquoted
attributes, character references in attributes, quoted delimiters, and content
across an explicit page break. A two-page optional-tag fixture covers document,
paragraph, list-item, caption, column-group, table-section, row, and cell boundaries.
List checks cover outside markers, bullet characters, indentation, and item spacing.
A two-page static-element fixture covers nested lists, descriptions, quotations,
figures, semantic and legacy text, preserved whitespace, tab stops, author
ordering, and heading margins after a page break. Its monospace fonts use the
same pinned files in both renderers.
Flex alignment checks preserve explicit image and block dimensions while still
stretching automatic dimensions. A two-page compact stock-sticker fixture uses
synthetic item data and square QR images on 5 cm by 3 cm labels.
PNG coverage includes all legal static color types and sample depths, including
Adam7 interlacing. Font checks include subsetting and signed TrueType glyph deltas.

See the [parity tests](https://github.com/Cees-Kettenis/native_elixir_pdf_utilities/blob/main/test/html_to_pdf/browser_parity_test.exs) for the
current fixtures and thresholds. Each comparison writes its measurements and
tool versions to `tmp/browser_parity/<fixture>/stats.exs`.


`html_readonly_controls.html` compares text/search inputs and textareas with
minimized, empty, uppercase and `readonly="false"` attributes, empty values,
multiline content, combined disabled state and authored attribute selectors.
Its author styles use the existing pixel-aligned control geometry. Readonly adds
no disabled appearance. Public tests verify initial values, exact PDF flags,
read-only fill diagnostics, flattening and static byte equivalence. The remaining
fractional widget raster gap and complete native UA theme are still open.


`html_contact_inputs.html` compares telephone, URL and single-email values with
case variants, CR/LF, ASCII edge whitespace, empty controls, readonly/disabled
state, invalid initial strings and selectors on authored types and values.
It uses the existing pixel-aligned author styles. Public tests compare complete
PDFs with equivalent sanitized text controls and check values, generated names,
flags, filling, flattening and unsupported-attribute source diagnostics.


`html_multiple_email.html` compares single/list controls, case variants and boolean
presence, ASCII token edges, readonly/disabled state, empty values, empty tokens
and trailing commas. It uses the existing author geometry and thresholds. Public
tests verify complete text-equivalent PDFs, string field values, flags, filling,
flattening, original selectors, internal newline handling and failure locations.


`html_input_type_defaults.html` compares missing, empty, whitespace, unknown,
untrimmed, obsolete and non-ASCII lookalike type strings with Text controls.
It retains readonly/disabled states, empty values, CR/LF sanitation and author
selectors on raw type strings. Public tests check complete Text-equivalent PDFs,
field values/names, filling, flattening and unsupported-state diagnostics before
resource resolution. This covers type defaults, not the unfinished recognized states.


`html_input_buttons.html` compares submit/reset/button labels with case variants,
missing and empty values, disabled boolean presence, author alignment/selectors,
repeated spaces and multiline labels. Empty button elements retain their collapsed
geometry and never print their submission value. Public tests verify complete
button-equivalent PDFs, pagination, empty block/flex/grid height, no fields or name
reservation, appearance limits and labels retained after filling/flattening other
fields. Complete native sizing, long-label clipping and ownership remain open.


`html_control_state_selectors.html` compares enabled/disabled control colors with
boolean presence, readonly independence, type defaults, hidden inputs, button
states, multiline textareas, negation and ASCII case variants. Ordinary ARIA-styled
text retains its color. Public tests check exact class-selector PDF equivalents,
field flags, no hidden-content asset requests and CSS failure diagnostics. This
covers currently supported hosts; fieldsets, optgroups and inherited disabling remain
unfinished. Existing thresholds and fixtures remain unchanged.


`html_image_dimensions.html` compares width-only, height-only, distorted dimensions,
fraction and percentage prefixes, invalid ignored attributes, author overrides and
automatic sizing. Zero block/flex/grid images preserve following content and flow.
Unit tests compare complete PDFs against CSS equivalents in all three contexts and
object-fit modes, and check resource failures before assets, denied assets at zero
size and public CSS diagnostics. Numeric limits and browser thresholds are unchanged.


`html_password_inputs.html` checks masked ASCII, initial CR/LF removal, whitespace,
empty values, composed accents, supplementary scalars, a family emoji and a flag,
with readonly and disabled presence. Tests compare static PDFs to bullet references,
verify original interactive values/flags, masked fill/flatten artwork, external
password fields, aggregate appearance bytes and public limit/readonly failures.
Comb and rich-text flags retain their diagnostic. Browser thresholds are unchanged.


`html_script_data_blocks.html` checks hidden data/resource-looking strings, raw
character references, escaped/double-escaped end boundaries, empty data, MIME
parameter strings, whitespace-only types and legacy non-JavaScript language.
Unit checks retain all raw DOM data, compare complete static PDFs against literal
references and check source/text limits and executable-content diagnostics.
The browser threshold remains below 1% per page. Source preprocessing and generic
malformed recovery are separate unfinished/excluded rules.


Browser comparisons now create each fresh Chromium profile with JavaScript denied.
The comparison record includes `scripting: :disabled`. A reference-only probe
checks that `noscript` fallback prints and a script that would replace the page
cannot run. Native comparison uses equivalent ordinary paragraphs; executable
input rejection and remaining native noscript/composition boundaries are retained.
This makes the reference mode explicit for later composition work. Earlier batch
comparisons of markup without executable scripts remain useful, but should not be
described as having explicitly disabled scripting.


`html_inert_templates.html` checks head/body/nested unused content, denied-looking
fonts/images, duplicate field names, literal script data and an unused SVG tree.
Selectors count the template element but cannot reach its separate fragment.
Author CSS exposes two empty bordered boxes; all visible labels remain present.
The initial scripting-disabled comparison passed at 0.149712% changed pixels on
one page. Full browser/native pages were inspected, with the existing 1% per-page
threshold unchanged. Focused checks cover direct DOM shape/UTF-8 failures, static
input policy, default node/depth limits, complete PDFs and active form ownership.


`html_positional_selectors.html` checks all four forward/reverse child/type
functions, signed/zero coefficients, bounded sequences, only-child/type,
compound negation and hidden template siblings. All labels and geometry remain
present. The scripting-disabled one-page comparison passed at 0.579837% changed
pixels with the unchanged 1% threshold; both full pages were inspected.


`html_script_supporting_parents.html` checks actual list/DL/table/select sibling
identity, numbering, visible list data and empty template boxes, implicit table
groups and selected-choice labels. The one-page scripting-disabled comparison
passed at 0.824592% changed pixels with every label retained and both full pages
inspected. An initial default select comparison was 1.503497%; existing supported
explicit control dimensions retain all cases and pass. Default intrinsic control
geometry and arrow painting remain unfinished, with unchanged tolerances.


`html_noscript_initial.html` retains inline entities/emphasis, active head styles
and metadata, explicit block fallback with an editable field, and unused template
fallback with duplicate names, styles and a denied-looking image. Scripting is
disabled. One-page parity passes at 0.213068%, with complete pages inspected.
The initial fractional control comparison and full pages remain preserved at
1.281566%; the final source uses the existing explicit input fixture style while
keeping every case. Native default control geometry remains unfinished.


`html_anonymous_inline_blocks.html` retains default inline noscript around a block,
text before/after nested CSS blocks, percentage-width table-cell content, atomic
inline flow and painted parent margin collapse with and without padding. Pinned
and host parity pass at 0.441632% on one page. Both complete pages and every label
were inspected; tolerances are unchanged.


`css_identifier_escapes.html` compares Unicode/combining/nonbreaking class names,
escaped punctuation and type/attribute/pseudo names, Unicode case and generated
prefixes. Pinned and host parity pass one page at 0.366162% changed pixels. Both
complete pages and all eight labels were inspected; extracted text matches.
Tolerances remain unchanged.


`html_custom_containers.html` compares Unicode custom names, inherited inline
content, nested block flow, custom block styling, empty content, inert templates
and builtin `is` metadata. Its complete page passes pinned Chromium at 0.430871%
changed pixels and extracted text matches.

`html_hidden_attribute.html` compares empty/invalid keywords, author display
overrides, custom hidden descendants, actual hidden siblings, dormant duplicate
controls, active stylesheet metadata and visible literal data. Its complete page
passes at 0.301227%. Every visible label matches; extraction differs by a blank
line. Both full page pairs were inspected. Tolerances remain unchanged.

`html_inert_attribute.html` retains visible initial controls, inherited/custom
inert state, inactive link regions, real sibling selectors, empty content and
author colors. It passes at 0.722854%. Every visible label and initial value is
retained; extraction differs by a blank line.

`html_semantic_inline.html` retains two pages of mark/ins/del/strike defaults,
overrides, nested colors, wrapped backgrounds, mixed font sizes, relative
positioning, atomic inline contents, editable values and active styled links.
Its pages pass at 0.839360% and 0.750402%. Active links have explicit authored
blue/underline styles because complete anchor UA defaults remain open. The
existing two-page text-decoration comparison remains below 0.607587%.


`html_anchor_defaults.html` compares placeholder inheritance, default HTTP/HTTPS/mailto colors and underlines, wrapping, nested bold, inert and hidden content, empty content, and author overrides. The complete page passes at 0.989899% with identical extracted text. Full native/Chromium pages were inspected. Existing inert and protocol/entity pages pass at 0.722854% and 0.091190%; per-page tolerances remain unchanged.


`html_inline_word_boundaries.html` retains styled words, spaces, hyphens, empty relative markers, nowrap/pre-wrap, emergency wrapping, highlight backgrounds and combining marks split across styles. Both complete pages pass the unchanged 1% threshold at 0.818182%/0.584596%, with identical extracted text and full-page inspection.


`html_soft_breaks.html` retains nine complete pages with empty, consecutive,
leading and trailing opportunities; own and ancestor styles; nowrap/pre/nobr;
relative and absolute contexts; authored display:none; actual sibling counting;
active and inert URI regions; pseudo/counter effects; empty flex/grid slots;
forced and optional empty lines at automatic page boundaries;
styled words, hyphens and atomic table minima; mixed column allocation; expanded
captions; and styled nowrap spaces. Every page passes the unchanged 1% threshold.
Complete word positions and line grouping are checked separately. The fixture
retains every earlier case. Full-height blocks place empty lines at automatic
page boundaries, with separate coordinate and complete-PDF assertions. A full-input
synthetic-table diagnostic records the existing seven-native-versus-nine-Chromium
page disagreement; that broader table/pagination gap remains open.


`html_heading_offsets.html` covers offsets, inclusive resets, parsing edges, the level-nine cap, valid custom containers, flex/grid, anonymous flow, inline and atomic headings, links, inert/hidden content and unused templates. Every page uses the unchanged 1% threshold. Exact bookmark trees and complete visible/reference PDFs are checked separately. The retained mixed-inline diagnostic originally recorded a 0.75-point baseline gap. It is now tested intact by css_inline_block_baselines.html.


`css_inline_block_baselines.html` retains the complete mixed-heading case and adds single/multiline, declared-height, empty, overflow-hidden, block-child, relative, nested and blank-line blocks. Both pages pass the unchanged 1% threshold. Focused tests verify exact baselines, line extents, trailing text placement and complete reference PDFs with URI regions and initial editable fields.


The native form appearance path retains all 124 existing browser comparisons and their thresholds. Direct/legacy object-graph tests cover appearance artwork and resources, while mixed-control scaling checks compare every initial flattened text coordinate with independent static output and retain field values, flags and URI labels.


The language-selector fixture compares inheritance, explicit overrides, empty and
unknown values, escaped arguments, language lists, wildcard subtags, words and URI
regions. Pinned Chromium ignores direct list and wildcard rules; its reference
uses equivalent separate selectors and `:lang(de-DE)` for the fixture's single
German tag. The native test renderer substitutes the specification syntax while
retaining markup, text, fonts, links and page geometry. The direct 3.349% changed
pixels remains a recorded browser capability disagreement. Equivalent markup
passes the existing less-than-1% threshold. Alias/extlang canonicalization and
singleton boundaries have direct validator coverage.

The `html_form_ownership.html` fixture covers ordinary form and label containers,
last-authored radio checkedness, separate owners, forward explicit references,
and independent nameless controls in static mode. The 1% pixel threshold remains
unchanged. Focused tests also check effective PDF values and recoverable mapping
failures. Native circle artwork still differs from Chromium's control skin.

The `html_selection_state.html` fixture checks last-authored and first-enabled
selection, explicit disabled selections, empty/all-disabled/blank dropdowns and
initial checked/negated checked input selectors. All captions and selected text
are checked in addition to the unchanged pixel thresholds. Focused tests verify
PDF values, empty selection round trips and recoverable malformed-state errors.

The `html_option_semantics.html` fixture checks label attributes, normalized text,
disabled groups, explicit disabled selections and intrinsic empty/blank/unselected
dropdown height without an authored height. Captions and selected text are
checked in addition to unchanged pixel thresholds. Layout coverage spans block,
inline-block, flex and grid placement and native line-height resets.

The `html_popover_initial.html` fixture checks initial closed Auto, Hint, Manual
and invalid-value popovers, nesting, actual sibling positions and static
`:popover-open` matching. Both outputs must retain every visible caption, omit
all closed text and create no fields. The existing pixel thresholds remain.
Unit tests separately cover CSS-exposed diagnostics, hidden input precedence,
unused resources, radio-group state, executable markup and active URL failures.

The `css_named_colors.html` fixture retains all 148 named colors in foreground,
background and borders, including fractional-height atomic inline baselines.
The `html_horizontal_rules.html` fixture covers native defaults, legacy hints,
author overrides, malformed values and visible content around rules. Browser
references explicitly document differences in Chromium's legacy rule sizing.
The existing `html_positional_selectors.html` fixture now exercises all ten
structural positional pseudo-classes on the document element. Original captions,
page checks and the less-than-1% changed-pixel threshold remain unchanged.


The existing `html_control_state_selectors.html` fixture now includes requirement
styling for empty/default fields, boolean presence, readonly and disabled text,
textarea and dropdown controls. Original captions remain, hidden inputs stay
unpainted, and both PDFs retain all visible captions after appearance flattening.
Optional styling explicitly excludes inapplicable hidden/button input types,
whose native applicability is covered independently by validator/style and
complete PDF reference tests. The new section uses a small top margin to align
its borders on whole-point coordinates; general fractional field-border painting
remains open. The fixture preserves the 1% pixel threshold and a single page.

The `html_disclosures.html` fixture compares authored open/closed state, boolean
attribute presence, fallback legends, nesting, named groups, marker overrides
and block summaries. Both PDFs must preserve every visible caption and omit
closed body text. The unchanged pixel threshold includes default marker shape
differences. Focused tests check pagination, fields, unused assets, original DOM
positions, template-group isolation and recoverable input failures.
