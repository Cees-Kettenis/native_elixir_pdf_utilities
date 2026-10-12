# MathML 1.1.0 implementation checklist

MathML rendering remains unfinished. Track the pinned
[MathML Core snapshot](https://www.w3.org/TR/2025/CR-mathml-core-20250624/)
and [HTML integration](https://html.spec.whatwg.org/#mathml), rather than only
HTML element rows. Every row needs tests, print comparisons, diagnostics, and
performance evidence before completion.

| Requirement | Acceptance |
| --- | --- |
| Namespace integration | Foreign attributes, token integration points, `annotation-xml`, fragments |
| Tokens | Whitespace, identifiers, numbers, strings, spaces, embedded text |
| Operators | Dictionary, form, embellishment, spacing, accents, stretching |
| Rows | Anonymous groups, baselines, stretch order |
| Fractions | Numerator/denominator measurement, zero/nonzero rule |
| Radicals | Root indices, variant selection, assemblies |
| Scripts | Sub/superscripts, limits, prescripts, depth and alignment |
| Grouping | Style, errors, padding, phantom geometry |
| Tables | Rows, cells, alignment, nested matrices |
| Selection | Semantics, annotations, static action state, unknown MathML |
| CSS | Math display, transforms, style, shift, depth, default stylesheet |
| Font data | Constants, glyph information, corrections, variants, assemblies |
| Composition | Inline/block placement, atomic pagination, surrounding content |

Direction and broader shaping retain their named 1.2.0 dependencies. Static
selection must preserve content without enabling scripts or interactions.

The plan requires pinned, unmodified STIX Two Math and its complete license.
Caller overrides must select their own metrics. Missing glyphs or metrics require
a diagnostic. Read the selected font's
[OpenType MATH data](https://learn.microsoft.com/en-us/typography/opentype/spec/math);
do not replace it with dimensions copied from the bundled font.
Supplementary mathematical characters need
[Unicode cmap coverage](https://learn.microsoft.com/en-us/typography/opentype/spec/cmap#format-12-segmented-coverage).

Font offsets, coverage, glyph IDs, counts, assemblies, and decoded work require
validator checks and shared limits. Parsing and caches must remain tied to
selected font bytes. Child resources retain their caller authorization.

`Font.math_metrics/1` now reads constants, italic corrections, accent attachments,
extended-shape coverage, corner kerning, device adjustments, and vertical and
horizontal variants and assemblies from the selected TrueType font. It validates
offsets, counts, coverage order, glyph indices, and decoded work in validators.
The shared `max_font_math_work` limit counts decoded bytes and expanded entries
cumulatively within a font-budget scope. Memoized results retain their source
bytes and are reused only in that scope. Ordinary font loading does not parse
MATH data. The `:substitutions` result now reads the selected default `math`
language system's `ssty`, `flac`, and `dtls` GSUB features. Ordered single,
alternate, and extension lookups retain their glyph lists. Missing optional
features leave the original glyphs unchanged. Unsupported contextual lookups,
glyph filtering, and feature variations return diagnostics. A mathematical glyph measurement layer now applies script substitutions and
retains their single/alternate lookup kinds. Complete token styling and equation
composition still need integration. `Font.glyph_bounds/1` reads glyph ink bounds from the selected font's
TrueType outlines. Location tables, header bounds, and cumulative decoded work
are validated; identical bytes reuse their result within a font budget.

The HTML parser now retains MathML and SVG namespaces, foreign attribute
identities, integration points, and foreign fragment contexts. Tokens inside
MathML text integration points use the HTML rules, with the required `mglyph`
and `malignmark` exceptions. `annotation-xml` honors its encoding and SVG
integration rules. The low-level PDF writer now accepts explicit selected glyph IDs while retaining
original Unicode and selected widths. A writer fixture compares equivalent
PUA aliases in an independently generated, renamed font subset because Chromium
did not apply `ssty` to the tested text or MathML. Both comparison paths use
identical reference font bytes; glyph geometry, hints, and advances are checked
against pinned STIX. Direct writer tests also use unmodified STIX. Equation
geometry, substitution application in layout, stretch assembly text semantics,
and complete mixed-content rendering remain unfinished.

Measure cold renders, repeated equations, nesting, many operators, reductions,
memory, and output size separately from unchanged HTML workloads. Inventory rows
remain partial until all included behavior and release requirements pass.

`MathLayout` now measures selected glyphs and rows. Its validator owns input
shape, italic Unicode mapping, script feature selection, missing-glyph
failures, source metrics, and device adjustments. Empty rows and baseline/italic
spacing have focused tests. Advance tuples reuse the shared font measurement
cache and work budgets. These functions are geometry building blocks, not
complete MathML element layout.

The glyph-row fixture compares an independent CSS reference at 30 points.
A direct 30-point MathML experiment differs because Chromium uses raster ink
bounds. Its 3.640% changed-pixel result and baseline differences are retained
under `.quality/math-row-unadjusted/`. A 30-point equivalent CSS experiment also
initially had a 1.020% raster difference. The selected-glyph horizontal tie
correction now makes that CSS reference pixel-identical, and the accepted fixture
has returned to 30 points. The direct MathML ink-metric difference remains open
for renderer acceptance; no standards checklist row becomes complete from this fixture.

The writer now accepts a text-only `:glyph_group` with one `:actual_text` string.
It emits `/Span` marked content so several painted operator pieces can preserve
one original Unicode string. The extractor applies the replacement once,
validates every piece and counts suppressed text against its resource limits.
The glyph-row foundation fixture now checks exact Unicode without stripping
whitespace. HTML integration remains unfinished.

`MathLayout.stretch/6` now selects natural glyphs and ordered variants, then
constructs horizontal or vertical assemblies. Every extender repeats equally.
Connector overlap stays within the font's bounds, expansion reserves shared work
and layout budgets first, and original parts supply orthogonal dimensions even
when an extender is omitted. Unusable assemblies fall back to the final variant.
The independent CSS fixture includes parentheses, radicals, integrals and a
horizontal brace, with exact replacement Unicode for each assembled operator.
Operator dictionary rules, symmetry about the math axis, min/max sizing and
equation composition still need integration.

The pinned [MathML Core section 5.3.1](https://www.w3.org/TR/2025/CR-mathml-core-20250624/#the-glyphassembly-table)
requires minimum overlap at every part endpoint, including unused outer ends,
and excludes the opposite ends when calculating maximum overlap. That wording
would reject STIX's ordinary assemblies with zero unused connectors.
[OpenType MATH](https://learn.microsoft.com/en-us/typography/opentype/spec/math#glyphassembly-table)
permits an absent connector to have length zero.
[Chromium 152.0.7977.82](https://chromium.googlesource.com/chromium/src/+/refs/tags/152.0.7977.82/third_party/blink/renderer/platform/fonts/shaping/stretchy_operator_shaper.cc)
checks only the ends that actually join pieces. The implementation follows that
actual-join rule. This is a recorded specification disagreement, not complete
conformance to the dated Core wording. All-extender assemblies require at least
one repetition for a positive target, avoiding the dated formula's empty result
when the target is smaller than minimum overlap. Focused synthetic tests cover
this edge case. STIX's right-arrow assembly has a zero connector at a required
join and therefore uses its last variant; its font data is not patched.


Stretch repetition now checks the shared font-work ceiling before dividing by
extender growth. A bounded target with an extremely small positive font size
returns a resource diagnostic instead of raising an arithmetic exception.
Single-glyph validation matches one UTF-8 scalar directly, so rejecting long
multi-scalar input does not allocate a full character list. Existing font tests
cover both cases; these checks do not complete equation composition.

## Bounded radical composition

`msqrt` and `mroot` now prepare selected-font constants, measure radicands and
degrees, select surd glyphs and place the overbar through the shared native math
layout. Validators own measured transport, numeric bounds and font work limits.
Native text and links retain their original ownership.

The four pure radical trees in the fixed fixture use a reviewed source-derived
CSS artwork comparison. The original direct Chromium comparison failed and is
retained separately. The linked root stays an unchanged MathML tree. The generator
and independent verifier are retained in `test/fixtures/mathml_radical_reference`
with the pinned font derivative, source records and complete license. Independent
agreement does not settle the dated indexed-root line/baseline and degree-offset
ambiguities or assembly endpoint wording. This is bounded implementation evidence,
not full Core conformance, direct interior parity or accessibility output.

Scripts, limits, accents, remaining schemata and broader combined contexts remain
required. Source-preview traversal leaves MathML font contexts unresolved rather
than loading a font or inventing selected MATH metrics.
