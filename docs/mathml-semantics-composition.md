# MathML semantics and mixed annotation composition

`semantics` and `maction` initially expose their first element child. Text and whitespace do not change that selection. A hidden first element stays selected; native does not choose a later fallback. Later annotations remain authored source nodes for selectors and static validation, and they do not fetch assets or allocate measured glyphs while hidden. Author CSS can hide the first child and expose alternatives.

The MathML Core annotation and annotation-xml algorithms use mtext geometry. The encoding attribute affects HTML namespace integration for text/html and application/xhtml+xml; it does not choose an alternative or change token rendering. Native retains this Core behavior for standalone annotation text. Pinned Chromium 152 omits standalone annotations in the original mathml_semantics_annotations.html source, including the nested annotation in its maction case. The existing original logical-content check therefore remains red. The fixture, checks and fixed thresholds remain unchanged. Native does not add invisible content or replace the authored source to match this browser departure.

CSS-visible annotations with ordinary inline, block, inline-block, flex or grid display use the existing HTML child measurement contract. They preserve the owner's width, height, minimum/maximum constraints, box sizing, background, border, padding, line layout and first alphabetic baseline. Owner edges and paint are measured once. Flex and grid content use native HTML composition. Direct atomic MathML and SVG children stay as actual source atoms inside anonymous CSS item owners, so the shared item builders consume their prepared paint rather than empty child lists. This integration does not implement a second layout engine. Selected CSS-visible payloads use the ordinary resource authorization and shared diagnostic contract.

HTML phrasing descendants, nested math contexts and authorized SVG scenes can share an annotation or token. A MathML entry reached through HTML or a token owns an independently prepared atomic math box. Nested fractions, rows and other supported schemata keep the existing selected-font metrics, drawing order and atomic overflow policy. Original Unicode contributes to extraction once. SVG text uses the scene's actual selected-font logical content; raster and vector artwork remain actual scene paint. Generated metadata and extraction-only surrogate text do not supply missing artwork.

The mathml_annotation_composition.html fixture checks sized multiline owners, HTML containing nested math, CSS-exposed XML alternatives, genuine SVG artwork and text, and unselected mixed sources. The mathml_annotation_css_alternatives.html fixture checks real flex and grid annotation owners with nested math. The mathml_annotation_svg_flex.html fixture checks selected SVG artwork and real text in flex and grid annotation owners. All three retain the standard 1% pixel and average-delta gates and require their original visible logical content in each renderer. The existing mathml_html_children.html and mathml_semantics_annotations.html sources remain unchanged.

Dependencies and remaining integration work are finite. MathML grouping/display-contents behavior, fractions/radicals with ordinary CSS schemata, matrix owner boxes and MathML link geometry remain with their assigned owners. SVG image/resource support uses the existing scene resource owner; this batch adds no SVG resolver. The original standalone-annotation Core/Blink disagreement still needs a milestone acceptance decision, while complete browser parity, the quality matrix, coverage and performance remain deferred until feature integration ends.

References: [MathML Core semantics and presentation](https://www.w3.org/TR/mathml-core/#semantics-and-presentation), [MathML Core visual formatting](https://www.w3.org/TR/mathml-core/#visual-formatting-model), and the preserved original parity artifacts in the lane packet.

Default math-formatting `mi`, `mn`, `mtext`, `ms`, `annotation` and
`annotation-xml` owners now resolve shared CSS dimensions, min/max constraints
and box sizing. Relative placement, clipping and decoded background images
use the genuine source owner around unchanged token glyphs or HTML/SVG child
paint. Definite owner content width supplies the descendant CSS percentage and
line-wrapping basis. The first real child baseline remains authoritative;
ordinary CSS groups consume completed token edges once. Hidden annotations
retain initial-selection and no-asset-allocation behavior. Standalone annotation
omission by Chromium remains the preserved original compatibility gap.

Block-equation percentage owners now retain the actual source for placement
remeasurement. The canonical Math layout validator discovers percentage
dependencies in ordinary sizes and validated CSS calculations; table cells
keep their separate allocated-width boundary. An enclosing 200pt box therefore
supplies the real basis for a token `width:calc(50% + 10pt)`, producing 110pt.
The table validator delegates this source-context policy instead of maintaining
a second interpretation.

Direct HTML children of default Math token owners use the canonical Math
parent formatting context, including source-preserving blockification. Source
fragment targets on token, group, operator and radical owners reach the actual
native paint box and scoped PDF destination. The anonymous inner atom does
not duplicate the target. Decoded image validity is checked by the shared
writer validator for both token and table backgrounds.

The new combined token-owner fixture retains the standard 1% browser gates.
Its first row of sized and bordered token owners has no differing pixels, but
the complete mixed HTML/SVG/nested-fraction fixture remains above the gate.
That physical comparison remains open. The original annotation composition,
alternatives and SVG/flex fixtures retain their logical and pixel checks.
