# Nested SVG text lengths

Inline SVG ancestral and nested `textLength` with `lengthAdjust="spacing"` uses the actual selected glyph metrics in the original source hierarchy. Descendants resolve first. An ancestor distributes spacing over its typographic characters and resolved descendants, preserving the following text cursor and the original Unicode. Parent adjustment can subsequently change a descendant's final extent. This follows the [SVG 2 text-length algorithm](https://svgwg.org/svg2-draft/text.html#TextLengthAttribute).

The temporary natural-metrics parse and final parse share the same byte-only font database, resource bindings, viewport and source text. Compiler-issued `dx` replaces length declarations only in the separate prepared source. Original IDs and UTF-8 text stay intact. Final placement verifies actual font and glyph identities, cluster text and positions. This internal geometry evidence grants no original-source or document permission.

Temporary source copies, retained source strings and distribution work charge existing SVG source and expansion budgets. Ordinary text and existing simple-length cases use their previous route. Composite glyph scaling, rotated/path length distribution and absolute-position chunks retain diagnostics until their actual geometry is implemented.

Focused regressions cover nested hierarchy, Unicode, embedded original font bytes and shared budget diagnostics. The unchanged original browser fixture checks real visible text in both PDFs as well as the fixed pixel thresholds.
