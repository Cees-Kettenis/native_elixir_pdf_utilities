# Ordinary CSS MathML groups

`mrow`, `mstyle`, `merror` and unknown MathML row groups accept ordinary CSS
flow, flex and grid displays. This follows the pinned
[MathML Core CSS display rule](https://www.w3.org/TR/2025/CR-mathml-core-20250624/#the-display-block-math-and-display-inline-math-value).
The actual source owner goes through shared HTML child measurement. Width,
height, supported `min()` expressions, min/max dimensions, margins, padding,
borders and box sizing use its native CSS rules. Paint and logical text retain
the original Math children and selected font data.

Computed source display stays intact. Math, flex and grid parent contexts issue
the used block-container display through the canonical validator. CSS flow
parents retain ordinary inline sizing behavior. Math parents use the first
actual content baseline, while CSS flow retains its existing atomic baseline.
Nested native owner boxes paint their decorations once.

Default math-formatting `math`, `mrow`, `mstyle`, `merror` and unknown row
owners also use shared CSS width, height, min/max and box-sizing resolution.
Their authored Math content keeps its original selected glyphs and stays
aligned with the content box start edges when the requested box is smaller.
Owner padding, borders, margins and backgrounds paint once. Definite owner
content widths provide the basis for descendant CSS child layout. Block math
roots keep their actual native CSS owner around an anonymous atomic Math box,
so normal CSS layout handles constrained and automatic block widths.

Padding changes line extents without changing token ink baselines. Actual
upper/lower borders set the corresponding ink baselines. Inline edges also
contribute to existing italic correction and top accent attachment as prescribed by
MathML Core section 3.1.2, so stretching and accents see source box geometry.

Default inline Math roots and row groups support relative placement and hidden
overflow through the actual CSS owner and shared native child painter. The
first content baseline stays independent of the atomic bottom baseline used
by ordinary hidden CSS boxes, so adjacent Math children retain their baseline.

Math tables and cells accept decoded background images through the existing
CSS background painter. Row images share their row positioning origin across
occupied cells, with each occupied rectangle clipping its own paint. Separate
border spacing remains visible between those rectangles. Separate-border rows ignore their authored padding and borders, while the
prepared source style stays intact. Invalid prepared
image descriptors return the shared Math table diagnostic.

Absolute/fixed Math owners and positioned/clipped cells,
additional CSS calculation grammar and Ruby CJK wrapping remain separate
implementation work. Full browser parity, quality matrix, coverage and fixed-baseline
performance acceptance remain open.

Default `msqrt` and `mroot` owners also resolve shared CSS width/height
expressions, min/max constraints and box sizing. Their definite content widths
provide the CSS percentage basis to radicand descendants. Relative placement
and hidden overflow use the same real source owner path as default row groups.
Completed owner geometry carries its canonical completion flag into ordinary
CSS composition so source borders, padding and dimensions are consumed once.
Selected radical variants, overbars, degree placement and fonts keep their
pinned Core geometry. The preserved original Core/Blink physical differences
remain separate acceptance gaps.

Default `mo` owners now use the same CSS dimensions, box-sizing, relative
placement and hidden overflow path. Operator semantics, actual selected glyph
advance, and preferred stretch width remain separate from the author CSS box.
Ordinary group composition consumes the complete owner once. Operator and
radical background images use the decoded shared background painter.
Display-style large operators select a variant by its actual outline height,
as Core requires; stretchy selection continues to use MATH advanceMeasurement.
