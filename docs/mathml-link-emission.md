# MathML link emission

An enclosing HTML anchor can link an inline or block Math equation to a validated
URI or initial HTML fragment. Layout emits an annotation-only `link_region` from
the equation's measured width, ascent and descent. The original glyphs, native
Math text children and source records stay intact. Row and cell background
regions use their actual allocated geometry and validated link state.

Math layout and table validators share link-style validation. A prepared
fragment must satisfy the existing HTML link validator's destination contract;
raw fragment maps, conflicting URI and fragment state, and invalid URI values
return the shared actionable diagnostic. Existing Math sizing and formatting
constraints remain in force. A default inline anchor may carry an actual block
Math equation through the existing anonymous flow splitter when its other
formatting satisfies that splitter's constraints.

Equation text decorations use the actual measured baseline and width. The
shared drawing geometry and writer authorities clip annotation rectangles at
ordinary overflow and embedded-document viewports. Child document destinations
retain their issued document scope, and pagination resolves fragment coordinates
against the selected target's real page. No source IDs, target identities or
permission scopes are fabricated to admit an equation.
