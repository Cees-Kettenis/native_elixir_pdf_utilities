This reconstruction is retained as historical evidence. The active browser test
now compares `mathml_radicals.html` directly against Chromium without replacing
its MathML. Its text and link behavior remains covered by the ordinary radical
tests. The generator, source font and old comparison evidence below are archival.

# Fixed radical artwork comparison

This directory retains the approved source-derived CSS artwork comparison for
exactly four pure roots in `html_to_pdf/browser_parity/mathml_radicals.html`.
The native renderer receives that original authored fixture. The browser helper
accepts only its pinned SHA-256 and returns the pinned complete reference.
The linked sqrt, link URL, captions, math sizes and all outside content stay
byte-identical. Only the four retained outer sqrt/root tags get ordinary CSS
display, width, height, zero line-height and zero font-size transport. The
interiors use source glyph contours and rules through mtext integration hosts.

The unchanged manifest records the proposal before activation. Its
`activation: false` is retained archival metadata. Activation belongs to the
fixed fixture helper in `browser_parity_test.exs`, after independent verification.
It does not authorize an arbitrary root transform or assembly artwork.

The complete reference SHA-256 is
`dee328c2a651e5f0c8770658e71055d1b4606dbbf39c4259dfedd376fd89d070`.
The original fixture SHA-256 is
`cc76de79b7b35335a26c78ffe0efa09a5832b31cc5f6798d72365602ce042666`.
The renamed Math Radical Core Reference font retains selected original STIX
outlines, instructions, advances, global hints, units and hhea metrics. Its
SHA-256 is `57c71736ad6f2fffbb9cd1d4c4ca23dd8f5a90b0aed048246ba981a89379439f`.
The complete original OFL is retained alongside it.

`generate-reference.py` retains the exact reviewed generator bytes. It reads only
the authored fixture, pinned STIX and the June 24, 2025 MathML Core snapshot.
`verify-reference.py` imports no generator or renderer. It independently derives
all four records using rational arithmetic, reads the pinned default latn GPOS
script/language/kern feature/lookups for both selected source pairs, and parses
both actual HTML outputs. It checks every glyph, rule, baseline witness and
outer dimension against the separate source records. Both scripts require
fontTools 4.60.1. With the pinned Core snapshot at `tmp/references/mathml-core.html`,
run them from the repository root in that Python environment:

```sh
python test/fixtures/mathml_radical_reference/generate-reference.py --check
python test/fixtures/mathml_radical_reference/verify-reference.py
```

The scripts' three-level path location preserves the generator without changing
its bytes. `proof.json` is the deterministic source-only verifier output.
No lib, beams, native boxes, browser geometry, PDF, raster or delta feedback is
used in generation or verification. CSS font ascent rounding is the declared
source hhea transport; this fixture does not establish that premise for other
fonts, sizes or contexts.

Direct scripting-disabled Chromium comparison failed at 1.7924382716049382%
changed pixels under the unchanged 1%/1% gates. The retained adapted experiment
passed at 0.7986111111111110%. This is a CSS artwork structural comparison. It
does not establish direct MathML interior parity, accessibility or full Core
conformance. Native Unicode extraction and links are tested separately.

The dated indexed-root text has unresolved inconsistencies. Its line ascent
omits index descent although its baseline includes it. Its base offset sentence
omits the following surd and after-degree kern. The implementation and proof
use the written line equations, stated baseline and complete-B translation.
Their agreement does not resolve that ambiguity. Actual-join assembly wording
also remains open. The fixed roots select natural GID 1657 and nested variants
1659/1660; this comparison contains no assemblies.
