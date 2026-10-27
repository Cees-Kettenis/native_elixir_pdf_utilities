"""Record STIX italic corrections independently of the Elixir renderer.

Requires fontTools 4.60.1. Reads only the pinned font and authored HTML fixture.
The reviewed cases describe MathML Core row transitions. Add correction before
a token with zero correction or at a row end, but not between two corrected
tokens. Whitespace prevents math-auto's single-character substitution.
Run with --check to verify the retained JSON without writing it.
"""
import argparse
import hashlib
import json
from pathlib import Path

import fontTools
from fontTools.ttLib import TTFont

ROOT = Path(__file__).resolve().parents[2]
FONT = ROOT / "priv/fonts/stix/STIXTwoMath-Regular.ttf"
FIXTURE = ROOT / "test/fixtures/html_to_pdf/browser_parity/mathml_tokens_rows.html"
OUTPUT = FIXTURE.parent / "assets/math-token-italic-reference.json"
FONT_SHA = "49b22767985171bce579307b21d98f4d35012fa39e316acc4ffcab8644dfb69b"
# Changing the authored fixture requires reviewing the fixed row-transition cases.
FIXTURE_SHA = "d3e63db0f8278988e3e31316ad136f254a9d815a5e96b318c0e774410ad5e276"
assert hashlib.sha256(FIXTURE.read_bytes()).hexdigest() == FIXTURE_SHA
assert fontTools.__version__ == "4.60.1"
assert hashlib.sha256(FONT.read_bytes()).hexdigest() == FONT_SHA
font = TTFont(FONT, recalcTimestamp=False)
cmap = font.getBestCmap()
units = font["head"].unitsPerEm
info = font["MATH"].table.MathGlyphInfo.MathItalicsCorrectionInfo
corrections = dict(zip(info.Coverage.glyphs, info.ItalicsCorrection))
glyphs = {}
# Reviewed Unicode Mathematical Italic substitutions for the authored scalars.
for text, codepoint in [("x", 0x1D465), ("y", 0x1D466), ("z", 0x1D467), ("α", 0x1D6FC)]:
    name = cmap[codepoint]
    metric = corrections.get(name)
    assert metric is None or metric.DeviceTable is None
    glyphs[text] = {"codepoint": codepoint, "glyph": name,
                    "glyph_id": font.getGlyphID(name), "correction_units": metric.Value if metric else 0}

transition_html = ('<math><mi data-core-reference="x">x</mi>'
                   '<mi data-core-reference="y" style="font-size:20pt">y</mi><mn>1</mn>'
                   '<mrow data-core-reference="row"><mi data-core-reference="end">x</mi></mrow>'
                   '<mn>2</mn><mi data-core-reference="space"> x </mi><mn>3</mn></math>')


def margins(cases):
    return {identity: {"token": token, "size_pt": size, "applied": applied,
                       "points": glyphs[token]["correction_units"] * size / units if applied else 0.0}
            for identity, token, size, applied in cases}


result = {
    "generator": "scripts/fixtures/generate-mathml-token-reference.py",
    "fonttools": fontTools.__version__,
    "font": {"path": str(FONT.relative_to(ROOT)), "sha256": FONT_SHA, "units_per_em": units},
    "glyphs": glyphs,
    "cases": [
        {"name": "mathml_tokens_rows", "source": str(FIXTURE.relative_to(ROOT)),
         "sha256": hashlib.sha256(FIXTURE.read_bytes()).hexdigest(),
         "margins": margins([("x", "x", 12, True), ("y", "y", 20, True), ("z", "z", 30, True)])},
        {"name": "row_transitions", "html": transition_html,
         "sha256": hashlib.sha256(transition_html.encode()).hexdigest(),
         "margins": margins([("x", "x", 12, False), ("y", "y", 20, True),
                             ("row", "x", 12, True), ("end", "x", 12, True),
                             ("space", "x", 12, False)])}
    ]
}
encoded = json.dumps(result, indent=2, sort_keys=True) + "\n"
parser = argparse.ArgumentParser(description=__doc__)
parser.add_argument("--check", action="store_true")
args = parser.parse_args()
if args.check:
    assert OUTPUT.read_text() == encoded, "Regenerate the independent MathML token reference"
else:
    OUTPUT.write_text(encoded)
print(f"Verified independent MathML token reference: {OUTPUT.relative_to(ROOT)}")
