# Source-pinned fraction comparison

The marked `mathml_fractions.html` remains the actual source rendered by the
public native API. The browser comparison replaces eight top fraction interiors
with independently Core/font-derived supported CSS artwork. Their outer `mfrac`
tags and authored attributes remain. This is an adapted structural reference,
not direct Chromium MathML-interior parity. The independent ordinary fixture
`mathml_fractions_ordinary.html` uses direct, unchanged Chromium MathML.

The coordinator approved the exact Core/CSS generator proposal SHA256
8a0036482d4f351a76431e8ef86c2d14e8d4dfa1d9859ad46be2783bb6d86aae.
Production changes only restore checkout paths, accept a verified `--core-source`
path and enforce the approved output hashes. The complete reference remains
3356f56ab4bc49b93c56fe25e2d04caea2aca84e5b2d7d4b376bfa890381a8b5,
fragments af4d0ed9659cdab757f92d6c16b69b9b4f9c85a48fa270223f4fb795ebf65cef,
and manifest 0201c16d80858e1700a0ca492451d3e179a4a68ac669f98ae1d172cc0755dcc2.
The immutable proposal manifest retains its historical `activation:false` field;
fixture configuration controls activation. Approval is for this experiment and
its gates, not native M3 acceptance or future reference changes.

Geometry uses rational original STIX outlines, advances, MATH constants, default
math ssty GSUB and source GPOS pair advances. Core 3.3.1, 3.3.2 and B.1 supply
fraction shifts, gaps, depth, operator form and spacing. The independent verifier
reads source font tables and the authored tree directly. It imports no generator,
library, native measurement, browser output or pixel measurements.

Each manifest record separates interior glyph/rule geometry from outer CSS
content width, ascent, descent and baseline witness. One CSS pixel equals 3/4
point, including Core's default fraction padding. Reviewed hhea rounding supplies
the font-to-CSS transport. There are no fitted origins, baseline/CTM shifts,
changes to authored native spacing or surrounding page/paragraph positions.
All original fragments reverse byte-for-byte to the marked input. Captions,
styles, outside operators, font bytes and every case remain. The linked fraction
keeps its actual mfrac/mtext/span/a tree and href; only its numerator receives
the separately approved script artwork.

The script-artwork face selects default math-language ssty digits and plus from
unmodified STIX Two Math v2.13b171, source SHA256
49b22767985171bce579307b21d98f4d35012fa39e316acc4ffcab8644dfb69b.
The renamed 9,252-byte face is Math Fraction Script Reference, SHA256
b1eed6017e8ac0976744ddb108c995747bb19d49bfb24fc51ee472922e6e0217;
its token manifest is
fedc0dcf1b34aa794b7991c8fb2c906ae243489863a29b59d873eff5974e7214.
Outlines, instructions, advances, hints, MATH information and baseline metrics
are checked against the original. Plus retains U+002B and dictionary semantics.
The prior alias-only font is reproduced in memory from the source. The only
permitted changes are its explicit cmap and related coverage/checksum fields.
The full source OFL is `math-fraction-script-reference-LICENSE.txt`.
Native ActualText preserves original Unicode. Browser alias extraction is not
an exact Unicode or accessibility assertion.

From a clean checkout:

```sh
python3 -m venv tmp/math-reference-env
tmp/math-reference-env/bin/pip install fonttools==4.60.1
mkdir -p tmp/references
curl --fail --location --output tmp/references/mathml-core.html \
  https://www.w3.org/TR/2025/CR-mathml-core-20250624/
printf '%s  %s\n' f61ebea74ba54c489646e6969983ef977f448c26602edff75ec4e3ba77216373 \
  tmp/references/mathml-core.html | sha256sum --check
tmp/math-reference-env/bin/python scripts/generate-mathml-fraction-reference.py --check
tmp/math-reference-env/bin/python scripts/generate-mathml-fraction-core-reference.py --check
tmp/math-reference-env/bin/python scripts/verify-mathml-fraction-core-reference.py
```

All three scripts accept `--core-source /path/to/verified/mathml-core.html`.
Omit `--check` from generators to regenerate the exact approved products.
The dated Core snapshot must hash to
f61ebea74ba54c489646e6969983ef977f448c26602edff75ec4e3ba77216373;
no latest draft is substituted. Outputs require no ignored review files.
The complete reference's reviewed relative font URLs are retained byte-for-byte.
The parity transform expands them to verified absolute font URLs for temporary
Chromium input. For standalone inspection, copy the complete reference under
`.quality/orchestration/reference/`, where its relative paths resolve to the
checkout root.

At DPI 72 and channel tolerance 12, thresholds remain 1% changed pixels and
1% average delta. The adapted host preflight passed at 0.397569444% changed
pixels, average delta 0.000983924897. The direct ordinary fixture passed at
0.599537037%, average delta 0.002008033769. The original compact 1.8362268519%,
quoted-artwork 2.05150463%, and approved-cmap 1.9560185185% comparisons remain
recorded disagreements, not passes. Their original sources, PDFs, PPMs, metrics
and font hashes are preserved in lane-local ignored orchestration evidence.
Full native acceptance requires the unchanged supported-runtime quality gates.
