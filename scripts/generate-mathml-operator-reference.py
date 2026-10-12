#!/usr/bin/env python3
"""Independent, rational Core 3.2.4.3/5.3.2 tall-operator browser reference.

Requires fontTools 4.60.1. No native geometry, browser measurements or fitted
positions are inputs. --check reproduces the font, metrics and HTML fragments.
"""
from fractions import Fraction as F
from pathlib import Path
import hashlib
import json
import math
import sys
import tempfile

import fontTools
from fontTools import subset
from fontTools.ttLib import TTFont

assert fontTools.__version__ == '4.60.1'
root = Path(__file__).resolve().parent.parent
source = root / 'priv/fonts/stix/STIXTwoMath-Regular.ttf'
core = root / 'tmp/references/mathml-core.html'
font_sha = '49b22767985171bce579307b21d98f4d35012fa39e316acc4ffcab8644dfb69b'
core_sha = 'f61ebea74ba54c489646e6969983ef977f448c26602edff75ec4e3ba77216373'
assert hashlib.sha256(source.read_bytes()).hexdigest() == font_sha
assert hashlib.sha256(core.read_bytes()).hexdigest() == core_sha
font = TTFont(source, recalcTimestamp=False)
units = font['head'].unitsPerEm
cmap = font.getBestCmap()
variants = font['MATH'].table.MathVariants
axis_record = font['MATH'].table.MathConstants.AxisHeight
assert axis_record.DeviceTable is None
cases = [
    ('left16', '(', 16, 35, 12, True, F(1), None),
    ('right16', ')', 16, 35, 12, True, F(1), None),
    ('bracket16', '[', 16, 35, 12, False, F(1), None),
    ('min-bracket16', ']', 16, 35, 12, True, F(3, 2), F(3, 2)),
    ('left-brace12', '{', 12, 65, 25, True, F(1), None),
    ('right-brace12', '}', 12, 65, 25, True, F(1), None),
]
records = {}
names = []
for key, text, size, ua, ud, symmetric, minimum_ratio, maximum_ratio in cases:
    scale = F(size, units)
    axis = axis_record.Value * scale
    base = cmap[ord(text)]
    outline = font['glyf'][base]
    natural = (outline.yMax - outline.yMin) * scale
    a, d = F(ua), F(ud)
    if symmetric:
        half = max(a - axis, d + axis)
        a, d = half + axis, half - axis
    minimum = max(natural * minimum_ratio, 0)
    maximum = max(natural * maximum_ratio, minimum) if maximum_ratio is not None else None
    total = a + d
    target = minimum if total <= 0 or total < minimum else min(total, maximum) if maximum is not None else total
    a = target / 2 + axis if total <= 0 else max(0, (a - axis) * target / total + axis)
    d = target - a
    construction = variants.VertGlyphConstruction[variants.VertGlyphCoverage.glyphs.index(base)]
    chosen = base if natural >= target else next((v.VariantGlyph for v in construction.MathGlyphVariantRecord if v.AdvanceMeasurement * scale >= target), None)
    if chosen is not None:
        parts = [(chosen, F(0), F(0))]
        ink_a, ink_d = font['glyf'][chosen].yMax * scale, -font['glyf'][chosen].yMin * scale
        width = font['hmtx'].metrics[chosen][0] * scale
        overlap, repetitions = F(0), 0
        kind = 'glyph'
    else:
        assembly = construction.GlyphAssembly
        raw = assembly.PartRecords
        fixed = [p for p in raw if not p.PartFlags & 1]
        extenders = [p for p in raw if p.PartFlags & 1]
        fixed_sum = sum(p.FullAdvance for p in fixed)
        extender_sum = sum(p.FullAdvance for p in extenders)
        min_overlap = variants.MinConnectorOverlap
        max_overlap = min([p.StartConnectorLength for i, p in enumerate(raw) if i or p.PartFlags & 1] + [p.EndConnectorLength for i, p in enumerate(raw) if i < len(raw) - 1 or p.PartFlags & 1])
        assert max_overlap >= min_overlap
        growth = extender_sum - min_overlap * len(extenders)
        assert growth > 0
        repetitions = max(math.ceil(max(target / scale - fixed_sum + min_overlap * (len(fixed) - 1), 0) / growth), 0 if fixed else 1)
        expanded = [p for p in raw for _ in range(repetitions if p.PartFlags & 1 else 1)]
        total_advance = sum(p.FullAdvance for p in expanded)
        overlap = F(0) if len(expanded) <= 1 else max(min_overlap, min(max_overlap, (total_advance - target / scale) / (len(expanded) - 1)))
        ink_a = (total_advance - overlap * max(len(expanded) - 1, 0)) * scale
        ink_d = F(0)
        width = max(font['hmtx'].metrics[p.glyph][0] * scale for p in raw)
        parts, offset = [], F(0)
        for p in expanded:
            parts.append((p.glyph, F(0), (offset - font['glyf'][p.glyph].yMin) * scale))
            offset += p.FullAdvance - overlap
        kind = 'assembly'
    shift = ((a - d) - (ink_a - ink_d)) / 2
    ascent, descent = ink_a + shift, ink_d - shift
    parts = [(name, x, y + shift) for name, x, y in parts]
    names.extend(name for name, _, _ in parts)
    records[key] = {'text': text, 'font_size': size, 'kind': kind,
                    'target_ascent': float(a), 'target_descent': float(d),
                    'ascent': float(ascent), 'descent': float(descent),
                    'width': float(width), 'axis': float(axis),
                    'overlap_design_units': float(overlap), 'repetitions': repetitions,
                    'pieces': [{'glyph_id': font.getGlyphID(name), 'name': name,
                                'x': float(x), 'y': float(y)} for name, x, y in parts]}
names = list(dict.fromkeys(names))
family = 'Math Operator Core Reference'
aliases = {name: 0xE300 + i for i, name in enumerate(names)}
paint = TTFont(source, recalcTimestamp=False)
for table in paint['cmap'].tables:
    if table.isUnicode():
        table.cmap.update({cp: name for name, cp in aliases.items()})
for tag in ['GSUB', 'GPOS', 'MATH']:
    del paint[tag]
options = subset.Options()
options.recalc_bounds = False
options.recalc_timestamp = False
subsetter = subset.Subsetter(options=options)
subsetter.populate(unicodes=list(aliases.values()))
subsetter.subset(paint)
for name in names:
    a, b = font['glyf'][name], paint['glyf'][name]
    assert a.getCoordinates(font['glyf']) == b.getCoordinates(paint['glyf'])
    assert a.program.getBytecode() == b.program.getBytecode()
    assert font['hmtx'].metrics[name] == paint['hmtx'].metrics[name]
for tag in ['fpgm', 'prep', 'cvt ']:
    assert font[tag].compile(font) == paint[tag].compile(paint)
for name_id in [1, 3, 4, 6, 16, 18]:
    for record in paint['name'].names:
        if record.nameID == name_id:
            record.string = (family.replace(' ', '') if name_id == 6 else family).encode(record.getEncoding())
with tempfile.TemporaryDirectory() as temporary:
    out = Path(temporary) / 'reference.ttf'
    paint.save(out)
    font_bytes = out.read_bytes()
for key, record in records.items():
    size = record['font_size']
    css_ascent = round(F(font['hhea'].ascent * size, units) / F(3, 4)) * F(3, 4)
    spans = ''.join(f'<span style="position:absolute;left:{p["x"]:.8f}pt;top:{record["ascent"] - p["y"] - float(css_ascent):.8f}pt;font-family:\'{family}\';font-size:{size}pt;line-height:{size}pt">{chr(aliases[p["name"]])}</span>' for p in record['pieces'])
    record['html'] = f'<mtext data-core-reference="{key}" style="display:inline-block;position:relative;width:{record["width"]:.8f}pt;height:{record["ascent"]+record["descent"]:.8f}pt;line-height:0;font-size:0"><span style="display:inline-block;width:0;height:{record["ascent"]:.8f}pt;line-height:0;font-size:0"></span>{spans}</mtext>'
    for piece in record['pieces']:
        piece['alias'] = aliases[piece['name']]
output = root / 'test/fixtures/html_to_pdf/browser_parity/assets'
manifest = {'font_sha256': font_sha, 'core_sha256': core_sha,
            'reference_sha256': hashlib.sha256(font_bytes).hexdigest(), 'records': records}
for path, data in [(output / 'math-operator-core-reference.ttf', font_bytes),
                   (output / 'math-operator-core-reference.json', (json.dumps(manifest, indent=2, ensure_ascii=False)+'\n').encode()),
                   (output / 'math-operator-core-reference-LICENSE.txt', (root / 'priv/fonts/stix/OFL.txt').read_bytes())]:
    if '--check' in sys.argv:
        assert path.read_bytes() == data, f'{path.name} differs from the independently generated reference'
    else:
        path.write_bytes(data)
print('Pinned independent operator reference: six Core cases; outlines/instructions/advances/hints verified')
