# Reproduce the browser-only glyph alias fixture with fontTools 4.60.1.
# The original bundled face remains unmodified. This fixture uses the same
# renamed subset bytes in Chromium and the native writer; only Chromium text
# uses PUA aliases to select forms it did not expose through ssty.
import fontTools
assert fontTools.__version__ == "4.60.1"
from fontTools.ttLib import TTFont
from fontTools import subset
from pathlib import Path
import hashlib,html
source=Path('priv/fonts/stix/STIXTwoMath-Regular.ttf');assert hashlib.sha256(source.read_bytes()).hexdigest()=='49b22767985171bce579307b21d98f4d35012fa39e316acc4ffcab8644dfb69b';original=TTFont(source,recalcTimestamp=False);font=TTFont(source,recalcTimestamp=False)
cmap=original.getBestCmap();table=original['GSUB'].table;script=next(r.Script for r in table.ScriptList.ScriptRecord if r.ScriptTag=='math');feature=next(table.FeatureList.FeatureRecord[i].Feature for i in script.DefaultLangSys.FeatureIndex if table.FeatureList.FeatureRecord[i].FeatureTag=='ssty');alts=table.LookupList.Lookup[feature.LookupListIndex[0]].SubTable[0].alternates
chars='𝑥𝑦𝑧';selected=[cmap[ord(c)] for c in chars];selected+= [alts[cmap[ord(c)]][i] for i in [0,1] for c in chars]
def make_reference(family, filename, keep_math, glyph_names=None):
    names = selected if glyph_names is None else glyph_names
    aliases = list(zip(range(0xE000, 0xE006), selected[3:])) if glyph_names is None else list(zip(range(0xE100, 0xE100 + len(names)), names))
    font = TTFont(source, recalcTimestamp=False)
    for table in font['cmap'].tables:
        if table.isUnicode():
            for cp, name in aliases:
                table.cmap[cp] = name
    for tag in ['GSUB', 'GPOS'] + ([] if keep_math else ['MATH']):
        del font[tag]
    options = subset.Options()
    options.recalc_bounds = False
    options.recalc_timestamp = False
    ss = subset.Subsetter(options=options)
    ss.populate(unicodes=([ord(c) for c in chars] if glyph_names is None else []) + [cp for cp, _ in aliases])
    ss.subset(font)
    for name in names:
        a = original['glyf'][name]
        b = font['glyf'][name]
        assert a.getCoordinates(original['glyf']) == b.getCoordinates(font['glyf'])
        assert a.program.getBytecode() == b.program.getBytecode()
        assert original['hmtx'].metrics[name] == font['hmtx'].metrics[name]
    for tag in ['fpgm', 'prep', 'cvt ']:
        assert original[tag].compile(original) == font[tag].compile(font)
    if keep_math:
        for field, coverage, values in [
            ('MathItalicsCorrectionInfo', 'Coverage', 'ItalicsCorrection'),
            ('MathTopAccentAttachment', 'TopAccentCoverage', 'TopAccentAttachment')]:
            original_info = getattr(original['MATH'].table.MathGlyphInfo, field)
            subset_info = getattr(font['MATH'].table.MathGlyphInfo, field)
            before = dict(zip(getattr(original_info, coverage).glyphs, getattr(original_info, values)))
            after = dict(zip(getattr(subset_info, coverage).glyphs, getattr(subset_info, values)))
            for name in selected:
                assert (name in before) == (name in after)
                if name in before:
                    assert before[name].Value == after[name].Value
                    assert before[name].DeviceTable == after[name].DeviceTable
    for nid in [1, 3, 4, 6, 16, 18]:
        for record in font['name'].names:
            if record.nameID == nid:
                value = family.replace(' ', '') if nid == 6 else family
                record.string = value.encode(record.getEncoding())
    path = asset / filename
    font.save(path)
    return path

asset = Path('test/fixtures/html_to_pdf/browser_parity/assets')
asset.mkdir(exist_ok=True)
p = make_reference('Math Glyph Test Reference', 'math-glyph-reference.ttf', False)
row_path = make_reference('Math Row Test Reference', 'math-row-reference.ttf', True)
license_source = Path('priv/fonts/stix/OFL.txt')
if not license_source.exists():
    license_source = next(Path('priv/fonts/stix').glob('*LICENSE*'))
(asset / 'math-glyph-reference-LICENSE.txt').write_bytes(license_source.read_bytes())
rows=[]
for level,text in [(0,chars*2),(1,''.join(chr(i) for i in range(0xE000,0xE003))*2),(2,''.join(chr(i) for i in range(0xE003,0xE006))*2)]:
 names=selected[level*3:level*3+3]*2;ids=' '.join(str(original.getGlyphID(n)) for n in names)
 rows.append(f'<span id="level{level}" data-level="{level}" data-text="{chars*2}" data-glyph-ids="{ids}">{text}</span>')
source_sha=hashlib.sha256(source.read_bytes()).hexdigest();reference_sha=hashlib.sha256(p.read_bytes()).hexdigest()
result='''<!doctype html>
<html><head><style>
@page { size: 240pt 132pt; margin: 0; }
body { margin: 0; }
span { position: absolute; left: 12pt; font-family: 'Math Glyph Test Reference'; font-size: 30pt; line-height: 30pt; }
#level0 { top: 12pt; }
#level1 { top: 48pt; }
#level2 { top: 84pt; }
</style></head><body>
<!-- Writer comparison only. Chromium ignored STIX ssty in text and MathML.
Equivalent supported markup uses PUA aliases in a subset reference font.
Source STIX Two Math v2.13b171 sha256 '''+source_sha+'''.
Reference fontTools 4.60.1 sha256 '''+reference_sha+'''.
All nine glyph outlines, instructions, advances, global hints, units, and ascent
are verified against the unmodified source. Native text retains original Unicode.
This fixture does not assert native HTML/MathML rendering compatibility. -->
'''+''.join(rows)+'''</body></html>
'''
Path('test/fixtures/html_to_pdf/browser_parity/font_glyph_variants.html').write_text(result)
print('reference bytes',p.stat().st_size,reference_sha)

math_rows = []
for level in range(3):
    text = chars * 2 if level == 0 else ''.join(chr(0xE000 + (level - 1) * 3 + i) for i in range(3)) * 2
    ids = ' '.join(str(original.getGlyphID(name)) for name in selected[level * 3:level * 3 + 3] * 2)
    tokens = ''.join(f'<mi>{character}</mi>' for character in text)
    math_rows.append(f'<math id="level{level}" data-level="{level}" data-text="{chars * 2}" data-glyph-ids="{ids}">{tokens}</math>')
row_sha = hashlib.sha256(row_path.read_bytes()).hexdigest()
math_html = f'''<!doctype html>
<html><head><style>
@page {{ size: 240pt 132pt; margin: 0; }}
body {{ margin: 0; }}
math {{ position: absolute; left: 12pt; font-family: 'Math Row Test Reference'; font-size: 30pt; }}
#level0 {{ top: 12pt; }}
#level1 {{ top: 48pt; }}
#level2 {{ top: 84pt; }}
</style></head><body>
<!-- Measurement comparison only, not native HTML/MathML rendering.
STIX Two Math source sha256 {source_sha}.
Equivalent MathML uses PUA aliases to select script forms Chromium did not expose.
Math Row Test Reference sha256 {row_sha}, fontTools 4.60.1.
Selected outlines, instructions, advances, hints, and MATH italic/accent records
are verified against original STIX. Native painting retains original Unicode. -->
{''.join(math_rows)}</body></html>
'''
Path('.quality').mkdir(exist_ok=True)
Path('.quality/math-glyph-rows-unadjusted.html').write_text(math_html)
print('row reference bytes', row_path.stat().st_size, row_sha)

# Independent vector-geometry reference. Chromium MathML token ink metrics are
# rasterized and differ from design-unit bounds, so CSS places each same-font
# glyph at the baseline and italic offsets specified by the outline data.
css_rows = []
units = original['head'].unitsPerEm
size = 30.0
css_ascent = round(original['hhea'].ascent * size / units / .75) * .75
italics_info = original['MATH'].table.MathGlyphInfo.MathItalicsCorrectionInfo
italics = dict(zip(italics_info.Coverage.glyphs, italics_info.ItalicsCorrection))
for level in range(3):
    names = selected[level * 3:level * 3 + 3] * 2
    codepoints = [ord(c) for c in chars] * 2 if level == 0 else list(range(0xE000 + (level - 1) * 3, 0xE003 + (level - 1) * 3)) * 2
    ids = ' '.join(str(original.getGlyphID(name)) for name in names)
    baseline = 12 + level * 36 + max(original['glyf'][name].yMax for name in names) * size / units
    offset, previous, tokens = 0, 0, []
    for name, unicode in zip(names, codepoints):
        correction = italics[name].Value * size / units if name in italics else 0
        assert name not in italics or italics[name].DeviceTable is None
        if correction == 0:
            offset += previous
        tokens.append(f'<span style="left:{12 + offset:.8f}pt;top:{baseline - css_ascent:.8f}pt">{chr(unicode)}</span>')
        offset += original['hmtx'].metrics[name][0] * size / units
        previous = correction
    css_rows.append(f'<div data-level="{level}" data-font-size="{size}" data-text="{chars * 2}" data-glyph-ids="{ids}">{"".join(tokens)}</div>')
css_html = f'''<!doctype html>
<html><head><style>
@page {{ size: 240pt 132pt; margin: 0; }}
body {{ margin: 0; }}
span {{ position: absolute; font-family: 'Math Row Test Reference'; font-size: {size}pt; line-height:{size}pt; }}
</style></head><body>
<!-- Outline-based glyph row measurement, not native HTML/MathML rendering.
STIX Two Math source sha256 {source_sha}.
Equivalent CSS uses independently calculated design-unit bounds and MATH italic spacing.
Math Row Test Reference sha256 {row_sha}, fontTools 4.60.1.
A preserved direct MathML comparison differs because Chromium uses raster ink bounds.
The native writer preserves each selected glyph's original Unicode. -->
{''.join(css_rows)}</body></html>
'''
Path('test/fixtures/html_to_pdf/browser_parity/math_glyph_rows.html').write_text(css_html)

# Independent OpenType assembly reference, in exact rational design units.
# Unused outside connectors are excluded, as OpenType and Chromium 152 require.
from fractions import Fraction
import math
stretch_cases = [('(', 'vertical', 130, 20, 180),
                 ('√', 'vertical', 130, 75, 180),
                 ('∫', 'vertical', 130, 125, 180),
                 ('(', 'vertical', 50, 180, 180),
                 ('(', 'vertical', 20, 215, 180),
                 ('⏞', 'horizontal', 200, 20, 225)]
size = Fraction(123, 4)
scale = size / units
variants = original['MATH'].table.MathVariants
stretch_rows, stretch_names = [], []
for character, axis, target, left, baseline in stretch_cases:
    name = cmap[ord(character)]
    direction = 'Vert' if axis == 'vertical' else 'Horiz'
    construction = getattr(variants, direction + 'GlyphConstruction')[getattr(variants, direction + 'GlyphCoverage').glyphs.index(name)]
    natural = original['hmtx'].metrics[name][0] if axis == 'horizontal' else original['glyf'][name].yMax - original['glyf'][name].yMin
    chosen = name if natural * scale >= target else next((record.VariantGlyph for record in construction.MathGlyphVariantRecord if record.AdvanceMeasurement * scale >= target), None)
    if chosen:
        pieces = [(chosen, Fraction(0), Fraction(0))]
    else:
        parts = construction.GlyphAssembly.PartRecords
        minimum = variants.MinConnectorOverlap
        extenders = [p for p in parts if p.PartFlags & 1]
        fixed = [p for p in parts if not p.PartFlags & 1]
        fixed_sum = sum(p.FullAdvance for p in fixed)
        extender_sum = sum(p.FullAdvance for p in extenders)
        maximum = min([p.StartConnectorLength for i, p in enumerate(parts) if i or p.PartFlags & 1] + [p.EndConnectorLength for i, p in enumerate(parts) if i < len(parts) - 1 or p.PartFlags & 1])
        assert maximum >= minimum and extender_sum > minimum * len(extenders)
        repeats = max(math.ceil((Fraction(target) / scale - fixed_sum + minimum * (len(fixed) - 1)) / (extender_sum - minimum * len(extenders))), 0 if fixed else 1)
        expanded = [p for p in parts for _ in range(repeats if p.PartFlags & 1 else 1)]
        total = sum(p.FullAdvance for p in expanded)
        overlap = 0 if len(expanded) <= 1 else max(minimum, min(maximum, (total - Fraction(target) / scale) / (len(expanded) - 1)))
        assert (total - overlap * (len(expanded) - 1)) * scale >= target
        offset, pieces = Fraction(0), []
        for part in expanded:
            x = offset * scale if axis == 'horizontal' else Fraction(0)
            y = (offset - original['glyf'][part.glyph].yMin) * scale if axis == 'vertical' else Fraction(0)
            pieces.append((part.glyph, x, y))
            offset += part.FullAdvance - overlap
    stretch_rows.append((character, axis, target, left, baseline, pieces))
    stretch_names.extend(name for name, _, _ in pieces)
stretch_names = list(dict.fromkeys(stretch_names))
stretch_path = make_reference('Math Stretch Test Reference', 'math-stretch-reference.ttf', False, stretch_names)
stretch_aliases = {name: 0xE100 + i for i, name in enumerate(stretch_names)}
css_ascent = round(original['hhea'].ascent * size / units / Fraction(3, 4)) * Fraction(3, 4)
html_rows = []
for character, axis, target, left, baseline, pieces in stretch_rows:
    ids = ' '.join(str(original.getGlyphID(name)) for name, _, _ in pieces)
    tokens = ''.join(f'<span style="left:{float(left + x):.8f}pt;top:{float(baseline - y - css_ascent):.8f}pt">{chr(stretch_aliases[name])}</span>' for name, x, y in pieces)
    html_rows.append(f'<div data-text="{html.escape(character)}" data-axis="{axis}" data-target="{target}" data-left="{left}" data-baseline="{baseline}" data-font-size="{float(size)}" data-glyph-ids="{ids}">{tokens}</div>')
stretch_sha = hashlib.sha256(stretch_path.read_bytes()).hexdigest()
Path('test/fixtures/html_to_pdf/browser_parity/math_stretch_glyphs.html').write_text(f'''<!doctype html>
<html><head><style>
@page {{ size: 264pt 264pt; margin: 0; }}
body {{ margin: 0; }}
span {{ position: absolute; font-family: 'Math Stretch Test Reference'; font-size: {float(size)}pt; line-height:{float(size)}pt; }}
</style></head><body>
<!-- Stretch geometry compared with equivalent positioned CSS, not HTML/MathML integration.
STIX Two Math source sha256 {source_sha}.
Exact rational reference calculations use OpenType MATH variants and assemblies.
Unused outside connectors are excluded. Every required join meets MinConnectorOverlap.
Selected outlines, instructions, advances and global hints match the unmodified source.
Reference fontTools 4.60.1 sha256 {stretch_sha}.
ActualText preserves one original Unicode character for each operator assembly. -->
{''.join(html_rows)}</body></html>
''')
print('stretch reference bytes', stretch_path.stat().st_size, stretch_sha)
