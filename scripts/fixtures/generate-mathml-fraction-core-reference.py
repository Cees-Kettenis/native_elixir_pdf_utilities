"""Approved Core fraction/CSS reference. Inputs never include native/browser data.

Geometry is rational source math. Outer CSS inline-box transport is recorded
separately from fraction interior geometry. Original fragments are byte-reversible.
"""
import argparse
from fractions import Fraction as F
import hashlib
import html
from html.parser import HTMLParser
import json
from pathlib import Path
import re

import fontTools
from fontTools.ttLib import TTFont

ROOT = Path(__file__).resolve().parents[2]
OUT = ROOT / 'test/fixtures/html_to_pdf/browser_parity/assets'
SOURCE = ROOT / 'priv/fonts/stix/STIXTwoMath-Regular.ttf'
arguments = argparse.ArgumentParser()
arguments.add_argument('--check', action='store_true')
arguments.add_argument('--core-source', type=Path, default=ROOT / 'tmp/references/mathml-core.html')
args = arguments.parse_args()
CORE = args.core_source
FIXTURE = ROOT / 'test/fixtures/html_to_pdf/browser_parity/mathml_fractions.html'
ASSETS = ROOT / 'test/fixtures/html_to_pdf/browser_parity/assets'
PINNED = {
    SOURCE: '49b22767985171bce579307b21d98f4d35012fa39e316acc4ffcab8644dfb69b',
    CORE: 'f61ebea74ba54c489646e6969983ef977f448c26602edff75ec4e3ba77216373',
    FIXTURE: 'f661e98a620b999f1c1cc0b8e99c4d948c487b43f6d75e233cea365a8b8603ea',
    ASSETS / 'math-fraction-script-reference.ttf': 'b1eed6017e8ac0976744ddb108c995747bb19d49bfb24fc51ee472922e6e0217',
    ASSETS / 'math-fraction-script-reference.json': 'fedc0dcf1b34aa794b7991c8fb2c906ae243489863a29b59d873eff5974e7214',
}
assert fontTools.__version__ == '4.60.1'
for path, expected in PINNED.items():
    assert hashlib.sha256(path.read_bytes()).hexdigest() == expected
source_html = FIXTURE.read_text()
core_html = CORE.read_text()
font = TTFont(SOURCE, recalcTimestamp=False)
units = font['head'].unitsPerEm
constants = font['MATH'].table.MathConstants
cmap = font.getBestCmap()
gsub = font['GSUB'].table
script = next(r.Script for r in gsub.ScriptList.ScriptRecord if r.ScriptTag == 'math')
feature = next(gsub.FeatureList.FeatureRecord[i].Feature for i in script.DefaultLangSys.FeatureIndex
               if gsub.FeatureList.FeatureRecord[i].FeatureTag == 'ssty')
ssty = gsub.LookupList.Lookup[feature.LookupListIndex[0]].SubTable[0].alternates
alias_face = TTFont(ASSETS / 'math-fraction-script-reference.ttf', recalcTimestamp=False)
alias_cmap = alias_face.getBestCmap()
# Mapping by original outline identity, independent of any native glyph outputs.
aliases = {}
for level in (1, 2):
    for index, character in enumerate('0123456789+'):
        name = ssty[cmap[ord(character)]][level - 1]
        cp = 0xE300 + (level - 1) * 11 + index
        assert alias_face['glyf'][alias_cmap[cp]].getCoordinates(alias_face['glyf']) == font['glyf'][name].getCoordinates(font['glyf'])
        assert alias_face['hmtx'][alias_cmap[cp]] == font['hmtx'][name]
        aliases[name] = cp


pair_proofs = []


def source_pair_advance(left, right, script_tag):
    """Independent selected-font default language-system kern advance."""
    gpos = font['GPOS'].table
    scripts = {record.ScriptTag: record.Script for record in gpos.ScriptList.ScriptRecord}
    script = scripts.get(script_tag, scripts.get('DFLT'))
    if script is None or script.DefaultLangSys is None:
        return 0
    advance = 0
    for index in script.DefaultLangSys.FeatureIndex:
        feature = gpos.FeatureList.FeatureRecord[index]
        if feature.FeatureTag != 'kern':
            continue
        for lookup_index in feature.Feature.LookupListIndex:
            lookup = gpos.LookupList.Lookup[lookup_index]
            # STIX kern lookups ignore marks. These original selected digits
            # are base glyphs; no filtering changes this pair's participation.
            assert lookup.LookupFlag in [0, 8]
            assert all(font['GDEF'].table.GlyphClassDef.classDefs.get(g, 0) != 3 for g in [left, right])
            for table in lookup.SubTable:
                kind = lookup.LookupType
                if kind == 9:
                    kind, table = table.ExtensionLookupType, table.ExtSubTable
                assert kind == 2
                if left not in table.Coverage.glyphs:
                    continue
                record = None
                if table.Format == 1:
                    pairs = table.PairSet[table.Coverage.glyphs.index(left)].PairValueRecord
                    record = next((r for r in pairs if r.SecondGlyph == right), None)
                else:
                    assert table.Format == 2
                    a = table.ClassDef1.classDefs.get(left, 0)
                    b = table.ClassDef2.classDefs.get(right, 0)
                    record = table.Class1Record[a].Class2Record[b]
                if record is not None:
                    value = record.Value1
                    assert all(getattr(value, field, 0) == 0 for field in ['XPlacement', 'YPlacement', 'YAdvance'])
                    advance += getattr(value, 'XAdvance', 0)
                    break
    pair_proofs.append(dict(left=font.getGlyphID(left), right=font.getGlyphID(right), script=script_tag, design_advance=advance))
    return advance


def source_rows(identity):
    figure = re.search('<figure id="' + identity + '">(.*?)</figure>', core_html, re.S).group(1)
    return [[html.unescape(re.sub('<[^>]*>', '', cell)).strip()
             for cell in re.findall('<td>(.*?)</td>', row, re.S)]
            for row in re.findall('<tr>(.*?)</tr>', figure, re.S) if '<td>' in row]


categories = {r[0]: (F(r[3].removesuffix("em")).limit_denominator(18), F(r[4].removesuffix("em")).limit_denominator(18))
              for r in source_rows('operator-dictionary-categories-values')}
entries = {}
for description, category in source_rows('operator-dictionary-category-table'):
    form = re.search('in (infix|prefix|postfix) form', description).group(1)
    for first, last in re.findall(r'U\+([0-9A-F]+)(?:–U\+([0-9A-F]+))?', description):
        for cp in range(int(first, 16), int(last or first, 16) + 1):
            entries[(cp, form)] = category
assert len(entries) == 725


class Tree(HTMLParser):
    def __init__(self):
        super().__init__(convert_charrefs=True)
        self.stack = []
        self.fractions = []
        self.lines = [0]
        for match in re.finditer('\n', source_html):
            self.lines.append(match.end())

    def source_offset(self):
        line, column = self.getpos()
        return self.lines[line - 1] + column

    def handle_starttag(self, tag, attrs):
        if tag in ['meta', 'link', 'br', 'img', 'hr', 'input']:
            return
        node = dict(tag=tag, attrs=dict(attrs), children=[], text='', start=self.source_offset(),
                    opening=self.get_starttag_text())
        if self.stack:
            self.stack[-1]['children'].append(node)
            node['parent'] = self.stack[-1]
        self.stack.append(node)

    def handle_data(self, text):
        if self.stack:
            self.stack[-1]['text'] += text

    def handle_endtag(self, tag):
        node = self.stack.pop()
        assert node['tag'] == tag
        node['end'] = self.source_offset() + len('</' + tag + '>')
        if tag == 'mfrac':
            self.fractions.append(node)


def declarations(node):
    return dict(part.strip().lower().split(':', 1) for part in node['attrs'].get('style', '').split(';') if ':' in part)


def scaling(depth):
    assert 0 <= depth <= 2
    return [F(1), F(constants.ScriptPercentScaleDown, 100), F(constants.ScriptScriptPercentScaleDown, 100)][depth]


def length(value, size, default=None):
    match = re.fullmatch(r'([+-]?(?:[0-9]+(?:\.[0-9]*)?|\.[0-9]+))(pt|px|em|%)?', value.strip())
    if not match:
        assert default is not None
        return default
    number, unit = F(match[1]), match[2] or ''
    if unit == '%':
        assert default is not None
        return number * default / 100
    return number * {'': F(1), 'pt': F(1), 'px': F(3, 4), 'em': size}[unit]


def state(node, parent=None):
    if parent is None:
        if 'parent' in node:
            parent = state(node['parent'])
        else:
            parent = dict(size=F(12), depth=0, mode='normal', color='black', tag='')
    size, depth, mode, color = (parent[k] for k in ['size', 'depth', 'mode', 'color'])
    if node['tag'] == 'math':
        depth = 0
        mode = 'normal' if node['attrs'].get('display') == 'block' else 'compact'
    elif parent['tag'] == 'mfrac':
        depth += int(parent['mode'] == 'compact')
        mode = 'compact'
        size *= scaling(depth) / scaling(parent['depth'])
    if node['attrs'].get('displaystyle') in ['true', 'false']:
        mode = 'normal' if node['attrs']['displaystyle'] == 'true' else 'compact'
    css = declarations(node)
    if 'math-depth' in css:
        depth = int(css['math-depth'])
        size = parent['size'] * scaling(depth) / scaling(parent['depth'])
    if 'font-size' in css:
        size = length(css['font-size'], parent['size'])
    if 'math-style' in css:
        mode = css['math-style']
    color = css.get('color', node['attrs'].get('mathcolor', color))
    assert mode in ['normal', 'compact'] and color in ['black', 'red', 'blue']
    return dict(size=size, depth=depth, mode=mode, color=color, tag=node['tag'])


def value(name, size):
    record = getattr(constants, name)
    assert record.DeviceTable is None
    return F(record.Value * size, units)


def move(box, x, y):
    return {key: [{**item, 'x': item['x'] + x, 'y': item['y'] + y} for item in box[key]] for key in ['glyphs', 'rules']}


def measure(node, inherited=None):
    st = state(node, inherited)
    tag, size = node['tag'], st['size']
    if tag in ['mn', 'mo']:
        assert not node['children'] and node['text']
        glyphs = []
        x = F(0)
        previous = None
        for character in node['text']:
            name = cmap[ord(character)]
            if st['depth'] and name in ssty:
                name = ssty[name][min(st['depth'], 2) - 1]
            if previous is not None:
                x += F(source_pair_advance(previous, name, 'latn' if tag == 'mn' else 'math') * size, units)
            previous = name
            outline = font['glyf'][name]
            width = F(font['hmtx'][name][0] * size, units)
            glyphs.append(dict(text=character, name=name, gid=font.getGlyphID(name), size=size, x=x, y=F(0), width=width,
                               ascent=F(outline.yMax * size, units), descent=-F(outline.yMin * size, units), color=st['color']))
            x += width
        return dict(width=x, ascent=max(g['ascent'] for g in glyphs), descent=max(g['descent'] for g in glyphs), glyphs=glyphs, rules=[])
    assert tag in ['math', 'mrow', 'mfrac'] and not node['text'].strip()
    children = [measure(child, st) for child in node['children']]
    if tag == 'mfrac':
        assert len(children) == 2
        numerator, denominator = children
        normal = st['mode'] == 'normal'
        axis = value('AxisHeight', size)
        thickness = max(length(node['attrs'].get('linethickness', 'invalid'), size, value('FractionRuleThickness', size)), F(0))
        if thickness:
            up = max(value('FractionNumeratorDisplayStyleShiftUp' if normal else 'FractionNumeratorShiftUp', size),
                     axis + thickness / 2 + value('FractionNumDisplayStyleGapMin' if normal else 'FractionNumeratorGapMin', size) + numerator['descent'])
            down = max(value('FractionDenominatorDisplayStyleShiftDown' if normal else 'FractionDenominatorShiftDown', size),
                       thickness / 2 + value('FractionDenomDisplayStyleGapMin' if normal else 'FractionDenominatorGapMin', size) + denominator['ascent'] - axis)
        else:
            up = value('StackTopDisplayStyleShiftUp' if normal else 'StackTopShiftUp', size)
            down = value('StackBottomDisplayStyleShiftDown' if normal else 'StackBottomShiftDown', size)
            gap = value('StackDisplayStyleGapMin' if normal else 'StackGapMin', size)
            deficit = max(gap - (up - numerator['descent'] + down - denominator['ascent']), F(0))
            up += deficit / 2
            down += deficit / 2
        natural_width = max(c['width'] for c in children)
        css = declarations(node)
        width = length(css['width'], size) if 'width' in css else natural_width
        padding = length(css.get('padding', '1px'), size)
        ascent = max(up + numerator['ascent'], -down + denominator['ascent'])
        descent = max(-up + numerator['descent'], down + denominator['descent'], F(0))
        a = move(numerator, padding + (width - numerator['width']) / 2, up)
        b = move(denominator, padding + (width - denominator['width']) / 2, -down)
        rules = a['rules'] + b['rules']
        if thickness:
            ascent = max(ascent, axis + thickness / 2)
            descent = max(descent, thickness / 2 - axis)
            rules.append(dict(x=padding, y=axis-thickness/2, width=width, height=thickness, color=st['color']))
        return dict(width=width+2*padding, content_width=width, padding=padding, ascent=ascent, descent=descent,
                    glyphs=a['glyphs']+b['glyphs'], rules=rules, algorithm=dict(up=up, down=down, axis=axis, rule_thickness=thickness))
    x = F(0)
    glyphs, rules = [], []
    for index, (child, measured) in enumerate(zip(node['children'], children)):
        left = right = F(0)
        if child['tag'] == 'mo':
            form = 'prefix' if index == 0 and len(children) > 1 else 'postfix' if index == len(children)-1 and len(children)>1 else 'infix'
            category = entries.get((ord(child['text']), child['attrs'].get('form', form)), 'Default')
            left, right = [v * size for v in categories[category]]
        translated = move(measured, x+left, 0)
        glyphs += translated['glyphs']; rules += translated['rules']
        x += measured['width']+left+right
    return dict(width=x, ascent=max(c['ascent'] for c in children), descent=max(c['descent'] for c in children), glyphs=glyphs, rules=rules)


def pure(node):
    return node['tag'] in ['mfrac', 'mrow', 'mn', 'mo'] and all(pure(child) for child in node['children'])


def number(value):
    return f'{float(value):.12f}'.rstrip('0').rstrip('.')


def serialize(value):
    if isinstance(value, F):
        return dict(numerator=value.numerator, denominator=value.denominator, points=float(value))
    if isinstance(value, dict):
        return {key: serialize(item) for key, item in value.items()}
    if isinstance(value, list):
        return [serialize(item) for item in value]
    return value


parser = Tree(); parser.feed(source_html)
assert len(parser.fractions) == 10
records = {}
replacements = []
for node in sorted(parser.fractions, key=lambda n: n['start']):
    if node['parent']['tag'] == 'mfrac' or not pure(node):
        continue
    key = f'fraction-{len(records)+1:02}'
    measured = measure(node)
    ascent, descent = measured['ascent'], measured['descent']
    # This baseline witness belongs ONLY to the outer CSS inline-box transport.
    # Its bottom-edge baseline is the independently calculated content ascent.
    witness = f'<span data-core-baseline="{key}" style="display:inline-block;width:0;height:{number(ascent)}pt;line-height:0;font-size:0"></span>'
    painted = []
    for glyph in measured['glyphs']:
        name = glyph['name']
        family = 'Math Fraction Script Reference' if name in aliases else 'STIX Two Math'
        text = chr(aliases[name]) if name in aliases else glyph['text']
        # CSS point conversion plus rounded font ascent, as in the already
        # reviewed operator reference. This does not move the outer box baseline.
        css_ascent = round(F(font['hhea'].ascent * glyph['size'], units) / F(3,4)) * F(3,4)
        painted.append(f'<span data-core-text="{html.escape(glyph["text"], quote=True)}" style="position:absolute;left:{number(glyph["x"]-measured["padding"])}pt;top:{number(ascent-glyph["y"]-css_ascent)}pt;font-family:\'{family}\';font-size:{number(glyph["size"])}pt;line-height:{number(glyph["size"])}pt;color:{glyph["color"]};white-space:nowrap">{html.escape(text)}</span>')
        glyph['css_transport_ascent'] = css_ascent
    for rule in measured['rules']:
        painted.append(f'<span data-core-rule="{key}" style="position:absolute;left:{number(rule["x"]-measured["padding"])}pt;top:{number(ascent-rule["y"]-rule["height"])}pt;width:{number(rule["width"])}pt;height:{number(rule["height"])}pt;background:{rule["color"]}"></span>')
    transport = f'display:inline-block;width:{number(measured["content_width"])}pt;height:{number(ascent+descent)}pt;line-height:0;font-size:0'
    opening = node['opening']
    if re.search(r'\bstyle=["\']', opening):
        opening = re.sub(r'\bstyle=(["\'])(.*?)\1', lambda m: f'style={m[1]}{m[2]};{transport}{m[1]}', opening)
    else:
        opening = opening[:-1] + f' style="{transport}">'
    opening = opening[:-1] + f' data-core-fraction-reference="{key}">'
    # mtext is a real HTML integration point. An HTML span supplies the CSS
    # containing block; spans directly under mfrac would remain foreign nodes.
    host = (f'<mtext data-core-html-host="{key}" style="display:inline-block;line-height:0;font-size:0">'
            f'<span data-core-content-box="{key}" style="display:inline-block;position:relative;width:{number(measured["content_width"])}pt;height:{number(ascent+descent)}pt;line-height:0;font-size:0">'
            + witness + ''.join(painted) + '</span></mtext>')
    fragment = opening + host + '</mfrac>' 
    original = source_html[node['start']:node['end']]
    records[key] = dict(original_fragment=original, original_sha256=hashlib.sha256(original.encode()).hexdigest(),
                        original_text=''.join(g['text'] for g in measured['glyphs']),
                        original_opening=node['opening'], original_attributes=node['attrs'],
                        interior=serialize(dict(glyphs=measured['glyphs'], rules=measured['rules'], algorithm=measured['algorithm'])),
                        outer_css_transport=serialize(dict(content_width=measured['content_width'], padding=measured['padding'],
                                                           ascent=ascent, descent=descent, height=ascent+descent, baseline_witness_height=ascent,
                                                           html_content_origin_x=measured["padding"], css_containing_block="HTML span inside mtext integration point")),
                        reference_fragment=fragment)
    replacements.append((node['start'], node['end'], fragment))
assert len(records) == 8
reference = source_html
for start, end, fragment in reversed(replacements):
    reference = reference[:start]+fragment+reference[end:]
# Byte-exact reversal proves authored boundaries, attributes, text and source order.
restored = reference
for record in records.values():
    assert restored.count(record['reference_fragment']) == 1
    restored = restored.replace(record['reference_fragment'], record['original_fragment'], 1)
assert restored.encode() == FIXTURE.read_bytes()
linked = '<mfrac><mn data-script-reference="linked-n">11</mn><mtext><span><a href="https://fraction.test/">Link</a></span></mtext></mfrac>'
assert linked in reference and source_html.count(linked) == reference.count(linked) == 1
assert re.findall(r'<style[^>]*>.*?</style>', reference, re.S) == re.findall(r'<style[^>]*>.*?</style>', source_html, re.S)
assert re.findall(r'<a\b[^>]*>.*?</a>', reference, re.S) == re.findall(r'<a\b[^>]*>.*?</a>', source_html, re.S)
assert reference.count('data-script-reference="linked-n"') == 1
assert 'script' not in reference.lower().replace('data-script-reference', '').replace('math fraction script reference', '')
manifest = dict(activation=False, inputs={('tmp/references/mathml-core.html' if path == CORE else str(path.relative_to(ROOT))):sha for path,sha in PINNED.items()},
                source_url='https://www.w3.org/TR/2025/CR-mathml-core-20250624/', fonttools_version=fontTools.__version__,
                algorithm='Core 3.3.1 / 3.3.2; UA depth/style; B.1 dictionary; original selected outlines/MATH/GSUB',
                outer_transport_separate=True, empirical_offsets=[], native_or_browser_geometry_inputs=False,
                original_input_reproduced_byte_exactly=True, unchanged_linked_fraction=linked,
                unchanged_stylesheet_sha256=hashlib.sha256(''.join(re.findall(r'<style[^>]*>.*?</style>',source_html,re.S)).encode()).hexdigest(),
                original_mfrac_count=10, replaced_top_fractions=8, retained_linked_fraction_count=1,
                selected_source_pair_advances=pair_proofs,
                original_source_and_native_path_unchanged=True, unchanged_thresholds=dict(max_changed_ratio=.01,max_average_delta=.01,pixel_channel_tolerance=12,dpi=72),
                records=records)
# A complete read-only browser document uses the same five source faces and
# the already approved script-artwork face. Relative asset paths reproduce in a
# clean checkout without embedding this lane's filesystem identity.
faces = [
    ('DejaVu Sans', 'priv/fonts/dejavu/DejaVuSans.ttf', 400, 'normal'),
    ('DejaVu Sans', 'priv/fonts/dejavu/DejaVuSans-Bold.ttf', 700, 'normal'),
    ('DejaVu Sans', 'priv/fonts/dejavu/DejaVuSans-Oblique.ttf', 400, 'italic'),
    ('DejaVu Sans', 'priv/fonts/dejavu/DejaVuSans-BoldOblique.ttf', 700, 'italic'),
    ('STIX Two Math', 'priv/fonts/stix/STIXTwoMath-Regular.ttf', 400, 'normal'),
    ('Math Fraction Script Reference', 'test/fixtures/html_to_pdf/browser_parity/assets/math-fraction-script-reference.ttf', 400, 'normal'),
]
css = '<style data-core-reference-fonts="source-pinned">' + ''.join(
    f"@font-face{{font-family:'{family}';src:url('../../../{path}') format('truetype');font-weight:{weight};font-style:{style}}}"
    for family,path,weight,style in faces) + '</style>'
# Only the already approved artwork transformation applies to the untouched
# linked fraction. Its original math/HTML tree, href and computed size remain.
linked_node = next(node for node in parser.fractions if not pure(node))['children'][0]
st = state(linked_node)
assert st['depth'] == 1 and st['size'] == F(42,5) and linked_node['text'] == '11'
linked_original = source_html[linked_node['start']:linked_node['end']]
linked_alias_text = ''.join(chr(aliases[ssty[cmap[ord(c)]][st['depth']-1]]) for c in linked_node['text'])
linked_artwork = linked_node['opening'][:-1] + ' style="font-family:&quot;Math Fraction Script Reference&quot;">' + linked_alias_text + '</mn>'
ready = reference.replace(linked_original, linked_artwork, 1).replace('</head>', css + '</head>', 1)
reverse_ready = ready.replace(css, '', 1).replace(linked_artwork, linked_original, 1)
for record in records.values():
    reverse_ready = reverse_ready.replace(record['reference_fragment'], record['original_fragment'], 1)
assert reverse_ready.encode() == FIXTURE.read_bytes()
manifest['browser_packaging'] = dict(font_stylesheet=css,
    faces=[dict(family=family,path=path,weight=weight,style=style,sha256=hashlib.sha256((ROOT/path).read_bytes()).hexdigest()) for family,path,weight,style in faces],
    artwork_only_original=linked_original,artwork_only_reference=linked_artwork,
    no_layout_change_to_linked_tree=True, complete_reference_byte_reversible=True)
products={OUT/'math-fraction-core-reference-fragments.html':reference.encode(), OUT/'math-fraction-core-reference.html':ready.encode(), OUT/'math-fraction-core-reference.json':(json.dumps(manifest,indent=2,ensure_ascii=True)+'\n').encode()}
approved_hashes = {
    'math-fraction-core-reference-fragments.html': 'af4d0ed9659cdab757f92d6c16b69b9b4f9c85a48fa270223f4fb795ebf65cef',
    'math-fraction-core-reference.html': '3356f56ab4bc49b93c56fe25e2d04caea2aca84e5b2d7d4b376bfa890381a8b5',
    'math-fraction-core-reference.json': '0201c16d80858e1700a0ca492451d3e179a4a68ac669f98ae1d172cc0755dcc2',
}
for path,content in products.items():
    assert hashlib.sha256(content).hexdigest() == approved_hashes[path.name]
    if args.check: assert path.read_bytes()==content
    else:path.write_bytes(content)
print('Verified eight source-derived fraction interiors; byte-exact input reversal; linked actual MathML retained')
