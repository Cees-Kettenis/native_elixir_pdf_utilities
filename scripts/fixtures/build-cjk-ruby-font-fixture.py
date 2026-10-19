#!/usr/bin/env python3
"""Subset genuine Noto Sans JP outlines at weight 400 for horizontal CJK ruby tests.

Requires fonttools 4.66.1. Input is the unmodified upstream variable TTF.
The manifest records the pinned source identity and preserved glyph metrics/outlines.
"""
import argparse
import hashlib
import json
from pathlib import Path
import fontTools
from fontTools import subset
from fontTools.pens.recordingPen import DecomposingRecordingPen
from fontTools.ttLib import TTFont
from fontTools.varLib.instancer import instantiateVariableFont

parser = argparse.ArgumentParser(description=__doc__)
parser.add_argument('source', type=Path)
parser.add_argument('--revision', required=True)
args = parser.parse_args()
assert fontTools.__version__ == "4.66.1", "rebuild requires pinned fonttools 4.66.1"
root = Path(__file__).resolve().parents[2]
assets = root / 'test/fixtures/html_to_pdf/browser_parity/assets'
text = ''.join(chr(c) for c in range(32, 127)) + '漢字東京京都大阪日本語中文ひらがなとうきょうおさかにほんごかんじ。、（）「」仮名'
source = TTFont(args.source, recalcTimestamp=False)
font = instantiateVariableFont(source, {'wght': 400}, inplace=False)
font.recalcTimestamp = False
static_source = args.source.with_name('NotoSansJP-Regular-instance.ttf')
font.save(static_source)
font = TTFont(static_source, recalcTimestamp=False)
cmap = font.getBestCmap()
metrics = {}
for char in sorted(set(text)):
    glyph = cmap[ord(char)]
    pen = DecomposingRecordingPen(font.getGlyphSet())
    font.getGlyphSet()[glyph].draw(pen)
    metrics[f'U+{ord(char):04X}'] = {
        'advance': font['hmtx'][glyph][0],
        'left_side_bearing': font['hmtx'][glyph][1],
        'outline_sha256': hashlib.sha256(repr(pen.value).encode()).hexdigest(),
        'outline_commands': len(pen.value),
    }
options = subset.Options()
options.name_IDs = ['*']
subsetter = subset.Subsetter(options=options)
subsetter.populate(text=text)
subsetter.subset(font)
# OFL modified fonts use a distinct family, retaining copyright/license records.
for record in font['name'].names:
    names = {1: 'CJK Ruby Fixture', 2: 'Regular', 3: 'CJK Ruby Fixture Regular',
             4: 'CJK Ruby Fixture Regular', 6: 'CJKRubyFixture-Regular',
             16: 'CJK Ruby Fixture', 17: 'Regular'}
    if record.nameID in names:
        record.string = names[record.nameID].encode(record.getEncoding())
font.save(assets / 'cjk-ruby-fixture.ttf')
output = TTFont(assets / 'cjk-ruby-fixture.ttf')
for codepoint, expected in metrics.items():
    glyph = output.getBestCmap()[int(codepoint[2:], 16)]
    pen = DecomposingRecordingPen(output.getGlyphSet())
    output.getGlyphSet()[glyph].draw(pen)
    assert output['hmtx'][glyph] == (expected['advance'], expected['left_side_bearing'])
    assert hashlib.sha256(repr(pen.value).encode()).hexdigest() == expected['outline_sha256']
manifest = {
    'family': 'CJK Ruby Fixture',
    'source_repository': 'https://github.com/google/fonts',
    'source_revision': args.revision,
    'source_path': 'ofl/notosansjp/NotoSansJP[wght].ttf',
    'source_sha256': hashlib.sha256(args.source.read_bytes()).hexdigest(),
    'static_instance_sha256': hashlib.sha256(static_source.read_bytes()).hexdigest(),
    'fixture_sha256': hashlib.sha256((assets / 'cjk-ruby-fixture.ttf').read_bytes()).hexdigest(),
    'license': 'OFL-1.1', 'fonttools_version': '4.66.1', 'instance': {'wght': 400},
    'units_per_em': output['head'].unitsPerEm, 'glyphs': metrics,
}
(assets / 'cjk-ruby-fixture.json').write_text(json.dumps(manifest, indent=2) + '\n')
