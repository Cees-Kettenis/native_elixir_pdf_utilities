#!/usr/bin/env python3
"""Build the real water-glyph source metrics fixture from pinned Noto Sans JP.

Requires fonttools 4.66.1. Run from the repository root with the authenticated
upstream TTF path. The output manifest records source and preserved outlines.
"""
import argparse
import hashlib,json
from pathlib import Path
import fontTools
from fontTools import subset
from fontTools.ttLib import TTFont
from fontTools.varLib.instancer import instantiateVariableFont
from fontTools.pens.recordingPen import DecomposingRecordingPen
parser = argparse.ArgumentParser(description=__doc__)
parser.add_argument('source', type=Path)
source = parser.parse_args().source
assert fontTools.__version__ == '4.66.1'
assert hashlib.sha256(source.read_bytes()).hexdigest()=='c2f3b4d463500a2ddcd3849cded1fceeb9fd6d1c32e6cbecd568453ba50fc68f'
font=instantiateVariableFont(TTFont(source,recalcTimestamp=False),{'wght':400},inplace=False)
font.recalcTimestamp=False
static_source=source.with_name('NotoSansJP-Regular-instance.ttf')
font.save(static_source)
font=TTFont(static_source,recalcTimestamp=False)
metrics={}
for cp in map(ord,'H0x水'):
 name=font.getBestCmap()[cp];pen=DecomposingRecordingPen(font.getGlyphSet());font.getGlyphSet()[name].draw(pen)
 metrics[f'U+{cp:04X}']={'advance':font['hmtx'][name][0],'left_side_bearing':font['hmtx'][name][1],'outline_sha256':hashlib.sha256(repr(pen.value).encode()).hexdigest()}
options=subset.Options();options.name_IDs=['*']
subsetter=subset.Subsetter(options=options);subsetter.populate(text='H0x水');subsetter.subset(font)
# OFL derivative uses its own family while retaining genuine source outlines.
family='CSS Source Metrics Fixture'
names={1:family,2:'Regular',3:family+' Regular',4:family+' Regular',6:'CSSSourceMetricsFixture-Regular',16:family,17:'Regular'}
for record in font['name'].names:
 if record.nameID in names:record.string=names[record.nameID].encode(record.getEncoding())
out=Path('test/fixtures/fonts/source-metrics-water.ttf');font.save(out)
result=TTFont(out,recalcTimestamp=False)
for key,m in metrics.items():
 glyph=result.getBestCmap()[int(key[2:],16)];pen=DecomposingRecordingPen(result.getGlyphSet());result.getGlyphSet()[glyph].draw(pen)
 assert result['hmtx'][glyph]==(m['advance'],m['left_side_bearing'])
 assert hashlib.sha256(repr(pen.value).encode()).hexdigest()==m['outline_sha256']
metadata={'family':family,'source_url':'https://raw.githubusercontent.com/google/fonts/66928797f271fed5ee428544e7229685e2d90127/ofl/notosansjp/NotoSansJP%5Bwght%5D.ttf','source_revision':'66928797f271fed5ee428544e7229685e2d90127','source_sha256':hashlib.sha256(source.read_bytes()).hexdigest(),'static_instance_sha256':hashlib.sha256(static_source.read_bytes()).hexdigest(),'fixture_sha256':hashlib.sha256(out.read_bytes()).hexdigest(),'fonttools_version':fontTools.__version__,'weight':400,'units_per_em':result['head'].unitsPerEm,'glyphs':metrics,'license':'OFL-1.1','license_file':'source-metrics-water-LICENSE.txt','rebuild_script':'scripts/fixtures/build-source-metrics-water-fixture.py'}
out.with_suffix('.json').write_text(json.dumps(metadata,indent=2)+'\n')
Path('test/fixtures/fonts/source-metrics-water-LICENSE.txt').write_bytes(Path('test/fixtures/html_to_pdf/browser_parity/assets/cjk-ruby-fixture-LICENSE.txt').read_bytes())
print(json.dumps(metadata,indent=2))
