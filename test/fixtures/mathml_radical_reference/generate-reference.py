"""Unactivated M4 proposal, derived only from pinned Core, font and source HTML.

It never reads native code, beams, boxes, PDFs, browser outputs or pixel results.
Four pure radical subtrees get reversible CSS transport. The linked tree stays
byte-identical. Explicit outer display overrides are NEW requested approval.
"""
import argparse
import base64
from fractions import Fraction as F
import hashlib
import html
from html.parser import HTMLParser
import io
import json
from pathlib import Path
import re

import fontTools
from fontTools import subset
from fontTools.ttLib import TTFont

ROOT = Path(__file__).resolve().parents[3]
parser = argparse.ArgumentParser()
parser.add_argument('--core-source', type=Path, default=ROOT/'tmp/references/mathml-core.html')
parser.add_argument('--out-dir', type=Path, default=Path(__file__).resolve().parent)
parser.add_argument('--check', action='store_true')
args = parser.parse_args()
OUT = args.out_dir
pins = {
    'priv/fonts/stix/STIXTwoMath-Regular.ttf': '49b22767985171bce579307b21d98f4d35012fa39e316acc4ffcab8644dfb69b',
    'priv/fonts/stix/OFL.txt': '0c8825913b60d858aacdb33c4ca6660a7d64b0d6464702efbb19313f5765861a',
    'test/fixtures/html_to_pdf/browser_parity/mathml_radicals.html': 'cc76de79b7b35335a26c78ffe0efa09a5832b31cc5f6798d72365602ce042666',
    'tmp/references/mathml-core.html': 'f61ebea74ba54c489646e6969983ef977f448c26602edff75ec4e3ba77216373',
}
assert fontTools.__version__ == '4.60.1'
for path, expected in pins.items():
    actual = args.core_source if path == 'tmp/references/mathml-core.html' else ROOT/path
    assert hashlib.sha256(actual.read_bytes()).hexdigest() == expected
core = args.core_source.read_text()
assert all(f'id="{section}"' in core for section in ['radical-symbol', 'square-root', 'root-with-index'])
SOURCE = ROOT/'priv/fonts/stix/STIXTwoMath-Regular.ttf'
FIXTURE = ROOT/'test/fixtures/html_to_pdf/browser_parity/mathml_radicals.html'
source_html = FIXTURE.read_text()
font = TTFont(SOURCE, recalcTimestamp=False)
units = font['head'].unitsPerEm
constants = font['MATH'].table.MathConstants
cmap = font.getBestCmap()
gsub = font['GSUB'].table
script = next(r.Script for r in gsub.ScriptList.ScriptRecord if r.ScriptTag == 'math')
feature = next(gsub.FeatureList.FeatureRecord[i].Feature for i in script.DefaultLangSys.FeatureIndex
               if gsub.FeatureList.FeatureRecord[i].FeatureTag == 'ssty')
ssty = gsub.LookupList.Lookup[feature.LookupListIndex[0]].SubTable[0].alternates
pair_proofs=[]


def pair_advance(left,right,script_tag):
    """Read selected default-language-system pair advances from source GPOS."""
    gpos=font['GPOS'].table
    scripts={r.ScriptTag:r.Script for r in gpos.ScriptList.ScriptRecord}
    language=scripts.get(script_tag,scripts.get('DFLT')).DefaultLangSys
    advance=0
    for i in language.FeatureIndex:
        feature=gpos.FeatureList.FeatureRecord[i]
        if feature.FeatureTag!='kern': continue
        for lookup_index in feature.Feature.LookupListIndex:
            lookup=gpos.LookupList.Lookup[lookup_index]
            assert lookup.LookupFlag in [0,8]
            assert all(font['GDEF'].table.GlyphClassDef.classDefs.get(n,0)!=3 for n in [left,right])
            for table in lookup.SubTable:
                kind=lookup.LookupType
                if kind==9: kind,table=table.ExtensionLookupType,table.ExtSubTable
                assert kind==2
                if left not in table.Coverage.glyphs: continue
                if table.Format==1:
                    records=table.PairSet[table.Coverage.glyphs.index(left)].PairValueRecord
                    record=next((r for r in records if r.SecondGlyph==right),None)
                else:
                    assert table.Format==2
                    a=table.ClassDef1.classDefs.get(left,0); b=table.ClassDef2.classDefs.get(right,0)
                    record=table.Class1Record[a].Class2Record[b]
                if record is not None:
                    value=record.Value1
                    assert all(getattr(value,n,0)==0 for n in ['XPlacement','YPlacement','YAdvance'])
                    advance+=getattr(value,'XAdvance',0); break
    pair_proofs.append(dict(left=font.getGlyphID(left),right=font.getGlyphID(right),script=script_tag,design_advance=advance))
    return advance


class Tree(HTMLParser):
    def __init__(self):
        super().__init__(); self.stack = []; self.roots = []
        self.lines = [0]
        self.lines += [m.end() for m in re.finditer('\n', source_html)]

    def handle_starttag(self, tag, attributes):
        if tag in ['meta', 'link', 'br', 'img', 'hr', 'input']:
            return
        line, column = self.getpos()
        node = dict(tag=tag, attrs=dict(attributes), text='', children=[],
                    start=self.lines[line-1]+column, opening=self.get_starttag_text())
        if self.stack:
            node['parent'] = self.stack[-1]
            self.stack[-1]['children'].append(node)
        self.stack.append(node)

    def handle_data(self, data):
        if self.stack:
            self.stack[-1]['text'] += data

    def handle_endtag(self, tag):
        node = self.stack.pop(); assert node['tag'] == tag
        line, column = self.getpos()
        node['end'] = self.lines[line-1]+column+len('</'+tag+'>')
        if tag in ['msqrt', 'mroot'] and node['parent']['tag'] == 'math':
            self.roots.append(node)


def value(name, size):
    record = getattr(constants, name)
    assert record.DeviceTable is None
    return F(record.Value*size, units)


def scale(depth):
    return [F(1), F(constants.ScriptPercentScaleDown,100), F(constants.ScriptScriptPercentScaleDown,100)][depth]


def state(node, inherited=None):
    if inherited is None:
        inherited = state(node['parent']) if 'parent' in node else dict(size=F(12),depth=0,mode='normal',color='black',tag='')
    size, depth, mode, color = (inherited[k] for k in ['size','depth','mode','color'])
    if node['tag'] == 'math':
        depth=0; mode='normal' if node['attrs'].get('display') == 'block' else 'compact'
    elif inherited['tag'] == 'mfrac':
        depth += int(inherited['mode'] == 'compact'); mode='compact'
        size *= scale(depth)/scale(inherited['depth'])
    elif inherited['tag'] == 'mroot' and node is not node['parent']['children'][0]:
        depth += 2; mode='compact'; size *= scale(depth)/scale(inherited['depth'])
    if 'displaystyle' in node['attrs']:
        mode='normal' if node['attrs']['displaystyle'] == 'true' else 'compact'
    css=dict(part.strip().split(':',1) for part in node['attrs'].get('style','').split(';') if ':' in part)
    if 'font-size' in css:
        assert css['font-size'].endswith('pt'); size=F(css['font-size'][:-2])
    color=css.get('color',node['attrs'].get('mathcolor',color))
    return dict(size=size,depth=depth,mode=mode,color=color,tag=node['tag'])


def glyph(name, size, text, color):
    outline=font['glyf'][name]
    return dict(name=name,gid=font.getGlyphID(name),size=size,text=text,color=color,
                x=F(0),y=F(0),width=F(font['hmtx'][name][0]*size,units),
                ascent=F(outline.yMax*size,units),descent=-F(outline.yMin*size,units))


def move(box,x,y):
    return {key:[{**item,'x':item['x']+x,'y':item['y']+y} for item in box[key]] for key in ['glyphs','rules']}


def row(nodes,children,size):
    x=F(0); glyphs=[]; rules=[]
    for i,(node,box) in enumerate(zip(nodes,children)):
        # The fixed fixture's sole row operator is Core category B infix U+002B.
        spacing=F(0)
        if node['tag']=='mo':
            assert node['text']=='+' and 0<i<len(nodes)-1
            figure=re.search('<figure id="operator-dictionary-categories-values">(.*?)</figure>',core,re.S).group(1)
            rows=[[html.unescape(re.sub('<[^>]*>','',cell)).strip() for cell in re.findall('<td>(.*?)</td>',r,re.S)] for r in re.findall('<tr>(.*?)</tr>',figure,re.S) if '<td>' in r]
            category=next(r for r in rows if r[0]=='B')
            assert F(category[3].removesuffix('em')).limit_denominator(18)==F(4,18)
            assert F(category[4].removesuffix('em')).limit_denominator(18)==F(4,18)
            assert 'U+002B' in core
            spacing=size*F(4,18)
        x+=spacing; positioned=move(box,x,0); glyphs+=positioned['glyphs']; rules+=positioned['rules']; x+=box['width']+spacing
    return dict(width=x,ascent=max(b['ascent'] for b in children),descent=max(b['descent'] for b in children),
                ink_ascent=max(b['ink_ascent'] for b in children),ink_descent=max(b['ink_descent'] for b in children),glyphs=glyphs,rules=rules)


def stretch(size,target,color):
    name=cmap[0x221A]; variants=font['MATH'].table.MathVariants
    construction=variants.VertGlyphConstruction[variants.VertGlyphCoverage.glyphs.index(name)]
    natural=glyph(name,size,'',color)
    names=[name]+[v.VariantGlyph for v in construction.MathGlyphVariantRecord]
    if construction.GlyphAssembly:
        names += [p.glyph for p in construction.GlyphAssembly.PartRecords]
    preferred=max(F(font['hmtx'][n][0]*size,units) for n in names)
    if natural['ascent']+natural['descent'] >= target:
        return dict(width=natural['width'],ascent=natural['ascent'],descent=natural['descent'],glyphs=[natural],preferred=preferred)
    selected=next((v for v in construction.MathGlyphVariantRecord if v.AdvanceMeasurement*size/units>=target),None)
    if selected:
        g=glyph(selected.VariantGlyph,size,'',color)
        return dict(width=g['width'],ascent=g['ascent'],descent=g['descent'],glyphs=[g],preferred=preferred)
    # Core expansion with the previously recorded OpenType actual-join rule.
    parts=construction.GlyphAssembly.PartRecords
    minimum=F(variants.MinConnectorOverlap*size,units)
    assert all(F(p.StartConnectorLength*size,units)>=minimum for p in parts[1:])
    assert all(F(p.EndConnectorLength*size,units)>=minimum for p in parts[:-1])
    count=0
    while True:
        expanded=[p for p in parts for _ in range(count if p.PartFlags&1 else 1)]
        maximum_size=sum(F(p.FullAdvance*size,units) for p in expanded)-minimum*max(len(expanded)-1,0)
        if expanded and maximum_size>=target:
            break
        count+=1
    maximum=min([F(p.EndConnectorLength*size,units) for p in expanded[:-1]]+[F(p.StartConnectorLength*size,units) for p in expanded[1:]])
    overlap=min(maximum,max(minimum,(sum(F(p.FullAdvance*size,units) for p in expanded)-target)/max(len(expanded)-1,1)))
    advance=F(0); drawn=[]
    for p in expanded:
        g=glyph(p.glyph,size,'',color); g['y']=advance+g['descent']; drawn.append(g)
        advance+=F(p.FullAdvance*size,units)-overlap
    return dict(width=max(F(font['hmtx'][p.glyph][0]*size,units) for p in parts),
                ascent=maximum_size-(overlap-minimum)*max(len(expanded)-1,0),descent=F(0),glyphs=drawn,preferred=preferred)


def measure(node,inherited=None):
    st=state(node,inherited); size=st['size']; tag=node['tag']
    if tag in ['mn','mo']:
        result=[]; x=F(0); previous=None
        for char in node['text']:
            name=cmap[ord(char)]
            if st['depth'] and name in ssty:
                name=ssty[name][min(st['depth'],2)-1]
            if previous is not None:
                x+=F(pair_advance(previous,name,'latn' if tag=='mn' else 'math')*size,units)
            previous=name
            g=glyph(name,size,char,st['color']); g['x']=x; x+=g['width']; result.append(g)
        return dict(width=x,ascent=max(g['ascent'] for g in result),descent=max(g['descent'] for g in result),
                    ink_ascent=max(g['ascent'] for g in result),ink_descent=max(g['descent'] for g in result),glyphs=result,rules=[])
    children=[measure(n,st) for n in node['children']]
    if tag in ['msqrt','mroot']:
        base=children[0] if tag=='mroot' else row(node['children'],children,size)
        index=children[1] if tag=='mroot' else None
        gap=value('RadicalVerticalGap' if st['mode']=='compact' else 'RadicalDisplayStyleVerticalGap',size)
        thickness=value('RadicalRuleThickness',size); extra=value('RadicalExtraAscender',size)
        surd=stretch(size,base['ink_ascent']+base['ink_descent']+gap+thickness,st['color'])
        ascent=max(base['ascent'],base['ink_ascent']+gap+thickness+extra)
        descent=max(base['descent'],surd['ascent']+surd['descent']+extra-ascent)
        shift=F(0); index_glyphs=[]; index_rules=[]; final_ascent=ascent; final_descent=descent
        if index:
            before=max(F(0),value('RadicalKernBeforeDegree',size))
            shift=before+index['width']+max(-index['width'],value('RadicalKernAfterDegree',size))
            raised=F(constants.RadicalDegreeBottomRaisePercent,100)*(ascent+descent)
            positioned=move(index,before,-descent+raised+index['descent'])
            index_glyphs=positioned['glyphs']; index_rules=positioned['rules']
            final_ascent=max(ascent,-descent+raised+index['ascent'])
            final_descent=max(descent,descent-raised+index['ascent'])
        surd_y=ascent-extra-surd['ascent']
        surd_glyphs=[{**g,'x':g['x']+shift,'y':g['y']+surd_y} for g in surd['glyphs']]
        positioned=move(base,shift+surd['width'],F(0))
        ink_ascent=max(ascent-extra,base['ink_ascent'])
        ink_descent=max(surd['descent']-surd_y,base['ink_descent'])
        if index:
            ink_ascent=max(ink_ascent,index['ink_ascent']-descent+raised+index['descent'])
            ink_descent=max(ink_descent,index['ink_descent']+descent-raised-index['descent'])
        return dict(width=shift+surd['width']+base['width'],ascent=final_ascent,descent=final_descent,
                    ink_ascent=ink_ascent,ink_descent=ink_descent,
                    glyphs=surd_glyphs+positioned['glyphs']+index_glyphs,
                    rules=[dict(x=shift+surd['width'],y=ascent-extra-thickness,width=base['width'],height=thickness,color=st['color'])]+positioned['rules']+index_rules)
    if tag=='mfrac':
        a,b=children; normal=st['mode']=='normal'; axis=value('AxisHeight',size); thickness=value('FractionRuleThickness',size)
        up=max(value('FractionNumeratorDisplayStyleShiftUp' if normal else 'FractionNumeratorShiftUp',size),axis+thickness/2+value('FractionNumDisplayStyleGapMin' if normal else 'FractionNumeratorGapMin',size)+a['descent'])
        down=max(value('FractionDenominatorDisplayStyleShiftDown' if normal else 'FractionDenominatorShiftDown',size),thickness/2+value('FractionDenomDisplayStyleGapMin' if normal else 'FractionDenominatorGapMin',size)+b['ascent']-axis)
        width=max(a['width'],b['width']); padding=F(3,4)
        ga=move(a,padding+(width-a['width'])/2,up); gb=move(b,padding+(width-b['width'])/2,-down)
        return dict(width=width+2*padding,ascent=max(up+a['ascent'],-down+b['ascent'],axis+thickness/2),descent=max(-up+a['descent'],down+b['descent'],thickness/2-axis,F(0)),
                    ink_ascent=max(up+a['ink_ascent'],-down+b['ink_ascent'],axis+thickness/2),
                    ink_descent=max(-up+a['ink_descent'],down+b['ink_descent'],thickness/2-axis),
                    glyphs=ga['glyphs']+gb['glyphs'],rules=[dict(x=padding,y=axis-thickness/2,width=width,height=thickness,color=st['color'])]+ga['rules']+gb['rules'])
    assert tag in ['math','mrow']
    return row(node['children'],children,size)


def pure(node):
    return node['tag'] in ['msqrt','mroot','mrow','mfrac','mn','mo'] and all(pure(c) for c in node['children'])


def serialized(value):
    if isinstance(value,F): return dict(numerator=value.numerator,denominator=value.denominator,points=float(value))
    if isinstance(value,dict): return {k:serialized(v) for k,v in value.items()}
    if isinstance(value,list): return [serialized(v) for v in value]
    return value


def number(value):
    return f'{float(value):.12f}'.rstrip('0').rstrip('.')


tree=Tree(); tree.feed(source_html)
records=[]
for node in sorted(tree.roots,key=lambda n:n['start']):
    if pure(node): records.append((node,measure(node)))
assert len(records)==4
names=list(dict.fromkeys(g['name'] for _,m in records for g in m['glyphs']))
aliases={name:0xE500+i for i,name in enumerate(names)}
derivative=TTFont(SOURCE,recalcTimestamp=False)
for table in derivative['cmap'].tables:
    if table.isUnicode():
        for name,cp in aliases.items(): table.cmap[cp]=name
for tag in ['MATH','GSUB','GPOS']: del derivative[tag]
options=subset.Options(); options.recalc_bounds=False; options.recalc_timestamp=False
subsetter=subset.Subsetter(options=options); subsetter.populate(unicodes=list(aliases.values())); subsetter.subset(derivative)
for name in names:
    before=font['glyf'][name]; after=derivative['glyf'][name]
    assert before.getCoordinates(font['glyf'])==after.getCoordinates(derivative['glyf'])
    assert before.program.getBytecode()==after.program.getBytecode()
    assert font['hmtx'][name]==derivative['hmtx'][name]
for tag in ['cvt ','fpgm','prep']: assert font[tag].compile(font)==derivative[tag].compile(derivative)
family='Math Radical Core Reference'
for record in derivative['name'].names:
    if record.nameID in [1,3,4,6,16,18]: record.string=(family.replace(' ','') if record.nameID==6 else family).encode(record.getEncoding())
buffer=io.BytesIO(); derivative.save(buffer,reorderTables=False); font_bytes=buffer.getvalue()
css=f'<style data-m4-source-font="proposal">@font-face{{font-family:"{family}";src:url(data:font/ttf;base64,{base64.b64encode(font_bytes).decode()}) format("truetype")}}</style>'
manifest=dict(activation=False,inputs=pins,fonttools_version=fontTools.__version__,
              core_url='https://www.w3.org/TR/2025/CR-mathml-core-20250624/#radicals-msqrt-mroot',
              native_or_browser_geometry_inputs=False,empirical_offsets=[],
              unchanged_thresholds=dict(max_changed_ratio=.01,max_average_delta=.01,dpi=72,pixel_channel_tolerance=12),
              new_outer_radical_display_overrides=4,reference_font_sha256=hashlib.sha256(font_bytes).hexdigest(),
              selected_source_pair_advances=pair_proofs,
              aliases=[dict(name=n,cp=cp,source_gid=font.getGlyphID(n)) for n,cp in aliases.items()],
              font_stylesheet=css,records=[])
reference=source_html
for node,m in reversed(records):
    ascent,descent=m['ascent'],m['descent']; paint=[]
    for g in m['glyphs']:
        css_ascent=round(F(font['hhea'].ascent*g['size'],units)/F(3,4))*F(3,4)
        paint.append(f'<span data-m4-text="{html.escape(g["text"],quote=True)}" style="position:absolute;left:{number(g["x"])}pt;top:{number(ascent-g["y"]-css_ascent)}pt;font-family:\'{family}\';font-size:{number(g["size"])}pt;line-height:{number(g["size"])}pt;color:{g["color"]};white-space:nowrap">{chr(aliases[g["name"]])}</span>')
    for r in m['rules']:
        paint.append(f'<span style="position:absolute;left:{number(r["x"])}pt;top:{number(ascent-r["y"]-r["height"])}pt;width:{number(r["width"])}pt;height:{number(r["height"])}pt;background:{r["color"]}"></span>')
    witness=f'<span style="display:inline-block;width:0;height:{number(ascent)}pt;line-height:0;font-size:0"></span>'
    transport=f'display:inline-block;width:{number(m["width"])}pt;height:{number(ascent+descent)}pt;line-height:0;font-size:0'
    opening=node['opening'][:-1]+f' style="{transport}">'
    fragment=opening+f'<mtext style="display:inline-block;line-height:0;font-size:0"><span style="display:inline-block;position:relative;width:{number(m["width"])}pt;height:{number(ascent+descent)}pt;line-height:0;font-size:0">'+witness+''.join(paint)+'</span></mtext></'+node['tag']+'>'
    original=source_html[node['start']:node['end']]
    manifest['records'].insert(0,dict(original_fragment=original,original_sha256=hashlib.sha256(original.encode()).hexdigest(),reference_fragment=fragment,geometry=serialized(m)))
    reference=reference[:node['start']]+fragment+reference[node['end']:]
fragments=reference
reference=reference.replace('</head>',css+'</head>',1)
restored=reference.replace(css,'',1)
for r in manifest['records']: restored=restored.replace(r['reference_fragment'],r['original_fragment'],1)
assert restored==source_html
linked='<msqrt><mtext><a href="https://radical.test/">Link</a></mtext></msqrt>'
assert linked in reference and reference.count(linked)==source_html.count(linked)==1
assert re.findall(r'<a\b[^>]*>.*?</a>',reference)==re.findall(r'<a\b[^>]*>.*?</a>',source_html)
OUT.mkdir(parents=True,exist_ok=True)
products={'reference.html':reference.encode(),'reference-fragments.html':fragments.encode(),
          'reference-manifest.json':(json.dumps(manifest,indent=2)+'\n').encode(),
          'math-radical-core-reference.ttf':font_bytes,'math-radical-core-reference-LICENSE.txt':(ROOT/'priv/fonts/stix/OFL.txt').read_bytes()}
for name,data in products.items():
    if args.check: assert (OUT/name).read_bytes()==data
    else: (OUT/name).write_bytes(data)
print('Unactivated M4 proposal: four reversible pure trees, four NEW display overrides, exact source outlines/hints/advances, unchanged linked tree')
