"""Independent source-only proof for the fixed unactivated M4 proposal.

No generator imports, native code, measurements, browser or PDF inputs.
"""
from fractions import Fraction as Q
import base64
import hashlib
import html
from html.parser import HTMLParser
import json
from pathlib import Path
import re
import fontTools
from fontTools.ttLib import TTFont

assert fontTools.__version__ == '4.60.1'
ROOT=Path(__file__).resolve().parents[3]
OUT=Path(__file__).resolve().parent
manifest=json.loads((OUT/'reference-manifest.json').read_text())
for name,sha in manifest['inputs'].items():
    assert hashlib.sha256((ROOT/name).read_bytes()).hexdigest()==sha
assert manifest['activation'] is False
assert manifest['native_or_browser_geometry_inputs'] is False and manifest['empirical_offsets']==[]
assert manifest['new_outer_radical_display_overrides']==4
assert manifest['unchanged_thresholds']==dict(max_changed_ratio=.01,max_average_delta=.01,dpi=72,pixel_channel_tolerance=12)
original=(ROOT/'test/fixtures/html_to_pdf/browser_parity/mathml_radicals.html').read_text()
complete=(OUT/'reference.html').read_text()
fragments=(OUT/'reference-fragments.html').read_text()
for content in [complete.replace(manifest['font_stylesheet'],'',1),fragments]:
    restored=content
    for r in manifest['records']:
        assert content.count(r['reference_fragment'])==1
        assert hashlib.sha256(r['original_fragment'].encode()).hexdigest()==r['original_sha256']
        restored=restored.replace(r['reference_fragment'],r['original_fragment'],1)
    assert restored==original
linked='<msqrt><mtext><a href="https://radical.test/">Link</a></mtext></msqrt>'
assert linked in complete and linked in fragments
assert re.findall(r'<a\b[^>]*>.*?</a>',complete)==re.findall(r'<a\b[^>]*>.*?</a>',original)
assert complete.count('<math ')==original.count('<math ')==5
source=TTFont(ROOT/'priv/fonts/stix/STIXTwoMath-Regular.ttf',recalcTimestamp=False)
reference=TTFont(OUT/'math-radical-core-reference.ttf',recalcTimestamp=False)
data=(OUT/'math-radical-core-reference.ttf').read_bytes()
assert hashlib.sha256(data).hexdigest()==manifest['reference_font_sha256']
assert base64.b64decode(re.search(r'base64,([^)]*)',manifest['font_stylesheet']).group(1))==data
assert (OUT/'math-radical-core-reference-LICENSE.txt').read_bytes()==(ROOT/'priv/fonts/stix/OFL.txt').read_bytes()
for r in manifest['aliases']:
    name=r['name']; refname=reference.getBestCmap()[r['cp']]
    assert source.getGlyphID(name)==r['source_gid']
    a,b=source['glyf'][name],reference['glyf'][refname]
    assert a.getCoordinates(source['glyf'])==b.getCoordinates(reference['glyf'])
    assert a.program.getBytecode()==b.program.getBytecode()
    assert source['hmtx'][name]==reference['hmtx'][refname]
for name in ['head','hhea']:
    for field in (['unitsPerEm'] if name=='head' else ['ascent','descent','lineGap']):
        assert getattr(source[name],field)==getattr(reference[name],field)
for name in ['cvt ','fpgm','prep']: assert source[name].compile(source)==reference[name].compile(reference)
c=source['MATH'].table.MathConstants; cmap=source.getBestCmap(); units=source['head'].unitsPerEm
gpos=source['GPOS'].table
latn=next(r.Script for r in gpos.ScriptList.ScriptRecord if r.ScriptTag=='latn')
language=latn.DefaultLangSys
assert language.ReqFeatureIndex==0xFFFF and language.FeatureIndex==[3]
kern=gpos.FeatureList.FeatureRecord[3]
assert kern.FeatureTag=='kern' and kern.Feature.LookupListIndex==[0,3]
pair_proofs=[]


def pair_adjustment(left_gid,right_gid):
    left,right=map(source.getGlyphName,[left_gid,right_gid])
    assert all(source['GDEF'].table.GlyphClassDef.classDefs.get(n,0)!=3 for n in [left,right])
    advance=0; paths=[]
    for lookup_index in kern.Feature.LookupListIndex:
        lookup=gpos.LookupList.Lookup[lookup_index]
        assert lookup.LookupType==2 and lookup.LookupFlag==8
        matched=None
        for subtable_index,table in enumerate(lookup.SubTable):
            assert table.Format in [1,2]
            if left not in table.Coverage.glyphs: continue
            if table.Format==1:
                records=table.PairSet[table.Coverage.glyphs.index(left)].PairValueRecord
                record=next((r for r in records if r.SecondGlyph==right),None)
            else:
                first=table.ClassDef1.classDefs.get(left,0)
                second=table.ClassDef2.classDefs.get(right,0)
                record=table.Class1Record[first].Class2Record[second]
            if record is None: continue
            for side in ['Value1','Value2']:
                value=getattr(record,side,None)
                fields=vars(value) if value is not None else {}
                assert all(v==0 for k,v in fields.items() if not (side=='Value1' and k=='XAdvance')),fields
            advance+=getattr(record.Value1,'XAdvance',0)
            matched=dict(subtable=subtable_index,format=table.Format)
            break
        paths.append(dict(lookup=lookup_index,match=matched))
    pair_proofs.append(dict(left=left_gid,right=right_gid,script='latn',design_advance=advance,
                            feature_index=3,feature='kern',lookups=paths))
    return advance


pair_advances={(left,right):pair_adjustment(left,right) for left,right in [(1138,1143),(1138,1139)]}
assert manifest['selected_source_pair_advances']==[
    {k:r[k] for k in ['left','right','script','design_advance']} for r in pair_proofs]
gsub=source['GSUB'].table
script=next(r.Script for r in gsub.ScriptList.ScriptRecord if r.ScriptTag=='math')
feature=next(gsub.FeatureList.FeatureRecord[i].Feature for i in script.DefaultLangSys.FeatureIndex if gsub.FeatureList.FeatureRecord[i].FeatureTag=='ssty')
ssty=gsub.LookupList.Lookup[feature.LookupListIndex[0]].SubTable[0].alternates


def metric(text,size,depth=0):
    drawn=[]; width=Q(0)
    names=[ssty[cmap[ord(char)]][depth-1] if depth and cmap[ord(char)] in ssty else cmap[ord(char)] for char in text]
    for i,(char,name) in enumerate(zip(text,names)):
        gl=source['glyf'][name]; advance=Q(source['hmtx'][name][0]*size,units)
        if i+1<len(names):
            advance+=Q(pair_advances[(source.getGlyphID(name),source.getGlyphID(names[i+1]))]*size,units)
        drawn.append(dict(gid=source.getGlyphID(name),size=size,text=char,x=width,y=Q(0),width=advance))
        width+=advance
    ascent=max(Q(source['glyf'][source.getGlyphName(g['gid'])].yMax*size,units) for g in drawn)
    descent=max(-Q(source['glyf'][source.getGlyphName(g['gid'])].yMin*size,units) for g in drawn)
    return dict(width=width,ascent=ascent,descent=descent,ink_ascent=ascent,ink_descent=descent,glyphs=drawn,rules=[])


def value(name,size):
    record=getattr(c,name); assert record.DeviceTable is None
    return Q(record.Value*size,units)


def translate(draw,x,y):
    return [{**g,'x':g['x']+x,'y':g['y']+y} for g in draw]


def radical(base,size,index=None):
    gap=value('RadicalVerticalGap',size); t=value('RadicalRuleThickness',size); extra=value('RadicalExtraAscender',size)
    target=base['ink_ascent']+base['ink_descent']+gap+t
    name=cmap[0x221A]; natural=source['glyf'][name]
    variants=source['MATH'].table.MathVariants
    construction=variants.VertGlyphConstruction[variants.VertGlyphCoverage.glyphs.index(name)]
    if Q((natural.yMax-natural.yMin)*size,units)<target:
        name=next(r.VariantGlyph for r in construction.MathGlyphVariantRecord if Q(r.AdvanceMeasurement*size,units)>=target)
    gl=source['glyf'][name]; surd_a=Q(gl.yMax*size,units); surd_d=-Q(gl.yMin*size,units)
    surd_w=Q(source['hmtx'][name][0]*size,units)
    a=max(base['ascent'],base['ink_ascent']+gap+t+extra)
    d=max(base['descent'],surd_a+surd_d+extra-a)
    shift=Q(0); index_glyphs=[]; final_a=a; final_d=d
    ink_a=max(a-extra,base['ink_ascent']); ink_d=max(surd_d-a+extra+surd_a,base['ink_descent'])
    if index:
        before=max(Q(0),value('RadicalKernBeforeDegree',size))
        shift=before+index['width']+max(-index['width'],value('RadicalKernAfterDegree',size))
        raised=Q(c.RadicalDegreeBottomRaisePercent,100)*(a+d)
        y=-d+raised+index['descent']
        index_glyphs=translate(index['glyphs'],before,y)
        final_a=max(a,-d+raised+index['ascent']); final_d=max(d,d-raised+index['ascent'])
        ink_a=max(ink_a,index['ink_ascent']+y); ink_d=max(ink_d,index['ink_descent']-y)
    drawn=[dict(gid=source.getGlyphID(name),size=size,text='',x=shift,y=a-extra-surd_a,width=surd_w)]
    drawn+=translate(base['glyphs'],shift+surd_w,0)+index_glyphs
    rules=[dict(x=shift+surd_w,y=a-extra-t,width=base['width'],height=t)]+translate(base['rules'],shift+surd_w,0)
    return dict(width=shift+surd_w+base['width'],ascent=final_a,descent=final_d,ink_ascent=ink_a,ink_descent=ink_d,glyphs=drawn,rules=rules)


def fraction(a,b,size):
    axis=value('AxisHeight',size); t=value('FractionRuleThickness',size)
    up=max(value('FractionNumeratorDisplayStyleShiftUp',size),axis+t/2+value('FractionNumDisplayStyleGapMin',size)+a['ink_descent'])
    down=max(value('FractionDenominatorDisplayStyleShiftDown',size),t/2+value('FractionDenomDisplayStyleGapMin',size)+b['ink_ascent']-axis)
    w=max(a['width'],b['width']); pad=Q(3,4)
    asc=max(up+a['ascent'],-down+b['ascent'],axis+t/2)
    desc=max(-up+a['descent'],down+b['descent'],t/2-axis,0)
    return dict(width=w+2*pad,ascent=asc,descent=desc,ink_ascent=asc,ink_descent=desc,
                glyphs=translate(a['glyphs'],pad+(w-a['width'])/2,up)+translate(b['glyphs'],pad+(w-b['width'])/2,-down),
                rules=[dict(x=pad,y=axis-t/2,width=w,height=t)])


def row():
    items=[metric(char,Q(24)) for char in '7+9']; offset=Q(0); draw=[]
    for i,item in enumerate(items):
        spacing=Q(24*4,18) if i==1 else Q(0)
        offset+=spacing; draw+=translate(item['glyphs'],offset,0); offset+=item['width']+spacing
    a=max(b['ascent'] for b in items); d=max(b['descent'] for b in items)
    return dict(width=offset,ascent=a,descent=d,ink_ascent=a,ink_descent=d,glyphs=draw,rules=[])


assert [r['original_fragment'] for r in manifest['records']]==[
    '<msqrt><mn>2</mn></msqrt>',
    '<mroot><mn>16</mn><mn>3</mn></mroot>',
    '<msqrt mathcolor="blue"><mn>7</mn><mo>+</mo><mn>9</mn></msqrt>',
    '<mroot><msqrt><mfrac displaystyle="true"><mn>12</mn><mn>3</mn></mfrac></msqrt><mn>5</mn></mroot>',
]
small=radical(metric('2',Q(12)),Q(12))
degree_size=Q(18*c.ScriptScriptPercentScaleDown,100)
medium=radical(metric('16',Q(18)),Q(18),metric('3',degree_size,2))
large=radical(row(),Q(24))
frac=fraction(metric('12',Q(18)),metric('3',Q(18)),Q(18))
nested=radical(radical(frac,Q(18)),Q(18),metric('5',degree_size,2))
independent_cases=[small,medium,large,nested]
for record,independent in zip(manifest['records'],independent_cases):
    generated=record['geometry']
    for name in ['width','ascent','descent','ink_ascent','ink_descent']:
        assert Q(generated[name]['numerator'],generated[name]['denominator'])==independent[name]
    for kind in ['glyphs','rules']:
        assert len(generated[kind])==len(independent[kind])
        for a,b in zip(generated[kind],independent[kind]):
            for name,value in b.items():
                actual=Q(a[name]['numerator'],a[name]['denominator']) if isinstance(value,Q) else a[name]
                assert actual==value,(kind,name,actual,value)


class Artwork(HTMLParser):
    """Parse actual emitted documents without trusting manifest CSS strings."""
    def __init__(self):
        super().__init__(convert_charrefs=True)
        self.stack=[]; self.roots=[]
    def handle_starttag(self,tag,attrs):
        if tag in ['meta','link','br','img','hr','input']: return
        assert len(dict(attrs))==len(attrs)
        node=dict(tag=tag,attrs=dict(attrs),children=[],text='')
        if self.stack: self.stack[-1]['children'].append(node)
        if tag in ['msqrt','mroot'] and self.stack and self.stack[-1]['tag']=='math':
            self.roots.append(node)
        self.stack.append(node)
    def handle_data(self,text):
        if self.stack: self.stack[-1]['text']+=text
    def handle_endtag(self,tag):
        assert self.stack.pop()['tag']==tag


def css(node,expected):
    declarations=[v.strip().split(':',1) for v in node['attrs']['style'].split(';')]
    actual=dict(declarations)
    assert len(actual)==len(declarations) and actual.keys()==expected.keys(),actual
    for name,value in expected.items():
        if isinstance(value,Q):
            assert actual[name].endswith('pt')
            assert abs(Q(actual[name][:-2])-value)<=Q(1,10**12),(name,actual[name],value)
        else:
            assert actual[name]==value,(name,actual[name],value)


original_tree=Artwork(); original_tree.feed(original)
assert len(original_tree.roots)==5
css_counts=[]
for document in [complete,fragments]:
    tree=Artwork(); tree.feed(document)
    assert len(tree.roots)==5 and not tree.stack
    counts=dict(glyphs=0,rules=0,witnesses=0,outer_dimensions=0)
    for i,(outer,authored,geometry) in enumerate(zip(tree.roots,original_tree.roots,independent_cases)):
        assert outer['tag']==authored['tag'] and outer['text']==''
        assert {k:v for k,v in outer['attrs'].items() if k!='style'}==authored['attrs']
        dimensions=dict(width=geometry['width'],height=geometry['ascent']+geometry['descent'])
        css(outer,dict(display='inline-block',**dimensions,**{'line-height':'0','font-size':'0'}))
        counts['outer_dimensions']+=1
        assert len(outer['children'])==1
        host=outer['children'][0]
        assert host['tag']=='mtext' and host['text']=='' and host['attrs'].keys()=={'style'}
        css(host,{'display':'inline-block','line-height':'0','font-size':'0'})
        assert len(host['children'])==1
        canvas=host['children'][0]
        assert canvas['tag']=='span' and canvas['text']=='' and canvas['attrs'].keys()=={'style'}
        css(canvas,dict(display='inline-block',position='relative',**dimensions,**{'line-height':'0','font-size':'0'}))
        witness,*paint=canvas['children']
        assert witness['tag']=='span' and witness['attrs'].keys()=={'style'} and witness['children']==[] and witness['text']==''
        css(witness,{'display':'inline-block','width':'0','height':geometry['ascent'],'line-height':'0','font-size':'0'})
        counts['witnesses']+=1
        assert len(paint)==len(geometry['glyphs'])+len(geometry['rules'])
        color='blue' if i==2 else 'black'
        for node,glyph in zip(paint,geometry['glyphs']):
            assert node['tag']=='span' and node['children']==[]
            assert node['attrs'].keys()=={'style','data-m4-text'} and node['attrs']['data-m4-text']==glyph['text']
            assert len(node['text'])==1
            alias=next(r for r in manifest['aliases'] if r['cp']==ord(node['text']))
            assert source.getGlyphID(alias['name'])==glyph['gid']
            ascent=round(Q(source['hhea'].ascent,units)*glyph['size']/Q(3,4))*Q(3,4)
            css(node,{'position':'absolute','left':glyph['x'],'top':geometry['ascent']-glyph['y']-ascent,
                      'font-family':"'Math Radical Core Reference'",'font-size':glyph['size'],
                      'line-height':glyph['size'],'color':color,'white-space':'nowrap'})
            counts['glyphs']+=1
        for node,rule in zip(paint[len(geometry['glyphs']):],geometry['rules']):
            assert node['tag']=='span' and node['children']==[] and node['text']=='' and node['attrs'].keys()=={'style'}
            css(node,{'position':'absolute','left':rule['x'],'top':geometry['ascent']-rule['y']-rule['height'],
                      'width':rule['width'],'height':rule['height'],'background':color})
            counts['rules']+=1
    assert tree.roots[4]==original_tree.roots[4]
    assert counts==dict(glyphs=16,rules=6,witnesses=4,outer_dimensions=4)
    css_counts.append(counts)
result=dict(activation=False,reversible_source_sha256=hashlib.sha256(original.encode()).hexdigest(),
            independent_rational_geometry_cases=4,source_outlines_hints_advances_verified=True,
            linked_tree_byte_identical=True,new_outer_display_overrides=4,reference_font_sha256=manifest['reference_font_sha256'],
            fonttools_version=fontTools.__version__,source_gpos_default_latn_pair_proofs=pair_proofs,
            complete_and_fragments_css_source_geometry_verified=css_counts)
(OUT/'proof.json').write_text(json.dumps(result,indent=2)+'\n')
print(json.dumps(result,indent=2))
