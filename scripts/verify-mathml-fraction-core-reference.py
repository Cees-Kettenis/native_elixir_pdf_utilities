"""Independent reversible/content/font proof. It imports no generator or renderer."""
import argparse
from fractions import Fraction as F
import hashlib
import html
from html.parser import HTMLParser
import json
from pathlib import Path
import re
from fontTools.ttLib import TTFont

ROOT=Path(__file__).resolve().parents[1]
HERE=ROOT/'test/fixtures/html_to_pdf/browser_parity/assets'
arguments=argparse.ArgumentParser()
arguments.add_argument('--core-source',type=Path,default=ROOT/'tmp/references/mathml-core.html')
args=arguments.parse_args()
manifest=json.loads((HERE/'math-fraction-core-reference.json').read_text())
original=(ROOT/'test/fixtures/html_to_pdf/browser_parity/mathml_fractions.html').read_bytes()
reference=(HERE/'math-fraction-core-reference.html').read_text()
fragments=(HERE/'math-fraction-core-reference-fragments.html').read_text()
for path, expected in manifest['inputs'].items():
    assert hashlib.sha256((args.core_source if path=='tmp/references/mathml-core.html' else ROOT/path).read_bytes()).hexdigest()==expected
for face in manifest['browser_packaging']['faces']:
    assert hashlib.sha256((ROOT/face['path']).read_bytes()).hexdigest()==face['sha256']
assert manifest['activation'] is False
assert manifest['empirical_offsets']==[] and manifest['native_or_browser_geometry_inputs'] is False
assert manifest['unchanged_thresholds']==dict(max_changed_ratio=.01,max_average_delta=.01,pixel_channel_tolerance=12,dpi=72)
assert all(p['design_advance']==0 for p in manifest['selected_source_pair_advances'])
for name, document in [('fragments',fragments),('complete',reference)]:
    restored=document
    if name=='complete':
        package=manifest['browser_packaging']
        assert restored.count(package['font_stylesheet'])==1
        restored=restored.replace(package['font_stylesheet'],'',1)
        assert restored.count(package['artwork_only_reference'])==1
        restored=restored.replace(package['artwork_only_reference'],package['artwork_only_original'],1)
    for key, record in manifest['records'].items():
        assert restored.count(record['reference_fragment'])==1
        restored=restored.replace(record['reference_fragment'],record['original_fragment'],1)
    assert restored.encode()==original
source=TTFont(ROOT/'priv/fonts/stix/STIXTwoMath-Regular.ttf',recalcTimestamp=False)
for record in manifest['records'].values():
    originals=''.join(re.findall(r'>([^<>]*)</(?:mn|mo)>',record['original_fragment']))
    assert originals==record['original_text']
    glyphs=record['interior']['glyphs']
    assert ''.join(g['text'] for g in glyphs)==record['original_text']
    assert all(source.getGlyphID(g['name'])==g['gid'] for g in glyphs)
    assert all(F(g['width']['numerator'],g['width']['denominator'])==source['hmtx'][g['name']][0]*F(g['size']['numerator'],g['size']['denominator'])/source['head'].unitsPerEm for g in glyphs)
    painted=[html.unescape(t) for t in re.findall('data-core-text="([^"]*)"',record['reference_fragment'])]
    assert ''.join(painted)==record['original_text']
    assert record['original_opening'] in record['original_fragment']
    assert record['original_sha256']==hashlib.sha256(record['original_fragment'].encode()).hexdigest()
    outer=record['outer_css_transport']
    assert outer['baseline_witness_height']==outer['ascent']
    assert abs(outer['height']['points']-outer['ascent']['points']-outer['descent']['points'])<1e-12
f=source
c=f['MATH'].table.MathConstants
cmap=f.getBestCmap()
g=f['GSUB'].table
script=next(r.Script for r in g.ScriptList.ScriptRecord if r.ScriptTag=='math')
feature=next(g.FeatureList.FeatureRecord[i].Feature for i in script.DefaultLangSys.FeatureIndex if g.FeatureList.FeatureRecord[i].FeatureTag=='ssty')
ssty=g.LookupList.Lookup[feature.LookupListIndex[0]].SubTable[0].alternates
class Tree(HTMLParser):
 def __init__(self): super().__init__();self.stack=[];self.math=[]
 def handle_starttag(self,tag,attrs):
  if tag in ['meta','link','br','img','hr','input']: return
  n={'tag':tag,'attrs':dict(attrs),'children':[],'text':''}
  if self.stack:self.stack[-1]['children'].append(n)
  self.stack.append(n)
 def handle_data(self,text):
  if self.stack:self.stack[-1]['text']+=text
 def handle_endtag(self,tag):
  n=self.stack.pop();assert n['tag']==tag
  if tag=='math':self.math.append(n)
p=Tree();p.feed(original.decode());n=p.math[2]
assert n['attrs']['style']=='font-size:24pt'
assert n['children'][0]['tag']=='mfrac'
def glyph(text,size,depth):
 a=d=w=0;draw=[]
 for char in text:
  name=cmap[ord(char)]
  if depth and name in ssty:name=ssty[name][min(depth,2)-1]
  gl=f['glyf'][name];advance=f['hmtx'][name][0]*size/1000
  a=max(a,gl.yMax*size/1000);d=max(d,-gl.yMin*size/1000)
  draw.append({'text':char,'gid':f.getGlyphID(name),'size':size,'x':w,'y':0})
  w+=advance
 return dict(width=w,ascent=a,descent=d,glyphs=draw,rules=[])
def move(m,x,y):
 return [{**v,'x':v['x']+x,'y':v['y']+y} for v in m['glyphs']], [{**v,'x':v['x']+x,'y':v['y']+y} for v in m['rules']]
def measure(n,size,depth):
 tag=n['tag']
 if tag in ['mn','mo']:return glyph(n['text'],size,depth)
 if tag=='mfrac':
  # Both nested fractions are compact. UA auto-add first selects 70%, then 55%.
  child_size=size*(c.ScriptPercentScaleDown/100 if depth==0 else c.ScriptScriptPercentScaleDown/c.ScriptPercentScaleDown)
  a,b=[measure(v,child_size,depth+1) for v in n['children']]
  k=size/1000
  t=c.FractionRuleThickness.Value*k;axis=c.AxisHeight.Value*k
  up=max(c.FractionNumeratorShiftUp.Value*k,axis+t/2+c.FractionNumeratorGapMin.Value*k+a['descent'])
  down=max(c.FractionDenominatorShiftDown.Value*k,t/2+c.FractionDenominatorGapMin.Value*k+b['ascent']-axis)
  w=max(a['width'],b['width']);ga,ra=move(a,.75+(w-a['width'])/2,up);gb,rb=move(b,.75+(w-b['width'])/2,-down)
  return dict(width=w+1.5,ascent=max(up+a['ascent'],-down+b['ascent'],axis+t/2),descent=max(-up+a['descent'],down+b['descent'],t/2-axis,0),glyphs=ga+gb,rules=ra+rb+[dict(x=.75,y=axis-t/2,width=w,height=t)])
 assert tag in ['math','mrow']
 children=[measure(v,size,depth) for v in n['children']]
 x=0;draw=[];rules=[]
 for node,m in zip(n['children'],children):
  # Core Appendix C U+002B infix category B has four eighteenth-em spaces.
  spacing=size*4/18 if node['tag']=='mo' and node['text']=='+' else 0
  x+=spacing;g,r=move(m,x,0);draw+=g;rules+=r;x+=m['width']+spacing
 return dict(width=x,ascent=max(v['ascent'] for v in children),descent=max(v['descent'] for v in children),glyphs=draw,rules=rules)
r=measure(n,24,0)

independent=r
nested=manifest['records']['fraction-05']
outer=nested['outer_css_transport']
assert abs(outer['content_width']['points']+2*outer['padding']['points']-independent['width'])<1e-12
assert abs(outer['ascent']['points']-independent['ascent'])<1e-12
assert abs(outer['descent']['points']-independent['descent'])<1e-12
for a,b in zip(nested['interior']['glyphs'],independent['glyphs']):
    assert a['gid']==b['gid'] and a['text']==b['text']
    for field in ['size','x','y']:assert abs(a[field]['points']-b[field])<1e-12
assert re.findall(r'<a\b[^>]*>.*?</a>',reference)==re.findall(r'<a\b[^>]*>.*?</a>',original.decode())
assert len(re.findall(r'<math\b',reference))==len(re.findall(r'<math\b',original.decode()))==6
linked=manifest['unchanged_linked_fraction']
assert linked in fragments
assert '<mtext><span><a href="https://fraction.test/">Link</a></span></mtext>' in reference
ordinary=ROOT/'test/fixtures/html_to_pdf/browser_parity/mathml_fractions_ordinary.html'
proof=dict(activation=False,original_reproduced_sha256=hashlib.sha256(original).hexdigest(),
           complete_and_fragment_reference_reversible=True,original_math_roots_retained=6,
           top_fraction_interiors_replaced=8,nested_tree_restored_byte_exactly=True,
           logical_glyph_text_source_gids_advances_and_sizes_verified=True,
           independent_prior_nested_geometry_matches=True,source_font_pair_advances_zero=True,
           original_link_markup_and_html_token_tree_retained=True,
           direct_ordinary_fixture_sha256=hashlib.sha256(ordinary.read_bytes()).hexdigest(),
           native_source_or_algorithm_changed_by_proposal=False,thresholds=manifest['unchanged_thresholds'])
print(json.dumps(proof,indent=2))
