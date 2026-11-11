"""Build the licensed CFF parser corpus with fonttools 4.66.1.

Uses the existing generated DejaVu subset, with no network access.
The OpenType and raw CFF bytes share independently measured glyph bounds.
"""
from pathlib import Path
import hashlib,json
from fontTools.ttLib import TTFont
from fontTools.fontBuilder import FontBuilder
from fontTools.pens.t2CharStringPen import T2CharStringPen
from fontTools.pens.qu2cuPen import Qu2CuPen
from fontTools.pens.boundsPen import BoundsPen

root=Path(__file__).resolve().parents[2]
out=root/'test/fixtures/fonts'
source_path=root/'test/fixtures/fonts/selected-container-sans.ttf'
source=TTFont(source_path,recalcTimestamp=False)
upm=source['head'].unitsPerEm
order=source.getGlyphOrder()
glyphs=source.getGlyphSet()
programs={}
for name in order:
    pen=T2CharStringPen(source['hmtx'][name][0],glyphs)
    glyphs[name].draw(Qu2CuPen(pen,0.5,all_cubic=True))
    programs[name]=pen.getCharString()
builder=FontBuilder(upm,isTTF=False)
builder.setupGlyphOrder(order)
mapping = source.getBestCmap()
mapping[0x210B] = mapping[ord("H")]
builder.setupCharacterMap(mapping)
builder.setupHorizontalMetrics(source['hmtx'].metrics)
builder.setupHorizontalHeader(ascent=source['hhea'].ascent,descent=source['hhea'].descent,lineGap=source['hhea'].lineGap)
builder.setupNameTable({'familyName':'Static CFF Sans','styleName':'Regular','uniqueFontIdentifier':'StaticCffSans','fullName':'Static CFF Sans','psName':'StaticCffSans'})
builder.setupOS2(fsType=source['OS/2'].fsType,sTypoAscender=source['OS/2'].sTypoAscender,sTypoDescender=source['OS/2'].sTypoDescender,sTypoLineGap=source['OS/2'].sTypoLineGap,usWinAscent=source['OS/2'].usWinAscent,usWinDescent=source['OS/2'].usWinDescent)
builder.setupPost()
builder.setupCFF('StaticCffSans',{'FullName':'Static CFF Sans','FamilyName':'Static CFF Sans','Weight':'Regular'},programs,{})
builder.font['head'].created=source['head'].created
builder.font['head'].modified=source['head'].modified
builder.font.recalcTimestamp=False
path=out/'cff-static-sans.otf'
builder.save(path)
face=TTFont(path,recalcTimestamp=False)
selected=face.getGlyphSet()
expected={}
for name in order:
    pen=BoundsPen(selected)
    selected[name].draw(pen)
    expected[name]={'bounds':pen.bounds or [0,0,0,0],'advance':face['hmtx'][name][0]}
manifest={'source_sha256':hashlib.sha256(source_path.read_bytes()).hexdigest(),'font_sha256':hashlib.sha256(path.read_bytes()).hexdigest(),'units_per_em':upm,'glyph_order':order,'glyphs':expected,'cff_sha256':hashlib.sha256(face.getTableData('CFF ')).hexdigest(),'generator':'scripts/fixtures/build-cff-fonts.py','source_font':'selected-container-sans.ttf','fonttools_version':__import__('fontTools').__version__,'license':'test/fixtures/fonts/selected-container-LICENSE.txt'}
(out/'cff-static-sans.json').write_text(json.dumps(manifest,indent=2)+'\n')
(out/'cff-static-sans.cff').write_bytes(face.getTableData('CFF '))
print(json.dumps({'font_bytes':path.stat().st_size,'cff_bytes':len(face.getTableData('CFF ')),'glyphs':len(order),'expected':expected},indent=2))

# Independent Type 2 operator vectors. BoundsPen measures cubic extrema rather
# than a control-point box; RecordingPen also preserves exact path endpoints.
from fontTools.misc.psCharStrings import T2CharString
from fontTools.pens.recordingPen import RecordingPen
from types import SimpleNamespace
private = SimpleNamespace(defaultWidthX=600, nominalWidthX=100, Subrs=[])
global_subrs = []
private.Subrs = [T2CharString(program=[10, 20, 'rlineto', 'return'], private=private, globalSubrs=global_subrs)]
global_subrs.append(T2CharString(program=[30, 40, 'rlineto', 'return'], private=private, globalSubrs=global_subrs))
operations = {
    'line_pairs': [10,20,30,-10,'rlineto'],
    'horizontal_lines': [10,20,30,'hlineto'],
    'vertical_lines': [10,20,30,'vlineto'],
    'cubic_extrema': [100,200,100,-200,100,0,'rrcurveto'],
    'curve_line': [10,20,30,40,50,60,70,80,'rcurveline'],
    'line_curve': [10,20,30,40,50,60,70,80,'rlinecurve'],
    'vertical_curves': [10,20,30,40,50,60,70,80,'vvcurveto'],
    'vertical_optional': [5,10,20,30,40,'vvcurveto'],
    'horizontal_curves': [10,20,30,40,50,60,70,80,'hhcurveto'],
    'horizontal_optional': [5,10,20,30,40,'hhcurveto'],
    'alternating_hv': [10,20,30,40,50,60,70,80,'hvcurveto'],
    'alternating_hv_optional': [10,20,30,40,5,'hvcurveto'],
    'alternating_vh': [10,20,30,40,50,60,70,80,'vhcurveto'],
    'alternating_vh_optional': [10,20,30,40,5,'vhcurveto'],
    'hflex': [10,20,30,40,50,60,70,'hflex'],
    'flex': [10,20,30,40,50,60,70,80,90,100,110,120,50,'flex'],
    'hflex1': [10,20,30,40,50,60,70,80,90,'hflex1'],
    'flex1_horizontal': [10,1,20,2,30,3,40,4,50,5,60,'flex1'],
    'flex1_vertical': [1,10,2,20,3,30,4,40,5,50,60,'flex1'],
    'local_and_global_subrs': [-107,'callsubr',-107,'callgsubr'],
}
vectors=[]
for name, operation in operations.items():
    program = [500,20,30,'rmoveto']+operation+['endchar']
    charstring=T2CharString(program=program,private=private,globalSubrs=global_subrs)
    bounds_pen=BoundsPen(None)
    charstring.draw(bounds_pen)
    pen=RecordingPen()
    charstring.draw(pen)
    charstring.compile()
    vectors.append({'name':name,'program':charstring.bytecode.hex(),'bounds':bounds_pen.bounds or [0,0,0,0],'width':charstring.width,'commands':pen.value})
for subr in private.Subrs+global_subrs:
    subr.compile()
(out/'cff-type2-vectors.json').write_text(json.dumps({'generator':'scripts/fixtures/build-cff-fonts.py','fonttools_version':__import__('fontTools').__version__,'local_subrs':[s.bytecode.hex() for s in private.Subrs],'global_subrs':[s.bytecode.hex() for s in global_subrs],'vectors':vectors},indent=2)+'\n')

# Independently authored CID-keyed counterpart, preserving glyph IDs and outlines.
from fontTools.cffLib import FDArrayIndex,FontDict,FDSelect
f=TTFont(path,recalcTimestamp=False)
cff=f['CFF '].cff
top=cff.topDictIndex[0]
fd=FontDict();fd.Private=top.Private
fdarray=FDArrayIndex();fdarray.append(fd)
select=FDSelect();select.format=0;select.gidArray=[0]*len(top.charset)
names=['.notdef']+[f'cid{i:05d}' for i in range(1,len(top.charset))]
top.CharStrings.charStrings={new:top.CharStrings.charStrings[old] for new,old in zip(names,top.charset)}
top.charset=names
top.ROS=('Adobe','Identity',0)
top.CIDCount=len(names)
top.FDArray=fdarray;top.FDSelect=select
top.CharStrings.fdArray=fdarray;top.CharStrings.fdSelect=select
del top.Private
if hasattr(top,'Encoding'): del top.Encoding
cid_path=out/'cff-static-cid.otf'
f.save(cid_path)
cid_face=TTFont(cid_path,recalcTimestamp=False)
manifest['cid_font_sha256']=hashlib.sha256(cid_path.read_bytes()).hexdigest()
manifest['cid_cff_sha256']=hashlib.sha256(cid_face.getTableData('CFF ')).hexdigest()
(out/'cff-static-sans.json').write_text(json.dumps(manifest,indent=2)+'\n')
