"""Pinned script-glyph browser reference. It never measures or positions layout."""
import argparse
from copy import deepcopy
import hashlib
import io
import json
from html.parser import HTMLParser
import re
from pathlib import Path

import fontTools
from fontTools import subset
from fontTools.ttLib import TTFont
from fontTools.ttLib.tables.otBase import OTTableWriter

assert fontTools.__version__ == "4.60.1"
ROOT = Path(__file__).resolve().parents[1]
SOURCE = ROOT / "priv/fonts/stix/STIXTwoMath-Regular.ttf"
SOURCE_SHA = "49b22767985171bce579307b21d98f4d35012fa39e316acc4ffcab8644dfb69b"
CORE_SHA = "f61ebea74ba54c489646e6969983ef977f448c26602edff75ec4e3ba77216373"
ASSETS = ROOT / "test/fixtures/html_to_pdf/browser_parity/assets"
FIXTURE = ROOT / "test/fixtures/html_to_pdf/browser_parity/mathml_fractions.html"
FAMILY = "Math Fraction Script Reference"
parser = argparse.ArgumentParser()
parser.add_argument("--check", action="store_true")
parser.add_argument("--core-source", type=Path, default=ROOT / "tmp/references/mathml-core.html")
args = parser.parse_args()
assert hashlib.sha256(SOURCE.read_bytes()).hexdigest() == SOURCE_SHA
assert hashlib.sha256(args.core_source.read_bytes()).hexdigest() == CORE_SHA
original = TTFont(SOURCE, recalcTimestamp=False)
font = TTFont(SOURCE, recalcTimestamp=False)
cmap = original.getBestCmap()
gsub = original["GSUB"].table
script = next(record.Script for record in gsub.ScriptList.ScriptRecord if record.ScriptTag == "math")
feature = next(gsub.FeatureList.FeatureRecord[index].Feature for index in script.DefaultLangSys.FeatureIndex
               if gsub.FeatureList.FeatureRecord[index].FeatureTag == "ssty")
alternates = gsub.LookupList.Lookup[feature.LookupListIndex[0]].SubTable[0].alternates
aliases = []
for level in [1, 2]:
    for character in "0123456789+":
        glyph = alternates[cmap[ord(character)]][level - 1]
        aliases.append({"character": character, "level": level, "alias": 0xE300 + len(aliases),
                        "source_gid": original.getGlyphID(glyph), "glyph": glyph,
                        "advance": original["hmtx"][glyph][0]})
for table in font["cmap"].tables:
    if table.isUnicode():
        for record in aliases:
            table.cmap[record["alias"]] = record["glyph"]
# Reproduce the prior alias-only face in memory from the immutable source. This
# transition proof never depends on an active or archived generated product.
prior_font = deepcopy(font)
for table in font["cmap"].tables:
    if table.isUnicode():
        # Keep original operator text and dictionary semantics.
        table.cmap[ord("+")] = alternates[cmap[ord("+")]][0]


def subset_face(face, unicodes):
    """Apply identical source subsetting and naming to both coverage variants."""
    for tag in ["GSUB", "GPOS"]:
        del face[tag]
    options = subset.Options()
    options.recalc_bounds = False
    options.recalc_timestamp = False
    subsetter = subset.Subsetter(options=options)
    subsetter.populate(unicodes=unicodes)
    subsetter.subset(face)
    for record in face["name"].names:
        if record.nameID in [1, 4, 6, 16]:
            name = FAMILY if record.nameID != 6 else "MathFractionScriptReference"
            record.string = name.encode(record.getEncoding())


subset_face(font, [record["alias"] for record in aliases] + [ord("+")])
subset_face(prior_font, [record["alias"] for record in aliases])
prior_output = io.BytesIO()
prior_font.save(prior_output, reorderTables=False)
assert hashlib.sha256(prior_output.getvalue()).hexdigest() == "837daf3822d0172ed2bdd8f8d647c200e39a25bd095d60427a214f9322b00871"
approved = TTFont(io.BytesIO(prior_output.getvalue()), recalcTimestamp=False)

for record in aliases:
    name = record["glyph"]
    before, after = original["glyf"][name], font["glyf"][name]
    assert before.getCoordinates(original["glyf"]) == after.getCoordinates(font["glyf"])
    assert before.program.getBytecode() == after.program.getBytecode()
    assert original["hmtx"][name] == font["hmtx"][name]
for tag in ["cvt ", "fpgm", "prep"]:
    assert font[tag].compile(font) == original[tag].compile(original)


def constants_bytes(face):
    writer = OTTableWriter(tableTag="MATH")
    face["MATH"].table.MathConstants.compile(writer, face)
    return writer.getAllData()


assert constants_bytes(font) == constants_bytes(original)

# Each selected glyph retains its own MATH information, including absence.
for field, coverage, values in [
    ("MathItalicsCorrectionInfo", "Coverage", "ItalicsCorrection"),
    ("MathTopAccentAttachment", "TopAccentCoverage", "TopAccentAttachment"),
    ("MathKernInfo", "MathKernCoverage", "MathKernInfoRecords")
]:
    before_info = getattr(original["MATH"].table.MathGlyphInfo, field)
    after_info = getattr(font["MATH"].table.MathGlyphInfo, field)
    before = {} if before_info is None else dict(zip(getattr(before_info, coverage).glyphs, getattr(before_info, values)))
    after = {} if after_info is None else dict(zip(getattr(after_info, coverage).glyphs, getattr(after_info, values)))
    for record in aliases:
        name = record["glyph"]
        assert (name in before) == (name in after)
        if name in before:
            before_writer, after_writer = OTTableWriter(tableTag="MATH"), OTTableWriter(tableTag="MATH")
            before[name].compile(before_writer, original)
            after[name].compile(after_writer, font)
            assert before_writer.getAllData() == after_writer.getAllData()
for tag, fields in [
    ("head", ["unitsPerEm", "xMin", "yMin", "xMax", "yMax"]),
    ("hhea", ["ascent", "descent", "lineGap"]),
    ("OS/2", ["sTypoAscender", "sTypoDescender", "sTypoLineGap", "usWinAscent", "usWinDescent", "sxHeight", "sCapHeight"])
]:
    for field in fields:
        assert getattr(font[tag], field) == getattr(original[tag], field)
output = io.BytesIO()
font.save(output, reorderTables=False)
reference_bytes = output.getvalue()
corrected = TTFont(io.BytesIO(reference_bytes), recalcTimestamp=False)
corrected_map = {cp: corrected.getGlyphID(name) for cp, name in corrected.getBestCmap().items()}
approved_map = {cp: approved.getGlyphID(name) for cp, name in approved.getBestCmap().items()}
assert {cp: gid for cp, gid in corrected_map.items() if cp != ord("+")} == approved_map
assert corrected_map[ord("+")] == approved_map[0xE30A]
assert corrected["maxp"].numGlyphs == approved["maxp"].numGlyphs
# Serialized glyph names may change when a Unicode alias is added to post format
# 3. Prove binary table equality and stable glyph IDs rather than comparing names.
assert set(corrected.reader.keys()) == set(approved.reader.keys())
for tag in corrected.reader.keys():
    if tag == "cmap":
        continue
    before_table, after_table = approved.reader[tag], corrected.reader[tag]
    if tag == "head":
        # SFNT checkSumAdjustment necessarily changes with the cmap table.
        before_table = before_table[:8] + bytes(4) + before_table[12:]
        after_table = after_table[:8] + bytes(4) + after_table[12:]
    if tag == "OS/2":
        # Adding U+002B updates only character coverage bookkeeping. No metric
        # changes: ulUnicodeRange1 gains Basic Latin; usFirstCharIndex becomes +.
        assert corrected["OS/2"].ulUnicodeRange1 == approved["OS/2"].ulUnicodeRange1 | 1
        assert corrected["OS/2"].usFirstCharIndex == ord("+")
        permitted = {45: 1, 64: 0, 65: ord("+")}
        assert {index: right for index, (left, right) in enumerate(zip(before_table, after_table)) if left != right} == permitted
        continue
    assert before_table == after_table, f"unexpected non-cmap delta in {tag}"



class SourceDepth(HTMLParser):
    """Core 3.3.2 UA depth defaults, then authored hints and inline overrides.

    This fixture has no selector-based math-depth/math-style overrides. HTML
    token descendants retain ordinary glyphs and are deliberately unmarked.
    No native prepared state, glyph outputs, measurement or browser data is read.
    """
    def __init__(self):
        super().__init__(convert_charrefs=True)
        self.stack = []
        self.tokens = {}
        self.markers = []
        self.operator_markers = []

    def handle_starttag(self, tag, attributes):
        if tag in ["area", "base", "br", "col", "embed", "hr", "img", "input", "link", "meta", "param", "source", "track", "wbr"]:
            return
        attributes = dict(attributes)
        parent = self.stack[-1] if self.stack else {"depth": 0, "style": "normal", "math": False, "tag": ""}
        mathematical = tag == "math" or parent["math"] and parent["tag"] not in ["mtext", "mi", "mn", "mo", "ms"]
        depth, style = parent["depth"], parent["style"]
        if tag == "math":
            depth, style = 0, "normal" if attributes.get("display") == "block" else "compact"
        elif mathematical and parent["tag"] == "mfrac":
            depth += int(parent["style"] == "compact")
            style = "compact"
        inherited_depth, inherited_style = parent["depth"], parent["style"]
        if "displaystyle" in attributes and attributes["displaystyle"].lower() in ["true", "false"]:
            style = "normal" if attributes["displaystyle"].lower() == "true" else "compact"
        if "scriptlevel" in attributes:
            value = attributes["scriptlevel"]
            depth = inherited_depth + int(value) if value.startswith(("+", "-")) else int(value)
        for declaration in attributes.get("style", "").split(";"):
            if ":" not in declaration:
                continue
            name, value = (part.strip().lower() for part in declaration.split(":", 1))
            if name == "math-style":
                assert value in ["normal", "compact"]
                style = value
            elif name == "math-depth":
                if value == "auto-add":
                    depth = inherited_depth + int(inherited_style == "compact")
                elif value.startswith("add("):
                    depth = inherited_depth + int(value[4:-1].strip())
                else:
                    depth = int(value)
        self.stack.append({"tag": tag, "math": mathematical, "depth": depth, "style": style,
                           "attributes": attributes, "text": "", "nested": False})
        if len(self.stack) > 1:
            self.stack[-2]["nested"] = True

    def handle_endtag(self, tag):
        assert self.stack and self.stack[-1]["tag"] == tag
        node = self.stack.pop()
        marker = node["attributes"].get("data-script-reference")
        if marker:
            assert node["math"] and tag in ["mn", "mo"] and not node["nested"]
            level = min(max(node["depth"], 0), 2)
            assert level in [1, 2]
            selected = [next(record for record in aliases if record["level"] == level and
                             record["character"] == character) for character in node["text"]]
            if tag == "mo":
                assert node["text"] == "+" and node["depth"] == 1
                self.operator_markers.append(marker)
                alias_text = "+"
            else:
                assert "+" not in node["text"]
                alias_text = "".join(chr(record["alias"]) for record in selected)
            self.tokens[marker] = {"text": node["text"], "computed_depth": node["depth"],
                                  "level": level, "alias_text": alias_text,
                                  "source_gids": [record["source_gid"] for record in selected],
                                  "advances": [record["advance"] for record in selected]}
            self.markers.append(marker)

    def handle_data(self, text):
        if self.stack:
            self.stack[-1]["text"] += text

    def handle_startendtag(self, tag, attributes):
        if tag in ["area", "base", "br", "col", "embed", "hr", "img", "input", "link", "meta", "param", "source", "track", "wbr"]:
            return
        self.handle_starttag(tag, attributes)
        self.handle_endtag(tag)


source_html = FIXTURE.read_text()
stylesheet = "\n".join(re.findall(r"<style[^>]*>(.*?)</style>", source_html, re.S | re.I))
assert not re.search(r"math-(?:depth|style)\s*:", stylesheet), "extend independent authored selector evaluation first"
assert 'math {font-family:"STIX Two Math"}' in stylesheet
depths = SourceDepth()
depths.feed(source_html)
assert len(depths.tokens) == len(set(depths.markers)) == 15
assert depths.operator_markers == ["large-op"]
assert depths.tokens["large-op"]["source_gids"] == [4283]
assert depths.tokens["large-op"]["computed_depth"] == 1
assert hashlib.sha256(reference_bytes).hexdigest() == "b1eed6017e8ac0976744ddb108c995747bb19d49bfb24fc51ee472922e6e0217"
manifest = {"source_sha256": SOURCE_SHA, "core_sha256": CORE_SHA,
            "reference_sha256": hashlib.sha256(reference_bytes).hexdigest(),
            "fonttools_version": fontTools.__version__, "family": FAMILY,
            "units_per_em": original["head"].unitsPerEm, "aliases": aliases,
            "tokens": depths.tokens, "layout_adjustments": []}
manifest_bytes = (json.dumps(manifest, ensure_ascii=True, indent=2) + "\n").encode()
assert hashlib.sha256(manifest_bytes).hexdigest() == "fedc0dcf1b34aa794b7991c8fb2c906ae243489863a29b59d873eff5974e7214"
products = {
    ASSETS / "math-fraction-script-reference.ttf": reference_bytes,
    ASSETS / "math-fraction-script-reference.json": manifest_bytes,
    ASSETS / "math-fraction-script-reference-LICENSE.txt": (ROOT / "priv/fonts/stix/OFL.txt").read_bytes()
}
for path, content in products.items():
    if args.check:
        assert path.read_bytes() == content, f"regenerate {path.relative_to(ROOT)}"
    else:
        path.write_bytes(content)
print(f"{'Checked' if args.check else 'Generated'} {len(aliases)} selected glyphs and {len(depths.tokens)} source-derived markers; "
      f"no layout adjustments; font SHA256 {manifest['reference_sha256']}")
