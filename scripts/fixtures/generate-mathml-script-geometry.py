#!/usr/bin/env python3
"""Independent fixed M5 geometry witnesses from pinned Core and font bytes.

This program reads no library, beams, native boxes, PDF or browser measurements.
It creates test expectations, not browser artwork or an approved reference.
"""
import argparse
import hashlib
import json
from fractions import Fraction as Q
from pathlib import Path

import fontTools
from fontTools.ttLib import TTFont

ROOT = Path(__file__).resolve().parents[2]
FONT = ROOT / "priv/fonts/stix/STIXTwoMath-Regular.ttf"
OUTPUT = ROOT / "test/fixtures/mathml_scripts_geometry.json"
OPENTYPE_MATH_URL = "https://learn.microsoft.com/en-us/typography/opentype/spec/math#mathkerninfo-table"
CORE_SHA = "f61ebea74ba54c489646e6969983ef977f448c26602edff75ec4e3ba77216373"
FONT_SHA = "49b22767985171bce579307b21d98f4d35012fa39e316acc4ffcab8644dfb69b"


def sha(data):
    return hashlib.sha256(data).hexdigest()


def node(tag, children=None, text=None, **attributes):
    result = {"tag": tag, "attributes": attributes, "children": children or []}
    if text is not None:
        result["text"] = text
    return result


def token(text, italic=False):
    return node("mi" if italic else "mn", text=text)


def styles(tree, size, depth=0, shift="normal", mode="compact"):
    attrs = tree["attributes"]
    size = Q(attrs.get("size", size))
    depth = int(attrs.get("depth", depth))
    shift = attrs.get("shift", shift)
    mode = attrs.get("mode", mode)
    tree["style"] = {"font_size": size, "math_depth": depth,
                     "math_shift": shift, "math_style": mode}
    for index, child in enumerate(tree["children"]):
        child_size, child_depth, child_shift, child_mode = size, depth, shift, mode
        if index:
            child_depth += 1
            child_mode = "compact"
            if depth == 0:
                child_size *= Q(C.ScriptPercentScaleDown, 100)
            elif depth == 1:
                child_size *= Q(C.ScriptScriptPercentScaleDown, C.ScriptPercentScaleDown)
            else:
                child_size *= Q(71, 100)
            if (tree["tag"] in ("munder", "munderover") and index == 1
                    and attrs.get("accentunder", "").lower() == "true") or (
                    tree["tag"] in ("mover", "munderover")
                    and index == len(tree["children"]) - 1
                    and attrs.get("accent", "").lower() == "true"):
                child_size = size
            if tree["tag"] in ("msub", "msubsup") and index == 1:
                child_shift = "compact"
        elif tree["tag"] in ("mover", "munderover") and attrs.get("accent", "").lower() == "true":
            child_shift = "compact"
        styles(child, child_size, child_depth, child_shift, child_mode)
    return tree


def glyph(name, original, size):
    outline = F["glyf"][name]
    xmin, ymin, xmax, ymax = (getattr(outline, key, 0) for key in ("xMin", "yMin", "xMax", "yMax"))
    scale = size / UNITS
    italic = ITALICS.get(name, 0) * scale
    attachment = ACCENTS.get(name)
    advance = F["hmtx"][name][0] * scale
    return {"width": advance, "min_content_width": advance, "max_content_width": advance,
            "ascent": ymax * scale, "descent": -ymin * scale,
            "ink_ascent": ymax * scale, "ink_descent": -ymin * scale,
            "italic": italic, "accent": attachment * scale if attachment is not None else None,
            "glyphs": [{"glyph_id": IDS[name], "font_size": size, "width": advance,
                        "x": Q(0), "y": Q(0), "text": original}], "text": original}


def stretch(name, text, size, target):
    base = glyph(name, text, size)
    if base["width"] >= target:
        return base
    construction = HORIZONTAL[name]
    for record in construction.MathGlyphVariantRecord:
        if Q(record.AdvanceMeasurement) * size / UNITS >= target:
            result = glyph(record.VariantGlyph, text, size)
            result["min_content_width"] = result["max_content_width"] = base["width"]
            return result
    assembly = construction.GlyphAssembly
    assert assembly is not None
    parts = assembly.PartRecords
    minimum = Q(MV.MinConnectorOverlap) * size / UNITS
    fixed = sum(Q(p.FullAdvance) * size / UNITS for p in parts if not p.PartFlags & 1)
    growing = sum(Q(p.FullAdvance) * size / UNITS for p in parts if p.PartFlags & 1)
    extender_count = sum(bool(p.PartFlags & 1) for p in parts)
    fixed_count = len(parts) - extender_count
    growth = growing - minimum * extender_count
    required = (target - fixed + minimum * (fixed_count - 1)) / growth
    repeat = max(0, -(-required.numerator // required.denominator))
    expanded = [p for p in parts for _ in range(repeat if p.PartFlags & 1 else 1)]
    # OpenType actual joins, retaining the accepted dated-Core disagreement.
    joins = [Q(p.StartConnectorLength) * size / UNITS for i, p in enumerate(parts) if i or p.PartFlags & 1]
    joins += [Q(p.EndConnectorLength) * size / UNITS for i, p in enumerate(parts) if i != len(parts) - 1 or p.PartFlags & 1]
    assert joins and min(joins) >= minimum
    total = sum(Q(p.FullAdvance) * size / UNITS for p in expanded)
    overlap = min(min(joins), max(minimum, (total - target) / (len(expanded) - 1)))
    result = dict(base)
    metric_parts = [glyph(p.glyph, text, size) for p in parts]
    result.update(width=total - overlap * (len(expanded) - 1),
                  ascent=max(p["ascent"] for p in metric_parts), descent=max(p["descent"] for p in metric_parts),
                  italic=Q(assembly.ItalicsCorrection.Value) * size / UNITS, accent=None, glyphs=[])
    result["ink_ascent"], result["ink_descent"] = result["ascent"], result["descent"]
    offset = Q(0)
    for part in expanded:
        painted = glyph(part.glyph, text, size)["glyphs"][0]
        painted["x"] = offset
        result["glyphs"].append(painted)
        offset += Q(part.FullAdvance) * size / UNITS - overlap
    return result


def selected(tree, target=None):
    text = tree["text"]
    code = {"x": 0x1D465, "f": 0x1D453, "n": 0x1D45B, "i": 0x1D456}.get(text, ord(text)) if tree["tag"] == "mi" else ord(text)
    name = F.getBestCmap()[code]
    depth, size = tree["style"]["math_depth"], tree["style"]["font_size"]
    if depth > 0:
        for lookup in SSTY:
            if lookup.LookupType == 1:
                name = lookup.mapping.get(name, name)
            else:
                alternatives = lookup.alternates.get(name, [])
                level = min(depth, 2) - 1
                if level < len(alternatives):
                    name = alternatives[level]
    if tree["tag"] == "mo" and text == "∑" and tree["style"]["math_style"] == "normal":
        variants = VERTICAL[name].MathGlyphVariantRecord
        name = next((r.VariantGlyph for r in variants if r.AdvanceMeasurement >= C.DisplayOperatorMinHeight), variants[-1].VariantGlyph)
    if target is not None and name in HORIZONTAL:
        return stretch(name, text, size, target)
    return glyph(name, text, size)


def corner_value(painted, corner, height):
    if painted is None:
        return Q(0)
    record = KERNS.get(F.getGlyphOrder()[painted["glyph_id"]])
    table = getattr(record, corner, None) if record else None
    if table is None:
        return Q(0)
    scale = painted["font_size"] / UNITS
    heights = table.CorrectionHeight
    values = table.KernValue
    assert all(getattr(v, "DeviceTable", None) is None for v in heights + values)
    height -= painted["y"]
    index = sum(height >= Q(v.Value) * scale for v in heights)
    return Q(values[index].Value) * scale


def pair_kern(base, script, shift, lower):
    if script is None:
        return Q(0)
    b = base["glyphs"][0] if len(base["glyphs"]) == 1 and base["width"] == base["glyphs"][0]["width"] else None
    s = script["glyphs"][0] if len(script["glyphs"]) == 1 and script["width"] == script["glyphs"][0]["width"] else None
    heights = [shift + script["ink_ascent"], -base["ink_descent"]] if lower else [shift - script["ink_descent"], base["ink_ascent"]]
    bc, sc = ("BottomRightMathKern", "TopLeftMathKern") if lower else ("TopRightMathKern", "BottomLeftMathKern")
    return min(corner_value(b, bc, h) + corner_value(s, sc, h - shift) for h in heights)


def measure(tree):
    tag, attrs, style = tree["tag"], tree["attributes"], tree["style"]
    size = style["font_size"]
    if "text" in tree:
        return selected(tree)
    if tag == "mspace":
        width, ascent, descent = (Q(attrs.get(k, 0)) for k in ("width", "height", "depth"))
        return {"width": width, "min_content_width": width, "max_content_width": width,
                "ascent": ascent, "descent": descent, "ink_ascent": ascent,
                "ink_descent": descent, "italic": Q(0), "accent": None, "glyphs": [], "text": ""}
    trees = tree["children"]
    base_tree = trees[0]
    large = base_tree.get("text") == "∑"
    horizontal_base = base_tree["tag"] == "mo" and base_tree.get("text") == "⏞"
    side = tag in ("msub", "msup", "msubsup") or (large and style["math_style"] == "compact")
    lower = 1 if tag in ("msub", "msubsup", "munder", "munderover") else None
    upper = 1 if tag in ("msup", "mover") else 2 if tag in ("msubsup", "munderover") else None
    boxes = [measure(child) for child in trees]
    if not side:
        pending = [i for i, child in enumerate(trees) if child["tag"] == "mo" and child.get("text") in ("⏞", "^", "_")]
        ordinary = [box for i, box in enumerate(boxes) if i not in pending]
        target = max(b["width"] for b in ordinary or boxes)
        for i in pending:
            boxes[i] = selected(trees[i], target)
    base = boxes[0]
    sub, sup = boxes[lower] if lower else None, boxes[upper] if upper else None
    value = lambda name: Q(getattr(C, name).Value) * size / UNITS
    large_italic = base["italic"] if large else Q(0)
    italic = Q(0) if large else base["italic"]
    extra_under = extra_over = Q(0)
    if side:
        down = max(value("SubscriptShiftDown"), sub["ink_ascent"] - value("SubscriptTopMax"), value("SubscriptBaselineDropMin") + base["ink_descent"]) if sub else Q(0)
        up = max(value("SuperscriptShiftUpCramped" if style["math_shift"] == "compact" else "SuperscriptShiftUp"), value("SuperscriptBottomMin") + sup["ink_descent"], base["ink_ascent"] - value("SuperscriptBaselineDropMax")) if sup else Q(0)
        if sub and sup:
            deficit = max(Q(0), value("SubSuperscriptGapMin") - (down - sub["ink_ascent"] + up - sup["ink_descent"]))
            transfer = min(deficit, max(Q(0), value("SuperscriptBottomMaxWithSubscript") - (up - sup["ink_descent"])))
            up += transfer
            down += deficit - transfer
    else:
        down = up = Q(0)
        if sub:
            if large:
                down = max(value("LowerLimitBaselineDropMin"), value("LowerLimitGapMin") + sub["ascent"])
            elif horizontal_base:
                down = max(value("StretchStackBottomShiftDown"), value("StretchStackGapAboveMin") + sub["ascent"])
            else:
                down = Q(0) if attrs.get("accentunder", "").lower() == "true" else value("UnderbarVerticalGap")
                extra_under = value("UnderbarExtraDescender")
        if sup:
            if large:
                up = max(value("UpperLimitBaselineRiseMin"), value("UpperLimitGapMin") + sup["descent"])
            elif horizontal_base:
                up = max(value("StretchStackTopShiftUp"), value("StretchStackGapBelowMin") + sup["descent"])
            else:
                up = max(Q(0), value("AccentBaseHeight") - base["ascent"]) if attrs.get("accent", "").lower() == "true" else value("OverbarVerticalGap")
                extra_over = value("OverbarExtraAscender")
    sub_kern = pair_kern(base, sub, -down, True) if side and CORNER_KERNS else Q(0)
    sup_kern = pair_kern(base, sup, up, False) if side and CORNER_KERNS else Q(0)
    widths = {}
    for field in ("width", "min_content_width", "max_content_width"):
        if side:
            choices = []
            if sub:
                choices.append(base[field] - large_italic + sub_kern + sub[field] + value("SpaceAfterScript"))
            if sup:
                choices.append(base[field] + italic + sup_kern + sup[field] + value("SpaceAfterScript"))
            widths[field] = max(choices)
            minimum = Q(0)
        else:
            attachment = sup["accent"] if sup and sup["accent"] is not None else sup[field] / 2 if sup else Q(0)
            left, right = [-base[field] / 2], [base[field] / 2]
            if sub:
                left.append(-sub[field] / 2 - large_italic / 2)
                right.append(sub[field] / 2 - large_italic / 2)
            if sup:
                left.append(-attachment + large_italic / 2)
                right.append(sup[field] - attachment + large_italic / 2)
            widths[field] = abs(max(right) - min(left))
            if field == "width":
                minimum = min(left)
    offsets = []
    for index, box in enumerate(boxes):
        if side:
            offset = (Q(0), Q(0)) if index == 0 else (base["width"] - large_italic + sub_kern, -down) if index == lower else (base["width"] + italic + sup_kern, up)
        else:
            offset = (-minimum - base["width"] / 2, Q(0)) if index == 0 else (-minimum - box["width"] / 2 - large_italic / 2, -base["ink_descent"] - down) if index == lower else (-minimum - box["width"] / 2 + large_italic / 2, base["ink_ascent"] + up)
        offsets.append(offset)
    result = dict(widths, italic=Q(0), accent=None, text="".join(box["text"] for box in boxes), glyphs=[])
    result["ascent"] = max([base["ascent"]] + ([sub["ascent"] - down] if sub else []) + ([sup["ascent"] + up + extra_over] if sup else []))
    result["descent"] = max([base["descent"]] + ([sub["descent"] + down + extra_under] if sub else []) + ([sup["descent"] - up] if sup else []))
    result["ink_ascent"] = max(box["ink_ascent"] + y for box, (x, y) in zip(boxes, offsets))
    result["ink_descent"] = max(box["ink_descent"] - y for box, (x, y) in zip(boxes, offsets))
    for box, (x, y) in zip(boxes, offsets):
        for painted in box["glyphs"]:
            result["glyphs"].append(dict(painted, x=painted["x"] + x, y=painted["y"] + y))
    return result


def serialize(value):
    if isinstance(value, Q):
        return {"numerator": value.numerator, "denominator": value.denominator}
    if isinstance(value, dict):
        return {k: serialize(v) for k, v in value.items()}
    if isinstance(value, list):
        return [serialize(v) for v in value]
    return value


def main():
    global F, C, MV, UNITS, IDS, ITALICS, ACCENTS, HORIZONTAL, VERTICAL, SSTY, KERNS, CORNER_KERNS
    parser = argparse.ArgumentParser()
    parser.add_argument("--core-source", type=Path, default=ROOT / "tmp/references/mathml-core.html")
    parser.add_argument("--check", action="store_true")
    parser.add_argument("--opentype-corner-kerns", action="store_true")
    args = parser.parse_args()
    CORNER_KERNS = args.opentype_corner_kerns
    output = ROOT / "test/fixtures/mathml_script_corner_geometry.json" if CORNER_KERNS else OUTPUT
    assert fontTools.__version__ == "4.60.1"
    assert sha(args.core_source.read_bytes()) == CORE_SHA
    assert sha(FONT.read_bytes()) == FONT_SHA
    F = TTFont(FONT, recalcTimestamp=False)
    C, MV = F["MATH"].table.MathConstants, F["MATH"].table.MathVariants
    UNITS = Q(F["head"].unitsPerEm)
    IDS = {name: i for i, name in enumerate(F.getGlyphOrder())}
    info = F["MATH"].table.MathGlyphInfo
    KERNS = dict(zip(info.MathKernInfo.MathKernCoverage.glyphs, info.MathKernInfo.MathKernInfoRecords))
    ITALICS = dict(zip(info.MathItalicsCorrectionInfo.Coverage.glyphs, (r.Value for r in info.MathItalicsCorrectionInfo.ItalicsCorrection)))
    ACCENTS = dict(zip(info.MathTopAccentAttachment.TopAccentCoverage.glyphs, (r.Value for r in info.MathTopAccentAttachment.TopAccentAttachment)))
    HORIZONTAL = dict(zip(MV.HorizGlyphCoverage.glyphs, MV.HorizGlyphConstruction))
    VERTICAL = dict(zip(MV.VertGlyphCoverage.glyphs, MV.VertGlyphConstruction))
    gsub = F["GSUB"].table
    math = next(r.Script for r in gsub.ScriptList.ScriptRecord if r.ScriptTag == "math")
    features = [gsub.FeatureList.FeatureRecord[i] for i in math.DefaultLangSys.FeatureIndex]
    indices = [i for feature in features if feature.FeatureTag == "ssty" for i in feature.Feature.LookupListIndex]
    SSTY = [sub for i in indices for sub in gsub.LookupList.Lookup[i].SubTable]
    # The selected font's ssty uses alternates. Assert the source lookup kind.
    assert all(gsub.LookupList.Lookup[i].LookupType == 3 for i in indices)
    for sub in SSTY:
        sub.LookupType = 3
    cases = []
    for size in (12, 18, 24):
        for tag, children in (("msub", [token("x", True), token("1")]),
                              ("msup", [token("f", True), token("2")]),
                              ("msubsup", [token("x", True), token("1"), token("2")])):
            cases.append((f"{tag}-{size}", styles(node(tag, children), Q(size))))
    for tag, attrs in (("munder", {}), ("mover", {}), ("munderover", {}),
                       ("munder", {"accentunder": "true"}), ("mover", {"accent": "true"})):
        children = [token("7"), token("3")] + ([token("2")] if tag == "munderover" else [])
        cases.append((f"{tag}-{'accent' if attrs else 'normal'}", styles(node(tag, children, **attrs), Q(18))))
    for mode in ("normal", "compact"):
        cases.append((f"sum-{mode}", styles(node("munderover", [node("mo", text="∑"), token("1"), token("n", True)]), Q(18), mode=mode)))
    cases.append(("nested", styles(node("msup", [node("msub", [token("x", True), token("i", True)]), node("msup", [token("2"), token("3")])]), Q(18))))
    cases.append(("cramped", styles(node("msup", [token("f", True), token("2")], shift="compact"), Q(24))))
    for width in (30, 90):
        cases.append((f"brace-{width}", styles(node("mover", [node("mspace", width=str(width), height="12"), node("mo", text="⏞")], accent="true"), Q(18))))
    cases.append(("stretch-stack", styles(node("munderover", [node("mo", text="⏞"), token("1"), token("2")]), Q(18))))
    if CORNER_KERNS:
        for tag in ("msub", "msup", "msubsup"):
            for char, italic in (("f", True), ("x", False)):
                children = [token(char, italic), token("1")] + ([token("2")] if tag == "msubsup" else [])
                cases.append((f"corner-{tag}-{char}-{'italic' if italic else 'latin'}", styles(node(tag, children), Q(20))))
    package = {"fonttools": fontTools.__version__, "core_sha256": CORE_SHA, "font_sha256": FONT_SHA,
               "source_tables": {t: sha(F.getTableData(t)) for t in ("MATH", "GSUB", "GPOS", "glyf", "hmtx")},
               "scope": "fixed source geometry witnesses, no browser reference approval",
               "dated_ambiguities": ["limit line extents versus translated ink", "overscript half-width offset versus attachment width", "actual assembly joins"],
               "cases": [{"name": name, "tree": tree, "expected": measure(tree)} for name, tree in cases]}
    if CORNER_KERNS:
        package["opentype_corner_source"] = OPENTYPE_MATH_URL
        package["scope"] = "Core vertical metrics with selected-font OpenType corner spacing; source-only, no browser reference approval"
    data = json.dumps(serialize(package), indent=2, sort_keys=True, ensure_ascii=False) + "\n"
    if args.check:
        assert output.read_text() == data
    else:
        output.write_text(data)
    print(f"source-only geometry witnesses={len(cases)} sha256={sha(data.encode())}")


if __name__ == "__main__":
    main()
