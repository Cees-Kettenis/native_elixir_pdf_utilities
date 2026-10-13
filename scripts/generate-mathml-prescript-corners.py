#!/usr/bin/env python3
"""Source-only left-corner witnesses from pinned Core and selected MATH font.

Reuses the source reader, rational arithmetic and vertical Core equations from
our existing source generator. Reads no renderer, PDF or browser output.
"""
import argparse
import hashlib
import importlib.util
import json
from pathlib import Path
import sys

ROOT = Path(__file__).resolve().parent.parent
OUTPUT = ROOT / "test/fixtures/mathml_prescript_corner_geometry.json"


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--core-source", type=Path, required=True)
    parser.add_argument("--check", action="store_true")
    args = parser.parse_args()
    spec = importlib.util.spec_from_file_location("source_geometry", ROOT / "scripts/generate-mathml-script-geometry.py")
    source = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(source)
    sys.argv = [str(spec.origin), "--check", "--core-source", str(args.core_source)]
    source.main()
    cases = []
    for size in (18, 20, 24):
        for char, italic in ((chr(0x1D434), False), (chr(0x1D44A), False)):
            tree = source.styles(source.node("msubsup", [source.token(char, italic), source.token("1"), source.token("2")]), source.Q(size))
            vertical = source.measure(tree)
            base, sub, sup = [source.selected(child) for child in tree["children"]]
            down = -vertical["glyphs"][1]["y"]
            up = vertical["glyphs"][2]["y"]
            left = lambda child, shift, lower: min(
                source.corner_value(base["glyphs"][0], "BottomLeftMathKern" if lower else "TopLeftMathKern", height) +
                source.corner_value(child["glyphs"][0], "TopRightMathKern" if lower else "BottomRightMathKern", height - shift)
                for height in ([shift + child["ink_ascent"], -base["ink_descent"]] if lower else [shift - child["ink_descent"], base["ink_ascent"]]))
            lower_kern = left(sub, -down, True)
            upper_kern = left(sup, up, False)
            spacing = source.Q(source.C.SpaceAfterScript.Value) * source.Q(size) / source.UNITS
            advance = max(sub["width"] + lower_kern, sup["width"] + upper_kern) + spacing
            cases.append({"name": f"left-{char}-{size}", "base": tree["children"][0], "sub": tree["children"][1], "sup": tree["children"][2],
                          "expected": {"glyph_ids": [b["glyphs"][0]["glyph_id"] for b in (base, sub, sup)],
                                       "lower_kern": lower_kern, "upper_kern": upper_kern,
                                       "width": advance + base["width"],
                                       "offsets": [[advance, 0], [advance, 0], [advance - sub["width"] - lower_kern, -down], [advance - sup["width"] - upper_kern, up]]}})
    package = {"scope": "selected-font left-corner source geometry only; no browser reference",
               "core_sha256": source.CORE_SHA, "font_sha256": source.FONT_SHA,
               "fonttools": source.fontTools.__version__, "opentype_source": source.OPENTYPE_MATH_URL,
               "source_math_sha256": source.sha(source.F.getTableData("MATH")), "cases": cases}
    data = json.dumps(source.serialize(package), indent=2, sort_keys=True, ensure_ascii=False) + "\n"
    if args.check:
        assert OUTPUT.read_text() == data
    else:
        OUTPUT.write_text(data)
    print(f"source-only prescript witnesses={len(cases)} sha256={hashlib.sha256(data.encode()).hexdigest()}")


if __name__ == "__main__":
    main()
