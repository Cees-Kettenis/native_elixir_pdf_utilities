"""Build small, licensed font container fixtures from bundled DejaVu 2.37.

Run with Python and fonttools 4.66.1. No network access is used by this script.
The source fonts and their license are already checked in under priv/fonts/dejavu.
"""
import hashlib
import json
from pathlib import Path
import shutil

import fontTools
from fontTools import subset
from fontTools.ttLib import TTFont, TTCollection

root = Path(__file__).resolve().parents[2]
out = root / "test/fixtures/fonts"
manifest = {"generator": "scripts/fixtures/build-font-containers.py", "fonttools": fontTools.__version__, "license": "selected-container-LICENSE.txt", "sources": {}, "faces": [], "files": {}}
fonts = []
for suffix, source in [("sans", "DejaVuSans.ttf"), ("mono", "DejaVuSansMono.ttf")]:
    source_path = root / "priv/fonts/dejavu" / source
    manifest["sources"][source] = hashlib.sha256(source_path.read_bytes()).hexdigest()
    font = TTFont(source_path, recalcTimestamp=False)
    options = subset.Options()
    options.recalc_timestamp = False
    options.drop_tables.append("FFTM")
    options.name_IDs = [1, 2, 4, 6]
    reducer = subset.Subsetter(options=options)
    reducer.populate(text=" Hx0")
    reducer.subset(font)
    family = f"Container {suffix.title()}"
    for name in font["name"].names:
        value = {1: family, 2: "Regular", 4: family, 6: family.replace(" ", "")}.get(name.nameID)
        if value:
            name.string = value.encode(name.getEncoding())
    path = out / f"selected-container-{suffix}.ttf"
    font.save(path)
    font = TTFont(path, recalcTimestamp=False)
    fonts.append(font)
    manifest["faces"].append({"file": path.name, "glyph_order": font.getGlyphOrder(), "metrics": {str(code): list(font["hmtx"][glyph]) for code, glyph in font.getBestCmap().items()}, "tables": {tag: hashlib.sha256(font.getTableData(tag)).hexdigest() for tag in font.keys() if tag != "GlyphOrder" and tag != "head"}})
woff = TTFont(out / "selected-container-sans.ttf", recalcTimestamp=False)
woff.flavor = "woff"
woff.save(out / "selected-container.woff")
# Decode using fontTools independently of the production reconstruction path.
woff = TTFont(out / "selected-container.woff", recalcTimestamp=False)
for tag in woff.keys():
    if tag not in ("GlyphOrder", "head"):
        assert hashlib.sha256(woff.getTableData(tag)).hexdigest() == manifest["faces"][0]["tables"][tag]
collection = TTCollection()
collection.fonts = fonts
collection.save(out / "selected-container.ttc")
shutil.copyfile(root / "priv/fonts/dejavu/LICENSE.txt", out / manifest["license"])
for name in ["selected-container-sans.ttf", "selected-container-mono.ttf", "selected-container.woff", "selected-container.ttc"]:
    manifest["files"][name] = hashlib.sha256((out / name).read_bytes()).hexdigest()
(out / "selected-containers.json").write_text(json.dumps(manifest, indent=2) + "\n")
