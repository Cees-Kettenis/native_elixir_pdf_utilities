"""Rebuild the variable-font and WOFF2 references with fontTools 4.66.1 and Brotli 1.2.0."""
import argparse
import hashlib
import io
import json
from pathlib import Path

import brotli
import fontTools
from fontTools.pens.recordingPen import RecordingPen
from fontTools.ttLib import TTFont, woff2
from fontTools.varLib.instancer import instantiateVariableFont


fixtures = Path(__file__).resolve().parents[2] / "test/fixtures/fonts"


def digest(data):
    return hashlib.sha256(data).hexdigest()


def write_manifest(path, data):
    path.write_text(json.dumps(data, indent=2) + "\n", encoding="utf-8")


def outline(font, name):
    table = font["glyf"]
    glyph = table[name]
    points, ends, flags = glyph.getCoordinates(table)
    return {
        "points": list(points),
        "ends": list(ends),
        "flags": list(flags),
        "bounds": [getattr(glyph, field, 0) for field in ("xMin", "yMin", "xMax", "yMax")],
    }


def variable_references(out, filename, manifest):
    source = fixtures / filename
    font = TTFont(source, recalcTimestamp=False)
    instances = []
    for weight, width in [(100, 100), (900, 100), (650, 112.5), (400, 75)]:
        location = {"wght": weight, "wdth": width}
        instance = instantiateVariableFont(font, location)
        # Saving rounds TrueType point coordinates to their encoded integers.
        encoded = io.BytesIO()
        instance.save(encoded)
        encoded.seek(0)
        face = TTFont(encoded)
        glyphs = {}
        reference = {"coordinates": location, "glyphs": glyphs}
        if "glyf" in face:
            reference.update(ascent=face["hhea"].ascent, x_height=face["OS/2"].sxHeight)
        else:
            # Draw from the variable CFF2 program to retain fractional blend results.
            blended = font.getGlyphSet(location=location)
        for index, name in enumerate(face.getGlyphOrder()):
            if "glyf" in face:
                glyph = outline(face, name)
            else:
                pen = RecordingPen()
                blended[name].draw(pen)
                glyph = {"commands": pen.value}
            glyph["metrics"] = face["hmtx"][name]
            glyphs[str(index)] = glyph
        instances.append(reference)
    write_manifest(out / manifest, {
        "fonttools": fontTools.__version__,
        "source_sha256": digest(source.read_bytes()),
        "instances": instances,
    })


def woff_references(out):
    files = {}
    for filename, original, transforms in [
        ("selected-transformed.woff2", "selected-container-sans.ttf", ["glyf", "loca", "hmtx"]),
        ("selected-literal.woff2", "selected-container-mono.ttf", []),
        ("selected-cff.woff2", "cff-static-sans.otf", []),
    ]:
        source = fixtures / original
        woff2.compress(source, out / filename, transform_tables=transforms)
        face = TTFont(out / filename, recalcTimestamp=False)
        files[filename] = {
            "source": original,
            "source_sha256": digest(source.read_bytes()),
            "tables": {
                tag: digest(face.getTableData(tag)) for tag in face.keys()
                if tag not in {"GlyphOrder", "head", "glyf", "loca"}
            },
            "glyphs": {
                str(index): outline(face, name) for index, name in enumerate(face.getGlyphOrder())
            } if "glyf" in face else {},
        }
    # The three-face collection repeats Sans. Its literal directory shares
    # identical records except for the glyf/loca pair owned by each face.
    records = []
    indices = []
    for original in ("selected-container-sans.ttf", "selected-container-mono.ttf"):
        source = TTFont(fixtures / original)
        face_indices = []
        for tag in sorted(source.reader.keys()):
            record = (tag, source.reader[tag])
            if tag in {"glyf", "loca"} or record not in records:
                index = len(records)
                records.append(record)
            else:
                index = records.index(record)
            face_indices.append(index)
        indices.append(face_indices)
    sans, mono = files["selected-transformed.woff2"]["tables"], files["selected-literal.woff2"]["tables"]
    collection = {
        "file": "selected-collection.woff2",
        "tables": [sans, mono, sans],
        "indices": [indices[0], indices[1], indices[0]],
    }
    write_manifest(out / "woff2-fonts.json", {
        "fonttools": fontTools.__version__, "files": files, "collection": collection,
    })


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("output", type=Path, help="directory for reconstructed references")
    args = parser.parse_args()
    if (fontTools.__version__, brotli.__version__) != ("4.66.1", "1.2.0"):
        parser.error("use fontTools 4.66.1 and Brotli 1.2.0 for byte-identical references")
    args.output.mkdir(parents=True, exist_ok=True)
    variable_references(args.output, "selected-variable.ttf", "variable-fonts.json")
    variable_references(args.output, "selected-variable-cff2.otf", "variable-cff2-fonts.json")
    woff_references(args.output)
