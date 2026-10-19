#!/usr/bin/env python3
"""Regenerate the bundled STIX token device metrics using system FreeType."""
from pathlib import Path
import hashlib
import json
import shlex
import struct
import subprocess
import tempfile
import zlib

ROOT = Path(__file__).resolve().parents[3]
FONT = ROOT / 'priv/fonts/stix/STIXTwoMath-Regular.ttf'
DESTINATION = FONT.with_name('token-ink-metrics.bin')

with tempfile.TemporaryDirectory(prefix='math-token-metrics-') as directory:
    executable = Path(directory) / 'generate'
    raw = Path(directory) / 'metrics.bin'
    flags = shlex.split(subprocess.check_output(
        ['pkg-config', '--cflags', '--libs', 'freetype2'], text=True))
    subprocess.run(['cc', '-O2', str(Path(__file__).with_name('generate.c')),
                    *flags, '-lm', '-o', str(executable)], check=True)
    result = subprocess.run([str(executable), str(FONT), str(raw)],
                            check=True, stderr=subprocess.PIPE, text=True)
    data = raw.read_bytes()
    glyph_count, maximum_size = struct.unpack_from('>HH', data)
    fingerprint = hashlib.sha256(FONT.read_bytes()).digest()
    output = zlib.compress(fingerprint + data, 9)
    DESTINATION.write_bytes(output)
    metadata = {
        'font': FONT.name,
        'font_sha256': fingerprint.hex(),
        'generator': 'scripts/data/math-token-metrics/generate.py',
        'freetype': result.stderr.splitlines()[0],
        'load_flags': ['FT_LOAD_TARGET_LIGHT', 'FT_LOAD_NO_BITMAP'],
        'glyph_count': glyph_count,
        'minimum_css_px': 1,
        'maximum_css_px': maximum_size / 64,
        'size_step_css_px': '1/64',
        'rounding': 'outward integer CSS-pixel bounds of the hinted outline control box',
        'sha256': hashlib.sha256(output).hexdigest(),
        'bytes': len(output),
    }
    DESTINATION.with_suffix('.json').write_text(json.dumps(metadata, indent=2) + '\n')
    print(json.dumps(metadata, indent=2))
