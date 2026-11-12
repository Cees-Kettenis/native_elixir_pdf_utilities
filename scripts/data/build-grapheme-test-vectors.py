#!/usr/bin/env python3
"""Retain the complete Unicode 17 grapheme conformance corpus from a local file."""
import hashlib
import json
from pathlib import Path
import sys

raw = Path(sys.argv[1]).read_bytes()
vectors = []
for number, line in enumerate(raw.decode().splitlines(), 1):
    parts = line.partition('#')[0].split()
    if not parts:
        continue
    offset, breaks = 0, []
    points = parts[1::2]
    for separator, point in zip(parts[::2], points):
        if separator == '÷':
            breaks.append(offset)
        offset += len(chr(int(point, 16)).encode())
    vectors.append({'line': number, 'points': points, 'breaks': breaks + [offset]})

output = {
    'unicode': '17.0.0',
    'source': 'https://www.unicode.org/Public/17.0.0/ucd/auxiliary/GraphemeBreakTest.txt',
    'sha256': hashlib.sha256(raw).hexdigest(),
    'license': 'priv/licenses/unicode-character-properties-UNICODE-3.0.txt',
    'vectors': vectors,
}
Path('test/fixtures/html_to_pdf/typography/grapheme_break_vectors.json').write_text(
    json.dumps(output, ensure_ascii=False, separators=(',', ':')) + '\n')
print(f'{len(vectors)} vectors')
