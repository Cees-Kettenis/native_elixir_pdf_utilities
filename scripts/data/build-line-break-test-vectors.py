#!/usr/bin/env python3
"""Select Unicode 17 line-break rule/property regressions from a local UCD file."""
import hashlib
import json
from pathlib import Path
import re
import sys

raw = Path(sys.argv[1]).read_bytes()
vectors, seen = [], set()
for number, line in enumerate(raw.decode().splitlines(), 1):
    body, _, comment = line.partition('#')
    parts = body.split()
    if not parts:
        continue
    markers = set(re.findall(r'\[([0-9.]+)\]|\(([^()]+)\)', comment))
    points = parts[1::2]
    if markers <= seen and not (len(points) > 4 and number % 7 == 0):
        continue
    seen |= markers
    offset, breaks = 0, []
    for separator, point in zip(parts[::2], points):
        if separator == '÷':
            breaks.append(offset)
        offset += len(chr(int(point, 16)).encode())
    vectors.append({'line': number, 'points': points, 'breaks': breaks + [offset]})

output = {
    'unicode': '17.0.0',
    'source': 'https://www.unicode.org/Public/17.0.0/ucd/auxiliary/LineBreakTest.txt',
    'sha256': hashlib.sha256(raw).hexdigest(),
    'license': 'priv/licenses/unicode-character-properties-UNICODE-3.0.txt',
    'vectors': vectors,
}
Path('test/fixtures/html_to_pdf/typography/line_break_vectors.json').write_text(
    json.dumps(output, ensure_ascii=False, separators=(',', ':')) + '\n')
print(f'{len(vectors)} vectors')
