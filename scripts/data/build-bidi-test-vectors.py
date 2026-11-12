#!/usr/bin/env python3
"""Select compact Unicode 17 bidi regressions from a caller-supplied UCD file."""
import hashlib
import json
from pathlib import Path
import sys

source = Path(sys.argv[1])
raw = source.read_bytes()
vectors = []
for number, line in enumerate(raw.decode().splitlines(), 1):
    if not line or line.startswith('#'):
        continue
    points, direction, base, levels, order = line.split(';')
    points = points.split()
    if len(points) <= 40 and (len(vectors) < 48 or number % 7919 == 0):
        vectors.append({'line': number, 'points': points, 'direction': int(direction),
                        'base': int(base), 'levels': levels.split(),
                        'order': [int(value) for value in order.split()]})
output = {'unicode': '17.0.0',
          'source': 'https://www.unicode.org/Public/17.0.0/ucd/BidiCharacterTest.txt',
          'sha256': hashlib.sha256(raw).hexdigest(),
          'license': 'priv/licenses/unicode-character-properties-UNICODE-3.0.txt',
          'vectors': vectors}
Path('test/fixtures/html_to_pdf/typography/bidi_vectors.json').write_text(
    json.dumps(output, ensure_ascii=False, separators=(',', ':')) + '\n')
