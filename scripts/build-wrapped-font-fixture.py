"""Rebuild the signed cmap-delta fixture from the bundled, licensed DejaVu font."""

from pathlib import Path
import struct

root = Path(__file__).resolve().parent.parent
data = (root / "priv/fonts/dejavu/DejaVuSans.ttf").read_bytes()
tables = {}
for index in range(struct.unpack_from(">H", data, 4)[0]):
    tag, _, offset, size = struct.unpack_from(">4sIII", data, 12 + index * 16)
    tables[tag] = data[offset:offset + size]

# Add an unhinted rectangle at glyph 40,000, leaving intervening glyphs empty.
# A simple shape isolates character mapping from font rasterizer differences.
glyph_count = struct.unpack_from(">H", tables[b"maxp"], 4)[0]
metric_count = struct.unpack_from(">H", tables[b"hhea"], 34)[0]
assert struct.unpack_from(">h", tables[b"head"], 50)[0] == 1
locations = list(struct.unpack(f">{glyph_count + 1}I", tables[b"loca"]))
glyph = struct.pack(">5h2H", 1, 0, 0, 1000, 1000, 3, 0)
glyph += b"\x01" * 4
glyph += struct.pack(">8h", 0, 1000, 0, -1000, 0, 0, 1000, 0)
glyph += b"\0" * (-len(glyph) % 4)
end = len(tables[b"glyf"])
tables[b"glyf"] += glyph
locations += [end] * (40000 - glyph_count) + [end + len(glyph)]
tables[b"loca"] = struct.pack(f">{len(locations)}I", *locations)
metrics = tables[b"hmtx"][:metric_count * 4]
last_width = metrics[-4:-2]
for index in range(metric_count, glyph_count):
    start = metric_count * 4 + (index - metric_count) * 2
    metrics += last_width + tables[b"hmtx"][start:start + 2]
metrics += b"\0\0\0\0" * (40000 - glyph_count) + metrics[36 * 4:37 * 4]
tables[b"hmtx"] = metrics
tables[b"hhea"] = tables[b"hhea"][:34] + struct.pack(">H", 40001)
tables[b"maxp"] = tables[b"maxp"][:4] + struct.pack(">H", 40001) + tables[b"maxp"][6:]
# Two segments: A -> 40000 via signed delta -25601, plus the sentinel.
# Build explicitly to keep the signed-delta bytes easy to audit.
format4 = struct.pack(">7H", 4, 32, 0, 4, 4, 1, 0)
format4 += struct.pack(">5H2h2H", 65, 65535, 0, 65, 65535, -25601, 1, 0, 0)
tables[b"cmap"] = struct.pack(">4HI", 0, 1, 3, 1, 12) + format4
tables[b"post"] = struct.pack(">I", 0x00030000) + tables[b"post"][4:32]
names = [(1, "Wrapped Glyph Fixture"), (2, "Regular"),
         (4, "Wrapped Glyph Fixture"), (6, "WrappedGlyphFixture")]
records, strings = b"", b""
for name_id, name in names:
    encoded = name.encode("utf-16-be")
    records += struct.pack(">6H", 3, 1, 0x409, name_id, len(encoded), len(strings))
    strings += encoded
tables[b"name"] = struct.pack(">3H", 0, len(names), 6 + len(records)) + records + strings
head = bytearray(tables[b"head"])
head[8:12] = b"\0" * 4
tables[b"head"] = bytes(head)

count = len(tables)
power = 2 ** (count.bit_length() - 1)
header = struct.pack(">I4H", 0x10000, count, power * 16,
                     power.bit_length() - 1, count * 16 - power * 16)
offset, directory, bodies = 12 + count * 16, b"", b""
head_offset = None
for tag, table in sorted(tables.items()):
    padded = table + b"\0" * (-len(table) % 4)
    checksum = sum(struct.unpack(f">{len(padded) // 4}I", padded)) & 0xFFFFFFFF
    directory += struct.pack(">4sIII", tag, checksum, offset, len(table))
    if tag == b"head":
        head_offset = offset
    bodies += padded
    offset += len(padded)
output = bytearray(header + directory + bodies)
checksum = sum(struct.unpack(f">{len(output) // 4}I", output)) & 0xFFFFFFFF
struct.pack_into(">I", output, head_offset + 8, (0xB1B0AFBA - checksum) & 0xFFFFFFFF)
destination = root / "test/fixtures/fonts/wrapped-glyph.ttf"
destination.parent.mkdir(parents=True, exist_ok=True)
destination.write_bytes(output)
