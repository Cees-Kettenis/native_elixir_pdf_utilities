# MathML source transport

Math styling retains the real parsed node map while replacing computed style and
rendered children. Canonical node, document, tree-scope and parent identities,
source spans, selector metadata and genuine clone origins reach Math layout.
Metadata-free caller nodes remain metadata-free; styling invents no source IDs.

Math table row and cell preparation copy these existing facts into their source
records. Allocated row/cell background drawing items retain those records as
`_math_source`. Selected-content Math copies retain their fresh destination
identities and immediate `cloned_from_node_id` links. These fields describe actual
nodes and placement; they do not authenticate original source by themselves,
issue C2 receipts or grant resource permissions. Original-source observation must
use its validator-owned immutable-source and clone-chain authority.

A native fragment link can target a Math root containing a table. Enclosing HTML
links around equations still require real Math glyph/native link geometry
emission; their validator denial remains in place until that integration exists.
