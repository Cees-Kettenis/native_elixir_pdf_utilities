# Selected SVG child viewports

Selected SVG documents in `object`, `embed`, `iframe` and `frame` use the existing
SVG converter and ordinary PNG image transport. Their resource context retains
the original source label, host, authorization root, document identity and shared
image budget.

`SvgDocumentImage.prepare/6` issues the selected document descriptor from actual
source bytes. `compose/4` resolves its physical child viewport and returns decoded
PNG artwork or `:skipped` for a zero viewport. It does not create a PDF Form or
invoke the removed custom SVG engine.

Initial document sizing and replaced-element sizing remain distinct host layout
rules. Existing viewport validation resolves those rules before converter work.
The source root and its `viewBox` still control the converter's painting inside
the resulting viewport. Layout must preserve surrounding flow, clipping and
page count without inventing coordinate corrections.

Prepared descriptors are valid only in their issued resource scope. Changed
source identity, host, budget or dimensions must fail through shared diagnostics.
Authorized resource and font bytes remain attached to that scope. Cache reuse
must retain the same actual source, viewport and resource configuration.

The original child-document browser fixtures remain the acceptance evidence.
PNG transport does not supply SVG link annotations, extractable vector text or
foreignObject HTML fields. Actual converter gaps remain open until qualified
against the unchanged source and comparison gates.
