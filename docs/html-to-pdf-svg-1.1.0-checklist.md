# Static inline SVG implementation checklist

This is the separate SVG checklist required by the 1.1.0 plan. Existing asset
conversion does not complete these requirements. Every row needs focused tests,
per-page Chromium comparison, resource-policy checks, and scaling evidence.
The inline feature remains unfinished.

The HTML parser now retains SVG namespaces, adjusted element/attribute names,
foreign attribute identities, self-closing foreign elements, CDATA, HTML
integration points, and foreign fragments. This supplies the namespace tree.
SVG layout, host style composition, painting, and resource processing remain
unfinished, and the renderer diagnoses these inputs.

| Requirement | Acceptance cases | Source |
| --- | --- | --- |
| Host integration and namespaces | HTML foreign-content parsing, adjusted SVG names, namespace attributes, HTML integration points, empty elements, fragment contexts | [HTML foreign content](https://html.spec.whatwg.org/#parsing-main-inforeign) |
| Processing modes | Host restrictions propagate into nested SVG and HTML; image and reference contexts apply their required stricter mode | [SVG processing modes](https://www.w3.org/TR/SVG/conform.html#processing-modes) |
| Viewports and coordinates | Intrinsic and CSS dimensions, nested viewports, `viewBox`, aspect preservation, percentages and overflow | [Coordinates](https://www.w3.org/TR/SVG/coords.html) |
| Shape and path geometry | Every static shape and path command, compound paths, fill rules, stroke geometry, transforms and clipping | [Paths](https://www.w3.org/TR/SVG/paths.html), [shapes](https://www.w3.org/TR/SVG/shapes.html) |
| Host and SVG styles | Presentation attributes, author CSS, inheritance, current color, display, visibility, opacity and source order | [Styling](https://www.w3.org/TR/SVG/styling.html) |
| Painting and paint servers | Fills, strokes, gradients, patterns, coordinate units, stops, paint order and markers | [Painting](https://www.w3.org/TR/SVG/painting.html), [paint servers](https://www.w3.org/TR/SVG/pservers.html) |
| Text and fonts | Selected authorized font bytes, nested text spans, positions, anchors, lengths and text paths; diagnose missing required support | [Text](https://www.w3.org/TR/SVG/text.html) |
| Clipping, masking and filters | Units, regions, nested effects and required static primitives; decoded surfaces and work share configured budgets | [Masking](https://www.w3.org/TR/css-masking-1/), [filter effects](https://www.w3.org/TR/filter-effects-1/) |
| References and reuse | Same-document and eligible authorized references, inheritance, repeated instances, cycles and changed source content | [Linking](https://www.w3.org/TR/SVG/linking.html), [structure](https://www.w3.org/TR/SVG/struct.html) |
| Images and nested HTML | Authorized still images, nested SVG, `foreignObject` layout inside its viewport, transforms, paint order and clipping | [Embedded content](https://www.w3.org/TR/SVG/embedded.html) |
| PDF composition | Inline baselines, block placement, surrounding text, page boundaries and clipped link regions | [Printing requirements](https://www.w3.org/TR/SVG/conform.html#Printing) |
| Input policy and limits | Executable content, DTDs, external entities, unauthorized references and exhausted cumulative budgets produce diagnostics before backend access | [Milestone boundaries](../tmp/1.1.0-plan.md#boundaries-that-must-remain-explicit) |
| Caching and performance | One/many SVGs, repeated and changed resources, cold renders, increasing depth, reductions, native memory and PDF size | [Milestone performance requirements](../tmp/1.1.0-plan.md#performance-is-a-release-requirement) |

A backend must preserve visible content for every accepted input. Backend feature
omission is not successful rendering. The restricted SVG asset converter needs
its own mode and policy tests; its existing tests do not establish inline parity.


The installed image converter is resvg_nif 0.6.0 with resvg/usvg 0.47.0.
The 2026-10-08 local backend probe in
`tmp/performance/1.1.0/batch-33/backend-probe.exs` confirms two gaps with otherwise
accepted, self-contained source. A 40x20 SVG retains its blue shape, while all
400 pixels assigned to HTML foreignObject content remain transparent. An 80x20
SVG text-only image has 1,600 transparent pixels when the current converter
receives no font database. These excluded functional probes are saved under
`.quality/svg-backend-probe-*`; they are not parity or performance acceptance.

Inline integration requires native HTML composition in foreignObject and an
authorized font-byte/database path. It must preserve paint order, transforms,
clipping, host CSS, logical text and link regions. Supplying approved font bytes
must not reread changed or unauthorized files through the backend. Retain the
restricted image API and its existing resource boundary while implementing the
full inline painter; do not count these probes as delivered SVG support.
Relevant installed backend code includes `deps/resvg/native/resvg/src/lib.rs`,
and the versioned upstream converter is
[usvg 0.47.0](https://github.com/linebender/resvg/blob/v0.47.0/crates/usvg/src/parser/converter.rs).
The backend's feature list is separate from the required static profile.
