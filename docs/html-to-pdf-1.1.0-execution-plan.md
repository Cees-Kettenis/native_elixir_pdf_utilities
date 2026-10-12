# 1.1.0 execution plan

Prepared on 2026-10-12 from the previous agent's handoff and seven independent area audits. The milestone plan in `tmp/1.1.0-plan.md` remains authoritative.

Work began from verified batch55, preserving the previous agent's unfinished batch56. Seven combined waves have passed their required gates and are copied into the original checkout. The latest verified integration is `e9074a1d71ca4b67d162934c9d6eaeb24094b72e`. Both supported runtimes passed 1,862 unit tests at 100% every-module and total coverage and 173 parity tests; performance, Dialyzer and exact host parity also passed. The latest wave includes bounded inline SVG, iframe scrollbar and media geometry, measured HTML-child composition, MathML fractions, and parser prerequisites. Subsequent lane checkpoints remain queued. The full milestone and final cumulative idle timing audit are unfinished.

The eighth source batch combines validated CSS declaration/cascade ownership,
limited original source-context traversal and bounded MathML radicals. It keeps
all source-policy, resource, template replacement and dated-Core limitations
explicit. The select/listbox and authored CSS provenance checkpoints are queued
for a subsequent integration. New producer work is not imported while a combined
candidate is frozen. Each source batch needs both supported matrices, exact host
parity and source/performance hash checks before publication.

## Checkpoint estimate

The initial estimate is **47 remaining checkpoints: 45 feature batches and two final audits**. These are proposed scopes, not a promise that every large batch fits one agent turn. Split a batch when its changes cannot be reviewed or validated together. Merge related scopes when that improves implementation and verification. Assign accepted batch numbers centrally after integration; lane labels below avoid numbering collisions.

Inventory record percentages do not measure development effort. Several broad inventory descriptions are stale even where narrow behavior is implemented. Neither bookkeeping nor a foundational API closes a broader feature.

| Family | Proposed feature batches | Active ownership |
| --- | ---: | --- |
| HTML, controls, parsing and document metadata | 12 | controls, parsing and metadata links |
| Layout, tables, lists, ruby and images | 8 | layout/images |
| Parser composition and local embedded documents | 8 | composition/embeds |
| Complete static inline SVG | 8 | SVG |
| MathML Core and HTML integration | 9 | MathML |
| Combined-document acceptance and cumulative performance audit | 2 | coordinator after feature integration |
| Total | 47 | Seven feature owners plus one integration validator |

## Feature batches

### HTML and document plumbing

- **H1. Finish batch 56 editability.** Keyword defaults, inheritance, nested false/editable regions, mutable controls, namespaces, selector escapes/negation, budgets and cache isolation. Verified in648e5db; fieldset integration follows H3.
- **H2. Microsyntax and valid structure.** CR/CRLF preprocessing with original diagnostic locations, transparent anchors, valid button phrasing content and remaining optional/context rules. CR/transparent-anchor/button first slice verified in648e5db; remaining parser rules are open.
- **H3. Associated controls.** Fieldset/legend disabled inheritance and first-legend exception, output/datalist, ownership and labelability. Coordinate legend layout with L1.
- **H4. Existing control dimensions and selection.** Textarea initial values/whitespace/rows/cols/wrap, input size and placeholders, multiple/listbox selection. Coordinate rich options with C4.
- **H5. Numeric and temporal inputs.** Number/range/date/month/week/time/datetime-local grammar, sanitation, defaults, min/max/step, initial display and applicability. Depends on H2-H4.
- **H6. Color/file/image inputs.** Pinned color alpha/colorspace semantics, empty file selection with zero authored-path reads, authorized image inputs. Depends on H2-H4 and image selection.
- **H7. Initial constraint state.** Canonical candidacy and validity, radio/select aggregation, pattern semantics and applicable bounds. Constraint violation remains renderable state. Depends on H3-H6; broader deferred selectors stay deferred.
- **H8. Progress and meter.** Defaults, clamping, bounds, classification, label association and static artwork. Depends on shared numeric parsing.
- **H9. Dialog.** Closed versus authored-open nonmodal visibility, hidden-content policy, positioning and sizing. No fabricated modal/top-layer state.
- **H10. Ordinary and legacy markup.** Q quotation generation and overrides, menu/acronym/font/body hints, xmp/plaintext/noembed boundaries and applicable removed-element static behavior. Coordinate list and typography work; split if needed.
- **H11. Document context and selection.** First-base resolution without authorization expansion, relative/fragment destinations after pagination, clipped link regions, supported meta/style/media selection and source order. Base/link slice active. Preserve explicit stylesheets; discovered links/imports remain excluded.
- **H12. Inert metadata.** Microdata attributes and translate/global metadata preservation with validator-owned semantics, selector-visible values and no fetching or invented text. Bounded authored-tree preservation452cb51b passed; composed-tree integration remains open.

### Layout and images

- **L1. Replaced inline images and shared sizing.** Natural/authored dimensions, baselines, wrapping, links, borders/margins, clipping and intrinsic measurement; complete relevant auto margins/containing-size propagation. First inline-image slice9def7148 passed; broader sizing/table/positioned contexts remain open.
- **L2. Table formation and associations.** Rowspan zero, fixed span bounds, empty/implied rows, headers/automatic associations, model diagnostics. Preserve semantics without deferred PDF accessibility structure.
- **L3. Table presentation and captions.** Legacy hints with CSS precedence, block captions, hidden table structure and collapsed/separate borders. Depends on L1-L2.
- **L4. Fragmentation.** Decorated blocks, lists/captions, spanning cells, oversized rows, markers/clips/links/outlines and repeated headings. Depends on measured composition and table structure.
- **L5. General list composition.** CSS-created list items, marker images, menu/legacy containers, nested description defaults and flex/grid participation. Reuse existing ordinal/counter machinery.
- **L6. Ruby.** Canonical base/annotation groups, optional tags, fallback suppression, measurement, baselines, wrapping and pagination. Depends on atomic inline sizing; vertical writing/shaping remain deferred.
- **L7. Responsive images.** Picture/source/srcset/sizes, format/media choice, density-correct natural sizing, selected-source dimensions and selected-only resource loading. Depends on L1 and H11 print context. Selection and default inline pictures passed and are included in the original checkout's verified wave 3. Source-size metrics, advanced media grammar and lazy-auto follow-ups remain open.
- **L8. Fitting and image maps.** None/scale-down and full applicable positioning; region lookup/order, transformed and clipped rectangle/circle/polygon/default link geometry. Depends on L1/L7 and H11 destinations.

### Composition and embedded documents

- **C1. Addressable parser trees and processing instructions.** Earlier-node targets, scope/insertion locations, PI syntax and nonpainting/source identity. Coordinate with H2.
- **C2. Parser-time template-for replacement.** Marker/range search at insertion time, template contexts, close cleanup, inert fallback and validation of removed content. Depends on C1.
- **C3. Declarative shadow trees and slots.** Logical versus composed trees, named/default/manual initial assignment, fallback, nested scopes and required host/slotted styling. Depends on C1-C2.
- **C4. Selected-content clones.** Actual selected child subtrees, disabledness, identity/budgets and initial parsing/patch updates. Depends on H4 and composition scope.
- **C5. Embedded authorization/context.** Immutable permission root, mutable resolution base, pre-resolver local-only source restrictions, inline srcdoc, traversal/symlinks/cycles and shared budgets. Active.
- **C6. Isolated child viewport rendering.** HTML/text/supported image dispatch, document/style/selector/form isolation, clipped translated composition and shared budgets. Depends on C5 and reusable layout/writer contracts.
- **C7. Iframe/object/embed selection.** Element-specific source precedence, blank initialization, fallback, MIME/content validation and useful legacy hints. Depends on C6; PDF embedding remains a diagnosed 1.5.0 dependency.
- **C8. Frames and framesets.** Fixed/percent/relative tracks, nested grids, borders/margins, clipped children and applicable noframes selection. Depends on C6-C7.

### SVG

- **S1. Processing modes and backend adapter.** Validator-owned input/resource checks, cumulative work, authorized bytes/fonts and disabled native resolution. Active. Keep restricted image API; do not enable partial inline rendering as complete support.
- **S2. Host cascade and viewports.** Presentation properties/CSS/inheritance, intrinsic and authored sizing, nested viewBox/aspect/overflow and inline baseline. Depends on S1.
- **S3. Geometry and composition.** Complete included shapes/paths/strokes/transforms/fill rules, clipping and atomic artwork/logical-content geometry. Depends on S2.
- **S4. References and paint servers.** Defs/symbol/use, instance inheritance, gradients/patterns/markers and authorized nested images, expansion/cycles. Depends on S1-S3 and C5.
- **S5. Text and selected fonts.** Exact validated font bytes, spans/positions/anchors/lengths/text paths, Unicode and link geometry. Depends on S2-S4; named typography deferrals remain explicit.
- **S6. ForeignObject.** Native bounded HTML surface inserted at its SVG scene position before ancestor effects, with transforms/paint order/clips and logical text/links. Depends on S3-S5 and C6.
- **S7. Masks and filters.** Required static primitives/graphs, units/regions/color spaces, effects over HTML surfaces and preallocated cumulative surface/work limits. Depends on S4-S6.
- **S8. Integration and caching.** Context/byte/style/font/viewport keys, hit authorization/accounting, pagination/annotations, cold/repeated/scaling evidence. Depends on S1-S7.

### MathML

- **M1. Prepared styles, tokens and rows.** Validator-owned math semantics, selected font metrics, layout/ink geometry, real math token/row rendering and atomic inline/block flow. Active.
- **M2. Operators.** Pinned dictionary/form/spacing, embellishment, equation-aware stretching/axis/minmax and exact assembly ActualText. Depends on M1.
- **M3. Fractions.** Child sizing/depth/shift, axis/gaps/rules and zero-rule stacks. Depends on M1-M2.
- **M4. Roots.** Variants/assemblies, radicand/degree measurement, overbar and selected-font clearances. Depends on M2-M3.
- **M5. Scripts, limits and accents.** All required pre/postscripts, kerning/corrections, movable limits, accents and equation-dependent substitutions. Depends on M1-M4.
- **M6. Grouping and prescribed error rendering.** Mstyle/merror/mpadded/mphantom, unknown/anonymous content and selector-visible versus generated nodes. Depends on M1-M5.
- **M7. Matrices.** Atomic table sizing/alignment/spans, nested cells and math-axis centering. Depends on M1-M6 and shared table measurement.
- **M8. Semantics and annotations.** Core initial selection, inert annotations, HTML/SVG integrations and CSS-visible alternatives. Depends on M1/M6 and relevant SVG/HTML rendering.
- **M9. Combined equation flow.** Baseline/intrinsic/decoration/link contribution, atomic pagination and oversized equations without truncation or arbitrary splitting. Depends on M1-M8 and other composition features.

### Final audits

- **F1. Combined-document acceptance and inventory reconciliation.** Realistic multipage mixes of all included features, content/visibility/pages/links/fields/resources as well as pixels; every canonical ID accounted once, evidence-based support descriptions and explicit later dependencies. Include canvas scripting-disabled fallback and any applicability gaps identified by coverage audit. Run complete required quality gates.
- **F2. Cumulative performance audit.** Compare the fixed pre-milestone revision, previous accepted source and final candidate on unchanged workloads/runtimes/inputs/assets; report elapsed time, reductions, memory, size, cold/resource/scaling cases and all tradeoffs. Up to three balanced independent VM rounds for inconsistent timings. User decides whether remaining cost is acceptable; no release or larger exception on their behalf.

## Execution and integration rules

Seven feature agents own isolated worktrees under `/home/cees/Projects/native_elixir_pdf_utilities-110-orchestration/`. A dedicated integration validator checks preserved patches and combined checkpoints. Controls absorbs fieldsets, layout/images absorbs responsive images, document metadata absorbs metadata preservation, MathML absorbs operators, and composition absorbs viewport groups. Retired worktrees and their handoffs remain available. They work only on the first assigned bounded batch, then return reviewed commits and evidence. The coordinator integrates, supplies updated interfaces/base source, and assigns the next batch using a new delegated task ID. No agent writes into the original checkout or another lane.

The coordinator alone owns main progress/compatibility/changelog and canonical inventory edits. Agents report proposed row changes in ignored evidence. Broad records stay partial until all their included obligations pass. Explicit 1.2 typography/CSS, 1.3 accessibility, 1.5 PDF rendering and 1.6 full editing dependencies are preserved.

Validators own input, option, structure and semantic rules. Shared resource handling reuses existing render/font/image budgets and RenderCache hit accounting. Child permission is immutable and can only become stricter. Base URL is resolution information, not read permission. No child web fetching or callbacks can bypass local-only embedding policy.

MathLayout preparation remains in MathLayoutValidator. The agreed first measured-content contract separates layout/ink extents, intrinsic width, italic/accent metrics and positioned selected-glyph drawing items; atomic inline-block placement reuses existing baseline/pagination machinery. General transformed/clipped viewport groups are a later shared C6/S6 prerequisite, not a replacement for text-specific glyph groups.

Each implementation change requires meaningful focused tests, visible browser fixtures and the full quality matrix plus exact host parity command. Full gates run through `run-validation` with eight concurrent compute stages, each pinned to one CPU in an eight-CPU pool. Performance stages temporarily drain that pool for exclusive measurements; the original benchmark VM settings and thresholds are preserved. Independent worktrees retain their own lane locks and caches. Agents inspect every FAIL/WARN log. Existing thresholds and content checks remain unchanged. Detailed cumulative timing investigations wait for F2; approved overhead is 10% total against the original baseline, never 10% per batch.

Only passing batches count as accepted. Preserve user changes and prior evidence when copying integrated changes back. Local coordination commits remain in the isolated repository; no push or release is authorized.

## Durable coordination records

- `tmp/1.1.0-orchestration-state.json`: preserved source and worktree paths.
- `tmp/1.1.0-active-tasks.json`: task IDs and backing child thread IDs.
- `tmp/1.1.0-audits/`: independent area and shared-contract audits.
- `docs/html-to-pdf-1.1.0-progress.md`: accepted work and handoff history.

Current status: audits complete; the team is consolidating to seven feature owners and one integration validator. Combined parser and editability checkpoint 648e5db passed both runtime matrices and exact host parity, with 100% coverage, no FAIL/WARN entries, and matching source guards. Its thirteen changed source/test/fixture paths are copied into the original checkout after hash checks. Other feature batches remain incomplete until their required gates pass.
