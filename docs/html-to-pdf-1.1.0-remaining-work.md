# Remaining 1.1.0 work

Assessment dated 2026-10-13 against verified source
`3c91ae5c842f4f2efe063ffde37da7d71fc65c4e`. The planning range is **38 to 46
feature batches and two final audits** with six implementation workers and two
independent validators. The larger-wave target is **8 to 12 combined waves**,
including six to ten feature freezes and two audits, if each accepted wave
closes five to seven coherent batches. The earlier ten-to-fourteen-wave estimate
remains a conservative range until one or two larger waves establish accepted
throughput. Two or three adjacent implementation slices do not automatically
close two or three complete batches.

These are coherent, reviewable changes, not inventory rows or a promised
schedule. Several earlier scopes have delivered foundations while their broader
features remain partial. Large dependent features may need more than one batch.
Unaccepted recovery candidates are excluded from this estimate's delivered work.

| Family | Remaining feature batches | Principal unfinished work |
| --- | ---: | --- |
| HTML and document plumbing | 9 to 11 | Remaining structure rules, output/datalist, range and temporal inputs, color/file/image inputs, validity aggregation, dialog, legacy defaults, composed metadata and source policy |
| Layout and images | 7 to 9 | General intrinsic sizing, table presentation and fragmentation, list composition, ruby, responsive-image metrics, fitting and image maps |
| Composition and embedded documents | 6 to 7 | Template replacement, shadow trees/slots, selected-content clones, complete child composition, object/embed dispatch and framesets |
| Static inline SVG | 8 to 10 | Complete viewports/clips, references and paint servers, text, native foreignObject HTML, masks/filters and full resource/cache integration |
| MathML | 8 to 9 | Remaining operator interactions, fraction/root contexts, scripts/limits/accents, grouping/error rendering, matrices, semantics/annotations and complete equation flow |

The main serial dependencies are SVG geometry and references before text,
foreignObject and effects; Math operators before scripts, grouping and matrices;
and validated parser/source contexts before template and shadow composition.
Shared native HTML surfaces and intrinsic measurement connect those families.
Six workers can run independent changes concurrently, but those contracts and
qualification failures affect the number of combined waves.

The final compatibility audit must verify realistic mixed multipage documents,
visibility, content, links, fields, resources and every canonical inventory ID.
The separate performance audit compares the fixed pre-milestone revision
`fee6c4d` with the final candidate on matching runtimes, fonts, inputs and VM
settings. The allowance is 10% cumulative elapsed-time growth per existing
workload for the entire milestone. Passing ordinary reduction gates does not
complete that audit.

Both supported runtimes passed 1,970 tests at 100% coverage, unchanged
performance checks, Dialyzer and 185 browser comparisons. Exact host parity
also passed 185 comparisons.
[Hosted CI also passed](https://github.com/Cees-Kettenis/native_elixir_pdf_utilities/actions/runs/38329634380).
The enlarged wave 10 adds qualified bounded controls, source policy, table
presentation, SVG references/gradients and selected embedded content. Its exact
expanded-v4 patch passed 2,090 tests at 100% coverage, all original performance
checks, Dialyzer and 207 browser comparisons on both supported runtimes; host
parity passed 207 comparisons. It is applied locally and has not been pushed.
The broader named feature batches remain partial, so their planning range is
retained pending the independent closure accounting. Failed full-family
MathML candidates and inactive reference proposals remain separate evidence.

The canonical accounting audit retained all 17,267 source IDs exactly once.
Inventory counts measure retained specification obligations rather than effort
or verified compatibility. See the [execution plan](html-to-pdf-1.1.0-execution-plan.md)
for named scopes and the [progress record](html-to-pdf-1.1.0-progress.md) for
accepted evidence.

## Rolling implementation update

Waves 10 through 14 are published. The current predecessor for Wave 15 is
`83454f6d9395647456ef3c5107ed0afbff3bf019`. The planning range above is historical;
it predates these waves and is not a current estimate of work remaining.
Fourteen implementation workers now run focused checks. Full acceptance,
coverage and performance review remain deferred until implementation is complete,
as requested by the user.

Wave 14 delivered initial constraint validation with a bounded native Unicode-v
pattern/WHATWG URL helper, original-source MathML identity, SVG text paths and
coordinate defaults, child-document MIME handling, script/multiscript geometry,
fraction/radical validator preparation, image maps, canvas fallback, composed
editability/translation, and light-tree style selection. These are implementation
packets with focused evidence. Known original browser failures remain recorded;
this wave does not claim final qualification.

The layout owner identifies four substantial remaining packets: actual
page-capacity span surfaces, cell and oversized-row continuation, block captions
and decorated-table continuation, and remaining percentage/containing-height
and positioned replaced sizing. Table model/presentation needs an audit afterward.
Modern color initial values are implemented in Wave 15. Cross-owner inert
control/clone/label integration still needs implementation and review.

Other active work includes actual template replacement, scoped font lookup,
embedded controls/fragment and link transport, language/metadata applicability,
responsive source-size grammar, SVG authorized images/cache accounting, actual
HTML inputs to filters, selected-font stroke and hyperlink geometry, and remaining
MathML operator contexts, schema CSS flow, grouping/padding and matrices. Counts
from these owners are still being reconciled; no percentage or completion date
is inferred from the number of workers or specification inventory rows.

Original SVG pattern/mask/foreignObject edges and several complete MathML
comparisons remain unresolved. Wave 11 hosted coverage reached 100 percent
before performance limits failed. Later full qualification remains deferred.
Final review must restore the fixed performance baseline, achieve 100 percent
coverage on both supported runtimes, and reconcile every included specification
obligation. Unsupported diagnostics alone do not complete an included behavior.

Wave 15 adds shared responsive CSS math and calculated image positions, ordinary
fraction/root CSS flow, Math grouping/padding, real enclosing equation links,
renderer-owned template replacement, continuous/span/percentage matrix sizing,
input attribute applicability, modern color parsing, authorized SVG images and
nested HTML Forms, actual HTML filter inputs and live-source filter text. Its
combined native/Elixir focused run passed 412 tests. The subsequent overflow
coordinate correction passed 142 affected tests, including the original child
link and zero-viewport assertions. These results are focused implementation
evidence; the complete matrix, coverage and original browser comparisons remain
pending final review.

The SVG text owner identifies nine remaining coherent families, including links,
stroke, text paint, ancestral lengths, path profiles, complete clip geometry,
host SVG CSS/transform behavior, processing/work propagation and expanded-instance
logical geometry. Ancestral lengths and complex clipping are now assigned to
additional workers. These family counts are broader than the individual packets
already delivered. Remaining effort still requires multiple larger waves.
