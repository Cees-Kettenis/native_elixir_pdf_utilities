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
The enlarged wave 10 candidate adds controls, source policy, table presentation,
SVG references/gradients and selected embedded content. Independent packet review
and quick checks are recorded, but full combined qualification is still pending.
The estimate remains anchored to the accepted source above. Failed full-family
MathML candidates and inactive reference proposals remain separate evidence.

The canonical accounting audit retained all 17,267 source IDs exactly once.
Inventory counts measure retained specification obligations rather than effort
or verified compatibility. See the [execution plan](html-to-pdf-1.1.0-execution-plan.md)
for named scopes and the [progress record](html-to-pdf-1.1.0-progress.md) for
accepted evidence.
