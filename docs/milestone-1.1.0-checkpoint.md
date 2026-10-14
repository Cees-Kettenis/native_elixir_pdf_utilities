# Milestone 1.1.0 checkpoint and remaining work

This is an unfinished development checkpoint, published at the user's explicit request on 2026-10-14 before final acceptance. It contains the cumulative work since published wave 17, including the current wave 23 converter correction and the completed Math spacing/source-fragment packet. It is not a completed milestone or a qualified release. Implementation and validation agents are stopped after publication.

The previous published commit is `4e6a5d11fb28b7e42c7e54f14fa792e175c698a5`. The frozen integration source before the final Math join is `f0bd47520adee716edb4bb9d12dfa99b90985f8f`. The source tree after that reviewed join, before adding this handoff and evidence, is `54a53f7519c1eddcffca4e7dee9cd80c2121ff07`.

## Required behavior

- Extract SVG and send it through the existing stock Resvg converter, then embed its PNG through the ordinary image workflow. The custom SVG renderer, SVG vendor forks, Type3/glyph/Form rendering and SVG source certificates have been removed.
- Treat SVG as image artwork. Ordinary HTML/Math/PDF text and links remain functional; SVG text is not selectable/extractable and internal SVG links are not clickable. Enclosing HTML anchors remain clickable.
- Return explicit unsupported-feature diagnostics for every foreignObject conversion, including zero-sized, hidden and transparent cases. Valid Style preparation remains allowed; invalid Gaussian source parameters still diagnose.
- Use actual Chromium printed output as the rendering benchmark, even where it differs from the specification. Preserve original authored sources, fonts and physical tolerances. Do not restore custom SVG rendering to satisfy obsolete internal expectations.
- Keep native/html_backend limited to its current utility API. Do not add SVG native parsing/rendering dependencies. SVG source grammar admission lives in Elixir validators.
- Final acceptance still requires all functional tests, static checks, strict Chrome parity and meaningful 100% coverage for every module and the total. Performance is the only permitted exception. The user separately authorized this unfinished checkpoint push.

## Completed work included

Reviewed source contributions for Font/writer behavior, ordinary form appearance translation, Ruby typographic centering, UAX/text-work accounting, native Port lifecycle, layout/style cleanup, selected content, generic clip geometry, Math source ownership and spacing, image budgets and SVG source admission are integrated. The completed Math packet retains original glyph/font/source ownership and passes the original owner-paint Chrome gate.

Independent image-font options now pass separately from strict converter context. The actual existing source summary drives FontValidator.prepare_svg_image_fonts/3. Empty source text avoids implicit discovery after normal option and configured-font admission. Document image extraction preserves caller permission before child Style isolation, uses genuine independent font bytes, and retains selected-face subset checks. Explicit caller default-font selection is preserved.

The Elixir path-syntax validator uses the existing numeric parser and central bounds. The final shared source-tree dispatch is included for rendering, direct conversion and extraction. This last dispatch edit has not received a fresh complete matrix.

## Validation status at the stop

The last complete matrix ran on source `68ee7de9ad8effe716274cc1e08d7b0b03c45044`, before the newest joins:

| Check | Elixir 1.19 | Elixir 1.20 |
| --- | --- | --- |
| Compilation and dependencies | Pass, zero warnings | Pass, zero warnings |
| Formatting | Not applicable in that matrix stage | Pass |
| Unit run | 3,829 tests, 42 failures | 42 failures |
| Browser cases excluded from unit run | 511 | 511 |
| Total coverage | 99.18% | 99.19% |
| Modules below 100% | 27 | 27 |
| Fresh static continuation on that exact source | Pass, zero errors/skips | Pass, zero errors/skips |
| Full performance and Chrome parity stages | Skipped after unit/coverage failure | Skipped after unit/coverage failure |

These are historical diagnostics, not current-source acceptance results. Several reported failures were subsequently fixed. The new snapshot has not had its complete unit/coverage/static/Chrome matrix rerun. Skipped stages are not passes.

Later focused evidence includes 92/92 supported 1.20 ordinary/capability checks, 49/49 original document/font/image caller checks, and 28/28 explicit-default/path checks, all without warnings. The private Font summary adapter had 113 passing focused tests; the private Elixir path adapter passed five supported focused checks. The completed Math packet had 429 focused tests and six scoped modules at 100%; its original Chrome pixel ratio improved from 0.01090812 to 0.00804487 against unchanged 0.01 gates. These scoped results do not establish full checkpoint acceptance. Publication-only checks verified the frozen source and completed-packet replay, parsed 542 Elixir source/test files without syntax errors, and passed the cumulative Git whitespace check. These are not a replacement for the quality matrix.

The complete 42-failure details, 27-module coverage lists and missed-source-line reports from both matrix runtimes are tracked in [checkpoint-evidence/2026-10-14](checkpoint-evidence/2026-10-14). Their source revision is the historical matrix source above.

## Remaining work

1. Run the full current Chrome baseline centrally, then a fresh supported quality matrix after any necessary fixes. Use one owner for expensive checks. Keep source revision, fonts, runtime, inputs and configuration attached to every result.
2. Complete nested SVG font-policy transport. The current converter embeds referenced SVG resources before preparing the root independent snapshot. Child summaries are already parsed, but child font admission still uses the original strict context fonts and does not receive the separate caller discovery options. A shape-only outer SVG containing an approved child SVG with text needs a genuine permitted child snapshot and actual converter font-byte union. Disabled discovery and old configured-byte-only paths must remain restricted. No second parser, host CSS font import or new context authority flag.
3. Complete XML document spelling alignment. Preserve actual unqualified XML attribute names from the canonical raw SAX summary before HTML-oriented parsing/style projection changes their spelling. Keep inline HTML SVG foreign-name normalization and namespaced attributes correct.
4. Repair explicit SVG image fill sizing. The known 20x10 requested PNG case still follows the stock converter's inscribed FitTo::Size result of 10x10. Reuse canonical natural dimensions and faithful viewport transport; preserve root x/y, percentages, viewBox, preserveAspectRatio, CSS and resources. Do not introduce a second painter or duplicate allocation accounting.
5. Qualify the stock-converter tiny-canvas transport workaround. The original 1x1 red marker is transparent in direct stock Resvg. Read-only integer-padding and stock PNG bitmap-window probes recovered the original 1x1 red output, but no production workaround is included. Prove source/CSS/root/viewport semantics and charge both real canvases, PNG bytes and decoded work. The ordinary unpadded 1x1 rectangle under a one-pixel ceiling must remain valid.
6. Diagnose retained SVG rendering differences using exact same-source Chrome evidence. Historical residuals include mixed-rule clipping, marker orientation/zero dimensions/letterboxing/percentages, fractional pattern coverage, non-scaling strokes, large text rotations, mask/gradient transfer and filter source/result ink. Distinguish converter rendering from PDF image reconstruction. Correct an obsolete manually derived reference only when actual Chrome evidence supports that correction; preserve genuine functional/pixel obligations and all gates.
7. Rerun original source/geometry/font-declaration policy and public PDF/image tests after the final shared source-tree join. Keep the original invalid-source diagnostics and valid/empty path behavior. Investigate any remaining missing textPath source/structure obligations without reviving the deleted engine.
8. Reach meaningful 100% coverage on the current source. The historical module and missed-line reports identify the initial gaps, including TemplateExecutionValidator and TemplateRendererContextValidator. Newly integrated native tests close some old gaps; rerun coverage before assigning exact residuals. Do not fabricate execution state, exclude production lines or remove reachable failure contracts for a percentage.
9. Requalify static analysis on both supported runtimes after all source changes. The earlier zero-error results apply only to the old matrix source. Resolve every project warning and inspect all FAIL/WARN logs.
10. Measure performance after functional closure. The retained reduction-bound failure in the original mixed-style case is eligible for the user's performance-only exception. Do not silently raise thresholds or call skipped performance checks successful.
11. Review the final cumulative diff and update release/milestone status only when acceptance actually passes. No new implementation wave should start under the user's stop instruction.

## Resume locations

The local immutable worker packets and integration evidence remain under `/home/cees/Projects/native_elixir_pdf_utilities-110-orchestration`. The main integration working directory is `wave15/combined`; its coordination and detailed logs are under `.quality/orchestration/wave23-converter-integration-work` and `.quality/orchestration/wave23-release-closure-work`. Those ignored local artifacts are not a substitute for the tracked handoff and evidence above.

The original checkout's unrelated CHANGELOG.md modification is intentionally excluded from this commit. No release version or ROADMAP update is made by this checkpoint.
