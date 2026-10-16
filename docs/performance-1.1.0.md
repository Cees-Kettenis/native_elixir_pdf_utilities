# Performance repair, October 17, 2026

All 19 workload medians are below the original baseline on both supported
runtimes. The target is the original performance itself. The 10% CI allowance
is not the stopping criterion. The original revision remains
`fee6c4d6051844e8d82041ba9e83939b28e1c724`; no baseline or numerical guard changed.

## Results against the original

The retained source is 5.7% to 62.4% faster than the earlier fixed original
reference on Elixir 1.19, and 7.8% to 63.1% faster on 1.20. Every median is also
below the fresh paired original captures.

| Workload | Original → current, 1.19 | Faster | Original → current, 1.20 | Faster |
| --- | ---: | ---: | ---: | ---: |
| 100-row HTML | 159.7 → 131.1 ms | 17.9% | 148.7 → 120.0 ms | 19.3% |
| 100-row full font | 171.8 → 133.1 ms | 22.5% | 160.4 → 129.7 ms | 19.1% |
| PDF text extraction | 65.5 → 53.1 ms | 18.9% | 64.4 → 49.8 ms | 22.7% |
| PDF visual text extraction | 68.4 → 55.7 ms | 18.6% | 67.3 → 53.9 ms | 19.8% |
| 160 table headings | 65.7 → 62.0 ms | 5.7% | 66.7 → 61.5 ms | 7.8% |
| Government form | 53.6 → 28.2 ms | 47.4% | 53.5 → 29.1 ms | 45.7% |

The complete 19-workload comparison and raw samples are in
`.quality/performance/results/performance-20261017/reports.json`.
No workload was removed, replaced or shortened. Existing reduction, output-size
and absolute-time guards pass in every candidate capture. The separately
approved government-form size allowance remains unchanged.

Wall-clock gains do not mean every reduction count is below original. Text
extraction uses 2.9% more reductions than the fixed original on 1.19 and 10.1%
more on 1.20; visual extraction uses 2.9% and 9.2% more. Both execute faster.
The existing numerical reduction guards use their original recorded counts,
which differ from this measured original reference; those guards also pass.

## What changed

The first two passes remain in place. They removed unnecessary PDF escaping,
number-formatting copies, whitespace processing, inherited-style allocation and
invisible background allocation. They also reused compressed full-font programs
through the bounded font cache, skipped unnecessary CSS variable scanning after
canonical validation, reused unchanged style maps and batched source-index work.

The third pass addresses the remaining table and full-font costs:

- Share fixed edge maps and discard cascade-only inheritance metadata after
  declarations resolve. Source-cascade metadata and computed transform metrics
  remain available. Reuse already resolved border maps.
- Skip automatic table-header scan slots beyond every possible header. Header
  principals retain the original opaque-block behavior. The canonical full-scan
  work extent is still charged before skipping slots.
- Release the exact validated DOM memo after style preparation, so it does not
  keep the source tree alive throughout layout.
- Measure visible text without allocating paint-plan maps. The shared validator
  still owns text preparation, errors and work charging. Painting keeps the full
  selection/paint plan.
- Use direct binary construction for embedded CIDs. Keep the original UTF-8,
  ligature and font-mapping behavior.
- Use the existing bounded compression cache for embedded font streams as well
  as form snapshots. Compression thresholds, ratio bounds and fallback behavior
  remain unchanged.
- Skip empty cascade traversal while retaining the same default calculations.
  Skip authored-template traversal when the source cannot contain a template.
  Mixed-case template tags retain the normal path.
- Keep the binary collection threshold above the checked font working set for
  the outer font-budget scope, then restore the caller's setting on every exit.

## Garbage collection and memory

Diagnostic tracing found repeated major collections while font parse memos and
registries still referenced the font binaries. A slow 100-row sample took about
210 ms with 21 major collections; a faster sample took about 153 ms with four.
Layout collection time alone differed by roughly 42 ms. Those traces explain
an optimization target; they are separate from the accepted timed captures.

The retained font scope sets the process's minimum binary virtual heap to at
least twice its cumulative checked font-source bytes, converted to VM words.
It never lowers the caller's existing minimum. Nested scopes share the outer
setting, and an `after` block restores it on success, diagnostic failure or
exception. No global VM flags or allocation/work limits change. OTP documents
this setting as a garbage-collection trigger in its
[process flag reference](https://www.erlang.org/doc/apps/erts/erlang.html#process_flag/2)
and explains the binary virtual heap in its
[garbage collection guide](https://www.erlang.org/doc/apps/erts/garbagecollection.html).

There is a memory cost. Across the final workload captures, sampled peak total
BEAM memory changes by -2.2 to +27.4 MiB on 1.19 and +1.8 to +28.7 MiB on 1.20
against the fresh original. The largest 1.20 increase is full-font rendering,
104.4 to 133.0 MiB. The largest 1.19 increase is text extraction, 82.2 to
109.5 MiB. These are whole-VM peaks including shared caches and binaries, sampled
every 10 ms in separate runs. They are not isolated per-render allocations or
OS RSS, and the difference cannot all be attributed to the collection setting.
The font cache retains its existing 64-entry and 100 MB bounds.

## Measurement method

Three complete captures per source and runtime ran sequentially in balanced
original/candidate order on the same host, with 12 schedulers. Each capture has
two warmups and five timed samples per workload. The results use the median of
all 15 timed samples, not selected fast samples or medians of medians. Separate
memory runs do not affect timed samples. No compilation or validation ran
alongside these captures.

All 73 original production files match the original Git revision byte for byte.
Only comparison infrastructure, font inventory and dependency pins were aligned
to provide identical workload inputs. The reports verify runtime, VM and input
metadata. All six candidate captures match the same frozen 175-file production
manifest. Accepted captures ran on Elixir 1.19.5/OTP 28 and Elixir 1.20.2/OTP 29
using the maintained CI images.

Some original reruns exceed their own earlier wall-clock reference. The first
1.19 original capture has 15 such failures; the other affected original captures
have one each. All original numerical guards pass. These failures and slow
samples remain in the report. Every candidate capture passes, and the combined
candidate medians beat both the earlier fixed original and the fresh paired
original. The result therefore does not depend on the slower original reruns.

## Correctness and accounting

Standalone checks on both runtimes reproduce all 19 pre-third-pass workload
results byte for byte. ASCII, CRLF, Unicode, template and table parser probes
also preserve source projections and resource totals. The public-render probe
keeps identical PDF bytes and every counter except tree work. That counter falls
from 1,007 to 1,001, exactly the six canonical node visits eliminated by skipping
the unnecessary no-template traversal.

A differential check of 156 real parsed table documents preserves every table
model and header-work total. Existing tests were extended for whitespace,
source-memo release, header scans, text preparation and font-scope restoration.
A real public-render resource-limit case exercises the paint-stage failure
path. This pass adds no test registrations or browser allowances.

## Evidence and validation

- Final aggregate, all raw captures, source hashes and report hashes:
  `.quality/performance/results/performance-20261017/reports.json`.
- Fixed original reference:
  `tmp/performance-second/original-reference.json`.
- Exact production manifest: `tmp/performance-third/frozen-source.json`.
- Output and accounting checks:
  `tmp/performance-third/output-and-accounting.json`.
- Table differential: `tmp/performance-third/header-differential.log`.
- Profiles, rejected experiments, raw logs and exact third-pass production diff:
  `tmp/performance-third/`.
- Earlier partial passes and their failures remain historical evidence in
  `.quality/performance/results/performance-second-20261016/reports.json`.
  The first-pass development reference remains separate in
  `.quality/performance/baselines/1.1.0/optimized-20261016/reports.json`.
  Neither becomes a replacement acceptance baseline.

`./scripts/quality-matrix` passes on the frozen production source for both
supported runtimes. Logs are in `.quality/logs/20261016T230704Z-274547/`.

| Check | Elixir 1.19.5 / OTP 28 | Elixir 1.20.2 / OTP 29 |
| --- | --- | --- |
| Ordinary tests | 3,803 pass | 3,803 pass |
| Every module and Total coverage | 100% | 100% |
| Existing numerical performance guards | All 19 pass | All 19 pass |
| Largest reduction change under those guards | -0.37% | +5.97% |
| Dialyzer | Pass | Pass |
| Chromium with approved fixture allowances | 380 pass, 55.9s | 380 pass, 56.6s |
| Warnings | 0 | 0 |

The exact host command
`CHROMIUM_BIN=/usr/bin/chromium mise exec -- mix test.browser_parity` passes
380 checks in 56.4 seconds. Its log is
`tmp/performance-third/host-browser-parity.log`.

The matrix numerical guard check and the separate cumulative timing comparison
both pass. Every candidate median is below original, without using the elapsed
allowance. No new browser allowances or test registrations were added.
The final source/input manifest and validation inventory are retained in
`tmp/performance-third/final-validation.json`. Reports remain local in ignored
directories, following the contributing guide.
