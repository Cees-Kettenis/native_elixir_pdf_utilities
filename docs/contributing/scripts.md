# Contributor scripts

See [Contributing](../../CONTRIBUTING.md) for setup and
[testing](testing.md) for required checks. Run these commands from the repository root. Paths in the table are relative to
`scripts/`.

| Path | Purpose |
| --- | --- |
| `quality-matrix` | Validate supported runtimes in parallel, then measure latest-runtime performance |
| `test-quality-matrix` | Exercise matrix orchestration with simulated containers and Mix commands |
| `performance-regression.exs` | Measure the existing guarded workloads and compare a fixed capture |
| `benchmarks/` | Focused HTML-rendering and image benchmarks |
| `fixtures/` | Rebuild browser sheets, reference fonts, PNG/PDF cases and Math reference inputs |
| `data/` | Generate HTML entities, quotation data and their retained licenses |
| `examples/` | Rebuild the documented HTML/PDF examples |

Common commands:

```bash
./scripts/quality-matrix
QUALITY_MATRIX_JOBS=1 ./scripts/quality-matrix
./scripts/quality-matrix --matrix-json
mise exec -- mix test.performance
MIX_ENV=test mise exec -- mix run scripts/fixtures/build-browser-sheets.exs
mise exec -- mix run scripts/examples/generate-html-to-pdf.exs
```

The default is two concurrent runtime lanes. Use `QUALITY_MATRIX_JOBS=1` on a
memory-constrained host, or raise it when admitting another supported runtime.
`--matrix-json` validates and prints the runtime configuration without Docker;
CI uses the same validation.

Read a generator's header before rebuilding its checked-in inputs. Some require
pinned source fonts, FontTools or explicitly selected upstream data. Regeneration
is a deliberate change to those inputs and their recorded provenance.

The performance harness retains stable workload IDs and logical input keys. Its `input_sources` report maps those identities to current descriptive
file paths. Moving a document does not reset its baseline: its source hash,
workload membership and all reduction, timing and output-size guards still apply.

Store experiments under `tmp/` and validation evidence under `.quality/`. Keep
maintained scripts named for their operation and feature.
