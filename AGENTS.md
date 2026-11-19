# Agent guidelines

Elixir library. Make the simplest correct change. Explicit user instructions
prevail. These rules are mandatory.

## Start

- Inspect status and diff; preserve existing work. Never discard, automatically
  stash or commit unrelated changes. Use a separate worktree for unrelated work.
- Read `docs/contributing/architecture.md` and `docs/contributing/testing.md`
  before implementation; follow their code contracts, structure and procedures.

## Workflow

- Identify acceptance checks and risky interactions first. Resolve uncertainty
  with small experiments; finish small slices with focused positive/negative tests.
- Reuse existing helpers. Avoid unrelated cleanup, speculative abstractions,
  unnecessary dependencies, assets and new tooling.
- Use `./scripts/quality-matrix --check test` with relevant test paths while
  editing. Use `--check coverage` for standalone CI coverage and `--check parity`
  with affected test paths for visible changes. Run independent checks concurrently
  when resources allow and keep performance measurements isolated. Investigate
  failures with a hypothesis and reproducer before repeating broad checks.
- Check common-path performance early. Preserve workloads and baselines; stop
  profiling after requirements pass unless new evidence warrants more work.
- Review scope, diagnostics, boundary cases, fixture registration, coverage and
  documentation before final validation. Flag expensive uncertainty early.
- Aim for one final matrix per completed change set. Freeze code while validating.
  Further edits must address a concrete issue and require revalidation.

## Verification

- Except for documentation-only changes below, run `./scripts/quality-matrix`
  once the related change set is ready. Require a green matrix for the final source
  before completion/push. Focused checks cannot replace it. Preserve 100% coverage,
  parity/performance thresholds and protection unless the user authorizes an exception.
- Inspect every FAIL/WARN log; resolve project warnings and disclose third-party
  warnings. Verify fixes narrowly before rerunning the matrix.
- Follow the documented runtime/cache policy. Never share an active lane's build
  directory. Run performance without competing work. Never cache test outcomes.
- Renderer changes need unit coverage; visible changes need parity on every
  runtime. Use container Chromium unless the user requests host investigation.
- Reuse/register fixtures and shared references. Preserve inputs/assertions/limits.
  Grouping or capture changes require pixel, page-bound and text equivalence on
  every runtime. Justify extra captures, raster work and unexpected fallbacks.
- Prefer minimal test inputs over bulky fonts/assets. Preserve reference bytes,
  generator provenance and required license notices.

## Documentation and handoff

- User docs: `README.md`/`docs/`. Contributor docs: `docs/contributing/`, through
  `CONTRIBUTING.md`. Keep AGENTS.md agent-only, outside public links/ExDoc/packages.
- Keep work records in ignored `tmp/development/`, evidence/caches in `.quality/`.
  Never publish them or create `docs/development/`. Use feature names in maintained
  tests/fixtures. Keep policies, release notes, roadmap and asset notices in place.
- Build docs with warnings as errors for publishing, not routine checks.
- Documentation-only changes, including edits limited to `@doc` and `@spec`,
  are exempt from all tests and the quality matrix. Review the edited content
  and report the exemption. This applies when no executable code, configuration,
  tests or fixtures change.
- Report progress and limitations concisely. Record verification commands,
  durations/results, failures and rerun reasons using existing tools. Summarize
  targeted/matrix counts and timings; distinguish elapsed time from overlapping
  summed runtimes. Verify commit/push/CI claims. Leave review drafts uncommitted.
