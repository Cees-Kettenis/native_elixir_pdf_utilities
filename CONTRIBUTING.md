# Contributing

Start with the [architecture](docs/contributing/architecture.md),
[testing guide](docs/contributing/testing.md) and
[script guide](docs/contributing/scripts.md). They explain where code belongs,
how to write focused tests and how to run the required checks. For using the
library in an application, see the [user guide](docs/README.md).

## Set up a checkout

Install a supported Elixir release and compatible Erlang/OTP. The
[testing guide](docs/contributing/testing.md#full-quality-matrix) lists the
supported runtime images and platform prerequisites. Use your preferred runtime
installer; commands prefixed with `mise exec --` use mise when available.

Fork the repository, clone your fork and create a branch:

```bash
git clone https://github.com/YOUR_USERNAME/native_elixir_pdf_utilities.git
cd native_elixir_pdf_utilities
git switch -c my-change
mix deps.get
```

## Make and check a change

Keep the change focused. Public functions need `@doc` and `@spec`.
Validators own input and semantic rules; consumers use their validated results.
Recoverable public failures use
shared [diagnostics](docs/diagnostics.md), and tunable resource ceilings belong
in `NativeElixirPdfUtilities.Limits`.

Name source files for their module and tests for their observable behavior.
Use synthetic fixture data. Rendering changes need focused unit coverage and
browser parity fixtures when the output changes visibly. Run focused checks
while working, then run the complete quality matrix before submitting a pull
request:

```bash
./scripts/quality-matrix
```

The [testing guide](docs/contributing/testing.md) covers coverage, performance,
Chromium comparisons, warnings and result interpretation. Local validation and
required CI checks must pass. Report any check you could not run and have a
maintainer complete it before the change is ready.

## Document a change

Update the relevant user guide when supported behavior changes. Keep reusable
architecture, testing and contribution procedures in `docs/contributing/`.

Put implementation plans, wave records, investigations, audits and temporary
coverage inventories in ignored `tmp/`. Keep validation logs and measurement
artifacts in ignored `.quality/`. Never stage, commit, push, publish or package
those local records.

## Try the manual app

The separate
[manual application](https://github.com/Cees-Kettenis/native_elixir_pdf_utilities/tree/HEAD/dev/manual_web)
exercises rendering and PDF editing workflows:

```bash
cd dev/manual_web
mix deps.get
mix run --no-halt
```

Open [the app](http://127.0.0.1:4001) or its
[OpenAPI document](http://127.0.0.1:4001/openapi.json). Use synthetic fixture
files. The app is development tooling and is not included in the Hex package.

## Submit a change or report a problem

Review everything you submit, including code produced with AI tools. Push your
branch to your fork and open a pull request against `main`. Describe the problem,
resulting behavior, validation and any unresolved warnings. Address review
feedback and rerun affected checks after changes. The maintainer decides when to
release.

For a [bug report](https://github.com/Cees-Kettenis/native_elixir_pdf_utilities/issues),
include the library version, a small synthetic reproduction, expected behavior
and diagnostics. Follow the
[security policy](https://github.com/Cees-Kettenis/native_elixir_pdf_utilities/blob/HEAD/SECURITY.md)
for private vulnerability reports.
