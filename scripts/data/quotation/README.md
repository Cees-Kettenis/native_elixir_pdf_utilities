The quotation table is exported from ICU 78 CLDR locale data. Chromium
152.0.7977.82 uses the same four single-code-unit delimiters and longest locale
prefix fallback in its `LayoutLocale::GetQuotesData` implementation.

Source reference:
https://chromium.googlesource.com/chromium/src/+/refs/tags/152.0.7977.82/third_party/blink/renderer/platform/text/layout_locale.cc

The generation input `/usr/lib/libicudata.so.78` has SHA-256
`d72003b08760852bee4c80cff32c1e6248ae6b1fda5bd313482733a4d5e70dc9`.
The output includes 906 recognized locales in 26 delimiter groups. Unknown
locales use Chromium's default English curly quotation pairs. Rendering does
not load ICU, CLDR files, or network resources.

Regenerate with `python3 scripts/data/quotation/generate.py`, then run
`mise exec -- mix format lib/html_to_pdf/quotation_data.ex`.
The data license is included as `UNICODE-LICENSE.txt`.
