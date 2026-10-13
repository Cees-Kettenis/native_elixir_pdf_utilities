# Native temporal picker artwork

`calendar_icon.svg` and `time_icon.svg` retain the original Chromium
152.0.7977.82 artwork bytes from
`third_party/blink/renderer/core/html/resources/images/`.
The Chromium BSD license is in `LICENSE.chromium`.

The native renderer compiles these immutable artwork bytes into its module and
resolves the fixed light-theme WindowText fill to black. It uses the existing
validated SVG rasterizer, image decoder and render cache. These assets contain
no references, fonts, scripts or external resources. Disabled and readonly
indicators retain their CSS geometry while their artwork is hidden.
