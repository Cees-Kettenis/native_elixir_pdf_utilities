# Static lazy image auto sizing

The pinned HTML Standard [auto-sizing rule](https://html.spec.whatwg.org/multipage/embedded-content.html#the-img-element)
requires an authored `img` in the Lazy loading state whose `sizes` is `auto` or
starts with `auto,`, using ASCII case-insensitive comparisons. Eager, invalid
loading states, and image inputs retain normal source-size fallback behavior.
Omitted picture-source `sizes` means `auto` when the following image permits it.

During styling, ResponsiveImageValidator issues an opaque request within the
active document resource scope. Its original authorization root, resolution
base, options and shared image budget remain fixed. No resource bytes are read.
Size containment supplies the HTML UA fallback of 300 by 150 CSS pixels; it has
no intrinsic aspect ratio. Authored width/height and aspect-ratio hints still
participate in ordinary inline, block, flex and grid layout.

Layout measurements carry geometry without a fabricated image. After final
layout, `Layout.resolve_images/1` recursively resolves only surviving requests,
selects using their final used width, and loads that selected resource once.
Decoded pixels are fitted into the allocated viewport with the ordinary image
fitting and SVG document painting paths. Selected failures retain shared
diagnostics and never trigger an alternate-candidate fetch.

The shared document preparation executor resolves requests before pagination.
Printing resolves all visible lazy images; browser timing and scrolling are not
simulated. Call `HtmlToPdf.render/2` for the complete scope. Internal direct Style
and Layout callers must retain a ResourceContext and perform final resolution
before painting. Issued requests expire with that context, and forged or expired
requests return an actionable `invalid_layout` diagnostic.
