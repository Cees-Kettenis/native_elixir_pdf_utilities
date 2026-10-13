# Replaced elements and containing sizes

Block images now use the same definite parent content-height reference as inline
images. Percentage heights remain automatic when the reference is indefinite;
a minimum height alone does not give an ordinary block child a percentage basis.
The computed HTML root height is retained separately from canvas paint and
resolved against the actual page or embedded client viewport before the body's
percentages are resolved.

Absolutely positioned images and HTML embedded surfaces keep intrinsic auto
sizes even when opposing insets are supplied. Percentage dimensions use the
positioned containing block's padding box. Insets position this allocated box;
they stretch ordinary non-replaced auto boxes through the existing rule.

Flex item measurement receives the real main and cross dimension references.
Final stretch reflows percentage descendants with the allocated content height.
Nested containers use their allocated border box once, retaining their authored
box-sizing conversion. Grid images resolve both dimensions together against the
final grid area, preserving a natural ratio when only one axis is definite.

A definite replaced axis remains definite when min/max constraints alter its
automatic partner. Conflicting automatic-axis constraints retain both bounds,
including cases where the natural ratio cannot be preserved. Existing image
selection, fitting, clips, embedded resource budgets and page preparation remain
the execution authorities for those operations.

The original single-page containing-size, flex/grid and replaced-constraint
fixtures compare these cases with Chromium at the unchanged 1% thresholds.
The flex/grid fixture uses the public repository `base_url` so its explicitly
configured test fonts are inside the same resource root as its iframe.

The original 480pt containing-size fixture and failed artifacts are retained.
Its 516pt content exposes a separate fixed decorated-block fragmentation issue:
Chromium splits the last positioned host; native layout moves it to the second
page. The L4 owner is implementing that shared continuation. This sizing patch
does not certify that cross-page case.

The dimensional behavior follows [CSS 2 replaced sizing](https://www.w3.org/TR/CSS2/visudet.html#inline-replaced-width)
and [Flexbox definite sizes](https://drafts.csswg.org/css-flexbox/#definite-sizes).

Computed HTML roots retain their width/min/max and both outer margin axes along
with padding and canonical border paint. The renderer uses the actual root
content width as the body percentage reference. Auto root width deducts fixed
outer and inner edges once; border-box dimensions convert once before body
layout. Root border paint uses the measured root height when authored height is
automatic. The document canvas continues using the existing root/body background
propagation contract. Root fragment destinations retain the actual border origin.

Original root cases include constrained percentage width, automatic margins,
content and border sizing, borders and padding, and a genuine srcdoc document.
The child resolves against its actual iframe client viewport. Its test opts set
the supported repository base_url so the configured font files are authorized;
HTML inputs and raster thresholds remain unchanged. Fixed root borders across
multiple PDF pages share the outstanding L4 decoration continuation contract.
