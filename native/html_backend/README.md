# Native HTML utilities

This package preserves the existing CSS color parser, Unicode line-break
boundaries and disposable form-value executable. It contains no SVG parser,
painter, glyph observer, filter sampler, PDF interpreter or SVG renderer.

The private NIF exposes only unicode_line_breaks/1, css_color_math_components/1
and parse_css_color/3. Elixir validators keep ownership of caller input policy,
resource limits and diagnostics. The native_form_values executable retains its
existing bounded regular-expression and URL/value parsing protocol.

The CSS color grammar and form regular-expression budget instrumentation are
unchanged. Their vendored licenses and existing provenance records move with
the helpers. SVG conversion belongs to the installed upstream resvg dependency.

SVG input never enters this helper package. The HTML layer extracts it, validates
its resource/font inputs, calls the existing converter's documented APIs and
embeds decoded PNG bytes. See [SVG conversion](../../docs/svg-converter-workflow.md)
for that separate workflow and its current acceptance limits.
