# Initial shadow composition

Declarative `<template shadowrootmode="open">` and `closed` roots render the
first eligible root of each HTML host. Original templates and light children
remain available through `HtmlComposition.compose/1` as the unchanged logical
tree. The separate composed projection retains authored IDs, sources, parents
and tree scopes. `composed_parent_id` describes placement and grants no resource
permissions.

Named and default slots assign direct light children to the first matching slot.
Text belongs to the default slot. Missing assignments render fallback children;
a second slot with the same name renders its fallback. Manual initial assignment
has no assigned nodes because no `assign()` call has executed. Nested hosts create
independent roots. Ineligible hosts and invalid mode values retain ordinary inert
templates.

Shadow sheets apply within their defining root. `:host`, `:host(compound)` and
`slot::slotted(compound)` match the host and actual assigned elements. Outer normal
declarations win across encapsulation boundaries; important declarations favor the
inner root. Slots use `display:contents` and inherit computed styles before their
children enter layout. Explicit slot box styles still create boxes.

The original authored tree is validated separately from active shadow contents.
Caller resource authorization remains unchanged. Template `for` mutation remains
blocked until its seven existing source, insertion, font, geometry and resource
obligations are actually closed. Shadow composition does not supply a replacement
receipt or authorize that path.

Scoped font-family declaration origins are retained as `_font_family_scope`.
Resolution of duplicate font families in different scopes is coordinated with the
font loader; the current shared registry still requires that lookup integration.
