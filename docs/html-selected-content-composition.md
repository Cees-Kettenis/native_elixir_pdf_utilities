# Initial selected content

`HtmlSelectedContent.compose/1` keeps an authored logical document and builds the
initial selected-content projection. It takes selectedness from
`HtmlFormValidator`, including authored disabled selections and enabled-option
fallback. It copies the actual selected option's child subtrees, independently of
its `label` attribute. No selection clears the destination. Multiple selects skip
updates; option, selected-content and repeated select ancestry disable cloning.
These rules follow the [HTML selected-content algorithms](https://html.spec.whatwg.org/multipage/form-elements.html#the-selectedcontent-element).

Every clone receives a fresh identity, destination document/scope/parent, and
`cloned_from_node_id` pointing to its original source node. Its source span is
unchanged. Template fragment clones receive independent scope IDs and remain
inert. Recomposition after a validated tree change replaces the old clones and
recomputes selector and form metadata. Clone growth shares `max_html_nodes` with
authored nodes. Tree work and depth use the existing Limits entries.

The native renderer supports rich noninteractive option children and an initial
first child button. `appearance:base-select` renders that button's real cloned
contents. Computed base defaults inherit document fonts and use the initial flex
presentation; `::picker-icon` uses the existing generated-content cascade and can
be styled or hidden. Closed option popovers remain unpainted. Ordinary native
selects retain their existing label presentation.

This phase does not enable template `for` mutations. Once their existing source,
insertion, font, geometry and resource obligations are closed, the same initial
composition step can consume the validated updated tree. No clone grants resource
permissions. The source observer still requires an exact validator-owned clone
origin bridge before fresh clone IDs can contribute original-source evidence.

A selected child with an unclonable shadow root copies its actual light children
and omits the shadow contents. A clonable root receives a fresh fragment identity,
new scope and host association, fresh descendants, initial slot distribution and
scoped stylesheet registration. Selects within cloned roots initialize their own
selected content after attachment. Ordinary cloned template fragments stay inert.
The clone registry keeps actual logical light/root children and updated destination
records separately from composed placement, so form facts and positional selectors
can resolve the latest node by identity.

Form initialization uses the unchanged logical document, all active roots and
actual clone records. Unassigned light controls still participate in their logical
radio groups and ID associations. Composed inheritance and inertness remain on the
rendering projection. PDF control validation receives the validator-owned logical
children while painting uses the actual composed surface. Scoped duplicate-font
lookup and the original-source observer bridge remain coordinated follow-up work.
