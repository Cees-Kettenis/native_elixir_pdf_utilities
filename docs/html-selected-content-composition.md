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

Cloning a selected child that itself hosts a shadow root still requires completion
of clonable-root handling. Unclonable roots must omit their shadow children;
clonable roots need fresh identities, scopes, slot assignment and stylesheet
registration. That integration and scoped duplicate-font lookup remain in progress.
