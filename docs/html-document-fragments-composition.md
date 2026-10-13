# Document fragments after composition

HTML fragment navigation selects the first matching ID or legacy anchor name in
the logical Document tree. Shadow descendants do not become document fragment
targets. A link authored inside a shadow root still navigates its owning document.
This follows the HTML Standard's [indicated-part algorithm](https://html.spec.whatwg.org/dev/browsing-the-web.html#scroll-to-the-fragment-identifier).

Link preparation keeps rendered link discovery separate from target selection.
The target tree retains authored light-DOM order, including unassigned nodes and
slotted nodes below shadow ancestors. A non-rendered first duplicate prevents a
later visible duplicate from taking its place. Consumed declarative templates
are absent from the actual document; ordinary inert templates remain candidates
for their own ID, while their content fragments stay separate.

Initial selected-content destination records supply their actual fresh clone
children. These clones participate at their real logical placement before their
option originals, and cloned shadow descendants remain outside Document lookup.
The authored validation tree stays unchanged. Prepared selector identities join
these actual targets to the rendering projection; issued document/permission
scope remains distinct from authored document, node and source identity. Ordinary
DOM uses the existing preparation route without a second tree traversal.

The form writer validates an initial executed DOM separately from authored
control markup. Children copied into selectedcontent satisfy the option's
noninteractive child model, including its allowed flow content. Original authored
child checks and interactive-descendant restrictions remain in force. This
surface distinction neither issues a template receipt nor authorizes resources.

The original shadow-fragment browser case also exposed the existing shared
Chromium print-matrix raster boundary rule for underline paint. Its PDF uses
scales 0.23999999 and 3.125, giving 0.74999996875 points per CSS pixel. The first
underline's top is therefore 25.49999894 points, where exact native 25.5 lies on
the other side of a raster tie. Other lines use the same transform. Text-decoration
paint now uses the existing fixed boundary policy already applied to inline
backgrounds and frame borders. Logical bounds and annotation geometry are
unchanged, and no new tunable offset is introduced. Original red artifacts and
operator evidence are retained; separate fonts, sizes and positions exercise the
common paint rule. Final acceptance will review shared precision across roles.
