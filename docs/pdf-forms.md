# PDF forms

Use `NativeElixirPdfUtilities.Forms` to inspect, fill and flatten AcroForm
fields. HTML rendering creates these fields by default. An ordinary PDF with
printed lines or scanned boxes has no field names to fill; use `Stamp.text/3`
for explicit text placement in those documents.

## Generate and complete a form

```elixir
alias NativeElixirPdfUtilities.{Forms, HtmlToPdf}

html = """
<div>Your name: <input name="full_name"></div>
<div><input type="checkbox" name="consent"> I agree</div>
"""

{:ok, pdf} = HtmlToPdf.render(html)
{:ok, fields} = Forms.fields(pdf)
{:ok, completed} = Forms.fill(pdf, %{
  "full_name" => "Cees Kettenis",
  "consent" => true
})
{:ok, flattened} = Forms.flatten(completed)
File.write!("completed.pdf", flattened)
```

`fields/1` returns a list of maps with `:name`, `:type`, `:value`, `:choices`,
`:read_only`, `:export_values` and `:widgets`. Each widget has a one-based
`:page` and a `:rect` of `[left, bottom, right, top]` in PDF page coordinates.
Choice entries contain `:value` and `:label`. Checkbox values are booleans;
radio export values are strings.

## HTML field names and compatibility

Text/search/telephone/URL/email inputs, textareas, checkboxes, radio buttons and supported single/multiple-selection selects
become interactive fields. Button elements and submit/reset/button inputs remain static artwork. Use
`HtmlToPdf.render(html, forms: :static)` for the pre-0.18 behavior.

Hidden inputs produce neither artwork nor fields in either mode. Their names
and values remain in the parsed HTML tree and do not reserve PDF field names.
Author display declarations cannot make a hidden input visible.

Submit/reset/button inputs use their initial value as the printed label. A missing
submit/reset value uses English `Submit`/`Reset`; a missing button value is empty.
An explicit empty value stays empty. They create no fields, reserve no names and
consume no appearance widgets, even in interactive mode. Button elements print
child content; their `value` supplies no fallback label. Printed button labels
remain page content when other fields are filled or flattened.

Text/search/telephone input values strip CR and LF before field creation and appearance generation.
URL and single-email inputs also trim leading/trailing ASCII whitespace. Invalid
initial URL/email strings still render; their supported initial constraint checks
record the violation separately from validation candidacy.
For example, `value="A&#10;B"` initializes the field to `"AB"`. The same value
appears in static mode. Textareas keep their multiline values.

For `<input type="email" multiple>`, initial values first strip CR and LF.
Tokens then trim ASCII edge whitespace and rejoin with commas in the same order.
Empty tokens remain. For example, ` A&#10;B , C&#13;D ` becomes `AB,CD`.
`multiple="false"` still enables this boolean attribute.
The PDF field remains `:text` with a string value, without multiple-choice flags.
Readonly/disabled flags, generated `TEXT` names, filling and flattening retain
their existing behavior. Filling uses the PDF text API; it does not rerun the
HTML initial-value algorithm. IDN display conversion remains unfinished.

Missing, empty or unknown input types use the Text state, with the same initial
CR/LF sanitation, `:text` fields, `TEXT` generated names and readonly behavior.
Keyword matching is ASCII only and does not trim; `type="hidden "` is a visible
text input. The authored string remains available to CSS. Recognized unsupported
states return a rendering diagnostic instead of defaulting.

A nonempty HTML `name` becomes the field key. Otherwise the renderer generates
`TYPE_PAGENR_ELEMENT`, such as `TEXT_1_1`, `CHECKBOX_1_2`, `RADIO_2_1` or
`CHOICE_2_2`. Textareas and all supported text-entry input types use `TEXT`. Pages and element numbers start at 1.
The element number counts controls on that page in HTML document order after
pagination. CSS visual ordering does not change this order. The HTML `id`
attribute does not supply a field name.

All explicit names are reserved before assigning generated names. A generated
name that collides gains `_1`, `_2`, and so on until unique. Explicit duplicate
names fail, except for radio controls sharing a group name. Radio group values
must be distinct, nonempty, and different from the reserved `Off` state.
Initial groups use actual HTML form ownership and nonempty authored names.
If several members have `checked`, the last in HTML tree order wins. Radios
without a nonempty name remain independent. Authored attributes stay intact for
CSS attribute selectors; printed artwork and PDF values use effective checkedness.
Separate owner groups sharing one PDF name return `:unsupported_form` with that
name as diagnostic context. Use unique names or `forms: :static`.
An explicit `form` reference uses the first matching ID and must target an HTML
form. A missing or non-form target leaves the control unowned rather than falling
back to an ancestor. Select options require unique
values. HTML names cannot contain a period or NUL; periods separate nested
field names in existing PDFs.

Ordinary single-select dropdowns use the last authored selected option. Without
an authored selection they use the first enabled option. An explicitly selected
disabled option remains selected. Empty selects and all-disabled selects without
an authored selection have no selection and expose `nil` as their PDF value.
An empty option can be selected and has the empty string value. Empty and absent
values remain distinct through inspection, filling and flattening.

Static artwork, PDF values and initial `:checked` selectors consume canonical
state. Authored `selected` and `checked` attributes remain intact for CSS
attribute selectors. `:checked` applies to effective checkbox/radio checkedness
and option selectedness in the supported ordinary HTML tree. Ordinary optgroups preserve their label metadata and
option order. Disabled groups disable their options for fallback selection and
initial selectors; an explicitly selected disabled option remains selected.
A nonempty option `label` attribute supplies its display label. Empty or absent
labels use ASCII-trimmed and collapsed option text. Explicit `value` attributes
are preserved, while absent values use normalized text independently of the
label. NBSP remains intact. Empty dropdowns reserve the same normal font line as
populated dropdowns without an authored height. Native dropdown appearance uses
the font's normal line height rather than an authored CSS line-height. Authored
`display:inline` retains the complete measured native control box. An initial
`appearance:none` menu has no native arrow or internal native padding. Initial
values, complete option exports, actual owners and widget rectangles survive
filling and flattening. Disabled native select page paint and widget appearances
use one isolated transparency Form with the pinned UA opacity.
Multiple selects and listboxes retain selected values, option indices and initial
scroll state. Logical shadow/slot/clone trees preserve source ownership and
disabledness. Rich/custom controls and complete interactive disabled-option
policy remain unfinished.

Generated names can change when pagination changes. Supply `name` for keys
used by application code. Controls cannot span pages or repeat with the same
control identity in page furniture. HTML `disabled` makes a field read-only.
Text/search/telephone/URL/email inputs and textareas also accept boolean `readonly`, including
`readonly="false"`. It preserves the value and styling, and sets PDF flag bit 1.
Combined disabled/readonly sets that bit once; textareas keep multiline bit 4096.
`Forms.fill/3` returns `:read_only_form_field` with the shared diagnostic for a
read-only field. `Forms.flatten/2` can still flatten it. Static mode adds no
readonly appearance unless author CSS selects the attribute.
All members of a radio group must be enabled or all disabled. Interactive
rendering rejects mixed groups with `:unsupported_form` because PDF read-only
status applies to the whole field. Use `forms: :static` to render mixed groups
as artwork without interactive fields.
The HTML parser retains its documented supported subset.

Initial appearances are native Form XObjects with shared font, image and graphics-state resources. Temporary appearance frames use the appearance limits rather than the actual-page limits. Default-limit tests cover rendering, filling and flattening 1,000 controls; document content must still fit the field, object, byte and font budgets.

Initial appearances use the rendered HTML styling. Text extraction reads page
content, so flatten fields first when you need their values included in
`Text.extract/2` output.

## Fill an existing PDF

```elixir
{:ok, original} = File.read("application.pdf")
{:ok, fields} = Forms.fields(original)
IO.inspect(fields)
{:ok, completed} = Forms.fill(original, %{"applicant.full_name" => "Cees Kettenis"})
```

Use the fully qualified names returned by inspection, including parent names
separated by periods. Repeated widgets of a field are updated together.
Unselected fields and unrelated annotations, metadata and page content remain
in place.

| Field type | Fill value |
| --- | --- |
| Single-line text | UTF-8 string without line breaks, or `nil` to clear |
| Multiline text | UTF-8 string with explicit line breaks, or `nil` to clear |
| Checkbox | `true` or `false` |
| Radio group | One declared export-value string |
| Single choice | One declared value, including `""` if offered, or `nil` to clear |
| Multiple choice | A list of distinct declared values, or `nil` to clear |

Multiple choice is supported for existing PDFs with the appropriate field flag.
Clearing a choice with `nil` removes its value; selecting an option with an empty
export value (`""`) keeps that option selected. Inspection includes declared
choices and export values. Unknown names,
read-only fields, invalid selections and field `MaxLen` violations return
[diagnostics](diagnostics.md).

By default, filling uses bundled DejaVu Sans and black text. Text shrinks to fit,
up to 12 points, with a two-point inset. Explicit line breaks are retained; automatic
word wrapping is not performed. PDF left, center and right alignment are
respected. Missing glyphs and widgets too small for the inset return diagnostics.
External font/style instructions without a supported template use this
appearance style.

Use `appearance: :source` to fill library-generated HTML fields with their
original embedded fonts, text style, geometry and clipping rules:

```elixir
{:ok, completed} = Forms.fill(original, %{"applicant.full_name" => "Cees Kettenis"},
  appearance: :source)
```

This policy preserves the original size rather than shrinking to fit, so long
values may be clipped. A value that the captured fonts cannot draw returns a
diagnostic. Fields without a supported source template still use the default
fitted appearance. `appearance: :fit` selects that default explicitly.

Filling a list box preserves its visible option labels and redraws selection
highlights, including when the selection is cleared. The view scrolls to reveal
the first selected option. Dropdowns continue to show their selected label.
New HTML list boxes retain row geometry, option visibility, group labels and
row fonts for `appearance: :source`. Older templates without row geometry and
external list boxes use rows reconstructed from their PDF choice entries.

Library-generated HTML fields retain a validated appearance template containing
approved embedded font bytes, text style, editor geometry and clip. The existing
`NEPUSourceEditor` PDF format remains readable for these ordinary templates;
it contains no SVG source, executable effect graph or external resource lookup.
Retained editor fonts use the same compression decision as ordinary PDF streams.
When compression would exceed the configured reader decompression ratio, the
font program is stored uncompressed.

PDFs depending on the removed custom SVG appearance programs remain inspectable,
but modifying their fields returns an `unsupported_form` diagnostic. The
existing SVG converter does not support foreignObject HTML; successful
foreignObject/widget rendering remains a compatibility blocker. Ordinary HTML
fields and generic PDF appearance composition do not use that removed engine.

Checkbox and radio fills preserve their existing Off/on artwork. Fields
generated by this library retain their background and border artwork when
filled. Existing external text-field decorations inside an old appearance are
not reconstructed. All filled fields have self-contained appearances and do
not depend on a viewer regenerating them.

Partial fills preserve the existing form-wide `NeedAppearances` request because
untouched appearances may be missing or stale. Filling every field clears the
request after preparing appearances for all fields.

## Flatten selected fields

```elixir
{:ok, flat_name} = Forms.flatten(completed, fields: ["applicant.full_name"])
{:ok, filled_and_flat} = Forms.fill(original,
  %{"applicant.full_name" => "Cees Kettenis"}, flatten: true)
```

`flatten: true` flattens only the fields supplied to that fill call.
`Forms.flatten(pdf)` flattens all supported fields. `fields: []` selects none.
Flattening paints current appearances into page content, removes selected
widgets and prunes their field tree. Remaining fields stay interactive.
Selected widgets are painted in each page's annotation order, preserving their
stacking relative to one another even when the field tree has a different order.
Appearance transforms, page rotation and existing resource names are preserved.
Flattening preserves screen visibility. Widgets marked Hidden or NoView are
removed without painting their appearances, including fields whose widgets
are all hidden. The Print flag does not change this screen policy;
print-only widgets are omitted and screen-only widgets become ordinary page
content. Invisible does not hide the supported Widget annotation type.
Missing or degenerate appearances on visible widgets return errors.

Updates are incremental. Earlier field values and attachments can remain in
historical revisions. Flattening removes active interactivity and is not secure
redaction.

## Unsupported structures

XFA, pushbuttons, signature operations, comb/rich-text fields,
editable combo boxes, field/widget actions and widget-specific rotation are
unsupported. Changing signed PDFs or PDFs with signature permissions is
rejected. Encrypted input follows the reader's existing rejection policy.
Malformed field trees, ambiguous names and widgets not uniquely associated
with a page fail with diagnostics. Automatic detection or conversion of drawn
or scanned forms is outside this API.

Selected fields and widgets with either primary `/A` or additional `/AA`
actions are rejected, including indirect actions and action chains. Inspection
and editing unrelated fields do not certify a PDF as safe to open. Filling,
flattening, merging and other edits are not document sanitization; applications
must apply their own policy to uploaded PDFs before distributing them.

See [resource limits](resource-limits.md#forms-and-attachments) for field,
appearance-object and byte budgets, and [attachments](pdf-attachments.md) for
embedding supporting files.


HTML password inputs produce Text fields with the PDF Password flag. Initial and
newly filled appearances show grapheme bullets. `Forms.fields/1` returns their
stored values, and readonly/disabled fields retain the usual fill diagnostic.
Filling externally supplied password Text fields is supported; flattening preserves
current screen appearances. Stored values and earlier incremental revisions are
retained, so these operations do not redact password data.


HTML controls support boolean `required` attributes and initial `:required` /
`:optional` styling for the existing supported control types. Empty initial values
remain valid rendering inputs. Editable required fields carry the native PDF
Required flag; disabled and readonly fields retain their existing read-only flags.
Named radio widgets share a requirement when any radio in their actual HTML form
owner group is required. CSS matching still follows each button's own attribute.
This does not add submission actions or complete HTML constraint validation.

`output` retains its descendant text, form owner, label association and ordered
source references. It creates no PDF field. Inapplicable `disabled` and
`required` metadata stay available to selectors without changing its value or
constraint-validation candidacy.

`datalist` collects options and suggestions from its actual source tree. Its
initial display is hidden; authored CSS can expose option rows or ordinary
fallback content and controls. Visible options use their applicable label or
normalized descendant text. A mutable input with a valid `list` association
paints its closed initial text in `forms: :static`, without selecting a suggestion
or painting a menu. Readonly or disabled inputs retain scalar field transport.
Horizontal range inputs paint enabled, valid suggested values as static tick
marks without changing the initial value. Bounds and decimal step state filter
the ticks; labels remain source metadata. Interactive mutable suggestion
selection returns its existing diagnostic. Suggestion labels and values never
load resources.

Ordinary controls use inline-block flow inside HTML ancestry. Normalized
standalone control fragments keep their established root block layout. Authored
CSS can override either default. Clipped text editors retain their text baseline.

Initial constraint state includes required, email/absolute-URL type, Unicode-v
pattern, numeric/temporal bounds and step checks. Initial authored values have no
user-edit dirty state, so minlength/maxlength do not set length failures.
`:valid` and `:invalid` match validation candidates. Forms aggregate their owned
candidates, including external controls; fieldsets aggregate same-tree descendant
candidates. A fieldset is itself barred and retains its own valid, empty
ValidityState even when its aggregate selector is invalid. Disabled, readonly and
other barred failures do not invalidate an aggregate. Constraint-invalid initial
values still render and retain their canonical PDF values. Filling a PDF does
not rerun these HTML initial-state checks. Submission and user interaction remain
outside this static profile.
