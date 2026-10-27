# PDF forms

Use `NativeElixirPdfUtilities.Forms` to inspect, fill and flatten AcroForm fields.
HTML rendering creates these fields by default. Drawn lines and scanned boxes
have no fields to fill; use `Stamp.text/3` to place text on those PDFs.

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

`Forms.fields/1` returns a list of field maps:

| Field | Meaning |
| --- | --- |
| `:name`, `:type`, `:value` | Field key, type and current value |
| `:read_only` | `true` means filling is blocked |
| `:choices` | Choice entries with `:value` and `:label` |
| `:export_values` | Declared radio/checkbox export values |
| `:widgets` | Each widget's one-based `:page` and `:rect`: `[left, bottom, right, top]` in PDF coordinates |

## HTML field names and compatibility

Controls become editable fields by default. Use `forms: :static` for artwork only.

| HTML control | PDF field |
| --- | --- |
| Text, search, telephone, URL, email and number inputs | Text |
| Password | Masked text; stored value remains readable |
| Textarea | Multiline text |
| Checkbox / radio | Checkbox / radio group |
| Single / multiple select | Choice field |
| Buttons and submit/reset/button inputs | Static artwork |
| Hidden input | No output |

Missing or unknown input types use Text. Type names ignore case, but whitespace
is not trimmed. See [other control restrictions](html-to-pdf-compatibility.md#forms).

### Field names

Set `name` for stable application keys. Generated names can change with pagination.

| Rule | Behavior |
| --- | --- |
| Explicit name | Nonempty HTML `name` becomes the key; `id` does not |
| Generated names | `TEXT_1_1`, `CHECKBOX_1_2`, `RADIO_2_1`, `CHOICE_2_2` |
| Numbering | One-based page and control numbers, in document order after pagination |
| Collisions | Explicit names reserved first; generated collisions gain `_1`, `_2`, etc. |
| Duplicates | Rejected, except radios in the same group |
| HTML name restrictions | No period or NUL. Existing PDF names may use periods for nesting |
| Radio groups | Follow HTML form ownership; separate owners need distinct PDF names |
| Radio values | Distinct, nonempty strings; `Off` is reserved |
| Initial checked radio | Last checked member wins; unnamed radios stay independent |
| Explicit `form` reference | Must identify a form; unmatched references leave the control unowned |

### Initial values and choices

| Input | Value cleanup |
| --- | --- |
| Text, search, telephone, password | Remove CR/LF line breaks: `A&#10;B` becomes `AB` |
| URL / single email | Also trim surrounding ASCII whitespace |
| Multiple email | Remove CR/LF, trim each address, rejoin with commas |
| Textarea | Keep line breaks |
| Internationalized domains | No display conversion |

Invalid initial values still render. Constraints such as `required` and `pattern`
affect initial HTML selectors. Filling a PDF does not rerun HTML validation.
Editable required fields carry the PDF Required flag.

| Choice setting | Behavior |
| --- | --- |
| Dropdown selection | Last selected option; otherwise first enabled option |
| Selected disabled option | Stays selected |
| No available selection | `nil`; a selected empty option returns `""` |
| Multiple select | List of selected values |
| Option values | Must be unique; explicit `value` preserved, otherwise collapsed option text |
| Display labels | Nonempty `label`, otherwise option text |
| Disabled option groups | Affect initial selection; PDF viewers may still allow choosing those options |

### Read-only fields and placement

| Setting | Behavior |
| --- | --- |
| `disabled` | Makes the PDF field read-only |
| `readonly` | Applies to text-entry inputs and textareas; keeps normal styling |
| Boolean attributes | Presence enables them, even `readonly="false"` or `disabled="false"` |
| Read-only filling | Returns `:read_only_form_field`; flattening is allowed |
| Radio groups | All enabled or all disabled; use static mode for mixed groups |
| Placement | Controls cannot span pages or repeat in page furniture |
| Child-document controls | Editable, with separate names from `Forms.fields/1`; cannot repeat across pages or page furniture |
| Unsupported widget transforms | Return diagnostics; use static mode for artwork |
| HTML appearance | Uses HTML styling; use `[readonly]` to style read-only fields |
| Text extraction | Flatten first so values become page content |

Passwords display bullets, but `Forms.fields/1` returns their stored values.
Filling and flattening do not redact passwords or earlier revisions.

## Fill an existing PDF

```elixir
{:ok, original} = File.read("application.pdf")
{:ok, fields} = Forms.fields(original)
IO.inspect(fields)
{:ok, completed} = Forms.fill(original, %{"applicant.full_name" => "Cees Kettenis"})
```

Use exact names from `Forms.fields/1`, including any parent names and periods.
Filling a field updates all its widgets. Other fields, annotations, metadata and
page content stay in place.

| Field type | Fill value |
| --- | --- |
| Single-line text | UTF-8 string without line breaks, or `nil` to clear |
| Multiline text | UTF-8 string with explicit line breaks, or `nil` to clear |
| Checkbox | `true` or `false` |
| Radio group | One declared export-value string |
| Single choice | One declared value, including `""` if offered, or `nil` to clear |
| Multiple choice | A list of distinct declared values, or `nil` to clear; PDF must allow multiple selections |

`nil` clears a choice; `""` selects its empty-value option. Unknown names, read-only
fields, invalid choices and `MaxLen` violations return [diagnostics](diagnostics.md).

### Fill appearance

| Setting | Behavior |
| --- | --- |
| Default / `appearance: :fit` | Black DejaVu Sans, shrunk to fit, up to 12pt; 2pt inset |
| Line breaks | Explicit breaks kept; no automatic word wrapping |
| Alignment | PDF left, center or right alignment preserved |
| Unrenderable values | Missing glyphs or widgets too small for the inset return diagnostics |
| Checkboxes / radios | Keep existing Off/on artwork |
| Backgrounds and borders | Preserved for library-generated fields; old external text-field decorations are not reconstructed |
| List boxes | Keep labels and redraw highlights, even when cleared; scroll to the first selection |
| Dropdowns | Show the selected label |
| Viewer regeneration | Filled appearances are self-contained |
| `NeedAppearances` | Partial fills keep the request; filling every field clears it |

To keep a library-generated field's original fonts and styling:

```elixir
{:ok, completed} = Forms.fill(original, %{"applicant.full_name" => "Cees Kettenis"},
  appearance: :source)
```

| Source appearance | Behavior |
| --- | --- |
| Fonts and geometry | Keep original size, style and clipping; long values may be clipped |
| Missing glyphs | Return a diagnostic if the captured fonts cannot draw the value |
| No supported source template | Fall back to fitted appearance |
| New HTML list boxes | Preserve row geometry, visibility, group labels and fonts |
| Older / external list boxes | Reconstruct rows from PDF choice entries |
| Unsupported appearance programs | Inspection allowed; modifying fields returns `:unsupported_form` |

See [SVG restrictions](image-processing.md#svg-images) when combining SVG and controls.

## Flatten selected fields

```elixir
{:ok, flat_name} = Forms.flatten(completed, fields: ["applicant.full_name"])
{:ok, filled_and_flat} = Forms.fill(original,
  %{"applicant.full_name" => "Cees Kettenis"}, flatten: true)
```

| Call | Selected fields |
| --- | --- |
| `Forms.flatten(pdf)` | All supported fields |
| `Forms.flatten(pdf, fields: names)` | Only those names; `[]` selects none |
| `Forms.fill(..., flatten: true)` | Only fields supplied to that fill call |

Flattening paints selected appearances into page content and removes their widgets
and field-tree entries. Other fields remain editable.

| Detail | Behavior |
| --- | --- |
| Stacking | Uses page annotation order |
| Geometry | Preserves appearance transforms, page rotation and resource names |
| Hidden / NoView widgets | Removed without painting |
| Print flag | Screen visibility wins: print-only widgets omitted; screen-only widgets become page content |
| Invisible flag | Does not hide Widget annotations |
| Missing / degenerate visible appearance | Returns an error |

Earlier values and attachments can remain in PDF history. Flattening is not secure redaction.

## Unsupported structures

| Input | Result |
| --- | --- |
| XFA, pushbuttons, signatures, comb/rich-text fields, editable combo boxes | Unsupported |
| Field/widget actions or widget-specific rotation | Unsupported |
| Signed PDFs or signature permissions | Changes rejected |
| Encrypted PDFs | Rejected by the reader |
| Malformed trees, ambiguous names, widgets without a unique page | Diagnostic error |
| Selected field/widget `/A` or `/AA` actions | Rejected, including indirect actions and chains |

Inspection and edits do not sanitize a PDF. Apply your own upload policy before sharing it.

[Resource limits](resource-limits.md#forms-and-attachments) · [Attachments](pdf-attachments.md).
