# PDF Tokenizer

`NativeElixirPdfUtilities.Tokenizer` converts a PDF byte stream into lexical
tokens without resolving the document structure. It is useful for inspecting
PDF syntax or building advanced PDF tooling. Applications that need active
objects, decoded streams, page traversal, text extraction, or merging should
normally use `NativeElixirPdfUtilities.Pdf.Reader`,
`NativeElixirPdfUtilities.Text`, or `NativeElixirPdfUtilities.Merge` instead.

## Tokenizing a binary

Create tokenizer state with `new/1`, then consume one token at a time:

```elixir
alias NativeElixirPdfUtilities.Tokenizer

state = Tokenizer.new("<< /Type /Example /Count 2 >>")

{first_token, state} = Tokenizer.next(state)
{second_token, _state} = Tokenizer.next(state)

first_token
#=> :dict_start

second_token
#=> {:name, "Type"}
```

`peek/1` returns the next token without advancing the supplied state.
`tokenize_all/1` consumes the remaining input and returns every token except
the final `{:eof, nil}` marker.

The tokenizer recognizes integers, real numbers, booleans, `null`, PDF names,
literal and hexadecimal strings, arrays, dictionaries, indirect-reference
markers, structural keywords, content operators, and stream data. Whitespace
and PDF comments are skipped. Hexadecimal escapes in names and escapes in
literal strings are decoded.

## Byte spans

Use `next_with_span/1` or `tokenize_all_with_spans/1` when the original byte
range is needed:

```elixir
state = Tokenizer.new("/Title (Report)")

[
  {{:name, "Title"}, %{from: 0, to: 6, stream_mode?: nil}},
  {{:string, "Report"}, %{from: 7, to: 15, stream_mode?: nil}}
] = Tokenizer.tokenize_all_with_spans(state)
```

`:from` is inclusive and `:to` is exclusive. For stream data, the span starts
after the end-of-line sequence following the `stream` keyword. The
`:stream_mode?` field reports whether the data boundary came from a direct
length or from scanning for `endstream`.

## Stream lengths

After the tokenizer emits `:stream`, `pending_stream_length/1` reports the
length information found in the preceding dictionary:

- `{:direct, length}` for a non-negative direct `/Length`
- `{:indirect, {object, generation}}` for an indirect `/Length`
- `:unknown` when no length hint is available

The tokenizer uses a direct length when available. It can report an indirect
length reference, but it does not resolve that reference; unresolved stream
data is located by scanning for `endstream`. Use the PDF reader when stream
boundaries must be validated against resolved document objects.

## Errors and boundaries

Malformed lexical input is returned as an `{:error, reason}` token. Tokenizer
errors include the relevant byte position, but this low-level API does not use
the shared diagnostic tuple returned by the reader, merger, text extractor, and
HTML renderer.

`new/1` expects a binary, and the tokenizer does not impose the process-wide PDF
input or document-complexity limits. Callers using it directly are responsible
for bounding input size and token-consumption work. The shared PDF reader
applies the configured limits and validates cross-reference revisions, active
object generations, indirect references, streams, encryption, and page trees.
