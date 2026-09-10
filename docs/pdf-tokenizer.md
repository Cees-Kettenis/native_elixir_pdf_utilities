# Reading PDF syntax tokens

Use `NativeElixirPdfUtilities.Tokenizer` for raw PDF syntax and byte positions.
Use the [PDF reader](pdf-reader.md) to resolve objects, references, and pages.

## Tokenizing a binary

```elixir
alias NativeElixirPdfUtilities.Tokenizer

state = Tokenizer.new("<< /Type /Example /Count 2 >>")
{:dict_start, state} = Tokenizer.next(state)
{{:name, "Type"}, state} = Tokenizer.next(state)
tokens = Tokenizer.tokenize_all(state)
```

`new/1` takes a binary. `next/1` returns `{token, next_state}`; `peek/1` returns
the next token without advancing. `tokenize_all/1` returns remaining tokens
without the final `{:eof, nil}` marker.

Tokens represent numbers, names, strings, booleans, null, array/dictionary
boundaries, PDF keywords, operators, and stream bytes. Whitespace and comments
are skipped; name and string escapes are decoded.

## Byte spans

```elixir
state = Tokenizer.new("/Title (Report)")

[
  {{:name, "Title"}, %{from: 0, to: 6, stream_mode?: nil}},
  {{:string, "Report"}, %{from: 7, to: 15, stream_mode?: nil}}
] = Tokenizer.tokenize_all_with_spans(state)
```

`:from` is inclusive and `:to` is exclusive in the original bytes.
`next_with_span/1` returns one token and span at a time. For stream data,
`:stream_mode?` indicates whether its boundary came from a direct length or
an `endstream` scan.

## Stream lengths

After `:stream`, `pending_stream_length/1` returns `{:direct, length}`,
`{:indirect, {object, generation}}`, or `:unknown`. Direct lengths locate the
stream bytes; indirect lengths are not resolved by the tokenizer. Use
[Reader.decoded_stream/2](pdf-reader.md#streams) for validated, decoded content.

## Errors and boundaries

Malformed syntax returns `{:error, {reason, diagnostic}}` from all token-reading
functions, using the shared [diagnostic contract](diagnostics.md). Bulk calls
stop at the first error without returning partial tokens.

The diagnostic includes the failed operation, a syntax explanation, one-based
line and byte column, and a zero-based byte offset in its message. Positions
refer to the binary passed to `new/1`. Match errors before handling a token/state
pair or token list.

The tokenizer does not validate the PDF document or apply document-wide
resource limits. Bound input size and token consumption when using it directly.
