# Native Elixir PDF Utilities

A small, native Elixir toolkit for working with classic PDFs:

- A fast, pure-Elixir tokenizer that turns a PDF byte stream into tokens.
- A pragmatic PDF merger that renumbers objects, collects pages, and writes a
  fresh xref/trailer to produce a valid combined PDF.

No NIFs, no ports, no external binaries just Elixir. Targeted at structural tasks
and best-effort merging for common PDFs.

## Status

- Elixir: `~> 1.18`
- Version: `0.1.0` (API may evolve)

---

## Features

- Tokenizer:

  - Numbers (integers/reals), names (`/Name` with `#xx` hex escapes), strings (literal and hex).
  - PDF keywords and punctuation: `obj`, `endobj`, `stream`, `endstream`, `xref`, `trailer`,
    `startxref`, `R`, `<<`, `>>`, `[`, `]`.
  - Operators surfaced as `{:op, word}` for content streams.
  - Stream handling: emits `:stream` then `{:stream_data, binary}` using `/Length` when present,
    or scans up to `endstream` as fallback. Provides span info via `next_with_span/1`.
- Merger:

  - Merges multiple PDF binaries: renumbers objects to avoid collisions.
  - Rewrites indirect references to the new numbering.
  - Builds a new `Catalog` and `Pages` tree and collects all page objects.
  - Preserves stream bytes and declared `/Length` (direct or indirect reference hints).
  - Emits a classic xref table and trailer (not an xref stream).

## Installation

This project is not yet published on Hex. Add it as a local dependency or use a
Git reference once public.

Local path (for development):

```elixir
def deps do
  [
    {:native_elixir_pdf_utilities, path: "../native_elixir_pdf_utilities"}
  ]
end
```

When the package is published to Hex, it will look like:

```elixir
def deps do
  [
    {:native_elixir_pdf_utilities, "~> 0.1.0"}
  ]
end
```

---

## Quick Start

Interactive shell:

```bash
iex -S mix
```

Tokenize a PDF binary:

```elixir
alias NativeElixirPdfUtilities.Tokenizer

pdf = File.read!("sample.pdf")
st = Tokenizer.new(pdf)
tokens = Tokenizer.tokenize_all(st)
IO.inspect(tokens, limit: 50)
```

Merge PDFs:

```elixir
alias NativeElixirPdfUtilities.Merge

bins = [
  File.read!("a.pdf"),
  File.read!("b.pdf")
]

{:ok, merged} = Merge.merge(bins)
File.write!("merged.pdf", merged)
```

---

## Tokenizer API

Module: `NativeElixirPdfUtilities.Tokenizer`

- `new(binary)`: Initialize with a PDF byte stream.
- `next(t)`: Return `{token, t2}` for the next token; emits `{:eof, nil}` at end.
- `peek(t)`: Look at the next token without advancing.
- `next_with_span(t)`: Like `next/1` but also returns byte-span metadata for the token
  (`%{from: pos, to: pos, stream_mode?: :length | :scanned | nil}`).
- `tokenize_all(t)`: Tokenize all tokens into a list.
- `tokenize_all_with_spans(t)`: Tokenize all tokens with spans included.
- `pending_stream_length(t)`: If just saw `:stream`, returns `{:direct, int}` when `/Length`
  was a direct int, `{:indirect, {obj, gen}}` for an indirect hint, or `:unknown`.

Token forms include:

- `{:int, integer}` | `{:real, float}` | `{:name, binary}` | `{:string, binary}`
- `{:hex_string, binary}` | `{:stream_data, binary}` | `{:op, binary}`
- `:lbracket` | `:rbracket` | `:dict_start` | `:dict_end` | `:R`
- `:obj` | `:endobj` | `:stream` | `:endstream` | `:xref` | `:trailer` | `:startxref`
- `:null` | `true` | `false` | `{:eof, nil}`

Notes:

- Whitespace and `%` comments are skipped.
- Literal strings support escapes (`\n`, `\r`, `\t`, `\b`, `\f`, `\\`, octal) and nested parentheses.
- Hex strings `<...>` are decoded; odd nibble counts are padded per PDF spec.
- For streams, one EOL immediately after `stream` is not part of stream data.

---

## Merging PDFs

Module: `NativeElixirPdfUtilities.Merge`

- `merge([pdf_bin]) :: {:ok, pdf_bin}`
  - Indexes each input into objects and page ids using the tokenizer.
  - Assigns non-overlapping id ranges and remaps indirect references.
  - Builds a fresh `Pages` (object 1) and `Catalog` (object 2) and appends all input objects.
  - Constructs a classic xref table and trailer pointing to the generated root.

Example:

```elixir
{:ok, out} = NativeElixirPdfUtilities.Merge.merge([
  File.read!("doc1.pdf"),
  File.read!("doc2.pdf"),
  File.read!("doc3.pdf")
])
File.write!("merged.pdf", out)
```

Behavior and constraints:

- Preserves all object bodies and stream bytes; does not decode or re-encode filters.
- Uses `/Length` when the value is a direct integer. For indirect lengths, keeps the reference
  and scans to `endstream` when emitting token streams.
- Expects classic PDFs (xref tables). PDFs using only xref streams or incremental updates
  may not be fully handled yet.
- Rewrites Page dictionaries to ensure a valid tree: sets `Parent`, keeps or injects `Resources`
  and `MediaBox` when missing; builds a combined `Pages` tree.

---

## Running Tests

```bash
mix test
```

The test suite exercises the tokenizer (numbers, names, strings, dicts, arrays,
operators, streams via `/Length` and fallback scanning).

---

## Roadmap / Ideas

- Include more PDF utilities as I think of them, or suggested by the community.
- Make it able to handle more kinds of PDF's.

---

## License

MIT. See `LICENSE`.
