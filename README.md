<p align="center">
  <img src="assets/readme-banner.svg" alt="Native Elixir PDF Utilities" />
</p>

<p align="center">
  <a href="https://hex.pm/packages/native_elixir_pdf_utilities"><img src="https://img.shields.io/hexpm/v/native_elixir_pdf_utilities.svg" alt="Hex.pm" /></a> <a href="https://native-elixir-pdf-utilities.hexdocs.pm/api-reference.html"><img src="https://img.shields.io/badge/hex-docs-blue.svg" alt="HexDocs" /></a> <a href="LICENSE"><img src="https://img.shields.io/hexpm/l/native_elixir_pdf_utilities.svg" alt="License" /></a> <img src="https://img.shields.io/badge/elixir-~%3E%201.18-4B275F.svg" alt="Elixir ~> 1.18" />
</p>

# Native Elixir PDF Utilities

Native Elixir PDF Utilities is a small library for developers who need practical PDF building blocks without command line tools.

PDFs are useful, awkward, and full of edge cases. This project focuses on the common structural work that Elixir applications often need: reading PDF bytes, understanding the object stream, extracting embedded text when it is available, and combining documents in a predictable way.

The goal is not to be a full PDF engine overnight. It is a steadily improving toolkit, handled by an excited developer who wants this to become a dependable native Elixir option for day-to-day PDF utility work.

## Package and Docs

- Package: https://hex.pm/packages/native_elixir_pdf_utilities
- API docs: https://native-elixir-pdf-utilities.hexdocs.pm/api-reference.html

## What It Does

1. Tokenizer - turns classic PDF byte streams into structured Elixir tokens.
2. Merger - combines multiple PDF binaries into a fresh PDF with rewritten object references.
3. Reader - extracts embedded text from PDFs when the document contains readable text data.

## Installation

```elixir
def deps do
  [
    {:native_elixir_pdf_utilities, "~> 0.1.0"}
  ]
end
```

## Development

Run the project checks before contributing:

```bash
mise exec -- mix test
MIX_ENV=test mise exec -- mix dialyzer
mise exec -- mix format
```

## License

MIT. See `LICENSE`.
