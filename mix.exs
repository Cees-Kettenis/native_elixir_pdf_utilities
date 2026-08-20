defmodule NativeElixirPdfUtilities.MixProject do
  use Mix.Project

  def project do
    [
      app: :native_elixir_pdf_utilities,
      version: "0.12.0",
      elixir: "~> 1.19",
      start_permanent: Mix.env() == :prod,
      description:
        "Pure Elixir PDF utilities for tokenizing, merging, text extraction, and native HTML/CSS rendering.",
      deps: deps(),
      package: package(),
      docs: docs(),
      aliases: aliases(),
      test_ignore_filters: [~r/test\/support\//],
      test_coverage: [summary: [threshold: 100]]
    ]
  end

  def cli do
    [
      preferred_envs: [
        "test.browser_parity": :test
      ]
    ]
  end

  # Run "mix help compile.app" to learn about applications.
  def application do
    [
      extra_applications: [:logger],
      mod: {NativeElixirPdfUtilities.Application, []}
    ]
  end

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    [
      {:dialyxir, "~> 1.4", only: [:dev, :test], runtime: false},
      {:ex_doc, "~> 0.37", only: :dev, runtime: false},
      {:resvg, "~> 0.5.0"}
    ]
  end

  defp package do
    [
      name: "native_elixir_pdf_utilities",
      files: ~w(lib assets priv docs mix.exs README.md CHANGELOG.md LICENSE),
      licenses: ["MIT", "Bitstream-Vera", "BSD-3-Clause"],
      links: %{"GitHub" => "https://github.com/Cees-Kettenis/native_elixir_pdf_utilities"}
    ]
  end

  defp docs do
    [
      main: "readme",
      assets: %{"assets" => "assets"},
      extras: [
        "README.md",
        "CHANGELOG.md",
        "docs/licenses.md",
        "docs/diagnostics.md",
        "docs/resource-limits.md",
        "docs/pdf-tokenizer.md",
        "docs/pdf-reader.md",
        "docs/pdf-validation.md",
        "docs/text-extraction.md",
        "docs/pdf-merging.md",
        "docs/html-to-pdf-compatibility.md",
        "docs/html-to-pdf-browser-parity-coverage.md",
        "docs/html-to-pdf-examples.md"
      ],
      groups_for_extras: [
        "Reference Guides": [
          "docs/diagnostics.md",
          "docs/resource-limits.md"
        ],
        "PDF Guides": [
          "docs/pdf-tokenizer.md",
          "docs/pdf-reader.md",
          "docs/pdf-validation.md",
          "docs/text-extraction.md",
          "docs/pdf-merging.md"
        ],
        "HTML to PDF": [
          "docs/html-to-pdf-compatibility.md",
          "docs/html-to-pdf-browser-parity-coverage.md",
          "docs/html-to-pdf-examples.md"
        ]
      ],
      groups_for_modules: [
        "Public APIs": [
          NativeElixirPdfUtilities.HtmlToPdf,
          NativeElixirPdfUtilities.Merge,
          NativeElixirPdfUtilities.Text,
          NativeElixirPdfUtilities.Tokenizer,
          NativeElixirPdfUtilities.Limits
        ],
        "PDF Building Blocks": [
          NativeElixirPdfUtilities.Pdf.Reader,
          NativeElixirPdfUtilities.Pdf.GlyphName,
          NativeElixirPdfUtilities.Diagnostics
        ],
        "HTML Rendering Pipeline": [
          NativeElixirPdfUtilities.HtmlToPdf.HtmlParser,
          NativeElixirPdfUtilities.HtmlToPdf.CssParser,
          NativeElixirPdfUtilities.HtmlToPdf.Style,
          NativeElixirPdfUtilities.HtmlToPdf.Font,
          NativeElixirPdfUtilities.HtmlToPdf.FontFallback,
          NativeElixirPdfUtilities.HtmlToPdf.Layout,
          NativeElixirPdfUtilities.HtmlToPdf.Pagination,
          NativeElixirPdfUtilities.HtmlToPdf.PageGeometry,
          NativeElixirPdfUtilities.HtmlToPdf.PageFurniture,
          NativeElixirPdfUtilities.HtmlToPdf.PdfWriter
        ],
        "Validation Pipeline": [
          NativeElixirPdfUtilities.Validators.PdfValidator,
          NativeElixirPdfUtilities.Validators.MergeValidator,
          NativeElixirPdfUtilities.Validators.TextValidator,
          NativeElixirPdfUtilities.Validators.WriterValidator
        ]
      ],
      source_ref: "v0.12.0",
      source_url: "https://github.com/Cees-Kettenis/native_elixir_pdf_utilities"
    ]
  end

  defp aliases do
    [
      "test.browser_parity": "test --only browser_parity"
    ]
  end
end
