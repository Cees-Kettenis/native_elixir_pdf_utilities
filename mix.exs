defmodule NativeElixirPdfUtilities.MixProject do
  use Mix.Project

  def project do
    [
      app: :native_elixir_pdf_utilities,
      version: "0.6.0",
      elixir: "~> 1.18",
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
      extra_applications: [:logger]
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
      files: ~w(lib assets docs mix.exs README.md CHANGELOG.md LICENSE),
      licenses: ["MIT"],
      links: %{"GitHub" => "https://github.com/Cees-Kettenis/native_elixir_pdf_utilities"}
    ]
  end

  defp docs do
    [
      main: "readme",
      extras: [
        "README.md",
        "LICENSE",
        "CHANGELOG.md",
        "docs/diagnostics.md",
        "docs/pdf-reader.md",
        "docs/text-extraction.md",
        "docs/pdf-merging.md",
        "docs/html-to-pdf-compatibility.md",
        "docs/html-to-pdf-browser-parity-coverage.md",
        "docs/html-to-pdf-examples.md"
      ],
      source_ref: "v0.6.0",
      source_url: "https://github.com/Cees-Kettenis/native_elixir_pdf_utilities"
    ]
  end

  defp aliases do
    [
      "test.browser_parity": "test --only browser_parity"
    ]
  end
end
