defmodule NativeElixirPdfUtilities.MixProject do
  use Mix.Project

  def project do
    [
      app: :native_elixir_pdf_utilities,
      version: "0.2.0",
      elixir: "~> 1.18",
      start_permanent: Mix.env() == :prod,
      description: "A PDF tokenizer and utilities in pure Elixir.",
      deps: deps(),
      package: package(),
      docs: docs(),
      test_coverage: [summary: [threshold: 100]]
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
      files: ~w(lib assets mix.exs README.md CHANGELOG.md LICENSE),
      licenses: ["MIT"],
      links: %{"GitHub" => "https://github.com/Cees-Kettenis/native_elixir_pdf_utilities"}
    ]
  end

  defp docs do
    [
      main: "readme",
      extras: ["README.md", "CHANGELOG.md"],
      source_ref: "v0.2.0",
      source_url: "https://github.com/Cees-Kettenis/native_elixir_pdf_utilities"
    ]
  end
end
