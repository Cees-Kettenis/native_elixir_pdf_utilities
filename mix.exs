defmodule NativeElixirPdfUtilities.MixProject do
  use Mix.Project

  def project do
    [
      app: :native_elixir_pdf_utilities,
      version: "0.1.0",
      elixir: "~> 1.18",
      start_permanent: Mix.env() == :prod,
      description: "A PDF tokenizer and utilities in pure Elixir.",
      deps: deps(),
      package: package()
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
    []
  end

  defp package do
    [
      name: "Native Elixir PDF Utilities",
      licenses: ["MIT"],
      links: %{"GitHub" => "https://github.com/Cees-Kettenis/native_elixir_pdf_utilities"}
    ]
  end
end
