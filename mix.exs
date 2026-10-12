defmodule Mix.Tasks.Compile.SvgBackend do
  use Mix.Task.Compiler

  @doc false
  @spec run(list()) :: {:ok, list()} | {:error, list()}
  def run(_args) do
    manifest = Path.expand("native/svg_backend/Cargo.toml")
    target = Path.expand(Path.join(Mix.Project.build_path(), "svg_backend"))

    library =
      case :os.type() do
        {:win32, _} -> "native_svg_backend.dll"
        {:unix, :darwin} -> "libnative_svg_backend.dylib"
        _ -> "libnative_svg_backend.so"
      end

    nif_library =
      if match?({:win32, _}, :os.type()),
        do: "libnative_svg_backend.dll",
        else: "libnative_svg_backend.so"

    # Mix normally symlinks priv into the source tree. Keep generated native
    # artifacts inside this runtime's build, including when caches were copied.
    priv = Path.join(Mix.Project.app_path(), "priv")

    case File.read_link(priv) do
      {:ok, _source} -> File.rm!(priv)
      _ -> :ok
    end

    File.mkdir_p!(priv)
    File.cp_r!("priv", priv)
    destination = Path.join(priv, "native")
    File.mkdir_p!(destination)

    case System.cmd(
           "cargo",
           [
             "build",
             "--release",
             "--locked",
             "--manifest-path",
             manifest,
             "--target-dir",
             target
           ],
           env: [{"CARGO_HOME", Path.expand(".quality/cargo")}, {"CARGO_BUILD_JOBS", "2"}],
           stderr_to_stdout: true
         ) do
      {output, 0} ->
        if output != "", do: Mix.shell().info(output)
        File.cp!(Path.join([target, "release", library]), Path.join(destination, nif_library))
        # Register the application before the private binding's on_load callback
        # resolves priv on a fresh build, before compile.app has run.
        compile_path = Mix.Project.compile_path()
        File.mkdir_p!(compile_path)
        :code.add_patha(String.to_charlist(compile_path))
        :code.add_patha(String.to_charlist(destination))
        bindings = Path.expand("native/svg_backend/bindings.ex")
        fingerprint = :crypto.hash(:sha256, File.read!(bindings))
        binding_manifest = Path.join(target, "bindings.sha256")

        beam =
          Path.join(
            destination,
            "Elixir.NativeElixirPdfUtilities.HtmlToPdf.SvgBackend.NativeBytes.beam"
          )

        if File.read(binding_manifest) != {:ok, fingerprint} or not File.exists?(beam) do
          for {module, binary} <- Code.compile_file(bindings) do
            File.write!(Path.join(destination, Atom.to_string(module) <> ".beam"), binary)
          end

          File.write!(binding_manifest, fingerprint)
        end

        {:ok, []}

      {output, _status} ->
        Mix.shell().error(output)
        {:error, []}
    end
  end
end

defmodule NativeElixirPdfUtilities.MixProject do
  use Mix.Project

  def project do
    build_root =
      System.get_env("MIX_BUILD_PATH") || Path.join("_build", Atom.to_string(Mix.env()))

    application_root = Path.join([build_root, "lib", "native_elixir_pdf_utilities"])

    [
      app: :native_elixir_pdf_utilities,
      version: "1.0.0",
      elixir: "~> 1.19",
      start_permanent: Mix.env() == :prod,
      description:
        "Pure Elixir utilities for PDF inspection, metadata, tokenizing, merging, page transforms, splitting, stamping, forms, attachments, outlines, text extraction, and native HTML/CSS rendering.",
      deps: deps(),
      compilers: [:svg_backend] ++ Mix.compilers(),
      # Analyze the tracked private NIF binding as well as application modules.
      dialyzer: [
        paths: [Path.join(application_root, "ebin"), Path.join(application_root, "priv/native")]
      ],
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
        "test.browser_parity": :test,
        "test.performance": :test
      ]
    ]
  end

  # Run "mix help compile.app" to learn about applications.
  def application do
    [
      extra_applications: [:logger, :xmerl],
      mod: {NativeElixirPdfUtilities.Application, []}
    ]
  end

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    [
      {:dialyxir, "~> 1.4", only: [:dev, :test], runtime: false},
      {:elixir_font_discovery, "~> 0.2.0"},
      {:ex_doc, "~> 0.37", only: :dev, runtime: false},
      {:resvg, "~> 0.6.0"}
    ]
  end

  defp package do
    [
      name: "native_elixir_pdf_utilities",
      files:
        ~w(lib native assets priv/fonts priv/licenses docs mix.exs README.md CHANGELOG.md LICENSE),
      licenses: ["MIT", "Bitstream-Vera", "BSD-3-Clause", "OFL-1.1"],
      links: %{"GitHub" => "https://github.com/Cees-Kettenis/native_elixir_pdf_utilities"}
    ]
  end

  defp docs do
    [
      main: "readme",
      assets: %{"assets" => "assets", "docs/assets" => "assets"},
      extras: [
        "README.md",
        "CHANGELOG.md",
        {"docs/README.md", [filename: "user-guide"]},
        "docs/licenses.md",
        "docs/diagnostics.md",
        "docs/resource-limits.md",
        "docs/image-processing.md",
        "docs/pdf-tokenizer.md",
        "docs/pdf-reader.md",
        "docs/pdf-information.md",
        "docs/pdf-validation.md",
        "docs/text-extraction.md",
        "docs/pdf-merging.md",
        "docs/pdf-page-transforms.md",
        "docs/pdf-outlines.md",
        "docs/pdf-stamping.md",
        "docs/pdf-forms.md",
        "docs/pdf-attachments.md",
        "docs/html-to-pdf-compatibility.md",
        "docs/html-to-pdf-browser-parity-coverage.md",
        "docs/html-to-pdf-examples.md"
      ],
      groups_for_extras: [
        "Reference Guides": [
          "docs/diagnostics.md",
          "docs/resource-limits.md",
          "docs/image-processing.md"
        ],
        "PDF Guides": [
          "docs/pdf-tokenizer.md",
          "docs/pdf-reader.md",
          "docs/pdf-information.md",
          "docs/pdf-validation.md",
          "docs/text-extraction.md",
          "docs/pdf-merging.md",
          "docs/pdf-page-transforms.md",
          "docs/pdf-outlines.md",
          "docs/pdf-stamping.md",
          "docs/pdf-forms.md",
          "docs/pdf-attachments.md"
        ],
        "HTML to PDF": [
          "docs/html-to-pdf-compatibility.md",
          "docs/html-to-pdf-browser-parity-coverage.md",
          "docs/html-to-pdf-examples.md"
        ]
      ],
      groups_for_modules: [
        "Public APIs": [
          NativeElixirPdfUtilities.Attachments,
          NativeElixirPdfUtilities.Forms,
          NativeElixirPdfUtilities.HtmlToPdf,
          NativeElixirPdfUtilities.Info,
          NativeElixirPdfUtilities.Limits,
          NativeElixirPdfUtilities.Merge,
          NativeElixirPdfUtilities.Outlines,
          NativeElixirPdfUtilities.Split,
          NativeElixirPdfUtilities.Stamp,
          NativeElixirPdfUtilities.Text,
          NativeElixirPdfUtilities.Tokenizer,
          NativeElixirPdfUtilities.Transform
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
          NativeElixirPdfUtilities.Validators.InfoValidator,
          NativeElixirPdfUtilities.Validators.MergeValidator,
          NativeElixirPdfUtilities.Validators.OutlineValidator,
          NativeElixirPdfUtilities.Validators.TextValidator,
          NativeElixirPdfUtilities.Validators.WriterValidator
        ]
      ],
      source_ref: "v1.0.0",
      source_url: "https://github.com/Cees-Kettenis/native_elixir_pdf_utilities"
    ]
  end

  defp aliases do
    [
      "test.browser_parity": "test --only browser_parity",
      "test.performance": "run scripts/performance-regression.exs"
    ]
  end
end
