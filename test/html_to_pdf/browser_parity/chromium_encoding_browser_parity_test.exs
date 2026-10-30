defmodule NativeElixirPdfUtilities.HtmlToPdf.ChromiumEncodingBrowserParityTest do
  use ExUnit.Case, async: true

  alias NativeElixirPdfUtilities.TestSupport.{Paths, PdfVisualCompare}
  @moduletag :browser_parity

  test "generated references preserve UTF-8 after a long ASCII preamble with or without a BOM" do
    directory = Paths.project("tmp/browser_parity/chromium_encoding")
    File.mkdir_p!(directory)

    for {name, prefix} <- [{"undeclared", ""}, {"declared", <<239, 187, 191>>}] do
      fixture = Path.join(directory, name <> ".html")

      source =
        prefix <>
          "<!doctype html><html><head><!--" <>
          String.duplicate("ASCII preamble ", 2_048) <>
          """
          --><style>
          @page {size:240pt 80pt;margin:0}
          body {margin:12pt;font-size:18pt;font-family:'DejaVu Sans'}
          </style></head><body>Café Ω Кирилл</body></html>
          """

      File.write!(fixture, source)

      stats =
        PdfVisualCompare.assert_browser_match!(fixture,
          artifact_dir: Path.join(directory, name),
          render_opts: [system_font_discovery: false],
          max_changed_ratio: 0.01,
          max_average_delta: 0.01
        )

      assert File.read!(fixture) == source

      assert <<239, 187, 191, "<!doctype", _rest::binary>> =
               File.read!(Path.join(stats.artifact_dir, "chromium-input.html"))

      [frame] =
        stats.artifact_dir
        |> Path.join("chromium.pdf.readiness.json")
        |> File.read!()
        |> :json.decode()

      assert frame["encoding"] == "UTF-8"

      for renderer <- ["chromium", "native"] do
        {text, 0} =
          System.cmd("pdftotext", [Path.join(stats.artifact_dir, renderer <> ".pdf"), "-"])

        assert String.trim(text) == "Café Ω Кирилл"
      end
    end
  end
end
