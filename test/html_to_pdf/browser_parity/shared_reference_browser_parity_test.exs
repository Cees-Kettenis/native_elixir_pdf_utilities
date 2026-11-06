defmodule NativeElixirPdfUtilities.HtmlToPdf.SharedReferenceBrowserParityTest do
  use ExUnit.Case, async: true
  @moduletag :browser_parity
  alias NativeElixirPdfUtilities.TestSupport.{Paths, PdfVisualCompare}

  test "reference sharing preserves separate native checks and rejects changed browser identities" do
    root = Path.expand("tmp/browser_parity/shared-reference-contract")
    File.mkdir_p!(root)
    fixture = Path.join(root, "source.html")

    source = """
    <!doctype html><html><head><style>
    @page {size:180pt 120pt;margin:0}
    html,body {margin:0;font-family:'DejaVu Sans';font-size:12pt}
    p {margin:0;color:#174a86}
    </style></head><body><p>Shared reference</p></body></html>
    """

    File.write!(fixture, source)
    reference = PdfVisualCompare.prepare_browser_reference!(fixture)
    opts = [reference: reference, max_changed_ratio: 0.01, max_average_delta: 0.01]

    for mode <- [:static, :interactive] do
      stats =
        PdfVisualCompare.assert_browser_match!(
          fixture,
          opts ++ [artifact_dir: Path.join(root, to_string(mode)), render_opts: [forms: mode]]
        )

      assert stats.page_count == 1
      assert stats.embedded_fonts.chromium == reference.embedded_fonts

      assert File.read!(Path.join(stats.artifact_dir, "chromium.pdf")) ==
               File.read!(reference.chromium_pdf)
    end

    for directory <- [
          reference.artifact_dir,
          Path.dirname(reference.artifact_dir),
          Path.join(reference.artifact_dir, "variant")
        ] do
      assert_raise ExUnit.AssertionError, ~r/output directory overlaps/, fn ->
        PdfVisualCompare.assert_browser_match!(fixture, opts ++ [artifact_dir: directory])
      end

      assert File.exists?(reference.chromium_pdf)
    end

    for {name, changed} <- [
          {"density", [dpi: 96]},
          {"geometry", [render_opts: [page_size: {200, 120}]]},
          {"resources", [render_opts: [base_url: Paths.fixture("html_to_pdf")]]},
          {"fonts", [render_opts: [fonts: []]]},
          {"adapter", [svg_reference: :png]},
          {"transformed source", [chromium_transform: &String.replace(&1, "Shared", "Changed")]}
        ] do
      assert_raise ExUnit.AssertionError, ~r/reference identity differs/, fn ->
        PdfVisualCompare.assert_browser_match!(
          fixture,
          Keyword.merge(opts, changed ++ [artifact_dir: Path.join(root, name)])
        )
      end
    end

    File.write!(fixture, String.replace(source, "Shared", "Changed"))

    assert_raise ExUnit.AssertionError, ~r/reference identity differs/, fn ->
      PdfVisualCompare.assert_browser_match!(
        fixture,
        opts ++ [artifact_dir: Path.join(root, "source-change")]
      )
    end

    File.write!(fixture, source)
    File.write!(reference.chromium_pdf, "changed artifact")

    assert_raise ExUnit.AssertionError, ~r/reference artifact changed/, fn ->
      PdfVisualCompare.assert_browser_match!(
        fixture,
        opts ++ [artifact_dir: Path.join(root, "artifact-change")]
      )
    end
  end
end
