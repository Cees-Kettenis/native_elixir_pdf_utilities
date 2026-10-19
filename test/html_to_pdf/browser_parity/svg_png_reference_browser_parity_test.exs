defmodule NativeElixirPdfUtilities.HtmlToPdf.SvgPngReferenceBrowserParityTest do
  use ExUnit.Case, async: true
  @moduletag :browser_parity
  alias NativeElixirPdfUtilities.{HtmlToPdf, Limits}
  alias NativeElixirPdfUtilities.TestSupport.PdfVisualCompare

  test "Chrome PNG reference detects lost artwork and changed color, size and position" do
    root = Path.expand("tmp/browser_parity/svg_png_reference_contract")
    File.mkdir_p!(root)
    fixture = Path.join(root, "source.html")

    html = """
    <!doctype html><html><head><style>
    @page { size:180pt 120pt; margin:0 }
    html,body { margin:0; padding:0 }
    svg { display:block; width:160px; height:100px }
    </style></head><body><svg viewBox="0 0 160 100"><rect x="10" y="10" width="120" height="70" fill="#174a86"/></svg></body></html>
    """

    File.write!(fixture, html)

    opts = [
      svg_reference: :png,
      render_opts: [fonts: []],
      max_changed_ratio: 0.01,
      max_average_delta: 0.01
    ]

    stats =
      PdfVisualCompare.assert_browser_match!(
        fixture,
        Keyword.put(opts, :artifact_dir, Path.join(root, "correct"))
      )

    assert stats.reference == :chromium_svg_png
    assert stats.dpi == 96 * Limits.get(:svg_raster_scale)
    assert stats.strict_vector_comparison.reference == :chromium_vector_pdf
    assert File.exists?(Path.join(stats.artifact_dir, "chromium-vector.pdf"))
    assert File.exists?(Path.join(stats.artifact_dir, "strict-vector-stats.exs"))

    for {name, original, replacement} <- [
          {"color", "#174a86", "#ff0000"},
          {"size", "width:160px", "width:100px"},
          {"position", "display:block", "display:block;margin-left:30px"},
          {"missing", "<rect x=", "<rect display=\"none\" x="}
        ] do
      assert_raise ExUnit.AssertionError, ~r/changed-pixel threshold exceeded/, fn ->
        PdfVisualCompare.assert_browser_match!(
          fixture,
          opts ++
            [
              artifact_dir: Path.join(root, name),
              native_renderer: fn source, render_opts ->
                HtmlToPdf.render(String.replace(source, original, replacement), render_opts)
              end
            ]
        )
      end
    end
  end
end
