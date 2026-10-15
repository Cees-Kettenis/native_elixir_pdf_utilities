Code.require_file("../support/svg_public_workflow.ex", __DIR__)
Code.require_file("../support/png_fixture.ex", __DIR__)

defmodule NativeElixirPdfUtilities.HtmlToPdf.SvgIndependentClipWindingTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.HtmlToPdf.{PngDecoder}
  alias NativeElixirPdfUtilities.Validators.HtmlValidator

  @fixture Path.expand(
             "../fixtures/html_to_pdf/browser_parity/inline_svg_complete_clips.html",
             __DIR__
           )
  defp rasterize_pdf(pdf) do
    directory =
      Path.join(
        System.tmp_dir!(),
        "native-clip-conformance-#{System.unique_integer([:positive])}"
      )

    File.mkdir_p!(directory)

    try do
      path = Path.join(directory, "source.pdf")
      File.write!(path, pdf)

      assert {_output, 0} =
               System.cmd(
                 "pdftoppm",
                 ["-r", "96", "-singlefile", "-png", path, Path.join(directory, "paint")],
                 stderr_to_stdout: true
               )

      assert {:ok, image} =
               PngDecoder.decode(
                 File.read!(Path.join(directory, "paint.png")),
                 HtmlValidator.new_image_budget(),
                 false
               )

      image
    after
      File.rm_rf!(directory)
    end
  end

  defp pixel(image, x, y) do
    binary_part(image.data, (y * image.width_px + x) * 3, 3)
  end

  test "the original mixed-rule clip paints every independent child interior in native vector and raster output" do
    [source | _] =
      Regex.scan(~r/<svg\b[^>]*>.*?<\/svg>/s, File.read!(@fixture)) |> Enum.map(&hd/1)

    # The fixture sizes this SVG through `body > svg { width: 360px; height:
    # 84px }`, which the extracted standalone source loses; re-attach the CSS
    # declared viewport as attributes so the raster and PDF transports render
    # the same 360x84 canvas (a bare viewBox-only SVG otherwise falls back to
    # the CSS default object size, and dimension-less SVGs fill the available
    # width in Chromium, so 360x84 is the fixture's declared size).
    source = String.replace(source, "<svg ", "<svg width='360' height='84' ", global: false)

    assert {:ok, png} =
             NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.png(
               source,
               [width: 360, height: 84],
               %{mode: :inline}
             )

    assert {:ok, raster} = PngDecoder.decode(png, HtmlValidator.new_image_budget(), false)
    assert pixel(raster, 50, 14) == <<23, 74, 134>>
    assert :binary.at(raster.alpha_data, 14 * raster.width_px + 50) == 255
    assert :binary.at(raster.alpha_data, 30 * raster.width_px + 30) == 0

    assert {:ok, pdf} =
             NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.pdf(
               source,
               [width: 360, height: 84],
               %{mode: :inline}
             )

    refute pdf =~ "/Subtype /Link"
    assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf, false)
    vector = rasterize_pdf(pdf)
    assert pixel(vector, 50, 14) == <<23, 74, 134>>
    assert pixel(vector, 30, 30) == <<255, 255, 255>>
    assert pixel(vector, 306, 14) == <<160, 51, 51>>
  end
end
