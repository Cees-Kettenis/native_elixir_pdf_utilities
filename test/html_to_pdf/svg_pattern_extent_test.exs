Code.require_file("../support/svg_public_workflow.ex", __DIR__)
Code.require_file("../support/png_fixture.ex", __DIR__)

defmodule NativeElixirPdfUtilities.HtmlToPdf.SvgPatternExtentTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.{Limits}
  alias NativeElixirPdfUtilities.Pdf.Reader

  setup do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    %{limits: limits, data: File.read!("priv/fonts/dejavu/DejaVuSans.ttf")}
  end

  test(
    "opaque source background and clipped checker foreground repeat over real selected glyph ink",
    %{data: data}
  ) do
    for dimensions <- [
          "patternUnits='userSpaceOnUse' x='-2' y='3' width='12' height='10'",
          "width='.12' height='.3'"
        ] do
      source =
        "<svg width='300' height='100'><defs><pattern id='p' #{dimensions} patternTransform='rotate(12)'><rect width='32' height='20' fill='blue'/><rect width='5' height='5' fill='red'/><circle cx='4' cy='4' r='2' fill='green' fill-opacity='.4'/></pattern></defs><g transform='translate(8 5)' opacity='.7'><text x='4' y='42' font-family='NativeSvgFont0' font-size='28' fill='url(#p)' fill-opacity='.6'>office café Ω</text></g></svg>"

      assert {:ok, pdf} =
               NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.pdf(source, [], %{
                 mode: :inline,
                 fonts: [%{data: data}]
               })

      refute pdf =~ "/Subtype /Link"
      assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf, false)
      assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf)

      assert {:error, {:no_extractable_text, _}} =
               NativeElixirPdfUtilities.Text.extract(pdf, layout: false)

      assert {:ok, _document} = Reader.read(pdf)
    end
  end

  test("translucent or degenerate source backgrounds retain original tile semantics", %{
    data: data
  }) do
    for background <- [
          "<rect width='12' height='10' fill='blue' fill-opacity='.5'/>",
          "<path d='M0 0H12H0V10Z' fill='blue'/>"
        ] do
      source =
        "<svg width='300' height='60'><defs><pattern id='p' patternUnits='userSpaceOnUse' width='12' height='10'>#{background}<rect width='5' height='5' fill='red'/></pattern></defs><text y='40' font-family='NativeSvgFont0' font-size='28' fill='url(#p)'>Tile Ω</text></svg>"

      assert {:ok, pdf} =
               NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.pdf(source, [], %{
                 mode: :inline,
                 fonts: [%{data: data}]
               })

      refute pdf =~ "/Subtype /Link"
      assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf, false)
      assert {:ok, _doc} = Reader.read(pdf)
    end
  end
end
