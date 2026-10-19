alias NativeElixirPdfUtilities.TestSupport.Paths

Code.require_file(Paths.support("svg_converted_image.ex"))
Code.require_file(Paths.support("svg_public_workflow.ex"))
Code.require_file(Paths.support("png_fixture.ex"))

defmodule NativeElixirPdfUtilities.HtmlToPdf.SvgTextPathTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.{Limits}

  setup do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    %{data: File.read!("priv/fonts/dejavu/DejaVuSans.ttf"), limits: limits}
  end

  test "original path and tspan typography paint converter glyph pixels" do
    html =
      "<svg width=400 height=150 font-size=20><defs><path id='line' d='M20 45 H380'/><text id='label' y=100>Reuse Ωé</text></defs><text><textPath href='#line' startOffset='1em'>Path <tspan fill='#a03333'>Ωé</tspan></textPath></text><use href='#label' x='20'/></svg>"

    image = NativeElixirPdfUtilities.TestSupport.SvgConvertedImage.render(html)
    assert NativeElixirPdfUtilities.TestSupport.SvgConvertedImage.coverage(image) > 100
  end

  test("SVG2 inline paths return an explicit converter limitation", %{data: data}) do
    for attributes <- [
          "path='M20 50 H380'",
          "path='M20 50 H380' href='#other'",
          "path='' href='#other'"
        ] do
      source =
        "<svg width='400' height='100'><defs><path id='other' d='M20 50 H380'/></defs><text font-family='NativeSvgFont0' font-size='20'><textPath font-family='NativeSvgFont0' #{attributes} startOffset='10%'>Inline Ωé</textPath></text></svg>"

      for result <- [
            NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.pdf(source, [], %{
              mode: :inline,
              fonts: [%{data: data}]
            }),
            NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.png(source, [], %{
              mode: :inline,
              fonts: [%{data: data}]
            })
          ] do
        assert {:error, {:unsupported_html, diagnostic}} = result
        assert diagnostic.source == "textPath path"
        assert diagnostic.stage == :layout
        assert diagnostic.message =~ "referenced path with href"
      end
    end
  end
end
