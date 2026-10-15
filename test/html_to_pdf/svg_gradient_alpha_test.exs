Code.require_file("../support/svg_public_workflow.ex", __DIR__)
Code.require_file("../support/png_fixture.ex", __DIR__)

defmodule NativeElixirPdfUtilities.HtmlToPdf.SvgGradientAlphaTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.{Limits}
  alias NativeElixirPdfUtilities.Pdf.Reader

  setup do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    %{limits: limits, data: File.read!("priv/fonts/dejavu/DejaVuSans.ttf")}
  end

  test("varying stop opacity has a separate aligned shading over genuine selected CID glyphs", %{
    data: data
  }) do
    for gradient <- [
          "linearGradient id='g' gradientUnits='userSpaceOnUse' x1='10' x2='220' gradientTransform='rotate(8 10 40)'",
          "radialGradient id='g' cx='.4' cy='.5' r='.7'"
        ] do
      tag = gradient |> String.split() |> hd()

      source =
        "<svg width='300' height='100'><defs><#{gradient}><stop offset='0' stop-color='red' stop-opacity='0'/><stop offset='.4' stop-color='green' stop-opacity='.8'/><stop offset='1' stop-color='blue' stop-opacity='.2'/></#{tag}></defs><g opacity='.7' transform='translate(8 5)'><text x='10' y='50' font-family='NativeSvgFont0' font-size='30' fill='url(#g)' fill-opacity='.6' stroke='black' stroke-width='1'>Alpha ffi Ωé</text></g></svg>"

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

      assert {:ok, _doc} = Reader.read(pdf)
    end
  end

  test "constant stop alpha keeps the existing single-shading opacity program" do
    source =
      "<svg width='100' height='40'><defs><linearGradient id='g'><stop offset='0' stop-color='red' stop-opacity='.4'/><stop offset='1' stop-color='blue' stop-opacity='.4'/></linearGradient></defs><rect width='100' height='40' fill='url(#g)'/></svg>"

    assert {:ok, pdf} =
             NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.pdf(source, [], %{
               mode: :inline
             })

    refute pdf =~ "/Subtype /Link"
    assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf, false)
    assert {:ok, _doc} = Reader.read(pdf)
  end

  test "host stop CSS supplies real independent opacity and malformed opacity remains diagnostic" do
    html =
      "<style>#a{stop-opacity:.15}#b{stop-opacity:.9}</style><svg width='140' height='50'><defs><linearGradient id='g'><stop id='a' offset='0' stop-color='red'/><stop id='b' offset='1' stop-color='blue'/></linearGradient></defs><rect width='140' height='50' fill='url(#g)'/></svg>"

    assert {:ok, pdf} = NativeElixirPdfUtilities.HtmlToPdf.render(html)
    refute pdf =~ "/Subtype /Link"
    assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf, false)

    source =
      "<svg width='100' height='40'><defs><linearGradient id='g'><stop offset='0' stop-opacity='bad'/><stop offset='1'/></linearGradient></defs><rect width='100' height='40' fill='url(#g)'/></svg>"

    assert {:error, {:invalid_css, diagnostic}} =
             NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.pdf(source, [], %{
               mode: :inline
             })

    assert diagnostic.stage == :css
    assert diagnostic.reason == :invalid_css
    assert diagnostic.source != nil and diagnostic.operation == :prepare

    (
      assert diagnostic.source == "stop-opacity"
      assert diagnostic.message =~ "bad"
    )
  end
end
