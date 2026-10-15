Code.require_file("../support/svg_public_workflow.ex", __DIR__)
Code.require_file("../support/png_fixture.ex", __DIR__)

defmodule NativeElixirPdfUtilities.HtmlToPdf.SvgNestedTextLengthTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.{Limits}

  setup do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    %{data: File.read!("priv/fonts/dejavu/DejaVuSans.ttf"), limits: limits}
  end

  test("ancestral spacing distributes over actual nested selected glyphs", %{data: data}) do
    source =
      "<svg width='240' height='80'><text x='20' y='40' font-size='20' font-family='NativeSvgFont0' textLength='120'>AB<tspan font-family='NativeSvgFont0'>CD</tspan>EF</text></svg>"

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

    assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf)

    assert {:error, {:no_extractable_text, _}} =
             NativeElixirPdfUtilities.Text.extract(pdf, layout: false)

    assert {:ok, png} =
             NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.png(source, [], %{
               mode: :inline,
               fonts: [%{data: data}]
             })

    assert png != <<>>
  end

  test("ancestor spacing follows resolved descendant character boundaries", %{data: data}) do
    source =
      "<svg width='240' height='80'><text x='20' y='40' font-size='20' font-family='NativeSvgFont0' textLength='120'>AB<tspan font-family='NativeSvgFont0' textLength='40'>CD</tspan>EF</text></svg>"

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

    assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf)

    assert {:error, {:no_extractable_text, _}} =
             NativeElixirPdfUtilities.Text.extract(pdf, layout: false)
  end

  test("multiple nested spacing owners use actual source hierarchy", %{data: data}) do
    source =
      "<svg width='280' height='90'><text x='20' y='40' font-size='20' font-family='NativeSvgFont0' textLength='180'>A<tspan font-family='NativeSvgFont0' textLength='100'>B<tspan font-family='NativeSvgFont0' textLength='40'>CD</tspan>E</tspan>F</text></svg>"

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

    assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf)

    assert {:error, {:no_extractable_text, _}} =
             NativeElixirPdfUtilities.Text.extract(pdf, layout: false)
  end

  test("nested spacing preserves original Unicode, source IDs and actual selected fonts", %{
    data: data
  }) do
    source =
      "<svg width='240' height='80'><text id='original-root' x='20' y='40' font-size='20' font-family='NativeSvgFont0' textLength='120'>AΩ<tspan id='original-child' font-family='NativeSvgFont0' textLength='40'>éD</tspan>EF</text></svg>"

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

    assert {:ok, _doc} = NativeElixirPdfUtilities.Pdf.Reader.read(pdf)
  end
end
