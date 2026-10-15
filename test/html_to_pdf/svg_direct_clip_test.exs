Code.require_file("../support/svg_public_workflow.ex", __DIR__)
Code.require_file("../support/png_fixture.ex", __DIR__)

defmodule NativeElixirPdfUtilities.HtmlToPdf.SvgDirectClipTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.{Limits}
  alias NativeElixirPdfUtilities.Pdf.Reader

  setup do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    %{limits: limits}
  end

  test("direct complex link projection uses the unchanged shared limit and diagnostics", %{
    limits: limits
  }) do
    Limits.install(Map.put(limits, :max_clip_projection_work, 1))

    assert {:ok, pdf} =
             NativeElixirPdfUtilities.HtmlToPdf.render(
               "<svg width='100' height='80'><defs><clipPath id='c'><circle cx='50' cy='40' r='35'/></clipPath></defs><a href='https://example.test'><rect width='100' height='80' clip-path='url(#c)'/></a></svg>"
             )

    assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf)
    refute pdf =~ "/Subtype /Link"
  end

  test "direct native ring links preserve integer contour instructions and genuine holes" do
    source =
      "<svg width='100' height='80'><defs><clipPath id='c'><path d='M0 0H100V80H0Z M30 20H70V60H30Z' clip-rule='evenodd'/></clipPath></defs><a href='https://example.test/ring'><rect width='100' height='80' clip-path='url(#c)'/></a></svg>"

    assert {:ok, pdf} =
             NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.pdf(source, [], %{
               mode: :inline
             })

    refute pdf =~ "/Subtype /Link"
    assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf, false)
    assert {:ok, _doc} = Reader.read(pdf)
  end

  test "selected-font clipped text and unclipped siblings retain targets and real paint order" do
    html =
      "<svg width='300' height='100'><defs><clipPath id='c'><polygon points='0,0 140,0 70,90'/></clipPath></defs><a href='https://example.test/clipped'><text x='10' y='35' font-size='25' clip-path='url(#c)'>office Ω</text></a><a href='#target'><rect x='180' y='10' width='40' height='30'/></a><text id='target' x='180' y='80'>Destination</text></svg>"

    assert {:ok, pdf} = NativeElixirPdfUtilities.HtmlToPdf.render(html)
    refute pdf =~ "/Subtype /Link"
    assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf, false)
    assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf)

    assert {:error, {:no_extractable_text, _}} =
             NativeElixirPdfUtilities.Text.extract(pdf, layout: false)

    assert {:ok, _doc} = Reader.read(pdf)
  end

  test "actual selected-font text clips preserve their contours without emitting clip text" do
    data = File.read!("priv/fonts/dejavu/DejaVuSans.ttf")

    source =
      "<svg width='100' height='80' font-family='NativeSvgFont0'><defs><clipPath id='c'><text x='10' y='60' font-size='60' font-family='NativeSvgFont0'>O</text></clipPath></defs><a href='https://example.test/font-clip'><rect width='80' height='70' clip-path='url(#c)'/></a><text x='60' y='75' font-size='8' font-family='NativeSvgFont0'>Sibling</text></svg>"

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
