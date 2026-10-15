Code.require_file("../support/svg_public_workflow.ex", __DIR__)
Code.require_file("../support/png_fixture.ex", __DIR__)

defmodule NativeElixirPdfUtilities.HtmlToPdf.SvgStrokeAdjustmentTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.{Limits}
  alias NativeElixirPdfUtilities.Pdf.Reader

  setup do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    %{limits: limits}
  end

  test "native solid shape strokes issue registered adjustment and independent stroke alpha" do
    for {cap, join, _cap_operator, _join_operator} <- [
          {"butt", "miter", "0 J", "0 j"},
          {"round", "round", "1 J", "1 j"},
          {"square", "bevel", "2 J", "2 j"}
        ] do
      source =
        "<svg width='240' height='120'><path d='M10 40 H100 L180 80' fill='none' stroke='#174a86' stroke-width='3' stroke-opacity='.4' stroke-linecap='#{cap}' stroke-linejoin='#{join}' stroke-miterlimit='5'/></svg>"

      assert {:ok, pdf} =
               NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.pdf(source, [], %{
                 mode: :inline
               })

      refute pdf =~ "/Subtype /Link"
      assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf, false)
      assert {:ok, document} = Reader.read(pdf)
      [_page] = document.pages

      assert {:ok, _png} =
               NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.png(source, [], %{
                 mode: :inline
               })
    end
  end

  test "shape adjustment resources remain bound inside imported opacity and clipping Forms" do
    html =
      "<svg width='240' height='120'><defs><clipPath id='c'><rect x='5' y='5' width='210' height='105'/></clipPath></defs><g opacity='.5' clip-path='url(#c)' transform='translate(5 5)'><path d='M10 35 H180' fill='none' stroke='#174a86' stroke-width='3' stroke-opacity='.4'/><path d='M10 65 H180' fill='none' stroke='#a03333' stroke-width='3' stroke-opacity='.7'/></g></svg>"

    assert {:ok, pdf} = NativeElixirPdfUtilities.HtmlToPdf.render(html)
    refute pdf =~ "/Subtype /Link"
    assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf, false)
    assert {:ok, %{document: _document}} = Reader.read_validated(pdf)
  end

  test "uniform rotations, reflections and composed similarities retain native stroke geometry" do
    for {outer, inner, _matrix} <- [
          {"", "rotate(30 100 70)", "0.8660254 0.5 -0.5 0.8660254"},
          {"translate(230 0)", "scale(-1 1)", "-1 0 0 1"},
          {"translate(20 10) rotate(23) scale(1.2)", "rotate(-17) scale(-.7 .7)", nil},
          {"scale(2 1)", "scale(.5 1) rotate(29)", nil},
          {"", "matrix(.6 .8 .8 -.6 160 10)", "0.6 0.8 0.8 -0.6"}
        ] do
      source =
        "<svg width='300' height='180'><g transform='#{outer}' opacity='.7'><path transform='#{inner}' d='M20 40 Q80 5 130 60 L190 90' fill='none' stroke='#174a86' stroke-width='3' stroke-opacity='.4' stroke-linecap='round' stroke-linejoin='round'/></g></svg>"

      assert {:ok, pdf} =
               NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.pdf(source, [], %{
                 mode: :inline
               })

      refute pdf =~ "/Subtype /Link"
      assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf, false)
      assert {:ok, _document} = Reader.read(pdf)

      assert {:ok, _png} =
               NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.png(source, [], %{
                 mode: :inline
               })
    end
  end

  test "complex transformed, dashed and paint server shape strokes retain their outline route" do
    for attributes <- [
          "transform='scale(1.2 .8)'",
          "transform='skewX(10)'",
          "transform='rotate(10) scale(1.2 .8)'",
          "transform='matrix(1 0.001 0 1 0 0)'",
          "stroke-dasharray='5 3'",
          "stroke='url(#gradient)'"
        ] do
      stroke =
        if attributes =~ "stroke='" do
          ""
        else
          "stroke='#174a86'"
        end

      source =
        "<svg width='300' height='150'><defs><linearGradient id='gradient'><stop stop-color='red'/><stop offset='1' stop-color='blue'/></linearGradient></defs><path d='M20 40 H180 L220 90' fill='none' #{stroke} stroke-width='3' #{attributes}/></svg>"

      assert {:ok, pdf} =
               NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.pdf(source, [], %{
                 mode: :inline
               })

      refute pdf =~ "/Subtype /Link"
      assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf, false)
      assert {:ok, _document} = Reader.read(pdf)

      assert {:ok, _png} =
               NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.png(source, [], %{
                 mode: :inline
               })
    end
  end
end
