Code.require_file("../support/svg_public_workflow.ex", __DIR__)
Code.require_file("../support/png_fixture.ex", __DIR__)

defmodule NativeElixirPdfUtilities.HtmlToPdf.SvgType3PaintServerTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.{Limits}
  alias NativeElixirPdfUtilities.HtmlToPdf.{Font}
  alias NativeElixirPdfUtilities.Pdf.Reader

  setup do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    data = File.read!("priv/fonts/dejavu/DejaVuSans.ttf")

    assert {:ok, registry} =
             Font.load_registry(
               fonts: [%{family: "Proof", data: data}],
               system_font_discovery: false
             )

    assert {:ok, ["Proof"], face} = Font.resolve("Proof", 400, :normal, registry)
    %{limits: limits, data: data, face: face, context: %{mode: :inline, fonts: [%{data: data}]}}
  end

  test(
    "every original glyph fills its approved stroke with one shared native scene sampler with exact source advance and separate CID provenance",
    %{data: _data, face: face, context: context}
  ) do
    source =
      "<svg width='230' height='90'><defs><linearGradient id='g' gradientUnits='userSpaceOnUse' x1='10' x2='200'><stop offset='0' stop-color='red'/><stop offset='1' stop-color='blue'/></linearGradient></defs><text x='10' y='55' font-family='NativeSvgFont0' font-size='40' fill='none' stroke='url(#g)' stroke-width='3'>AAA</text></svg>"

    assert {:ok, pdf} =
             NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.pdf(source, [], context)

    refute pdf =~ "/Subtype /Link"
    assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf, false)
    assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf)

    assert {:error, {:no_extractable_text, _}} =
             NativeElixirPdfUtilities.Text.extract(pdf, layout: false)

    assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf)

    assert {:error, {:no_extractable_text, _}} =
             NativeElixirPdfUtilities.Text.extract(pdf, layout: false)

    assert {:ok, _doc} = Reader.read(pdf)
    _gid = Map.fetch!(face.cmap, 65)
  end

  test("native samples retain approved original stroke coverage including authored dashes", %{
    context: context
  }) do
    for dash <- ["", "stroke-dasharray='3 2' stroke-linejoin='bevel' stroke-linecap='round'"] do
      source =
        "<svg width='100' height='70'><defs><linearGradient id='g'><stop offset='0' stop-color='red'/><stop offset='1' stop-color='blue'/></linearGradient></defs><text x='10' y='45' font-family='NativeSvgFont0' font-size='30' fill='none' stroke='url(#g)' stroke-width='2' #{dash}>A</text></svg>"

      assert {:ok, pdf} =
               NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.pdf(source, [], context)

      refute pdf =~ "/Subtype /Link"
      assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf, false)
      assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf)

      assert {:error, {:no_extractable_text, _}} =
               NativeElixirPdfUtilities.Text.extract(pdf, layout: false)
    end
  end

  test(
    "varying alpha spread and linearRGB share color and alpha coordinates through transformed original glyphs",
    %{context: context}
  ) do
    for tag <- ["linearGradient", "radialGradient"], spread <- ["repeat", "reflect"] do
      source =
        "<svg width='300' height='110'><defs><#{tag} id='g' spreadMethod='#{spread}' color-interpolation='linearRGB' gradientTransform='rotate(13)'><stop offset='0' stop-color='red' stop-opacity='.2'/><stop offset='1' stop-color='blue' stop-opacity='.8'/></#{tag}></defs><g transform='translate(10 8) skewX(12)' opacity='.7'><text x='15' y='65' font-family='NativeSvgFont0' font-size='32' rotate='10' fill='#eeddbb' stroke='url(#g)' stroke-width='2' stroke-opacity='.6' paint-order='stroke fill'>ffi café Ω</text></g></svg>"

      assert {:ok, pdf} =
               NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.pdf(source, [], context)

      refute pdf =~ "/Subtype /Link"
      assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf, false)
      assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf)

      assert {:error, {:no_extractable_text, _}} =
               NativeElixirPdfUtilities.Text.extract(pdf, layout: false)

      assert {:ok, _doc} = Reader.read(pdf)
    end
  end

  test(
    "independent original glyph rotations and scaled advances keep their paint in scene coordinates",
    %{context: context}
  ) do
    source =
      "<svg width='280' height='100'><defs><linearGradient id='g' gradientUnits='userSpaceOnUse' x1='0' x2='260'><stop offset='0' stop-color='red'/><stop offset='1' stop-color='blue'/></linearGradient></defs><g transform='translate(8 5) rotate(4)'><text x='15' y='60' font-family='NativeSvgFont0' font-size='32' rotate='0 10 -7' fill='none' stroke='url(#g)' stroke-width='2'>Rotate Ω</text></g></svg>"

    assert {:ok, pdf} =
             NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.pdf(source, [], context)

    refute pdf =~ "/Subtype /Link"
    assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf, false)
    assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf)

    assert {:error, {:no_extractable_text, _}} =
             NativeElixirPdfUtilities.Text.extract(pdf, layout: false)

    assert {:ok, _doc} = Reader.read(pdf)

    source =
      String.replace(
        source,
        "rotate='0 10 -7'",
        "textLength='220' lengthAdjust='spacingAndGlyphs'"
      )

    assert {:ok, pdf} =
             NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.pdf(source, [], context)

    refute pdf =~ "/Subtype /Link"
    assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf, false)
    assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf)

    assert {:error, {:no_extractable_text, _}} =
             NativeElixirPdfUtilities.Text.extract(pdf, layout: false)
  end

  test(
    "authored pattern phase and expanded stroke extent stay shared inside masks and ordered CID fills",
    %{context: context}
  ) do
    for paint_order <- ["fill stroke", "stroke fill"], fill <- ["none", "#ddbbaa"] do
      source =
        "<svg width='260' height='100'><defs><pattern id='p' patternUnits='userSpaceOnUse' x='-3' y='2' width='11' height='9' patternTransform='rotate(12)'><rect width='11' height='9' fill='blue'/><rect width='5' height='4' fill='red' fill-opacity='.5'/></pattern><mask id='m'><rect width='260' height='100' fill='white'/></mask></defs><g mask='url(#m)' transform='translate(8 5)'><text x='10' y='55' font-family='NativeSvgFont0' font-size='30' fill='#{fill}' stroke='url(#p)' stroke-width='8' paint-order='#{paint_order}'>Pattern ffi Ω</text></g></svg>"

      assert {:ok, pdf} =
               NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.pdf(source, [], context)

      refute pdf =~ "/Subtype /Link"
      assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf, false)
      assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf)

      assert {:error, {:no_extractable_text, _}} =
               NativeElixirPdfUtilities.Text.extract(pdf, layout: false)

      assert {:ok, _doc} = Reader.read(pdf)
    end
  end

  test("canonical host admission consumes actual paint-server strokes", %{data: _data}) do
    html =
      "<style>text{stroke:url(#g);stroke-width:2;fill:none}</style><svg width='200' height='70'><defs><linearGradient id='g'><stop offset='0' stop-color='red'/><stop offset='1' stop-color='blue'/></linearGradient></defs><text x='10' y='45' font-size='30'>Host ffi Ω</text></svg>"

    assert {:ok, pdf} = NativeElixirPdfUtilities.HtmlToPdf.render(html)
    refute pdf =~ "/Subtype /Link"
    assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf, false)
    assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf)

    assert {:error, {:no_extractable_text, _}} =
             NativeElixirPdfUtilities.Text.extract(pdf, layout: false)

    assert {:ok, _doc} = Reader.read(pdf)
  end

  test(
    "a mask applies once to independent stroke cells while overlapping fill passes retain group composition",
    %{context: context}
  ) do
    source = File.read!("test/fixtures/html_to_pdf/svg_type3_native/mask_hole.svg")

    for fill <- ["none", "#eeddbb"] do
      assert {:ok, pdf} =
               NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.pdf(
                 String.replace(source, "fill='none'", "fill='#{fill}'"),
                 [],
                 context
               )

      refute pdf =~ "/Subtype /Link"
      assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf, false)
      assert {:ok, _document} = Reader.read(pdf)
    end
  end
end
