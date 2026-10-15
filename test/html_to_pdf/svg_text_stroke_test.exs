Code.require_file("../support/svg_converted_image.ex", __DIR__)
Code.require_file("../support/svg_public_workflow.ex", __DIR__)
Code.require_file("../support/png_fixture.ex", __DIR__)

defmodule NativeElixirPdfUtilities.HtmlToPdf.SvgTextStrokeTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.{Limits}

  setup do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    %{data: File.read!("priv/fonts/dejavu/DejaVuSans.ttf"), limits: limits}
  end

  test("genuine selected glyph programs for stroke and ordered fills replace Unicode once", %{
    data: data
  }) do
    for attrs <- ["fill='none'", "fill='#f4e2bc'", "fill='#f4e2bc' paint-order='stroke fill'"] do
      source =
        "<svg width='240' height='80'><text x='10' y='45' font-family='NativeSvgFont0' font-size='30' stroke='#18428a' stroke-width='2' #{attrs}>ffi Ωé</text></svg>"

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

      assert {:ok, _doc} = NativeElixirPdfUtilities.Pdf.Reader.read(pdf)
    end
  end

  test("independent fill stroke alpha and zero-fill strokes retain real visible logical paint", %{
    data: data
  }) do
    for attrs <- [
          "fill-opacity='.3' stroke-opacity='.7'",
          "fill-opacity='0' stroke-opacity='.7'",
          "fill='none' stroke-opacity='.7'"
        ] do
      source =
        "<svg width='240' height='80'><text x='10' y='45' font-family='NativeSvgFont0' font-size='30' stroke='red' stroke-width='2' #{attrs}>Alpha Ω</text></svg>"

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

      assert {:ok, png} =
               NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.png(source, [], %{
                 mode: :inline,
                 fonts: [%{data: data}]
               })

      assert png =~ <<137, 80, 78, 71>>
    end
  end

  test "host cascade strokes coexist with positions lengths paths and transformed clipping" do
    html =
      "<svg width='400' height='200' font-size='26'><defs><path id='p' d='M20 140 Q200 60 380 140'/><clipPath id='c'><rect x='10' y='5' width='370' height='180'/></clipPath></defs><g transform='translate(5 8) skewX(5)' clip-path='url(#c)' opacity='.7' stroke='#143e8a' stroke-width='1.5'><text x='20' y='45' textLength='220' lengthAdjust='spacingAndGlyphs'>Length Ωé</text><text fill='none'><textPath href='#p' startOffset='10%'>Path ffi Ωé</textPath></text></g></svg>"

    assert {:ok, pdf} = NativeElixirPdfUtilities.HtmlToPdf.render(html)
    refute pdf =~ "/Subtype /Link"
    assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf, false)
    assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf)

    assert {:error, {:no_extractable_text, _}} =
             NativeElixirPdfUtilities.Text.extract(pdf, layout: false)
  end

  test("stroke glyph programs reference the exact approved GID advance and source face", %{
    data: data
  }) do
    alias NativeElixirPdfUtilities.HtmlToPdf.Font

    assert {:ok, registry} =
             Font.load_registry(
               fonts: [%{family: "Proof", data: data}],
               system_font_discovery: false
             )

    {:ok, ["Proof"], face} = Font.resolve("Proof", 400, :normal, registry)
    _gid = Map.fetch!(face.cmap, 65)

    source =
      "<svg width='100' height='60'><text x='5' y='40' font-family='NativeSvgFont0' font-size='30' fill='none' stroke='red' stroke-width='2'>A</text></svg>"

    assert {:ok, pdf} =
             NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.pdf(source, [], %{
               mode: :inline,
               fonts: [%{data: data}]
             })

    refute pdf =~ "/Subtype /Link"
    assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf, false)
    assert {:ok, _doc} = NativeElixirPdfUtilities.Pdf.Reader.read(pdf)
    assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf)

    assert {:error, {:no_extractable_text, _}} =
             NativeElixirPdfUtilities.Text.extract(pdf, layout: false)
  end

  test("stroke font embedding flags remain governed by the shared source policy", %{data: data}) do
    <<_::binary-size(4), count::16, _::binary-size(6), records::binary>> = data

    table =
      for <<tag::binary-size(4), _checksum::32, offset::32,
            _length::32 <- binary_part(records, 0, count * 16)>>,
          into: %{} do
        {tag, offset}
      end

    offset = Map.fetch!(table, "OS/2") + 8

    source =
      "<svg width='150' height='60'><text y='35' font-family='NativeSvgFont0' fill='none' stroke='red'>Original Ω</text></svg>"

    for flags <- [2, 512, 256] do
      modified =
        binary_part(data, 0, offset) <>
          <<flags::16>> <> binary_part(data, offset + 2, byte_size(data) - offset - 2)

      result =
        NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.pdf(source, [], %{
          mode: :inline,
          fonts: [%{data: modified}]
        })

      if flags == 256 do
        assert {:ok, pdf} = result
        assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf)

        assert {:error, {:no_extractable_text, _}} =
                 NativeElixirPdfUtilities.Text.extract(pdf, layout: false)
      else
        assert {:error, {:invalid_document, diagnostic}} = result
        assert diagnostic.stage == :font
        assert diagnostic.message =~ "does not permit outline embedding"
      end
    end
  end

  test("actual stroke ink extends link bounds and source filters retain stroke-only Unicode", %{
    data: data
  }) do
    context = %{mode: :inline, fonts: [%{data: data}]}

    for attrs <- [
          "fill='none' stroke-width='10'",
          "fill='black' stroke-width='2' stroke-dasharray='2 100'"
        ] do
      source =
        "<svg width='120' height='70'><a href='https://example.test/stroke'><text x='20' y='45' font-family='NativeSvgFont0' font-size='30' stroke='red' #{attrs}>A</text></a></svg>"

      assert {:ok, pdf} =
               NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.pdf(source, [], context)

      refute pdf =~ "/Subtype /Link"
      assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf, false)

      if attrs =~ "10" do
        plain =
          String.replace(source, "stroke='red'", "stroke='none'")
          |> String.replace("fill='none'", "fill='black'")

        assert {:ok, stroke_fill_pdf} =
                 NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.pdf(
                   plain,
                   [],
                   context
                 )

        refute stroke_fill_pdf =~ "/Subtype /Link"

        assert :ok =
                 NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(
                   stroke_fill_pdf,
                   false
                 )
      end

      assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf)

      assert {:error, {:no_extractable_text, _}} =
               NativeElixirPdfUtilities.Text.extract(pdf, layout: false)
    end

    source =
      "<svg width='180' height='70'><defs><filter id='f'><feGaussianBlur stdDeviation='1'/></filter></defs><text x='10' y='45' filter='url(#f)' font-family='NativeSvgFont0' font-size='30' fill='none' stroke='red' stroke-width='2'>Stroke Ω</text></svg>"

    assert {:ok, pdf} =
             NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.pdf(source, [], context)

    refute pdf =~ "/Subtype /Link"
    assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf, false)
    assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf)

    assert {:error, {:no_extractable_text, _}} =
             NativeElixirPdfUtilities.Text.extract(pdf, layout: false)
  end

  test("native program work counts expanded reuse even when one font program is cached", %{
    data: data
  }) do
    one =
      "<svg width='160' height='60'><defs><text id='t' y='35' font-family='NativeSvgFont0' fill='none' stroke='red' stroke-width='2'>A</text></defs><use href='#t'/></svg>"

    two = String.replace(one, "</svg>", "<use href='#t' x='50'/></svg>")

    assert {:ok, _} =
             NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.pdf(one, [], %{
               mode: :inline,
               fonts: [%{data: data}]
             })

    assert {:ok, pdf} =
             NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.pdf(two, [], %{
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
