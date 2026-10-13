defmodule NativeElixirPdfUtilities.HtmlToPdf.SvgTextStrokeTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.{HtmlToPdf, Text, Limits}
  alias NativeElixirPdfUtilities.HtmlToPdf.SvgBackend

  setup do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    %{data: File.read!("priv/fonts/dejavu/DejaVuSans.ttf"), limits: limits}
  end

  test "genuine selected glyph programs for stroke and ordered fills replace Unicode once", %{
    data: data
  } do
    for attrs <- ["fill='none'", "fill='#f4e2bc'", "fill='#f4e2bc' paint-order='stroke fill'"] do
      source =
        "<svg width='240' height='80'><text x='10' y='45' font-family='NativeSvgFont0' font-size='30' stroke='#18428a' stroke-width='2' #{attrs}>ffi Ωé</text></svg>"

      assert {:ok, pdf} =
               SvgBackend.vectorize(source, [], %{mode: :inline, fonts: [%{data: data}]})

      assert {:ok, "ffi Ωé"} = Text.extract(pdf, layout: false)
      assert {:ok, %{pages: [%{spans: spans}]}} = Text.extract_spans(pdf)
      assert Enum.all?(spans, &(&1.paints_text? and &1.font_size == 30))
      assert hd(spans).text == "ffi"
      assert pdf =~ "/Subtype /Type3"
      assert pdf =~ "/NativeOriginalGID"
      assert pdf =~ "/FontFile2"
      refute pdf =~ "3 Tr"
      refute pdf =~ "/Subtype /Image"
      assert {:ok, doc} = NativeElixirPdfUtilities.Pdf.Reader.read(pdf)
      assert Enum.any?(Map.values(doc.objects), &(Map.get(&1, :stream) == data))
    end
  end

  test "independent fill stroke alpha and zero-fill strokes retain real visible logical paint", %{
    data: data
  } do
    for attrs <- [
          "fill-opacity='.3' stroke-opacity='.7'",
          "fill-opacity='0' stroke-opacity='.7'",
          "fill='none' stroke-opacity='.7'"
        ] do
      source =
        "<svg width='240' height='80'><text x='10' y='45' font-family='NativeSvgFont0' font-size='30' stroke='red' stroke-width='2' #{attrs}>Alpha Ω</text></svg>"

      assert {:ok, pdf} =
               SvgBackend.vectorize(source, [], %{mode: :inline, fonts: [%{data: data}]})

      assert {:ok, "Alpha Ω"} = Text.extract(pdf, layout: false)
      assert pdf =~ "/ca 0.7"

      assert {:ok, png} =
               SvgBackend.rasterize(source, [], %{mode: :inline, fonts: [%{data: data}]})

      assert png =~ <<137, 80, 78, 71>>
    end
  end

  test "host cascade strokes coexist with positions lengths paths and transformed clipping" do
    html =
      "<svg width='400' height='200' font-size='26'><defs><path id='p' d='M20 140 Q200 60 380 140'/><clipPath id='c'><rect x='10' y='5' width='370' height='180'/></clipPath></defs><g transform='translate(5 8) skewX(5)' clip-path='url(#c)' opacity='.7' stroke='#143e8a' stroke-width='1.5'><text x='20' y='45' textLength='220' lengthAdjust='spacingAndGlyphs'>Length Ωé</text><text fill='none'><textPath href='#p' startOffset='10%'>Path ffi Ωé</textPath></text></g></svg>"

    assert {:ok, pdf} = HtmlToPdf.render(html)
    assert {:ok, text} = Text.extract(pdf, layout: false)
    assert text =~ "Length Ωé"
    assert text =~ "Path ffi Ωé"
  end

  test "new glyph stroke expansion still respects work and vector surface modes", %{
    data: data,
    limits: limits
  } do
    source =
      "<svg width='240' height='80'><text y='45' font-family='NativeSvgFont0' font-size='30' stroke='red' stroke-width='2' stroke-dasharray='3 2'>Budget Ω</text></svg>"

    context = %{mode: :inline, fonts: [%{data: data}]}
    Limits.install(Map.put(limits, :max_svg_stroke_work, 1))
    assert {:error, {:resource_limit_exceeded, d}} = SvgBackend.vectorize(source, [], context)
    assert d.message =~ "max_svg_stroke_work"
    Limits.install(Map.put(limits, :max_aggregate_svg_surface_pixels, 1))
    assert {:ok, _pdf} = SvgBackend.vectorize(source, [], context)
    assert {:error, {:resource_limit_exceeded, d}} = SvgBackend.rasterize(source, [], context)
    assert d.message =~ "max_aggregate_svg_surface_pixels"
  end

  test "stroke glyph programs reference the exact approved GID advance and source face", %{
    data: data
  } do
    alias NativeElixirPdfUtilities.HtmlToPdf.Font

    assert {:ok, registry} =
             Font.load_registry(
               fonts: [%{family: "Proof", data: data}],
               system_font_discovery: false
             )

    {:ok, ["Proof"], face} = Font.resolve("Proof", 400, :normal, registry)
    gid = Map.fetch!(face.cmap, ?A)

    source =
      "<svg width='100' height='60'><text x='5' y='40' font-family='NativeSvgFont0' font-size='30' fill='none' stroke='red' stroke-width='2'>A</text></svg>"

    assert {:ok, pdf} = SvgBackend.vectorize(source, [], %{mode: :inline, fonts: [%{data: data}]})
    assert {:ok, doc} = NativeElixirPdfUtilities.Pdf.Reader.read(pdf)

    [program] =
      for %{value: %{"Subtype" => {:name, "Type3"}} = value} <- Map.values(doc.objects), do: value

    assert program["NativeOriginalGID"] == gid
    assert {:ref, source_id} = program["NativeSourceFont"]
    assert doc.objects[source_id].value["Subtype"] == {:name, "Type0"}
    assert [width] = program["Widths"]
    assert_in_delta width, Enum.at(face.widths, gid) * 1000 / face.units_per_em, 0.001
    assert {:ref, proc_id} = program["CharProcs"]["NativeOriginalGlyph"]
    charproc = doc.objects[proc_id]
    assert charproc.value["NativeOriginalGID"] == gid
    assert charproc.stream =~ " d1"
    assert charproc.stream =~ "f"
    assert Enum.any?(Map.values(doc.objects), &(Map.get(&1, :stream) == data))
    assert {:ok, %{pages: [%{spans: [span]}]}} = Text.extract_spans(pdf)
    assert span.text == "A"
    assert span.font_size == 30
    assert span.end_x > span.x
  end

  test "stroke font embedding flags remain governed by the shared source policy", %{data: data} do
    <<_::binary-size(4), count::16, _::binary-size(6), records::binary>> = data

    table =
      for <<tag::binary-size(4), _checksum::32, offset::32,
            _length::32 <- binary_part(records, 0, count * 16)>>, into: %{}, do: {tag, offset}

    offset = Map.fetch!(table, "OS/2") + 8

    source =
      "<svg width='150' height='60'><text y='35' font-family='NativeSvgFont0' fill='none' stroke='red'>Original Ω</text></svg>"

    for flags <- [0x0002, 0x0200, 0x0100] do
      modified =
        binary_part(data, 0, offset) <>
          <<flags::16>> <> binary_part(data, offset + 2, byte_size(data) - offset - 2)

      result = SvgBackend.vectorize(source, [], %{mode: :inline, fonts: [%{data: modified}]})

      if flags == 0x0100 do
        assert {:ok, pdf} = result
        assert {:ok, doc} = NativeElixirPdfUtilities.Pdf.Reader.read(pdf)
        assert Enum.any?(Map.values(doc.objects), &(Map.get(&1, :stream) == modified))
        assert {:ok, "Original Ω"} = Text.extract(pdf, layout: false)
      else
        assert {:error, {:invalid_document, diagnostic}} = result
        assert diagnostic.stage == :font
        assert diagnostic.message =~ "does not permit outline embedding"
      end
    end
  end

  test "actual stroke ink extends link bounds and source filters retain stroke-only Unicode", %{
    data: data
  } do
    alias NativeElixirPdfUtilities.Validators.SvgFormValidator
    context = %{mode: :inline, fonts: [%{data: data}]}

    for attrs <- [
          "fill='none' stroke-width='10'",
          "fill='black' stroke-width='2' stroke-dasharray='2 100'"
        ] do
      source =
        "<svg width='120' height='70'><a href='https://example.test/stroke'><text x='20' y='45' font-family='NativeSvgFont0' font-size='30' stroke='red' #{attrs}>A</text></a></svg>"

      assert {:ok, pdf} = SvgBackend.vectorize(source, [], context)
      assert {:ok, form} = SvgFormValidator.prepare_source(pdf)
      assert [%{rect: [l, b, r, t]}] = form.links
      assert r > l and t > b

      if attrs =~ "10" do
        plain =
          String.replace(source, "stroke='red'", "stroke='none'")
          |> String.replace("fill='none'", "fill='black'")

        assert {:ok, fill_pdf} = SvgBackend.vectorize(plain, [], context)

        assert {:ok, %{links: [%{rect: [fl, fb, fr, ft]}]}} =
                 SvgFormValidator.prepare_source(fill_pdf)

        assert l <= fl and b <= fb and r >= fr and t >= ft
        assert (r - l) * (t - b) > (fr - fl) * (ft - fb)
      end

      assert {:ok, "A"} = Text.extract(pdf, layout: false)
    end

    source =
      "<svg width='180' height='70'><defs><filter id='f'><feGaussianBlur stdDeviation='1'/></filter></defs><text x='10' y='45' filter='url(#f)' font-family='NativeSvgFont0' font-size='30' fill='none' stroke='red' stroke-width='2'>Stroke Ω</text></svg>"

    assert {:ok, pdf} = SvgBackend.vectorize(source, [], context)
    assert pdf =~ "/Subtype /Image"
    assert {:ok, "Stroke Ω"} = Text.extract(pdf, layout: false)
  end

  test "program reuse charges expanded work and actual PDF objects", %{data: data, limits: limits} do
    context = %{mode: :inline, fonts: [%{data: data}]}

    source =
      "<svg width='240' height='70'><defs><text id='t' y='40' font-family='NativeSvgFont0' stroke='red' stroke-width='2'>AAAA</text></defs><use href='#t'/><use href='#t' x='80'/></svg>"

    assert {:ok, _} = SvgBackend.vectorize(source, [], context)
    Limits.install(Map.put(limits, :max_pdf_objects, 10))

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             SvgBackend.vectorize(source, [], context)

    assert diagnostic.message =~ "max_pdf_objects"
    Limits.install(Map.put(limits, :max_svg_stroke_work, 100))

    for render <- [&SvgBackend.vectorize/3, &SvgBackend.rasterize/3] do
      assert {:error, {:resource_limit_exceeded, diagnostic}} = render.(source, [], context)
      assert diagnostic.message =~ "max_svg_stroke_work"
    end
  end

  test "native program work counts expanded reuse even when one font program is cached", %{
    data: data
  } do
    one =
      "<svg width='160' height='60'><defs><text id='t' y='35' font-family='NativeSvgFont0' fill='none' stroke='red' stroke-width='2'>A</text></defs><use href='#t'/></svg>"

    two = String.replace(one, "</svg>", "<use href='#t' x='50'/></svg>")
    assert {:ok, _} = SvgBackend.vectorize(one, [], %{mode: :inline, fonts: [%{data: data}]})
    native = NativeElixirPdfUtilities.HtmlToPdf.SvgBackend.NativeBytes
    assert {:ok, {single, 0}} = native.validate_shape_work(one, {1_000_000, 100_000, [data]})
    assert {:ok, {reused, 0}} = native.validate_shape_work(two, {1_000_000, 100_000, [data]})
    assert reused == 2 * single

    assert {:error, "max_svg_stroke_work"} =
             native.validate_shape_work(two, {reused - 1, 100_000, [data]})

    assert {:ok, pdf} = SvgBackend.vectorize(two, [], %{mode: :inline, fonts: [%{data: data}]})
    assert {:ok, "AA"} = Text.extract(pdf, layout: false)
    assert {:ok, doc} = NativeElixirPdfUtilities.Pdf.Reader.read(pdf)

    assert 1 ==
             Enum.count(
               Map.values(doc.objects),
               &(get_in(&1, [:value, "Subtype"]) == {:name, "Type3"})
             )
  end
end
