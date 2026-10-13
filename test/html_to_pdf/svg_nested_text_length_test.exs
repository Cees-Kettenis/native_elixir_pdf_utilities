defmodule NativeElixirPdfUtilities.HtmlToPdf.SvgNestedTextLengthTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.HtmlToPdf.SvgBackend
  alias NativeElixirPdfUtilities.{Text, Limits}
  alias NativeElixirPdfUtilities.Validators.SvgBackendValidator

  setup do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    %{data: File.read!("priv/fonts/dejavu/DejaVuSans.ttf"), limits: limits}
  end

  test "ancestral spacing distributes over actual nested selected glyphs", %{data: data} do
    source =
      "<svg width='240' height='80'><text x='20' y='40' font-size='20' font-family='NativeSvgFont0' textLength='120'>AB<tspan font-family='NativeSvgFont0'>CD</tspan>EF</text></svg>"

    assert {:ok, pdf} = SvgBackend.vectorize(source, [], %{mode: :inline, fonts: [%{data: data}]})
    assert {:ok, "ABCDEF"} = Text.extract(pdf, layout: false)
    assert {:ok, %{pages: [%{spans: spans}]}} = Text.extract_spans(pdf)
    assert_in_delta hd(spans).x, 15, 0.001
    assert_in_delta List.last(spans).end_x - hd(spans).x, 90, 0.001
    assert Enum.all?(spans, & &1.paints_text?)
    assert {:ok, png} = SvgBackend.rasterize(source, [], %{mode: :inline, fonts: [%{data: data}]})
    assert png != <<>>
  end

  test "ancestor spacing follows resolved descendant character boundaries", %{data: data} do
    source =
      "<svg width='240' height='80'><text x='20' y='40' font-size='20' font-family='NativeSvgFont0' textLength='120'>AB<tspan font-family='NativeSvgFont0' textLength='40'>CD</tspan>EF</text></svg>"

    assert {:ok, pdf} = SvgBackend.vectorize(source, [], %{mode: :inline, fonts: [%{data: data}]})
    assert {:ok, "ABCDEF"} = Text.extract(pdf, layout: false)
    assert {:ok, %{pages: [%{spans: spans}]}} = Text.extract_spans(pdf)
    assert Enum.at(spans, 3).end_x - Enum.at(spans, 2).x > 30
    assert_in_delta List.last(spans).end_x - hd(spans).x, 90, 0.001
  end

  test "multiple nested spacing owners use actual source hierarchy", %{data: data} do
    source =
      "<svg width='280' height='90'><text x='20' y='40' font-size='20' font-family='NativeSvgFont0' textLength='180'>A<tspan font-family='NativeSvgFont0' textLength='100'>B<tspan font-family='NativeSvgFont0' textLength='40'>CD</tspan>E</tspan>F</text></svg>"

    assert {:ok, pdf} = SvgBackend.vectorize(source, [], %{mode: :inline, fonts: [%{data: data}]})
    assert {:ok, "ABCDEF"} = Text.extract(pdf, layout: false)
    assert {:ok, %{pages: [%{spans: spans}]}} = Text.extract_spans(pdf)
    assert_in_delta List.last(spans).end_x - hd(spans).x, 135, 0.001
  end

  test "nested spacing preserves original Unicode, source IDs and actual selected fonts", %{
    data: data
  } do
    source =
      "<svg width='240' height='80'><text id='original-root' x='20' y='40' font-size='20' font-family='NativeSvgFont0' textLength='120'>AΩ<tspan id='original-child' font-family='NativeSvgFont0' textLength='40'>éD</tspan>EF</text></svg>"

    assert {:ok, request} =
             SvgBackendValidator.prepare_vector(
               source,
               [],
               %{mode: :inline, fonts: [%{data: data}]},
               nil
             )

    assert request.source =~ "id='original-root'"
    assert request.source =~ "id='original-child'"
    assert request.source =~ ">AΩ<tspan"
    assert request.source =~ ">éD</tspan>EF"
    refute request.source =~ "textLength"
    assert request.fonts == [data]
    assert {:ok, pdf} = SvgBackend.vectorize(source, [], %{mode: :inline, fonts: [%{data: data}]})
    assert {:ok, "AΩéDEF"} = Text.extract(pdf, layout: false)
    assert {:ok, doc} = NativeElixirPdfUtilities.Pdf.Reader.read(pdf)
    assert Enum.any?(Map.values(doc.objects), &(Map.get(&1, :stream) == data))
    refute pdf =~ "3 Tr"
    refute pdf =~ "/Subtype /Image"
  end

  test "natural geometry and placement plans charge the existing source and work budgets", %{
    data: data,
    limits: limits
  } do
    source =
      "<svg width='240' height='80'><text x='20' y='40' font-size='20' font-family='NativeSvgFont0' textLength='120'>AB<tspan font-family='NativeSvgFont0' textLength='40'>CD</tspan>EF</text></svg>"

    context = %{mode: :inline, fonts: [%{data: data}]}

    for {key, value} <- [
          {:max_aggregate_svg_source_bytes, byte_size(source)},
          {:max_svg_bytes, byte_size(source)},
          {:max_svg_expansion_work, 10}
        ] do
      Limits.install(Map.put(limits, key, value))
      assert {:error, {:resource_limit_exceeded, d}} = SvgBackend.vectorize(source, [], context)
      assert d.message =~ Atom.to_string(key)
      assert d.stage in [:limits, :layout]
      assert d.reason == :resource_limit_exceeded
      assert is_binary(d.source) and is_binary(d.message)
    end

    Limits.install(Map.put(limits, :max_aggregate_svg_surface_pixels, 1))
    assert {:ok, _pdf} = SvgBackend.vectorize(source, [], context)
    assert {:error, {:resource_limit_exceeded, d}} = SvgBackend.rasterize(source, [], context)
    assert d.message =~ "max_aggregate_svg_surface_pixels"
  end
end
