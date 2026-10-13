defmodule NativeElixirPdfUtilities.HtmlToPdf.SvgTextPaintTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.{HtmlToPdf, Limits, Text}
  alias NativeElixirPdfUtilities.Pdf.Reader
  alias NativeElixirPdfUtilities.HtmlToPdf.SvgBackend

  setup do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    data = File.read!("priv/fonts/dejavu/DejaVuSans.ttf")
    %{limits: limits, data: data}
  end

  test "gradient and pattern servers fill actual selected glyph IDs and preserve complete font bytes",
       %{data: data} do
    for {server, pattern_type, shading_type} <- [
          {"<linearGradient id='paint'><stop offset='0' stop-color='red' /><stop offset='.4' stop-color='green' /><stop offset='1' stop-color='blue' /></linearGradient>",
           2, 2},
          {"<radialGradient id='paint' fx='30%' fy='40%'><stop offset='0' stop-color='red' /><stop offset='1' stop-color='blue' /></radialGradient>",
           2, 3},
          {"<pattern id='paint' patternUnits='userSpaceOnUse' width='8' height='8'><rect width='8' height='8' fill='blue' /><rect width='4' height='4' fill='red' /></pattern>",
           1, nil}
        ] do
      source =
        "<svg width='300' height='60'><defs>#{server}</defs><text x='5' y='35' font-size='28' font-family='NativeSvgFont0' fill='url(#paint)'>office café Ω</text></svg>"

      assert {:ok, pdf} =
               SvgBackend.vectorize(source, [width: 300, height: 60], %{
                 mode: :inline,
                 fonts: [%{data: data}]
               })

      assert {:ok, "office café Ω"} = Text.extract(pdf)
      assert {:ok, %{pages: [page]}} = Text.extract_spans(pdf)

      assert Enum.all?(
               page.spans,
               &(&1.paints_text? and &1.render_mode == 0 and &1.font_size == 28)
             )

      assert Enum.count(page.spans, &(&1.text == "ffi")) == 1
      assert {:ok, document} = Reader.read(pdf)
      streams = Enum.map(Map.values(document.objects), &(Map.get(&1, :stream) || ""))
      assert data in streams
      content = Enum.join(streams)
      assert content =~ " Tj"
      assert content =~ "/Pattern cs"
      refute content =~ "3 Tr"
      refute content =~ "7 Tr"
      refute pdf =~ "/Subtype /Image"
      assert pdf =~ "/FontFile2"
      assert pdf =~ "/CIDToGIDMap /Identity"
      assert pdf =~ "/PatternType #{pattern_type}"
      # A single source paint server is allocated per span, irrespective of text length.
      assert Enum.count(
               Map.values(document.objects),
               &(get_in(&1, [:value, "PatternType"]) == pattern_type)
             ) == 1

      if shading_type, do: assert(pdf =~ "/ShadingType #{shading_type}")
    end
  end

  test "whole-object and user-space gradient matrices remain independent of cluster text matrices",
       %{data: data} do
    for {units, attrs} <- [
          {"objectBoundingBox", "x1='0' y1='0' x2='1' y2='0'"},
          {"userSpaceOnUse", "x1='10' y1='20' x2='180' y2='20'"}
        ] do
      source =
        "<svg width='300' height='100' viewBox='0 0 300 100'><defs><linearGradient id='paint' gradientUnits='#{units}' #{attrs} gradientTransform='rotate(10)'><stop offset='0' stop-color='red' /><stop offset='1' stop-color='blue' /></linearGradient></defs><g transform='translate(12 8) rotate(-8)'><text x='10' y='42' font-family='NativeSvgFont0' font-size='30' fill='url(#paint)'>ABCD</text></g></svg>"

      context = %{mode: :inline, fonts: [%{data: data}]}
      assert {:ok, pdf} = SvgBackend.vectorize(source, [width: 300, height: 100], context)

      assert {:ok, solid} =
               SvgBackend.vectorize(
                 String.replace(source, "fill='url(#paint)'", "fill='red'"),
                 [width: 300, height: 100],
                 context
               )

      assert {:ok, painted_spans} = Text.extract_spans(pdf)
      assert {:ok, solid_spans} = Text.extract_spans(solid)

      fields = [
        :text,
        :x,
        :y,
        :end_x,
        :end_y,
        :font_size,
        :transform,
        :render_mode,
        :paints_text?
      ]

      assert Enum.map(hd(painted_spans.pages).spans, &Map.take(&1, fields)) ==
               Enum.map(hd(solid_spans.pages).spans, &Map.take(&1, fields))

      assert {:ok, document} = Reader.read(pdf)

      pattern =
        Enum.find(Map.values(document.objects), &(get_in(&1, [:value, "PatternType"]) == 2))

      assert [_, _, _, _, _, _] = pattern.value["Matrix"]

      if units == "userSpaceOnUse" do
        shading =
          Enum.find(Map.values(document.objects), &(get_in(&1, [:value, "ShadingType"]) == 2))

        assert shading.value["Coords"] == [10, 20, 180, 20]
      end
    end
  end

  test "opacity forms use their local default paint space while nested groups retain authored offsets",
       %{data: data} do
    source =
      "<svg width='300' height='100'><defs><linearGradient id='g' gradientUnits='userSpaceOnUse' x1='0' x2='160'><stop offset='0' stop-color='red' /><stop offset='1' stop-color='blue' /></linearGradient></defs><g transform='translate(25 10) rotate(12)' opacity='.7'><g transform='translate(10 6)'><text x='4' y='35' font-family='NativeSvgFont0' font-size='28' fill='url(#g)'>Local Ω</text></g></g></svg>"

    assert {:ok, pdf} =
             SvgBackend.vectorize(source, [width: 300, height: 100], %{
               mode: :inline,
               fonts: [%{data: data}]
             })

    assert {:ok, "Local Ω"} = Text.extract(pdf, layout: false)
    assert {:ok, document} = Reader.read(pdf)
    pattern = Enum.find(Map.values(document.objects), &(get_in(&1, [:value, "PatternType"]) == 2))
    assert pattern.value["Matrix"] == [1, 0, 0, 1, 10, 6]
    assert pdf =~ "/ca 0.7"
  end

  test "object-bbox and user-space tiles retain text, clip, alpha and independently painted spans" do
    for units <- ["objectBoundingBox", "userSpaceOnUse"] do
      dimensions =
        if units == "objectBoundingBox",
          do: "width='.15' height='.25'",
          else: "width='12' height='10'"

      html =
        "<svg width=300 height=90 font-family='DejaVu Sans' font-size=24><defs><pattern id=p patternUnits='#{units}' #{dimensions} patternTransform='rotate(15)'><rect width=20 height=12 fill=blue /><circle cx=4 cy=4 r=3 fill=red /></pattern><clipPath id=c><rect width=290 height=80 /></clipPath></defs><g clip-path='url(#c)' opacity='.65' transform='translate(3 4)'><text x=5 y=32 fill='url(#p)' fill-opacity='.5'>Tile Ω<tspan x=5 y=65 fill=green font-weight=700>Bold café</tspan></text></g></svg>"

      assert {:ok, pdf} = HtmlToPdf.render(html)
      assert {:ok, text} = Text.extract(pdf)
      assert text =~ "Tile Ω" and text =~ "Bold café"
      assert pdf =~ "/PatternType 1"
      assert pdf =~ "/ca 0.5" and pdf =~ "/ca 0.65"
      assert {:ok, %{pages: [page]}} = Text.extract_spans(pdf)
      assert Enum.all?(page.spans, & &1.paints_text?)
      refute pdf =~ "/Subtype /Image"
    end
  end

  test "pattern child text keeps its issued real selected-font clusters" do
    html =
      "<svg width=260 height=65 font-family='DejaVu Sans'><defs><pattern id=p patternUnits='userSpaceOnUse' width=20 height=20><rect width=20 height=20 fill=blue /><text x=2 y=14 font-size=10 fill=red>A</text></pattern></defs><text x=5 y=45 font-size=36 fill='url(#p)'>Pattern Ω</text></svg>"

    assert {:ok, pdf} = HtmlToPdf.render(html)
    assert {:ok, "Pattern Ω"} = Text.extract(pdf)
    assert pdf =~ "/PatternType 1"
    assert pdf =~ "/FontFile2"
    refute pdf =~ "/Subtype /Image"
  end

  test "HTML enclosing link retains the same text plan when fill is a server" do
    html =
      "<a href='https://example.com/paint' style='text-decoration:none'><svg width=230 height=60 font-family='DejaVu Sans'><defs><linearGradient id=g><stop offset=0 stop-color=red /><stop offset=1 stop-color=blue /></linearGradient></defs><text x=5 y=35 font-size=24 fill='url(#g)'>Linked Ω</text></svg></a>"

    assert {:ok, pdf} = HtmlToPdf.render(html)
    assert {:ok, solid} = HtmlToPdf.render(String.replace(html, "fill='url(#g)'", "fill='red'"))
    assert {:ok, painted_document} = Reader.read(pdf)
    assert {:ok, solid_document} = Reader.read(solid)

    rectangles = fn document ->
      document.objects
      |> Map.values()
      |> Enum.filter(&(get_in(&1, [:value, "Subtype"]) == {:name, "Link"}))
      |> Enum.map(& &1.value["Rect"])
    end

    assert rectangles.(painted_document) != []
    assert rectangles.(painted_document) == rectangles.(solid_document)
    assert {:ok, "Linked Ω"} = Text.extract(pdf)
  end

  test "gradient, glyph and pattern quotas fail with shared actionable diagnostics before painting",
       %{limits: limits, data: data} do
    gradient =
      "<svg width='300' height='60'><defs><linearGradient id='g'><stop offset='0' stop-color='red' /><stop offset='.5' stop-color='green' /><stop offset='1' stop-color='blue' /></linearGradient></defs><text y='40' font-size='32' font-family='NativeSvgFont0' fill='url(#g)'>Budget office Ω</text></svg>"

    context = %{mode: :inline, fonts: [%{data: data}]}
    assert {:ok, _} = SvgBackend.vectorize(gradient, [width: 300, height: 60], context)

    for {key, maximum} <- [
          max_svg_gradient_work: 2,
          max_svg_stroke_work: 1,
          max_svg_font_work_bytes: 1
        ] do
      Limits.install(Map.put(limits, key, maximum))

      assert {:error, {:resource_limit_exceeded, diagnostic}} =
               SvgBackend.vectorize(gradient, [width: 300, height: 60], context)

      assert diagnostic.stage in [:limits, :font]
      assert diagnostic.reason == :resource_limit_exceeded
      assert diagnostic.message =~ Atom.to_string(key)
      assert diagnostic.source != ""
    end

    Limits.install(limits)

    for {replacement, reason, stage, message} <- [
          {"fill='url(#missing)'", :invalid_document, :style, "target"},
          {"fill='url(https://example.com/paint.svg#g)'", :invalid_document, :style,
           "not authorized"}
        ] do
      assert {:error, {^reason, diagnostic}} =
               SvgBackend.vectorize(
                 String.replace(gradient, "fill='url(#g)'", replacement),
                 [width: 300, height: 60],
                 context
               )

      assert diagnostic.reason == reason and diagnostic.message =~ message
      assert diagnostic.stage == stage
    end
  end
end
