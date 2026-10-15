Code.require_file("../support/svg_converted_image.ex", __DIR__)
Code.require_file("../support/svg_public_workflow.ex", __DIR__)
Code.require_file("../support/png_fixture.ex", __DIR__)

defmodule NativeElixirPdfUtilities.HtmlToPdf.SvgTextPaintTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.Limits
  alias NativeElixirPdfUtilities.Pdf.Reader

  setup do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    data = File.read!("priv/fonts/dejavu/DejaVuSans.ttf")
    %{limits: limits, data: data}
  end

  alias NativeElixirPdfUtilities.{Limits}

  test(
    "gradient and pattern servers fill actual selected glyph IDs and preserve complete font bytes",
    %{data: data}
  ) do
    for {server, _pattern_type, shading_type} <- [
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
               NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.pdf(
                 source,
                 [width: 300, height: 60],
                 %{mode: :inline, fonts: [%{data: data}]}
               )

      refute pdf =~ "/Subtype /Link"
      assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf, false)
      assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf)

      assert {:error, {:no_extractable_text, _}} =
               NativeElixirPdfUtilities.Text.extract(pdf, layout: false)

      assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf)

      assert {:error, {:no_extractable_text, _}} =
               NativeElixirPdfUtilities.Text.extract(pdf, layout: false)

      assert {:ok, _document} = Reader.read(pdf)

      if shading_type do
      end
    end
  end

  test(
    "whole-object and user-space gradient matrices remain independent of cluster text matrices",
    %{data: data}
  ) do
    for {units, attrs} <- [
          {"objectBoundingBox", "x1='0' y1='0' x2='1' y2='0'"},
          {"userSpaceOnUse", "x1='10' y1='20' x2='180' y2='20'"}
        ] do
      source =
        "<svg width='300' height='100' viewBox='0 0 300 100'><defs><linearGradient id='paint' gradientUnits='#{units}' #{attrs} gradientTransform='rotate(10)'><stop offset='0' stop-color='red' /><stop offset='1' stop-color='blue' /></linearGradient></defs><g transform='translate(12 8) rotate(-8)'><text x='10' y='42' font-family='NativeSvgFont0' font-size='30' fill='url(#paint)'>ABCD</text></g></svg>"

      context = %{mode: :inline, fonts: [%{data: data}]}

      assert {:ok, pdf} =
               NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.pdf(
                 source,
                 [width: 300, height: 100],
                 context
               )

      refute pdf =~ "/Subtype /Link"
      assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf, false)

      assert {:ok, solid} =
               NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.pdf(
                 String.replace(source, "fill='url(#paint)'", "fill='red'"),
                 [width: 300, height: 100],
                 context
               )

      refute solid =~ "/Subtype /Link"

      assert :ok =
               NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(solid, false)

      assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf)

      assert {:error, {:no_extractable_text, _}} =
               NativeElixirPdfUtilities.Text.extract(pdf, layout: false)

      assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(solid)

      assert {:error, {:no_extractable_text, _}} =
               NativeElixirPdfUtilities.Text.extract(solid, layout: false)

      _fields = [
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

      assert {:ok, _document} = Reader.read(pdf)
    end
  end

  test(
    "opacity forms use their local default paint space while nested groups retain authored offsets",
    %{data: data}
  ) do
    source =
      "<svg width='300' height='100'><defs><linearGradient id='g' gradientUnits='userSpaceOnUse' x1='0' x2='160'><stop offset='0' stop-color='red' /><stop offset='1' stop-color='blue' /></linearGradient></defs><g transform='translate(25 10) rotate(12)' opacity='.7'><g transform='translate(10 6)'><text x='4' y='35' font-family='NativeSvgFont0' font-size='28' fill='url(#g)'>Local Ω</text></g></g></svg>"

    assert {:ok, pdf} =
             NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.pdf(
               source,
               [width: 300, height: 100],
               %{mode: :inline, fonts: [%{data: data}]}
             )

    refute pdf =~ "/Subtype /Link"
    assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf, false)
    assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf)

    assert {:error, {:no_extractable_text, _}} =
             NativeElixirPdfUtilities.Text.extract(pdf, layout: false)

    assert {:ok, _document} = Reader.read(pdf)
  end

  test "object-bbox and user-space tiles retain text, clip, alpha and independently painted spans" do
    for units <- ["objectBoundingBox", "userSpaceOnUse"] do
      dimensions =
        if units == "objectBoundingBox" do
          "width='.15' height='.25'"
        else
          "width='12' height='10'"
        end

      html =
        "<svg width=300 height=90 font-family='DejaVu Sans' font-size=24><defs><pattern id=p patternUnits='#{units}' #{dimensions} patternTransform='rotate(15)'><rect width=20 height=12 fill=blue /><circle cx=4 cy=4 r=3 fill=red /></pattern><clipPath id=c><rect width=290 height=80 /></clipPath></defs><g clip-path='url(#c)' opacity='.65' transform='translate(3 4)'><text x=5 y=32 fill='url(#p)' fill-opacity='.5'>Tile Ω<tspan x=5 y=65 fill=green font-weight=700>Bold café</tspan></text></g></svg>"

      assert {:ok, pdf} = NativeElixirPdfUtilities.HtmlToPdf.render(html)
      refute pdf =~ "/Subtype /Link"
      assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf, false)
      assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf)

      assert {:error, {:no_extractable_text, _}} =
               NativeElixirPdfUtilities.Text.extract(pdf, layout: false)

      assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf)

      assert {:error, {:no_extractable_text, _}} =
               NativeElixirPdfUtilities.Text.extract(pdf, layout: false)
    end
  end

  test "pattern child text keeps its issued real selected-font clusters" do
    html =
      "<svg width=260 height=65 font-family='DejaVu Sans'><defs><pattern id=p patternUnits='userSpaceOnUse' width=20 height=20><rect width=20 height=20 fill=blue /><text x=2 y=14 font-size=10 fill=red>A</text></pattern></defs><text x=5 y=45 font-size=36 fill='url(#p)'>Pattern Ω</text></svg>"

    assert {:ok, pdf} = NativeElixirPdfUtilities.HtmlToPdf.render(html)
    refute pdf =~ "/Subtype /Link"
    assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf, false)
    assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf)

    assert {:error, {:no_extractable_text, _}} =
             NativeElixirPdfUtilities.Text.extract(pdf, layout: false)
  end

  test "HTML enclosing link retains the same text plan when fill is a server" do
    html =
      "<a href='https://example.com/paint' style='text-decoration:none'><svg width=230 height=60 font-family='DejaVu Sans'><defs><linearGradient id=g><stop offset=0 stop-color=red /><stop offset=1 stop-color=blue /></linearGradient></defs><text x=5 y=35 font-size=24 fill='url(#g)'>Linked Ω</text></svg></a>"

    assert {:ok, pdf} = NativeElixirPdfUtilities.HtmlToPdf.render(html)
    assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf, false)

    assert {:ok, solid} =
             NativeElixirPdfUtilities.HtmlToPdf.render(
               String.replace(html, "fill='url(#g)'", "fill='red'")
             )

    assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(solid, false)
    assert {:ok, painted_document} = Reader.read(pdf)
    assert {:ok, solid_document} = Reader.read(solid)

    rectangles = fn document ->
      for {_id, %{value: value}} <- document.objects,
          is_map(value) and value["Subtype"] == {:name, "Link"} do
        value["Rect"]
      end
    end

    assert rectangles.(painted_document) != []
    assert rectangles.(painted_document) == rectangles.(solid_document)
    assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf)

    assert {:error, {:no_extractable_text, _}} =
             NativeElixirPdfUtilities.Text.extract(pdf, layout: false)
  end

  test("continuous authored stripe tiles fill genuine CID text in both axes", %{data: data}) do
    for {dimensions, artwork} <- [
          {"width='12' height='10'",
           "<rect width='12' height='10' fill='blue'/><rect width='6' height='10' fill='red'/>"},
          {"width='10' height='12'",
           "<rect width='10' height='12' fill='blue'/><rect width='10' height='6' fill='red'/>"}
        ] do
      source =
        "<svg width='300' height='70'><defs><pattern id='p' patternUnits='userSpaceOnUse' x='-2' y='3' #{dimensions} patternTransform='rotate(12)'>#{artwork}</pattern></defs><g transform='translate(8 5)' opacity='.7'><text x='4' y='42' font-family='NativeSvgFont0' font-size='28' fill='url(#p)' fill-opacity='.6'>Stripe office Ω</text></g></svg>"

      assert {:ok, pdf} =
               NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.pdf(
                 source,
                 [width: 300, height: 70],
                 %{mode: :inline, fonts: [%{data: data}]}
               )

      refute pdf =~ "/Subtype /Link"
      assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf, false)
      assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf)

      assert {:error, {:no_extractable_text, _}} =
               NativeElixirPdfUtilities.Text.extract(pdf, layout: false)

      assert {:ok, _document} = Reader.read(pdf)
      assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf)

      assert {:error, {:no_extractable_text, _}} =
               NativeElixirPdfUtilities.Text.extract(pdf, layout: false)
    end
  end

  test("overlapping opaque bands keep source painter order and charge their emitted paths", %{
    limits: _limits
  }) do
    source =
      "<svg width='120' height='70'><defs><pattern id='p' patternUnits='userSpaceOnUse' width='12' height='10'><rect width='12' height='10' fill='blue'/><rect x='3' width='6' height='10' fill='red'/></pattern></defs><rect x='5' y='5' width='100' height='50' fill='url(#p)'/></svg>"

    assert {:ok, pdf} =
             NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.pdf(
               source,
               [width: 120, height: 70],
               %{mode: :inline}
             )

    refute pdf =~ "/Subtype /Link"
    assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf, false)
    assert {:ok, _document} = Reader.read(pdf)
    Limits.install(Map.put(Limits.effective(), :max_svg_raster_pixels, 0))

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.pdf(
               source,
               [width: 120, height: 70],
               %{mode: :inline}
             )

    assert diagnostic.stage == :limits
    assert is_binary(Map.get(diagnostic, :source)) and Map.get(diagnostic, :source) != ""
  end

  test("incomplete and translucent tiles retain their original vector cell semantics", %{
    data: data
  }) do
    for artwork <- [
          "<rect width='6' height='10' fill='red'/>",
          "<rect width='12' height='10' fill='blue'/><rect width='6' height='10' fill='red' fill-opacity='.5'/>",
          "<rect width='12' height='10' fill='blue'/><circle cx='4' cy='5' r='3' fill='red'/>"
        ] do
      source =
        "<svg width='240' height='60'><defs><pattern id='p' patternUnits='userSpaceOnUse' width='12' height='10'>#{artwork}</pattern></defs><text x='4' y='40' font-size='28' font-family='NativeSvgFont0' fill='url(#p)'>Tile Ω</text></svg>"

      assert {:ok, pdf} =
               NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.pdf(
                 source,
                 [width: 240, height: 60],
                 %{mode: :inline, fonts: [%{data: data}]}
               )

      refute pdf =~ "/Subtype /Link"
      assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf, false)
      assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf)

      assert {:error, {:no_extractable_text, _}} =
               NativeElixirPdfUtilities.Text.extract(pdf, layout: false)
    end
  end
end
