defmodule NativeElixirPdfUtilities.HtmlToPdf.SvgClippingTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.{HtmlToPdf, Limits, Text}
  alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, Style, SvgBackend}
  alias NativeElixirPdfUtilities.Validators.{SvgClipValidator, SvgStyleValidator, SvgValidator}

  setup do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    %{limits: limits}
  end

  test "host cascade composes local clip references with inherited clip rules" do
    html = """
    <style>.art{clip-path:url(#cut)} #art{clip-path:none} g{clip-rule:evenodd}</style>
    <svg width=40 height=40><defs><g  /></defs></svg>
    """

    assert {:ok, _pdf} = HtmlToPdf.render(html)

    html = """
    <style>.art{clip-path:url(#cut)} #art{clip-path:none} defs{clip-rule:evenodd}</style>
    <svg width=40 height=40><defs><clipPath id=cut><path d='M0 0H40V40H0Z M10 10H30V30H10Z' /></clipPath></defs>
    <rect class=art width=40 height=40 fill=red /><rect class=art id=art width=5 height=5 fill=blue /></svg>
    """

    assert {:ok, dom} = HtmlParser.parse_detailed(html)
    assert {:ok, %{children: [svg]}} = Style.compute_detailed(dom)
    assert svg.style.svg_scene.source =~ "clip-path=\"url(#cut)\""
    assert svg.style.svg_scene.source =~ "clip-rule=\"evenodd\""
    assert {:ok, pdf} = HtmlToPdf.render(html)
    assert streams(pdf) =~ "W*"
    assert pdf =~ "/Subtype /Form"
    refute pdf =~ "/Subtype /Image"
  end

  test "clip definitions preserve full path grammar and transformed single geometry" do
    for shape <- [
          "<path d='M2 2H30V30H2Z M6 6C7 8 9 8 10 6S12 2 14 6Q18 10 20 6T24 6A3 2 20 0 1 28 8z' />",
          "<rect width='30' height='30' rx='4' transform='rotate(10 15 15)' />",
          "<circle cx='15' cy='15' r='12' />",
          "<ellipse cx='15' cy='15' rx='12' ry='8' />",
          "<polygon points='0,0 30,0 15,30' />",
          "<polyline points='0,0 30,0 15,30' />"
        ] do
      source =
        "<svg width='40' height='40'><defs><clipPath id='c' transform='translate(2 2)'>#{shape}</clipPath></defs><rect width='40' height='40' clip-path='url(#c)' /></svg>"

      assert {:ok, pdf} = SvgBackend.vectorize(source, [width: 40, height: 40], %{mode: :inline})
      assert streams(pdf) =~ "W\n"
    end
  end

  test "unfinished clip definition scopes diagnose before painting" do
    for definition <- [
          "<clipPath id=c><rect /><circle /></clipPath>",
          "<clipPath id=c />",
          "<clipPath id=c clipPathUnits=objectBoundingBox><rect /></clipPath>",
          "<clipPath id=c><g><rect /></g></clipPath>",
          "<clipPath id=c><line x2=20 y2=20 /></clipPath>",
          "<clipPath id=c clip-path='url(#other)'><rect /></clipPath>",
          "<clipPath id=c><rect clip-path='url(#other)' /></clipPath>"
        ] do
      assert {:error, {:unsupported_html, diagnostic}} =
               HtmlToPdf.render(
                 "<svg width=40 height=40><defs><clipPath id=other><circle r=10 /></clipPath>#{definition}</defs><rect width=40 height=40 clip-path='url(#c)' /></svg>"
               )

      assert diagnostic.module == SvgClipValidator
      assert diagnostic.stage == :layout
      assert diagnostic.reason == :unsupported_html
      assert diagnostic.operation == :prepare
      assert diagnostic.source in ["clipPath", "rect"]
      assert diagnostic.message != ""
    end
  end

  test "malformed definitions and wrong local targets use shared diagnostics" do
    for content <- [
          "<clipPath id=c><rect /></clipPath>",
          "<defs><clipPath><rect /></clipPath></defs>",
          "<rect id=c /><rect clip-path='url(#c)' />"
        ] do
      assert {:error,
              {:invalid_document,
               %{module: SvgClipValidator, stage: :layout, operation: :prepare}}} =
               HtmlToPdf.render("<svg width=40 height=40>#{content}</svg>")
    end

    assert {:error, {:unsupported_html, %{module: SvgClipValidator, source: "svg"}}} =
             HtmlToPdf.render(
               "<svg width=40 height=40 clip-path='url(#c)'><defs><clipPath id=c><rect /></clipPath></defs></svg>"
             )

    assert {:error, {:invalid_document, %{message: message}}} =
             SvgBackend.vectorize(
               "<svg width='40' height='40'><rect clip-path='url(#missing)' /></svg>",
               [width: 40, height: 40],
               %{mode: :inline}
             )

    assert message =~ "no target"

    for shape <- ["<rect transform='bad' />", "<rect />"] do
      transform = if shape == "<rect />", do: "transform='bad'", else: ""

      source =
        "<svg width='40' height='40'><defs><clipPath id='c' #{transform}>#{shape}</clipPath></defs></svg>"

      assert {:error, {:invalid_document, %{module: SvgClipValidator, message: message}}} =
               SvgBackend.vectorize(source, [width: 40, height: 40], %{mode: :inline})

      assert message =~ "affine transform"
    end

    source =
      "<svg width='40' height='40'><defs><clipPath id='c'><rect width='20' height='20' /></clipPath></defs><rect width='40' height='40' style='clip-path:url(#c)' /></svg>"

    assert {:error, {:unsupported_html, %{message: message}}} =
             SvgBackend.vectorize(source, [width: 40, height: 40], %{mode: :inline})

    assert message =~ "host cascade"

    assert {:error, {:unsupported_html, _}} =
             SvgBackend.rasterize(source, [width: 40, height: 40], %{mode: :image})
  end

  test "direct backend validates nested artwork in clip geometry through the shared structure validator" do
    source =
      "<svg width='40' height='40'><defs><clipPath id='c'><rect width='20' height='20'><circle r='10' /></rect></clipPath></defs><rect width='40' height='40' clip-path='url(#c)' /></svg>"

    assert {:error,
            {:invalid_document,
             %{
               module: NativeElixirPdfUtilities.Validators.SvgSceneValidator,
               source: "rect",
               operation: :prepare,
               message: message
             }}} =
             SvgBackend.vectorize(source, [width: 40, height: 40], %{mode: :inline})

    assert message =~ "not nested artwork"
  end

  test "hidden clip geometry emits an empty clip while retaining visible sibling artwork" do
    source =
      "<svg width='40' height='40'><defs><clipPath id='c'><title>Clip</title><desc>Metadata</desc><rect width='30' height='30' visibility='hidden' /></clipPath></defs><rect width='40' height='40' clip-path='url(#c)' fill='red' /><rect width='5' height='5' fill='blue' /></svg>"

    assert {:ok, pdf} = SvgBackend.vectorize(source, [width: 40, height: 40], %{mode: :inline})
    assert streams(pdf) =~ "0 0 0 0 re\nW\nn\n"
    assert streams(pdf) =~ "0 0 1 rg"
  end

  test "clip URL grammar accepts bounded quoted fragments and rejects external or malformed references" do
    for value <- ["url(#cut)", "url( '#cut' )", "url(\"#cut\")"] do
      assert {:ok, {:fragment, "cut"}} = SvgStyleValidator.parse("clip-path", value)
    end

    for value <- [
          "url(https://example.test/c.svg#cut)",
          "circle(20px)",
          "url('#cut\")",
          "url(#)",
          :bad
        ] do
      assert :error = SvgStyleValidator.parse("clip-path", value)
    end

    assert {:error, {:invalid_document, _}} =
             SvgBackend.vectorize(
               "<svg width='40' height='40'><rect clip-path='circle(20px)' /></svg>",
               [width: 40, height: 40],
               %{mode: :inline}
             )
  end

  test "reused clip geometry charges shared expansion, reference and surface budgets", %{
    limits: limits
  } do
    source =
      "<svg width='40' height='40'><defs><clipPath id='c'><path d='M0 0H20V20H0Z' /></clipPath></defs><rect width='40' height='40' clip-path='url(#c)' /><rect width='40' height='40' clip-path='url(#c)' /></svg>"

    assert {:ok, summary} = SvgValidator.validate_processing(source, %{mode: :inline})
    assert summary.clip_references == 2
    assert summary.expanded_work > summary.nodes
    assert length(summary.hrefs) == 2

    for {key, value} <- [
          max_svg_references: 1,
          max_svg_expanded_path_bytes: 20,
          max_svg_stroke_work: 10,
          max_aggregate_svg_surface_pixels: 1000
        ] do
      Limits.install(Map.put(limits, key, value))

      assert {:error, {:resource_limit_exceeded, %{message: message}}} =
               SvgBackend.rasterize(source, [width: 40, height: 40], %{mode: :inline})

      assert message =~ Atom.to_string(key)
    end
  end

  test "vector-only conversion reserves no raster surfaces while raster and scene probes remain bounded",
       %{limits: limits} do
    source =
      "<svg width='40' height='40'><defs><clipPath id='c'><rect width='20' height='20' /></clipPath></defs><g clip-path='url(#c)' opacity='.5'><rect width='40' height='40' /></g><g opacity='.5'><rect width='5' height='5' /></g></svg>"

    assert {:ok, summary} = SvgValidator.validate_processing(source, %{mode: :inline})
    assert summary.clip_references == 1
    assert summary.opacity_layers == 1
    Limits.install(Map.put(limits, :max_aggregate_svg_surface_pixels, 1))
    assert {:ok, _pdf} = SvgBackend.vectorize(source, [width: 40, height: 40], %{mode: :inline})

    for result <- [
          SvgBackend.rasterize(source, [width: 40, height: 40], %{mode: :inline}),
          HtmlToPdf.render(source)
        ] do
      assert {:error, {:resource_limit_exceeded, %{message: message}}} = result
      assert message =~ "max_aggregate_svg_surface_pixels"
    end

    Limits.install(Map.put(limits, :max_svg_stroke_work, 1))

    assert {:error, {:resource_limit_exceeded, %{message: message}}} =
             SvgBackend.vectorize(source, [width: 40, height: 40], %{mode: :inline})

    assert message =~ "max_svg_stroke_work"
    Limits.install(Map.put(limits, :max_svg_raster_dimension, 10))

    assert {:error, {:resource_limit_exceeded, %{message: message}}} =
             SvgBackend.vectorize(source, [width: 40, height: 40], %{mode: :inline})

    assert message =~ "40x40"
    assert message =~ "10-pixel per-axis limit"
  end

  test "original path clip browser fixture remains vector artwork with surrounding logical text" do
    html =
      File.read!(
        Path.expand(
          "../fixtures/html_to_pdf/browser_parity/inline_svg_path_clips_scoped.html",
          __DIR__
        )
      )

    assert {:ok, pdf} = HtmlToPdf.render(html)
    assert {:ok, text} = Text.extract(pdf)
    assert text =~ "host cascade Ω"
    assert text =~ "viewports café"
    assert text =~ "Clip artwork survived"
    assert streams(pdf) =~ "W*"
    assert streams(pdf) =~ "W\n"
    refute pdf =~ "/Subtype /Image"
  end

  defp streams(pdf) do
    assert {:ok, context} = NativeElixirPdfUtilities.Pdf.Reader.read(pdf)
    context.objects |> Map.values() |> Enum.map_join("\n", &(Map.get(&1, :stream) || ""))
  end
end
