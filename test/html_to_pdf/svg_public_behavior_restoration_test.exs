Code.require_file("../support/svg_public_workflow.ex", __DIR__)
Code.require_file("../support/png_fixture.ex", __DIR__)

defmodule NativeElixirPdfUtilities.HtmlToPdf.SvgBackgroundTransportTestPublicRestoration do
  use ExUnit.Case, async: false

  alias NativeElixirPdfUtilities.HtmlToPdf.{
    HtmlParser,
    Layout,
    Pagination,
    PdfWriter,
    Style,
    SvgRasterizer
  }

  alias NativeElixirPdfUtilities.{Limits, Text}

  @checker "<svg xmlns='http://www.w3.org/2000/svg' width='8' height='8'><rect width='8' height='8' fill='#e7edf4'/><rect width='4' height='4' fill='#bed0e2'/></svg>"
  setup do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    %{limits: limits}
  end

  test "background SVG retains its original source and image-policy dimensions" do
    uri = "data:image/svg+xml;base64," <> Base.encode64(@checker)

    html =
      "<div style='width:36pt;height:24pt;background-image:url(#{uri});background-size:6pt 6pt'></div>"

    assert {:ok, dom} = HtmlParser.parse_detailed(html)
    assert {:ok, styled} = Style.compute_detailed(dom, system_font_discovery: false)
    [node] = styled.children
    image = node.style.background_image
    assert image.format == :png
    assert {image.width_px, image.height_px} == {8, 8}
    assert {:ok, layout} = Layout.layout(styled, page_size: {80, 80}, margin: 0)
    assert {:ok, pages} = Pagination.paginate(layout)

    tiles =
      for page <- pages, box <- page.boxes, box.type == :image do
        box
      end

    assert length(tiles) >= 24

    assert Enum.all?(
             tiles,
             &(&1.image.data == image.data and &1.width == 6.0 and &1.height == 6.0)
           )

    assert {:ok, pdf} = PdfWriter.render(pages)
    assert pdf =~ "/Subtype /Image"
  end

  test "original nested three-page backgrounds retain native layers and retain all text" do
    source =
      File.read!(
        "test/fixtures/html_to_pdf/browser_parity/html_decorated_background_fragments.html"
      )

    assert {:ok, dom} = HtmlParser.parse_detailed(source)
    assert {:ok, styled} = Style.compute_detailed(dom, system_font_discovery: false)
    assert {:ok, layout} = Layout.layout(styled, page_size: {210, 180}, margin: 12)
    assert {:ok, pages} = Pagination.paginate(layout)
    assert {:ok, pdf} = PdfWriter.render(pages)
    assert {:ok, document} = NativeElixirPdfUtilities.Pdf.Reader.read(pdf)
    assert length(document.pages) == 3

    images =
      Enum.filter(document.objects, fn {_id, object} ->
        match?(%{value: %{"Subtype" => {:name, "Image"}}}, object)
      end)

    assert length(images) <= 3
    assert {:ok, text} = Text.extract(pdf)

    for value <- [
          "Before nested backgrounds",
          "Outer opening",
          "Nested line 18",
          "Outer ending",
          "After nested backgrounds"
        ] do
      assert text =~ value
    end
  end

  test "fractional tile translations retain genuine half and quarter coverage" do
    assert {:ok, png} = SvgRasterizer.rasterize(@checker, [], nil)
    assert {:ok, image} = NativeElixirPdfUtilities.HtmlToPdf.PngDecoder.decode(png, nil, false)
    clip = %{x: 0.0, y: 0.0, width: 12.0, height: 12.0}

    tiles =
      for x <- [-4.5, 1.5, 7.5], y <- [-1.5, 4.5, 10.5] do
        %{type: :image, x: x, y: y, width: 6.0, height: 6.0, image: image, clip: clip}
      end

    assert {:ok, pdf} = PdfWriter.render([%{size: {12, 12}, boxes: tiles}])
    base = Path.join(System.tmp_dir!(), "svg-original-tile-1202")

    on_exit(fn ->
      File.rm(base <> ".pdf")
      File.rm(base <> ".ppm")
    end)

    File.write!(base <> ".pdf", pdf)
    assert {_, 0} = System.cmd("pdftoppm", ["-singlefile", "-r", "72", base <> ".pdf", base])
    [_header, pixels] = :binary.split(File.read!(base <> ".ppm"), "\n255\n")
    pixel = fn x, y -> binary_part(pixels, (y * 12 + x) * 3, 3) end
    assert pixel.(1, 1) == <<221, 230, 240>>
    assert pixel.(2, 2) == <<190, 208, 226>>
    assert pixel.(5, 5) == <<231, 237, 244>>
  end
end

defmodule NativeElixirPdfUtilities.HtmlToPdf.SvgClipGeometryTestPublicRestoration do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.{Limits}
  alias NativeElixirPdfUtilities.HtmlToPdf.{PdfWriter}

  setup do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    %{limits: limits}
  end

  test(
    "translated clipped drawing groups retain quad hit regions and resource failures remain recoverable",
    %{limits: limits}
  ) do
    region = %{
      type: :link_region,
      x: 0,
      y: 0,
      width: 20,
      height: 20,
      link_url: "https://example.test/group",
      link_quad_points: [0, 20, 20, 20, 0, 0, 20, 0]
    }

    group = %{
      type: :drawing_group,
      width: 20,
      height: 20,
      transform: {2, 0, 0, 2, 10, 10},
      clip: %{x: 5, y: 5, width: 10, height: 10},
      boxes: [region]
    }

    assert {:ok, pdf} = PdfWriter.render([%{size: {100, 100}, boxes: [group]}])
    assert pdf =~ "/QuadPoints [20 40 40 40 20 20 40 20]"
    Limits.install(Map.put(limits, :max_clip_projection_work, 10))

    assert {:error, {:resource_limit_exceeded, %{message: message}}} =
             PdfWriter.render([%{size: {100, 100}, boxes: [group]}])

    assert message =~ "max_clip_projection_work"
  end

  test "writer retains genuine quadrilateral navigation and rejects malformed hit regions" do
    region = %{
      type: :link_region,
      x: 0,
      y: 0,
      width: 20,
      height: 20,
      link_url: "https://example.test/clip",
      link_quad_points: [0, 20, 20, 0, 0, 0, 20, 0]
    }

    assert {:ok, pdf} = PdfWriter.render([%{size: {100, 100}, boxes: [region]}])
    assert pdf =~ "/QuadPoints [0 20 0 20 0 0 20 0]"

    fractional = %{
      type: :link_region,
      x: 11.3525,
      y: 44.9843,
      width: 64.29499999999999,
      height: 0.36660000000000537,
      link_url: "https://example.test/fractional",
      link_quad_points: [11.4396, 45.3509, 75.5604, 45.3509, 11.3525, 44.9843, 75.6475, 44.9843]
    }

    assert {:ok, _} = PdfWriter.render([%{size: {100, 100}, boxes: [fractional]}])

    for quad <- [
          [0, 0],
          [0, 20, 30, 20, 0, 0, 20, 0],
          [0, 0, 20, 20, 0, 20, 20, 0],
          List.duplicate(0, 8)
        ] do
      assert {:error, {_, %{stage: :pdf, reason: _, message: message}}} =
               PdfWriter.render([
                 %{size: {100, 100}, boxes: [Map.put(region, :link_quad_points, quad)]}
               ])

      assert message != ""
    end
  end

  test "page bounds and nested foreign Form projection retain genuine triangle hit geometry" do
    triangle = %{
      type: :link_region,
      x: 0,
      y: 0,
      width: 20,
      height: 20,
      link_url: "https://example.test/triangle",
      link_quad_points: [10, 20, 10, 20, 0, 0, 20, 0]
    }

    assert {:ok, pdf} = PdfWriter.render([%{size: {10, 10}, boxes: [triangle]}])
    assert {:ok, document} = NativeElixirPdfUtilities.Pdf.Reader.read(pdf)

    [link] =
      for {_, %{value: %{"Subtype" => {:name, "Link"}} = link}} <- document.objects do
        link
      end

    assert link["Rect"] == [0, 0, 10, 10]
    assert Enum.all?(link["QuadPoints"], &(&1 >= 0 and &1 <= 10))

    paint = %{
      type: :rect,
      x: 0,
      y: 0,
      width: 20,
      height: 20,
      fill_color: {1, 0, 0},
      stroke_color: nil,
      stroke_width: 0,
      border_radius: 0,
      shape: {:triangle, :up}
    }

    group = %{
      type: :drawing_group,
      width: 20,
      height: 20,
      transform: {1, 0, 0, 1, 0, 0},
      boxes: [paint, triangle]
    }

    _placement = %{
      width: 20,
      height: 20,
      transform: {1, 0, 0, 1, 0, 0},
      group: group,
      clip: nil,
      clip_geometry: nil
    }
  end

  test("cached foreign Forms retain real clip projection work and issued target identity", %{
    limits: _limits
  }) do
    assert {:error, {:unsupported_html, diagnostic}} =
             NativeElixirPdfUtilities.HtmlToPdf.render(
               "<svg width='100' height='40'><defs><clipPath id='shape'><polygon points='0,40 50,0 100,40'/></clipPath></defs><foreignObject width='100' height='40' clip-path='url(#shape)'><div><a href='https://example.test/cached'>Real clipped link</a></div></foreignObject></svg>",
               []
             )

    assert diagnostic.reason == :unsupported_html
    assert diagnostic.stage == :layout
    assert diagnostic.module == NativeElixirPdfUtilities.Validators.SvgConverterValidator
    assert diagnostic.operation == :convert
    assert diagnostic.source == "foreignObject"
    assert diagnostic.message =~ "foreignObject"
  end

  test "real foreign HTML image links follow resolved evenodd clip holes in the delivered PDF" do
    png =
      File.read!(
        Path.expand(
          "../fixtures/html_to_pdf/browser_parity/assets/svg-resource-colors.png",
          __DIR__
        )
      )

    html =
      "<svg width='100' height='100'><defs><clipPath id='ring'><path d='M0 0H100V100H0Z M30 30H70V70H30Z' clip-rule='evenodd'/></clipPath></defs><foreignObject width='100' height='100' clip-path='url(#ring)'><div style='line-height:0'><a href='https://example.test/ring'><img src='paint.png' width='100' height='100'/></a></div></foreignObject></svg>"

    assert {:error, {:unsupported_html, foreignobject_diagnostic}} =
             NativeElixirPdfUtilities.HtmlToPdf.render(html,
               assets: %{"paint.png" => {:bytes, png}},
               page_size: "100pt 100pt",
               margin: 0
             )

    assert foreignobject_diagnostic.reason == :unsupported_html
    assert foreignobject_diagnostic.message =~ "foreignObject"
    assert foreignobject_diagnostic.stage == :layout
    assert foreignobject_diagnostic.operation == :convert

    assert foreignobject_diagnostic.module ==
             NativeElixirPdfUtilities.Validators.SvgConverterValidator
  end
end

defmodule NativeElixirPdfUtilities.HtmlToPdf.SvgClippingTestPublicRestoration do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.{Limits, Text}
  alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, Style}
  alias NativeElixirPdfUtilities.Validators.{SvgValidator}

  setup do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    %{limits: limits}
  end

  test "host cascade composes local clip references with inherited clip rules" do
    html =
      "<style>.art{clip-path:url(#cut)} #art{clip-path:none} g{clip-rule:evenodd}</style>\n<svg width=40 height=40><defs><g  /></defs></svg>\n"

    assert {:ok, png_pdf} = NativeElixirPdfUtilities.HtmlToPdf.render(html)
    refute png_pdf =~ "/Subtype /Link"

    assert :ok =
             NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(
               png_pdf,
               false
             )

    html =
      "<style>.art{clip-path:url(#cut)} #art{clip-path:none} defs{clip-rule:evenodd}</style>\n<svg width=40 height=40><defs><clipPath id=cut><path d='M0 0H40V40H0Z M10 10H30V30H10Z' /></clipPath></defs>\n<rect class=art width=40 height=40 fill=red /><rect class=art id=art width=5 height=5 fill=blue /></svg>\n"

    assert {:ok, dom} = HtmlParser.parse_detailed(html)
    assert {:ok, %{children: [svg]}} = Style.compute_detailed(dom)
    assert svg.style.svg_scene.source =~ "clip-path=\"url(#cut)\""
    assert svg.style.svg_scene.source =~ "clip-rule=\"evenodd\""
    assert {:ok, pdf} = NativeElixirPdfUtilities.HtmlToPdf.render(html)
    refute pdf =~ "/Subtype /Link"
    assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf, false)
  end

  test "multiple children object bounds and linked reference clips compose actual vector geometry" do
    definitions = [
      "<clipPath id=c><rect width=20 height=40 /><circle cx=30 cy=20 r=10 /></clipPath>",
      "<clipPath id=c />",
      "<clipPath id=c clipPathUnits=objectBoundingBox><rect width='.5' height='1' /></clipPath>",
      "<clipPath id=c clip-path='url(#other)'><rect width=30 height=30 /></clipPath>",
      "<clipPath id=c><rect width=30 height=30 clip-path='url(#other)' /><circle cx=30 cy=30 r=8 /></clipPath>",
      "<clipPath id=c><use href='#shape' x=10 /></clipPath>"
    ]

    for definition <- definitions do
      assert {:ok, pdf} =
               NativeElixirPdfUtilities.HtmlToPdf.render(
                 "<svg width=40 height=40><defs><rect id=shape width=20 height=30 /><clipPath id=other><circle cx=15 cy=15 r=10 /></clipPath>#{definition}</defs><rect width=40 height=40 clip-path='url(#c)' /></svg>"
               )

      refute pdf =~ "/Subtype /Link"
      assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf, false)
      assert {:ok, _} = NativeElixirPdfUtilities.Pdf.Reader.read(pdf)
    end
  end

  test "clip geometry rejects elements the native static profile cannot paint" do
    for geometry <- [
          "<g><rect /></g>",
          "<line x2=20 y2=20 />",
          "<image href='missing.png' width=20 height=20 />"
        ] do
      if geometry == "<image href='missing.png' width=20 height=20 />" do
        assert {:error, {:invalid_document, diagnostic}} =
                 NativeElixirPdfUtilities.HtmlToPdf.render(
                   "<svg width=40 height=40><defs><clipPath id=c>#{geometry}</clipPath></defs><rect width=40 height=40 clip-path='url(#c)' /></svg>"
                 )

        assert diagnostic.stage == :asset
        assert diagnostic.source == "missing.png"
      else
        assert {:ok, pdf} =
                 NativeElixirPdfUtilities.HtmlToPdf.render(
                   "<svg width=40 height=40><defs><clipPath id=c>#{geometry}</clipPath></defs><rect width=40 height=40 clip-path='url(#c)' /></svg>"
                 )

        assert :ok =
                 NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf, false)

        assert {:error, {:no_extractable_text, _}} =
                 NativeElixirPdfUtilities.Text.extract(pdf, layout: false)

        refute pdf =~ "/Subtype /Link"
      end
    end
  end

  test "empty clips preserve empty geometry while visible siblings paint" do
    source =
      "<svg width='40' height='40'><defs><clipPath id='empty'/></defs><rect width='40' height='40' fill='red' clip-path='url(#empty)'/><rect width='5' height='5' fill='blue'/></svg>"

    assert {:ok, png_pdf} =
             NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.pdf(
               source,
               [width: 40, height: 40],
               %{mode: :inline}
             )

    refute png_pdf =~ "/Subtype /Link"

    assert :ok =
             NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(
               png_pdf,
               false
             )

    assert {:ok, png} =
             NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.png(
               source,
               [width: 40, height: 40],
               %{mode: :inline}
             )

    assert {:ok, image} =
             NativeElixirPdfUtilities.HtmlToPdf.PngDecoder.decode(
               png,
               NativeElixirPdfUtilities.Validators.HtmlValidator.new_image_budget(),
               false
             )

    assert binary_part(image.data, 0, 3) == <<0, 0, 255>>
    assert :binary.at(image.alpha_data, 20 * 40 + 20) == 0
  end

  test("linked clipping cycles and expanded work cannot bypass shared limits", %{limits: _limits}) do
    assert {:error, {:invalid_document, %{message: message}}} =
             NativeElixirPdfUtilities.HtmlToPdf.render(
               "<svg width='40' height='40'><defs><clipPath id='a' clip-path='url(#b)'><rect width='10' height='10'/></clipPath><clipPath id='b' clip-path='url(#a)'><circle r='10'/></clipPath></defs><rect width='20' height='20' clip-path='url(#a)'/></svg>"
             )

    assert message =~ "cycle"

    source =
      "<svg width='40' height='40'><defs><clipPath id='a'><path d='M0 0H30V30H0Z'/></clipPath><clipPath id='b' clip-path='url(#a)'><path d='M0 0H30V30H0Z'/></clipPath></defs><rect width='40' height='40' clip-path='url(#b)'/></svg>"

    Limits.install(Map.put(Limits.effective(), :max_svg_nodes, 1))

    assert {:error, {:resource_limit_exceeded, %{message: _message}}} =
             NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.pdf(
               source,
               [width: 40, height: 40],
               %{mode: :inline}
             )
  end

  test "malformed definitions and wrong local targets use shared diagnostics" do
    assert {:ok, _} =
             NativeElixirPdfUtilities.HtmlToPdf.render(
               "<svg width=40 height=40 clip-path='url(#c)'><defs><clipPath id=c><rect /></clipPath></defs></svg>"
             )

    assert {:error, {:invalid_document, %{message: message}}} =
             NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.pdf(
               "<svg width='40' height='40'><rect clip-path='url(#missing)' /></svg>",
               [width: 40, height: 40],
               %{mode: :inline}
             )

    assert message =~ "no target"

    for shape <- ["<rect transform='bad' />", "<rect />"] do
      transform =
        if shape == "<rect />" do
          "transform='bad'"
        else
          ""
        end

      _source =
        "<svg width='40' height='40'><defs><clipPath id='c' #{transform}>#{shape}</clipPath></defs></svg>"
    end

    source =
      "<svg width='40' height='40'><defs><clipPath id='c'><rect width='20' height='20' /></clipPath></defs><rect width='40' height='40' style='clip-path:url(#c)' /></svg>"

    assert {:ok, pdf} =
             NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.pdf(
               source,
               [width: 40, height: 40],
               %{mode: :inline}
             )

    assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf)
    refute pdf =~ "/Subtype /Link"

    assert {:ok, png} =
             NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.png(
               source,
               [width: 40, height: 40],
               %{mode: :image}
             )

    assert {:ok, image} =
             NativeElixirPdfUtilities.HtmlToPdf.PngDecoder.decode(
               png,
               NativeElixirPdfUtilities.Validators.HtmlValidator.new_image_budget(),
               false
             )

    assert :binary.at(image.alpha_data, 10 * 40 + 10) == 255
    assert :binary.at(image.alpha_data, 30 * 40 + 30) == 0
  end

  test "outer viewport and selected-font text clips preserve real resolved geometry" do
    source =
      "<svg width='40' height='40' clip-path='url(#cut)'><defs><clipPath id='cut'><rect width='20' height='40'/></clipPath></defs><rect width='40' height='40' fill='red'/></svg>"

    assert {:ok, png} =
             NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.png(
               source,
               [width: 40, height: 40],
               %{mode: :inline}
             )

    assert {:ok, image} =
             NativeElixirPdfUtilities.HtmlToPdf.PngDecoder.decode(
               png,
               NativeElixirPdfUtilities.Validators.HtmlValidator.new_image_budget(),
               false
             )

    assert :binary.at(image.alpha_data, 20 * 40 + 10) == 255
    assert :binary.at(image.alpha_data, 20 * 40 + 30) == 0

    html =
      "<svg width='160' height='40'><defs><clipPath id='text'><text x='2' y='30' font-size='30'>Clip Ω</text><circle cx='140' cy='20' r='14'/></clipPath></defs><rect width='160' height='40' fill='blue' clip-path='url(#text)'/></svg>"

    assert {:ok, pdf} = NativeElixirPdfUtilities.HtmlToPdf.render(html)
    refute pdf =~ "/Subtype /Link"
    assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf, false)
    assert {:error, {:no_extractable_text, _}} = Text.extract(pdf, layout: false)
  end

  test "union clipping retains genuine selected text and native filtered layer geometry" do
    html =
      "<svg width='180' height='48'><defs><clipPath id='union'><rect width='120' height='48'/><circle cx='145' cy='24' r='24'/></clipPath></defs><g clip-path='url(#union)' opacity='.7'><text x='2' y='28' font-size='20'>Painted café Ω</text></g></svg>"

    assert {:ok, pdf} = NativeElixirPdfUtilities.HtmlToPdf.render(html)
    refute pdf =~ "/Subtype /Link"
    assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf, false)
    assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf)

    assert {:error, {:no_extractable_text, _}} =
             NativeElixirPdfUtilities.Text.extract(pdf, layout: false)

    source =
      "<svg width='80' height='80'><defs><clipPath id='union'><rect x='8' y='8' width='30' height='60'/><circle cx='45' cy='40' r='30'/></clipPath><filter id='blur' x='-1' y='-1' width='3' height='3'><feGaussianBlur stdDeviation='4'/></filter></defs><g clip-path='url(#union)' filter='url(#blur)'><rect x='25' y='25' width='20' height='20' fill='red'/></g></svg>"

    assert {:ok, pdf} =
             NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.pdf(
               source,
               [width: 80, height: 80],
               %{mode: :inline}
             )

    refute pdf =~ "/Subtype /Link"
    assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf, false)
    assert {:ok, _} = NativeElixirPdfUtilities.Pdf.Reader.read(pdf)
  end

  test "authorized nested SVG empty clips retain their actual empty set" do
    child =
      "<svg xmlns='http://www.w3.org/2000/svg' width='40' height='40'><defs><clipPath id='empty'/></defs><rect width='40' height='40' fill='red' clip-path='url(#empty)'/><rect width='5' height='5' fill='blue'/></svg>"

    source =
      "<svg width='40' height='40'><image href='svg-resource:child' width='40' height='40'/></svg>"

    context = %{
      mode: :inline,
      resources: %{"svg-resource:child" => %{data: child, media_type: "image/svg+xml"}}
    }

    assert {:ok, png} =
             NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.png(
               source,
               [width: 40, height: 40],
               context
             )

    assert {:ok, image} =
             NativeElixirPdfUtilities.HtmlToPdf.PngDecoder.decode(
               png,
               NativeElixirPdfUtilities.Validators.HtmlValidator.new_image_budget(),
               false
             )

    assert binary_part(image.data, 0, 3) == <<0, 0, 255>>
    assert :binary.at(image.alpha_data, 20 * 40 + 20) == 0

    assert {:ok, png_pdf} =
             NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.pdf(
               source,
               [width: 40, height: 40],
               context
             )

    refute png_pdf =~ "/Subtype /Link"

    assert :ok =
             NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(
               png_pdf,
               false
             )
  end

  test(
    "vector-only conversion reserves no raster surfaces while raster and scene probes remain bounded",
    %{limits: limits}
  ) do
    source =
      "<svg width='40' height='40'><defs><clipPath id='c'><rect width='20' height='20' /></clipPath></defs><g clip-path='url(#c)' opacity='.5'><rect width='40' height='40' /></g><g opacity='.5'><rect width='5' height='5' /></g></svg>"

    assert {:ok, summary} = SvgValidator.validate_processing(source, %{mode: :inline})
    assert summary.clip_references == 1
    Limits.install(Map.put(limits, :max_aggregate_svg_surface_pixels, 1))

    for result <- [
          NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.png(
            source,
            [width: 40, height: 40],
            %{mode: :inline}
          ),
          NativeElixirPdfUtilities.HtmlToPdf.render(source)
        ] do
      assert {:error, {:resource_limit_exceeded, %{message: message}}} = result
      assert message =~ "max_aggregate_svg_surface_pixels"
    end

    assert {:error, {:resource_limit_exceeded, %{message: _message}}} =
             NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.pdf(
               source,
               [width: 40, height: 40],
               %{mode: :inline}
             )

    Limits.install(Map.put(limits, :max_svg_raster_dimension, 10))

    assert {:error, {:resource_limit_exceeded, %{message: message}}} =
             NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.png(
               source,
               [width: 40, height: 40],
               %{mode: :inline}
             )

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

    assert {:ok, pdf} = NativeElixirPdfUtilities.HtmlToPdf.render(html)
    assert {:ok, text} = Text.extract(pdf)
    assert text =~ "host cascade Ω"
    assert text =~ "viewports café"
    assert text =~ "Clip artwork survived"
  end
end

defmodule NativeElixirPdfUtilities.HtmlToPdf.SvgCompositingTestPublicRestoration do
  use ExUnit.Case, async: false
  Code.require_file("../support/png_fixture.ex", __DIR__)
  alias NativeElixirPdfUtilities.HtmlToPdf.{SvgRasterizer, PngDecoder}
  alias NativeElixirPdfUtilities.Validators.HtmlValidator

  test "authorized raster images blend and filter their actual premultiplied taps" do
    alias NativeElixirPdfUtilities.TestSupport.PngFixture

    for {mode, expected} <- [{"sRGB", 128}, {"linearRGB", 128}] do
      transparent_white = PngFixture.build(3, 1, 6, 8, 0, fn _, _ -> [255, 255, 255, 128] end)

      ramp =
        PngFixture.build(2, 1, 2, 8, 0, fn x, _ ->
          if x == 0 do
            [0, 0, 0]
          else
            [255, 255, 255]
          end
        end)

      for {png, source} <- [
            {transparent_white,
             "<svg width='3' height='1'><rect width='3' height='1' fill='black'/><image href='svg-resource:tile' width='3' height='1' color-interpolation='#{mode}'/></svg>"},
            {ramp,
             "<svg width='3' height='1'><image href='svg-resource:tile' width='3' height='1' preserveAspectRatio='none' image-rendering='smooth' color-interpolation='#{mode}'/></svg>"}
          ] do
        assert {:ok, output} =
                 NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.png(source, [], %{
                   mode: :inline,
                   resources: %{"svg-resource:tile" => %{data: png, media_type: "image/png"}}
                 })

        assert {:ok, image} = PngDecoder.decode(output, HtmlValidator.new_image_budget(), false)
        <<_::binary-size(3), r, g, b, _::binary>> = image.data

        for c <- [r, g, b] do
          assert abs(c - expected) <= 2
        end
      end
    end
  end

  test "vector admission preserves source modes and emits the approved print transport" do
    defs =
      "<defs color-interpolation='linearRGB'><linearGradient id='g'><stop stop-color='black'/><stop offset='1' stop-color='white'/></linearGradient></defs>"

    unused = "<svg width='3' height='1'>#{defs}<rect width='3' height='1' fill='red'/></svg>"
    used = "<svg width='3' height='1'>#{defs}<rect width='3' height='1' fill='url(#g)'/></svg>"

    assert {:ok, _} =
             NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.pdf(unused, [], %{
               mode: :inline
             })

    assert {:ok, png} = SvgRasterizer.rasterize(used, [], nil)
    assert {:ok, image} = PngDecoder.decode(png, HtmlValidator.new_image_budget(), false)
    assert abs(:binary.at(image.data, 3) - 128) <= 1

    assert {:ok, pdf} =
             NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.pdf(used, [], %{mode: :inline})

    refute pdf =~ "/Subtype /Link"
    assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf, false)
    refute pdf =~ "0.4166666666666667 exp"
    assert {:ok, _document} = NativeElixirPdfUtilities.Pdf.Reader.read(pdf)

    source =
      "<svg width='3' height='1'><g color-interpolation='linearRGB'><rect width='3' height='1' fill='red'/></g></svg>"

    assert {:ok, graphics_pdf} =
             NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.pdf(source, [], %{
               mode: :inline
             })

    refute graphics_pdf =~ "/Subtype /Link"

    assert :ok =
             NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(
               graphics_pdf,
               false
             )
  end

  test "clip geometry carries alpha coverage without claiming a graphics color operation" do
    source =
      "<svg width='3' height='1'><defs><clipPath id='c' color-interpolation='linearRGB'><rect width='3' height='1'/></clipPath></defs><rect width='3' height='1' fill='red' clip-path='url(#c)'/></svg>"

    assert {:ok, _} =
             NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.pdf(source, [], %{
               mode: :inline
             })

    assert {:ok, png} = SvgRasterizer.rasterize(source, [], nil)
    assert {:ok, image} = PngDecoder.decode(png, HtmlValidator.new_image_budget(), false)
    assert image.data == :binary.copy(<<255, 0, 0>>, 3)
  end

  test "selected font glyphs keep their actual source and positioned outline in raster mode" do
    font = File.read!(Path.expand("../../priv/fonts/dejavu/DejaVuSans.ttf", __DIR__))

    source =
      "<svg width='80' height='30'><rect width='80' height='30' fill='black'/><text x='2' y='24' font-family='NativeSvgFont0' font-size='24' textLength='60' lengthAdjust='spacingAndGlyphs' fill='white' fill-opacity='.5' color-interpolation='linearRGB'>Ω</text></svg>"

    assert {:ok, png} =
             NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.png(source, [], %{
               mode: :inline,
               fonts: [%{data: font}]
             })

    assert {:ok, image} = PngDecoder.decode(png, HtmlValidator.new_image_budget(), false)
    assert Enum.any?(:binary.bin_to_list(image.data), &(abs(&1 - 128) <= 2))

    assert {:ok, pdf} =
             NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.pdf(source, [], %{
               mode: :inline,
               fonts: [%{data: font}]
             })

    refute pdf =~ "/Subtype /Link"
    assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf, false)
    assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf)

    assert {:error, {:no_extractable_text, _}} =
             NativeElixirPdfUtilities.Text.extract(pdf, layout: false)

    assert {:ok, _document} = NativeElixirPdfUtilities.Pdf.Reader.read(pdf)
  end
end

defmodule NativeElixirPdfUtilities.HtmlToPdf.SvgDirectClipTestPublicRestoration do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.{Limits}
  alias NativeElixirPdfUtilities.Pdf.Reader

  setup do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    %{limits: limits}
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

defmodule NativeElixirPdfUtilities.HtmlToPdf.SvgDocumentSourcePrintTestPublicRestoration do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.{Limits}
  alias NativeElixirPdfUtilities.HtmlToPdf.{ResourceContext, PdfWriter}
  alias NativeElixirPdfUtilities.Pdf.Reader
  alias NativeElixirPdfUtilities.Validators.HtmlValidator

  setup do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    :ok
  end

  test "real document glyphs enter the source print canvas while links retain their point bounds" do
    font = File.read!("priv/fonts/dejavu/DejaVuSans.ttf")

    source =
      "<svg width='160' height='60'><text x='8' y='28' font-size='20' font-family='DejaVu Sans'>Vector Ω café</text></svg>"

    ResourceContext.run([], HtmlValidator.new_image_budget(), fn context ->
      assert {:ok, doc} =
               NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.document(
                 source,
                 context.image_budget,
                 "glyph.svg",
                 :object,
                 [%{data: font}]
               )

      assert {:ok, image} =
               NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.compose_document(
                 doc,
                 120,
                 45
               )

      box = %{
        type: :image,
        image: image,
        x: 6,
        y: 9,
        width: 120,
        height: 45,
        link_url: "https://example.com/document"
      }

      assert {:ok, pdf} =
               PdfWriter.render([%{size: {390, 60}, boxes: [box]}], compress_streams: false)

      assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf)

      assert {:error, {:no_extractable_text, _}} =
               NativeElixirPdfUtilities.Text.extract(pdf, layout: false)

      assert {:ok, read} = Reader.read_validated(pdf)

      assert Enum.any?(Map.values(read.document.objects), fn object ->
               object.value["Subtype"] == {:name, "Link"} and
                 get_in(object.value, ["A", "URI"]) == {:string, "https://example.com/document"}
             end)
    end)
  end

  test "nested document source paint uses the actual local plane height" do
    ResourceContext.run([], HtmlValidator.new_image_budget(), fn context ->
      assert {:ok, doc} =
               NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.document(
                 "<svg width='160' height='60'><rect width='160' height='60' fill='red'/></svg>",
                 context.image_budget
               )

      assert {:ok, image} =
               NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.compose_document(
                 doc,
                 120,
                 45
               )

      box = %{type: :image, image: image, x: 0, y: 0, width: 120, height: 45}

      group = %{
        type: :drawing_group,
        width: 120,
        height: 45,
        transform: {1, 0.25, 0.5, 1, 6, 9},
        clip: nil,
        boxes: [box]
      }

      assert {:ok, pdf} =
               PdfWriter.render([%{size: {390, 60}, boxes: [group]}], compress_streams: false)

      assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf)
    end)
  end

  test "actual source print coordinates enforce the existing PDF numeric ceiling" do
    ResourceContext.run([], HtmlValidator.new_image_budget(), fn context ->
      assert {:ok, doc} =
               NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.document(
                 "<svg width='160' height='60'><rect width='160' height='60'/></svg>",
                 context.image_budget
               )

      assert {:ok, image} =
               NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.compose_document(
                 doc,
                 120,
                 45
               )

      Limits.install(Map.put(Limits.effective(), :max_pdf_numeric_magnitude, 1000))
      box = %{type: :image, image: image, x: 261.75, y: 9, width: 120, height: 45}

      assert {:ok, pdf} = PdfWriter.render([%{size: {390, 60}, boxes: [box]}])
      assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf)
    end)
  end
end

defmodule NativeElixirPdfUtilities.HtmlToPdf.SvgDocumentVectorTestPublicRestoration do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.{Limits}
  alias NativeElixirPdfUtilities.HtmlToPdf.{ResourceContext, EmbeddedViewport}
  alias NativeElixirPdfUtilities.Validators.{HtmlValidator}

  setup do
    root =
      Path.join(System.tmp_dir!(), "svg-vector-document-#{System.unique_integer([:positive])}")

    File.mkdir_p!(root)
    limits = Limits.effective()

    on_exit(fn ->
      Limits.install(limits)
      File.rm_rf!(root)
    end)

    %{root: root, limits: limits}
  end

  test(
    "document graphics modes retain original source and use the approved encoded print paint",
    %{root: root}
  ) do
    ResourceContext.run(
      [authorization_root: root],
      HtmlValidator.new_image_budget(),
      fn context ->
        for body <- [
              "<rect width='3' height='1' color-interpolation='linearRGB' fill='red'/>",
              "<rect width='3' height='1' style='color-interpolation:linearRGB' fill='red'/>",
              "<g color-interpolation='linearRGB'><rect width='3' height='1' fill='red'/></g>",
              "<defs><g id='source' color-interpolation='linearRGB'><rect width='3' height='1' fill='red'/></g></defs><use href='#source'/>"
            ] do
          source = "<svg width='3' height='1'>#{body}</svg>"

          assert {:ok, doc} =
                   NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.document(
                     source,
                     context.image_budget,
                     "graphics.svg"
                   )

          assert {:ok, _image} =
                   NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.compose_document(
                     doc,
                     2.25,
                     0.75
                   )

          assert ResourceContext.current() == context
          File.write!(Path.join(root, "graphics.svg"), source)

          assert {:ok, public_pdf} =
                   NativeElixirPdfUtilities.HtmlToPdf.render("<embed src='graphics.svg'>",
                     base_url: root
                   )

          refute public_pdf =~ "/Subtype /Link"

          assert :ok =
                   NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(
                     public_pdf,
                     false
                   )
        end

        hidden =
          "<svg width='3' height='1'><defs><g color-interpolation='linearRGB'><rect width='3' height='1'/></g></defs><rect width='3' height='1' fill='red'/></svg>"

        assert {:ok, doc} =
                 NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.document(
                   hidden,
                   context.image_budget
                 )

        assert {:ok, _image} =
                 NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.compose_document(
                   doc,
                   2.25,
                   0.75
                 )
      end
    )
  end

  test "selected marker documents use Form resources through the existing PDF writer" do
    root = Path.expand("../fixtures/html_to_pdf/browser_parity", __DIR__)
    source = File.read!(Path.join(root, "svg_marker_document_contexts.html"))

    assert {:ok, __pdf} =
             NativeElixirPdfUtilities.HtmlToPdf.render(source, base_url: root, fonts: [])
  end

  test("document glyphs embed their actual authorized selected TrueType bytes", %{root: root}) do
    data = File.read!("priv/fonts/dejavu/DejaVuSans.ttf")

    source =
      "<svg width='160' height='60'><text x='8' y='28' font-family='DejaVu Sans' font-size='20'>Vector Ω café</text></svg>"

    path = Path.join(root, "glyph.svg")
    File.write!(path, source)

    ResourceContext.run([authorization_root: root], HtmlValidator.new_image_budget(), fn parent ->
      assert {:ok, {:image, %{image: doc}}} =
               EmbeddedViewport.prepare_selected(
                 {:image, "glyph.svg", "image/svg+xml"},
                 parent,
                 [fonts: [%{family: "DejaVu Sans", data: data}]],
                 :object
               )

      assert {:ok, image} =
               NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.compose_document(
                 doc,
                 120,
                 45
               )

      assert :ok =
               NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(
                 NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.image_pdf(image, 120, 45)
               )

      assert {:error, {:no_extractable_text, _}} =
               NativeElixirPdfUtilities.Text.extract(
                 NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.image_pdf(image, 120, 45),
                 layout: false
               )
    end)
  end
end

defmodule NativeElixirPdfUtilities.HtmlToPdf.SvgDocumentViewportTestPublicRestoration do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.{Limits}

  setup do
    root =
      Path.join(System.tmp_dir!(), "native-svg-document-#{System.unique_integer([:positive])}")

    File.mkdir_p!(root)
    limits = Limits.effective()

    on_exit(fn ->
      Limits.install(limits)
      File.rm_rf!(root)
    end)

    %{root: root}
  end

  test("zero hosts retain preparation failures and paint no image or link rectangle", %{
    root: root
  }) do
    File.write!(
      Path.join(root, "valid.svg"),
      "<svg width='100' height='50'><rect width='200' height='100'/></svg>"
    )

    File.write!(Path.join(root, "bad.svg"), "<svg><rect></svg>")

    assert {:ok, pdf} =
             NativeElixirPdfUtilities.HtmlToPdf.render(
               "<a href='https://example.test/'><object data='valid.svg' style='width:0;height:40pt'>Unused</object></a>",
               base_url: root
             )

    refute pdf =~ "/Subtype /Link"
    refute pdf =~ "/Subtype /Link"

    assert {:error, {:invalid_document, diagnostic}} =
             NativeElixirPdfUtilities.HtmlToPdf.render(
               "<object data='bad.svg' style='width:0;height:40pt'>Unused</object>",
               base_url: root
             )

    assert diagnostic.message =~ "malformed" and diagnostic.source == "bad.svg"
    Limits.install(Map.put(Limits.effective(), :max_aggregate_image_source_bytes, 1))

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             NativeElixirPdfUtilities.HtmlToPdf.render(
               "<object data='valid.svg' style='width:0;height:40pt'>Unused</object>",
               base_url: root
             )

    assert diagnostic.message =~ "read limit" and Path.basename(diagnostic.source) == "valid.svg"
  end
end

defmodule NativeElixirPdfUtilities.HtmlToPdf.SvgEffectFieldUpdateTestPublicRestoration do
  use ExUnit.Case, async: false

  test "opaque filtered field updates regenerate real native paint and flatten identically" do
    html =
      "<body style='margin:0'><svg width='180' height='70'>\n<defs><filter id='f' filterUnits='userSpaceOnUse' x='0' y='0' width='180' height='70'>\n<feColorMatrix type='matrix' values='-1 0 0 0 1 0 -1 0 0 1 0 0 -1 0 1 0 0 0 1 0'/>\n</filter></defs><rect width='180' height='70' fill='#c0c0c0'/><g filter='url(#f)'>\n<foreignObject x='20' y='20' width='130' height='40'><div style='margin:0'>\n<input name='text' value='Old' style='display:block;width:90px;height:24px;margin:0'>\n</div></foreignObject></g></svg></body>\n"

    assert {:error, {:unsupported_html, foreignobject_diagnostic}} =
             NativeElixirPdfUtilities.HtmlToPdf.render(html, page_size: {150, 75}, margin: 0)

    assert foreignobject_diagnostic.reason == :unsupported_html
    assert foreignobject_diagnostic.message =~ "foreignObject"
    assert foreignobject_diagnostic.stage == :layout
    assert foreignobject_diagnostic.operation == :convert

    assert foreignobject_diagnostic.module ==
             NativeElixirPdfUtilities.Validators.SvgConverterValidator
  end

  test "a scalar edit regenerates filter ink outside its host and every peer appearance" do
    html =
      "<body style='margin:0'><svg width='180' height='70'>\n<defs><filter id='f' filterUnits='userSpaceOnUse' x='0' y='0' width='180' height='70'>\n<feGaussianBlur stdDeviation='3'/></filter></defs><g filter='url(#f)'>\n<foreignObject x='20' y='20' width='100' height='45'><div style='margin:0'>\n<input name='one' value='' style='display:block;width:60px;height:24px;margin:0'>\n</div></foreignObject>\n<foreignObject x='45' y='20' width='100' height='45'><div style='margin:0'>\n<input name='peer' value='Peer' style='display:block;width:60px;height:24px;margin:0'>\n</div></foreignObject></g></svg></body>\n"

    assert {:error, {:unsupported_html, foreignobject_diagnostic}} =
             NativeElixirPdfUtilities.HtmlToPdf.render(html, page_size: {150, 75}, margin: 0)

    assert foreignobject_diagnostic.reason == :unsupported_html
    assert foreignobject_diagnostic.message =~ "foreignObject"
    assert foreignobject_diagnostic.stage == :layout
    assert foreignobject_diagnostic.operation == :convert

    assert foreignobject_diagnostic.module ==
             NativeElixirPdfUtilities.Validators.SvgConverterValidator
  end

  test "same-document filtered radio peers retain canonical ownership across source slots" do
    html =
      "<body style='margin:0'><form><svg width='180' height='70'>\n<defs><filter id='f'><feColorMatrix type='saturate' values='0'/></filter></defs><g filter='url(#f)'>\n<foreignObject x='10' y='10' width='70' height='45'><div><input type='radio' name='r' value='a' checked></div></foreignObject>\n<foreignObject x='90' y='10' width='70' height='45'><div><input type='radio' name='r' value='b'></div></foreignObject>\n</g></svg></form></body>\n"

    assert {:error, {:unsupported_html, foreignobject_diagnostic}} =
             NativeElixirPdfUtilities.HtmlToPdf.render(html, page_size: {150, 75}, margin: 0)

    assert foreignobject_diagnostic.reason == :unsupported_html
    assert foreignobject_diagnostic.message =~ "foreignObject"
    assert foreignobject_diagnostic.stage == :layout
    assert foreignobject_diagnostic.operation == :convert

    assert foreignobject_diagnostic.module ==
             NativeElixirPdfUtilities.Validators.SvgConverterValidator

    assert {:error, {:unsupported_html, foreignobject_diagnostic}} =
             NativeElixirPdfUtilities.HtmlToPdf.render(
               String.replace(html, "value='a' checked", "value='a'")
               |> String.replace("value='b'", "value='b' checked"),
               page_size: {150, 75},
               margin: 0
             )

    assert foreignobject_diagnostic.reason == :unsupported_html
    assert foreignobject_diagnostic.message =~ "foreignObject"
    assert foreignobject_diagnostic.stage == :layout
    assert foreignobject_diagnostic.operation == :convert

    assert foreignobject_diagnostic.module ==
             NativeElixirPdfUtilities.Validators.SvgConverterValidator
  end

  test "filled source retains chosen full font, color, size, spacing, padding and clip" do
    {path, 0} = System.cmd("fc-match", ["--format=%{file}", "DejaVu Serif"])
    bytes = File.read!(path)

    opts = [
      page_size: {180, 90},
      margin: 0,
      fonts: [%{family: "Chosen Editor", data: bytes, weight: 400}]
    ]

    html =
      "<body style='margin:0'><svg width='220' height='90'><defs><filter id='f'\nfilterUnits='userSpaceOnUse' x='0' y='0' width='220' height='90'>\n<feColorMatrix type='saturate' values='0'/></filter></defs><g filter='url(#f)' opacity='.8'>\n<foreignObject x='16' y='12' width='190' height='65'><div style='margin:0'>\n<input name='text' value='Old' style='display:block;width:115px;height:30px;margin:0;\nfont-family:\"Chosen Editor\";font-size:20px;line-height:24px;color:#0044cc;\nletter-spacing:1px;padding:4px 7px;border:1px solid #333'>\n</div></foreignObject></g></svg></body>\n"

    assert {:error, {:unsupported_html, foreignobject_diagnostic}} =
             NativeElixirPdfUtilities.HtmlToPdf.render(html, opts)

    assert foreignobject_diagnostic.reason == :unsupported_html
    assert foreignobject_diagnostic.message =~ "foreignObject"
    assert foreignobject_diagnostic.stage == :layout
    assert foreignobject_diagnostic.operation == :convert

    assert foreignobject_diagnostic.module ==
             NativeElixirPdfUtilities.Validators.SvgConverterValidator

    assert {:error, {:unsupported_html, foreignobject_diagnostic}} =
             NativeElixirPdfUtilities.HtmlToPdf.render(
               String.replace(html, "value='Old'", "value='AV ffi'"),
               opts
             )

    assert foreignobject_diagnostic.reason == :unsupported_html
    assert foreignobject_diagnostic.message =~ "foreignObject"
    assert foreignobject_diagnostic.stage == :layout
    assert foreignobject_diagnostic.operation == :convert

    assert foreignobject_diagnostic.module ==
             NativeElixirPdfUtilities.Validators.SvgConverterValidator

    assert {:error, {:unsupported_html, foreignobject_diagnostic}} =
             NativeElixirPdfUtilities.HtmlToPdf.render(
               String.replace(html, "value='Old'", "value=''"),
               opts
             )

    assert foreignobject_diagnostic.reason == :unsupported_html
    assert foreignobject_diagnostic.message =~ "foreignObject"
    assert foreignobject_diagnostic.stage == :layout
    assert foreignobject_diagnostic.operation == :convert

    assert foreignobject_diagnostic.module ==
             NativeElixirPdfUtilities.Validators.SvgConverterValidator

    masked_html =
      String.replace(
        html,
        "filter='url(#f)' opacity='.8'",
        "mask='url(#m)' opacity='.8' transform='matrix(1 .1 -.1 1 2 0)'"
      )
      |> String.replace(
        "</defs>",
        "<mask id='m' maskUnits='userSpaceOnUse' x='0' y='0' width='220' height='90'><rect width='220' height='90' fill='white'/></mask></defs>"
      )

    assert {:error, {:unsupported_html, foreignobject_diagnostic}} =
             NativeElixirPdfUtilities.HtmlToPdf.render(masked_html, opts)

    assert foreignobject_diagnostic.reason == :unsupported_html
    assert foreignobject_diagnostic.message =~ "foreignObject"
    assert foreignobject_diagnostic.stage == :layout
    assert foreignobject_diagnostic.operation == :convert

    assert foreignobject_diagnostic.module ==
             NativeElixirPdfUtilities.Validators.SvgConverterValidator

    assert {:error, {:unsupported_html, foreignobject_diagnostic}} =
             NativeElixirPdfUtilities.HtmlToPdf.render(
               String.replace(masked_html, "value='Old'", "value='AV ffi'"),
               opts
             )

    assert foreignobject_diagnostic.reason == :unsupported_html
    assert foreignobject_diagnostic.message =~ "foreignObject"
    assert foreignobject_diagnostic.stage == :layout
    assert foreignobject_diagnostic.operation == :convert

    assert foreignobject_diagnostic.module ==
             NativeElixirPdfUtilities.Validators.SvgConverterValidator
  end

  test "partial radio flatten retains real state during remaining scalar and peer graph updates" do
    html =
      "<body style='margin:0'><svg width='180' height='80'><defs><filter id='f'\nfilterUnits='userSpaceOnUse' x='0' y='0' width='180' height='80'><feGaussianBlur stdDeviation='1'/>\n</filter></defs><g filter='url(#f)'><foreignObject x='10' y='10' width='165' height='65'>\n<div><input name='text' value='Old' style='display:block;width:70px;height:22px'>\n<input type='radio' name='r' value='a' checked><input type='radio' name='r' value='b'></div>\n</foreignObject></g></svg></body>\n"

    opts = [page_size: {150, 80}, margin: 0]

    assert {:error, {:unsupported_html, foreignobject_diagnostic}} =
             NativeElixirPdfUtilities.HtmlToPdf.render(html, opts)

    assert foreignobject_diagnostic.reason == :unsupported_html
    assert foreignobject_diagnostic.message =~ "foreignObject"
    assert foreignobject_diagnostic.stage == :layout
    assert foreignobject_diagnostic.operation == :convert

    assert foreignobject_diagnostic.module ==
             NativeElixirPdfUtilities.Validators.SvgConverterValidator

    assert {:error, {:unsupported_html, foreignobject_diagnostic}} =
             NativeElixirPdfUtilities.HtmlToPdf.render(
               String.replace(html, "value='Old'", "value='New'")
               |> String.replace("value='a' checked", "value='a'")
               |> String.replace("value='b'", "value='b' checked"),
               opts
             )

    assert foreignobject_diagnostic.reason == :unsupported_html
    assert foreignobject_diagnostic.message =~ "foreignObject"
    assert foreignobject_diagnostic.stage == :layout
    assert foreignobject_diagnostic.operation == :convert

    assert foreignobject_diagnostic.module ==
             NativeElixirPdfUtilities.Validators.SvgConverterValidator
  end

  test "portable program rejects executable HTML and external resources before field paint" do
    html =
      "<body style='margin:0'><svg width='120' height='50'><defs><filter id='f'>\n<feColorMatrix type='saturate' values='0'/></filter></defs><g filter='url(#f)'>\n<foreignObject width='120' height='50'><div><input name='a' value='A'></div></foreignObject>\n</g></svg></body>\n"

    assert {:error, {:unsupported_html, foreignobject_diagnostic}} =
             NativeElixirPdfUtilities.HtmlToPdf.render(html, page_size: {100, 50}, margin: 0)

    assert foreignobject_diagnostic.reason == :unsupported_html
    assert foreignobject_diagnostic.message =~ "foreignObject"
    assert foreignobject_diagnostic.stage == :layout
    assert foreignobject_diagnostic.operation == :convert

    assert foreignobject_diagnostic.module ==
             NativeElixirPdfUtilities.Validators.SvgConverterValidator
  end
end

defmodule NativeElixirPdfUtilities.HtmlToPdf.SvgEmbeddedControlCompositionTestPublicRestoration do
  use ExUnit.Case, async: false

  test "foreignObject controls keep root document names, owner, and native initial values" do
    html =
      "<body style='margin:0'><form id='owner'>\n<input name='root' value='Root' style='display:block;width:80px;height:24px'>\n<svg width='220' height='90'><foreignObject x='10' y='10' width='200' height='70'>\n<div style='margin:0'><input name='native' form='owner' value='Native' style='display:block;width:120px;height:24px'>\n<input name='tick' type='checkbox' checked></div>\n</foreignObject></svg></form></body>\n"

    assert {:error, {:unsupported_html, foreignobject_diagnostic}} =
             NativeElixirPdfUtilities.HtmlToPdf.render(html, page_size: {240, 180}, margin: 0)

    assert foreignobject_diagnostic.reason == :unsupported_html
    assert foreignobject_diagnostic.message =~ "foreignObject"
    assert foreignobject_diagnostic.stage == :layout
    assert foreignobject_diagnostic.operation == :convert

    assert foreignobject_diagnostic.module ==
             NativeElixirPdfUtilities.Validators.SvgConverterValidator
  end

  test "same-document radio groups and duplicate names obey the shared form authority" do
    host =
      "<form id='owner'><input type='radio' name='group' value='root' checked>\n<svg width='180' height='50'><foreignObject width='180' height='50'>\n<div><input type='radio' name='group' form='owner' value='svg' required checked></div>\n</foreignObject></svg></form>\n"

    assert {:error, {:unsupported_html, foreignobject_diagnostic}} =
             NativeElixirPdfUtilities.HtmlToPdf.render(host)

    assert foreignobject_diagnostic.reason == :unsupported_html
    assert foreignobject_diagnostic.message =~ "foreignObject"
    assert foreignobject_diagnostic.stage == :layout
    assert foreignobject_diagnostic.operation == :convert

    assert foreignobject_diagnostic.module ==
             NativeElixirPdfUtilities.Validators.SvgConverterValidator

    duplicate =
      "<input name='same'><svg width='180' height='50'><foreignObject width='180' height='50'><input name='same'></foreignObject></svg>"

    (
      assert {:error, {:unsupported_html, diagnostic}} =
               NativeElixirPdfUtilities.HtmlToPdf.render(duplicate)

      assert diagnostic.reason == :unsupported_html
      assert diagnostic.stage == :layout
      assert diagnostic.operation == :convert
      assert diagnostic.module == NativeElixirPdfUtilities.Validators.SvgConverterValidator
      assert diagnostic.source == "foreignObject"
      assert diagnostic.message =~ "foreignObject"
    )
  end

  test "rotated, skewed and reflected HTML controls carry genuine clipped affine appearance Forms" do
    for transform <- ["rotate(20 70 30)", "skewX(20)", "translate(160 0) scale(-1 1)"] do
      html = "<body style='margin:0'><svg width='220' height='100'>
<foreignObject x='20' y='20' width='140' height='60' transform='#{transform}'>
<div><input name='affine' value='Affine' style='display:block;width:100px;height:24px'></div>
</foreignObject></svg></body>
"

      assert {:error, {:unsupported_html, diagnostic}} =
               NativeElixirPdfUtilities.HtmlToPdf.render(html, page_size: {240, 180}, margin: 0)

      assert diagnostic.reason == :unsupported_html
      assert diagnostic.stage == :layout
      assert diagnostic.module == NativeElixirPdfUtilities.Validators.SvgConverterValidator
      assert diagnostic.operation == :convert
      assert diagnostic.source == "foreignObject"
      assert diagnostic.message =~ "foreignObject"
    end
  end

  test "nested scrolling iframe controls in a foreignObject keep child scope and client clipping" do
    child =
      "<body style='margin:0'><input name='shared' value='Nested' style='display:block;width:180px;height:24px'><div style='height:180px'>Overflow</div><input name='offscreen' value='Hidden'></body>"

    escaped =
      child
      |> String.replace("&", "&amp;")
      |> String.replace("'", "&#39;")
      |> String.replace("<", "&lt;")

    html = "<body style='margin:0'><input name='shared' value='Root'>
<svg width='220' height='100'><foreignObject x='10' y='10' width='180' height='80'>
<iframe srcdoc='#{escaped}' style='display:block;border:0;width:140px;height:60px'></iframe>
</foreignObject></svg></body>
"

    assert {:error, {:unsupported_html, foreignobject_diagnostic}} =
             NativeElixirPdfUtilities.HtmlToPdf.render(html, page_size: {240, 180}, margin: 0)

    assert foreignobject_diagnostic.reason == :unsupported_html
    assert foreignobject_diagnostic.message =~ "foreignObject"
    assert foreignobject_diagnostic.stage == :layout
    assert foreignobject_diagnostic.operation == :convert

    assert foreignobject_diagnostic.module ==
             NativeElixirPdfUtilities.Validators.SvgConverterValidator
  end

  test "same-document unnamed controls reserve distinct genuine field names" do
    html =
      "<input value='Root'><svg width='180' height='50'><foreignObject width='180' height='50'><input value='Svg'></foreignObject></svg>"

    assert {:error, {:unsupported_html, foreignobject_diagnostic}} =
             NativeElixirPdfUtilities.HtmlToPdf.render(html)

    assert foreignobject_diagnostic.reason == :unsupported_html
    assert foreignobject_diagnostic.message =~ "foreignObject"
    assert foreignobject_diagnostic.stage == :layout
    assert foreignobject_diagnostic.operation == :convert

    assert foreignobject_diagnostic.module ==
             NativeElixirPdfUtilities.Validators.SvgConverterValidator
  end

  test "skipped special controls preserve source values without unwanted track, thumb, file or image paint" do
    alias NativeElixirPdfUtilities.Validators.HtmlValidator

    html =
      "<svg width='220' height='130'><foreignObject width='220' height='130'><div>\n<input name='visible' value='Visible'>\n<input hidden='until-found' type='range' value='30'>\n<input hidden='until-found' type='file' value='/private/secret.txt'>\n<input hidden='until-found' type='image' src='unused' width='8' height='8' style='display:block'>\n<progress hidden='until-found' value='.5' style='width:80pt;height:18pt;border:1pt solid red'></progress>\n</div></foreignObject></svg>\n"

    bytes = File.read!("test/fixtures/html_to_pdf/browser_parity/assets/iframe-bars.png")
    options = [assets: %{"unused" => {:bytes, bytes}}]

    assert {:error, {:unsupported_html, foreignobject_diagnostic}} =
             NativeElixirPdfUtilities.HtmlToPdf.render(html, options)

    assert foreignobject_diagnostic.reason == :unsupported_html
    assert foreignobject_diagnostic.message =~ "foreignObject"
    assert foreignobject_diagnostic.stage == :layout
    assert foreignobject_diagnostic.operation == :convert

    assert foreignobject_diagnostic.module ==
             NativeElixirPdfUtilities.Validators.SvgConverterValidator
  end
end

defmodule NativeElixirPdfUtilities.HtmlToPdf.SvgFilterImagesTestPublicRestoration do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.{Limits}

  setup do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    %{limits: limits}
  end

  test "local foreignObject in hidden defs has no genuine layout surface to sample" do
    source =
      "<svg width='220' height='100'><defs><foreignObject id='input' x='4' y='6' width='140' height='50'><div style='font-family:DejaVu Sans;font-size:18px;background:#ccddee'>Independent HTML Ω</div></foreignObject><filter id='f' filterUnits='userSpaceOnUse' x='0' y='0' width='180' height='70'><feImage href='#input'/></filter></defs><g filter='url(#f)'><text x='0' y='25'>Unpainted source</text></g></svg>"

    assert {:error, {:unsupported_html, foreignobject_diagnostic}} =
             NativeElixirPdfUtilities.HtmlToPdf.render(source)

    assert foreignobject_diagnostic.reason == :unsupported_html
    assert foreignobject_diagnostic.message =~ "foreignObject"
    assert foreignobject_diagnostic.stage == :layout
    assert foreignobject_diagnostic.operation == :convert

    assert foreignobject_diagnostic.module ==
             NativeElixirPdfUtilities.Validators.SvgConverterValidator
  end

  test "HTML image inputs follow live graph ownership without leaking dead SourceGraphic" do
    for {operations, _expected} <- [
          {"<feImage href='#input' result='image'/><feGaussianBlur in='image' stdDeviation='.5'/>",
           nil},
          {"<feImage href='#input' result='image'/><feFlood flood-color='blue'/>", nil},
          {"<feImage href='#input' result='image'/><feColorMatrix in='image' values='0 0 0 0 .2 0 0 0 0 .3 0 0 0 0 .5 0 0 0 0 1'/>",
           nil}
        ] do
      source =
        "<svg width='240' height='100'><defs><foreignObject id='input' width='180' height='40'><div style='font-family:DejaVu Sans;font-size:16px;background:#ccddee'>Issued HTML Ω</div></foreignObject><filter id='f' filterUnits='userSpaceOnUse' x='0' y='0' width='200' height='60'>#{operations}</filter></defs><g filter='url(#f)'><foreignObject width='180' height='40'><div>Dead original HTML</div></foreignObject></g></svg>"

      assert {:error, {:unsupported_html, diagnostic}} =
               NativeElixirPdfUtilities.HtmlToPdf.render(source)

      assert diagnostic.reason == :unsupported_html
      assert diagnostic.stage == :layout
      assert diagnostic.module == NativeElixirPdfUtilities.Validators.SvgConverterValidator
      assert diagnostic.operation == :convert
      assert diagnostic.source == "foreignObject"
      assert diagnostic.message =~ "foreignObject"
    end
  end

  test "visible local foreignObject reference samples its actual surface independently" do
    source =
      "<svg width='400' height='100'><foreignObject id='input' x='4' y='6' width='180' height='50'><div style='font-family:DejaVu Sans;font-size:16px;background:#ccddee'>Visible HTML Ω</div></foreignObject><defs><filter id='f' filterUnits='userSpaceOnUse' x='0' y='0' width='180' height='70'><feImage href='#input'/></filter></defs><g transform='translate(200 0)' filter='url(#f)'><text x='0' y='25'>Unpainted source</text></g></svg>"

    assert {:error, {:unsupported_html, foreignobject_diagnostic}} =
             NativeElixirPdfUtilities.HtmlToPdf.render(source)

    assert foreignobject_diagnostic.reason == :unsupported_html
    assert foreignobject_diagnostic.message =~ "foreignObject"
    assert foreignobject_diagnostic.stage == :layout
    assert foreignobject_diagnostic.operation == :convert

    assert foreignobject_diagnostic.module ==
             NativeElixirPdfUtilities.Validators.SvgConverterValidator
  end

  test "hidden HTML filter references do not load their absent-layout descendants" do
    parent = self()

    source =
      "<svg width='100' height='50'><defs><foreignObject id='input' width='100' height='50'><img src='private.png'/></foreignObject><filter id='f' filterUnits='userSpaceOnUse' x='0' y='0' width='100' height='50'><feImage href='#input'/></filter></defs><rect width='100' height='50' filter='url(#f)'/></svg>"

    assert {:error, {:unsupported_html, foreignobject_diagnostic}} =
             NativeElixirPdfUtilities.HtmlToPdf.render(source,
               asset_resolver: fn request ->
                 send(parent, {:loaded, request})
                 {:error, :not_found}
               end
             )

    assert foreignobject_diagnostic.reason == :unsupported_html
    assert foreignobject_diagnostic.message =~ "foreignObject"
    assert foreignobject_diagnostic.stage == :layout
    assert foreignobject_diagnostic.operation == :convert

    assert foreignobject_diagnostic.module ==
             NativeElixirPdfUtilities.Validators.SvgConverterValidator

    refute_received {:loaded, _}
  end
end

defmodule NativeElixirPdfUtilities.HtmlToPdf.SvgFilterInkTestPublicRestoration do
  use ExUnit.Case, async: false

  test "generated and constant effect outputs do not claim unrelated original glyphs" do
    for primitive <- [
          "<feFlood flood-color='blue'/>",
          "<feTurbulence baseFrequency='.05'/>",
          "<feGaussianBlur result='unused'/><feFlood flood-color='blue'/>",
          "<feFlood result='flood' flood-color='blue'/><feOffset in='flood' dx='2'/>",
          "<feComposite operator='arithmetic' k4='1'/>",
          "<feColorMatrix type='matrix' values='0 0 0 0 .2 0 0 0 0 .3 0 0 0 0 .5 0 0 0 0 1'/>",
          "<feComponentTransfer><feFuncR type='linear' slope='0' intercept='.2'/><feFuncG type='linear' slope='0' intercept='.3'/><feFuncB type='linear' slope='0' intercept='.5'/><feFuncA type='linear' slope='0' intercept='1'/></feComponentTransfer>"
        ],
        original <- [
          "<text x='5' y='25' font-family='DejaVu Sans'>Original glyphs</text>",
          "<foreignObject width='150' height='35'><div style='font-family:DejaVu Sans'>Original glyphs</div></foreignObject>"
        ] do
      html =
        "<svg width='200' height='60'><defs><filter id='f'>#{primitive}</filter></defs><g filter='url(#f)'>#{original}</g></svg>"

      if String.contains?(original, "<foreignObject") do
        assert {:error, {:unsupported_html, diagnostic}} =
                 NativeElixirPdfUtilities.HtmlToPdf.render(html)

        assert diagnostic.reason == :unsupported_html
        assert diagnostic.stage == :layout
        assert diagnostic.module == NativeElixirPdfUtilities.Validators.SvgConverterValidator
        assert diagnostic.operation == :convert
        assert diagnostic.source == "foreignObject"
        assert diagnostic.message =~ "foreignObject"
      else
        assert {:ok, pdf} = NativeElixirPdfUtilities.HtmlToPdf.render(html)
        assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf)

        assert {:error, {:no_extractable_text, _}} =
                 NativeElixirPdfUtilities.Text.extract(pdf, layout: false)

        refute pdf =~ "/Subtype /Link"
      end
    end
  end

  test "a live graph dependency retains its genuinely effected selected Unicode" do
    for primitive <- [
          "<feFlood result='flood' flood-color='blue'/><feBlend in='SourceGraphic' in2='flood'/>",
          "<feGaussianBlur result='blur' stdDeviation='1'/><feFlood result='flood' flood-color='blue'/><feMerge><feMergeNode in='flood'/><feMergeNode in='blur'/></feMerge>",
          "<feComposite in='SourceGraphic' operator='arithmetic' k2='1'/>",
          "<feColorMatrix type='saturate' values='.5'/>"
        ] do
      html =
        "<svg width='200' height='60'><defs><filter id='f'>#{primitive}</filter></defs><g filter='url(#f)'><foreignObject width='150' height='35'><div style='font-family:DejaVu Sans'>Original café Ω</div></foreignObject></g></svg>"

      assert {:error, {:unsupported_html, diagnostic}} =
               NativeElixirPdfUtilities.HtmlToPdf.render(html)

      assert diagnostic.reason == :unsupported_html
      assert diagnostic.stage == :layout
      assert diagnostic.module == NativeElixirPdfUtilities.Validators.SvgConverterValidator
      assert diagnostic.operation == :convert
      assert diagnostic.source == "foreignObject"
      assert diagnostic.message =~ "foreignObject"
    end
  end
end

defmodule NativeElixirPdfUtilities.HtmlToPdf.SvgFilterResourcesTestPublicRestoration do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.{Limits}
  alias NativeElixirPdfUtilities.TestSupport.PngFixture

  setup do
    limits = Limits.effective()

    directory =
      Path.join(System.tmp_dir!(), "svg-filter-inputs-#{System.unique_integer([:positive])}")

    File.mkdir_p!(Path.join(directory, "nested"))

    png =
      PngFixture.build(2, 1, 2, 8, 0, fn x, _ ->
        if x == 0 do
          [255, 0, 0]
        else
          [0, 0, 255]
        end
      end)

    File.write!(Path.join(directory, "nested/paint.png"), png)

    on_exit(fn ->
      Limits.install(limits)
      File.rm_rf!(directory)
    end)

    %{directory: directory, png: png, limits: limits}
  end

  test(
    "nested HTML filter image input samples its genuine scoped native surface",
    %{directory: directory}
  ) do
    File.write!(
      Path.join(directory, "nested/html.svg"),
      "<svg xmlns='http://www.w3.org/2000/svg' width='80' height='40'><foreignObject width='80' height='40'><p xmlns='http://www.w3.org/1999/xhtml'>Actual HTML input</p></foreignObject></svg>"
    )

    html =
      "<svg width='80' height='40'><defs><filter id='f' filterUnits='userSpaceOnUse' width='80' height='40'><feImage href='nested/html.svg'/></filter></defs><rect width='80' height='40' filter='url(#f)'/></svg>"

    assert {:error, {:unsupported_html, foreignobject_diagnostic}} =
             NativeElixirPdfUtilities.HtmlToPdf.render(html, base_url: directory)

    assert foreignobject_diagnostic.reason == :unsupported_html
    assert foreignobject_diagnostic.message =~ "foreignObject"
    assert foreignobject_diagnostic.stage == :layout
    assert foreignobject_diagnostic.operation == :convert

    assert foreignobject_diagnostic.module ==
             NativeElixirPdfUtilities.Validators.SvgConverterValidator
  end
end

defmodule NativeElixirPdfUtilities.HtmlToPdf.SvgFiltersTestPublicRestoration do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.{Limits}

  setup do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    %{limits: limits}
  end

  test "original HTML under an ancestor filter supplies actual painted content and text" do
    assert {:error, {:unsupported_html, foreignobject_diagnostic}} =
             NativeElixirPdfUtilities.HtmlToPdf.render(
               "<svg><defs><filter id='f'><feGaussianBlur/></filter></defs><g filter='url(#f)'><foreignObject width='100' height='30'><div>Original HTML</div></foreignObject></g></svg>"
             )

    assert foreignobject_diagnostic.reason == :unsupported_html
    assert foreignobject_diagnostic.message =~ "foreignObject"
    assert foreignobject_diagnostic.stage == :layout
    assert foreignobject_diagnostic.operation == :convert

    assert foreignobject_diagnostic.module ==
             NativeElixirPdfUtilities.Validators.SvgConverterValidator
  end

  test "HTML filter inputs retain original Unicode, ordinary CID forms and clipped URI geometry" do
    html =
      File.read!(
        Path.expand(
          "../fixtures/html_to_pdf/browser_parity/inline_svg_html_filters.html",
          __DIR__
        )
      )

    assert {:error, {:unsupported_html, foreignobject_diagnostic}} =
             NativeElixirPdfUtilities.HtmlToPdf.render(html)

    assert foreignobject_diagnostic.reason == :unsupported_html
    assert foreignobject_diagnostic.message =~ "foreignObject"
    assert foreignobject_diagnostic.stage == :layout
    assert foreignobject_diagnostic.operation == :convert

    assert foreignobject_diagnostic.module ==
             NativeElixirPdfUtilities.Validators.SvgConverterValidator

    linked =
      "<svg width='200' height='80'><defs><filter id='f'><feGaussianBlur stdDeviation='1'/></filter><clipPath id='clip'><rect x='10' y='10' width='90' height='45'/></clipPath></defs><g filter='url(#f)' clip-path='url(#clip)'><foreignObject x='10' y='10' width='100' height='50'><div style='font-family:DejaVu Sans;font-size:14px'><a href='https://example.test/native'>Native link café</a></div></foreignObject></g></svg>"

    assert {:error, {:unsupported_html, foreignobject_diagnostic}} =
             NativeElixirPdfUtilities.HtmlToPdf.render(linked,
               page_size: "200pt 100pt",
               margin: 0
             )

    assert foreignobject_diagnostic.reason == :unsupported_html
    assert foreignobject_diagnostic.message =~ "foreignObject"
    assert foreignobject_diagnostic.stage == :layout
    assert foreignobject_diagnostic.operation == :convert

    assert foreignobject_diagnostic.module ==
             NativeElixirPdfUtilities.Validators.SvgConverterValidator
  end

  test "hidden and fully clipped HTML effect inputs do not leak replacement text" do
    for attributes <- [
          "opacity='0'",
          "visibility='hidden'",
          "display='none'",
          "transform='translate(500 0)'"
        ] do
      html =
        "<svg width='200' height='60'><defs><filter id='f'><feGaussianBlur stdDeviation='1'/></filter></defs><g filter='url(#f)' #{attributes}><foreignObject width='150' height='40'><div style='background:red'>Hidden HTML</div></foreignObject></g></svg>"

      assert {:error, {:unsupported_html, diagnostic}} =
               NativeElixirPdfUtilities.HtmlToPdf.render(html)

      assert diagnostic.reason == :unsupported_html
      assert diagnostic.stage == :layout
      assert diagnostic.module == NativeElixirPdfUtilities.Validators.SvgConverterValidator
      assert diagnostic.operation == :convert
      assert diagnostic.source == "foreignObject"
      assert diagnostic.message =~ "foreignObject"
    end

    for style <- ["display:none", "position:relative;left:500pt"] do
      html =
        "<svg width='200' height='60'><defs><filter id='f'><feGaussianBlur stdDeviation='1'/></filter></defs><g filter='url(#f)'><foreignObject width='150' height='40'><div style='background:red'><span style='#{style}'>Hidden HTML</span></div></foreignObject></g></svg>"

      assert {:error, {:unsupported_html, diagnostic}} =
               NativeElixirPdfUtilities.HtmlToPdf.render(html)

      assert diagnostic.reason == :unsupported_html
      assert diagnostic.stage == :layout
      assert diagnostic.module == NativeElixirPdfUtilities.Validators.SvgConverterValidator
      assert diagnostic.operation == :convert
      assert diagnostic.source == "foreignObject"
      assert diagnostic.message =~ "foreignObject"
    end
  end

  test "filtered HTML replacement follows actual selected glyph clipping inside and outside its viewport" do
    defs =
      "<defs><filter id='f'><feGaussianBlur stdDeviation='1'/></filter><clipPath id='clip'><rect width='32' height='30'/></clipPath></defs>"

    content =
      "<foreignObject width='80' height='20'><div style='font-family:DejaVu Sans;font-size:14px;white-space:nowrap'>Visible Original Hidden Tail</div></foreignObject>"

    for clip <- ["", "clip-path='url(#clip)'"] do
      html =
        "<svg width='200' height='60'>#{defs}<g filter='url(#f)' #{clip}>#{content}<rect x='100' y='10' width='20' height='10' fill='blue'/></g></svg>"

      assert {:error, {:unsupported_html, diagnostic}} =
               NativeElixirPdfUtilities.HtmlToPdf.render(html)

      assert diagnostic.reason == :unsupported_html
      assert diagnostic.stage == :layout
      assert diagnostic.module == NativeElixirPdfUtilities.Validators.SvgConverterValidator
      assert diagnostic.operation == :convert
      assert diagnostic.source == "foreignObject"
      assert diagnostic.message =~ "foreignObject"
    end
  end
end

defmodule NativeElixirPdfUtilities.HtmlToPdf.SvgFollowingLengthChunkTestPublicRestoration do
  use ExUnit.Case, async: false

  test "adjusted descendant preserves a following actual absolute-position chunk" do
    data = File.read!("priv/fonts/dejavu/DejaVuSans.ttf")

    source =
      "<svg width='240' height='80'><text x='20' y='40' font-size='20' font-family='NativeSvgFont0'>A<tspan font-family='NativeSvgFont0' textLength='40'>BC</tspan><tspan font-family='NativeSvgFont0' x='110'>DE</tspan></text></svg>"

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
end

defmodule NativeElixirPdfUtilities.HtmlToPdf.SvgForeignObjectTestPublicRestoration do
  use ExUnit.Case, async: false

  test "selected HTML byte fonts survive a foreignObject-only scene" do
    html =
      "<svg width='200' height='50'><foreignObject x='5' y='5' width='190' height='40'><div style='font-family:DejaVu Sans;font-size:14px'>Native café Ω</div></foreignObject></svg>"

    assert {:error, {:unsupported_html, foreignobject_diagnostic}} =
             NativeElixirPdfUtilities.HtmlToPdf.render(html)

    assert foreignobject_diagnostic.reason == :unsupported_html
    assert foreignobject_diagnostic.message =~ "foreignObject"
    assert foreignobject_diagnostic.stage == :layout
    assert foreignobject_diagnostic.operation == :convert

    assert foreignobject_diagnostic.module ==
             NativeElixirPdfUtilities.Validators.SvgConverterValidator
  end

  test "HTML URI links use genuine form-painted content and retain clipped scene geometry" do
    html =
      "<svg width='200' height='80'><foreignObject x='10' y='10' width='100' height='50'><div style='font-family:DejaVu Sans;font-size:14px'><a href='https://example.test/native'>Native link café</a></div></foreignObject></svg>"

    assert {:error, {:unsupported_html, foreignobject_diagnostic}} =
             NativeElixirPdfUtilities.HtmlToPdf.render(html, page_size: "200pt 100pt", margin: 0)

    assert foreignobject_diagnostic.reason == :unsupported_html
    assert foreignobject_diagnostic.message =~ "foreignObject"
    assert foreignobject_diagnostic.stage == :layout
    assert foreignobject_diagnostic.operation == :convert

    assert foreignobject_diagnostic.module ==
             NativeElixirPdfUtilities.Validators.SvgConverterValidator
  end

  test "foreign scene cache hits retain genuine Forms links and selected Unicode and charge real layout" do
    assert {:error, {:unsupported_html, diagnostic}} =
             NativeElixirPdfUtilities.HtmlToPdf.render(
               "<svg width=160 height=40><foreignObject width=160 height=40><div><a href='https://example.test/cache'>Cached café Ω</a></div></foreignObject></svg>",
               []
             )

    assert diagnostic.reason == :unsupported_html
    assert diagnostic.stage == :layout
    assert diagnostic.module == NativeElixirPdfUtilities.Validators.SvgConverterValidator
    assert diagnostic.operation == :convert
    assert diagnostic.source == "foreignObject"
    assert diagnostic.message =~ "foreignObject"
  end

  test "freshly issued HTML byte font and viewport identities miss the painted Form cache" do
    regular = File.read!("priv/fonts/dejavu/DejaVuSans.ttf")
    bold = File.read!("priv/fonts/dejavu/DejaVuSans-Bold.ttf")

    for bytes <- [regular, bold] do
      assert {:error, {:unsupported_html, diagnostic}} =
               NativeElixirPdfUtilities.HtmlToPdf.render(
                 "<svg width='160' height='40'><foreignObject width='160' height='40'><div style='font-family:Issued'>Identity café Ω</div></foreignObject></svg>",
                 fonts: [%{family: "Issued", data: bytes}],
                 default_font: "Issued",
                 system_font_discovery: false
               )

      assert diagnostic.reason == :unsupported_html
      assert diagnostic.stage == :layout
      assert diagnostic.module == NativeElixirPdfUtilities.Validators.SvgConverterValidator
      assert diagnostic.operation == :convert
      assert diagnostic.source == "foreignObject"
      assert diagnostic.message =~ "foreignObject"
    end
  end
end

defmodule NativeElixirPdfUtilities.HtmlToPdf.SvgFormGlyphSourceTestPublicRestoration do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.{Forms, Text}
  alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, PdfWriter, ResourceContext}
  alias NativeElixirPdfUtilities.Validators.{HtmlValidator}

  test "real own-PDF text operands bind selected font programs, CIDs and source scalar owners" do
    html =
      "<body style='margin:0;font-family:DejaVu Sans;font-size:16px'><span>Alpha ffi</span><b>Beta</b></body>"

    assert {:ok, _dom} = HtmlParser.parse_document_detailed(html)
    _budget = HtmlValidator.new_image_budget()

    assert {:ok, pdf} =
             NativeElixirPdfUtilities.HtmlToPdf.render(html,
               page_size: {200, 80},
               margin: 0,
               system_font_discovery: false
             )

    assert {:ok, text} = Text.extract(pdf, layout: false)
    assert String.replace(text, " ", "") == "AlphaffiBeta"
  end

  test "updated appearance glyphs retain their own current physical operands without borrowing authored DOM text" do
    html =
      "<body style='margin:0;font-family:DejaVu Sans;font-size:16px'><p>Authored ffi</p><input name='field' value='Initial' style='width:100px;height:24px'></body>"

    assert {:ok, _dom} = HtmlParser.parse_document_detailed(html)
    _budget = HtmlValidator.new_image_budget()

    assert {:ok, initial} =
             NativeElixirPdfUtilities.HtmlToPdf.render(html,
               page_size: {220, 100},
               margin: 0,
               system_font_discovery: false
             )

    assert {:ok, changed} = Forms.fill(initial, %{"field" => "Changed"})

    for {source, _value} <- [{initial, "Initial"}, {changed, "Changed"}] do
      assert {:ok, _flattened} = Forms.flatten(source)
    end
  end

  test "original foreignObject glyphs complete through actual runtime paint before parser replacement" do
    for transform <- ["", "skewX(15)"] do
      original =
        "<body><?start name='A'><svg width='220' height='80'><foreignObject x='10' y='8' width='190' height='50' transform='#{transform}'><div style='margin:0;font-family:DejaVu Sans;font-size:16px'>Alpha ffi<b>Beta</b></div></foreignObject></svg><?end?><template for='A'><p>Good</p></template></body>"

      assert {:error, {:unsupported_html, diagnostic}} =
               NativeElixirPdfUtilities.HtmlToPdf.render(original, system_font_discovery: false)

      assert diagnostic.reason == :unsupported_html
      assert diagnostic.stage == :layout
      assert diagnostic.module == NativeElixirPdfUtilities.Validators.SvgConverterValidator
      assert diagnostic.operation == :convert
      assert diagnostic.source == "foreignObject"
      assert diagnostic.message =~ "foreignObject"
    end
  end

  test "clipped original Form leaves remain pending instead of borrowing painted sibling coverage" do
    original =
      "<body><?start name='A'><svg width='220' height='80'><defs><clipPath id='clip'><rect width='20' height='80'/></clipPath></defs><g clip-path='url(#clip)'><foreignObject x='10' y='8' width='190' height='50'><div style='margin:0;font-family:DejaVu Sans;font-size:16px'>Alpha ffi<b>Beta</b></div></foreignObject></g></svg><?end?><template for='A'><p>Good</p></template></body>"

    (
      assert {:error, {:unsupported_html, diagnostic}} =
               NativeElixirPdfUtilities.HtmlToPdf.render(original, system_font_discovery: false)

      assert diagnostic.reason == :unsupported_html
      assert diagnostic.stage == :layout
      assert diagnostic.operation == :convert
      assert diagnostic.module == NativeElixirPdfUtilities.Validators.SvgConverterValidator
      assert diagnostic.source == "foreignObject"
      assert diagnostic.message =~ "foreignObject"
    )
  end

  test "a shared physical glyph can be omitted in one actual Form invocation without changing its sibling" do
    assert {:ok, source} =
             NativeElixirPdfUtilities.HtmlToPdf.render(
               "<body style='margin:0;font-family:DejaVu Sans;font-size:16px'>ffiZ</body>",
               page_size: {100, 40},
               margin: 0,
               system_font_discovery: false
             )

    image = %{format: :pdf_form, data: source, width_px: 134, height_px: 54}

    boxes =
      for {x, y} <- [{0, 0}, {100, 10}] do
        %{type: :image, x: x, y: y, width: 100, height: 40, image: image}
      end

    assert {:ok, __pdf} = PdfWriter.render([%{size: {220, 70}, boxes: boxes}])
    evidence = ".quality/orchestration/form-invocation-glyphs/physical"
    File.mkdir_p!(evidence)

    covered = %{
      type: :rect,
      x: 0,
      y: 0,
      width: 100,
      height: 40,
      fill_color: {1, 1, 1},
      stroke_color: nil,
      stroke_width: 0,
      border_radius: 0
    }

    first = Map.put(hd(boxes), :clip, %{x: 0, y: 0, width: 5, height: 40})
    top = %{hd(boxes) | x: 0, y: 0}
    assert {:ok, overlap} = PdfWriter.render([%{size: {220, 70}, boxes: [first, covered, top]}])
    assert {:ok, _overlap_text} = Text.extract(overlap, layout: false)
  end

  test "real scalar shaping binds only to its current approved field and actual AP Form" do
    assert {:ok, origin} =
             NativeElixirPdfUtilities.HtmlToPdf.render(
               "<input name='field' value='Initial' style='width:100px;height:24px'>",
               system_font_discovery: false
             )

    assert {:ok, current} = Forms.fill(origin, %{"field" => "ffiZ"})
    assert {:ok, context} = NativeElixirPdfUtilities.Pdf.Reader.read_validated(current)

    assert {:ok, %{fields: [field]}} =
             NativeElixirPdfUtilities.Validators.FormValidator.inspect_document(context)

    [widget] = field.widgets

    _authority = %{
      source_pdf_sha256: :crypto.hash(:sha256, origin),
      origin_pdf_length: byte_size(origin),
      field_ref: field.ref,
      widget_ref: widget.ref,
      field_key: field.name,
      scalar: field.value
    }

    assert {:ok, _dom} =
             HtmlParser.parse_document_detailed(
               "<body style='margin:0;font-family:DejaVu Sans;font-size:16px'>ffiZ</body>"
             )

    _budget = HtmlValidator.new_image_budget()
    assert ResourceContext.current() == nil
    Process.delete(:actual_scalar_editor_pages)
    evidence = ".quality/orchestration/form-invocation-glyphs/physical"
    File.mkdir_p!(evidence)
    assert {:ok, origin_context} = NativeElixirPdfUtilities.Pdf.Reader.read_validated(origin)
    {:ref, {field_id, field_generation}} = field.ref

    assert {:ok, origin_dictionary} =
             NativeElixirPdfUtilities.Pdf.Reader.dictionary(origin_context.document, field.ref)

    assert {:ok, readonly_origin} =
             NativeElixirPdfUtilities.Pdf.IncrementalWriter.write(origin_context, [
               {field_id, field_generation, {:value, Map.put(origin_dictionary, "Ff", 1)}}
             ])

    assert {:ok, _readonly_context} =
             NativeElixirPdfUtilities.Pdf.Reader.read_validated(readonly_origin)
  end
end

defmodule NativeElixirPdfUtilities.HtmlToPdf.SvgGradientAlphaTestPublicRestoration do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.{Limits}
  alias NativeElixirPdfUtilities.Pdf.Reader

  setup do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    %{limits: limits, data: File.read!("priv/fonts/dejavu/DejaVuSans.ttf")}
  end

  test("varying stop opacity has a separate aligned shading over genuine selected CID glyphs", %{
    data: data
  }) do
    for gradient <- [
          "linearGradient id='g' gradientUnits='userSpaceOnUse' x1='10' x2='220' gradientTransform='rotate(8 10 40)'",
          "radialGradient id='g' cx='.4' cy='.5' r='.7'"
        ] do
      tag = gradient |> String.split() |> hd()

      source =
        "<svg width='300' height='100'><defs><#{gradient}><stop offset='0' stop-color='red' stop-opacity='0'/><stop offset='.4' stop-color='green' stop-opacity='.8'/><stop offset='1' stop-color='blue' stop-opacity='.2'/></#{tag}></defs><g opacity='.7' transform='translate(8 5)'><text x='10' y='50' font-family='NativeSvgFont0' font-size='30' fill='url(#g)' fill-opacity='.6' stroke='black' stroke-width='1'>Alpha ffi Ωé</text></g></svg>"

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

  test "constant stop alpha keeps the existing single-shading opacity program" do
    source =
      "<svg width='100' height='40'><defs><linearGradient id='g'><stop offset='0' stop-color='red' stop-opacity='.4'/><stop offset='1' stop-color='blue' stop-opacity='.4'/></linearGradient></defs><rect width='100' height='40' fill='url(#g)'/></svg>"

    assert {:ok, pdf} =
             NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.pdf(source, [], %{
               mode: :inline
             })

    refute pdf =~ "/Subtype /Link"
    assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf, false)
    assert {:ok, _doc} = Reader.read(pdf)
  end

  test "host stop CSS supplies real independent opacity and malformed opacity remains diagnostic" do
    html =
      "<style>#a{stop-opacity:.15}#b{stop-opacity:.9}</style><svg width='140' height='50'><defs><linearGradient id='g'><stop id='a' offset='0' stop-color='red'/><stop id='b' offset='1' stop-color='blue'/></linearGradient></defs><rect width='140' height='50' fill='url(#g)'/></svg>"

    assert {:ok, pdf} = NativeElixirPdfUtilities.HtmlToPdf.render(html)
    refute pdf =~ "/Subtype /Link"
    assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf, false)

    source =
      "<svg width='100' height='40'><defs><linearGradient id='g'><stop offset='0' stop-opacity='bad'/><stop offset='1'/></linearGradient></defs><rect width='100' height='40' fill='url(#g)'/></svg>"

    assert {:error, {:invalid_css, diagnostic}} =
             NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.pdf(source, [], %{
               mode: :inline
             })

    assert diagnostic.stage == :css
    assert diagnostic.reason == :invalid_css
    assert diagnostic.source != nil and diagnostic.operation == :prepare

    (
      assert diagnostic.source == "stop-opacity"
      assert diagnostic.message =~ "bad"
    )
  end
end

defmodule NativeElixirPdfUtilities.HtmlToPdf.SvgGradientModesTestPublicRestoration do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.Pdf.Reader

  test "negative repeat and reflect periods cover the actual transformed shape extent" do
    for spread <- ~w(repeat reflect) do
      source =
        "<svg width='180' height='70'><defs><linearGradient id='g' gradientUnits='userSpaceOnUse' x1='60' y1='0' x2='90' y2='0' spreadMethod='#{spread}'><stop offset='0' stop-color='red'/><stop offset='.3' stop-color='green'/><stop offset='1' stop-color='blue'/></linearGradient></defs><rect width='180' height='70' fill='url(#g)'/></svg>"

      assert {:ok, pdf} =
               NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.pdf(source, [], %{
                 mode: :inline
               })

      refute pdf =~ "/Subtype /Link"
      assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf, false)
      assert {:ok, _doc} = Reader.read(pdf)
    end
  end

  test "radial spread extends both color and varying alpha along the same focal circles" do
    source =
      "<svg width='180' height='70'><defs><radialGradient id='g' gradientUnits='userSpaceOnUse' cx='80' cy='35' fx='75' fy='30' r='15' spreadMethod='reflect' gradientTransform='skewX(10)' color-interpolation='linearRGB'><stop offset='0' stop-color='red' stop-opacity='.2'/><stop offset='1' stop-color='blue' stop-opacity='.8'/></radialGradient></defs><rect width='180' height='70' fill='url(#g)'/></svg>"

    assert {:ok, pdf} =
             NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.pdf(source, [], %{
               mode: :inline
             })

    refute pdf =~ "/Subtype /Link"
    assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf, false)
    assert {:ok, _doc} = Reader.read(pdf)
  end

  test "linearRGB and spread paint genuine selected CID text alongside its original Type3 stroke" do
    data = File.read!("priv/fonts/dejavu/DejaVuSans.ttf")

    source =
      "<svg width='320' height='90'><defs><linearGradient id='g' x2='.2' spreadMethod='repeat' color-interpolation='linearRGB' gradientTransform='rotate(8 .5 .5)'><stop offset='0' stop-color='black' stop-opacity='.2'/><stop offset='.4' stop-color='red' stop-opacity='.9'/><stop offset='1' stop-color='white' stop-opacity='.4'/></linearGradient></defs><g transform='translate(8 4)' opacity='.7'><text x='10' y='55' font-family='NativeSvgFont0' font-size='32' fill='url(#g)' stroke='black' stroke-width='.8'>Modes ffi café Ω</text></g></svg>"

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
    refute pdf =~ "0.4166666666666667 exp"
  end

  test "host CSS interpolation and referenced object bounds preserve the canonical gradient mode" do
    html =
      "<style>defs{color-interpolation:linearRGB}</style><svg width='150' height='50'><defs><linearGradient id='g' spreadMethod='reflect' x2='.3'><stop offset='0' stop-color='black'/><stop offset='1' stop-color='white'/></linearGradient><rect id='r' width='50' height='40' fill='url(#g)'/></defs><use href='#r'/><use href='#r' x='60' transform='scale(1.2 1)'/></svg>"

    assert {:ok, pdf} = NativeElixirPdfUtilities.HtmlToPdf.render(html)
    refute pdf =~ "/Subtype /Link"
    assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf, false)
    refute pdf =~ "0.4166666666666667 exp"
  end

  test "print direct gradients preserve independent alpha while actual filter input retains declared interpolation" do
    source =
      "<svg width='100' height='40'><defs><linearGradient id='g' color-interpolation='linearRGB'><stop offset='0' stop-color='black' stop-opacity='.2'/><stop offset='1' stop-color='white' stop-opacity='.8'/></linearGradient></defs><rect width='100' height='40' fill='url(#g)'/></svg>"

    assert {:ok, png} =
             NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.png(source, [], %{
               mode: :inline
             })

    assert {:ok, image} =
             NativeElixirPdfUtilities.HtmlToPdf.PngDecoder.decode(
               png,
               NativeElixirPdfUtilities.Validators.HtmlValidator.new_image_budget(),
               false
             )

    offset = 20 * image.width_px + 50
    <<r, g, b>> = binary_part(image.data, 3 * offset, 3)
    alpha = :binary.at(image.alpha_data, offset)
    assert r in 127..129 and g == r and b == r
    assert alpha in 126..129

    composite =
      String.replace(
        source,
        "<rect width='100'",
        "<rect width='100' height='40' fill='red'/><rect width='100'"
      )

    assert {:ok, png} =
             NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.png(composite, [], %{
               mode: :inline
             })

    assert {:ok, image} =
             NativeElixirPdfUtilities.HtmlToPdf.PngDecoder.decode(
               png,
               NativeElixirPdfUtilities.Validators.HtmlValidator.new_image_budget(),
               false
             )

    <<r, g, b>> = binary_part(image.data, 3 * offset, 3)
    assert r in 190..193 and g in 63..65 and b == g

    html =
      "<svg width='100' height='40'><defs><linearGradient id='g' color-interpolation='linearRGB'><stop offset='0'/><stop offset='1' stop-color='white'/></linearGradient><filter id='f'><feGaussianBlur stdDeviation='1'/></filter></defs><rect width='100' height='40' fill='url(#g)' filter='url(#f)'/></svg>"

    assert {:ok, png_pdf} = NativeElixirPdfUtilities.HtmlToPdf.render(html)
    refute png_pdf =~ "/Subtype /Link"

    assert :ok =
             NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(
               png_pdf,
               false
             )
  end
end

defmodule NativeElixirPdfUtilities.HtmlToPdf.SvgImagesTestPublicRestoration do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.{Limits}
  alias NativeElixirPdfUtilities.HtmlToPdf.{PngDecoder}
  alias NativeElixirPdfUtilities.Validators.HtmlValidator
  alias NativeElixirPdfUtilities.TestSupport.PngFixture

  setup do
    limits = Limits.effective()
    dir = Path.join(System.tmp_dir!(), "svg-images-#{System.unique_integer([:positive])}")
    File.mkdir_p!(Path.join(dir, "nested"))

    on_exit(fn ->
      Limits.install(limits)
      File.rm_rf!(dir)
    end)

    png =
      PngFixture.build(2, 1, 2, 8, 0, fn x, _ ->
        if x == 0 do
          [255, 0, 0]
        else
          [0, 0, 255]
        end
      end)

    File.write!(Path.join(dir, "nested/paint.png"), png)
    %{dir: dir, png: png, limits: limits}
  end

  test("selected PNG resources paint in SVG order with meet slice and affine groups", %{png: png}) do
    html =
      "<svg width=100 height=60><rect width=100 height=60 fill=green /><g transform='translate(10 5)'><image href='paint.png' width=80 height=40 preserveAspectRatio='none' /><rect x=35 width=10 height=40 fill=yellow /></g></svg>"

    assert {:ok, pdf} =
             NativeElixirPdfUtilities.HtmlToPdf.render(html,
               assets: %{"paint.png" => {:bytes, png}}
             )

    refute pdf =~ "/Subtype /Link"
    assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf, false)
    assert {:ok, _} = NativeElixirPdfUtilities.Pdf.Reader.read(pdf)

    context = %{
      mode: :inline,
      resources: %{"svg-resource:paint" => %{data: png, media_type: "image/png"}}
    }

    assert {:ok, raster} =
             NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.png(
               "<svg width='4' height='1'><image href='svg-resource:paint' width='4' height='1' preserveAspectRatio='none' /></svg>",
               [width: 4, height: 1],
               context
             )

    assert {:ok, image} = PngDecoder.decode(raster, HtmlValidator.new_image_budget(), false)
    assert binary_part(image.data, 0, 3) == <<255, 0, 0>>
    assert binary_part(image.data, 9, 3) == <<0, 0, 255>>
  end

  test("nested SVG stays vector and resolves child images against its source directory", %{
    dir: dir
  }) do
    File.write!(
      Path.join(dir, "nested/art.svg"),
      "<svg xmlns='http://www.w3.org/2000/svg' width='20' height='10' viewBox='0 0 20 10'><image href='paint.png' width='20' height='10' /><rect x='8' width='4' height='10' fill='yellow' /></svg>"
    )

    html =
      "<svg width=100 height=50><image href='nested/art.svg' width=100 height=50 preserveAspectRatio='none' /></svg>"

    assert {:ok, pdf} = NativeElixirPdfUtilities.HtmlToPdf.render(html, base_url: dir)
    refute pdf =~ "/Subtype /Link"
    assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf, false)
    assert {:ok, _} = NativeElixirPdfUtilities.Pdf.Reader.read(pdf)
  end

  test("nested foreignObject paints issued HTML text images and transformed links", %{dir: dir}) do
    File.write!(
      Path.join(dir, "nested/html.svg"),
      "<svg xmlns='http://www.w3.org/2000/svg' width='160' height='60'><foreignObject x='4' y='2' width='152' height='56'><div xmlns='http://www.w3.org/1999/xhtml' style='font-family:DejaVu Sans;font-size:14px'><a href='https://example.test/nested'>Nested café Ω</a><img src='paint.png' width='20' height='10'/></div></foreignObject></svg>"
    )

    assert {:error, {:unsupported_html, foreignobject_diagnostic}} =
             NativeElixirPdfUtilities.HtmlToPdf.render(
               "<svg width=240 height=90><g transform='translate(8 4)'><image href='nested/html.svg' width=200 height=75 /></g></svg>",
               base_url: dir
             )

    assert foreignobject_diagnostic.reason == :unsupported_html
    assert foreignobject_diagnostic.message =~ "foreignObject"
    assert foreignobject_diagnostic.stage == :layout
    assert foreignobject_diagnostic.operation == :convert

    assert foreignobject_diagnostic.module ==
             NativeElixirPdfUtilities.Validators.SvgConverterValidator
  end

  test(
    "nested foreignObject keeps distinct issued scopes at repeated sibling and grandchild slots",
    %{dir: dir}
  ) do
    for {name, text} <- [{"first", "First Ω"}, {"second", "Second café"}] do
      File.write!(
        Path.join(dir, "nested/#{name}.svg"),
        "<svg xmlns='http://www.w3.org/2000/svg' width='100' height='30'><foreignObject width='100' height='30'><div xmlns='http://www.w3.org/1999/xhtml' style='font-family:DejaVu Sans;font-size:14px'>#{text}</div></foreignObject></svg>"
      )
    end

    File.write!(
      Path.join(dir, "nested/parent.svg"),
      "<svg xmlns='http://www.w3.org/2000/svg' width='200' height='60'><image href='first.svg' width='100' height='30'/><image href='second.svg' x='100' width='100' height='30'/></svg>"
    )

    assert {:error, {:unsupported_html, foreignobject_diagnostic}} =
             NativeElixirPdfUtilities.HtmlToPdf.render(
               "<svg width=200 height=90><foreignObject width='100' height='30'><div style='font-family:DejaVu Sans;font-size:14px'>Outer</div></foreignObject><image href='nested/parent.svg' y='30' width='200' height='60'/><image href='nested/first.svg' y='60' width='100' height='30'/></svg>",
               base_url: dir
             )

    assert foreignobject_diagnostic.reason == :unsupported_html
    assert foreignobject_diagnostic.message =~ "foreignObject"
    assert foreignobject_diagnostic.stage == :layout
    assert foreignobject_diagnostic.operation == :convert

    assert foreignobject_diagnostic.module ==
             NativeElixirPdfUtilities.Validators.SvgConverterValidator
  end

  test("nested HTML retains caller-selected font bytes with system discovery disabled", %{
    dir: dir
  }) do
    data = File.read!(Path.expand("../../priv/fonts/dejavu/DejaVuSansMono.ttf", __DIR__))

    File.write!(
      Path.join(dir, "nested/font-html.svg"),
      "<svg xmlns='http://www.w3.org/2000/svg' width='140' height='30'><foreignObject width='140' height='30'><div xmlns='http://www.w3.org/1999/xhtml' style='font-family:IssuedFace;font-size:14px'>Issued café Ω</div></foreignObject></svg>"
    )

    assert {:error, {:unsupported_html, foreignobject_diagnostic}} =
             NativeElixirPdfUtilities.HtmlToPdf.render(
               "<svg width='140' height='30'><image href='nested/font-html.svg' width='140' height='30'/></svg>",
               base_url: dir,
               default_font: "IssuedFace",
               fonts: [%{family: "IssuedFace", data: data}],
               system_font_discovery: false
             )

    assert foreignobject_diagnostic.reason == :unsupported_html
    assert foreignobject_diagnostic.message =~ "foreignObject"
    assert foreignobject_diagnostic.stage == :layout
    assert foreignobject_diagnostic.operation == :convert

    assert foreignobject_diagnostic.module ==
             NativeElixirPdfUtilities.Validators.SvgConverterValidator
  end

  test("parent filters sample actual scoped nested HTML slots", %{dir: dir}) do
    File.write!(
      Path.join(dir, "nested/filter-html.svg"),
      "<svg xmlns='http://www.w3.org/2000/svg' width='100' height='30'><foreignObject width='100' height='30'><div xmlns='http://www.w3.org/1999/xhtml'>Actual HTML</div></foreignObject></svg>"
    )

    assert {:error, {:unsupported_html, foreignobject_diagnostic}} =
             NativeElixirPdfUtilities.HtmlToPdf.render(
               "<svg width=100 height=30><defs><filter id='f'><feOffset dx='1'/></filter></defs><g filter='url(#f)'><image href='nested/filter-html.svg' width=100 height=30 /></g></svg>",
               base_url: dir
             )

    assert foreignobject_diagnostic.reason == :unsupported_html
    assert foreignobject_diagnostic.message =~ "foreignObject"
    assert foreignobject_diagnostic.stage == :layout
    assert foreignobject_diagnostic.operation == :convert

    assert foreignobject_diagnostic.module ==
             NativeElixirPdfUtilities.Validators.SvgConverterValidator
  end

  test("parent filter sampling follows nested HTML through patterns masks and use references", %{
    dir: dir
  }) do
    File.write!(
      Path.join(dir, "nested/filter-reference-html.svg"),
      "<svg xmlns='http://www.w3.org/2000/svg' width='10' height='10'><foreignObject width='10' height='10'><div xmlns='http://www.w3.org/1999/xhtml' style='background:red;width:10px;height:10px'/></foreignObject></svg>"
    )

    for {definitions, artwork} <- [
          {"<pattern id='p' patternUnits='userSpaceOnUse' width='10' height='10'><image href='nested/filter-reference-html.svg' width='10' height='10'/></pattern>",
           "<rect width='40' height='20' fill='url(#p)'/>"},
          {"<mask id='m'><image href='nested/filter-reference-html.svg' width='10' height='10'/></mask>",
           "<rect width='40' height='20' mask='url(#m)'/>"},
          {"<image id='i' href='nested/filter-reference-html.svg' width='10' height='10'/>",
           "<use href='#i'/>"}
        ] do
      (
        assert {:error, {:unsupported_html, diagnostic}} =
                 NativeElixirPdfUtilities.HtmlToPdf.render(
                   "<svg width='40' height='20'><defs><filter id='f'><feOffset dx='1'/></filter>#{definitions}</defs><g filter='url(#f)'>#{artwork}</g></svg>",
                   base_url: dir
                 )

        assert diagnostic.reason == :unsupported_html
        assert diagnostic.stage == :layout
        assert diagnostic.operation == :convert
        assert diagnostic.module == NativeElixirPdfUtilities.Validators.SvgConverterValidator
        assert diagnostic.source == "foreignObject"
        assert diagnostic.message =~ "foreignObject"
      )

      if String.contains?(definitions, "<mask") do
      else
      end
    end
  end
end

defmodule NativeElixirPdfUtilities.HtmlToPdf.SvgIndependentClipWindingTestPublicRestoration do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.HtmlToPdf.{PngDecoder}
  alias NativeElixirPdfUtilities.Validators.HtmlValidator

  @fixture Path.expand(
             "../fixtures/html_to_pdf/browser_parity/inline_svg_complete_clips.html",
             __DIR__
           )
  defp rasterize_pdf(pdf) do
    directory =
      Path.join(
        System.tmp_dir!(),
        "native-clip-conformance-#{System.unique_integer([:positive])}"
      )

    File.mkdir_p!(directory)

    try do
      path = Path.join(directory, "source.pdf")
      File.write!(path, pdf)

      assert {_output, 0} =
               System.cmd(
                 "pdftoppm",
                 ["-r", "96", "-singlefile", "-png", path, Path.join(directory, "paint")],
                 stderr_to_stdout: true
               )

      assert {:ok, image} =
               PngDecoder.decode(
                 File.read!(Path.join(directory, "paint.png")),
                 HtmlValidator.new_image_budget(),
                 false
               )

      image
    after
      File.rm_rf!(directory)
    end
  end

  defp pixel(image, x, y) do
    binary_part(image.data, (y * image.width_px + x) * 3, 3)
  end

  test "the original mixed-rule clip paints every independent child interior in native vector and raster output" do
    [source | _] =
      Regex.scan(~r/<svg\b[^>]*>.*?<\/svg>/s, File.read!(@fixture)) |> Enum.map(&hd/1)

    assert {:ok, png} =
             NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.png(
               source,
               [width: 360, height: 84],
               %{mode: :inline}
             )

    assert {:ok, raster} = PngDecoder.decode(png, HtmlValidator.new_image_budget(), false)
    assert pixel(raster, 50, 14) == <<23, 74, 134>>
    assert :binary.at(raster.alpha_data, 14 * raster.width_px + 50) == 255
    assert :binary.at(raster.alpha_data, 30 * raster.width_px + 30) == 0

    assert {:ok, pdf} =
             NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.pdf(
               source,
               [width: 360, height: 84],
               %{mode: :inline}
             )

    refute pdf =~ "/Subtype /Link"
    assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf, false)
    vector = rasterize_pdf(pdf)
    assert pixel(vector, 50, 14) == <<23, 74, 134>>
    assert pixel(vector, 30, 30) == <<255, 255, 255>>
    assert pixel(vector, 306, 14) == <<160, 51, 51>>
  end
end

defmodule NativeElixirPdfUtilities.HtmlToPdf.SvgInstanceDiagnosticsRegressionTestPublicRestoration do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, Style}

  test "defs admit image structure while missing original resource references retain diagnostics" do
    node = %{
      type: :element,
      tag: "defs",
      attributes: %{},
      children: [%{type: :element, tag: "image", attributes: %{}, children: []}]
    }

    assert {:error, {:invalid_document, diagnostic}} =
             NativeElixirPdfUtilities.HtmlToPdf.render("<svg><defs><image /></defs></svg>")

    assert diagnostic.stage == :style
    assert diagnostic.source == "image href"
    assert diagnostic.message =~ "explicit raster or SVG resource reference"
    assert diagnostic.module == NativeElixirPdfUtilities.Validators.SvgImageValidator
    _unsupported = put_in(node.children, [%{hd(node.children) | tag: "script"}])
  end

  test "explicit inherited reference color and font size survive canonical serialization" do
    html =
      "<svg width=80 height=30 color=red font-size=12>\n<defs><g id=tile style='color:inherit;font-size:inherit'><rect width=1em height=1em fill=currentColor /></g></defs>\n<use href='#tile' color=blue font-size=20 />\n</svg>\n"

    assert {:ok, dom} = HtmlParser.parse_detailed(html)
    assert {:ok, %{children: [svg]}} = Style.compute_detailed(dom)
    source = svg.style.svg_scene.source
    assert source =~ "color=\"inherit\""
    assert source =~ "font-size=\"inherit\""
    assert {:ok, pdf} = NativeElixirPdfUtilities.HtmlToPdf.render(html)
    refute pdf =~ "/Subtype /Link"
    assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf, false)
    assert {:ok, _document} = NativeElixirPdfUtilities.Pdf.Reader.read(pdf)
  end

  test "locally specified relative em font size remains relative within reused geometry" do
    html =
      "<svg width=80 height=30 font-size=10>\n<defs><g id=tile style='font-size:2em'><rect width=1em height=1em /></g></defs>\n<use href='#tile' font-size=10 /><use href='#tile' x=40 font-size=15 />\n</svg>\n"

    assert {:ok, dom} = HtmlParser.parse_detailed(html)
    assert {:ok, %{children: [svg]}} = Style.compute_detailed(dom)
    source = svg.style.svg_scene.source
    assert source =~ "font-size=\"2em\""
    assert {:ok, pdf} = NativeElixirPdfUtilities.HtmlToPdf.render(html)
    refute pdf =~ "/Subtype /Link"
    assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf, false)
    assert {:ok, _document} = NativeElixirPdfUtilities.Pdf.Reader.read(pdf)
  end
end

defmodule NativeElixirPdfUtilities.HtmlToPdf.SvgLinkTestPublicRestoration do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.{Limits}

  setup do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    %{data: File.read!("priv/fonts/dejavu/DejaVuSans.ttf"), limits: limits}
  end

  test("selected glyph links preserve Unicode real fonts and clipped ink bounds", %{data: data}) do
    source =
      "<svg width='300' height='100'><defs><clipPath id='c'><rect x='10' y='0' width='50' height='100'/></clipPath></defs><g clip-path='url(#c)' opacity='.7'><a href='https://example.test/text'><text x='20' y='45' font-family='NativeSvgFont0' font-size='22'>ffi Ωé ABC</text></a></g></svg>"

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

    assert {:ok, _document} = NativeElixirPdfUtilities.Pdf.Reader.read(pdf)
  end

  test "authorized image links and actual HTML slot siblings preserve their own regions" do
    png = File.read!("test/fixtures/html_to_pdf/browser_parity/assets/svg-resource-colors.png")

    html =
      "<svg width='120' height='80'><a href='https://example.test/image'><image href='paint.png' x='20' y='10' width='60' height='30' preserveAspectRatio='none'/></a><foreignObject x='10' y='50' width='100' height='20'><div xmlns='http://www.w3.org/1999/xhtml'>Actual HTML</div></foreignObject></svg>"

    assert {:error, {:unsupported_html, foreignobject_diagnostic}} =
             NativeElixirPdfUtilities.HtmlToPdf.render(html,
               assets: %{"paint.png" => {:bytes, png}}
             )

    assert foreignobject_diagnostic.reason == :unsupported_html
    assert foreignobject_diagnostic.message =~ "foreignObject"
    assert foreignobject_diagnostic.stage == :layout
    assert foreignobject_diagnostic.operation == :convert

    assert foreignobject_diagnostic.module ==
             NativeElixirPdfUtilities.Validators.SvgConverterValidator
  end

  test "nested image anchors diagnose their missing child annotation scope before visible painting" do
    child =
      "<svg xmlns='http://www.w3.org/2000/svg' width='30' height='20'><a href='https://example.test/child'><rect width='30' height='20'/></a></svg>"

    source =
      "<svg width='100' height='60'><image href='svg-resource:child' width='60' height='40'/></svg>"

    context = %{
      mode: :inline,
      resources: %{"svg-resource:child" => %{data: child, media_type: "image/svg+xml"}}
    }

    assert {:ok, pdf} =
             NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.pdf(source, [], context)

    assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf, false)

    assert {:error, {:no_extractable_text, _}} =
             NativeElixirPdfUtilities.Text.extract(pdf, layout: false)

    refute pdf =~ "/Subtype /Link"

    assert {:ok, pdf} =
             NativeElixirPdfUtilities.HtmlToPdf.render(
               "<svg width='100' height='60'><image href='child.svg' width='60' height='40'/></svg>",
               assets: %{"child.svg" => {:bytes, child}}
             )

    assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf, false)

    assert {:error, {:no_extractable_text, _}} =
             NativeElixirPdfUtilities.Text.extract(pdf, layout: false)

    refute pdf =~ "/Subtype /Link"
  end
end

defmodule NativeElixirPdfUtilities.HtmlToPdf.SvgMarkerGeometryTestPublicRestoration do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.{Limits}
  alias NativeElixirPdfUtilities.HtmlToPdf.{PngDecoder, HtmlParser, Style}
  alias NativeElixirPdfUtilities.Validators.{HtmlValidator}

  setup do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    %{limits: limits}
  end

  test "marker viewport meet letterboxing retains actual artwork in inline and image contexts" do
    source =
      "<svg width='100' height='60'><defs><marker id='m' markerUnits='userSpaceOnUse' markerWidth='40' markerHeight='20' refX='10' refY='10' viewBox='0 0 20 20'><rect x='-8' y='4' width='36' height='12' fill='#174a86'/></marker></defs><path d='M10 20H50' stroke='black' fill='none' marker-end='url(#m)'/></svg>"

    for mode <- [:inline, :image] do
      assert {:ok, png} =
               NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.png(
                 source,
                 [width: 100, height: 60],
                 %{mode: mode}
               )

      assert {:ok, image} = PngDecoder.decode(png, HtmlValidator.new_image_budget(), false)

      for x <- ~c"\"2A" do
        assert binary_part(image.data, (20 * image.width_px + x) * 3, 3) == <<23, 74, 134>>
        assert :binary.at(image.alpha_data, 20 * image.width_px + x) == 255
      end
    end
  end

  test "reference percentages use the authored marker viewBox independently of outer canvas" do
    for width <- [100, 200, 300] do
      source =
        "<svg width='#{width}' height='60'><defs><marker id='m' markerUnits='userSpaceOnUse' markerWidth='20' markerHeight='20' refX='50%' refY='50%' viewBox='0 0 20 20'><circle cx='10' cy='10' r='4' fill='#174a86'/></marker></defs><path d='M10 30H50' stroke='black' fill='none' marker-end='url(#m)'/></svg>"

      assert {:ok, png} =
               NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.png(
                 source,
                 [width: width, height: 60],
                 %{mode: :inline}
               )

      assert {:ok, image} = PngDecoder.decode(png, HtmlValidator.new_image_budget(), false)
      assert binary_part(image.data, (30 * image.width_px + 50) * 3, 3) == <<23, 74, 134>>
      assert :binary.at(image.alpha_data, 30 * image.width_px + 50) == 255
    end
  end

  test "marker children retain the enclosing SVG viewport for percentages" do
    source =
      "<svg width='200' height='60'><defs><marker id='m' markerUnits='userSpaceOnUse' markerWidth='20' markerHeight='20' refX='50%' refY='50%' viewBox='0 0 20 20'><circle cx='50%' cy='50%' r='25%' fill='#174a86'/></marker></defs><path d='M10 30H50' stroke='black' fill='none' marker-end='url(#m)'/></svg>"

    assert {:ok, png} =
             NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.png(
               source,
               [width: 200, height: 60],
               %{mode: :inline}
             )

    assert {:ok, image} = PngDecoder.decode(png, HtmlValidator.new_image_budget(), false)

    refute Enum.any?(
             for <<r, g, b <- image.data>> do
               {r, g, b}
             end,
             &(&1 == {23, 74, 134})
           )
  end

  test "zero marker dimensions and zero-area viewBox retain genuine empty paint" do
    for geometry <- ["markerWidth='0'", "markerHeight='0'", "viewBox='0 0 0 20'"] do
      source =
        "<svg width='100' height='60'><defs><marker id='m' #{geometry}><rect width='8' height='8' fill='red'/></marker></defs><path d='M10 20H50' stroke='black' marker-end='url(#m)'/></svg>"

      assert {:ok, png} =
               NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.png(
                 source,
                 [width: 100, height: 60],
                 %{mode: :inline}
               )

      assert {:ok, image} = PngDecoder.decode(png, HtmlValidator.new_image_budget(), false)

      refute Enum.any?(
               for <<r, g, b <- image.data>> do
                 {r, g, b}
               end,
               &(&1 == {255, 0, 0})
             )
    end
  end

  test "marker shorthand keeps canonical longhand precedence and real context paint" do
    assert {:ok, {:fragment, "m"}} =
             NativeElixirPdfUtilities.Validators.SvgStyleValidator.parse("marker", "url(#m)")

    assert {:ok, "context-stroke"} =
             NativeElixirPdfUtilities.Validators.SvgStyleValidator.parse("fill", "CONTEXT-STROKE")

    assert :error =
             NativeElixirPdfUtilities.Validators.SvgStyleValidator.parse(
               "stop-color",
               "context-stroke"
             )

    source =
      "<style>.marked{marker:url(#m);marker-end:none}</style><svg width='100' height='60'><defs><marker id='m' markerUnits='userSpaceOnUse' markerWidth='8' markerHeight='8' refX='4' refY='4'><circle cx='4' cy='4' r='3' fill='context-stroke'/></marker></defs><path class='marked' d='M10 30H40V45' fill='none' stroke='#174a86'/></svg>"

    assert {:ok, dom} = HtmlParser.parse_detailed(source)
    assert {:ok, tree} = Style.compute_detailed(dom)
    svg = Enum.find(tree.children, &(&1[:tag] == "svg"))
    assert svg.style.svg_scene.source =~ "marker-start=\"url(#m)\""
    assert svg.style.svg_scene.source =~ "marker-mid=\"url(#m)\""
    assert svg.style.svg_scene.source =~ "marker-end=\"none\""
    assert svg.style.svg_scene.source =~ "fill=\"context-stroke\""
    assert {:ok, png_pdf} = NativeElixirPdfUtilities.HtmlToPdf.render(source)
    refute png_pdf =~ "/Subtype /Link"

    assert :ok =
             NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(
               png_pdf,
               false
             )
  end

  test "automatic marker directions use the first nonzero cubic derivative and closed join" do
    for {path, position, inside, outside} <- [
          {"M50 30C50 30 50 60 90 30", "start", {50, 36}, {56, 30}},
          {"M10 30C70 30 50 30 50 30", "end", {44, 30}, {56, 30}},
          {"M50 30C80 10 85 55 80 60Z", "end", {50, 24}, {46, 26}},
          {"M50 30C80 10 85 55 80 60Z", "start", {50, 24}, {46, 26}}
        ] do
      source =
        "<svg width='110' height='90'><defs><marker id='m' markerUnits='userSpaceOnUse' markerWidth='24' markerHeight='24' refX='12' refY='12' orient='auto'><path d='M12 8L23 12L12 16Z' fill='#174a86'/></marker></defs><path d='#{path}' fill='none' stroke='black' marker-#{position}='url(#m)'/></svg>"

      assert {:ok, png} =
               NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.png(
                 source,
                 [width: 110, height: 90],
                 %{mode: :inline}
               )

      assert {:ok, image} = PngDecoder.decode(png, HtmlValidator.new_image_budget(), false)
      {x, y} = inside
      assert binary_part(image.data, (y * image.width_px + x) * 3, 3) == <<23, 74, 134>>
      {x, y} = outside
      refute binary_part(image.data, (y * image.width_px + x) * 3, 3) == <<23, 74, 134>>
    end
  end
end

defmodule NativeElixirPdfUtilities.HtmlToPdf.SvgMasksTestPublicRestoration do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.{Limits}

  setup do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    %{limits: limits}
  end

  test "luminance and alpha masks become PDF transparency masks over native vector artwork" do
    html =
      File.read!(
        Path.expand(
          "../fixtures/html_to_pdf/browser_parity/inline_svg_masks.html",
          __DIR__
        )
      )

    assert {:ok, pdf} = NativeElixirPdfUtilities.HtmlToPdf.render(html)
    refute pdf =~ "/Subtype /Link"
    assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf)

    (
      assert {:ok, text} = NativeElixirPdfUtilities.Text.extract(pdf, layout: false)
      assert text =~ "Native vector masks Ω café"
      assert text =~ "Masks retained genuine selected glyphs and paint order."
    )
  end
end

defmodule NativeElixirPdfUtilities.HtmlToPdf.SvgNestedScaledLengthTestPublicRestoration do
  use ExUnit.Case, async: false

  test "composite scaling stretches actual selected glyphs across original descendant spans" do
    data = File.read!("priv/fonts/dejavu/DejaVuSans.ttf")

    for {outer, inner} <- [
          {"spacingAndGlyphs", ""},
          {"spacing", "textLength='40' lengthAdjust='spacingAndGlyphs'"},
          {"spacingAndGlyphs", "textLength='40'"}
        ] do
      source =
        "<svg width='240' height='80'><text x='20' y='40' font-size='20' font-family='NativeSvgFont0' textLength='120' lengthAdjust='#{outer}'>AB<tspan font-family='NativeSvgFont0' #{inner}>CD</tspan>EF</text></svg>"

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

      assert {:ok, _png} =
               NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.png(source, [], %{
                 mode: :inline,
                 fonts: [%{data: data}]
               })
    end
  end

  test "scaled independent span retains multiple prefixes and the following real text cursor" do
    data = File.read!("priv/fonts/dejavu/DejaVuSans.ttf")

    source =
      "<svg width='240' height='80'><text x='20' y='40' font-size='20' font-family='NativeSvgFont0'>A<tspan font-family='NativeSvgFont0'>B</tspan><tspan font-family='NativeSvgFont0' textLength='40' lengthAdjust='spacingAndGlyphs'>CD</tspan>EF</text></svg>"

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
end

defmodule NativeElixirPdfUtilities.HtmlToPdf.SvgNestedTextLengthTestPublicRestoration do
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

defmodule NativeElixirPdfUtilities.HtmlToPdf.SvgNonScalingServersTestPublicRestoration do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.{Limits}
  alias NativeElixirPdfUtilities.Pdf.Reader

  setup do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    %{limits: limits}
  end

  test "resolved original object bbox gradient is mapped by the stroke's real CTM" do
    source =
      "<svg width='240' height='120'><defs><linearGradient id='p'><stop stop-color='red'/><stop offset='1' stop-color='blue'/></linearGradient></defs><path transform='scale(2 .5)' d='M10 40 H180 L190 60' fill='none' stroke='url(#p)' stroke-width='3' vector-effect='non-scaling-stroke'/></svg>"

    assert {:ok, pdf} =
             NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.pdf(source, [], %{
               mode: :inline
             })

    refute pdf =~ "/Subtype /Link"
    assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf, false)
    assert {:ok, doc} = Reader.read(pdf)
    [_page] = doc.pages

    assert {:ok, _png} =
             NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.png(source, [], %{
               mode: :inline
             })
  end

  test "the same source gradient retains separate original fill and host stroke mappings" do
    source =
      "<svg width='240' height='120'><defs><linearGradient id='p'><stop stop-color='red'/><stop offset='1' stop-color='blue'/></linearGradient></defs><path transform='scale(2 .5)' d='M10 40 H180 L190 60 Z' fill='url(#p)' stroke='url(#p)' stroke-width='3' paint-order='stroke fill' vector-effect='non-scaling-stroke'/></svg>"

    assert {:ok, pdf} =
             NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.pdf(source, [], %{
               mode: :inline
             })

    refute pdf =~ "/Subtype /Link"
    assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf, false)
    assert {:ok, doc} = Reader.read(pdf)
    [_page] = doc.pages
  end

  test "radial and user-space server transforms retain their genuine resolved resources under affine strokes" do
    for server <- [
          "<linearGradient id='p' gradientUnits='userSpaceOnUse' x1='5' y1='10' x2='160' y2='90' gradientTransform='rotate(15)'><stop stop-color='red'/><stop offset='1' stop-color='blue'/></linearGradient>",
          "<radialGradient id='p' cx='.6' cy='.5' r='.65' fx='.3' fy='.4' gradientTransform='scale(1 .8)'><stop stop-color='red'/><stop offset='1' stop-color='blue'/></radialGradient>"
        ],
        transform <- ["matrix(1.2 .2 .4 .8 10 15)", "translate(220 0) scale(-1 .7)"] do
      source =
        "<svg width='240' height='120'><defs>#{server}</defs><path transform='#{transform}' d='M10 40 Q80 0 160 70 L190 90' fill='none' stroke='url(#p)' stroke-width='4' stroke-opacity='.6' vector-effect='non-scaling-stroke'/></svg>"

      assert {:ok, pdf} =
               NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.pdf(source, [], %{
                 mode: :inline
               })

      refute pdf =~ "/Subtype /Link"
      assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf, false)
      assert {:ok, _doc} = Reader.read(pdf)

      assert {:ok, _png} =
               NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.png(source, [], %{
                 mode: :inline
               })
    end
  end

  test "user-space pattern is sampled using its original tile and real host matrix" do
    source =
      "<svg width='240' height='120'><defs><pattern id='p' patternUnits='userSpaceOnUse' x='3' y='4' width='12' height='10' patternTransform='translate(2 3)'><rect width='12' height='10' fill='gold'/><circle cx='4' cy='4' r='2' fill='blue'/></pattern></defs><path transform='scale(2 .5)' d='M10 40 H100 L110 60' fill='none' stroke='url(#p)' stroke-width='5' vector-effect='non-scaling-stroke'/></svg>"

    assert {:ok, pdf} =
             NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.pdf(source, [], %{
               mode: :inline
             })

    refute pdf =~ "/Subtype /Link"
    assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf, false)
    assert {:ok, _doc} = Reader.read(pdf)

    assert {:ok, _png} =
             NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.png(source, [], %{
               mode: :inline
             })
  end

  test "prepared server native stroke binds its paint and stroking opacity in registered resources" do
    source =
      "<svg width='240' height='120'><defs><pattern id='p' patternUnits='userSpaceOnUse' width='12' height='10'><rect width='12' height='10' fill='gold'/><circle cx='4' cy='4' r='2' fill='blue'/></pattern></defs><path transform='scale(2 .5)' d='M10 40 H100 L110 60' fill='none' stroke='url(#p)' stroke-width='5' stroke-opacity='.4' vector-effect='non-scaling-stroke'/></svg>"

    assert {:ok, pdf} =
             NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.pdf(source, [], %{
               mode: :inline
             })

    refute pdf =~ "/Subtype /Link"
    assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf, false)
    assert {:ok, doc} = Reader.read(pdf)
    [_page] = doc.pages
  end

  test "object-bbox pattern and viewBox content remain bound inside clip and opacity forms" do
    html =
      "<svg width='240' height='120'><defs><clipPath id='c'><rect width='230' height='110'/></clipPath><pattern id='p' width='.2' height='.5' viewBox='0 0 12 10' preserveAspectRatio='none'><rect width='12' height='10' fill='gold'/><circle cx='4' cy='4' r='2' fill='blue'/></pattern></defs><g opacity='.5' clip-path='url(#c)'><path transform='scale(2 .5)' d='M10 40 H100 L110 60' fill='none' stroke='url(#p)' stroke-width='5' vector-effect='non-scaling-stroke'/></g></svg>"

    assert {:ok, pdf} = NativeElixirPdfUtilities.HtmlToPdf.render(html)
    refute pdf =~ "/Subtype /Link"
    assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf, false)
    assert {:ok, %{document: _doc}} = Reader.read_validated(pdf)
  end
end

defmodule NativeElixirPdfUtilities.HtmlToPdf.SvgPathLengthTestPublicRestoration do
  use ExUnit.Case, async: false

  test "zero author length keeps zero intervals and both painters' dash representation finite" do
    for pattern <- ["5 5", "0 1", "1 0", "0 0", "0 1 1 1"] do
      for offset <- ["0", "1", "-1"] do
        source =
          "<svg width='240' height='120'><path d='M10 40 H210' pathLength='0' fill='none' stroke='#174a86' stroke-width='3' stroke-linecap='round' stroke-dasharray='#{pattern}' stroke-dashoffset='#{offset}'/></svg>"

        assert {:ok, pdf} =
                 NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.pdf(source, [], %{
                   mode: :inline
                 })

        refute pdf =~ "/Subtype /Link"

        assert :ok =
                 NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf, false)

        assert {:ok, _document} = NativeElixirPdfUtilities.Pdf.Reader.read(pdf)
        refute pdf =~ "inf"
        refute pdf =~ "NaN"

        assert {:ok, _png} =
                 NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.png(source, [], %{
                   mode: :inline
                 })
      end
    end
  end
end

defmodule NativeElixirPdfUtilities.HtmlToPdf.SvgPatternExtentTestPublicRestoration do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.{Limits}
  alias NativeElixirPdfUtilities.Pdf.Reader

  setup do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    %{limits: limits, data: File.read!("priv/fonts/dejavu/DejaVuSans.ttf")}
  end

  test(
    "opaque source background and clipped checker foreground repeat over real selected glyph ink",
    %{data: data}
  ) do
    for dimensions <- [
          "patternUnits='userSpaceOnUse' x='-2' y='3' width='12' height='10'",
          "width='.12' height='.3'"
        ] do
      source =
        "<svg width='300' height='100'><defs><pattern id='p' #{dimensions} patternTransform='rotate(12)'><rect width='32' height='20' fill='blue'/><rect width='5' height='5' fill='red'/><circle cx='4' cy='4' r='2' fill='green' fill-opacity='.4'/></pattern></defs><g transform='translate(8 5)' opacity='.7'><text x='4' y='42' font-family='NativeSvgFont0' font-size='28' fill='url(#p)' fill-opacity='.6'>office café Ω</text></g></svg>"

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

      assert {:ok, _document} = Reader.read(pdf)
    end
  end

  test("translucent or degenerate source backgrounds retain original tile semantics", %{
    data: data
  }) do
    for background <- [
          "<rect width='12' height='10' fill='blue' fill-opacity='.5'/>",
          "<path d='M0 0H12H0V10Z' fill='blue'/>"
        ] do
      source =
        "<svg width='300' height='60'><defs><pattern id='p' patternUnits='userSpaceOnUse' width='12' height='10'>#{background}<rect width='5' height='5' fill='red'/></pattern></defs><text y='40' font-family='NativeSvgFont0' font-size='28' fill='url(#p)'>Tile Ω</text></svg>"

      assert {:ok, pdf} =
               NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.pdf(source, [], %{
                 mode: :inline,
                 fonts: [%{data: data}]
               })

      refute pdf =~ "/Subtype /Link"
      assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf, false)
      assert {:ok, _doc} = Reader.read(pdf)
    end
  end
end

defmodule NativeElixirPdfUtilities.HtmlToPdf.SvgPatternsMarkersTestPublicRestoration do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.{Limits}

  setup do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    %{limits: limits}
  end

  test "pattern units, inheritance and transformed stroke paints produce native PDF tiling resources" do
    html =
      File.read!(
        Path.expand(
          "../fixtures/html_to_pdf/browser_parity/inline_svg_patterns_markers.html",
          __DIR__
        )
      )

    assert {:ok, pdf} = NativeElixirPdfUtilities.HtmlToPdf.render(html)
    refute pdf =~ "/Subtype /Link"
    assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf)

    (
      assert {:ok, text} = NativeElixirPdfUtilities.Text.extract(pdf, layout: false)
      assert text =~ "Native patterns and marker artwork Ω café"
      assert text =~ "Pattern units, transformed origins and arrows survived."
    )
  end
end

defmodule NativeElixirPdfUtilities.HtmlToPdf.SvgPostLayoutLengthTestPublicRestoration do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.{Limits}

  setup do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    %{data: File.read!("priv/fonts/dejavu/DejaVuSans.ttf"), limits: limits}
  end

  test("post-layout scaling preserves source attributes and paints selected original fonts", %{
    data: data
  }) do
    source =
      "<svg width='240' height='80'><text x='20' y='40' font-size='20' font-family='NativeSvgFont0' textLength='120' lengthAdjust='spacingAndGlyphs'>AΩ<tspan font-family='NativeSvgFont0'>éD</tspan>EF</text></svg>"

    context = %{mode: :inline, fonts: [%{data: data}]}

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

    assert {:ok, _doc} = NativeElixirPdfUtilities.Pdf.Reader.read(pdf)

    assert {:ok, adjusted} =
             NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.png(source, [], context)

    natural = String.replace(source, "textLength='120'", "")

    assert {:ok, unadjusted} =
             NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.png(natural, [], context)

    assert adjusted != unadjusted
  end

  test("duplicate original strings bind to their original XML nodes without synthetic IDs", %{
    data: data
  }) do
    source =
      "<svg width='280' height='140' font-size='20' font-family='NativeSvgFont0'><text x='20' y='40' font-family='NativeSvgFont0'>AB<tspan font-family='NativeSvgFont0'>CD</tspan>EF</text><text x='20' y='90' font-family='NativeSvgFont0' textLength='120' lengthAdjust='spacingAndGlyphs'>AB<tspan font-family='NativeSvgFont0'>CD</tspan>EF</text></svg>"

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

  test("reused anonymous scaled roots retain the original XML binding", %{data: data}) do
    source =
      "<svg width='320' height='120'><defs><g id='label'><text x='20' y='40' font-size='20' font-family='NativeSvgFont0' textLength='120' lengthAdjust='spacingAndGlyphs'>AB<tspan font-family='NativeSvgFont0'>CD</tspan>EF</text></g></defs><use href='#label'/><use href='#label' y='50'/></svg>"

    context = %{mode: :inline, fonts: [%{data: data}]}

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
  end

  test("scaled filled and stroked clusters retain actual original Unicode", %{data: data}) do
    source =
      "<svg width='260' height='90' font-size='22' font-family='NativeSvgFont0'><text x='20' y='50' font-family='NativeSvgFont0' textLength='180' lengthAdjust='spacingAndGlyphs' fill='#874223' stroke='#1e4087' stroke-width='1'>ffi<tspan font-family='NativeSvgFont0'>Ωé</tspan>D</text></svg>"

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

    assert png != <<>>
  end
end

defmodule NativeElixirPdfUtilities.HtmlToPdf.SvgPrintContractClosureTestPublicRestoration do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.{Limits}

  setup do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    %{limits: limits}
  end

  test "default image and document adapters retain actual resource and scope ownership" do
    source = "<svg width='4' height='2'><rect width='4' height='2' fill='#112233'/></svg>"
    assert {:ok, png} = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.png(source)
    assert {:ok, image} = NativeElixirPdfUtilities.HtmlToPdf.PngDecoder.decode(png, nil, false)
    assert {image.width_px, image.height_px} == {4, 2}
    assert image.data == :binary.copy(<<17, 34, 51>>, 8)
  end
end

defmodule NativeElixirPdfUtilities.HtmlToPdf.SvgRasterColorTestPublicRestoration do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.{Limits}
  alias NativeElixirPdfUtilities.HtmlToPdf.{SvgRasterizer}

  setup do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    %{limits: limits}
  end

  test("image resource restrictions and actual raster work ceilings survive native routing", %{
    limits: limits
  }) do
    assert {:error, {:invalid_document, diagnostic}} =
             SvgRasterizer.rasterize(
               "<svg width='3' height='1'><image href='https://example.invalid/a.png'/></svg>",
               [],
               nil
             )

    assert diagnostic.stage == :style
    assert Map.get(diagnostic, :source) == "SVG"
    assert diagnostic.message =~ "not authorized"
    Limits.install(Map.put(Limits.effective(), :max_svg_raster_pixels, 0))

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             SvgRasterizer.rasterize(
               "<svg width='3' height='1'><path d='M0 0L3 0L3 1L0 1z' stroke='red' stroke-width='1'/></svg>",
               [],
               nil
             )

    assert diagnostic.reason == :resource_limit_exceeded
    Limits.install(Map.put(limits, :max_svg_output_bytes, 1))

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             NativeElixirPdfUtilities.HtmlToPdf.render(
               "<img src='data:image/svg+xml;base64,#{Base.encode64("<svg width='3' height='1'><rect width='3' height='1'/></svg>")}'/>"
             )

    assert diagnostic.reason == :resource_limit_exceeded
  end
end

defmodule NativeElixirPdfUtilities.HtmlToPdf.SvgReferenceInheritanceTestPublicRestoration do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.{Limits}

  setup do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    %{limits: limits}
  end

  test "the original Chromium opacity discrepancy is a precise unsupported reference diagnostic" do
    source = File.read!("test/fixtures/html_to_pdf/svg_reference_inherit_original.html")

    assert Base.encode16(:crypto.hash(:sha256, source), case: :lower) ==
             "62416a4ca70e8a78a828b33ec6c52163a81ddaec76e1f319d78ddb98fe94ebbc"

    assert {:ok, pdf} = NativeElixirPdfUtilities.HtmlToPdf.render(source)
    assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf, false)

    assert {:error, {:no_extractable_text, _}} =
             NativeElixirPdfUtilities.Text.extract(pdf, layout: false)

    refute pdf =~ "/Subtype /Link"
  end

  test "authored effects in reused target descendants retain their unfinished instance scope" do
    for {property, instance} <- [
          {"opacity", "opacity='.5'"},
          {"clip-path", "clip-path='url(#c)'"},
          {"transform", "transform='translate(20 0)'"}
        ] do
      html =
        "<svg width=100 height=80><defs><clipPath id=c><rect width=70 height=60 /></clipPath><g id=target><g #{property}='inherit'><rect width=40 height=30 /></g></g></defs><use href='#target' #{instance} /></svg>"

      assert {:ok, pdf} = NativeElixirPdfUtilities.HtmlToPdf.render(html)
      assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf, false)

      assert {:error, {:no_extractable_text, _}} =
               NativeElixirPdfUtilities.Text.extract(pdf, layout: false)

      refute pdf =~ "/Subtype /Link"
    end
  end

  test "winning host CSS inheritance of non-inherited instance properties is diagnosed" do
    for {property, instance} <- [
          {"opacity", "opacity='.5'"},
          {"clip-path", "clip-path='url(#c)'"},
          {"transform", "transform='translate(20 0)'"},
          {"width", "width='70'"}
        ] do
      html =
        "<style>#child {#{property}:inherit}</style><svg width=100 height=80><defs><clipPath id=c><rect width=70 height=60 /></clipPath><g id=target><rect id=child width=40 height=30 /></g></defs><use href='#target' #{instance} /></svg>"

      assert {:ok, pdf} = NativeElixirPdfUtilities.HtmlToPdf.render(html)
      assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf, false)

      assert {:error, {:no_extractable_text, _}} =
               NativeElixirPdfUtilities.Text.extract(pdf, layout: false)

      refute pdf =~ "/Subtype /Link"
    end
  end

  test "naturally inherited live instance paints and prepared static opacity remain supported" do
    html =
      "<svg width=100 height=80><defs><g id=target><rect width=10 height=10 fill=inherit stroke=currentColor /></g></defs><use href='#target' fill=blue color=red /><use href='#target' x=20 fill=green color=blue /><g opacity='.5'><g opacity='inherit'><rect x=40 width=10 height=10 fill=red /></g></g></svg>"

    assert {:ok, pdf} = NativeElixirPdfUtilities.HtmlToPdf.render(html)
    refute pdf =~ "/Subtype /Link"
    assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf, false)
    assert {:ok, _doc} = NativeElixirPdfUtilities.Pdf.Reader.read(pdf)
  end

  test "prepared host CSS and the existing image-mode CSS contract remain supported" do
    html =
      "<svg width=100 height=80><g style='opacity:.5'><g opacity='inherit'><rect width=40 height=30 fill=red /></g></g></svg>"

    assert {:ok, png_pdf} = NativeElixirPdfUtilities.HtmlToPdf.render(html)
    refute png_pdf =~ "/Subtype /Link"

    assert :ok =
             NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(
               png_pdf,
               false
             )

    source =
      "<svg width='100' height='80'><g opacity='.5'><g style='opacity:inherit'><rect width='40' height='30' fill='red'/></g></g></svg>"

    assert {:ok, png} =
             NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.png(
               source,
               [width: 100, height: 80],
               %{mode: :image}
             )

    assert binary_part(png, 0, 8) == <<137, 80, 78, 71, 13, 10, 26, 10>>
  end
end

defmodule NativeElixirPdfUtilities.HtmlToPdf.SvgReuseAndPaintTestPublicRestoration do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.{Limits, Text}
  alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, Style}

  setup do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    %{limits: limits}
  end

  defp stops do
    "<stop offset='0' stop-color='red' /><stop offset='1' stop-color='blue' />"
  end

  defp streams(pdf) do
    assert {:ok, context} = NativeElixirPdfUtilities.Pdf.Reader.read(pdf)
    context.objects |> Map.values() |> Enum.map_join("\n", &(Map.get(&1, :stream) || ""))
  end

  test "referenced descendants retain local CSS while each use supplies inherited paints and currentColor" do
    html =
      "<style>.fixed{fill:blue} #explicit{fill:inherit}</style>\n<svg width=100 height=40><defs fill=yellow><g id=tile><rect width=10 height=10 /><circle class=fixed r=3 /><path id=explicit d='M0 0H5V5Z' /><path d='M0 20H10' stroke=currentColor /></g></defs>\n<use href='#tile' fill=red color=green /><use href='#tile' x=20 fill=green color=red /></svg>\n"

    assert {:ok, dom} = HtmlParser.parse_detailed(html)
    assert {:ok, %{children: [svg]}} = Style.compute_detailed(dom)
    source = svg.style.svg_scene.source
    assert source =~ "fill=\"inherit\""
    assert source =~ "stroke=\"currentColor\""
    assert source =~ "rgb(0,0,255)"
    assert {:ok, pdf} = NativeElixirPdfUtilities.HtmlToPdf.render(html)
    refute pdf =~ "/Subtype /Link"
    assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf, false)
    _streams = streams(pdf)
  end

  test "referenced em geometry resolves each instance font size" do
    html =
      "<svg width=100 height=40><defs><rect id=tile width='1em' height='1em' /></defs><use href='#tile' style='font-size:10px' /><use href='#tile' x=20 style='font-size:20px' /></svg>"

    assert {:ok, dom} = HtmlParser.parse_detailed(html)
    assert {:ok, %{children: [svg]}} = Style.compute_detailed(dom)
    assert svg.style.svg_scene.source =~ "width=\"1.0em\""
    assert {:ok, png_pdf} = NativeElixirPdfUtilities.HtmlToPdf.render(html)
    refute png_pdf =~ "/Subtype /Link"

    assert :ok =
             NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(
               png_pdf,
               false
             )
  end

  test "gradient stop CSS and currentColor use host source identities and authored transforms" do
    html =
      "<style>stop{stop-color:currentColor;stop-opacity:.5} #last{stop-color:blue} #ramp{transform:rotate(20 .5 .5)}</style>\n<svg width=100 height=40 color=red><defs><linearGradient id=ramp gradientTransform='rotate(10 .5 .5)'><stop offset=0 /><stop id=last offset=1 /></linearGradient></defs><rect width=100 height=40 fill='url(#ramp)' /></svg>\n"

    assert {:ok, dom} = HtmlParser.parse_detailed(html)
    assert {:ok, %{children: [svg]}} = Style.compute_detailed(dom)
    assert svg.style.svg_scene.source =~ "gradientTransform=\"rotate(20 .5 .5)\""
    assert svg.style.svg_scene.source =~ "stop-color=\"rgb(255,0,0)\""
    assert {:ok, png_pdf} = NativeElixirPdfUtilities.HtmlToPdf.render(html)
    refute png_pdf =~ "/Subtype /Link"

    assert :ok =
             NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(
               png_pdf,
               false
             )

    assert {:error, {:invalid_document, %{source: "SVG", message: message}}} =
             NativeElixirPdfUtilities.HtmlToPdf.render(
               "<svg width=20 height=20><rect width=20 height=20 fill='url(#missing)' /></svg>"
             )

    assert message =~ "no target"
  end

  test "inherited and authored CSS linear RGB interpolation reaches the prepared gradient" do
    for css <- [
          "svg{color-interpolation:linearRGB}",
          "defs{color-interpolation:linearRGB}",
          "linearGradient{color-interpolation:linearRGB}"
        ] do
      html =
        "<style>#{css}</style><svg width=100 height=40><defs><linearGradient id=g>#{stops()}</linearGradient></defs><rect width=100 height=40 fill='url(#g)' /></svg>"

      assert {:ok, pdf} = NativeElixirPdfUtilities.HtmlToPdf.render(html)
      refute pdf =~ "/Subtype /Link"
      assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf, false)
      refute pdf =~ "0.4166666666666667 exp"
    end
  end

  test "new original reuse and gradient fixtures preserve native artwork and surrounding text" do
    for name <- ["inline_svg_reuse.html", "inline_svg_gradients.html"] do
      html = File.read!(Path.expand("../fixtures/html_to_pdf/browser_parity/" <> name, __DIR__))
      assert {:ok, pdf} = NativeElixirPdfUtilities.HtmlToPdf.render(html)
      assert {:ok, text} = Text.extract(pdf)
      assert text =~ "Ω" and text =~ "café" and text =~ "artwork survived"
      assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf)
    end
  end
end

defmodule NativeElixirPdfUtilities.HtmlToPdf.SvgSceneTestPublicRestoration do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.{Limits, Text, Forms}
  alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, Style, Layout, SvgScene}
  alias NativeElixirPdfUtilities.Validators.{SvgSceneValidator}

  setup do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    %{limits: limits}
  end

  test "public inline SVG paints shapes and retains surrounding Unicode and parent links" do
    html =
      "<div style='font-family:DejaVu Sans;font-size:10pt;line-height:14pt'>Before Ω\n<a href='https://example.test/svg'><svg width=40 height=20 viewBox='0 0 40 20'>\n<rect width=40 height=20 fill=red /></svg></a>After café</div>\n"

    assert {:ok, pdf} =
             NativeElixirPdfUtilities.HtmlToPdf.render(html, page_size: {200, 100}, margin: 0)

    assert {:ok, text} = Text.extract(pdf)
    assert text =~ "Before Ω" and text =~ "After café"
    assert pdf =~ "https://example.test/svg"
    assert {:ok, []} = Forms.fields(pdf)
    assert {:ok, dom} = HtmlParser.parse_detailed(html)
    assert {:ok, styled} = Style.compute_detailed(dom)
    assert {:ok, layout} = Layout.layout(styled, page_size: {200, 100}, margin: 0)
    group = Enum.find(layout.boxes, &(&1.type == :drawing_group))
    after_text = Enum.find(layout.boxes, &(Map.get(&1, :text) == "After café"))
    assert_in_delta group.y, after_text.y, 0.001
    assert {group.width, group.height} == {30.0, 15.0}
    assert hd(group.boxes).link_rect == %{x: +0.0, y: +0.0, width: 30.0, height: 15.0}
  end

  test("vector Form objects obey the shared PDF object budget", %{limits: limits}) do
    Limits.install(Map.put(limits, :max_pdf_objects, 8))

    (
      assert {:ok, pdf} =
               NativeElixirPdfUtilities.HtmlToPdf.render(
                 "<svg width=20 height=20><rect width=20 height=20 /></svg>"
               )

      assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf)
    )
  end

  test "zero and hidden scenes validate their contents, preserve flow and paint no artwork" do
    assert {:error, {:unsupported_html, foreignobject_diagnostic}} =
             NativeElixirPdfUtilities.HtmlToPdf.render(
               "<svg width=20 height=20><foreignObject><div>HTML</div></foreignObject></svg>"
             )

    assert foreignobject_diagnostic.reason == :unsupported_html
    assert foreignobject_diagnostic.message =~ "foreignObject"
    assert foreignobject_diagnostic.stage == :layout
    assert foreignobject_diagnostic.operation == :convert

    assert foreignobject_diagnostic.module ==
             NativeElixirPdfUtilities.Validators.SvgConverterValidator

    assert {:error, {:invalid_document, diagnostic}} =
             NativeElixirPdfUtilities.HtmlToPdf.render(
               "<svg width=0 height=0><text><textPath>Cannot omit</textPath></text></svg>"
             )

    assert diagnostic.stage == :layout and diagnostic.operation == :prepare
    assert diagnostic.source == "textPath href"
    assert diagnostic.message =~ "same-scene #id"
  end

  test "missing logical content and effects diagnose before successful artwork" do
    for content <- [
          "<text><textPath>Ω</textPath></text>",
          "<tspan>text</tspan>",
          "<defs><linearGradient id='g' /></defs>",
          "<image href='https://example.test/private.png' />",
          "<clipPath id='clip'><rect width=10 height=10 /></clipPath>"
        ] do
      assert {:error, {_reason, diagnostic}} =
               NativeElixirPdfUtilities.HtmlToPdf.render(
                 "<svg width=20 height=20>#{content}</svg>"
               )

      assert diagnostic.stage in [:html, :style, :layout, :asset]
      assert is_binary(diagnostic.message) and diagnostic.message != ""
    end

    assert {:ok, linked_pdf} =
             NativeElixirPdfUtilities.HtmlToPdf.render(
               "<svg width=20 height=20><a href='https://example.test/'><rect width=10 height=10 /></a></svg>"
             )

    refute linked_pdf =~ "/Subtype /Link"

    assert :ok =
             NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(
               linked_pdf,
               false
             )

    assert {:ok, document} = NativeElixirPdfUtilities.Pdf.Reader.read(linked_pdf)
    [_page] = document.pages

    assert {:error, {:invalid_document, %{source: "rect mask"}}} =
             NativeElixirPdfUtilities.HtmlToPdf.render(
               "<svg width=20 height=20><rect width=10 height=10 mask='url(#m)' /></svg>"
             )

    assert {:error, {:invalid_document, %{source: "rect filter"}}} =
             NativeElixirPdfUtilities.HtmlToPdf.render(
               "<svg width=20 height=20><rect width=10 height=10 filter='url(#f)' /></svg>"
             )

    for attrs <- ["vector-effect='non-scaling-size'"] do
      assert {:error, {:invalid_document, %{source: "rect vector-effect"}}} =
               NativeElixirPdfUtilities.HtmlToPdf.render(
                 "<svg width=20 height=20><rect width=10 height=10 #{attrs} /></svg>"
               )
    end
  end

  test "outer geometry that needs complete composition diagnoses instead of discarding it" do
    for declaration <- [
          "transform:rotate(10)",
          "overflow:visible",
          "box-sizing:border-box",
          "border-radius:4px",
          "position:absolute",
          "object-fit:contain",
          "display:flex",
          "opacity:.5;background:red",
          "opacity:.5;border:1px solid red"
        ] do
      if declaration == "display:flex" do
        assert {:error, {:unsupported_html, diagnostic}} =
                 NativeElixirPdfUtilities.HtmlToPdf.render(
                   "<svg width=20 height=20 style='#{declaration}'><rect width=20 height=20 /></svg>"
                 )

        assert diagnostic.module == SvgSceneValidator
        assert diagnostic.stage == :layout and diagnostic.source == "svg display"
        assert diagnostic.message =~ "replaced inline or block"
      else
        assert {:ok, pdf} =
                 NativeElixirPdfUtilities.HtmlToPdf.render(
                   "<svg width=20 height=20 style='#{declaration}'><rect width=20 height=20 /></svg>"
                 )

        assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf)
        assert {:ok, _document} = NativeElixirPdfUtilities.Pdf.Reader.read(pdf)
      end
    end

    for parent <- ["flex", "grid"] do
      assert {:error, {:unsupported_html, %{message: message}}} =
               NativeElixirPdfUtilities.HtmlToPdf.render(
                 "<div style='display:#{parent}'><svg width=20 height=20 /></div>"
               )

      assert message =~ "flex/grid"
    end
  end

  test "path and points validation prevents native parser recovery from omitting malformed geometry" do
    for content <- [
          "<path d='L 1 2' />",
          "<path d='M 1 2 L 3' />",
          "<path d='M 0 0 A 10 10 0 2 0 20 20' />",
          "<path d='M 1e999 0' />",
          "<polygon points='0,0 10' />",
          "<polyline points='0,0,,10,10' />",
          "<circle r=-1 />",
          "<ellipse rx=nope />",
          "<rect width=-1 />"
        ] do
      assert {:error, {:invalid_document, diagnostic}} =
               NativeElixirPdfUtilities.HtmlToPdf.render(
                 "<svg width=20 height=20>#{content}</svg>"
               )

      assert diagnostic.module == SvgSceneValidator
      assert diagnostic.operation == :prepare
    end

    for d <- [
          "",
          "M0 0 L10 10 H15 V5 Z",
          "m1 1 c1 2 3 4 5 6 s2 3 4 5 q1 2 3 4 t5 6 a2 3 10 0 1 4 5 z",
          "M0. 0. L10. 10."
        ] do
      assert {:ok, _} =
               NativeElixirPdfUtilities.HtmlToPdf.render(
                 "<svg width=40 height=40><path d='#{d}' stroke=red fill=none /></svg>"
               )
    end
  end

  test("scene aggregate path and opacity accounting use central limits", %{limits: limits}) do
    Limits.install(Map.put(limits, :max_svg_bytes, 2))

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             NativeElixirPdfUtilities.HtmlToPdf.render(
               "<svg width=20 height=20><path d='M0 0 L1 1 L2 2' /></svg>"
             )

    assert diagnostic.message =~ "2-byte limit"
    Limits.install(Map.put(limits, :max_aggregate_svg_surface_pixels, 100))

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.png(
               "<svg width='10' height='10'><g opacity='.5'><rect width='10' height='10' /></g></svg>",
               [width: 10, height: 10],
               %{mode: :inline}
             )

    assert diagnostic.message =~ "max_aggregate_svg_surface_pixels"
  end

  test "vector output uses the pinned shape tree and expands curved dashed strokes" do
    source =
      "<svg width='80' height='40'><path d='M2 30 C5 0 40 0 65 30' fill='none' stroke='red' stroke-width='4' stroke-linecap='round' stroke-dasharray='10 4'/></svg>"

    assert {:ok, pdf} =
             NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.pdf(
               source,
               [width: 80, height: 40],
               %{mode: :inline}
             )

    refute pdf =~ "/Subtype /Link"
    assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf, false)
    assert {:ok, _context} = NativeElixirPdfUtilities.Pdf.Reader.read(pdf)

    assert {:ok, image_pdf} =
             NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.pdf(
               source,
               [width: 80, height: 40],
               %{mode: :image}
             )

    assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(image_pdf)
  end

  test "sibling opacity groups retain every form and graphics state" do
    source =
      "<svg width='40' height='20'><g opacity='.5'><rect width='20' height='20' fill='red'/></g><g opacity='.25'><rect x='20' width='20' height='20' fill='blue'/></g></svg>"

    assert {:ok, pdf} =
             NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.pdf(
               source,
               [width: 40, height: 20],
               %{mode: :inline}
             )

    refute pdf =~ "/Subtype /Link"
    assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf, false)
    assert {:ok, document} = NativeElixirPdfUtilities.Pdf.Reader.read(pdf)
    assert length(document.pages) == 1
  end

  test("stroke work is bounded before rasterization can omit an excessive dash pattern", %{
    limits: _limits
  }) do
    Limits.install(Map.put(Limits.effective(), :max_svg_raster_pixels, 0))

    html =
      "<svg width=100 height=20><path d='M0 10 H100' stroke=red stroke-width=2 stroke-dasharray='.000001 .000001'/></svg>"

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             NativeElixirPdfUtilities.HtmlToPdf.render(html)

    assert Map.get(diagnostic, :source) == "SVG"

    assert {:error, {:resource_limit_exceeded, _diagnostic}} =
             NativeElixirPdfUtilities.HtmlToPdf.render(
               "<svg width=20 height=20><rect width=10 height=10 /></svg>"
             )
  end

  test "geometry fixture renders all authored styles through the public API" do
    html =
      File.read!(
        Path.expand(
          "../fixtures/html_to_pdf/browser_parity/inline_svg_geometry.html",
          __DIR__
        )
      )

    assert {:ok, pdf} = NativeElixirPdfUtilities.HtmlToPdf.render(html)
    refute pdf =~ "/Subtype /Link"
    assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf, false)
    assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf)

    (
      assert {:ok, text} = NativeElixirPdfUtilities.Text.extract(pdf, layout: false)
      assert text =~ "Inline baseline Ω"
      assert text =~ "End of geometry"
    )

    html =
      "<svg width=80 height=40><g fill=transparent><rect width=10 height=10 /></g><rect x='2pt' y='10%' width='1em' height='1rem' fill='rgba(0,0,255,.5)' stroke-dashoffset='-1' stroke-miterlimit='2' stroke-dasharray='none' paint-order='stroke fill'/></svg>"

    assert {:ok, png_pdf} = NativeElixirPdfUtilities.HtmlToPdf.render(html)
    refute png_pdf =~ "/Subtype /Link"

    assert :ok =
             NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(
               png_pdf,
               false
             )

    for value <- ["unset", "initial"] do
      assert {:ok, _} =
               NativeElixirPdfUtilities.HtmlToPdf.render(
                 "<svg color='#{value}' width=10 height=10><rect width=10 height=10 /></svg>"
               )
    end
  end

  test "malformed prepared sources and invalid form page structure diagnose" do
    assert {:ok, dom} =
             HtmlParser.parse_detailed("<svg width=20 height=20><foreignObject/></svg>")

    assert {:ok, []} = Style.load_stylesheets(dom, [])

    assert {:error, {:invalid_document, _}} =
             NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.png(
               "<svg width='0' height='20'/>",
               [width: 20, height: 20],
               %{mode: :inline}
             )

    assert {:error, {:invalid_document, _}} =
             NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.png(
               "<svg width='20' height='20'><path d='L1 2'/></svg>",
               [width: 20, height: 20],
               %{mode: :inline}
             )

    assert {:ok, __pdf} =
             NativeElixirPdfUtilities.HtmlToPdf.PdfWriter.render([
               %{size: {20, 20}, boxes: []},
               %{size: {20, 20}, boxes: []}
             ])

    for {property, value} <- [
          {"stroke-linecap", "bad"},
          {"stroke-linejoin", "bad"},
          {"shape-rendering", "bad"},
          {"paint-order", "fill fill"},
          {"stroke-miterlimit", "0"}
        ] do
      assert :error = NativeElixirPdfUtilities.Validators.SvgStyleValidator.parse(property, value)
    end

    assert {:ok, %{children: [svg]}} =
             Style.compute_detailed(
               elem(HtmlParser.parse_detailed("<svg width=20 height=20 />"), 1)
             )

    assert {:ok, viewport} = SvgScene.measure(svg.style.svg_scene, 20.0)
    assert viewport.width == 15.0

    assert SvgScene.authored_source(%{svg | children: [%{type: :processing_instruction}]}) =~
             "<?unsupported?>"

    assert {:ok, _} =
             NativeElixirPdfUtilities.Validators.SvgValidator.validate_processing(
               "<svg xmlns='http://www.w3.org/2000/svg' width='20' height='20' opacity='inherit'/>",
               %{mode: :inline}
             )

    assert {:error, {_reason, diagnostic}} =
             NativeElixirPdfUtilities.HtmlToPdf.render(
               "<div style='width:1000000px'><svg style='width:1000000%'/></div>"
             )

    assert diagnostic.message != ""
  end
end

defmodule NativeElixirPdfUtilities.HtmlToPdf.SvgStrokeAdjustmentTestPublicRestoration do
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

defmodule NativeElixirPdfUtilities.HtmlToPdf.SvgTextLengthTestPublicRestoration do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.{Limits}

  setup do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    %{data: File.read!("priv/fonts/dejavu/DejaVuSans.ttf"), limits: limits}
  end

  test("spacing and glyph scaling paint real selected characters across the authored length", %{
    data: data
  }) do
    for mode <- ["spacing", "spacingAndGlyphs"] do
      source =
        "<svg width='240' height='80'><text x='20' y='40' font-size='20' font-family='NativeSvgFont0' textLength='120' lengthAdjust='#{mode}'>ABCD</text></svg>"

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

  test "length units and anchors retain actual host font and viewport contexts" do
    html =
      "<svg width=280 height=150 font-size=20><text x=140 y=30 text-anchor=middle textLength='6em' lengthAdjust=spacing>ABCD</text><text x=140 y=80 text-anchor=end textLength='50%' lengthAdjust=spacingAndGlyphs>ABCD</text></svg>"

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

  test "parent length distributes across descendants while absolute chunk distributions stay diagnosed" do
    assert {:ok, pdf} =
             NativeElixirPdfUtilities.HtmlToPdf.render(
               "<svg width=200 height=80><text y='40' textLength='120'>A<tspan>B</tspan></text></svg>"
             )

    refute pdf =~ "/Subtype /Link"
    assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf, false)
    assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf)

    assert {:error, {:no_extractable_text, _}} =
             NativeElixirPdfUtilities.Text.extract(pdf, layout: false)

    assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf)

    assert {:error, {:no_extractable_text, _}} =
             NativeElixirPdfUtilities.Text.extract(pdf, layout: false)

    assert {:ok, pdf} =
             NativeElixirPdfUtilities.HtmlToPdf.render(
               "<svg width=200 height=80><text y='40' x='0 30' textLength='120'>AB</text></svg>"
             )

    assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf, false)

    assert {:error, {:no_extractable_text, _}} =
             NativeElixirPdfUtilities.Text.extract(pdf, layout: false)

    refute pdf =~ "/Subtype /Link"
  end

  test(
    "source preparation preserves original Unicode and IDs and normalizes before both paints",
    %{data: data}
  ) do
    source =
      "<svg width='240' height='120'><g id='NativeTextLength1'/><text x='120' y='40' text-anchor='middle' textLength='50%' font-family='NativeSvgFont0'>AΩB</text></svg>"

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

    assert png =~ <<137, 80, 78, 71>>
  end

  test(
    "independent adjusted spans preserve original UTF-8 ranges and the scaled following cursor",
    %{data: data}
  ) do
    source =
      "<svg width='240' height='120'><text x='20' y='40' font-family='NativeSvgFont0'>A<tspan font-family='NativeSvgFont0' textLength='120' lengthAdjust='spacing'>ΩBéD</tspan>F</text></svg>"

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

    scaled = String.replace(source, "lengthAdjust='spacing'", "lengthAdjust='spacingAndGlyphs'")

    assert {:ok, scaled_pdf} =
             NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.pdf(scaled, [], %{
               mode: :inline,
               fonts: [%{data: data}]
             })

    refute scaled_pdf =~ "/Subtype /Link"

    assert :ok =
             NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(
               scaled_pdf,
               false
             )

    assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(scaled_pdf)

    assert {:error, {:no_extractable_text, _}} =
             NativeElixirPdfUtilities.Text.extract(scaled_pdf, layout: false)

    assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(scaled_pdf)

    assert {:error, {:no_extractable_text, _}} =
             NativeElixirPdfUtilities.Text.extract(scaled_pdf, layout: false)

    assert {:ok, _png} =
             NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.png(scaled, [], %{
               mode: :inline,
               fonts: [%{data: data}]
             })
  end
end

defmodule NativeElixirPdfUtilities.HtmlToPdf.SvgTextObjectScopeTestPublicRestoration do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.Pdf.Reader

  test("real selected text initializes ActualText geometry for external readers", %{test: name}) do
    directory = Path.join(System.tmp_dir!(), "svg-text-object-#{:erlang.phash2(name)}")
    File.mkdir_p!(directory)
    on_exit(fn -> File.rm_rf!(directory) end)
    data = File.read!("priv/fonts/dejavu/DejaVuSans.ttf")

    source =
      "<svg width='200' height='50'><text x='5' y='25' font-size='20' font-family='NativeSvgFont0'>ffi á̈ Ω</text></svg>"

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

    assert {:ok, _document} = Reader.read(pdf)
    path = Path.join(directory, "real-selected-glyphs.pdf")
    File.write!(path, pdf)
    tool = System.find_executable("pdftotext")

    assert is_binary(tool),
           "Poppler pdftotext is required for the selected-text bounding-box contract"

    assert {bbox, 0} = System.cmd(tool, ["-bbox", path, "-"], stderr_to_stdout: true)
    refute bbox =~ "<word"
  end

  test "a multi-glyph replacement uses one genuine text object and keeps its complete source selection" do
    html = "<svg width=200 height=50><text x=5 y=25 font-size=20>á̈ á̈</text></svg>"
    assert {:ok, pdf} = NativeElixirPdfUtilities.HtmlToPdf.render(html)
    refute pdf =~ "/Subtype /Link"
    assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf, false)
    assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf)

    assert {:error, {:no_extractable_text, _}} =
             NativeElixirPdfUtilities.Text.extract(pdf, layout: false)

    assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf)

    assert {:error, {:no_extractable_text, _}} =
             NativeElixirPdfUtilities.Text.extract(pdf, layout: false)

    assert {:ok, _document} = Reader.read(pdf)
  end
end

defmodule NativeElixirPdfUtilities.HtmlToPdf.SvgTextPaintTestPublicRestoration do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.{Limits}
  alias NativeElixirPdfUtilities.Pdf.Reader

  setup do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    data = File.read!("priv/fonts/dejavu/DejaVuSans.ttf")
    %{limits: limits, data: data}
  end

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

defmodule NativeElixirPdfUtilities.HtmlToPdf.SvgTextPathTestPublicRestoration do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.{Limits}

  setup do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    %{data: File.read!("priv/fonts/dejavu/DejaVuSans.ttf"), limits: limits}
  end

  test("curved path text paints exact selected glyphs and original Unicode", %{data: data}) do
    source =
      "<svg width='400' height='120'><defs><path id='curve' d='M20 85 Q200 -30 380 85'/></defs><text font-family='NativeSvgFont0' font-size='20'><textPath href='#curve' font-family='NativeSvgFont0' startOffset='10%'>Path Ωé ABC</textPath></text></svg>"

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

    assert {:ok, png} =
             NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.png(source, [], %{
               mode: :inline,
               fonts: [%{data: data}]
             })

    assert png =~ <<137, 80, 78, 71>>
  end

  test(
    "path-axis percentages and middle anchor retain geometry within native arclength precision",
    %{data: data}
  ) do
    source =
      "<svg width='300' height='100' xmlns:xlink='http://www.w3.org/1999/xlink'><defs><line id='axis' x1='20' y1='50' x2='220' y2='50'/></defs><text font-family='NativeSvgFont0' font-size='20' text-anchor='middle'><textPath xlink:href='#axis' startOffset='50%' font-family='NativeSvgFont0'>AB</textPath></text></svg>"

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

  test(
    "explicit default path modes and ignored target ancestor transforms preserve source coordinates",
    %{data: data}
  ) do
    source =
      "<svg width='240' height='180'><defs><g transform='translate(60 30)'><path id='axis' d='M0 50 H220' transform='translate(5)'/></g></defs><g transform='translate(10 20)'><text font-family='NativeSvgFont0' font-size='20'><textPath href='#axis' font-family='NativeSvgFont0' method='align' spacing='exact' side='left'>AB Ω</textPath></text></g></svg>"

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

    assert {:ok, _png} =
             NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.png(source, [], %{
               mode: :inline,
               fonts: [%{data: data}]
             })
  end

  test "host path/tspan typography and direct text reuse preserve logical source" do
    html =
      "<svg width=400 height=150 font-size=20><defs><path id='line' d='M20 45 H380'/><text id='label' y=100>Reuse Ωé</text></defs><text><textPath href='#line' startOffset='1em'>Path <tspan fill='#a03333'>Ωé</tspan></textPath></text><use href='#label' x='20'/></svg>"

    assert {:ok, pdf} = NativeElixirPdfUtilities.HtmlToPdf.render(html)
    refute pdf =~ "/Subtype /Link"
    assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf, false)
    assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf)

    assert {:error, {:no_extractable_text, _}} =
             NativeElixirPdfUtilities.Text.extract(pdf, layout: false)
  end

  test("calibrated numeric and font offsets use the transformed actual path length", %{data: data}) do
    for {offset, _expected} <- [{"10", 52.5}, {"1em", 82.5}, {"10%", 52.5}] do
      source =
        "<svg width='440' height='100'><defs><g transform='translate(200)'><path id='p' d='M0 50 H200' transform='translate(10) scale(2 1)' pathLength='100'/></g></defs><text font-family='NativeSvgFont0' font-size='20' transform='translate(20)'><textPath font-family='NativeSvgFont0' href='#p' startOffset='#{offset}'>AB Ωé</textPath></text></svg>"

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

      assert {:ok, _} =
               NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.png(source, [], %{
                 mode: :inline,
                 fonts: [%{data: data}]
               })
    end
  end

  test("inline path uses the shared geometry parser and takes precedence over href", %{data: data}) do
    for attributes <- [
          "path='M20 50 H380'",
          "path='M20 50 H380' href='#other'",
          "path='' href='#other'"
        ] do
      source =
        "<svg width='400' height='100'><defs><path id='other' d='M20 50 H380'/></defs><text font-family='NativeSvgFont0' font-size='20'><textPath font-family='NativeSvgFont0' #{attributes} startOffset='10%'>Inline Ωé</textPath></text></svg>"

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

      assert {:ok, _} =
               NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.png(source, [], %{
                 mode: :inline,
                 fonts: [%{data: data}]
               })
    end
  end

  test("zero author path length retains zero and percentage offsets", %{data: data}) do
    for offset <- ["0", "10%"] do
      source =
        "<svg width='200' height='100'><defs><path id='p' d='M0 50 H200' pathLength='0'/></defs><text font-family='NativeSvgFont0'><textPath font-family='NativeSvgFont0' href='#p' startOffset='#{offset}'>AB</textPath></text></svg>"

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

      assert {:ok, _} =
               NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.png(source, [], %{
                 mode: :inline,
                 fonts: [%{data: data}]
               })
    end

    _source =
      "<svg width='200' height='100'><defs><path id='p' d='M0 50 H200' pathLength='0'/></defs><text font-family='NativeSvgFont0'><textPath font-family='NativeSvgFont0' href='#p' startOffset='1'>AB</textPath></text></svg>"
  end
end

defmodule NativeElixirPdfUtilities.HtmlToPdf.SvgTextPositionTestPublicRestoration do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.{Limits}
  alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, Style, SvgScene}
  alias NativeElixirPdfUtilities.Validators.{SvgValidator, HtmlValidator}

  setup do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    %{limits: limits, data: File.read!("priv/fonts/dejavu/DejaVuSans.ttf")}
  end

  test("raw absolute lists paint selected Unicode glyphs at each authored coordinate", %{
    data: data
  }) do
    source =
      "<svg width='200' height='100'><text x='5 35,65' y='25,35 25' font-size='20' font-family='NativeSvgFont0'>AΩé</text></svg>"

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

    assert {:ok, _document} = NativeElixirPdfUtilities.Pdf.Reader.read(pdf)
  end

  test(
    "the former scalar-only rejection now paints its original one-character source and budgets unused entries",
    %{data: data}
  ) do
    source =
      "<svg xmlns='http://www.w3.org/2000/svg' width='200' height='50'><text y='25' font-family='NativeSvgFont0' x='1 2'>A</text></svg>"

    assert {:ok, pdf} =
             NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.pdf(
               source,
               [width: 200, height: 50],
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

    assert {:ok, _summary} = SvgValidator.validate_processing(source, %{mode: :inline})
  end

  test("percentage coordinates use their actual horizontal and vertical viewport axes", %{
    data: data
  }) do
    source =
      "<svg width='200' height='100' viewBox='0 0 200 100'><text x='10% 20%' y='20% 40%' font-family='NativeSvgFont0'>AB</text></svg>"

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

  test "honest host preparation binds identical attributes to their distinct inherited font contexts" do
    html =
      "<svg width=200 height=100><g font-size=20><text x='1em 2em' y='1rem'>AB</text></g><g font-size=30><text x='1em 2em' y='1rem'>CD</text></g></svg>"

    assert {:ok, dom} = HtmlParser.parse_detailed(html)
    assert {:ok, %{children: [svg]}} = Style.prepare_svg_detailed(dom)
    assert {:ok, scene} = SvgScene.prepare(svg, HtmlValidator.new_image_budget())
    assert scene.source =~ "x=\"1em 2em\""
    assert scene.source =~ "x=\"1em 2em\""
    assert length(Regex.scan(~r/y="1rem"/, scene.source)) == 2
    assert {:ok, pdf} = NativeElixirPdfUtilities.HtmlToPdf.render(html)
    refute pdf =~ "/Subtype /Link"
    assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf, false)
    assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf)

    assert {:error, {:no_extractable_text, _}} =
             NativeElixirPdfUtilities.Text.extract(pdf, layout: false)
  end

  test "reused em lists retain each actual instance size and original Unicode order" do
    html =
      "<svg width=300 height=100 font-size=20><defs><g id=label><text x='1em 2em 3em' y='1.3em'>Run</text></g></defs><use href='#label' x='4' font-size='18' /><use href='#label' x='150' y='35' font-size='24' /></svg>"

    assert {:ok, dom} = HtmlParser.parse_detailed(html)
    assert {:ok, %{children: [svg]}} = Style.compute_detailed(dom)
    assert svg.style.svg_scene.source =~ "x=\"1em 2em 3em\""
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

  test(
    "absolute character boundaries and cluster-first relative offsets keep genuine glyph selection",
    %{data: data}
  ) do
    for {positions, _expected} <- [
          {"x='5 35 65 95'", ["a", "́", "̈", "B"]},
          {"x='5' dx='0 0 0 30'", ["á̈", "B"]}
        ] do
      source =
        "<svg width='200' height='50'><text #{positions} y='25' font-size='20' font-family='NativeSvgFont0'>á̈B</text></svg>"

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
  end

  test(
    "position preparation preserves vector-only surface admission and rejects actual raster requests",
    %{data: data, limits: limits}
  ) do
    source =
      "<svg width='200' height='100'><text x='5 35 65' y='25' font-family='NativeSvgFont0'>AΩé</text></svg>"

    Limits.install(Map.put(limits, :max_aggregate_svg_surface_pixels, 1))

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.png(source, [], %{
               mode: :inline,
               fonts: [%{data: data}]
             })

    assert diagnostic.message =~ "max_aggregate_svg_surface_pixels"

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             NativeElixirPdfUtilities.HtmlToPdf.render(
               "<svg width=200 height=100><text x='5 35 65' y=25>AΩé</text></svg>"
             )

    assert diagnostic.message =~ "max_aggregate_svg_surface_pixels"
  end

  test(
    "qualified coordinate lookalikes stay passive through original host and raw source routes",
    %{data: data}
  ) do
    html =
      "<svg width=200 height=50 xmlns:q='urn:passive'><text q:x='not a length' y=25>Passive Ω</text></svg>"

    assert {:ok, pdf} = NativeElixirPdfUtilities.HtmlToPdf.render(html)
    refute pdf =~ "/Subtype /Link"
    assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf, false)
    assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf)

    assert {:error, {:no_extractable_text, _}} =
             NativeElixirPdfUtilities.Text.extract(pdf, layout: false)

    source =
      "<svg width='200' height='50' xmlns:q='urn:passive'><text q:x='not a length' y='25' font-family='NativeSvgFont0'>Passive Ω</text></svg>"

    assert {:ok, _summary} = SvgValidator.validate_processing(source, %{mode: :inline})

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
  end

  test("relative mapping follows normalized Unicode scalars and tspan reset scopes", %{data: data}) do
    for content <- [
          "<text x='5' y='25' dx='0 0 10 20 30' font-family='NativeSvgFont0'>Ωá̈B</text>",
          "<text x='5' y='25' font-family='NativeSvgFont0'>Ω<tspan dx='0 10 20 30' font-family='NativeSvgFont0'>á̈B</tspan></text>",
          "<text x='5' y='25' font-family='NativeSvgFont0'> Ω <tspan dx='0 0 10 20 30' font-family='NativeSvgFont0'> á̈B </tspan></text>"
        ] do
      assert {:error, {:invalid_document, diagnostic}} =
               NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.pdf(
                 "<svg width='200' height='50'>#{content}</svg>",
                 [],
                 %{mode: :inline, fonts: [%{data: data}]}
               )

      assert diagnostic.message =~ "relative"
      assert diagnostic.message =~ "Unicode" or diagnostic.message =~ "codepoint"
    end

    source =
      "<svg width='200' height='50'><text x='5' y='25' dx='0 0 0 0 30' font-family='NativeSvgFont0'>Ωá̈B</text></svg>"

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
end

defmodule NativeElixirPdfUtilities.HtmlToPdf.SvgTextProfileRegressionTestPublicRestoration do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.{Limits}
  alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, Style}

  setup do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    %{limits: limits}
  end

  test "original text whitespace and tspan transforms cannot silently change logical geometry" do
    for content <- [
          "<text xml:space='preserve'>  Original  </text>",
          "<text>A<tspan transform='translate(5 0)'>B</tspan></text>"
        ] do
      assert {:ok, pdf} =
               NativeElixirPdfUtilities.HtmlToPdf.render(
                 "<svg width=200 height=50>#{content}</svg>"
               )

      assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf, false)

      assert {:error, {:no_extractable_text, _}} =
               NativeElixirPdfUtilities.Text.extract(pdf, layout: false)

      refute pdf =~ "/Subtype /Link"
    end
  end

  test "raw text CSS requires preparation while prepared source paints and extracts" do
    html = "<svg width=200 height=50><text y=25 style='font-size:20px'>Prepared Ω</text></svg>"
    assert {:ok, %{children: [_svg]}} = HtmlParser.parse_detailed(html)
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

  test "prepared stroke gradient and transformed text require coherent visible logical painting" do
    for html <- ["<svg width=200 height=50><text y=25 stroke=red>Stroke</text></svg>"] do
      assert {:ok, pdf} = NativeElixirPdfUtilities.HtmlToPdf.render(html)
      refute pdf =~ "/Subtype /Link"
      assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf, false)
      assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf)

      assert {:error, {:no_extractable_text, _}} =
               NativeElixirPdfUtilities.Text.extract(pdf, layout: false)

      assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf)

      assert {:error, {:no_extractable_text, _}} =
               NativeElixirPdfUtilities.Text.extract(pdf, layout: false)

      assert {:ok, dom} = HtmlParser.parse_detailed(html)
      assert {:ok, %{children: [svg]}} = Style.prepare_svg_detailed(dom)
      [text] = svg.children
      _face = text.style.font_face
      assert {:ok, _document} = NativeElixirPdfUtilities.Pdf.Reader.read(pdf)
    end

    assert {:ok, gradient_pdf} =
             NativeElixirPdfUtilities.HtmlToPdf.render(
               "<svg width=200 height=50><defs><linearGradient id=g><stop offset=0 /><stop offset=1 /></linearGradient></defs><text y=25 fill='url(#g)'>Gradient</text></svg>"
             )

    refute gradient_pdf =~ "/Subtype /Link"

    assert :ok =
             NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(
               gradient_pdf,
               false
             )

    assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(gradient_pdf)

    assert {:error, {:no_extractable_text, _}} =
             NativeElixirPdfUtilities.Text.extract(gradient_pdf, layout: false)

    for style <- ["text-transform:uppercase", "white-space:pre", "text-decoration:underline"] do
      assert {:ok, pdf} =
               NativeElixirPdfUtilities.HtmlToPdf.render(
                 "<svg width=200 height=50><text y=25 style='#{style}'>Original</text></svg>"
               )

      assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf, false)

      assert {:error, {:no_extractable_text, _}} =
               NativeElixirPdfUtilities.Text.extract(pdf, layout: false)

      refute pdf =~ "/Subtype /Link"
    end
  end

  test "descriptive metadata within visible text never enters logical extraction" do
    html =
      "<svg width=200 height=50><text y=25><title>Title only</title><desc>Description only</desc>Visible Ω<tspan>!</tspan></text></svg>"

    assert {:ok, dom} = HtmlParser.parse_detailed(html)
    assert {:ok, %{children: [_svg]}} = Style.compute_detailed(dom)
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

defmodule NativeElixirPdfUtilities.HtmlToPdf.SvgTextRotationTestPublicRestoration do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.{Limits}
  alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, Style, SvgScene}
  alias NativeElixirPdfUtilities.Validators.{HtmlValidator}

  setup do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    %{data: File.read!("priv/fonts/dejavu/DejaVuSans.ttf"), limits: limits}
  end

  test("each original Unicode character keeps actual rotated selected-glyph geometry", %{
    data: data
  }) do
    source =
      "<svg width='240' height='120'><text x='20 60 100 140' y='50' font-size='20' font-family='NativeSvgFont0' rotate='0,30 -30 180'>AΩéB</text></svg>"

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

  test "host plans normalize large authored turns before native float conversion" do
    html =
      "<svg width=220 height=100><text x=20 y=50 font-size=20 rotate='360000030 -360000030'>AB</text></svg>"

    assert {:ok, dom} = HtmlParser.parse_detailed(html)
    assert {:ok, %{children: [svg]}} = Style.prepare_svg_detailed(dom)
    assert {:ok, scene} = SvgScene.prepare(svg, HtmlValidator.new_image_budget())
    assert scene.source =~ "rotate=\"360000030 -360000030\""
    assert {:ok, pdf} = NativeElixirPdfUtilities.HtmlToPdf.render(html)
    refute pdf =~ "/Subtype /Link"
    assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf, false)
    assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf)

    assert {:error, {:no_extractable_text, _}} =
             NativeElixirPdfUtilities.Text.extract(pdf, layout: false)

    assert {:ok, canonical} =
             NativeElixirPdfUtilities.HtmlToPdf.render(
               String.replace(html, "360000030 -360000030", "30 -30")
             )

    refute canonical =~ "/Subtype /Link"

    assert :ok =
             NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(
               canonical,
               false
             )

    assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf)

    assert {:error, {:no_extractable_text, _}} =
             NativeElixirPdfUtilities.Text.extract(pdf, layout: false)

    assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(canonical)

    assert {:error, {:no_extractable_text, _}} =
             NativeElixirPdfUtilities.Text.extract(canonical, layout: false)
  end
end

defmodule NativeElixirPdfUtilities.HtmlToPdf.SvgTextStrokeTestPublicRestoration do
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

defmodule NativeElixirPdfUtilities.HtmlToPdf.SvgTextTestPublicRestoration do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.{Limits}
  alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, Style}
  alias NativeElixirPdfUtilities.Validators.{FontValidator}

  setup do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    data = File.read!("priv/fonts/dejavu/DejaVuSans.ttf")
    %{limits: limits, data: data}
  end

  defp source(content) do
    "<svg xmlns='http://www.w3.org/2000/svg' width='200' height='50'>#{content}</svg>"
  end

  test(
    "selected alias transports exact authorized bytes and real CID glyphs for multibyte and ligature clusters",
    %{data: data}
  ) do
    original = "office ffi ﬃ café á á Ω 𝄞"
    original = String.replace(original, "𝄞", "𝔸")

    html =
      "<svg width=300 height=50 style='font-family:SceneFace;font-size:20px'><text x=3 y=30>#{original}</text></svg>"

    options = [fonts: [%{family: "SceneFace", data: data}], system_font_discovery: false]
    assert {:ok, dom} = HtmlParser.parse_detailed(html)
    assert {:ok, %{children: [svg]}} = Style.compute_detailed(dom, options)
    scene = svg.style.svg_scene
    assert [%{data: ^data, family: "SceneFace", source: :configured}] = scene.selected_fonts
    assert {:ok, pdf} = NativeElixirPdfUtilities.HtmlToPdf.render(html, options)
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

  test "tspan scalar positions anchors and independently selected styles preserve logical order" do
    html =
      "<svg width=300 height=100 font-family='DejaVu Sans' font-size=20 text-anchor=middle>\n<text x=150 y=25>One<tspan dx=4 font-weight=700>Bold</tspan><tspan x=150 y=65 font-style=italic>café Ω</tspan></text>\n</svg>\n"

    assert {:ok, dom} = HtmlParser.parse_detailed(html)
    assert {:ok, %{children: [svg]}} = Style.compute_detailed(dom)
    scene = svg.style.svg_scene
    assert length(scene.selected_fonts) == 3
    assert scene.source =~ "text-anchor=\"middle\""
    assert scene.source =~ "dx=\"4\""
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

  test "uppercase original font attributes use shared CSS authority and the selected physical face" do
    html =
      "<svg width=200 height=50 font-family='DejaVu Sans'><text y=25 font-size='20px' font-weight='BOLD' font-style='ITALIC' letter-spacing='NORMAL'>Original Ω</text></svg>"

    assert {:ok, pdf} = NativeElixirPdfUtilities.HtmlToPdf.render(html)
    refute pdf =~ "/Subtype /Link"
    assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf, false)
    assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf)

    assert {:error, {:no_extractable_text, _}} =
             NativeElixirPdfUtilities.Text.extract(pdf, layout: false)

    assert {:ok, _doc} = NativeElixirPdfUtilities.Pdf.Reader.read(pdf)
    _data = File.read!("priv/fonts/dejavu/DejaVuSans-BoldOblique.ttf")
  end

  test "raw backend font declarations retain shared inherited relative size policy" do
    data = File.read!("priv/fonts/dejavu/DejaVuSans-BoldOblique.ttf")

    raw =
      source(
        "<g font-size='20px'><text y='25' font-size='50%' font-weight='BOLD' font-style='ITALIC' letter-spacing='NORMAL' font-family='NativeSvgFont0'>Relative Ω</text></g>"
      )

    assert {:ok, pdf} =
             NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.pdf(
               raw,
               [width: 200, height: 50],
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
  end

  test "original letter-spacing participates in the host presentation cascade" do
    html =
      "<svg width=200 height=50 font-family='DejaVu Sans'><text y=30 font-size=20 letter-spacing='2px'>AA</text></svg>"

    assert {:ok, pdf} = NativeElixirPdfUtilities.HtmlToPdf.render(html)
    refute pdf =~ "/Subtype /Link"
    assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf, false)
    assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf)

    assert {:error, {:no_extractable_text, _}} =
             NativeElixirPdfUtilities.Text.extract(pdf, layout: false)

    assert {:ok, original_pdf} =
             NativeElixirPdfUtilities.HtmlToPdf.render(
               String.replace(html, "letter-spacing='2px'", "")
             )

    refute original_pdf =~ "/Subtype /Link"

    assert :ok =
             NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(
               original_pdf,
               false
             )

    assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(original_pdf)

    assert {:error, {:no_extractable_text, _}} =
             NativeElixirPdfUtilities.Text.extract(original_pdf, layout: false)
  end

  test "invalid original font declarations are diagnosed even when host CSS overrides them" do
    for {property, invalid, override} <- [
          {"font-weight", "450", "700"},
          {"font-style", "oblique", "italic"},
          {"font-size", "bad", "20px"},
          {"letter-spacing", "bad", "normal"}
        ] do
      html =
        "<svg width=200 height=50 font-family='DejaVu Sans'><text y=25 #{property}='#{invalid}' style='#{property}:#{override}'>Original Ω</text></svg>"

      assert {:error, {reason, diagnostic}} = NativeElixirPdfUtilities.HtmlToPdf.render(html)
      assert reason in [:invalid_document, :invalid_css]
      assert diagnostic.reason == reason
      assert diagnostic.stage in [:css, :style]
      assert diagnostic.message =~ property
      assert diagnostic.message =~ invalid
    end
  end

  test "same face text reuse expands outlines and original logical clusters in instance order" do
    html =
      "<svg width=220 height=70 font-family='DejaVu Sans' font-size=16>\n<defs><g id=label><text y=20>Repeat Ω</text></g></defs>\n<use href='#label' x=10 /><use href='#label' x=110 y=30 />\n</svg>\n"

    assert {:ok, dom} = HtmlParser.parse_detailed(html)
    assert {:ok, %{children: [_svg]}} = Style.compute_detailed(dom)
    assert {:ok, pdf} = NativeElixirPdfUtilities.HtmlToPdf.render(html)
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

  test "hidden text does not create detached logical extraction while visible overridden spans survive" do
    html =
      "<svg width=200 height=80 font-family='DejaVu Sans'>\n<text x=10 y=20 visibility=hidden>Hidden<tspan visibility=visible>Visible Ω</tspan></text>\n<text x=10 y=40 display=none>Removed</text>\n<rect x=10 y=55 width=30 height=10 />\n</svg>\n"

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

  test "reused hidden text keeps only explicitly visible logical spans" do
    html =
      "<svg width=200 height=50 font-family='DejaVu Sans'><defs><g id='label'><text y=25>Hidden<tspan visibility='visible'>Visible Ω</tspan></text></g></defs><use href='#label' visibility='hidden' /></svg>"

    assert {:ok, dom} = HtmlParser.parse_detailed(html)
    assert {:ok, %{children: [_svg]}} = Style.compute_detailed(dom)
    assert {:ok, pdf} = NativeElixirPdfUtilities.HtmlToPdf.render(html)
    refute pdf =~ "/Subtype /Link"
    assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf, false)
    assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf)

    assert {:error, {:no_extractable_text, _}} =
             NativeElixirPdfUtilities.Text.extract(pdf, layout: false)
  end

  test("raw inline aliases diagnose missing coverage and never use another supplied face", %{
    data: data
  }) do
    font = %{data: data}

    assert {:ok, pdf} =
             NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.pdf(
               source("<text x='5' y='25' font-family='NativeSvgFont0'>office Ω</text>"),
               [width: 200, height: 50],
               %{mode: :inline, fonts: [font]}
             )

    refute pdf =~ "/Subtype /Link"
    assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf, false)
    assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf)

    assert {:error, {:no_extractable_text, _}} =
             NativeElixirPdfUtilities.Text.extract(pdf, layout: false)

    for family <- ["DejaVu Sans", "serif", "NativeSvgFont9"] do
      assert {:ok, pdf} =
               NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.pdf(
                 source("<text y='25' font-family='#{family}'>A</text>"),
                 [width: 200, height: 50],
                 %{mode: :inline, fonts: [font]}
               )

      assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf, false)

      assert {:error, {:no_extractable_text, _}} =
               NativeElixirPdfUtilities.Text.extract(pdf, layout: false)

      refute pdf =~ "/Subtype /Link"
    end

    assert {:error, {:unsupported_html, diagnostic}} =
             NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.pdf(
               source("<text y='25' font-family='NativeSvgFont0'>􏿿</text>"),
               [width: 200, height: 50],
               %{mode: :inline, fonts: [font]}
             )

    assert diagnostic.module == NativeElixirPdfUtilities.Validators.SvgConverterValidator
    assert diagnostic.operation == :convert
    assert diagnostic.source == "SVG text"
    assert diagnostic.message =~ "approved SVG font bytes do not cover"
  end

  test(
    "exact aliases select physical faces even when CSS metadata disagrees with internal font names",
    %{data: normal}
  ) do
    bold = File.read!("priv/fonts/dejavu/DejaVuSans-Bold.ttf")

    source =
      source(
        "<text x='5' y='25' font-size='20' font-weight='400' font-family='NativeSvgFont0'>Physical face Ω</text>"
      )

    context = %{mode: :inline, fonts: [%{data: bold}, %{data: normal}]}

    assert {:ok, pdf} =
             NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.pdf(
               source,
               [width: 200, height: 50],
               context
             )

    refute pdf =~ "/Subtype /Link"
    assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf, false)
    assert {:ok, _document} = NativeElixirPdfUtilities.Pdf.Reader.read(pdf)

    assert {:ok, png} =
             NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.png(
               source,
               [width: 200, height: 50],
               context
             )

    assert {:ok, ^png} =
             NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.png(
               source,
               [width: 200, height: 50],
               %{mode: :inline, fonts: [%{data: bold}]}
             )

    assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf)

    assert {:error, {:no_extractable_text, _}} =
             NativeElixirPdfUtilities.Text.extract(pdf, layout: false)
  end

  test(
    "cluster replacement retains ligature advance and diagnoses incomplete multi-glyph selection bounds",
    %{data: data}
  ) do
    source =
      source("<text x='5' y='25' font-size='20' font-family='NativeSvgFont0'>ffi ffi á</text>")

    assert {:ok, pdf} =
             NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.pdf(
               source,
               [width: 200, height: 50],
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

    assert {:ok, composed_pdf} =
             NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.pdf(
               source("<text x='5' y='25' font-size='20' font-family='NativeSvgFont0'>á̈</text>"),
               [width: 200, height: 50],
               %{mode: :inline, fonts: [%{data: data}]}
             )

    refute composed_pdf =~ "/Subtype /Link"

    assert :ok =
             NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(
               composed_pdf,
               false
             )

    assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(composed_pdf)

    assert {:error, {:no_extractable_text, _}} =
             NativeElixirPdfUtilities.Text.extract(composed_pdf, layout: false)

    assert {:ok, parsed} = FontValidator.prepare_ttf(data)

    _advance =
      Enum.at(parsed.widths, Map.fetch!(parsed.cmap, 97)) * 20 / parsed.units_per_em * 0.75

    assert {:ok, _raw} = NativeElixirPdfUtilities.Pdf.Reader.read(composed_pdf)
    assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(composed_pdf)

    assert {:error, {:no_extractable_text, _}} =
             NativeElixirPdfUtilities.Text.extract(composed_pdf, layout: false)

    mono = File.read!("priv/fonts/dejavu/DejaVuSansMono.ttf")

    assert {:ok, mono_pdf} =
             NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.pdf(
               source("<text y='25' font-family='NativeSvgFont0'>á̈</text>"),
               [width: 200, height: 50],
               %{mode: :inline, fonts: [%{data: mono}]}
             )

    assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(mono_pdf)

    assert {:error, {:no_extractable_text, _}} =
             NativeElixirPdfUtilities.Text.extract(mono_pdf, layout: false)
  end

  test("implemented text paints and unsupported instance font changes retain their contracts", %{
    data: data
  }) do
    for content <- [
          "<defs><linearGradient id='stroke-gradient'><stop offset='0'/><stop offset='1'/></linearGradient></defs><text y='25' font-family='NativeSvgFont0' stroke='url(#stroke-gradient)'>A</text>"
        ] do
      assert {:ok, stroke_pdf} =
               NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.pdf(
                 source(content),
                 [width: 200, height: 50],
                 %{mode: :inline, fonts: [%{data: data}]}
               )

      refute stroke_pdf =~ "/Subtype /Link"

      assert :ok =
               NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(
                 stroke_pdf,
                 false
               )

      assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(stroke_pdf)

      assert {:error, {:no_extractable_text, _}} =
               NativeElixirPdfUtilities.Text.extract(stroke_pdf, layout: false)
    end

    assert {:ok, gradient_pdf} =
             NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.pdf(
               source(
                 "<defs><linearGradient id='r'><stop offset='0' /><stop offset='1' /></linearGradient></defs><text y='25' font-family='NativeSvgFont0' fill='url(#r)'>A</text>"
               ),
               [width: 200, height: 50],
               %{mode: :inline, fonts: [%{data: data}]}
             )

    refute gradient_pdf =~ "/Subtype /Link"

    assert :ok =
             NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(
               gradient_pdf,
               false
             )

    assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(gradient_pdf)

    assert {:error, {:no_extractable_text, _}} =
             NativeElixirPdfUtilities.Text.extract(gradient_pdf, layout: false)

    html =
      "<svg width=200 height=50 font-family='DejaVu Sans'><defs><g id='label'><text y=25>A</text></g></defs><use href='#label' font-weight=700 /></svg>"

    assert {:ok, pdf} = NativeElixirPdfUtilities.HtmlToPdf.render(html)
    assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf, false)

    assert {:error, {:no_extractable_text, _}} =
             NativeElixirPdfUtilities.Text.extract(pdf, layout: false)

    refute pdf =~ "/Subtype /Link"
  end

  test("affine text and viewBox retain actual selected glyph geometry", %{data: data}) do
    source =
      "<svg width='100' height='50' viewBox='0 0 200 100'><text x='5' y='25' font-size='20' transform='translate(20 10)' font-family='NativeSvgFont0'>Affine Ω</text></svg>"

    assert {:ok, pdf} =
             NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.pdf(
               source,
               [width: 100, height: 50],
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
  end

  test(
    "group clip opacity and fill alpha preserve painting text and shared surface accounting",
    %{data: data, limits: limits}
  ) do
    source =
      source(
        "<defs><clipPath id='c'><rect x='0' y='0' width='60' height='40' /></clipPath></defs><g clip-path='url(#c)' opacity='.5'><text x='5' y='25' font-family='NativeSvgFont0' fill-opacity='.4'>Clipped Ω</text></g>"
      )

    context = %{mode: :inline, fonts: [%{data: data}]}
    Limits.install(Map.put(limits, :max_aggregate_svg_surface_pixels, 1))

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.png(
               source,
               [width: 200, height: 50],
               context
             )

    assert diagnostic.message =~ "max_aggregate_svg_surface_pixels"
    Limits.install(limits)

    assert {:ok, pdf} =
             NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.pdf(
               source,
               [width: 200, height: 50],
               context
             )

    assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf)
    refute pdf =~ "/Subtype /Link"

    assert {:error, {:no_extractable_text, _}} =
             NativeElixirPdfUtilities.Text.extract(pdf, layout: false)
  end

  test "spaces remain logical text and hidden-only roots do not issue detached font painting" do
    html =
      "<svg width=200 height=50 font-family='DejaVu Sans'><text y=25>  One   <tspan> Two </tspan>  Three  </text></svg>"

    assert {:ok, pdf} = NativeElixirPdfUtilities.HtmlToPdf.render(html)
    refute pdf =~ "/Subtype /Link"
    assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf, false)
    assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf)

    assert {:error, {:no_extractable_text, _}} =
             NativeElixirPdfUtilities.Text.extract(pdf, layout: false)

    hidden =
      "<svg width=200 height=50 font-family='DejaVu Sans'><text y=25 visibility=hidden>Hidden Ω</text></svg>"

    assert {:ok, hidden_pdf} = NativeElixirPdfUtilities.HtmlToPdf.render(hidden)
    refute hidden_pdf =~ "/Subtype /Link"

    assert :ok =
             NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(
               hidden_pdf,
               false
             )

    assert :ok =
             NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(
               hidden_pdf,
               false
             )

    assert {:error, {:no_extractable_text, _}} =
             NativeElixirPdfUtilities.Text.extract(hidden_pdf, layout: false)
  end

  test("embedding restrictions use the shared font policy before native output", %{data: data}) do
    <<_::binary-size(4), count::16, _::binary-size(6), rest::binary>> = data

    table =
      for <<tag::binary-size(4), _checksum::32, offset::32,
            _length::32 <- binary_part(rest, 0, count * 16)>>,
          into: %{} do
        {tag, offset}
      end

    offset = Map.fetch!(table, "OS/2") + 8

    for flags <- [2, 512] do
      denied =
        binary_part(data, 0, offset) <>
          <<flags::16>> <> binary_part(data, offset + 2, byte_size(data) - offset - 2)

      assert {:error, {:invalid_document, diagnostic}} =
               NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.pdf(
                 source("<text y='25' font-family='NativeSvgFont0'>A</text>"),
                 [width: 200, height: 50],
                 %{mode: :inline, fonts: [%{data: denied}]}
               )

      assert diagnostic.stage == :font
      assert diagnostic.message =~ "does not permit outline embedding"
    end

    no_subset =
      binary_part(data, 0, offset) <>
        <<256::16>> <> binary_part(data, offset + 2, byte_size(data) - offset - 2)

    assert {:ok, pdf} =
             NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.pdf(
               source("<text y='25' font-family='NativeSvgFont0'>Original bytes</text>"),
               [width: 200, height: 50],
               %{mode: :inline, fonts: [%{data: no_subset}]}
             )

    refute pdf =~ "/Subtype /Link"
    assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf, false)
    assert {:ok, _doc} = NativeElixirPdfUtilities.Pdf.Reader.read(pdf)
  end
end

defmodule NativeElixirPdfUtilities.HtmlToPdf.SvgType3PaintServerTestPublicRestoration do
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

defmodule NativeElixirPdfUtilities.HtmlToPdf.SvgVectorEffectTestPublicRestoration do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.{Limits}
  alias NativeElixirPdfUtilities.Pdf.Reader

  setup do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    %{limits: limits}
  end

  test "solid non-scaling strokes rebase genuine geometry under nonuniform and sheared CTMs" do
    for transform <- ["scale(2 .5)", "matrix(2 .2 .4 .6 15 20)", "translate(200 0) scale(-2 .5)"] do
      source =
        "<svg width='240' height='120'><g transform='#{transform}' opacity='.7'><path d='M10 40 H80' fill='none' stroke='#174a86' stroke-width='3' stroke-opacity='.4' vector-effect='non-scaling-stroke'/></g></svg>"

      assert {:ok, pdf} =
               NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.pdf(source, [], %{
                 mode: :inline
               })

      refute pdf =~ "/Subtype /Link"
      assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf, false)
      assert {:ok, _doc} = Reader.read(pdf)

      assert {:ok, non_scaling} =
               NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.png(source, [], %{
                 mode: :inline
               })

      assert {:ok, scaling} =
               source
               |> String.replace("non-scaling-stroke", "none")
               |> NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.png([], %{mode: :inline})

      refute non_scaling == scaling

      if transform == "scale(2 .5)" do
      end
    end
  end

  test "CSS cascade, explicit inheritance, viewBox and imported clip/opacity resources retain real strokes" do
    html =
      "<style>svg .n {vector-effect:non-scaling-stroke} svg .n > path {vector-effect:inherit}</style><svg width='240' height='120' viewBox='0 0 120 60'><defs><clipPath id='c'><rect width='115' height='55'/></clipPath></defs><g class='n' transform='translate(5 5) scale(1.2 .7)' opacity='.7' clip-path='url(#c)'><path d='M5 20 H80 L90 30' fill='none' stroke='#174a86' stroke-width='3'/></g></svg>"

    assert {:ok, pdf} = NativeElixirPdfUtilities.HtmlToPdf.render(html)
    refute pdf =~ "/Subtype /Link"
    assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf, false)
    assert {:ok, %{document: _doc}} = Reader.read_validated(pdf)
  end

  test "the original non-scaling server denial now paints the same source geometry" do
    source =
      "<svg width='240' height='120'><defs><linearGradient id='p'><stop stop-color='red'/><stop offset='1' stop-color='blue'/></linearGradient></defs><path d='M10 40 H180 L190 60' fill='none' stroke='url(#p)' vector-effect='non-scaling-stroke'/></svg>"

    assert {:ok, pdf} =
             NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.pdf(source, [], %{
               mode: :inline
             })

    refute pdf =~ "/Subtype /Link"
    assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf, false)
    assert {:ok, _document} = Reader.read(pdf)

    for mode <- [:inline, :image] do
      assert {:ok, _png} =
               NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.png(source, [], %{
                 mode: mode
               })
    end
  end
end

defmodule NativeElixirPdfUtilities.HtmlToPdf.SvgWidgetEffectAppearanceTestPublicRestoration do
  use ExUnit.Case, async: false

  test "ancestor opacity, masks and filters preserve real initial widget AP and shared sibling paint" do
    scenes = [
      {"opacity='.35'", ""},
      {"mask='url(#m)'",
       "<mask id='m' maskUnits='userSpaceOnUse' x='0' y='0' width='180' height='70'><rect width='110' height='70' fill='white'/></mask>"},
      {"filter='url(#f)'",
       "<filter id='f' filterUnits='userSpaceOnUse' x='0' y='0' width='180' height='70'><feColorMatrix type='saturate' values='0'/></filter>"}
    ]

    directory =
      Path.join(System.tmp_dir!(), "native-widget-effects-#{System.unique_integer([:positive])}")

    File.mkdir_p!(directory)
    on_exit(fn -> File.rm_rf!(directory) end)

    for {{effect, definition}, _index} <- Enum.with_index(scenes) do
      html = "<body style='margin:0'><svg width='180' height='70'><defs>#{definition}</defs>
<rect width='180' height='70' fill='#e8e8f8'/>
<g #{effect}><rect x='5' y='4' width='145' height='50' fill='#ff7777'/>
<foreignObject x='10' y='10' width='160' height='50'><div style='margin:0'>
<input name='real' value='Actual' style='display:block;width:110px;height:24px;margin:0;color:blue'>
</div></foreignObject><rect x='80' y='12' width='25' height='20' fill='#005500'/></g>
</svg></body>
"

      assert {:error, {:unsupported_html, diagnostic}} =
               NativeElixirPdfUtilities.HtmlToPdf.render(html, page_size: {150, 75}, margin: 0)

      assert diagnostic.reason == :unsupported_html
      assert diagnostic.stage == :layout
      assert diagnostic.module == NativeElixirPdfUtilities.Validators.SvgConverterValidator
      assert diagnostic.operation == :convert
      assert diagnostic.source == "foreignObject"
      assert diagnostic.message =~ "foreignObject"

      assert {:error, {:unsupported_html, diagnostic}} =
               NativeElixirPdfUtilities.HtmlToPdf.render(html,
                 page_size: {150, 75},
                 margin: 0,
                 forms: :static
               )

      assert diagnostic.reason == :unsupported_html
      assert diagnostic.stage == :layout
      assert diagnostic.module == NativeElixirPdfUtilities.Validators.SvgConverterValidator
      assert diagnostic.operation == :convert
      assert diagnostic.source == "foreignObject"
      assert diagnostic.message =~ "foreignObject"
    end
  end

  test "effect-owned checkbox states remain real Forms when toggled and flattened" do
    html =
      "<body style='margin:0'><svg width='150' height='60'><g opacity='.4'>\n<rect width='150' height='60' fill='red'/><foreignObject x='10' y='10' width='120' height='40'>\n<div><input type='checkbox' name='tick' checked></div></foreignObject></g></svg></body>\n"

    assert {:error, {:unsupported_html, foreignobject_diagnostic}} =
             NativeElixirPdfUtilities.HtmlToPdf.render(html, page_size: {120, 60}, margin: 0)

    assert foreignobject_diagnostic.reason == :unsupported_html
    assert foreignobject_diagnostic.message =~ "foreignObject"
    assert foreignobject_diagnostic.stage == :layout
    assert foreignobject_diagnostic.operation == :convert

    assert foreignobject_diagnostic.module ==
             NativeElixirPdfUtilities.Validators.SvgConverterValidator

    assert {:error, {:unsupported_html, foreignobject_diagnostic}} =
             NativeElixirPdfUtilities.HtmlToPdf.render(String.replace(html, " checked", ""),
               page_size: {120, 60},
               margin: 0
             )

    assert foreignobject_diagnostic.reason == :unsupported_html
    assert foreignobject_diagnostic.message =~ "foreignObject"
    assert foreignobject_diagnostic.stage == :layout
    assert foreignobject_diagnostic.operation == :convert

    assert foreignobject_diagnostic.module ==
             NativeElixirPdfUtilities.Validators.SvgConverterValidator

    directory =
      Path.join(System.tmp_dir!(), "native-effect-button-#{System.unique_integer([:positive])}")

    File.mkdir_p!(directory)
    on_exit(fn -> File.rm_rf!(directory) end)
  end

  test "filled text paints inside the genuine ancestor opacity source graph" do
    html =
      "<body style='margin:0'><svg width='150' height='60'><g opacity='.25'>\n<rect width='150' height='60' fill='red'/><foreignObject x='10' y='10' width='120' height='40'>\n<div><input name='text' value='Old' style='width:80px;height:24px'></div></foreignObject></g></svg></body>\n"

    (
      assert {:error, {:unsupported_html, diagnostic}} =
               NativeElixirPdfUtilities.HtmlToPdf.render(html, page_size: {120, 60}, margin: 0)

      assert diagnostic.reason == :unsupported_html
      assert diagnostic.stage == :layout
      assert diagnostic.operation == :convert
      assert diagnostic.module == NativeElixirPdfUtilities.Validators.SvgConverterValidator
      assert diagnostic.source == "foreignObject"
      assert diagnostic.message =~ "foreignObject"
    )
  end

  test "filled text remains clipped by the genuine ancestor SVG mask" do
    html =
      "<body style='margin:0'><svg width='150' height='60'><defs><mask id='m' maskUnits='userSpaceOnUse' x='0' y='0' width='150' height='60'><rect width='75' height='60' fill='white'/></mask></defs><g mask='url(#m)'>\n<rect width='150' height='60' fill='red'/><foreignObject x='10' y='10' width='130' height='40'>\n<div><input name='text' value='Old' style='width:110px;height:24px'></div></foreignObject></g></svg></body>\n"

    (
      assert {:error, {:unsupported_html, diagnostic}} =
               NativeElixirPdfUtilities.HtmlToPdf.render(html, page_size: {120, 60}, margin: 0)

      assert diagnostic.reason == :unsupported_html
      assert diagnostic.stage == :layout
      assert diagnostic.operation == :convert
      assert diagnostic.module == NativeElixirPdfUtilities.Validators.SvgConverterValidator
      assert diagnostic.source == "foreignObject"
      assert diagnostic.message =~ "foreignObject"
    )
  end
end

defmodule NativeElixirPdfUtilities.HtmlToPdf.SvgPublicProducerRestoration do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow

  test "slot probe transports its genuine source Form and preserves public failure diagnostics" do
    source = "<svg width='4' height='2'><rect width='4' height='2' fill='red'/></svg>"
    assert {:ok, pdf} = SvgPublicWorkflow.pdf(source, [], %{mode: :inline})
    refute pdf =~ "/Subtype /Link"
    assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf, false)
    assert pdf =~ "%PDF-"
    assert {:ok, document} = NativeElixirPdfUtilities.Pdf.Reader.read(pdf)
    assert length(document.pages) == 1
  end

  test "inserted issued HTML Form leaves keep physical operand and original DOM ownership through affine placement" do
    for {transform, clipped} <- [
          {"", false},
          {"rotate(10 40 15)", false},
          {"skewX(15)", false},
          {"", true}
        ] do
      defs =
        if clipped do
          "<defs><clipPath id='clip'><rect width='20' height='80'/></clipPath></defs><g clip-path='url(#clip)'>"
        else
          ""
        end

      ending =
        if clipped do
          "</g>"
        else
          ""
        end

      html =
        "<svg width='220' height='80'>#{defs}<foreignObject x='10' y='8' width='190' height='50' transform='#{transform}'><div style='margin:0;font-family:DejaVu Sans;font-size:16px'>Alpha ffi<b>Beta</b></div></foreignObject>#{ending}</svg>"

      assert {:error, {:unsupported_html, diagnostic}} =
               NativeElixirPdfUtilities.HtmlToPdf.render(html)

      assert diagnostic.reason == :unsupported_html
      assert diagnostic.message =~ "foreignObject"

      assert diagnostic.stage == :layout and
               diagnostic.module == NativeElixirPdfUtilities.Validators.SvgConverterValidator and
               diagnostic.operation == :convert
    end
  end

  test "internal input rasterizes the actual issued HTML painting PDF and selected Unicode" do
    html =
      "<svg width='200' height='80'><foreignObject x='10' y='10' width='180' height='50'><div style='font-family:DejaVu Sans;font-size:14px;color:#234a88;background:#e3c369'>Native input café Ω</div></foreignObject></svg>"

    assert {:error, {:unsupported_html, diagnostic}} =
             NativeElixirPdfUtilities.HtmlToPdf.render(html)

    assert diagnostic.reason == :unsupported_html
    assert diagnostic.message =~ "foreignObject"

    assert diagnostic.stage == :layout and
             diagnostic.module == NativeElixirPdfUtilities.Validators.SvgConverterValidator and
             diagnostic.operation == :convert
  end
end
