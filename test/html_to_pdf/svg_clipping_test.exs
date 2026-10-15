Code.require_file("../support/svg_converted_image.ex", __DIR__)
Code.require_file("../support/svg_public_workflow.ex", __DIR__)
Code.require_file("../support/png_fixture.ex", __DIR__)

defmodule NativeElixirPdfUtilities.HtmlToPdf.SvgClippingTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.{HtmlToPdf, Limits, Text}
  alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, Style}

  setup do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    %{limits: limits}
  end

  test "original path clip browser fixture paints PNG artwork with surrounding logical text" do
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
    assert pdf =~ "/Subtype /Image"
  end

  alias NativeElixirPdfUtilities.Validators.{SvgValidator}
  alias NativeElixirPdfUtilities.{Limits, Text}

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
