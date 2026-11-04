alias NativeElixirPdfUtilities.TestSupport.Paths

Code.require_file(Paths.support("svg_converted_image.ex"))
Code.require_file(Paths.support("svg_public_workflow.ex"))
Code.require_file(Paths.support("png_fixture.ex"))

defmodule NativeElixirPdfUtilities.HtmlToPdf.SvgTextTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.{Limits}
  alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, Style}

  setup do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    data = File.read!("priv/fonts/dejavu/DejaVuSans.ttf")
    %{limits: limits, data: data}
  end

  test "span positions and selected styles paint converter glyph pixels" do
    html =
      "<svg width=300 height=100 font-family='DejaVu Sans' font-size=20 text-anchor=middle>\n<text x=150 y=25>One<tspan dx=4 font-weight=700>Bold</tspan><tspan x=150 y=65 font-style=italic>café Ω</tspan></text>\n</svg>\n"

    assert {:ok, dom} = HtmlParser.parse_detailed(html)
    assert {:ok, %{children: [svg]}} = Style.compute_detailed(dom)
    assert length(svg.style.svg_scene.selected_fonts) == 3
    image = painted_image(html)
    assert coverage(image) > 100
  end

  test "uppercase original font attributes use shared CSS authority and paint glyphs" do
    html =
      "<svg width=200 height=50 font-family='DejaVu Sans'><text y=25 font-size='20px' font-weight='BOLD' font-style='ITALIC' letter-spacing='NORMAL'>Original Ω</text></svg>"

    image = painted_image(html)
    assert coverage(image) > 100
    assert {:ok, dom} = HtmlParser.parse_detailed(html)
    assert {:ok, %{children: [svg]}} = Style.compute_detailed(dom)
    [text] = svg.style.svg_scene.original_tree.children
    assert text.style.font_face.data == File.read!("priv/fonts/dejavu/DejaVuSans-BoldOblique.ttf")
  end

  test "original letter-spacing changes converter glyph placement" do
    html =
      "<svg width=200 height=50 font-family='DejaVu Sans'><text y=30 font-size=20 letter-spacing='2px'>AA</text></svg>"

    spaced = painted_image(html)
    unspaced = painted_image(String.replace(html, "letter-spacing='2px'", ""))
    assert coverage(spaced) > 20 and coverage(unspaced) > 20
    refute spaced.alpha == unspaced.alpha
  end

  test "same face text reuse paints both original converter instances" do
    html =
      "<svg width=220 height=70 font-family='DejaVu Sans' font-size=16>\n<defs><g id=label><text y=20>Repeat Ω</text></g></defs>\n<use href='#label' x=10 /><use href='#label' x=110 y=30 />\n</svg>\n"

    image = painted_image(html)
    single = painted_image(String.replace(html, "<use href='#label' x=110 y=30 />", ""))
    assert coverage(image) > coverage(single)
    assert coverage(single) > 50
  end

  test "hidden source text remains unpainted while explicitly visible spans survive" do
    html =
      "<svg width=200 height=80 font-family='DejaVu Sans'>\n<text x=10 y=20 visibility=hidden>Hidden<tspan visibility=visible>Visible Ω</tspan></text>\n<text x=10 y=40 display=none>Removed</text>\n<rect x=10 y=55 width=30 height=10 />\n</svg>\n"

    image = painted_image(html)
    hidden = painted_image(String.replace(html, "visibility=visible", "visibility=hidden"))
    assert coverage(image) > coverage(hidden)
    assert coverage(hidden) > 0
  end

  test "reused hidden text paints only explicitly visible converter spans" do
    html =
      "<svg width=200 height=50 font-family='DejaVu Sans'><defs><g id='label'><text y=25>Hidden<tspan visibility='visible'>Visible Ω</tspan></text></g></defs><use href='#label' visibility='hidden' /></svg>"

    visible = painted_image(html)
    hidden = painted_image(String.replace(html, "visibility='visible'", "visibility='hidden'"))
    assert coverage(visible) > 20
    assert coverage(hidden) == 0
  end

  test "original whitespace paints glyphs while hidden-only source has no alpha coverage" do
    html =
      "<svg width=200 height=50 font-family='DejaVu Sans'><text y=25>  One   <tspan> Two </tspan>  Three  </text></svg>"

    assert coverage(painted_image(html)) > 100

    hidden =
      "<svg width=200 height=50 font-family='DejaVu Sans'><text y=25 visibility=hidden>Hidden Ω</text></svg>"

    assert coverage(painted_image(hidden)) == 0
  end

  defp painted_image(html) do
    NativeElixirPdfUtilities.TestSupport.SvgConvertedImage.render(html)
  end

  defp coverage(image) do
    NativeElixirPdfUtilities.TestSupport.SvgConvertedImage.coverage(image)
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
