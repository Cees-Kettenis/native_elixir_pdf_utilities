Code.require_file("../support/svg_converted_image.ex", __DIR__)
Code.require_file("../support/svg_public_workflow.ex", __DIR__)
Code.require_file("../support/png_fixture.ex", __DIR__)

defmodule NativeElixirPdfUtilities.HtmlToPdf.SvgTextPositionTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.{Limits}
  alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, Style, SvgScene}
  alias NativeElixirPdfUtilities.Validators.{HtmlValidator}

  setup do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    %{limits: limits, data: File.read!("priv/fonts/dejavu/DejaVuSans.ttf")}
  end

  alias NativeElixirPdfUtilities.Validators.{SvgValidator, HtmlValidator}

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

  test("relative Unicode positioning is delegated to the existing converter", %{data: data}) do
    for content <- [
          "<text x='5' y='25' dx='0 0 10 20 30' font-family='NativeSvgFont0'>Ωá̈B</text>",
          "<text x='5' y='25' font-family='NativeSvgFont0'>Ω<tspan dx='0 10 20 30' font-family='NativeSvgFont0'>á̈B</tspan></text>",
          "<text x='5' y='25' font-family='NativeSvgFont0'> Ω <tspan dx='0 0 10 20 30' font-family='NativeSvgFont0'> á̈B </tspan></text>"
        ] do
      assert {:ok, pdf} =
               NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.pdf(
                 "<svg width='200' height='50'>#{content}</svg>",
                 [],
                 %{mode: :inline, fonts: [%{data: data}]}
               )

      assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf, false)
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
