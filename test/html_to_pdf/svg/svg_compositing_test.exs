alias NativeElixirPdfUtilities.TestSupport.Paths

defmodule NativeElixirPdfUtilities.HtmlToPdf.SvgCompositingTest do
  use ExUnit.Case, async: false
  Code.require_file(Paths.support("png_fixture.ex"))
  Code.require_file(Paths.support("svg_public_workflow.ex"))
  alias NativeElixirPdfUtilities.HtmlToPdf.{SvgRasterizer, PngDecoder}
  alias NativeElixirPdfUtilities.Validators.HtmlValidator

  test "print graphics modes retain source inheritance and composite in encoded RGB" do
    for {mode, expected} <- [{"sRGB", 128}, {"linearRGB", 128}] do
      image =
        raster(
          "<rect width='3' height='1' fill='black'/><rect width='3' height='1' fill='white' fill-opacity='.5' color-interpolation='#{mode}'/>"
        )

      for c <- :binary.bin_to_list(image.data), do: assert(abs(c - expected) <= 1)
      assert is_nil(image[:alpha_data])
    end

    image =
      raster(
        "<rect width='3' height='1' fill='black' fill-opacity='.5'/><rect width='3' height='1' fill='white' fill-opacity='.5' color-interpolation='linearRGB'/>"
      )

    for c <- :binary.bin_to_list(image.data), do: assert(abs(c - 170) <= 2)
    assert :binary.bin_to_list(image.alpha_data) == [192, 192, 192]

    inverse =
      raster(
        "<rect width='3' height='1' fill='white' fill-opacity='.5'/><rect width='3' height='1' fill='black' fill-opacity='.5' color-interpolation='linearRGB'/>"
      )

    for c <- :binary.bin_to_list(inverse.data), do: assert(abs(c - 85) <= 2)
    assert :binary.bin_to_list(inverse.alpha_data) == [192, 192, 192]

    transparent_gray =
      raster(
        "<rect width='3' height='1' fill='#808080' fill-opacity='.5' color-interpolation='linearRGB'/>"
      )

    for c <- :binary.bin_to_list(transparent_gray.data), do: assert(abs(c - 128) <= 1)
    assert :binary.bin_to_list(transparent_gray.alpha_data) == [128, 128, 128]
  end

  test "inherited and overridden modes survive isolated opacity boundaries" do
    cases = [
      {"<g color-interpolation='linearRGB'><rect width='3' height='1' fill='white' fill-opacity='.5'/></g>",
       128},
      {"<g color-interpolation='linearRGB'><rect width='3' height='1' color-interpolation='sRGB' fill='white' fill-opacity='.5'/></g>",
       128},
      {"<g color-interpolation='linearRGB' opacity='.5'><rect width='3' height='1' color-interpolation='sRGB' fill='white'/></g>",
       128},
      {"<g color-interpolation='linearRGB' opacity='.5'><g color-interpolation='sRGB' opacity='.5'><rect width='3' height='1' fill='white'/></g></g>",
       64},
      {"<g color-interpolation='linearRGB' opacity='.5'><rect width='3' height='1' fill='white'/><rect width='3' height='1' fill='black' fill-opacity='.5'/></g>",
       64}
    ]

    for {body, expected} <- cases do
      image = raster("<rect width='3' height='1' fill='black'/>#{body}")
      for c <- :binary.bin_to_list(image.data), do: assert(abs(c - expected) <= 2)
    end
  end

  test "gradients honor source interpolation before encoded RGB compositing" do
    # awaiting upstream fixes for resvg
    # PR #1172: the published converter gives 64 instead of linearRGB's 94.
    for {gradient_mode, expected} <- [{"sRGB", [64]}, {"linearRGB", [94, 64]}] do
      image =
        raster(
          "<defs><linearGradient id='g' color-interpolation='#{gradient_mode}'><stop stop-color='black'/><stop offset='1' stop-color='white'/></linearGradient></defs><rect width='3' height='1' fill='black'/><rect width='3' height='1' fill='url(#g)' fill-opacity='.5' color-interpolation='linearRGB'/>"
        )

      <<_::binary-size(3), r, g, b, _::binary>> = image.data

      assert Enum.any?(expected, fn value ->
               Enum.all?([r, g, b], &(abs(&1 - value) <= 2))
             end)
    end
  end

  test "mask luminance and alpha use rendered mask channels and independent transfer" do
    # awaiting upstream fixes for resvg
    # PR #1172: retain both the corrected luminance and published sRGB result.
    for {mode, kind, color, expected} <- [
          {"sRGB", "luminance", "#808080", [128]},
          {"linearRGB", "luminance", "#808080", [55, 128]},
          {"linearRGB", "alpha", "black", [255]}
        ] do
      image =
        raster(
          "<defs><mask id='m' color-interpolation='#{mode}' mask-type='#{kind}' maskUnits='userSpaceOnUse' x='0' y='0' width='3' height='1'><rect width='3' height='1' fill='#{color}'/></mask></defs><rect width='3' height='1' fill='red' mask='url(#m)'/>"
        )

      assert image.data == :binary.copy(<<255, 0, 0>>, 3)

      if expected == [255] do
        assert is_nil(image[:alpha_data])
      else
        assert Enum.any?(expected, fn value ->
                 Enum.all?(:binary.bin_to_list(image.alpha_data), &(abs(&1 - value) <= 1))
               end)
      end
    end
  end

  test "pattern, filter SourceGraphic and clip retain encoded print compositing" do
    cases = [
      "<defs><pattern id='p' patternUnits='userSpaceOnUse' width='3' height='1'><rect width='3' height='1' fill='white'/></pattern></defs><rect width='3' height='1' fill='url(#p)' fill-opacity='.5' color-interpolation='linearRGB'/>",
      "<defs><filter id='f' x='0' y='0' width='1' height='1'><feOffset dx='0' dy='0'/></filter></defs><g filter='url(#f)' color-interpolation='linearRGB' opacity='.5'><rect width='3' height='1' fill='white'/></g>",
      "<defs><clipPath id='c'><rect width='3' height='1'/></clipPath></defs><g clip-path='url(#c)' color-interpolation='linearRGB' opacity='.5'><rect width='3' height='1' fill='white'/></g>"
    ]

    for body <- cases do
      image = raster("<rect width='3' height='1' fill='black'/>#{body}")
      for c <- :binary.bin_to_list(image.data), do: assert(abs(c - 128) <= 2)
    end
  end

  test "authorized raster images blend and filter their premultiplied taps" do
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

  defp raster(body) do
    assert {:ok, png} =
             SvgRasterizer.rasterize("<svg width='3' height='1'>#{body}</svg>", [], nil)

    assert {:ok, image} = PngDecoder.decode(png, HtmlValidator.new_image_budget(), false)
    image
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
    # awaiting upstream fixes for resvg
    # https://github.com/linebender/resvg/pull/1172
    assert Enum.any?([188, 128], &(abs(:binary.at(image.data, 3) - &1) <= 1))

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

  test "selected font glyphs keep their source and positioned outline in raster mode" do
    font = File.read!(Paths.project("priv/fonts/dejavu/DejaVuSans.ttf"))

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
