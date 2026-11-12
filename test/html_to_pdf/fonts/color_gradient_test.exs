defmodule NativeElixirPdfUtilities.HtmlToPdf.ColorGradientTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.TestSupport.FontCorpus
  alias NativeElixirPdfUtilities.{Limits, Text}
  alias NativeElixirPdfUtilities.HtmlToPdf.{Font, PngDecoder, PdfWriter}

  alias NativeElixirPdfUtilities.Validators.{
    ColrGradientValidator,
    ColrPaintValidator,
    HtmlValidator
  }

  setup do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    :ok
  end

  test "OpenType radial cone equations select the largest valid root for growing, shrinking and negative circles" do
    # OpenType 1.9.1 COLR, Radial gradients: solve |p-c(t)|²=r(t)²,
    # then select the largest parameter whose radius is positive.
    for {circles, point, expected} <- [
          {[0, 0, 0, 0, 0, 10], {5, 0}, 0.5},
          {[0, 0, 10, 0, 0, 0], {5, 0}, 0.5},
          {[0, 0, -10, 0, 0, -20], {5, 0}, -1.5},
          {[0, 0, 5, 10, 0, 5], {5, 0}, 1.0},
          {[0, 0, 0, 10, 0, 10], {5, 0}, 0.25},
          {[0, 0, 0, 0, 0, 10], {0, 0}, nil},
          {[0, 0, 5, 10, 0, 5], {5, 6}, nil},
          {[0, 0, 5, 0, 0, 5], {5, 0}, nil},
          {[0, 0, 0, 10, 0, 10], {0, 5}, nil},
          {[0, 0, -5, 10, 0, -5], {5, 0}, nil}
        ] do
      {x, y} = point
      result = ColrGradientValidator.radial_parameter(circles, x, y)

      if expected == nil,
        do: assert(result == nil),
        else: assert_in_delta(result, expected, 1.0e-12)
    end
  end

  test "color-line extension, hard stops and alpha interpolate in premultiplied sRGB" do
    red = {1, 0, 0, 1}
    blue = {0, 0, 1, 0.5}
    line = [{0, red}, {1, blue}]
    assert ColrGradientValidator.sample(line, -1, 0) == red
    assert ColrGradientValidator.sample(line, 2, 0) == blue

    assert ColrGradientValidator.sample(line, 1.25, 1) ==
             ColrGradientValidator.sample(line, 0.25, 0)

    assert ColrGradientValidator.sample(line, -0.25, 1) ==
             ColrGradientValidator.sample(line, 0.75, 0)

    assert ColrGradientValidator.sample(line, 1.25, 2) ==
             ColrGradientValidator.sample(line, 0.75, 0)

    assert ColrGradientValidator.sample(line, 2.25, 2) ==
             ColrGradientValidator.sample(line, 0.25, 0)

    assert ColrGradientValidator.sample(line, 0.5, 0) == {2 / 3, 0, 1 / 3, 0.75}

    assert ColrGradientValidator.sample([{0, {1, 0, 0, 0}}, {1, {0, 0, 1, 0}}], 0.5, 0) ==
             {0, 0, 0, 0}

    assert ColrGradientValidator.sample([{0, red}, {0.5, red}, {0.5, blue}, {1, blue}], 0.5, 0) ==
             blue

    assert ColrGradientValidator.sample([{0.5, red}, {0.5, blue}], 0.25, 0) == red
    assert ColrGradientValidator.sample([{0.5, red}, {0.5, blue}], 0.75, 0) == blue
    assert ColrGradientValidator.sample([{0.5, red}, {0.5, blue}], 0.75, 1) == {0, 0, 0, 0}
  end

  test "radial PNG samples and nested color artwork retain alpha, resources and extraction" do
    line = [{0, {1, 0, 0, 1}}, {1, {0, 0, 1, 0}}]

    assert {:ok, png} =
             ColrGradientValidator.prepare_radial(
               [0, 0, 0, 0, 0, 100],
               0,
               line,
               {-100, -100, 100, 100},
               1000,
               30
             )

    assert {:ok, image} = PngDecoder.decode(png, HtmlValidator.new_image_budget(), true)
    assert image.width_px == 16 and image.height_px == 16
    assert is_binary(image.alpha_data)
    assert :binary.at(image.alpha_data, 8 * 16 + 8) > 200
    assert :binary.at(image.alpha_data, 0) == 0
    data = File.read!(FontCorpus.path("selected-colr.ttf"))
    assert {:ok, [font]} = Font.load_configured_faces([%{family: "Gradient", data: data}])

    for circles <- [[0, 0, 10, 0, 0, -10], [0, 0, -10, 0, 0, -20], [0, 0, 10, 0, 0, 10]] do
      paint = %{
        graph: {:glyph, font.cmap[?H], {:gradient, 6, circles, {0, line}}},
        clip: {0, 0, 600, 700}
      }

      assert {:ok, svg, _bounds, context} = ColrPaintValidator.prepare(font, paint, 24)
      assert svg =~ "svg-resource:radial"
      assert map_size(context.resources) == 1
    end

    assert {:error, {:invalid_font, diagnostic}} =
             ColrPaintValidator.prepare(font, %{graph: {:solid, {1, 0, 0, 1}}, clip: nil})

    assert diagnostic.message =~ "unbounded"
    assert {:ok, "", _, _} = ColrPaintValidator.prepare(font, %{graph: :empty, clip: nil})

    for {stops, extend, angles} <- [
          {line, 0, [300, 350, 1, -1]},
          {line, 1, [300, 350, 0, 0]},
          {line, 0, [300, 350, 0, 0]},
          {[{0.5, {1, 0, 0, 1}}, {0.5, {0, 1, 0, 1}}], 0, [300, 350, -1, 1]},
          {[{0, {1, 0, 0, 1}}], 0, [300, 350, -1, 1]},
          {[{0, {1, 0, 0, 1}}], 2, [300, 350, -1, 1]}
        ] do
      assert {:ok, svg, _, _} =
               ColrPaintValidator.prepare(font, %{
                 graph: {:glyph, font.cmap[?H], {:gradient, 8, angles, {extend, stops}}},
                 clip: nil
               })

      assert is_binary(svg)
    end

    box = %{
      type: :text,
      text: "H",
      x: 10,
      y: 10,
      font: Font.pdf_name(font),
      font_face: font,
      font_size: 24,
      color: {0, 0, 0}
    }

    assert {:ok, pdf} = PdfWriter.render([%{size: {100, 100}, boxes: [box]}])
    assert {:ok, "H"} = Text.extract(pdf, layout: false)
    Limits.install(Map.put(Limits.effective(), :max_font_color_work, 1))

    assert {:error, {:resource_limit_exceeded, _}} =
             ColrGradientValidator.prepare_radial(
               [0, 0, 0, 0, 0, 100],
               0,
               line,
               {-100, -100, 100, 100},
               1000,
               30
             )
  end
end
