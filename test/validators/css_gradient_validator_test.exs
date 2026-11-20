defmodule NativeElixirPdfUtilities.Validators.CssGradientValidatorTest do
  use ExUnit.Case, async: true
  alias NativeElixirPdfUtilities.Validators.CssGradientValidator
  alias NativeElixirPdfUtilities.HtmlToPdf.CssGradient

  @style %{font_size: 12.0, _root_font_size: 12.0, color: {0, 0, 0}}

  test "endpoint hints and malformed prepared radial geometry have deterministic admission" do
    assert :error = CssGradientValidator.prepare("linear-gradient()", @style)
    {:ok, linear} = CssGradientValidator.prepare("linear-gradient(red,blue)", @style)
    refute CssGradientValidator.valid_prepared?(%{linear | stops: [:bad | tl(linear.stops)]})

    assert {:ok, gradient} =
             CssGradientValidator.prepare("linear-gradient(to right, red, 0%, blue)", @style)

    assert {:ok, plan} = CssGradient.prepare(gradient, 100, 20, @style)
    assert CssGradient.sample(plan, 50, 10) == {0.0, 0.0, 1.0, 1.0}

    assert {:ok, radial} = CssGradientValidator.prepare("radial-gradient(red,blue)", @style)

    for radius <- [[], [:bad], [1, 2, 3]] do
      refute CssGradientValidator.valid_prepared?(%{
               radial
               | geometry: %{radial.geometry | radius: radius}
             })
    end

    assert {:ok, circle} =
             CssGradientValidator.prepare("radial-gradient(circle 10pt,red,blue)", @style)

    refute CssGradientValidator.valid_prepared?(%{
             circle
             | geometry: %{circle.geometry | radius: [1.0e100]}
           })
  end

  # CSS Images 3, 18 December 2023, section 3; Images 4, 30 September
  # 2025, section 3. Samples use exact normalized coordinates and RGB endpoints.
  test "linear fixup, double stops, hints and transparent endpoints retain their source intervals" do
    assert {:ok, gradient} =
             CssGradientValidator.prepare(
               "linear-gradient(to right, red 0% 20%, lime, 75%, blue 100%)",
               @style
             )

    assert {:ok, stops} = CssGradientValidator.used_stops(gradient, 100, @style)

    positions = Enum.map(Enum.filter(stops, &Map.has_key?(&1, :color)), & &1.position)
    for {a, b} <- Enum.zip(positions, [0.0, 0.2, 0.6, 1.0]), do: assert_in_delta(a, b, 0.000001)

    assert {:ok, plan} = CssGradient.prepare(gradient, 100, 20, @style)
    assert CssGradient.sample(plan, 10, 10) == {1.0, 0.0, 0.0, 1.0}
    {r, g, b, a} = CssGradient.sample(plan, 75, 10)
    assert_in_delta r, 0, 0.000001
    assert_in_delta g, 0.5, 0.000001
    assert_in_delta b, 0.5, 0.000001
    assert a == 1.0

    assert {:ok, gradient} =
             CssGradientValidator.prepare("linear-gradient(90deg, transparent, red)", @style)

    assert {:ok, plan} = CssGradient.prepare(gradient, 100, 20, @style)
    {r, g, b, a} = CssGradient.sample(plan, 50, 10)
    assert_in_delta r, 1, 0.000001
    assert_in_delta g, 0, 0.000001
    assert_in_delta b, 0, 0.000001
    assert_in_delta a, 0.5, 0.000001
  end

  test "repeating intervals and hard stops do not expand into unbounded paint pieces" do
    assert {:ok, gradient} =
             CssGradientValidator.prepare(
               "repeating-linear-gradient(to right, red 0pt 5pt, blue 5pt 10pt)",
               @style
             )

    assert {:ok, plan} = CssGradient.prepare(gradient, 100, 20, @style)

    for x <- [1, 4, 11, 21, 91],
        do: assert(CssGradient.sample(plan, x, 10) == {1.0, 0.0, 0.0, 1.0})

    for x <- [5, 8, 15, 25, 95],
        do: assert(CssGradient.sample(plan, x, 10) == {0.0, 0.0, 1.0, 1.0})

    assert {:ok, gradient} =
             CssGradientValidator.prepare("repeating-linear-gradient(red 0pt, blue 0pt)", @style)

    assert {:ok, plan} = CssGradient.prepare(gradient, 100, 20, @style)
    assert CssGradient.sample(plan, 50, 10) == {0.5, 0.0, 0.5, 1.0}
  end

  test "collapsed repeats average every stop pair in premultiplied sRGB" do
    for {source, expected} <- [
          {"repeating-linear-gradient(red 0pt,white 0pt,blue 0pt)", {0.75, 0.5, 0.75, 1.0}},
          {"repeating-radial-gradient(red 10%,transparent 10%,blue 10%)", {0.5, 0.0, 0.5, 0.5}},
          {"repeating-conic-gradient(in oklab,red 0deg,white 0deg,blue 0deg)",
           {0.75, 0.5, 0.75, 1.0}},
          {"repeating-linear-gradient(transparent 0pt,transparent 0pt)", {0.0, 0.0, 0.0, 0.0}}
        ] do
      assert {:ok, gradient} = CssGradientValidator.prepare(source, @style)
      assert {:ok, plan} = CssGradient.prepare(gradient, 100, 20, @style)
      for point <- [1, 50, 99], do: assert(CssGradient.sample(plan, point, 10) == expected)
    end
  end

  test "radial circles and ellipses use their selected center, radii and corner extent" do
    for {source, point, expected} <- [
          {"radial-gradient(circle 20pt at 30pt 30pt, red, blue)", {40, 30},
           {0.5, 0.0, 0.5, 1.0}},
          {"radial-gradient(ellipse 40pt 10pt at 50% 50%, red, blue)", {50, 45},
           {0.5, 0.0, 0.5, 1.0}},
          {"radial-gradient(circle closest-side, red, blue)", {50, 0}, {0.0, 0.0, 1.0, 1.0}},
          {"radial-gradient(ellipse farthest-corner, red, blue)", {100, 80}, {0.0, 0.0, 1.0, 1.0}}
        ] do
      assert {:ok, gradient} = CssGradientValidator.prepare(source, @style)
      assert {:ok, plan} = CssGradient.prepare(gradient, 100, 80, @style)
      {x, y} = point

      for {a, b} <-
            Enum.zip(Tuple.to_list(CssGradient.sample(plan, x, y)), Tuple.to_list(expected)),
          do: assert_in_delta(a, b, 0.000001)
    end
  end

  test "conic directions, percentage stops and explicit interpolation are selected independently" do
    assert {:ok, gradient} =
             CssGradientValidator.prepare(
               "conic-gradient(from 90deg at center, red, blue)",
               @style
             )

    assert {:ok, plan} = CssGradient.prepare(gradient, 100, 100, @style)
    assert CssGradient.sample(plan, 100, 50) == {1.0, 0.0, 0.0, 1.0}
    {r, g, b, a} = CssGradient.sample(plan, 0, 50)
    assert_in_delta r, 0.5, 0.000001
    assert_in_delta g, 0, 0.000001
    assert_in_delta b, 0.5, 0.000001
    assert a == 1.0

    assert {:ok, gradient} =
             CssGradientValidator.prepare(
               "linear-gradient(to right in srgb-linear, black, white)",
               @style
             )

    assert {:ok, plan} = CssGradient.prepare(gradient, 100, 20, @style)
    {r, g, b, _} = CssGradient.sample(plan, 50, 10)
    for c <- [r, g, b], do: assert_in_delta(c, 0.735356983, 0.000001)
  end

  test "invalid gradient grammar never reaches painting" do
    for source <- [
          "linear-gradient(red)",
          "linear-gradient(to left right,red,blue)",
          "linear-gradient(red,25%,50%,blue)",
          "linear-gradient(20%,red,blue)",
          "radial-gradient(circle 20%,red,blue)",
          "radial-gradient(ellipse 20pt,red,blue)",
          "conic-gradient(from nonsense,red,blue)",
          "linear-gradient(in srgb shorter hue,red,blue)",
          "linear-gradient(red,blue,)",
          "other(red,blue)"
        ] do
      assert :error = CssGradientValidator.prepare(source, @style), source
    end
  end

  test "radial extent keywords, elliptical lengths, side directions and conic stop angles resolve independently" do
    for {source, center, endpoint} <- [
          {"linear-gradient(to left,red,blue)", {50, 10}, {0, 10}},
          {"linear-gradient(to top,red,blue)", {50, 10}, {50, 0}},
          {"linear-gradient(to bottom right,red,blue)", {50, 10}, {100, 20}}
        ] do
      assert {:ok, gradient} = CssGradientValidator.prepare(source, @style)
      assert {:ok, plan} = CssGradient.prepare(gradient, 100, 20, @style)
      {r, g, b, _} = CssGradient.sample(plan, elem(center, 0), elem(center, 1))
      assert_in_delta r, 0.5, 0.000001
      assert g == 0
      assert_in_delta b, 0.5, 0.000001

      assert_in_delta elem(CssGradient.sample(plan, elem(endpoint, 0), elem(endpoint, 1)), 2),
                      1,
                      0.000001
    end

    for geometry <- [
          "circle",
          "ellipse",
          "closest-side",
          "farthest-side",
          "closest-corner",
          "farthest-corner",
          "circle closest-corner",
          "circle farthest-corner",
          "ellipse 20pt 10pt",
          "20pt",
          "ellipse 40% 20% at 25% 75%"
        ] do
      assert {:ok, gradient} =
               CssGradientValidator.prepare("radial-gradient(#{geometry},red,blue)", @style)

      assert {:ok, computed} = CssGradientValidator.compute(gradient, @style)
      assert CssGradientValidator.valid_prepared?(computed)
      assert {:ok, plan} = CssGradient.prepare(computed, 100, 40, @style)
      assert {_, _, _, 1.0} = CssGradient.sample(plan, 50, 20)
    end

    assert {:ok, gradient} =
             CssGradientValidator.prepare("conic-gradient(at center,red 0%,blue 1turn)", @style)

    assert {:ok, computed} = CssGradientValidator.compute(gradient, @style)
    assert CssGradientValidator.valid_prepared?(computed)
    assert {:ok, plan} = CssGradient.prepare(computed, 40, 40, @style)
    assert CssGradient.sample(plan, 20, 0) == {1.0, 0.0, 0.0, 1.0}
    assert {:ok, _} = CssGradientValidator.prepare("linear-gradient(0,red,blue)", @style)
  end

  test "malformed headers, stop lists and externally forged geometry fail source admission" do
    for source <- [
          "linear-gradient(to middle,red,blue)",
          "linear-gradient(to left right,red,blue)",
          "linear-gradient(blue)",
          "linear-gradient(red auto,blue)",
          "linear-gradient(red 1pt 2pt 3pt,blue)",
          "linear-gradient(25%,red,blue)",
          "radial-gradient(circle ellipse,red,blue)",
          "radial-gradient(at,red,blue)",
          "radial-gradient(circle 1pt 2pt,red,blue)",
          "conic-gradient(from bad,red,blue)",
          "conic-gradient(from 20deg at,red,blue)",
          "conic-gradient(red 1pt,blue)",
          "linear-gradient(in unknown,red,blue)",
          "linear-gradient(red,40%,50%,blue)"
        ] do
      refute match?({:ok, _}, CssGradientValidator.prepare(source, @style)), source
    end

    {:ok, linear} = CssGradientValidator.prepare("linear-gradient(red,blue)", @style)

    for geometry <- [
          %{direction: :bad},
          %{direction: {:sides, ["left", "right"]}},
          %{position: {{:percent, 0.5}, {:percent, 0.5}}, radius: [:bad], shape: :circle},
          %{position: {{:percent, 0.5}, {:percent, 0.5}}, from: :bad}
        ] do
      refute CssGradientValidator.valid_prepared?(%{linear | geometry: geometry})
    end

    refute CssGradientValidator.valid_prepared?(%{
             linear
             | stops: [%{color: hd(linear.stops).color, position: nil}, %{hint: nil}]
           })
  end

  test "unresolved calculation failures retain their diagnostic through stops, geometry and image containers" do
    alias NativeElixirPdfUtilities.Validators.{CssImageValueValidator, CssBackgroundValidator}
    bad = {:css_length_math, {"calc(1pt + 1s)", false, :length}}

    assert {:ok, radial} =
             CssGradientValidator.prepare("radial-gradient(circle 10pt,red,blue)", @style)

    broken_radius = %{radial | geometry: %{radial.geometry | radius: [bad]}}

    broken_stop = %{
      radial
      | stops: [Map.put(hd(radial.stops), :position, bad) | tl(radial.stops)]
    }

    for gradient <- [broken_radius, broken_stop] do
      assert {:error, {:invalid_layout, %{stage: :layout}}} =
               CssGradientValidator.compute(gradient, @style)

      assert {:error, {:invalid_layout, _}} =
               CssImageValueValidator.compute(
                 %{kind: :image_set, candidates: [%{image: gradient}]},
                 @style
               )

      assert {:error, {:invalid_layout, _}} =
               CssBackgroundValidator.compute_style(
                 Map.put(@style, :background_images, [gradient])
               )
    end

    assert {:error, {:invalid_layout, _}} =
             CssGradientValidator.used_stops(broken_stop, 10, @style)

    assert :error =
             CssBackgroundValidator.compute_style(
               Map.put(@style, :background_sizes, [
                 {{:css_size_math, {"calc(1pt + 1s)", false}}, :auto}
               ])
             )
  end
end
