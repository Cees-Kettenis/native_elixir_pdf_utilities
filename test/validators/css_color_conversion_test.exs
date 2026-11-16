defmodule NativeElixirPdfUtilities.Validators.CssColorConversionTest do
  use ExUnit.Case, async: true
  alias NativeElixirPdfUtilities.HtmlToPdf.CssColor
  alias NativeElixirPdfUtilities.Validators.CssColorValidator

  # Independent reference coordinates from CSS Color 4, 12 November 2026,
  # section 19, with D65 sRGB red and Bradford-adapted D50 Lab.
  test "conversion preserves extended channels until the output boundary" do
    for {space, expected} <- [
          {"xyz-d65", [0.4123907993, 0.2126390059, 0.0193308187]},
          {"lab", [54.29054, 80.80492, 69.89099]},
          {"oklab", [0.62795536, 0.22486306, 0.12584630]}
        ] do
      actual = CssColor.convert("srgb", [1.0, 0.0, 0.0], space)
      for {a, b} <- Enum.zip(actual, expected), do: assert_in_delta(a, b, 0.00005)
      restored = CssColor.convert(space, actual, "srgb")
      for {a, b} <- Enum.zip(restored, [1.0, 0.0, 0.0]), do: assert_in_delta(a, b, 0.00002)
    end

    extended = CssColor.convert("display-p3", [1.0, 0.0, 0.0], "srgb")
    assert hd(extended) > 1.0
    assert Enum.at(extended, 1) < 0.0
    assert CssColor.to_srgb("display-p3", [1.0, 0.0, 0.0]) == [1.0, 0.0, 0.0]

    for space <-
          ~w(srgb-linear display-p3 display-p3-linear a98-rgb prophoto-rgb rec2020 xyz-d50 hsl hwb lab lch oklab oklch) do
      converted = CssColor.convert("srgb", [0.2, 0.4, 0.6], space)
      restored = CssColor.convert(space, converted, "srgb")
      for {a, b} <- Enum.zip(restored, [0.2, 0.4, 0.6]), do: assert_in_delta(a, b, 0.00002)
    end
  end

  test "relative colors use converted channel coordinates and inherited alpha" do
    assert {:ok, {r, g, b, a}} =
             CssColorValidator.parse_function_color(
               "rgb(from rgb(10 20 30 / .4) calc(r + 10) g b / calc(alpha * .5))",
               nil
             )

    assert_in_delta r, 20 / 255, 0.000001
    assert_in_delta g, 20 / 255, 0.000001
    assert_in_delta b, 30 / 255, 0.000001
    assert_in_delta a, 0.2, 0.000001

    assert {:ok, %{space: "display-p3", channels: [r, g, b], alpha: 0.4}} =
             CssColorValidator.prepare(
               "color(from color(display-p3 .8 .1 .2 / .4) display-p3 calc(r * .5) g b)",
               nil
             )

    assert_in_delta r, 0.4, 0.000001
    assert_in_delta g, 0.1, 0.000001
    assert_in_delta b, 0.2, 0.000001

    for space <- ~w(rgb hsl hwb lab lch oklab oklch) do
      channels =
        case space do
          "rgb" -> "r g b"
          "hsl" -> "h s l"
          "hwb" -> "h w b"
          s when s in ~w(lab oklab) -> "l a b"
          _ -> "l c h"
        end

      assert {:ok, {r, g, b}} =
               CssColorValidator.parse_function_color("#{space}(from red #{channels})", nil)

      assert_in_delta r, 1, 0.00002
      assert_in_delta g, 0, 0.00002
      assert_in_delta b, 0, 0.00002
    end
  end

  test "mixing normalizes weights and premultiplies channels by alpha" do
    assert {:ok, {r, +0.0, b, a}} =
             CssColorValidator.parse_function_color(
               "color-mix(in srgb, rgb(255 0 0 / .5), blue)",
               nil
             )

    assert_in_delta r, 1 / 3, 0.000001
    assert_in_delta b, 2 / 3, 0.000001
    assert_in_delta a, 0.75, 0.000001

    assert {:ok, {0.5, +0.0, 0.5, 0.5}} =
             CssColorValidator.parse_function_color("color-mix(in srgb, red 25%, blue 25%)", nil)

    assert {:ok, {r, g, +0.0}} =
             CssColorValidator.parse_function_color(
               "color-mix(in srgb, rgb(none 0 0), rgb(100 200 0))",
               nil
             )

    assert_in_delta r, 100 / 255, 0.000001
    assert_in_delta g, 100 / 255, 0.000001

    assert {:ok, {r, g, b}} =
             CssColorValidator.parse_function_color(
               "color-mix(in hsl longer hue, hsl(350 100% 50%), hsl(10 100% 50%))",
               nil
             )

    assert_in_delta r, 0, 0.000001
    assert_in_delta g, 1, 0.000001
    assert_in_delta b, 1, 0.000001
  end

  test "invalid relative coordinates and interpolation controls are rejected" do
    for value <- [
          "rgb(from red q g b)",
          "color(from red unknown r g b)",
          "color-mix(in unknown, red, blue)",
          "color-mix(in srgb longer hue, red, blue)",
          "color-mix(in srgb, red 0%, blue 0%)",
          "color-mix(in srgb, red -1%, blue)",
          "color-mix(in srgb, red 101%, blue)",
          "rgb(from red r g)",
          "rgb(from currentcolor r g b)"
        ] do
      assert :error = CssColorValidator.parse_function_color(value, nil), value
    end

    assert {:ok, {1, 1, 1}} = CssColorValidator.parse_function_color("Canvas", nil)
  end

  test "polar hue paths and powerless channels follow independent angle arithmetic" do
    color = fn hue -> %{space: "hsl", channels: [hue, 1.0, 0.5], alpha: 1.0} end

    for {a, b, method, expected} <- [
          {350, 10, :shorter, 360},
          {10, 350, :shorter, 360},
          {10, 20, :longer, 195},
          {20, 10, :longer, 195},
          {350, 10, :increasing, 360},
          {10, 350, :decreasing, 360}
        ] do
      mixed = CssColor.interpolate(color.(a), color.(b), 0.5, "hsl", method)
      assert_in_delta hd(mixed.channels), expected, 0.000001
    end

    for {space, channels, other} <- [
          {"lch", [50.0, 0.0, 300.0], [50.0, 20.0, 120.0]},
          {"oklch", [0.5, 0.0, 300.0], [0.5, 0.2, 120.0]},
          {"hsl", [300.0, 0.0, 0.5], [120.0, 1.0, 0.5]},
          {"hwb", [300.0, 0.6, 0.4], [120.0, 0.2, 0.2]}
        ] do
      first = %{space: space, channels: channels, alpha: 1.0}
      last = %{space: space, channels: other, alpha: 1.0}
      mixed = CssColor.interpolate(first, last, 0.5, space)

      assert_in_delta Enum.at(mixed.channels, if(space in ["lch", "oklch"], do: 2, else: 0)),
                      120,
                      0.000001
    end

    first = %{space: "lab", channels: [:none, 0.0, 0.0], alpha: 1.0}
    last = %{space: "oklab", channels: [0.4, 0.0, 0.0], alpha: 1.0}
    assert_in_delta hd(CssColor.interpolate(first, last, 0.5, "oklab").channels), 0.4, 0.000001
    assert [r, g, b] = CssColor.convert("srgb-linear", [0.001, 0.001, 0.001], "rec2020")
    for n <- [r, g, b], do: assert_in_delta(n, 0.0045, 0.000001)
    assert CssColor.convert("hsla", [0, 1, 0.5], "hsl") == [0, 1, 0.5]
  end

  test "current alpha, system alternatives and implicit mix percentages retain their public color contract" do
    assert {:ok, %{alpha: 0.4}} = CssColorValidator.prepare("currentColor", {1, 0, 0, 0.4})
    assert {:ok, %{alpha: +0.0}} = CssColorValidator.prepare("currentColor", :transparent)
    assert {:ok, {1, 0, 0}} = CssColorValidator.parse_function_color("light-dark(red,blue)", nil)
    assert :error = CssColorValidator.prepare("light-dark(red)", nil)

    for {source, red, blue} <- [
          {"color-mix(in srgb,red,blue 20%)", 0.8, 0.2},
          {"color-mix(in srgb,red 20%,blue)", 0.2, 0.8}
        ] do
      assert {:ok, {r, +0.0, b}} = CssColorValidator.parse_function_color(source, nil)
      assert_in_delta r, red, 0.000001
      assert_in_delta b, blue, 0.000001
    end

    assert {:ok, xyz} = CssColorValidator.prepare("color(from red xyz x y z)", nil)
    [r, g, b] = CssColor.to_srgb(xyz.space, xyz.channels)
    assert_in_delta r, 1, 0.000001
    assert_in_delta g, 0, 0.000001
    assert_in_delta b, 0, 0.000001
    assert :error = CssColorValidator.prepare("color(from red xyz x y)", nil)
    assert {:ok, infinite} = CssColorValidator.prepare("color(srgb infinity 0 0)", nil)
    assert CssColorValidator.valid_prepared?(infinite)
    refute CssColorValidator.valid_prepared?(%{space: "srgb", channels: [0, 0, 0], alpha: 2})
    refute CssColorValidator.valid_prepared?(:bad)
  end

  test "mix operands reject extra weights without losing the shared parse failure" do
    assert :error =
             NativeElixirPdfUtilities.Validators.CssColorValidator.prepare(
               "color-mix(in srgb, red 10% 20%, blue)",
               {0, 0, 0}
             )
  end
end
