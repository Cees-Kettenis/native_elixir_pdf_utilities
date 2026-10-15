defmodule NativeElixirPdfUtilities.HtmlToPdf.ModernColorDimensionsTest do
  use ExUnit.Case, async: false

  alias NativeElixirPdfUtilities.Validators.{
    CssColorValidator,
    CssMathValidator,
    HtmlFormValidator
  }

  test "color token boundaries reject malformed commas and comments joining distinct tokens" do
    for color <- [
          "rgb(1,,2,3)",
          "rgb(,1,2,3)",
          "rgb(1,2,3,)",
          "rgb(1/**/2 3 4)",
          "r/**/ed",
          "rgb/**/(1 2 3)"
        ] do
      assert :error = CssColorValidator.parse_function_color(color, nil), color
    end

    assert {:ok, color} = CssColorValidator.parse_function_color("rgb(255/**/ 0 0)", nil)
    assert color == {1.0, 0.0, 0.0}
  end

  test "each color product requires a scalar factor and division requires a scalar divisor" do
    for source <- [
          "calc(120deg / 1deg)",
          "calc(25% / 5%)",
          "calc(1 / 2%)",
          "calc(1deg * 1deg)",
          "calc(1% * 2%)",
          "calc(2deg * 3%)",
          "calc(120deg * 1deg / 1deg)",
          "calc((2deg / 1deg) * 120deg)",
          "calc(1 / (1 / 1deg))",
          "calc(0% * 0%)",
          "min(calc(2deg / 1deg),3)",
          "sin(calc(2deg * 3deg / 1deg))",
          "atan2(calc(1% * 2%),calc(3% * 4%))"
        ] do
      assert :error = CssMathValidator.evaluate_color_component(source), source
    end
  end

  test "color scalar operations preserve numbers, percentages, angles and numeric functions" do
    for {source, expected, type} <- [
          {"calc(10 / 2)", 5, %{}},
          {"calc(2 * 10%)", 20, %{percentage: 1}},
          {"calc(10% * 2)", 20, %{percentage: 1}},
          {"calc(10% / 2)", 5, %{percentage: 1}},
          {"calc(240deg / 2)", :math.pi() * 2 / 3, %{angle: 1}},
          {"calc(2 * 60deg)", :math.pi() * 2 / 3, %{angle: 1}},
          {"sin(90deg)", 1, %{}},
          {"atan2(1,0)", :math.pi() / 2, %{angle: 1}},
          {"atan2(1deg,0deg)", :math.pi() / 2, %{angle: 1}},
          {"calc(asin(1) * 2)", :math.pi(), %{angle: 1}},
          {"round(up,10.1%,1%)", 11, %{percentage: 1}}
        ] do
      assert {:ok, actual, ^type} = CssMathValidator.evaluate_color_component(source), source
      assert_in_delta actual, expected, 1.0e-9, source
    end

    assert {:ok, :infinity, %{}} = CssMathValidator.evaluate_color_component("calc(1 / 0)")
  end

  test "general dimensional cancellation stays available to existing CSS length consumers" do
    for {source, expected, type} <- [
          {"calc(120deg / 1deg)", 120, %{}},
          {"calc(25% / 5%)", 5, %{}},
          {"calc(2px * 3px / 1px)", 6, %{length: 1}},
          {"calc(8s / 2s * 16px)", 64, %{length: 1}},
          {"calc(2turn / 180deg * 16px)", 64, %{length: 1}}
        ] do
      assert {:ok, actual, ^type} = CssMathValidator.evaluate(source, %{width: 0, height: 0}),
             source

      assert_in_delta actual, expected, 1.0e-9, source
    end
  end

  test "invalid dimensional colors sanitize to black through the original input policy" do
    for value <- [
          "hsl(calc(120deg / 1deg) 100% 50%)",
          "rgb(calc(25% / 5%) 20 30)",
          "hsl(calc(120deg * 1deg / 1deg) 100% 50%)"
        ] do
      assert {:ok, state} = HtmlFormValidator.prepare_initial_color(%{"value" => value})
      assert state.value == "#000000", value
      assert :error = CssColorValidator.parse_function_color(value, nil), value
    end
  end
end
