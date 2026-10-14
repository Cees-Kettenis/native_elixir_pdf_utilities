defmodule NativeElixirPdfUtilities.HtmlToPdf.CssNumericEdgeTest do
  use ExUnit.Case

  alias NativeElixirPdfUtilities.Limits
  alias NativeElixirPdfUtilities.Validators.{CssMathValidator, HtmlValidator}

  test "dimensional conversions retain their category and cancel only like units" do
    for {source, expected, type} <- [
          {"pi", :math.pi(), %{}},
          {"e", :math.exp(1), %{}},
          {"100grad", :math.pi() / 2, %{angle: 1}},
          {"500ms", 0.5, %{time: 1}},
          {"2hz", 2.0, %{frequency: 1}},
          {"2khz", 2000.0, %{frequency: 1}},
          {"2dppx", 2.0, %{resolution: 1}},
          {"192dpi", 2.0, %{resolution: 1}},
          {"calc(96dpcm / 2.54)", 1.0, %{resolution: 1}},
          {"3fr", 3.0, %{flex: 1}},
          {"1vh", 1.0, %{length: 1}},
          {"1vmin", 1.0, %{length: 1}},
          {"1vmax", 2.0, %{length: 1}},
          {"1in", 96.0, %{length: 1}},
          {"2.54cm", 96.0, %{length: 1}},
          {"25.4mm", 96.0, %{length: 1}},
          {"101.6q", 96.0, %{length: 1}},
          {"1rad", 1.0, %{angle: 1}}
        ] do
      assert {:ok, actual, ^type} = CssMathValidator.evaluate(source, %{width: 200, height: 100}),
             source

      assert_in_delta actual, expected, 1.0e-10, source
    end

    for source <- ["clamp(1px, 2px, 3s)", "pow(2,3,4)", "exp(1,2)", "unknown(1)"] do
      assert :error = CssMathValidator.evaluate(source, %{}), source
    end

    assert :error = CssMathValidator.evaluate("1unknown", %{})
    assert :invalid = CssMathValidator.component_tokens("\"", true)
    assert :error = CssMathValidator.evaluate("not-a-number", %{})
    assert [] == CssMathValidator.component_terms("calc(1 + 2", true)
  end

  test "non-finite arithmetic follows signed infinity and indeterminate-value rules" do
    for {source, expected} <- [
          {"calc(1 + NaN)", :nan},
          {"calc(NaN + 1)", :nan},
          {"sqrt(NaN)", :nan},
          {"calc(1 + infinity)", :infinity},
          {"calc(infinity + infinity)", :infinity},
          {"calc(infinity + -infinity)", :nan},
          {"calc(2 - infinity)", :negative_infinity},
          {"calc(2 - -infinity)", :infinity},
          {"calc(2 - NaN)", :nan},
          {"calc(2 / infinity)", 0.0},
          {"calc(2 / -infinity)", -0.0},
          {"calc(infinity / -2)", :negative_infinity},
          {"calc(infinity / infinity)", :nan},
          {"abs(-infinity)", :infinity},
          {"abs(infinity)", :infinity},
          {"abs(NaN)", :nan},
          {"sign(NaN)", :nan},
          {"sign(0)", 0.0},
          {"min(NaN, 1)", :nan},
          {"min(infinity, infinity)", :infinity},
          {"max(-infinity, -infinity)", :negative_infinity},
          {"hypot(NaN, 1)", :nan}
        ] do
      assert {:ok, ^expected, %{}} = CssMathValidator.evaluate(source, %{}), source
    end
  end

  test "rounding with infinite intervals preserves directional and remainder semantics" do
    for {source, expected} <- [
          {"round(up, 2, infinity)", :infinity},
          {"round(up, -2, infinity)", 0.0},
          {"round(down, -2, infinity)", :negative_infinity},
          {"round(down, 2, infinity)", 0.0},
          {"mod(2, infinity)", 2.0},
          {"mod(2, -infinity)", :nan},
          {"rem(2, infinity)", 2.0},
          {"round(infinity, 1)", :infinity},
          {"mod(infinity, 2)", :nan},
          {"round(down, 2.8, 1)", 2.0},
          {"round(to-zero, -2.8, 1)", -2.0}
        ] do
      assert {:ok, ^expected, %{}} = CssMathValidator.evaluate(source, %{}), source
    end
  end

  test "transcendental domains distinguish unbounded results from indeterminate ones" do
    for {source, expected} <- [
          {"atan(infinity)", :math.pi() / 2},
          {"atan(-infinity)", -:math.pi() / 2},
          {"atan2(infinity, infinity)", :math.pi() / 4},
          {"atan2(1, infinity)", 0.0},
          {"log(infinity, infinity)", :nan},
          {"log(infinity, 10)", :infinity},
          {"pow(infinity, 0)", 1.0},
          {"pow(1, infinity)", :nan},
          {"pow(2, infinity)", :infinity},
          {"pow(2, -infinity)", 0.0},
          {"pow(0.5, -infinity)", :infinity},
          {"pow(-infinity, 3)", :negative_infinity},
          {"pow(-infinity, 2)", :infinity},
          {"pow(-infinity, -2)", 0.0},
          {"pow(100, 1000)", :infinity},
          {"exp(1000)", :infinity}
        ] do
      assert {:ok, actual, %{}} = CssMathValidator.evaluate(source, %{}), source

      if is_number(expected),
        do: assert_in_delta(actual, expected, 1.0e-10, source),
        else: assert(actual == expected, source)
    end

    for {source, expected} <- [{"acos(0)", :math.pi() / 2}, {"atan(1)", :math.pi() / 4}] do
      assert {:ok, actual, %{}} = CssMathValidator.evaluate(source, %{})
      assert_in_delta actual, expected, 1.0e-10
    end
  end

  test "prepared lengths reject malformed inputs, non-finite results and excessive geometry" do
    for source <- [nil, 42, <<255>>] do
      assert {:error, {:invalid_document, diagnostic}} =
               CssMathValidator.length_dependencies(source)

      assert diagnostic.stage == :layout
      assert diagnostic.operation == :prepare_css_calculation
      assert diagnostic.module == CssMathValidator
      assert diagnostic.message =~ "text and measured unit scales"
    end

    assert {:ok, calculation} = CssMathValidator.prepare_length("calc(1px / 0)", %{"px" => 0.75})

    assert {:error, {:invalid_document, diagnostic}} =
             CssMathValidator.resolve_length(calculation, 100)

    assert diagnostic.message =~ "non-finite"
    assert diagnostic.source == "calc(1px / 0)"

    assert {:error, {:invalid_document, diagnostic}} =
             CssMathValidator.resolve_length(
               {:css_calculation, {"calc(1px + 1s)", %{"px" => 0.75}}},
               100
             )

    assert diagnostic.message =~ "validated expression"

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             CssMathValidator.resolve_length(
               calculation,
               Limits.get(:max_css_numeric_magnitude) + 1
             )

    assert diagnostic.message =~ "max_css_numeric_magnitude"
  end

  test "font-dependent calculations use selected initial metrics and retain loading failures" do
    environment = %{source_font_options: [system_font_discovery: false]}
    assert {:ok, value, %{length: 1}} = CssMathValidator.evaluate("1ex", environment)
    assert value > 0

    previous = Limits.effective()
    on_exit(fn -> Limits.install(previous) end)
    Limits.install(Map.put(previous, :max_font_source_bytes, 1))

    assert {:render_resource_limit, {:error, {:resource_limit_exceeded, diagnostic}}} =
             catch_throw(CssMathValidator.evaluate("1ex", environment))

    assert is_atom(diagnostic.stage)
    assert diagnostic.reason == :resource_limit_exceeded
    assert diagnostic.message =~ "max_font_source_bytes"
  end

  test "percentage color calculations retain their percentage scale" do
    assert {:ok, {red, green, blue}} =
             HtmlValidator.parse_css_color("rgb(calc(10% + 10%) 20% 30%)", nil)

    assert_in_delta red, 0.2, 1.0e-6
    assert_in_delta green, 0.2, 1.0e-6
    assert_in_delta blue, 0.3, 1.0e-6
  end
end
