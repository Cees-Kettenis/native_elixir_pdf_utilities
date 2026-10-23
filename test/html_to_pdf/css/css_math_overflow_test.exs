defmodule NativeElixirPdfUtilities.HtmlToPdf.CssMathOverflowTest do
  use ExUnit.Case

  alias NativeElixirPdfUtilities.HtmlToPdf

  alias NativeElixirPdfUtilities.Validators.{
    CssMathValidator,
    CssPositionValidator,
    HtmlValidator,
    PrintMediaValidator
  }

  test "shared arithmetic reports finite overflow with its expression and recovery guidance" do
    for source <- [
          "calc(1 / 1e-320)",
          "calc(-1 / 1e-320)",
          "calc(exp(700) * exp(700))",
          "calc(exp(709.7) + exp(709.7))",
          "hypot(exp(709.7), exp(709.7))",
          "mod(1, 1e-320)",
          "rem(1, 1e-320)",
          "round(1, 1e-320)",
          "round(up, 1, 1e-320)",
          "round(down, 1, 1e-320)",
          "round(to-zero, 1, 1e-320)"
        ] do
      assert {:error, {:resource_limit_exceeded, diagnostic}} =
               HtmlValidator.with_render_budget(fn -> CssMathValidator.evaluate(source, %{}) end)

      assert diagnostic.stage == :style
      assert diagnostic.reason == :resource_limit_exceeded
      assert diagnostic.operation == :evaluate_css_math
      assert diagnostic.module == CssMathValidator
      assert diagnostic.source == source
      assert diagnostic.message =~ "finite numeric range"
      assert diagnostic.message =~ "reduce intermediate magnitudes"
      assert diagnostic.message =~ "tiny nonzero divisors and step intervals"
    end
  end

  test "dependency discovery and used length evaluation share the overflow diagnostic" do
    source = "calc(1px / 1e-320)"

    assert {:error, {:resource_limit_exceeded, %{source: ^source}}} =
             CssMathValidator.length_dependencies(source)

    source = "mod(1%, 1e-310px)"
    assert {:ok, calculation} = CssMathValidator.prepare_length(source, %{"px" => 1})

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             CssMathValidator.resolve_length(calculation, 100)

    assert diagnostic.source == source
    assert diagnostic.module == CssMathValidator
    assert diagnostic.operation == :evaluate_css_math
  end

  test "public rendering returns diagnostics for overflowing widths and color components" do
    product = "calc(" <> Enum.join(List.duplicate("1e9", 34) ++ ["1e9px"], " * ") <> ")"

    for declaration <- [
          "width:calc(1px / 1e-320)",
          "width:mod(1px, 1e-320px)",
          "width:rem(1px, 1e-320px)",
          "width:round(1px, 1e-320px)",
          "width:#{product}",
          "color:rgb(calc(1 / 1e-320) 0 0)",
          "color:rgb(calc(exp(700) * exp(700)) 0 0)",
          "color:rgb(round(1, 1e-320) 0 0)",
          "transform:translateX(calc(1px / 1e-320))",
          "object-position:calc(1px / 1e-320) center"
        ] do
      assert {:error, {:resource_limit_exceeded, diagnostic}} =
               HtmlToPdf.render("<div style='#{declaration}'>Text</div>")

      assert diagnostic.stage == :style
      assert diagnostic.reason == :resource_limit_exceeded
      assert diagnostic.module == CssMathValidator
      assert diagnostic.operation == :evaluate_css_math
      assert diagnostic.message =~ "finite numeric range"
      assert is_binary(diagnostic.source)
      assert declaration =~ diagnostic.source
    end
  end

  test "positions and source sizes delegate overflow handling to shared math validation" do
    source = "calc(1px / 1e-320)"

    for result <- [
          CssPositionValidator.prepare_math(source),
          HtmlValidator.with_render_budget(fn ->
            CssPositionValidator.valid_prepared?({:css_position_math, source})
          end),
          PrintMediaValidator.source_length(source, %{width: 100, height: 100})
        ] do
      assert {:error, {:resource_limit_exceeded, diagnostic}} = result
      assert diagnostic.source == source
      assert diagnostic.module == CssMathValidator
      assert diagnostic.operation == :evaluate_css_math
    end
  end

  test "tiny finite values remain valid when their calculations stay representable" do
    for {source, expected} <- [
          {"calc(1e-320 / 1e-320)", 1.0},
          {"calc(1e-320 * 1e9)", 1.0e-311},
          {"mod(1e-320, 1e-320)", 0.0},
          {"rem(1e-320, 1e-320)", 0.0},
          {"round(1e-320, 1e-320)", 1.0e-320}
        ] do
      assert {:ok, actual, %{}} = CssMathValidator.evaluate(source, %{})
      assert_in_delta actual, expected, max(abs(expected) * 2.0e-5, 1.0e-323)
    end

    assert {:ok, calculation} =
             CssMathValidator.prepare_length("calc(1e-320px / 1e-320)", %{"px" => 1})

    assert {:ok, 1.0} = CssMathValidator.resolve_length(calculation, 100)

    assert {:ok, _} =
             HtmlToPdf.render("<div style='width:calc(1e-320px / 1e-320)'>Text</div>")
  end
end
