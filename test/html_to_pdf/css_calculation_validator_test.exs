defmodule NativeElixirPdfUtilities.HtmlToPdf.CssCalculationValidatorTest do
  use ExUnit.Case, async: false

  alias NativeElixirPdfUtilities.Limits
  alias NativeElixirPdfUtilities.Validators.{CssCalculationValidator, HtmlValidator}

  setup do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
  end

  test "percentage comparison retains its actual consumption basis" do
    assert {:ok, expression} = CssCalculationValidator.prepare("min(200%, 1em)", %{"em" => 20})
    assert {:ok, 10.0} = CssCalculationValidator.resolve(expression, 5)
    assert {:ok, 20.0} = CssCalculationValidator.resolve(expression, 15)

    assert {:ok, expression} =
             CssCalculationValidator.prepare("clamp(20pt, calc(150% + 3pt), 60pt)", %{"pt" => 1})

    assert {:ok, 48.0} = CssCalculationValidator.resolve(expression, 30)
  end

  test "CSS whitespace, scientific notation and comments keep dimensional arithmetic" do
    assert {:ok, expression} =
             CssCalculationValidator.prepare("calc(1e1pt\t+\n2pt/* gap */*3)", %{"pt" => 1})

    assert {:ok, 16.0} = CssCalculationValidator.resolve(expression, 0)
    assert :error = CssCalculationValidator.prepare("calc(1pt+2pt)", %{"pt" => 1})
    assert :error = CssCalculationValidator.prepare("calc(1pt + 2)", %{"pt" => 1})
  end

  test "invalid arguments and missing measured units use shared diagnostics" do
    for {value, scales} <- [{nil, %{}}, {"calc(1em)", []}, {"calc(1em)", %{"em" => -1}}] do
      assert {:error, {:invalid_document, diagnostic}} =
               CssCalculationValidator.prepare(value, scales)

      assert diagnostic.stage == :layout
      assert diagnostic.operation == :prepare_css_calculation
      assert diagnostic.message =~ "measured unit scales"
    end

    assert {:error, {:invalid_document, diagnostic}} =
             CssCalculationValidator.prepare("calc(1ch)", %{})

    assert diagnostic.source == "calc(1ch)"
    assert diagnostic.message =~ "ch"

    assert {:error, {:invalid_document, diagnostic}} =
             CssCalculationValidator.resolve(:invalid, 10)

    assert diagnostic.message =~ "validated expression"
  end

  test "token bytes, cumulative work and resolved magnitude stay bounded" do
    Limits.install(
      Map.merge(Limits.effective(), %{
        max_html_numeric_token_bytes: 32,
        max_css_work: 20,
        max_css_numeric_magnitude: 100
      })
    )

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             CssCalculationValidator.prepare("calc(1pt" <> String.duplicate(" ", 40) <> ")", %{
               "pt" => 1
             })

    assert diagnostic.message =~ "max_html_numeric_token_bytes"

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             HtmlValidator.with_render_budget(fn ->
               assert {:ok, _} = CssCalculationValidator.prepare("calc(10pt)", %{"pt" => 1})
               assert {:ok, _} = CssCalculationValidator.prepare("calc(10pt)", %{"pt" => 1})
               CssCalculationValidator.prepare("calc(10pt)", %{"pt" => 1})
             end)

    assert diagnostic.message =~ "max_css_work"
    Limits.install(Map.put(Limits.effective(), :max_css_work, 1_000))
    assert {:ok, expression} = CssCalculationValidator.prepare("calc(90pt + 90pt)", %{"pt" => 1})

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             CssCalculationValidator.resolve(expression, 0)

    assert diagnostic.message =~ "max_css_numeric_magnitude"
  end

  test "dependency discovery follows validated syntax and ignores CSS comments" do
    assert {:ok, ["ch", "rcap"]} =
             CssCalculationValidator.referenced_units("calc(1ch + 2rcap /* rlh */)")

    assert {:ok, []} = CssCalculationValidator.referenced_units("calc(1ch +2rcap)")
    assert {:ok, []} = CssCalculationValidator.referenced_units("invalid ch")
  end

  test "math length adapters delegate to the same shared grammar as CSS image consumers" do
    alias NativeElixirPdfUtilities.Validators.CssMathValidator
    assert {:ok, 32.0, %{length: 1}} = CssMathValidator.evaluate("calc(1pc + 1pc)", %{})

    assert {:ok, shared} =
             CssMathValidator.prepare_length("calc(1cqw + 1pc)", %{"cqw" => 2, "pc" => 12})

    assert {:ok, ^shared} =
             CssCalculationValidator.prepare("calc(1cqw + 1pc)", %{"cqw" => 2, "pc" => 12})

    assert {:ok, 14.0} = CssCalculationValidator.resolve(shared, 0)
    assert {:ok, ["cqw", "pc"]} = CssCalculationValidator.referenced_units("calc(1cqw + 1pc)")

    assert {:ok, 48.0, %{length: 1}} =
             CssMathValidator.evaluate("calc(3pc/* css comment */)", %{})
  end

  test "shared depth policy and non-finite evaluation preserve real failures" do
    assert {:ok, expression} = CssCalculationValidator.prepare("calc(1pt / 0)", %{"pt" => 1})

    assert {:error, {:invalid_document, diagnostic}} =
             CssCalculationValidator.resolve(expression, 0)

    assert diagnostic.stage == :layout
    assert diagnostic.message =~ "zero"
    Limits.install(Map.put(Limits.effective(), :max_css_calculation_depth, 2))

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             CssCalculationValidator.prepare("calc(min(max(1pt, 2pt), 3pt))", %{"pt" => 1})

    assert diagnostic.message =~ "max_css_calculation_depth"
  end

  test "shared point scales retain typed products and dictionary-relative percentages" do
    for {source, expected} <- [
          {"calc(1em + 2px)", 19.5},
          {"min(200%, 12px)", 8.0},
          {"calc((1em - 4pt) / 2 + 3px * 2)", 11.5},
          {"calc(1em * 2em / 4em)", 9.0},
          {"calc(150% - 1pt)", 5.0}
        ] do
      assert {:ok, {:css_calculation, _} = expression} =
               CssCalculationValidator.prepare(source, %{"em" => 18, "px" => 0.75, "pt" => 1})

      assert {:ok, result} = CssCalculationValidator.resolve(expression, 4)
      assert_in_delta result, expected, 1.0e-10
    end

    for source <- [
          "calc(1em+2px)",
          "calc(1em -2px)",
          "calc(1em + 2)",
          "calc(1em * 2px)",
          "calc(1em / 2px)",
          "calc(2)",
          "min()",
          "clamp(1em, 2em)"
        ] do
      assert :error = CssCalculationValidator.prepare(source, %{"em" => 18, "px" => 0.75})
    end
  end
end
