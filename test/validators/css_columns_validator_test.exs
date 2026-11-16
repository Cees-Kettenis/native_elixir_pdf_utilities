defmodule NativeElixirPdfUtilities.Validators.CssColumnsValidatorTest do
  use ExUnit.Case, async: true
  alias NativeElixirPdfUtilities.Validators.CssColumnsValidator

  test "column controls and rules use the shared border grammar and current color" do
    for {property, source, expected} <- [
          {"columns", "AUTO auto", %{column_count: :auto, column_width: :auto}},
          {"column-count", "auto", %{column_count: :auto}},
          {"column-width", "0", %{column_width: 0.0}},
          {"column-fill", "balance-all", %{column_fill: :balance_all}},
          {"column-span", "none", %{column_span: :none}},
          {"column-rule-width", "2pt", %{column_rule_width: 2.0}},
          {"column-rule-style", "double", %{column_rule_style: :double}},
          {"column-rule-color", "currentcolor", %{column_rule_color: {1, 0, 0}}},
          {"column-rule", "1pt solid currentcolor",
           %{column_rule_width: 1.0, column_rule_style: :solid, column_rule_color: {1, 0, 0}}}
        ] do
      assert {:ok, ^expected} = CssColumnsValidator.prepare(property, source, {1, 0, 0})
    end

    for {property, value} <- [
          {"columns", "2 3"},
          {"column-count", "0"},
          {"column-width", "-1pt"},
          {"column-width", "20%"},
          {"column-fill", "overflow"},
          {"column-span", "2"},
          {"column-rule", "solid dashed"},
          {"unknown", "auto"}
        ] do
      refute match?({:ok, _}, CssColumnsValidator.prepare(property, value))
    end
  end

  test "column validation rejects invalid rules and dimensions while retaining shared diagnostics" do
    for style <- [
          %{column_count: 0},
          %{column_count: 1.5},
          %{column_width: -1},
          %{column_rule_width: -1},
          %{column_rule_style: :wavy},
          %{column_rule_color: :red},
          %{column_fill: :other},
          %{column_span: :other}
        ] do
      assert {:error, {:invalid_layout, %{source: "columns", message: message}}} =
               CssColumnsValidator.validate_style(style, "columns")

      assert message != ""
    end

    assert :ok = CssColumnsValidator.validate_style(%{}, "empty")

    assert {:ok, %{column_width: 20}} =
             CssColumnsValidator.compute_style(%{column_width: {:rem, 2}, _root_font_size: 10})

    assert :ok = CssColumnsValidator.validate_fragment_geometry(20, 30)
    assert {:error, {:invalid_layout, _}} = CssColumnsValidator.validate_fragment_geometry(20, -1)
  end
end
