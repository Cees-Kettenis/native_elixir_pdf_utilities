defmodule NativeElixirPdfUtilities.Validators.CssGridValidatorTest do
  use ExUnit.Case, async: true
  alias NativeElixirPdfUtilities.Validators.{CssGridValidator, HtmlValidator}

  test "track syntax preserves intrinsic functions and admits only non-nested repetition" do
    parser = &HtmlValidator.parse_css_length(&1, :nonnegative)

    for source <- [
          "none",
          "subgrid [a] [b c]",
          "min-content max-content",
          "fit-content(calc(20pt + 5pt))",
          "repeat(2,[a] minmax(min-content,30pt))",
          "[a] minmax(auto,10pt) repeat(auto-fill,minmax(10pt,max-content))",
          "repeat(auto-fit,minmax(min-content,20pt))",
          "20% 1fr"
        ] do
      assert {:ok, _} = CssGridValidator.tracks(source, parser), source
    end

    for source <- [
          "",
          "[broken",
          "subgrid 20pt",
          "subgrid [span]",
          "subgrid repeat(0,[a])",
          "subgrid repeat(2,20pt)",
          "subgrid repeat(auto-fill,[a]) repeat(auto-fill,[b])",
          "subgrid [1bad]",
          "repeat(2)",
          "repeat(0,20pt)",
          "repeat(2,none)",
          "repeat(auto-fit,1fr)",
          "[a]",
          "repeat(1,repeat(2,20pt))",
          "minmax(1fr,20pt)",
          "fit-content(auto)",
          "repeat(auto-fill,20pt) 1fr"
        ] do
      assert :error = CssGridValidator.tracks(source, parser), source
    end
  end

  test "template grammar distinguishes implicit auto-flow tracks from explicit named areas" do
    parser = &HtmlValidator.parse_css_length(&1, :nonnegative)

    for {property, source, expected} <- [
          {"grid-template", "none", %{grid_template_rows: [], grid_template_columns: []}},
          {"grid-template", "\"a\" 20pt [end] / 10pt",
           %{grid_template_rows: [{:length, 20.0}, {:line_names, ["end"]}]}},
          {"grid-template", "20pt / 30pt",
           %{grid_template_rows: [{:length, 20.0}], grid_template_columns: [{:length, 30.0}]}},
          {"grid-template", "\"a a\" \"b b\"",
           %{grid_template_rows: [:auto, :auto], grid_template_areas: [["a", "a"], ["b", "b"]]}},
          {"grid", "20pt / auto-flow dense 30pt",
           %{grid_auto_flow: :column_dense, grid_auto_columns: {:length, 30.0}}},
          {"grid", "auto-flow 20pt / 30pt",
           %{grid_auto_flow: :row, grid_auto_rows: {:length, 20.0}}}
        ] do
      assert {:ok, actual} = CssGridValidator.template(property, source, parser)
      assert Map.take(actual, Map.keys(expected)) == expected
    end

    for source <- [
          "auto-flow auto-flow / 20pt",
          "20pt",
          "20pt / 30pt / 40pt",
          "\"a\" \"a a\" / 20pt"
        ] do
      assert :error = CssGridValidator.template("grid", source, parser)
    end

    assert {:ok, :row_dense} = CssGridValidator.flow("dense")
    assert :error = CssGridValidator.flow("row column")

    assert {:ok, {:track_cycle, [{:length, 10.0}, :min_content]}} =
             CssGridValidator.auto_tracks("10pt min-content", parser)

    assert :error = CssGridValidator.auto_tracks("subgrid", parser)
    assert :error = CssGridValidator.auto_tracks("none", parser)
  end

  test "typed grids reject forged tracks, placements and nonrectangular areas with diagnostics" do
    for style <- [
          %{grid_template_columns: [nil]},
          %{grid_template_areas: :forged},
          %{grid_template_columns: [:forged]},
          %{grid_template_columns: [{:repeat, :auto_fill, [:min_content]}]},
          %{grid_template_columns: [{:subgrid, [:forged]}]},
          %{grid_template_rows: [{:minmax, :auto, {:minmax, :auto, :auto}}]},
          %{grid_template_columns: [{:subgrid, []}, :auto]},
          %{grid_auto_columns: {:fr, -1}},
          %{grid_column_start: {:named_span, "span", 1}},
          %{grid_row_end: {:span, 0}},
          %{grid_auto_flow: :dense_column_row},
          %{grid_template_areas: [["a", "a"], ["a", "b"]]},
          %{grid_template_columns: [{:fit_content, -1}]},
          %{grid_template_columns: [{:line_names, ["auto"]}]}
        ] do
      assert {:error, {:invalid_layout, diagnostic}} =
               CssGridValidator.validate_style(style, "grid panel")

      assert %{stage: :layout, source: "grid panel", reason: :invalid_layout} = diagnostic
      assert diagnostic.message != ""
    end

    assert :error = CssGridValidator.line("span")
    assert :error = CssGridValidator.line("name name")
    assert :error = CssGridValidator.areas("none extra")
  end
end
