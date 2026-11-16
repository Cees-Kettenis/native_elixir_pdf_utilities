defmodule NativeElixirPdfUtilities.HtmlToPdf.FlowResourceBudgetTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.{Diagnostics, Limits}
  alias NativeElixirPdfUtilities.HtmlToPdf.{FloatLayout, FlowShape, GridTracks}

  alias NativeElixirPdfUtilities.Validators.{
    CssBoxAlignmentValidator,
    CssColumnsValidator,
    CssGridValidator,
    CssShapeValidator,
    HtmlValidator
  }

  setup do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
  end

  test "grid source and typed cardinality failures keep the shared resource diagnostic" do
    Limits.install(%{Limits.effective() | max_entries: 4})
    parser = &HtmlValidator.parse_css_length(&1, :nonnegative)

    for source <- ["repeat(5,10pt)", "subgrid repeat(5,[a])"] do
      assert {:error, {:resource_limit_exceeded, %{message: message}}} =
               CssGridValidator.tracks(source, parser)

      assert message =~ "4-item"
    end

    assert {:error, {:resource_limit_exceeded, _}} =
             CssGridValidator.auto_tracks("repeat(5,10pt)", parser)

    assert {:error, {:resource_limit_exceeded, _}} =
             CssGridValidator.validate_style(%{grid_column_start: 5}, "panel")
  end

  test "nested track grammars preserve an underlying length diagnostic" do
    failure =
      Diagnostics.error(:style, :invalid_document, "selected font is unavailable", source: "1ch")

    parser = fn _ -> failure end

    for source <- ["repeat(2,1ch)", "minmax(1ch,2ch)", "fit-content(1ch)"] do
      assert ^failure = CssGridValidator.tracks(source, parser)
    end
  end

  test "computed shape, track, column and gap lengths propagate failed calculations" do
    invalid = "calc(1pt +)"
    length = {:css_length_math, {invalid, false, :length}}
    size = {:css_size_math, {invalid, false}}

    for style <- [
          %{shape_margin: length},
          %{shape_outside: %{position: {{:css_position_math, invalid}, 0}}},
          %{shape_inside: [{:css_position_math, invalid}]}
        ] do
      refute match?({:ok, _}, CssShapeValidator.compute_style(style))
    end

    refute match?(
             {:ok, _},
             CssGridValidator.compute_style(%{grid_template_columns: [{:length, size}]})
           )

    refute match?({:ok, _}, CssColumnsValidator.compute_style(%{column_width: length}))

    assert {:error, {_, %{stage: :layout}}} =
             HtmlValidator.with_render_budget(fn ->
               CssBoxAlignmentValidator.used_gaps(%{column_gap: length}, 100, nil)
             end)

    assert {:error, {_, %{stage: :layout}}} =
             HtmlValidator.with_render_budget(fn ->
               GridTracks.resolve({:length, size}, 100, %{})
             end)

    assert {:ok, %{grid_template_columns: [{:length, prepared}]}} =
             CssGridValidator.compute_style(%{
               grid_template_columns: [{:length, {:css_size_math, {"calc(10pt + 10%)", true}}}]
             })

    assert {:length, 20.0} = GridTracks.resolve({:length, prepared}, 100, %{})
  end

  test "float setup and painting failures do not disappear inside a formatting context" do
    failure = Diagnostics.error(:layout, :invalid_layout, "cannot lay out float content")
    block = %{style: %{float: :left, line_height: 10}, children: []}

    assert ^failure =
             FloatLayout.with_context(fn ->
               FloatLayout.layout(block, 0, 100, 100, fn _, _, _, _ -> failure end, fn _, _ ->
                 {10, 10}
               end)
             end)

    bad = {:css_length_math, {"calc(1pt +)", false, :margin}}

    block = %{
      block
      | style: Map.put(block.style, :margin, %{top: bad, right: 0, bottom: 0, left: 0})
    }

    assert {:error, {_, %{stage: :layout}}} =
             HtmlValidator.with_render_budget(fn ->
               FloatLayout.with_context(fn ->
                 FloatLayout.layout(
                   block,
                   0,
                   100,
                   100,
                   fn _, _, _, _ -> flunk("invalid edges reached paint") end,
                   fn _, _ -> {10, 10} end
                 )
               end)
             end)
  end

  test "shape geometry retains length and position evaluation errors" do
    base = %{
      left: 0,
      right: 40,
      top: 100,
      bottom: 60,
      border: %{x: 0, top: 100, width: 40, height: 40}
    }

    invalid = "calc(1pt +)"

    for shape <- [
          %{
            type: :circle,
            reference: :border_box,
            radii: [{:css_length_math, {invalid, false, :length}}],
            position: {0, 0}
          },
          %{
            type: :circle,
            reference: :border_box,
            radii: [10],
            position: {{:css_position_math, invalid}, 0}
          }
        ] do
      float = Map.put(base, :style, %{shape_outside: shape})

      assert {:error, {_, %{stage: :layout}}} =
               HtmlValidator.with_render_budget(fn -> FlowShape.prepare_float(float, 100) end)
    end

    Limits.install(%{Limits.effective() | max_work: 4})

    assert {:error, {:resource_limit_exceeded, _}} =
             CssColumnsValidator.prepare("columns", "calc(10)")
  end
end
