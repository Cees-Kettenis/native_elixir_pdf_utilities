defmodule NativeElixirPdfUtilities.Validators.CssScalarUnitsTest do
  use ExUnit.Case

  alias NativeElixirPdfUtilities.TestSupport.CssStyles
  alias NativeElixirPdfUtilities.Validators.{CssMathValidator, HtmlValidator}

  # CSS Values 4, 12 March 2024, sections 5.1 and 10.1.
  test "calculated integers round ties toward positive infinity while literals retain integer grammar" do
    for {source, expected} <- [
          {"calc(1.5)", 2},
          {"calc(-1.5)", -1},
          {"calc(2pt / 1pt)", 2},
          {"round(2.8)", 3},
          {"+2", 2},
          {"-2", -2}
        ] do
      assert {:ok, ^expected} = CssMathValidator.scalar(source, :integer)
    end

    for source <- [
          "1.0",
          "1e0",
          "pi",
          "calc(2pt)",
          "calc(1 / 0)",
          "calc(50%)",
          "calc(1 + 1pt)"
        ] do
      assert :error = CssMathValidator.scalar(source, :integer)
    end

    assert {:ok, 0} = CssMathValidator.scalar("calc(NaN)", :integer)
    assert {:ok, +0.0} = CssMathValidator.scalar("calc(0 / 0)")
    assert {:ok, value} = CssMathValidator.scalar("calc(pi * 2)")
    assert_in_delta value, 2 * :math.pi(), 1.0e-12
  end

  test "integer and scalar properties consume the shared typed result" do
    styles =
      CssStyles.by_id(
        "<div id='values' style='z-index:calc(-1.5);orphans:calc(1.5);widows:calc(5 / 2);flex-grow:calc(4 / 2)'>Values</div>"
      )

    assert %{z_index: -1, orphans: 2, widows: 3, flex_grow: 2.0} = styles["values"]
  end

  test "all absolute length units retain their physical conversion and literal range" do
    for source <- ["1in", "2.54cm", "25.4mm", "101.6Q", "6pc", "96px", "72pt"] do
      assert {:ok, points} = HtmlValidator.parse_css_length(source, :nonnegative)
      assert_in_delta points, 72, 1.0e-10
    end

    assert :error = HtmlValidator.parse_css_size("-1ch", :width)
    assert :error = HtmlValidator.parse_css_size("-1pt", :width)
    assert {:ok, _} = HtmlValidator.parse_css_size("calc(-1ch)", :width)
    assert {:ok, 2.0, %{resolution: 1}} = CssMathValidator.evaluate("2x", %{})
  end

  test "contextual scalar products use computed font metrics before integer rounding and range clamping" do
    style =
      CssStyles.by_id("""
      <div id="values" style="font-family:DejaVu Sans Mono;font-size:20pt;line-height:30pt;order:calc(1em / 4pt);z-index:calc(1lh / 4pt);orphans:calc(-1em / 1pt);flex-grow:calc(1ch / 1pt);counter-reset:chapter calc(1em / 2pt)"></div>
      """)["values"]

    assert style.order == 5
    assert style.z_index == 8
    assert style.orphans == 1

    assert_in_delta style.flex_grow,
                    NativeElixirPdfUtilities.HtmlToPdf.Font.text_width("0", style.font_face, 20),
                    1.0e-10

    assert style.counter_reset == [{"chapter", 10}]
    assert {:ok, 0} = CssMathValidator.scalar_in_range("calc(-1)", :integer, 0)
    assert {:ok, 2} = CssMathValidator.scalar_in_range("calc(4)", :integer, 0, 2)
    assert :error = CssMathValidator.scalar_in_range("-1", :integer, 0)
    assert :error = CssMathValidator.scalar_in_range("4", :integer, 0, 2)
    assert {:ok, 1.0} = CssMathValidator.scalar("calc(50% / 50%)")
  end

  test "computed calculation metadata rejects forged units and dependency flags without acquiring fonts" do
    alias NativeElixirPdfUtilities.Validators.{
      CssPositionValidator,
      CssSizingValidator,
      CssLengthValidator
    }

    assert {:ok, calculation} =
             CssMathValidator.prepare_length("calc(50% + 2pt)", %{"pt" => 1 / 0.75}, :css_pixel)

    assert CssMathValidator.valid_prepared_length?(calculation)
    assert CssPositionValidator.valid_prepared?({:css_position_math, calculation})
    assert {:ok, 12.0} = CssMathValidator.resolve_length(calculation, 20, :property)
    assert {:ok, -8.0} = CssMathValidator.resolve_length(calculation, -20, :property)
    refute CssSizingValidator.valid_calculation?({:css_size_math, {calculation, false}})

    refute CssSizingValidator.valid_calculation?(
             {:css_size_math, {{:css_calculation, {"calc(1pt)", %{}, :css_pixel}}, false}}
           )

    assert {:error, {:invalid_document, diagnostic}} =
             CssMathValidator.prepare_length("1pt", %{"pt" => 1, "em" => 20}, :css_pixel)

    assert diagnostic.message =~ "referenced length units"
    refute CssMathValidator.valid_prepared_length?(:invalid)
    refute CssMathValidator.valid_unit_scales?(%{"invented" => 1})
    refute CssMathValidator.valid_unit_scales?(%{"pt" => -1})

    refute CssLengthValidator.valid_edge?(
             {:css_length_math, {calculation, true, :length}},
             :border_widths
           )

    refute CssLengthValidator.valid_edge?(
             {:css_length_math, {{:css_calculation, {"1pt", %{}, :css_pixel}}, false, :padding}},
             :padding
           )

    assert {:ok, 24} = CssLengthValidator.resolve({:rem, 2}, nil, %{_root_font_size: 12})
    assert {:ok, :auto} = CssLengthValidator.resolve(:auto, nil, %{})
  end
end
