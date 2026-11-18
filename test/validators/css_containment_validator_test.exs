defmodule NativeElixirPdfUtilities.Validators.CssContainmentValidatorTest do
  use ExUnit.Case, async: true

  alias NativeElixirPdfUtilities.Validators.CssContainmentValidator, as: Containment

  test "intrinsic placeholders preserve shorthand axes and reject malformed lengths" do
    for {property, source, result} <- [
          {"contain-intrinsic-size", "0",
           %{contain_intrinsic_width: 0.0, contain_intrinsic_height: 0.0}},
          {"contain-intrinsic-width", "auto 6pt", %{contain_intrinsic_width: 6.0}},
          {"contain-intrinsic-height", "none", %{contain_intrinsic_height: 0.0}}
        ] do
      assert {:ok, ^result} = Containment.intrinsic_size(%{}, property, source)
    end

    for {property, source} <- [
          {"contain-intrinsic-size", ""},
          {"contain-intrinsic-size", "auto"},
          {"contain-intrinsic-size", "4pt -1pt"},
          {"contain-intrinsic-size", "5pt 10pt 3pt"},
          {"contain-intrinsic-width", "3pt 5pt"},
          {"contain-intrinsic-height", "25%"},
          {"contain-intrinsic-height", "auto auto 5px"}
        ] do
      assert :error = Containment.intrinsic_size(%{}, property, source)
    end

    assert {:error, {:not_implemented, _}} =
             Containment.intrinsic_size(
               %{_position_viewport: %{width: nil, height: nil}},
               "contain-intrinsic-height",
               "1cqh"
             )

    assert {:error, {:not_implemented, _}} =
             Containment.intrinsic_size(
               %{_css_unit_scales: %{}},
               "contain-intrinsic-size",
               "2pt 1em"
             )

    refute Containment.valid_effects?(:size)

    assert Containment.intrinsic(%{contain: [:inline_size], writing_mode: :vertical_rl}, :height) ==
             0.0
  end
end
