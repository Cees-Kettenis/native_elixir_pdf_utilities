defmodule NativeElixirPdfUtilities.HtmlToPdf.CssPathGeometryTest do
  use ExUnit.Case, async: true
  alias NativeElixirPdfUtilities.Validators.SvgPathValidator
  alias NativeElixirPdfUtilities.HtmlToPdf.PathGeometry

  test "canonical path grammar retains implicit lines and compact arc flags" do
    assert {:ok, parsed} = SvgPathValidator.prepare("M0 0 10 10 A5 5 0 01 20 10z")
    assert parsed == [{?M, [0, 0]}, {?L, [10, 10]}, {?A, [5, 5, 0, 0, 1, 20, 10]}, {?z, []}]

    assert :error = SvgPathValidator.prepare("L1 1")
    assert :error = SvgPathValidator.prepare("M0 0A2 2 0 21 4 5")
    assert :error = SvgPathValidator.prepare(nil)
  end

  test "absolute cubic geometry preserves smooth controls, close points and arc endpoints" do
    assert {:ok, parsed} =
             SvgPathValidator.prepare(
               "m0 0h10v10l-10 0z M20 0q10 10 20 0t20 0C70 10 70 10 80 0s10 -10 20 0A10 10 0 0 1 120 0"
             )

    commands = PathGeometry.commands(parsed)

    assert Enum.take(commands, 5) == [
             {0, [0.0, 0.0]},
             {1, [10.0, 0.0]},
             {1, [10.0, 10.0]},
             {1, [0.0, 10.0]},
             {4, []}
           ]

    assert {3, last} = List.last(commands)
    assert Enum.take(last, -2) == [120, 0]
    assert {:ok, parsed} = SvgPathValidator.prepare("M0 0L30 0L30 40")
    commands = PathGeometry.commands(parsed)
    assert PathGeometry.sample(commands, 50, false).point == {30.0, 20.0}
    assert PathGeometry.sample(commands, 50, false).length == 70.0
    assert PathGeometry.sample(commands, -10, false).point == {-10.0, 0.0}
    assert PathGeometry.sample(commands, 80, false).point == {30.0, 50.0}
    assert PathGeometry.sample(commands, 80, true).point == {10.0, 0.0}
    assert PathGeometry.transform([{0, [1, 2]}], {2, 0, 0, -2, 3, 4}) == [{0, [5.0, 0.0]}]
  end
end
