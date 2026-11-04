defmodule NativeElixirPdfUtilities.HtmlToPdf.MathTokenItalicMetricsTest do
  use ExUnit.Case, async: true

  alias NativeElixirPdfUtilities.HtmlToPdf.{Font, MathLayout}
  alias NativeElixirPdfUtilities.TestSupport.Paths

  test "math-auto italic corrections match independent pinned FontTools metrics" do
    reference =
      Paths.fixture("html_to_pdf/browser_parity/assets/math-token-italic-reference.json")
      |> File.read!()
      |> :json.decode()

    font_bytes = File.read!(Paths.project(reference["font"]["path"]))

    assert Base.encode16(:crypto.hash(:sha256, font_bytes), case: :lower) ==
             reference["font"]["sha256"]

    assert reference["fonttools"] == "4.60.1"
    {:ok, registry} = Font.load_registry(system_font_discovery: false)
    {:ok, _, font} = Font.resolve("math", 400, :normal, registry)

    for {text, metric} <- reference["glyphs"], size <- [12, 20, 30] do
      expected = metric["correction_units"] * size / reference["font"]["units_per_em"]
      assert {:ok, measured} = MathLayout.glyph(text, font, size, text_transform: :math_auto)
      assert_in_delta measured.italic_correction, expected, 1.0e-12
    end

    for %{"source" => source, "sha256" => expected} <- reference["cases"] do
      actual = source |> Paths.project() |> File.read!() |> then(&:crypto.hash(:sha256, &1))
      assert Base.encode16(actual, case: :lower) == expected
    end
  end
end
