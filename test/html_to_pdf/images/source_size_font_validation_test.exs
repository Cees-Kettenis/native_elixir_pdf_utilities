defmodule NativeElixirPdfUtilities.HtmlToPdf.SourceSizeFontValidationTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.HtmlToPdf.Font
  alias NativeElixirPdfUtilities.Validators.SourceSizeValidator

  test "source font errors return actionable canonical diagnostics rather than a bare failure" do
    context = %{width: 800.0, height: 600.0, density: 1.0, type: :print}

    for source <- [
          %{family: "Broken", data: "invalid font bytes"},
          %{family: "Missing", path: "/missing/source-size-font.ttf"}
        ] do
      assert {:error, {:invalid_font, diagnostic}} =
               SourceSizeValidator.prepare_context(context, ["1ex"],
                 fonts: [source],
                 system_font_discovery: false
               )

      assert diagnostic.stage == :font and diagnostic.reason == :invalid_font
      assert diagnostic.operation == :prepare_context
      assert diagnostic.module == SourceSizeValidator
      assert diagnostic.source == "font-relative source sizing"
      assert diagnostic.message =~ "authorized TrueType"
    end
  end

  test "sparse source font derives CSS missing-glyph metric fallbacks from its metadata" do
    path = "test/fixtures/html_to_pdf/browser_parity/assets/math-glyph-reference.ttf"

    assert {:ok, registry} =
             Font.load_registry(
               fonts: [%{family: "Math Glyph Test Reference", path: path}],
               system_font_discovery: false
             )

    assert {:ok, _, font} = Font.resolve("Math Glyph Test Reference", 400, :normal, registry)
    assert {:ok, metrics} = SourceSizeValidator.prepare_font_metrics(font, 12, 15)
    assert metrics.ex == 8 and metrics.ch == 8 and metrics.ic == 16
    assert metrics.cap == font.ascent * 16 / font.units_per_em
    assert metrics.lh == 20
  end

  test "caller font metric failures return diagnostics before division" do
    assert {:ok, registry} =
             Font.load_registry(
               fonts: [
                 %{
                   family: "Math Glyph Test Reference",
                   path:
                     "test/fixtures/html_to_pdf/browser_parity/assets/math-glyph-reference.ttf"
                 }
               ],
               system_font_discovery: false
             )

    assert {:ok, _, face} = Font.resolve("Math Glyph Test Reference", 400, :normal, registry)

    for units <- [0, 1.5, :invalid] do
      assert {:error, {:invalid_document, diagnostic}} =
               SourceSizeValidator.prepare_font_metrics(%{face | units_per_em: units}, 12, 15)

      assert diagnostic.stage == :font and diagnostic.operation == :math_metrics
      assert diagnostic.message =~ "usable units"
    end
  end
end
