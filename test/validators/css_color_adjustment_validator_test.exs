defmodule NativeElixirPdfUtilities.Validators.CssColorAdjustmentValidatorTest do
  use ExUnit.Case, async: true
  alias NativeElixirPdfUtilities.HtmlToPdf
  alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, Layout, Style}
  alias NativeElixirPdfUtilities.Validators.CssColorAdjustmentValidator

  # CSS Color Adjustment 1, 16 December 2025. The print environment prefers
  # light, has no forced palette, and retains authored backgrounds when printing.
  test "print scheme precedes dependent colors regardless of declaration order and inherits" do
    html =
      "<div style='color:CanvasText;background-color:Canvas;color-scheme:dark;print-color-adjust:exact;forced-color-adjust:none'><span>Text</span></div>"

    assert {:ok, dom} = HtmlParser.parse(html)
    assert {:ok, %{children: [node]} = styled} = Style.compute(dom, system_font_discovery: false)
    assert node.style.color_scheme == :dark
    assert node.style.color == {1.0, 1.0, 1.0}

    assert {:ok, %{color: {1, 1, 1}}} =
             NativeElixirPdfUtilities.Validators.CssCascadeValidator.wide(
               %{color_scheme: :dark, color: {1, 0, 0}},
               "color",
               "initial"
             )

    assert node.style.background_color == {18 / 255, 18 / 255, 18 / 255}
    assert node.style.print_color_adjust == :exact
    assert node.style.forced_color_adjust == :none
    assert hd(node.children).style.color_scheme == :dark
    assert hd(node.children).style.print_color_adjust == :exact
    assert {:ok, _} = Layout.layout(styled)
    assert {:ok, "%PDF-" <> _} = HtmlToPdf.render(html, system_font_discovery: false)
  end

  test "light preference and exact adjustments remain independent of the host theme" do
    for {source, scheme} <- [
          {"normal", :light},
          {"only dark", :dark},
          {"dark only", :dark},
          {"dark light", :light},
          {"only light dark", :light}
        ] do
      assert {:ok, %{color_scheme: ^scheme}} =
               CssColorAdjustmentValidator.prepare("color-scheme", source)
    end

    for {property, value} <- [
          {"color-scheme", "only"},
          {"color-scheme", "light light"},
          {"color-scheme", "sepia"},
          {"print-color-adjust", "auto"},
          {"forced-color-adjust", "exact"},
          {"other", "none"}
        ] do
      assert :error = CssColorAdjustmentValidator.prepare(property, value)
    end

    assert {:ok, %{forced_color_adjust: :preserve_parent_color}} =
             CssColorAdjustmentValidator.prepare("forced-color-adjust", "preserve-parent-color")

    assert {:ok, %{print_color_adjust: :economy}} =
             CssColorAdjustmentValidator.prepare("print-color-adjust", "economy")

    assert {:ok, dom} =
             HtmlParser.parse(
               "<p style='color:light-dark(red,blue);color-scheme:only dark'>Text</p>"
             )

    assert {:ok, %{children: [node]}} = Style.compute(dom)
    assert node.style.color == {0, 0, 1}
  end

  test "forged adjustment metadata returns the shared actionable layout diagnostic" do
    for fields <- [
          %{color_scheme: :sepia},
          %{print_color_adjust: :auto},
          %{forced_color_adjust: :exact}
        ] do
      assert {:error,
              {:invalid_layout,
               %{stage: :layout, operation: :layout, source: "p", message: message}}} =
               CssColorAdjustmentValidator.validate_style(fields, "p")

      assert message =~ "color controls"
    end

    assert :ok = CssColorAdjustmentValidator.validate_style(%{}, "p")
  end

  test "dark control defaults precede authored colors and preserve transparent control backgrounds" do
    assert {:ok, %{forced_color_adjust: :auto}} =
             CssColorAdjustmentValidator.prepare("forced-color-adjust", "auto")

    html =
      "<div style='color-scheme:dark'><input value='Default'><button>Button</button><input style='color:red;background:blue'><input type=checkbox><input disabled></div>"

    assert {:ok, dom} = HtmlParser.parse(html)
    assert {:ok, %{children: [parent]}} = Style.compute(dom, system_font_discovery: false)
    [field, button, authored, checkbox, disabled] = parent.children
    assert field.style.background_color == {59 / 255, 59 / 255, 59 / 255}
    assert button.style.background_color == {107 / 255, 107 / 255, 107 / 255}
    assert authored.style.color == {1, 0, 0} and authored.style.background_color == {0, 0, 1}
    assert checkbox.style.background_color == nil
    assert disabled.style.color == {170 / 255, 170 / 255, 170 / 255}
  end
end
