defmodule NativeElixirPdfUtilities.HtmlToPdf.TablePresentationSupportTest do
  use ExUnit.Case, async: true

  alias NativeElixirPdfUtilities.HtmlToPdf
  alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, Layout, Style}
  alias NativeElixirPdfUtilities.Validators.HtmlTableValidator

  test "presentation attributes stay scoped to their table element kinds" do
    assert HtmlTableValidator.presentation_attribute?("table", "cellpadding")
    assert HtmlTableValidator.presentation_attribute?("td", "nowrap")
    refute HtmlTableValidator.presentation_attribute?("section", "cellpadding")
    refute HtmlTableValidator.presentation_attribute?("caption", "cellpadding")
    refute HtmlTableValidator.presentation_attribute?("td", "cellspacing")
  end

  test "inherited justification identifies the effective cell and an authored override renders" do
    assert {:error, {:unsupported_html, diagnostic}} =
             HtmlToPdf.render(
               "<table><tbody align=justify><tr><td>Inherited</td></tr></tbody></table>"
             )

    assert diagnostic.stage == :style and diagnostic.operation == :compute_style
    assert diagnostic.reason == :unsupported_html
    assert diagnostic.module == HtmlTableValidator
    assert diagnostic.source == "td inherited text-align=justify"
    assert diagnostic.message =~ "text justification"
    assert diagnostic.line == 1 and is_integer(diagnostic.column)

    assert {:ok, _pdf} =
             HtmlToPdf.render(
               "<table><tbody align=justify><tr><td style='text-align:left'>Override</td></tr></tbody></table>"
             )
  end

  test "a single authored left automatic table margin consumes the remaining inline space" do
    html =
      "<table style='width:120pt;margin-left:auto;margin-right:0;background:red;border-spacing:0'><tr><td style='height:12pt;padding:0'></td></tr></table>"

    assert {:ok, dom} = HtmlParser.parse_detailed(html)
    assert {:ok, styled} = Style.compute_detailed(dom)
    assert {:ok, %{boxes: [background]}} = Layout.layout(styled, page_size: {240, 240}, margin: 0)
    assert background.fill_color == {1, 0, 0}
    assert background.x == 120.0 and background.width == 120.0
  end
end
