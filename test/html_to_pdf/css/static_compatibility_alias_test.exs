defmodule NativeElixirPdfUtilities.HtmlToPdf.StaticCompatibilityAliasTest do
  use ExUnit.Case, async: true
  alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, Style}

  alias NativeElixirPdfUtilities.Validators.{
    CssCascadeValidator,
    CssValueValidator,
    HtmlValidator
  }

  test "aliases share targets and declarations participate in source order and important precedence" do
    for {alias_name, owner} <- [
          {"-webkit-border-radius", "border-radius"},
          {"-webkit-background-size", "background-size"},
          {"-webkit-box-shadow", "box-shadow"},
          {"-webkit-flex-flow", "flex-flow"},
          {"-webkit-print-color-adjust", "print-color-adjust"}
        ] do
      assert CssCascadeValidator.supported_property?(alias_name)
      assert CssCascadeValidator.canonical_property(alias_name) == owner
      assert CssCascadeValidator.targets(alias_name) == CssCascadeValidator.targets(owner)
    end

    style =
      style(
        "-webkit-border-radius:9pt / 3pt;border-radius:4pt;-webkit-border-top-left-radius:7pt 2pt"
      )

    assert style.border_radii.top_left == {7.0, 2.0}
    assert style.border_radii.top_right == {4.0, 4.0}
    style = style("-webkit-border-radius:9pt!important;border-radius:4pt")
    assert style.border_radius == 9
    style = style("border-radius:9pt;-webkit-border-radius:initial")
    assert style.border_radius == 0
    style = style("-webkit-text-stroke:2pt red;-webkit-text-stroke:blue")
    assert style.text_stroke_width == 0 and style.text_stroke_color == {0, 0, 1}
    style = style("-webkit-writing-mode:vertical-rl;border-start-start-radius:7pt 2pt")
    assert style.writing_mode == :vertical_rl
    assert style.border_radii.top_right == {7.0, 2.0}
  end

  test "legacy static box keyword mappings use the existing flex validator and reset both flow fields" do
    style =
      style(
        "display:-webkit-box;-webkit-box-orient:vertical;-webkit-box-align:start;-webkit-box-pack:justify;-webkit-box-flex:2;-webkit-box-ordinal-group:3"
      )

    assert style.display == :flex and style.flex_direction == :column
    assert style.align_items == :flex_start and style.justify_content == :space_between
    assert style.flex_grow == 2 and style.order == 3
    style = style("display:-webkit-inline-flex;-webkit-flex-flow:column wrap;flex-flow:row")

    assert style.display == :inline_flex and style.flex_direction == :row and
             style.flex_wrap == :nowrap

    assert :ok =
             CssValueValidator.validate_declarations(
               "-webkit-background-origin:content-box;-webkit-box-sizing:border-box;-webkit-print-color-adjust:exact"
             )

    refute CssCascadeValidator.supported_property?("-webkit-animation")
    refute CssCascadeValidator.supported_property?("-webkit-transition")

    for value <- ["row column", "wrap wrap", "row wrap reverse", "bad"] do
      assert {:error, _} =
               CssValueValidator.validate_declaration(
                 %{},
                 {"flex-flow", value},
                 HtmlValidator.new_css_budget()
               )
    end
  end

  defp style(source) do
    assert {:ok, dom} = HtmlParser.parse("<div style='#{source}'></div>")
    assert {:ok, styled} = Style.compute(dom, system_font_discovery: false)
    hd(styled.children).style
  end
end
