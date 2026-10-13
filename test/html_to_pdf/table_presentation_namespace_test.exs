defmodule NativeElixirPdfUtilities.HtmlToPdf.TablePresentationNamespaceTest do
  use ExUnit.Case, async: true

  alias NativeElixirPdfUtilities.Validators.HtmlTableValidator

  test "foreign table-like styles retain namespace isolation in each effective presentation branch" do
    for namespace <- [:svg, :mathml],
        style <- [
          %{display: :table_cell, vertical_align: :baseline},
          %{display: :table_cell, text_align: :justify}
        ] do
      node = %{tag: "td", namespace: namespace, attributes: %{"align" => "justify"}}
      assert :ok = HtmlTableValidator.validate_presentation_style(node, style)
      assert :ok = HtmlTableValidator.validate_presentation_style(node, style, :layout)
    end

    node = %{tag: "td", namespace: :html, attributes: %{}}

    assert {:error, {:unsupported_html, baseline}} =
             HtmlTableValidator.validate_presentation_style(node, %{
               display: :table_cell,
               vertical_align: :baseline,
               text_align: :justify
             })

    assert baseline.stage == :style
    assert baseline.source == "td effective vertical-align=baseline"

    assert {:error, {:unsupported_html, justify}} =
             HtmlTableValidator.validate_presentation_style(
               node,
               %{display: :table_cell, text_align: :justify},
               :layout
             )

    assert justify.stage == :layout
    assert justify.source == "td inherited text-align=justify"
  end
end
