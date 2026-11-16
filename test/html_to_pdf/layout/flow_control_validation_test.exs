defmodule NativeElixirPdfUtilities.HtmlToPdf.FlowControlValidationTest do
  use ExUnit.Case, async: true
  alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, Layout, Style}

  test "public layout rejects forged flow controls through their owning validators" do
    {:ok, dom} = HtmlParser.parse("<div>Text</div>")
    {:ok, %{children: [node]} = styled} = Style.compute(dom, system_font_discovery: false)

    for fields <- [
          %{column_count: 0},
          %{grid_template_columns: [nil]},
          %{shape_outside: %{type: :box, reference: :other}},
          %{float: :both},
          %{align_items: :other},
          %{column_gap: -1},
          %{break_before: :other},
          %{break_after: :other},
          %{break_inside: :page},
          %{empty_cells: :other}
        ] do
      forged = %{node | style: Map.merge(node.style, fields)}

      assert {:error, {:invalid_layout, %{stage: :layout, source: "div", message: message}}} =
               Layout.layout(%{styled | children: [forged]})

      assert message != ""
    end
  end
end
