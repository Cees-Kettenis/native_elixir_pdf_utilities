defmodule NativeElixirPdfUtilities.HtmlToPdf.MeasuredAtomicContextTest do
  use ExUnit.Case, async: true
  alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, Layout, ResourceContext, Style}
  alias NativeElixirPdfUtilities.Validators.{HtmlTableValidator, HtmlValidator}

  test "isolated measurement retains inherited atomic context and actual linked artwork" do
    ResourceContext.run([], HtmlValidator.new_image_budget(), fn context ->
      assert {:ok, dom} =
               HtmlParser.parse_detailed(
                 "<div style='display:inline-block'><a href='https://measure.test/child'>Measured</a></div>"
               )

      assert {:ok, %{children: [owner]}} = Style.compute_detailed(dom)
      [node] = owner.children
      assert node.style._table_atomic_context == true
      assert {:ok, measured} = Layout.measure_child(node, context)
      assert measured.width > 0 and measured.ascent > 0
      assert Enum.any?(measured.boxes, &(Map.get(&1, :link_url) == "https://measure.test/child"))
    end)
  end

  test "measurement ownership does not authorize forged flow intent or paginated ancestry" do
    ResourceContext.run([], HtmlValidator.new_image_budget(), fn context ->
      assert {:ok, dom} =
               HtmlParser.parse_detailed(
                 "<div style='display:inline-block'><a href='https://measure.test/child'>Measured</a></div>"
               )

      assert {:ok, %{children: [owner]} = styled} = Style.compute_detailed(dom)
      [node] = owner.children
      assert {:error, {:invalid_layout, paged}} = Layout.layout(%{styled | children: [node]})
      assert paged.stage == :layout and paged.module == HtmlTableValidator
      assert paged.message =~ "actual inline-block ancestor"
      forged = %{node | style: Map.put(node.style, :_atomic_table_flow, true)}
      assert {:error, {:invalid_layout, measured}} = Layout.measure_child(forged, context)
      assert measured.stage == :layout and measured.module == HtmlTableValidator
      assert measured.operation == :layout
      assert measured.source == "a"
      assert measured.message =~ "actual inline-block owner"
    end)
  end
end
