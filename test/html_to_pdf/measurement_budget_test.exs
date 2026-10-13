defmodule NativeElixirPdfUtilities.HtmlToPdf.MeasurementBudgetTest do
  use ExUnit.Case, async: false

  alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, Layout, ResourceContext, Style}
  alias NativeElixirPdfUtilities.Limits
  alias NativeElixirPdfUtilities.Validators.HtmlValidator

  test "measurement reports exhausted paint budgets locally and preserves subsequent document work" do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)

    assert :checked_inside_issued_document =
             ResourceContext.run([], HtmlValidator.new_image_budget(), fn context ->
               assert {:ok, dom} = HtmlParser.parse_detailed("<span>Measured content</span>")

               assert {:ok, %{children: [node]}} =
                        Style.compute_detailed(dom, [], context.image_budget)

               before = ResourceContext.current()
               Limits.install(Map.put(limits, :max_layout_boxes, 0))

               assert {:error, {:resource_limit_exceeded, diagnostic}} =
                        Layout.measure_child(node, context)

               assert diagnostic.stage == :layout
               assert diagnostic.reason == :resource_limit_exceeded
               assert diagnostic.message =~ "max_layout_boxes"
               assert ResourceContext.current() == before

               Limits.install(limits)
               assert {:ok, child} = Layout.measure_child(node, context)
               assert Enum.any?(child.boxes, &(Map.get(&1, :text) == "Measured content"))
               assert ResourceContext.current() == before
               :checked_inside_issued_document
             end)

    assert ResourceContext.current() == nil
  end
end
