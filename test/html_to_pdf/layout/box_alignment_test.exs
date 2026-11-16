defmodule NativeElixirPdfUtilities.HtmlToPdf.BoxAlignmentTest do
  use ExUnit.Case, async: true
  alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, Layout, Style}
  alias NativeElixirPdfUtilities.Validators.CssBoxAlignmentValidator

  # CSS Box Alignment 3, 8 October 2026, sections 4 and 8.
  test "place shorthands preserve separate safety modifiers and baseline phrases" do
    assert {:ok, %{align_items: {:safe, :center}, justify_items: {:unsafe, :end}}} =
             CssBoxAlignmentValidator.prepare("place-items", "safe center unsafe end")

    assert {:ok, %{align_self: :last_baseline, justify_self: :center}} =
             CssBoxAlignmentValidator.prepare("place-self", "last baseline center")

    assert {:ok, %{align_content: :space_evenly, justify_content: :space_evenly}} =
             CssBoxAlignmentValidator.prepare("place-content", "space-evenly")

    for {property, value} <- [
          {"place-unknown", "center"},
          {"align-items", "space-evenly"},
          {"align-self", "safe"},
          {"align-content", "auto"},
          {"justify-content", "baseline"},
          {"align-items", "safe stretch"},
          {"place-items", "center center center"}
        ] do
      assert {:error, :invalid_document} = CssBoxAlignmentValidator.prepare(property, value)
    end
  end

  test "safe overflow falls back to start while unsafe positions preserve overflow" do
    for display <- ["flex", "grid"],
        {alignment, expected} <- [{"safe center", 0}, {"unsafe center", -10}, {"unsafe end", -20}] do
      layout =
        layout("""
        <div style='display:#{display};width:100pt;height:20pt;align-items:#{alignment};grid-template-columns:100pt;grid-template-rows:20pt'>
          <div style='width:20pt;height:40pt;flex:none;background:red'></div>
        </div>
        """)

      [box] = Enum.filter(layout.boxes, &(&1.type == :rect))
      assert_in_delta 300 - box.y - box.height, expected, 0.75 / 64
    end
  end

  test "first and last flex baseline groups align actual descendant flow lines" do
    for alignment <- ["first baseline", "last baseline"], nested? <- [false, true] do
      large = if nested?, do: "<div>Large</div>", else: "Large"

      layout =
        layout("""
        <div style='display:flex;width:200pt;align-items:#{alignment}'>
          <div style='font-size:10pt;line-height:15pt;padding:3pt'>Small<br>Tail</div>
          <div style='font-size:20pt;line-height:25pt;padding:7pt'>#{large}</div>
        </div>
        """)

      target = if alignment == "first baseline", do: "Small", else: "Tail"
      small = Enum.find(layout.boxes, &(Map.get(&1, :text) == target))
      large = Enum.find(layout.boxes, &(Map.get(&1, :text) == "Large"))
      assert_in_delta small.y, large.y, 0.75 / 64
    end
  end

  test "grid items in a shared row align first and last baselines" do
    for alignment <- ["first baseline", "last baseline"] do
      layout =
        layout("""
        <div style='display:grid;grid-template-columns:100pt 100pt;grid-template-rows:80pt;align-items:#{alignment}'>
          <div style='font-size:10pt;line-height:15pt;padding:3pt'>Small<br>Tail</div>
          <div style='font-size:20pt;line-height:25pt;padding:7pt'>Large</div>
        </div>
        """)

      target = if alignment == "first baseline", do: "Small", else: "Tail"
      small = Enum.find(layout.boxes, &(Map.get(&1, :text) == target))
      large = Enum.find(layout.boxes, &(Map.get(&1, :text) == "Large"))
      assert_in_delta small.y, large.y, 0.75 / 64
    end
  end

  test "stretch obeys cross-axis minimum and maximum constraints" do
    layout =
      layout("""
      <div style='display:flex;width:200pt;height:100pt'>
        <div style='width:30pt;max-height:40pt;background:red'></div>
        <div style='width:30pt;min-height:120pt;background:blue'></div>
      </div>
      """)

    [first, second] = Enum.filter(layout.boxes, &(&1.type == :rect))
    assert first.height == 40
    assert second.height == 120
  end

  test "place longhands participate in cascade and invalid typed alignments return diagnostics" do
    layout =
      layout("""
      <style>.box {place-items:start center;justify-items:end}</style>
      <div class='box' style='display:grid;width:100pt;height:100pt;grid-template-columns:100pt;grid-template-rows:100pt'>
        <div style='width:20pt;height:10pt;background:red'></div>
      </div>
      """)

    [item] = Enum.filter(layout.boxes, &(&1.type == :rect))
    assert item.x == 80
    assert item.y + item.height == 300

    assert {:error, {:invalid_layout, diagnostic}} =
             CssBoxAlignmentValidator.validate_style(%{align_items: {:safe, :unknown}}, "panel")

    assert %{stage: :layout, reason: :invalid_layout, source: "panel"} = diagnostic
    assert diagnostic.message =~ "align-items"
  end

  test "typed alignment and gap records reject malformed values before layout" do
    assert "place-content" in CssBoxAlignmentValidator.properties()

    for {property, value} <- [
          {"align-items", "baseline"},
          {"justify-self", "left"},
          {"justify-items", "right"},
          {"align-self", "self-start"},
          {"justify-self", "self-end"}
        ] do
      assert {:ok, _} = CssBoxAlignmentValidator.prepare(property, value)
    end

    assert :ok =
             CssBoxAlignmentValidator.validate_style(
               %{align_content: :none, justify_content: :none},
               "defaults"
             )

    for style <- [%{align_items: 3}, %{align_content: :auto}, %{justify_content: :baseline}] do
      assert {:error, {:invalid_layout, _}} =
               CssBoxAlignmentValidator.validate_style(style, "alignment")
    end

    for style <- [%{row_gap: -1}, %{column_gap: :auto}, %{_column_gap_normal: 1}] do
      assert {:error, {:invalid_layout, %{source: "gutters"}}} =
               CssBoxAlignmentValidator.validate_gaps(style, "gutters")
    end

    assert {:error, {:invalid_layout, _}} =
             CssBoxAlignmentValidator.compute_gaps(%{
               row_gap: {:css_length_math, {"broken(", false, :padding}}
             })
  end

  defp layout(html) do
    assert {:ok, dom} = HtmlParser.parse(html)
    assert {:ok, styled} = Style.compute(dom, system_font_discovery: false)
    assert {:ok, layout} = Layout.layout(styled, page_size: {300, 300}, margin: 0)
    layout
  end
end
