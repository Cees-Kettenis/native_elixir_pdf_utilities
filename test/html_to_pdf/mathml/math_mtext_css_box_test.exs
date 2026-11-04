defmodule NativeElixirPdfUtilities.HtmlToPdf.MathMtextCssBoxTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.{HtmlToPdf, Text}
  alias NativeElixirPdfUtilities.HtmlToPdf.{Font, HtmlParser, ResourceContext, Style}
  alias NativeElixirPdfUtilities.Validators.{HtmlValidator, MathLayoutValidator}

  defp measure(css) do
    html =
      "<math><mtable style='width:180pt'><mtr><mtd style='padding:0'><mtext id='actual-token' style='font-family:DejaVu Sans;font-size:12pt;line-height:18pt;background:yellow;#{css}'><span>Alpha beta gamma delta epsilon</span></mtext></mtd></mtr></mtable></math>"

    ResourceContext.run([], HtmlValidator.new_image_budget(), fn _ ->
      with {:ok, dom} <- HtmlParser.parse_detailed(html),
           {:ok, %{children: [math]}} <- Style.compute_detailed(dom, system_font_discovery: false) do
        {:ok, math.style._math_box, html}
      end
    end)
  end

  test "ordinary inline and inline-block mtext blockify under Math and measure their CSS boxes" do
    for display <- ["inline", "inline-block"] do
      assert {:ok, box, html} =
               measure("display:#{display};width:60pt;padding:2pt;border:1px solid blue")

      backgrounds =
        for %{kind: :native, box: %{type: :rect} = rect} <- box.drawing_items, do: rect

      assert Enum.any?(backgrounds, &(abs(&1.width - 65.5) < 1.0e-9))
      assert_in_delta box.ascent + box.descent, 95.5, 1.0e-9
      assert {:ok, pdf} = HtmlToPdf.render(html, system_font_discovery: false)
      assert {:ok, "Alpha beta gamma delta epsilon"} = Text.extract(pdf, layout: false)
    end
  end

  test "allocated token percentages and shared min/max and border-box constraints paint once" do
    for {css, expected_width, expected_height} <- [
          {"display:inline-block;width:60%;padding:2pt;border:1px solid blue", 113.5, 59.5},
          {"display:inline-block;width:60pt;height:36pt;box-sizing:border-box;padding:2pt;border:1px solid blue",
           60, 36},
          {"display:inline;width:min(70pt,60pt);min-width:20pt;max-width:80pt;padding:2pt;border:1px solid blue",
           65.5, 95.5},
          {"display:inline;width:120pt;max-width:80pt;min-height:72pt;padding:2pt", 84, 76}
        ] do
      assert {:ok, box, _} = measure(css)

      backgrounds =
        for %{kind: :native, box: %{type: :rect, fill_color: {1, 1, 0}} = rect} <-
              box.drawing_items,
            do: rect

      assert length(backgrounds) == 1
      assert_in_delta hd(backgrounds).width, expected_width, 0.012
      assert_in_delta hd(backgrounds).height, expected_height, 0.012

      if String.contains?(css, "width:60%") do
        assert [%{kind: :native, x: x}] =
                 Enum.filter(box.drawing_items, fn item ->
                   item.kind == :native and Map.get(item.box, :fill_color) == {1, 1, 0}
                 end)

        assert_in_delta x, 0, 1.0e-9
      end
    end
  end

  test "ordinary CSS token size failures retain shared validator diagnostics" do
    {:ok, registry} = Font.load_registry(system_font_discovery: false)
    {:ok, _, font} = Font.resolve("math", 400, :normal, registry)

    style = %{
      display: :inline,
      _math_formatting: false,
      width: {:percent, :bad},
      font_size: 18,
      font_face: font,
      color: {0, 0, 0},
      line_height: 24
    }

    node = %{
      type: :element,
      namespace: :mathml,
      tag: "mtext",
      attributes: %{},
      children: [],
      style: style
    }

    assert {:error, {reason, diagnostic}} = MathLayoutValidator.prepare_element(node, %{})
    assert diagnostic.reason == reason
    assert diagnostic.stage == :layout
    assert is_binary(diagnostic.message)
    assert diagnostic.source == "mtext"
  end
end
