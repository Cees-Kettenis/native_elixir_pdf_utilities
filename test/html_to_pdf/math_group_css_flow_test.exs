defmodule NativeElixirPdfUtilities.HtmlToPdf.MathGroupCssFlowTest do
  use ExUnit.Case, async: false

  alias NativeElixirPdfUtilities.{HtmlToPdf, Text}
  alias NativeElixirPdfUtilities.HtmlToPdf.{Font, HtmlParser, MathLayout, ResourceContext, Style}
  alias NativeElixirPdfUtilities.Validators.{HtmlValidator, MathLayoutValidator}

  defp measured(html) do
    ResourceContext.run([], HtmlValidator.new_image_budget(), fn _ ->
      with {:ok, dom} <- HtmlParser.parse_detailed(html),
           {:ok, %{children: [node]}} <- Style.compute_detailed(dom, system_font_discovery: false) do
        {:ok, node.style._math_box}
      end
    end)
  end

  test "ordinary CSS groups use native owner dimensions and preserve selected Math children" do
    for tag <- ["mrow", "mstyle", "merror", "custom-group"],
        display <- ["inline", "inline-block", "block"] do
      html =
        "<math style='font-size:20pt'><#{tag} id='source-group' style='display:#{display};width:50pt;height:40pt;box-sizing:border-box;padding:4pt;border:1pt solid blue;background:yellow'><mn style='display:inline math;math-depth:0;font-size:20pt'>12</mn></#{tag}></math>"

      assert {:ok, box} = measured(html)
      assert_in_delta box.width, 50, 0.012
      assert_in_delta box.ascent + box.descent, 40, 0.012
      assert box.actual_text == "12"

      backgrounds =
        for %{kind: :native, box: %{type: :rect, fill_color: {1, 1, 0}} = rect} <-
              box.drawing_items,
            do: rect

      assert length(backgrounds) == 1
      assert_in_delta hd(backgrounds).width, 50, 0.012
      assert {:ok, pdf} = HtmlToPdf.render(html, system_font_discovery: false)
      assert {:ok, "12"} = Text.extract(pdf, layout: false)
    end
  end

  test "ordinary group flow wraps atomic children and applies shared min/max and CSS calculations" do
    children =
      "<mn style='display:inline math;math-depth:0;font-size:20pt'>12</mn><mn style='display:inline math;math-depth:0;font-size:20pt'>34</mn>"

    assert {:ok, natural} =
             measured(
               "<math style='font-size:20pt'><mrow style='display:inline-block'>#{children}</mrow></math>"
             )

    assert {:ok, constrained} =
             measured(
               "<math style='font-size:20pt'><mrow style='display:inline;width:min(30pt,40pt);max-width:25pt;min-height:40pt'>#{children}</mrow></math>"
             )

    assert_in_delta constrained.width, 25, 0.012
    assert constrained.ascent + constrained.descent > natural.ascent + natural.descent
    assert constrained.ascent + constrained.descent >= 40
    assert constrained.actual_text == "1234"
  end

  test "nested ordinary groups retain each real owner exactly once" do
    html =
      "<math style='font-size:20pt'><mrow style='display:inline-block;padding:2pt;background:yellow'><mstyle style='display:inline-block;padding:3pt;background:blue'><mn style='display:inline math;font-size:20pt'>12</mn></mstyle></mrow></math>"

    assert {:ok, nested} = measured(html)

    assert {:ok, inner} =
             measured(
               "<math style='font-size:20pt'><mstyle style='display:inline-block;padding:3pt;background:blue'><mn style='display:inline math;font-size:20pt'>12</mn></mstyle></math>"
             )

    assert_in_delta nested.width - inner.width, 4, 0.012

    for color <- [{1, 1, 0}, {0, 0, 1}] do
      assert 1 ==
               Enum.count(nested.drawing_items, fn item ->
                 item.kind == :native and Map.get(item.box, :fill_color) == color
               end)
    end
  end

  test "CSS group flex and grid keep equation children and native decoration" do
    for display <- ["flex", "inline-flex", "grid", "inline-grid"] do
      html =
        "<math style='font-size:20pt'><mrow style='display:#{display};gap:3pt;padding:2pt;background:yellow'><mn style='display:inline math;math-depth:0;font-size:20pt'>12</mn><mn style='display:inline math;math-depth:0;font-size:20pt'>34</mn></mrow></math>"

      assert {:ok, box} = measured(html)
      assert box.actual_text == "1234"
      assert box.width > 4
      assert {:ok, pdf} = HtmlToPdf.render(html, system_font_discovery: false)
      assert {:ok, "1234"} = Text.extract(pdf, layout: false)
    end
  end

  test "ordinary CSS parents retain ignored inline dimensions on nested groups and mtext" do
    for child <- ["mstyle", "mtext"] do
      contents = if child == "mtext", do: "12", else: "<mn style='display:inline math'>12</mn>"

      html =
        "<math style='font-size:20pt'><mrow style='display:inline-block'><#{child} style='display:inline;padding:0'>#{contents}</#{child}></mrow></math>"

      sized =
        String.replace(
          html,
          "display:inline;padding:0",
          "display:inline;padding:0;width:100pt;height:80pt"
        )

      assert {:ok, natural} = measured(html)
      assert {:ok, constrained} = measured(sized)
      assert constrained.width == natural.width
      assert constrained.ascent == natural.ascent
      assert constrained.descent == natural.descent
      assert constrained.actual_text == "12"
    end
  end

  test "native CSS groups retain actual operator and radical artwork and logical source" do
    html =
      "<math style='font-size:20pt'><mrow style='display:inline-block;padding:2pt;background:yellow'><mrow><mo>(</mo><mfrac><mn>1</mn><mn>2</mn></mfrac><mo>)</mo></mrow><msqrt><mn>3</mn></msqrt></mrow></math>"

    assert {:ok, box} = measured(html)
    assert box.actual_text == "(12)3"
    assert Enum.any?(box.drawing_items, &(&1.kind == :native))
    assert {:ok, pdf} = HtmlToPdf.render(html, system_font_discovery: false)
    assert {:ok, "(12)3"} = Text.extract(pdf, layout: false)
  end

  test "ordinary group validation delegates malformed CSS sizes and retains actionable diagnostics" do
    {:ok, registry} = Font.load_registry(system_font_discovery: false)
    {:ok, _, font} = Font.resolve("math", 400, :normal, registry)

    for tag <- ["mrow", "mstyle", "merror", "custom-group"] do
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
        tag: tag,
        attributes: %{},
        children: [],
        style: style
      }

      assert {:error, {reason, diagnostic}} = MathLayoutValidator.prepare_element(node, %{})
      assert diagnostic.reason == reason
      assert diagnostic.stage == :layout
      assert diagnostic.source == tag
      assert is_binary(diagnostic.message)
    end
  end

  test "source display remains intact when native CSS owner measurement blockifies an inline group" do
    ResourceContext.run([], HtmlValidator.new_image_budget(), fn _ ->
      {:ok, dom} =
        HtmlParser.parse_detailed(
          "<math><mrow style='display:inline;width:50pt;padding:2pt;background:yellow'><mn>12</mn></mrow></math>"
        )

      {:ok, %{children: [math]}} = Style.compute_detailed(dom, system_font_discovery: false)
      # Reprepare the actual parsed source in a containing Math context.
      group = %{
        type: :element,
        namespace: :mathml,
        tag: "mrow",
        attributes: %{},
        style:
          math.style
          |> Map.drop([:_math_box])
          |> Map.merge(%{display: :inline, _math_formatting: false, width: 50.0, height: nil}),
        children: []
      }

      assert {:ok, prepared} =
               MathLayoutValidator.prepare_element(group, %{
                 containing_formatting_context: :math,
                 resource_context: ResourceContext.current()
               })

      assert {:ok, request} =
               HtmlValidator.prepare_measured_child(group, ResourceContext.current(), %{
                 formatting_context: :math
               })

      assert request.node.style.display == :inline
      assert request.used_display == :inline_block
      assert prepared.outer_box_measured
      assert group.style.display == :inline

      assert {:ok, box} =
               MathLayout.measure(group, %{
                 containing_formatting_context: :math,
                 resource_context: ResourceContext.current()
               })

      assert_in_delta box.width, 50, 0.012
    end)
  end
end
