defmodule NativeElixirPdfUtilities.HtmlToPdf.MathRadicalOwnerCssTest do
  use ExUnit.Case, async: false

  alias NativeElixirPdfUtilities.{HtmlToPdf, Text}
  alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, ResourceContext, Style}
  alias NativeElixirPdfUtilities.Validators.{HtmlValidator, MathLayoutValidator}

  defp box(html) do
    ResourceContext.run([], HtmlValidator.new_image_budget(), fn _ ->
      with {:ok, dom} <- HtmlParser.parse_detailed(html),
           {:ok, %{children: [node]}} <- Style.compute_detailed(dom, system_font_discovery: false),
           do: {:ok, node.style._math_box}
    end)
  end

  defp text_commands(box) do
    Enum.flat_map(Map.get(box, :boxes, []), &text_commands/1) ++
      if(Map.get(box, :type) == :text and Map.has_key?(box, :glyph_ids), do: [box], else: [])
  end

  test "default radical owners resolve shared CSS constraints while retaining selected glyphs" do
    for tag <- ~w(msqrt mroot) do
      children = "<mn>12</mn>" <> if(tag == "mroot", do: "<mn>3</mn>", else: "")

      assert {:ok, natural} =
               box("<math style='font-size:20pt'><#{tag}>#{children}</#{tag}></math>")

      html =
        "<math style='font-size:20pt'><#{tag} style='width:min(70pt,80pt);min-width:60pt;max-width:75pt;height:42pt;box-sizing:border-box;padding:3pt;border:1px solid blue;background:yellow'>#{children}</#{tag}></math>"

      assert {:ok, measured} = box(html)
      assert_in_delta measured.width, 70, 0.012
      assert_in_delta measured.ascent + measured.descent, 42, 0.012
      assert Enum.map(measured.glyphs, & &1.glyph_id) == Enum.map(natural.glyphs, & &1.glyph_id)

      assert Enum.map(measured.glyphs, & &1.font_face.data) ==
               Enum.map(natural.glyphs, & &1.font_face.data)

      assert [%{width: width, height: height}] =
               Enum.filter(measured.drawing_items, &(&1.kind == :background))

      assert_in_delta width, 70, 0.012
      assert_in_delta height, 42, 0.012
      assert {:ok, pdf} = HtmlToPdf.render(html, system_font_discovery: false)
      assert {:ok, text} = Text.extract(pdf, layout: false)
      assert text == if(tag == "mroot", do: "123", else: "12")
    end
  end

  test "relative clipped radical owners preserve glyph commands and logical children" do
    for tag <- ~w(msqrt mroot) do
      children = "<mn>123456</mn>" <> if(tag == "mroot", do: "<mn>3</mn>", else: "")
      assert {:ok, natural} = box("<math><#{tag}>#{children}</#{tag}></math>")

      html =
        "<math><#{tag} style='width:25pt;height:24pt;position:relative;left:3pt;top:2pt;overflow:hidden;padding:2pt;background:yellow'>#{children}</#{tag}></math>"

      assert {:ok, measured} = box(html)
      assert_in_delta measured.width, 29, 0.012
      assert measured.actual_text == natural.actual_text

      commands =
        for %{kind: :native, box: native} <- measured.drawing_items,
            command <- text_commands(native),
            do: command

      assert Enum.flat_map(commands, & &1.glyph_ids) == Enum.map(natural.glyphs, & &1.glyph_id)

      assert Enum.map(commands, & &1.font_face.data) ==
               Enum.map(natural.glyphs, & &1.font_face.data)

      assert Enum.any?(commands, &Map.has_key?(&1, :clip))
      assert {:ok, pdf} = HtmlToPdf.render(html, system_font_discovery: false)
      assert {:ok, text} = Text.extract(pdf, layout: false)
      assert text == natural.actual_text
    end
  end

  test "ordinary CSS composition consumes completed radical owner boxes once" do
    for tag <- ~w(msqrt mroot) do
      children = "<mn>12</mn>" <> if(tag == "mroot", do: "<mn>3</mn>", else: "")

      owner =
        "<#{tag} style='width:70pt;height:42pt;box-sizing:border-box;padding:3pt;border:1px solid blue;background:yellow'>#{children}</#{tag}>"

      assert {:ok, direct} = box("<math>#{owner}</math>")

      assert {:ok, composed} =
               box("<math><mrow style='display:inline-block'>#{owner}</mrow></math>")

      assert_in_delta composed.width, direct.width, 0.012
      assert_in_delta composed.ascent + composed.descent, direct.ascent + direct.descent, 0.012
      assert composed.actual_text == direct.actual_text
      assert Enum.any?(composed.drawing_items, &(&1.kind == :native))
    end
  end

  test "definite radical owner widths supply the descendant CSS percentage basis" do
    html =
      "<math><msqrt style='width:100pt'><mtext style='display:inline-block;width:50%;font-family:DejaVu Sans;font-size:12pt;line-height:18pt;background:yellow'><span>Alpha beta gamma delta</span></mtext></msqrt></math>"

    assert {:ok, measured} = box(html)

    backgrounds =
      for %{kind: :native, box: %{type: :rect, fill_color: {1, 1, 0}} = rect} <-
            measured.drawing_items,
          do: rect

    assert length(backgrounds) == 1
    assert_in_delta hd(backgrounds).width, 50, 0.012
    assert_in_delta measured.width, 100, 0.012
    assert measured.actual_text == "Alpha beta gamma delta"
  end

  test "radical constraints preserve the canonical CSS minimum precedence and diagnostics" do
    assert {:ok, measured} =
             box(
               "<math><msqrt style='width:30pt;min-width:80pt;max-width:60pt;min-height:40pt'><mn>2</mn></msqrt></math>"
             )

    assert_in_delta measured.width, 80, 0.012
    assert measured.ascent + measured.descent >= 40
    assert :ok = MathLayoutValidator.validate_measured_geometry(measured)

    assert {:error, {reason, diagnostic}} =
             MathLayoutValidator.validate_measured_geometry(
               Map.put(measured, :css_math_owner, :forged)
             )

    assert diagnostic.reason == reason and diagnostic.stage == :layout
    assert diagnostic.operation == :measure_math and diagnostic.message =~ "math boxes"

    assert {:error, {reason, diagnostic}} =
             HtmlToPdf.render("<math><msqrt style='position:absolute'><mn>2</mn></msqrt></math>")

    assert diagnostic.reason == reason and diagnostic.stage == :layout
    assert diagnostic.source == "msqrt" and diagnostic.message =~ "positioned"
  end
end
