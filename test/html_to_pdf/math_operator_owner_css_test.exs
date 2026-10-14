defmodule NativeElixirPdfUtilities.HtmlToPdf.MathOperatorOwnerCssTest do
  use ExUnit.Case, async: false

  alias NativeElixirPdfUtilities.{HtmlToPdf, Text}
  alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, ResourceContext, Style}
  alias NativeElixirPdfUtilities.Validators.HtmlValidator

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

  test "operator owners resolve canonical dimensions and ordinary composition once" do
    owner =
      "<mo lspace='0' rspace='0' stretchy='false' style='width:min(44pt,60pt);height:30pt;box-sizing:border-box;padding:3pt;border:1px solid blue;background:yellow'>+</mo>"

    assert {:ok, natural} = box("<math style='font-size:20pt'><mo stretchy='false'>+</mo></math>")

    for wrapped <- [owner, "<mrow style='display:inline-block'>#{owner}</mrow>"] do
      assert {:ok, measured} = box("<math style='font-size:20pt'>#{wrapped}</math>")
      assert_in_delta measured.width, 44, 0.012
      assert_in_delta measured.ascent + measured.descent, 30, 0.012
      assert measured.actual_text == "+"

      glyphs =
        measured.glyphs ++
          for(
            %{kind: :native, box: native} <- measured.drawing_items,
            command <- text_commands(native),
            glyph <- command.glyph_ids,
            do: %{glyph_id: glyph, font_face: command.font_face}
          )

      assert Enum.map(glyphs, & &1.glyph_id) == Enum.map(natural.glyphs, & &1.glyph_id)
      assert Enum.all?(glyphs, &(&1.font_face.data == hd(natural.glyphs).font_face.data))
    end
  end

  test "positioned and clipped operator owners retain actual selected glyph leaves" do
    assert {:ok, measured} =
             box(
               "<math><mo lspace='0' rspace='0' stretchy='false' style='position:relative;left:3pt;top:2pt;width:8pt;height:12pt;padding:2pt;overflow:hidden;background:yellow'>(</mo></math>"
             )

    assert_in_delta measured.width, 12, 0.012

    commands =
      for %{kind: :native, box: native} <- measured.drawing_items,
          command <- text_commands(native),
          do: command

    assert length(commands) == 1
    assert hd(commands).glyph_ids == [1064]
    assert Map.has_key?(hd(commands), :clip)
    assert measured.actual_text == "("

    assert {:ok, pdf} =
             HtmlToPdf.render(
               "<math><mo lspace='0' rspace='0' style='min-width:24pt;max-width:20pt'>+</mo></math>"
             )

    assert {:ok, "+"} = Text.extract(pdf, layout: false)
  end

  test "operator and radical source backgrounds use the real decoded image painter" do
    image =
      "data:image/svg+xml;base64," <>
        Base.encode64(
          "<svg xmlns='http://www.w3.org/2000/svg' width='8' height='8'><rect width='8' height='8' fill='blue'/></svg>"
        )

    for {tag, children} <- [
          {"mo", "+"},
          {"msqrt", "<mn>2</mn>"},
          {"mroot", "<mn>2</mn><mn>3</mn>"}
        ] do
      html =
        "<math><#{tag} style='width:30pt;height:30pt;background-image:url(#{image});background-repeat:no-repeat;background-size:8pt 8pt'>#{children}</#{tag}></math>"

      assert {:ok, measured} = box(html)

      assert Enum.any?(
               measured.drawing_items,
               &(&1.kind == :background and is_map(&1.style.background_image))
             )

      assert {:ok, pdf} = HtmlToPdf.render(html, system_font_discovery: false)
      assert {:ok, text} = Text.extract(pdf, layout: false)
      assert text == if(tag == "mo", do: "+", else: if(tag == "msqrt", do: "2", else: "23"))
    end
  end

  test "operator owner constraints use canonical diagnostics" do
    assert {:error, {:unsupported_html, diagnostic}} =
             HtmlToPdf.render("<math><mo style='position:absolute'>+</mo></math>")

    assert diagnostic.stage == :layout and diagnostic.source == "mo"
    assert diagnostic.message =~ "static or relative"
  end

  @tag :browser_parity
  @tag :math_operator_owner_css_original
  test "operator source CSS boxes compare with original Chromium" do
    fixture =
      Path.expand(
        "../fixtures/html_to_pdf/browser_parity/mathml_operator_css_owners.html",
        __DIR__
      )

    NativeElixirPdfUtilities.TestSupport.PdfVisualCompare.assert_browser_match!(fixture,
      artifact_dir: "tmp/browser_parity/mathml_operator_css_owners",
      max_changed_ratio: 0.01,
      max_average_delta: 0.01
    )
  end
end
