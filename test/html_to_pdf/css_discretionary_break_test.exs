defmodule NativeElixirPdfUtilities.HtmlToPdf.CssDiscretionaryBreakTest do
  use ExUnit.Case
  alias NativeElixirPdfUtilities.HtmlToPdf.{Font, HtmlParser, Layout, ResourceContext, Style}
  alias NativeElixirPdfUtilities.Validators.{HtmlLineBreakValidator, HtmlValidator}
  alias NativeElixirPdfUtilities.Limits

  test "the selected genuine font paints a conditional hyphen only at a taken opportunity" do
    assert {:ok, registry} = Font.load_registry(system_font_discovery: false)
    assert {:ok, _, face} = Font.resolve("DejaVu Sans", 400, :normal, registry)

    for {source, selected, visible} <- [
          {"co\u00ADoperate", false, "cooperate"},
          {"of\u00ADfice", false, "office"},
          {"co\u00AD", true, "co\u2010"},
          {"co\u00AD ", true, "co "},
          {"A\u200BB", false, "AB"}
        ] do
      assert {:ok, %{text: ^visible, actual_text: ^source, discretionary?: true}} =
               HtmlLineBreakValidator.display_plan(source, face, selected)
    end

    assert {:ok, true} = HtmlLineBreakValidator.transparent_format_boundary?("office", "\u00AD")
    assert {:ok, true} = HtmlLineBreakValidator.transparent_format_boundary?("office\u00AD", "x")
    assert {:ok, false} = HtmlLineBreakValidator.transparent_format_boundary?("東京", "京都")
    assert Font.supports_text?(face, "\u2010")
    assert {:ok, ["co\u00AD", "operate"]} = HtmlLineBreakValidator.segments("co\u00ADoperate")
    assert Font.text_width("co\u00ADoperate", face, 12) > Font.text_width("cooperate", face, 12)
  end

  test "display-plan failures use diagnostics and the shared work budget" do
    assert {:ok, registry} = Font.load_registry(system_font_discovery: false)
    assert {:ok, _, face} = Font.resolve("DejaVu Sans", 400, :normal, registry)

    for {text, option} <- [{nil, false}, {<<255>>, false}, {"word", :invalid}] do
      assert {:error, {:invalid_layout, diagnostic}} =
               HtmlLineBreakValidator.display_plan(text, face, option)

      assert diagnostic.stage == :layout
      assert diagnostic.operation == :line_break
      assert diagnostic.message =~ "UTF-8" or diagnostic.message =~ "boolean"
    end

    assert {:error, {:invalid_layout, diagnostic}} =
             HtmlLineBreakValidator.display_plan("co\u00AD", nil, true)

    assert diagnostic.message =~ "selected font"

    assert {:error, {:invalid_layout, invalid}} =
             HtmlLineBreakValidator.transparent_format_boundary?("word", <<255>>)

    assert invalid.operation == :line_break and invalid.message =~ "UTF-8"
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    Limits.install(Map.put(limits, :max_layout_text_work, 1))

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             HtmlLineBreakValidator.display_plan("word", face)

    assert diagnostic.stage == :layout
    assert diagnostic.message =~ "max_layout_text_work"
  end

  test "ordinary soft wraps paint actual Unicode hyphens and preserve exact source selection" do
    for {content, width, visible} <- [
          {"co&shy;operate of&shy;fice", 180, ["cooperate office"]},
          {"co&shy;operate", 45, ["co‐", "operate"]},
          {"co<span>&shy;</span>operate", 45, ["co‐", "operate"]},
          {"co&shy;operate", 14, ["co‐", "operate"]},
          {"co&shy;<br>operate", 180, ["co", "operate"]}
        ] do
      html = "<p style='margin:0;width:#{width}pt;font-size:12pt;line-height:24pt'>#{content}</p>"
      assert {:ok, dom} = HtmlParser.parse_detailed(html)
      assert {:ok, styled} = Style.compute_detailed(dom, system_font_discovery: false)
      assert {:ok, layout} = Layout.layout(styled, page_size: {240, 240}, margin: 0)
      glyphs = texts(layout.boxes)

      lines =
        glyphs
        |> Enum.group_by(& &1.y)
        |> Enum.sort_by(fn {y, _} -> -y end)
        |> Enum.map(fn {_y, boxes} -> Enum.map_join(boxes, & &1.text) end)

      assert lines == visible

      assert Enum.any?(
               layout.boxes,
               &(&1.type == :glyph_group and String.contains?(&1.actual_text, "\u00AD"))
             )

      assert Enum.all?(glyphs, &(not String.contains?(&1.text, ["\u00AD", "\u200B"])))
    end
  end

  test "zero-width formatting characters do not change letter-spaced glyph positions or advance" do
    for source <- ["AB CD", "A\u200BB C\u00ADD"] do
      assert {:ok, dom} =
               HtmlParser.parse_detailed("<p style='margin:0;letter-spacing:2pt'>#{source}</p>")

      assert {:ok, styled} = Style.compute_detailed(dom, system_font_discovery: false)
      assert {:ok, layout} = Layout.layout(styled, page_size: {240, 240}, margin: 0)
      [box] = texts(layout.boxes)
      assert box.text == "AB CD"

      assert box.annotation_width ==
               Font.css_text_width("AB CD", box.font_face, box.font_size, 2.0) + 8.0
    end
  end

  test "PDF selection retains actual source characters without selecting a generated hyphen" do
    html =
      "<p style='margin:0;width:180pt'>of&shy;fice A&#x200B;B</p><p style='margin:0;width:45pt'>co&shy;operate</p>"

    assert {:ok, pdf} =
             NativeElixirPdfUtilities.HtmlToPdf.render(html,
               system_font_discovery: false,
               compress_streams: false
             )

    assert {:ok, text} = NativeElixirPdfUtilities.Text.extract(pdf, layout: false)
    assert text =~ "of\u00ADfice A\u200BB"
    assert text =~ "co\u00AD"
    refute text =~ "\u2010"
  end

  test "intrinsic soft-hyphen widths include a real hyphen only at a discretionary boundary" do
    ResourceContext.run([], HtmlValidator.new_image_budget(), fn context ->
      for content <- ["office&shy;x", "office<span>&shy;</span>x"] do
        assert {:ok, dom} =
                 HtmlParser.parse_detailed("<span style='font-size:12pt'>#{content}</span>")

        assert {:ok, %{children: [node]}} = Style.compute_detailed(dom, [], context.image_budget)
        assert {:ok, measured} = Layout.measure_child(node, context)

        assert_in_delta measured.min_content_width,
                        Font.css_text_width("office‐", node.style.font_face, 12),
                        1.0e-9

        assert_in_delta measured.max_content_width,
                        Font.css_text_width("officex", node.style.font_face, 12),
                        1.0e-9
      end
    end)
  end

  defp texts(boxes) do
    Enum.flat_map(boxes, fn
      %{type: :text} = box -> [box]
      %{type: :glyph_group, boxes: children} -> texts(children)
      _ -> []
    end)
  end
end
