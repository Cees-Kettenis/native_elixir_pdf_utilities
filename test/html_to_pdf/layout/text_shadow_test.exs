defmodule NativeElixirPdfUtilities.HtmlToPdf.TextShadowTest do
  use ExUnit.Case, async: true
  alias NativeElixirPdfUtilities.{HtmlToPdf, Text}
  alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, Layout, Style}
  alias NativeElixirPdfUtilities.Validators.CssTextShadowValidator

  test "shadow grammar preserves order, signed offsets, colors and resolved font lengths" do
    assert {:ok, %{text_shadow: [first, second]}} =
             CssTextShadowValidator.prepare(
               %{font_size: 12},
               "text-shadow",
               "red -1em 2pt 3pt, 1pt -2pt"
             )

    assert first == %{x: -12.0, y: 2.0, blur: 3.0, color: {1, 0, 0}}
    assert second == %{x: 1.0, y: -2.0, blur: 0.0, color: :current_color}
    assert {:ok, %{text_shadow: []}} = CssTextShadowValidator.prepare(%{}, "text-shadow", "none")

    for value <- [
          "",
          "red",
          "1pt",
          "1pt 2pt -1pt",
          "1pt 2pt 3pt 4pt",
          "red blue 1pt 2pt",
          "10% 2pt",
          "1pt 2pt,"
        ] do
      assert :error = CssTextShadowValidator.prepare(%{font_size: 12}, "text-shadow", value)
    end

    assert :error = CssTextShadowValidator.prepare(%{}, "other", "none")
    assert :ok = CssTextShadowValidator.validate_style(%{}, "p")

    assert {:error, {:invalid_layout, diagnostic}} =
             CssTextShadowValidator.validate_style(
               %{text_shadow: [%{x: 1, y: 2, blur: -1, color: {0, 0, 0}}]},
               "p"
             )

    assert diagnostic.stage == :layout and diagnostic.source == "p"
  end

  test "explicit current color and malformed prepared shadow records use the shared admission rules" do
    assert CssTextShadowValidator.properties() == ["text-shadow"]

    assert {:ok, %{text_shadow: [shadow]}} =
             CssTextShadowValidator.prepare(%{}, "text-shadow", "CURRENTCOLOR 1pt 2pt")

    assert shadow == %{color: :current_color, x: 1.0, y: 2.0, blur: 0.0}

    for shadows <- [
          :bad,
          [nil],
          [%{x: :bad, y: 0, blur: 0, color: :current_color}],
          [%{x: 0, y: 0, blur: 0, color: :bad}]
        ] do
      assert {:error, {:invalid_layout, diagnostic}} =
               CssTextShadowValidator.validate_style(%{text_shadow: shadows}, "p")

      assert diagnostic.stage == :layout and diagnostic.operation == :layout and
               diagnostic.source == "p"

      assert diagnostic.message =~ "text-shadow"
    end
  end

  test "blur on a hand-styled built-in PDF font returns an actionable font diagnostic" do
    style = %{
      font_face: %{type: :built_in},
      font_size: 12,
      text_shadow: [%{x: 1, y: 2, blur: 1, color: {0, 0, 0}}]
    }

    assert {:error, {:invalid_document, diagnostic}} =
             CssTextShadowValidator.validate_style(style, "p")

    assert diagnostic.stage == :font and diagnostic.source == "p"
    assert diagnostic.message =~ "embedded font"
  end

  test "unblurred layers paint behind split text and preserve one logical source and link" do
    html =
      "<p style='font:18pt/26pt sans-serif;color:black;text-shadow:1pt 2pt blue,-2pt -1pt red'><a href='https://shadow.test/'>ABC<span style='color:green'>DEF</span></a></p>"

    assert {:ok, dom} = HtmlParser.parse(html)
    assert {:ok, styled} = Style.compute_detailed(dom, system_font_discovery: false)
    assert {:ok, layout} = Layout.layout(styled, margin: 0)

    groups =
      Enum.filter(layout.boxes, &(Map.get(&1, :role) == :text_shadow and &1.type == :glyph_group))

    assert length(groups) == 4
    assert Enum.map(Enum.take(groups, 2), &hd(&1.boxes).color) == [{1, 0, 0}, {0, 0, 1}]

    assert Enum.all?(
             groups,
             &(&1.actual_text == "" and not Map.has_key?(hd(&1.boxes), :link_url))
           )

    assert {:ok, pdf} = HtmlToPdf.render(html, system_font_discovery: false)
    assert {:ok, "ABCDEF"} = Text.extract(pdf, layout: false)
  end

  test "blurred outline shadows have alpha, inherit into descendants and keep source extraction" do
    html =
      "<p style='font:18pt/26pt sans-serif;text-shadow:2pt 3pt 2pt rgba(0,0,255,0.5)'>ABC<span>DEF</span></p>"

    assert {:ok, dom} = HtmlParser.parse(html)
    assert {:ok, styled} = Style.compute_detailed(dom, system_font_discovery: false)
    assert {:ok, layout} = Layout.layout(styled, margin: 0)
    shadows = Enum.filter(layout.boxes, &(Map.get(&1, :role) == :text_shadow))
    assert length(shadows) == 2
    assert Enum.all?(shadows, &(&1.type == :image and is_binary(&1.image.alpha_data)))
    assert {:ok, pdf} = HtmlToPdf.render(html, system_font_discovery: false)
    assert {:ok, "ABCDEF"} = Text.extract(pdf, layout: false)
  end

  test "sharp shadows recolor dotted and dashed decoration borders without copying logical text" do
    for pattern <- ["dotted", "dashed"] do
      html =
        "<p style='font:18pt/26pt sans-serif;text-decoration:underline #{pattern} 1.5pt red;text-shadow:1pt 2pt blue'>ABC</p>"

      assert {:ok, dom} = HtmlParser.parse(html)
      assert {:ok, styled} = Style.compute_detailed(dom, system_font_discovery: false)
      assert {:ok, layout} = Layout.layout(styled, margin: 0)

      [line] =
        Enum.filter(layout.boxes, &(Map.get(&1, :role) == :text_shadow and &1.type == :rect))

      assert Enum.all?(Map.values(line.border_colors), &(&1 == {0, 0, 1}))
      assert {:ok, pdf} = HtmlToPdf.render(html, system_font_discovery: false)
      assert {:ok, "ABC"} = Text.extract(pdf, layout: false)
    end
  end

  test "blurred non-solid decorations retain alpha and one logical text owner" do
    for pattern <- ["solid", "double", "dotted", "dashed", "wavy"] do
      html =
        "<p style='font:18pt/26pt sans-serif;text-decoration:underline #{pattern} 1.5pt red;text-shadow:1pt 2pt 1pt blue'>ABCDEF</p>"

      assert {:ok, dom} = HtmlParser.parse(html)
      assert {:ok, styled} = Style.compute_detailed(dom, system_font_discovery: false)
      assert {:ok, layout} = Layout.layout(styled, margin: 0)
      [shadow] = Enum.filter(layout.boxes, &(Map.get(&1, :role) == :text_shadow))
      assert shadow.type == :image
      assert is_binary(shadow.image.alpha_data)
      assert shadow.width > 0 and shadow.height > 0
      assert {:ok, pdf} = HtmlToPdf.render(html, system_font_discovery: false)
      assert {:ok, "ABCDEF"} = Text.extract(pdf, layout: false)
    end
  end

  test "large blur extents fail through the existing raster allowance before allocation" do
    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             HtmlToPdf.render("<p style='text-shadow:0 0 100000px blue'>Visible</p>",
               system_font_discovery: false
             )

    assert diagnostic.reason == :resource_limit_exceeded
    assert is_atom(diagnostic.stage)
    assert diagnostic.message =~ "max_raster_dimension"
  end
end
