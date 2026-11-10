defmodule NativeElixirPdfUtilities.HtmlToPdf.OverflowAxesTest do
  use ExUnit.Case, async: true
  alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, Layout, Style, OverflowGeometry}
  alias NativeElixirPdfUtilities.Validators.CssOverflowValidator
  alias NativeElixirPdfUtilities.TestSupport.CssStyles

  # CSS Overflow 3, 7 October 2025, sections 3.1 and 3.2.
  test "overflow shorthand and longhands cascade independently before cross-axis computation" do
    for {source, expected} <- [
          {"overflow:clip visible", {:clip, :visible}},
          {"overflow:visible clip", {:visible, :clip}},
          {"overflow:visible hidden", {:auto, :hidden}},
          {"overflow:clip auto", {:hidden, :auto}},
          {"overflow:scroll", {:scroll, :scroll}},
          {"overflow:hidden;overflow-x:visible", {:auto, :hidden}},
          {"overflow-x:clip!important;overflow:hidden", {:hidden, :hidden}}
        ] do
      style = CssStyles.by_id("<div id=a style='#{source}'></div>")["a"]
      assert CssOverflowValidator.axes(style) == expected

      assert CssOverflowValidator.independent?(style) ==
               (elem(expected, 0) not in [:clip, :visible] or
                  elem(expected, 1) not in [:clip, :visible])
    end
  end

  test "a visible axis includes actual italic outline overhang and transformed paint" do
    source =
      "<div style='width:40pt;height:10pt;overflow:clip visible'><span style='font-size:30pt;font-style:italic'>f</span><div style='height:50pt;width:20pt;background:blue'></div></div>"

    assert {:ok, dom} = HtmlParser.parse_detailed(source)
    assert {:ok, styled} = Style.compute_detailed(dom, system_font_discovery: false)
    assert {:ok, layout} = Layout.layout(styled, page_size: {200, 200}, margin: 0)
    blue = Enum.find(layout.boxes, &(Map.get(&1, :fill_color) == {0, 0, 1}))
    assert blue.clip.x == 0 and blue.clip.width == 40
    assert blue.clip.y < 150
    assert blue.y >= blue.clip.y

    rectangle = %{
      type: :rect,
      x: 0,
      y: 0,
      width: 10,
      height: 10,
      stroke_width: 2,
      css_transform_scopes: [%{matrix: {1, 0, 0, 1, -20, 30}}]
    }

    assert {left, bottom, right, top} = OverflowGeometry.ink_bounds([rectangle], {0, 0, 0, 0})
    assert left <= -21 and bottom == 0 and right == 0 and top >= 41
  end

  test "clip margins select the content padding or border edge and apply only to clip" do
    padding = %{top: 2, right: 3, bottom: 4, left: 5}
    borders = %{top: 1, right: 2, bottom: 3, left: 4}
    box = %{x: 10, y: 20, width: 100, height: 80}

    for {reference, expected} <- [
          {:padding_box, %{x: 7, y: 17, width: 106, height: 86}},
          {:content_box, %{x: 12, y: 21, width: 98, height: 80}},
          {:border_box, %{x: 3, y: 14, width: 112, height: 90}}
        ] do
      style = %{
        overflow: :clip,
        overflow_clip_margin: {reference, 3},
        padding: padding,
        border_widths: borders
      }

      assert OverflowGeometry.clip([], style, box) == expected
      assert OverflowGeometry.clip([], %{style | overflow: :hidden}, box) == box
    end

    assert OverflowGeometry.clip([], %{}, box) == nil

    style =
      CssStyles.by_id(
        "<div id=a style='font-size:10pt;overflow:clip;overflow-clip-margin:content-box 2em'></div>"
      )["a"]

    assert style.overflow_clip_margin == {:content_box, 20.0}
  end

  test "overflow grammar rejects malformed axes lengths and markers" do
    for {property, source} <- [
          {"overflow", ""},
          {"overflow", "visible hidden clip"},
          {"overflow-x", "hidden visible"},
          {"overflow-y", "unknown"},
          {"overflow", "2"},
          {"overflow-clip-margin", "-1pt"},
          {"overflow-clip-margin", "10%"},
          {"overflow-clip-margin", "content-box border-box"},
          {"overflow-clip-margin", "1pt 2pt"},
          {"text-overflow", "bad"},
          {"block-ellipsis", "auto auto"},
          {"max-lines", "0"},
          {"max-lines", "1.5"},
          {"continue", "scroll"},
          {"line-clamp", "0 auto"},
          {"unknown", "auto"}
        ] do
      assert :error = CssOverflowValidator.prepare(property, source), inspect({property, source})
    end

    assert {:ok, %{text_overflow: ".."}} = CssOverflowValidator.prepare("text-overflow", "'..'")

    assert {:ok, %{text_overflow: {:clip, "…"}}} =
             CssOverflowValidator.prepare("text-overflow", "clip ellipsis")

    assert {:ok, %{block_ellipsis: "…", max_lines: 2, continue: :discard}} =
             CssOverflowValidator.prepare("line-clamp", "2 auto")
  end

  test "overflow longhands retain explicit none auto and discard values and reject fractional integers" do
    for {property, value, expected} <- [
          {"overflow-y", "clip", %{overflow_y: :clip}},
          {"max-lines", "none", %{max_lines: :none}},
          {"continue", "auto", %{continue: :auto}},
          {"continue", "discard", %{continue: :discard}},
          {"line-clamp", "none", %{max_lines: :none, continue: :auto, block_ellipsis: :clip}}
        ] do
      assert {:ok, ^expected} = CssOverflowValidator.prepare(property, value)
    end

    assert :error = CssOverflowValidator.prepare("max-lines", "1.0")
    assert :error = CssOverflowValidator.prepare("line-clamp", "")
    assert :ok = CssOverflowValidator.validate_style(%{text_overflow: {"left", :clip}}, "div")
  end

  test "ink bounds retain empty glyph allocation and descend through grouped selected paint" do
    alias NativeElixirPdfUtilities.HtmlToPdf.Font
    {:ok, registry} = Font.load_registry(system_font_discovery: false)
    {:ok, _, font} = Font.resolve("sans-serif", 400, :normal, registry)

    text = %{
      type: :text,
      text: " ",
      x: 3,
      y: 4,
      width: 8,
      height: 12,
      font_face: font,
      font_size: 12
    }

    group = %{type: :glyph_group, boxes: [text], x: 0, y: 0, width: 12, height: 16}
    assert {left, bottom, right, top} = OverflowGeometry.ink_bounds([group], {0, 0, 0, 0})
    assert left == 0 and bottom <= 0 and right >= 12 and top >= 16

    invalid = %{
      text
      | text: "A",
        font_face: %{
          type: :embedded,
          units_per_em: 1000,
          cmap: %{?A => 1},
          data: <<>>,
          tables: %{}
        }
    }

    assert {:render_resource_limit, {:error, {_, diagnostic}}} =
             catch_throw(OverflowGeometry.ink_bounds([invalid], {0, 0, 0, 0}))

    assert is_binary(diagnostic.message)
  end
end
