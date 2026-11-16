defmodule NativeElixirPdfUtilities.HtmlToPdf.EllipticalCornerTest do
  use ExUnit.Case, async: true
  alias NativeElixirPdfUtilities.{HtmlToPdf, Text}
  alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, Layout, Style, PageGeometry}
  alias NativeElixirPdfUtilities.Validators.{CssBorderRadiusValidator, WriterValidator}

  test "percentages use separate axes and overlapping corners share one reduction factor" do
    assert {:ok, style} =
             CssBorderRadiusValidator.prepare(%{}, "border-radius", "80% 40% / 60% 20%")

    assert {:ok, radii} = CssBorderRadiusValidator.used_radii(style, 100, 50)
    assert_in_delta elem(radii.top_left, 0), 200 / 3, 0.000001
    assert_in_delta elem(radii.top_left, 1), 25, 0.000001
    assert_in_delta elem(radii.top_right, 0), 100 / 3, 0.000001
    assert_in_delta elem(radii.top_right, 1), 25 / 3, 0.000001
  end

  test "logical corner winners and shorthand resets retain elliptical PDF paths and searchable text" do
    html =
      "<div style='width:100pt;height:40pt;border:3pt solid red;background:lime;border-radius:20pt / 8pt;direction:rtl;border-start-start-radius:4pt 12pt'>Corners</div>"

    assert {:ok, dom} = HtmlParser.parse(html)
    assert {:ok, styled} = Style.compute(dom, system_font_discovery: false)
    style = hd(styled.children).style
    assert style.border_radii.top_right == {4.0, 12.0}
    assert style.border_radii.top_left == {20.0, 8.0}
    assert {:ok, layout} = Layout.layout(styled)
    assert Enum.any?(layout.boxes, &is_map(Map.get(&1, :border_radii)))

    assert {:ok, pdf} =
             HtmlToPdf.render(html, system_font_discovery: false, compress_streams: false)

    assert pdf =~ " c "
    assert {:ok, text} = Text.extract(pdf)
    assert text =~ "Corners"

    assert {:ok, dom} =
             HtmlParser.parse(
               "<div style='border-top-left-radius:8pt 2pt;border-radius:3pt'></div>"
             )

    assert {:ok, styled} = Style.compute(dom, system_font_discovery: false)
    assert hd(styled.children).style.border_radius == 3
    assert Enum.uniq(Map.values(hd(styled.children).style.border_radii)) == [{3.0, 3.0}]
  end

  test "content clips subtract the matching edge from each radius" do
    host = %{
      x: 0,
      y: 0,
      width: 100,
      height: 40,
      border_radius: 0,
      border_radii: %{
        top_left: {20, 8},
        top_right: {4, 12},
        bottom_left: {7, 5},
        bottom_right: {9, 3}
      }
    }

    clip = PageGeometry.inset_rounded_clip(host, %{top: 2, right: 3, bottom: 4, left: 5})

    assert clip.border_radii == %{
             top_left: {15, 6},
             top_right: {1, 10},
             bottom_left: {2, 1},
             bottom_right: {6, 0}
           }

    assert clip.width == 92 and clip.height == 34
  end

  test "invalid source and forged corner maps fail at their validator boundary" do
    for source <- ["-1px", "1px /", "1px / 2px / 3px", "auto", "1px 2px 3px 4px 5px"] do
      refute match?({:ok, _}, CssBorderRadiusValidator.prepare(%{}, "border-radius", source))
    end

    assert {:error, {:invalid_layout, %{stage: :layout, source: "div"}}} =
             CssBorderRadiusValidator.validate_style(%{border_radii: %{top_left: {1, 2}}}, "div")

    box = %{
      type: :rect,
      x: 0,
      y: 0,
      width: 10,
      height: 10,
      fill_color: {1, 0, 0},
      stroke_color: nil,
      stroke_width: 0,
      border_radius: 0,
      border_radii: %{top_left: {1, 2}}
    }

    assert {:error, {_, %{stage: :pdf}}} = WriterValidator.validate_native_boxes([box])
  end
end
