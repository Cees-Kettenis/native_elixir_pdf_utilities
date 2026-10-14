defmodule NativeElixirPdfUtilities.HtmlToPdf.ControlMaxCharacterWidthTest do
  use ExUnit.Case
  alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, Layout, Style}

  test "native text inputs use the selected typeface maximum character width" do
    path = Path.expand("../../priv/fonts/dejavu/DejaVuSans.ttf", __DIR__)

    for type <- ["text", "password"] do
      assert {:ok, dom} = HtmlParser.parse_detailed("<input type='#{type}' value=Alice>")

      assert {:ok, tree} =
               Style.compute_detailed(dom,
                 fonts: [%{family: "DejaVu Sans", path: path}],
                 system_font_discovery: false
               )

      assert {:ok, layout} = Layout.layout(tree, page_size: {240, 300}, margin: 10)
      [host] = Enum.filter(layout.boxes, &(&1.type == :rect))
      [value] = Enum.filter(layout.boxes, &(&1.type == :text))

      # Chromium with the same genuine typeface allocates 179 CSS pixels.
      # hhea's maximum advance incorrectly reduced this to 166 CSS pixels.
      assert host.width == 179 * 0.75
      assert value.font_size == 10
      assert value.font_face.data == File.read!(path)
      assert value.x >= host.x and value.x + value.annotation_width <= host.x + host.width
      assert value.y >= host.y and value.y <= host.y + host.height
    end
  end
end
