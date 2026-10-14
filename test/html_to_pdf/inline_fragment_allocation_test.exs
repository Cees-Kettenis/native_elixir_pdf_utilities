defmodule NativeElixirPdfUtilities.HtmlToPdf.InlineFragmentAllocationTest do
  use ExUnit.Case
  alias NativeElixirPdfUtilities.HtmlToPdf.{FontFallback, HtmlParser, Layout, Style}

  test "ordinary and atomic styled text share Chromium's final fragment allocation" do
    font = Path.expand("../../priv/fonts/dejavu/DejaVuSans.ttf", __DIR__)
    opts = [fonts: [%{family: "Fixture Sans", path: font}], system_font_discovery: false]

    for display <- ["inline", "inline-block"] do
      html =
        "<div style='font-family:Fixture Sans;font-size:10pt;line-height:12pt;margin:0'>Before<span style='display:#{display}'><a href='https://example.com/baseline'>Inside</a></span>After</div>"

      assert {:ok, dom} = HtmlParser.parse_detailed(html)
      assert {:ok, styled} = Style.compute_detailed(dom, opts)
      assert {:ok, resolved} = FontFallback.resolve(styled)
      assert {:ok, layout} = Layout.layout(resolved, page_size: {200, 120}, margin: 0)
      [before, inside, after_text] = Enum.filter(layout.boxes, &(&1.type == :text))

      # Chromium 152 with these exact font bytes reports 43.578125 and
      # 83.265625 CSS pixels for both original source forms.
      assert before.x == 0
      assert inside.x == 43.578125 * 0.75
      assert after_text.x == 83.265625 * 0.75
      assert before.y == inside.y and inside.y == after_text.y
      assert inside.link_url == "https://example.com/baseline"
      assert inside.font_face.data == File.read!(font)
    end
  end
end
