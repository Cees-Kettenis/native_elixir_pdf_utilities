defmodule NativeElixirPdfUtilities.HtmlToPdf.TablePercentageBaselineTest do
  use ExUnit.Case, async: true
  alias NativeElixirPdfUtilities.Text
  alias NativeElixirPdfUtilities.HtmlToPdf
  alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, Style, Layout}

  test "allocated percentage spacers export their baseline in atomic cells and surfaces" do
    for content <- [
          "<div style='height:50%;background:#def'></div>",
          "<div style='height:calc(50% + 1pt);max-height:30pt'></div><span>Later</span>",
          "<div style='min-height:50%'></div><span>Later</span>",
          "<div style='height:50%;max-height:30pt'></div><span>Later</span>",
          "<div style='height:50%'><div style='height:50%;min-height:20pt'></div><span>Nested</span></div>"
        ] do
      html =
        "<body style='margin:0;font-family:DejaVu Sans;font-size:10pt;line-height:16pt'><div style='display:inline-block'><table style='height:80pt;width:160pt;border-collapse:collapse'><tr><td style='width:100pt;padding:0;vertical-align:baseline'>#{content}</td><td style='padding:0;vertical-align:baseline'>Peer</td></tr></table></div></body>"

      assert {:ok, dom} = HtmlParser.parse_document_detailed(html)
      assert {:ok, styled} = Style.compute_detailed(dom, system_font_discovery: false)

      for result <- [
            Layout.layout(styled, page_size: {200, 180}, margin: 0),
            Layout.layout_viewport(styled, 200, 180)
          ] do
        assert {:ok, layout} = result
        peer = Enum.find(layout.boxes, &(Map.get(&1, :text) == "Peer"))
        reference = Enum.find(layout.boxes, &(Map.get(&1, :text) in ["Later", "Nested"]))

        if reference do
          assert_in_delta peer.y, reference.y, 0.000001
        else
          spacer =
            Enum.find(layout.boxes, &(Map.get(&1, :fill_color) == {221 / 255, 238 / 255, 1.0}))

          assert_in_delta peer.y, spacer.y, 0.000001
          assert_in_delta spacer.height, 40.0, 0.000001
        end
      end

      assert {:ok, pdf} = HtmlToPdf.render(html, system_font_discovery: false)
      assert {:ok, text} = Text.extract(pdf)
      assert text =~ "Peer"
    end
  end
end
