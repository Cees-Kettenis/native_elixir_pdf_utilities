defmodule NativeElixirPdfUtilities.HtmlToPdf.TableVisibilityTest do
  use ExUnit.Case, async: true
  alias NativeElixirPdfUtilities.{HtmlToPdf, Text}
  alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, Style, Layout}

  # CSS 2.1 REC 2011-06-07, 17.5.5: collapsed tracks still supply intrinsic widths.
  test "collapsed rows retain column sizing while removing paint interaction and vertical allocation" do
    html =
      "<body style='margin:0;font:10pt sans-serif;line-height:12pt'><table style='border-spacing:0'><tr><td style='padding:0'>Visible</td></tr><tr style='visibility:collapse'><td style='padding:0'><a href='https://example.com'>Disappears</a></td></tr><tr><td style='padding:0'>Last</td></tr></table><p style='margin:0'>After</p></body>"

    assert {:ok, pdf} = HtmlToPdf.render(html, system_font_discovery: false)
    assert {:ok, text} = Text.extract(pdf)
    assert text =~ "Visible" and text =~ "Last" and text =~ "After"
    refute text =~ "Disappears"
    refute pdf =~ "/Subtype /Link"
    assert {:ok, dom} = HtmlParser.parse_detailed(html)
    assert {:ok, styled} = Style.compute_detailed(dom, system_font_discovery: false)
    assert {:ok, result} = Layout.layout(styled, page_size: {200, 200}, margin: 0)
    visible = Enum.find(result.boxes, &(Map.get(&1, :text) == "Visible"))
    after_box = Enum.find(result.boxes, &(Map.get(&1, :text) == "After"))
    assert_in_delta visible.y - after_box.y, 24, 0.75
  end

  test "collapsed columns and groups remove their grid bands and captions use the reduced table width" do
    for columns <- [
          "<colgroup><col style='visibility:collapse'><col></colgroup>",
          "<colgroup style='visibility:collapse'><col></colgroup><colgroup><col></colgroup>"
        ] do
      html =
        "<table style='table-layout:fixed;width:80pt;border-spacing:0'><caption style='caption-side:bottom'>Caption</caption>#{columns}<tr><td style='padding:0'>Hidden</td><td style='padding:0'>Shown</td></tr></table><p>After</p>"

      assert {:ok, pdf} = HtmlToPdf.render(html, system_font_discovery: false)
      assert {:ok, text} = Text.extract(pdf)
      refute text =~ "Hidden"
      assert text =~ "Shown" and text =~ "After"
    end
  end

  test "collapsed groups retain spanning cells, baseline alignment and following allocation" do
    for content <- [
          "<tbody style='visibility:collapse'><tr><td>Hidden</td></tr></tbody><tbody><tr><td style='vertical-align:baseline'>Shown</td></tr></tbody>",
          "<tr><td rowspan=2 style='vertical-align:baseline'>Spanning</td><td>Shown</td></tr><tr style='visibility:collapse'><td>Hidden</td></tr>",
          "<tr style='visibility:collapse'><td>Hidden</td></tr>"
        ] do
      html = "<table style='border-spacing:0'>#{content}</table><p>After</p>"
      assert {:ok, pdf} = HtmlToPdf.render(html, system_font_discovery: false)
      assert {:ok, text} = Text.extract(pdf)
      assert text =~ "After"
      refute text =~ "Hidden"
    end
  end

  test "track compression moves zero-area source anchors and numeric baselines with surviving rows" do
    alias NativeElixirPdfUtilities.HtmlToPdf.TableVisibility

    anchor = %{
      type: :flow_marker,
      x: 20.0,
      y: 70.0,
      width: 0.0,
      height: 0.0,
      html_targets: [%{identity: :target, dx: 0.0, dy: 0.0}]
    }

    fragments = [
      %{boxes: [], top: 100.0, bottom: 90.0, baseline: nil},
      %{boxes: [anchor], top: 90.0, bottom: 70.0, baseline: 80.0}
    ]

    plan = %{rows: [true, false], columns: [{0.0, 10.0}]}
    assert {[moved], 80.0, [fragment]} = TableVisibility.collapse(fragments, 0, plan, 0, 70)
    assert moved.x == 10.0 and moved.y == 80.0
    assert fragment.baseline == 90.0
    assert moved.html_targets == anchor.html_targets
  end
end
