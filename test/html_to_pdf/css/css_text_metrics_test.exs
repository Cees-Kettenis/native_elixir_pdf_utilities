alias NativeElixirPdfUtilities.TestSupport.Paths

defmodule NativeElixirPdfUtilities.HtmlToPdf.CssTextMetricsTest do
  use ExUnit.Case
  alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, Layout, Style}
  alias NativeElixirPdfUtilities.Validators.{HtmlLineBreakValidator, HtmlValidator}
  alias NativeElixirPdfUtilities.Limits
  @font Paths.fixture("html_to_pdf/browser_parity/assets/cjk-ruby-fixture.ttf")
  @fonts [%{family: "CJK Ruby Fixture", path: @font}]

  test "break-spaces keeps Unicode opportunities and adds a break after each preserved space" do
    assert {:ok, ["東", "京", "京", "都 ", " ", "大", "阪"]} =
             HtmlLineBreakValidator.segments("東京京都  大阪", "", true)

    assert {:ok, ["water-", "repellent ", " ", "word"]} =
             HtmlLineBreakValidator.segments("water-repellent  word", "", true)

    assert {:ok, true} = HtmlLineBreakValidator.boundary?("東京 ", " 京都", true)
    assert {:ok, false} = HtmlLineBreakValidator.boundary?("東京", "、京都", true)

    assert {:ok, ["-", "repellent"]} =
             HtmlLineBreakValidator.segments("-repellent", "water", true)

    assert {:ok, ["A\u2003", "\u2003", "B"]} =
             HtmlLineBreakValidator.segments("A\u2003\u2003B", "", true)

    assert {:ok, ["A\u00A0B ", "C"]} =
             HtmlLineBreakValidator.segments("A\u00A0B C", "", true)
  end

  test "preserved-space options fail through the shared diagnostics contract" do
    assert {:error, {:invalid_layout, diagnostic}} =
             HtmlLineBreakValidator.segments("word", "", :invalid)

    assert diagnostic.stage == :layout
    assert diagnostic.operation == :line_break
    assert diagnostic.message =~ "boolean"
  end

  test "preserved-space traversal shares the render text work budget" do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    Limits.install(Map.put(limits, :max_layout_text_work, 10))

    assert {:ok, _} =
             HtmlValidator.with_render_budget(fn ->
               HtmlLineBreakValidator.opportunities("A  B")
             end)

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             HtmlValidator.with_render_budget(fn ->
               HtmlLineBreakValidator.opportunities("A  B", true)
             end)

    assert diagnostic.stage == :layout
    assert diagnostic.message =~ "max_layout_text_work"
  end

  test "alphabetic numeric hyphens retain Unicode and complete-grapheme opportunities" do
    for {source, expected} <- [
          {"é-٢", ["é-", "٢"]},
          {"A-1́", ["A-", "1́"]},
          {"Á-1", ["Á-1"]},
          {"1-٢", ["1-٢"]},
          {"A--1", ["A--1"]}
        ] do
      assert {:ok, ^expected} = HtmlLineBreakValidator.segments(source)
      assert Enum.join(expected) == source
    end
  end

  test "empty positioned spans preserve complete unbroken styled words" do
    html =
      "<p style='margin:0;width:40pt;font-size:10pt;line-height:12pt'>go ABC<span style='position:relative;left:1.5pt'></span><strong>DEF</strong>GHI</p>"

    assert {:ok, dom} = HtmlParser.parse_detailed(html)
    assert {:ok, styled} = Style.compute_detailed(dom, system_font_discovery: false)
    assert {:ok, layout} = Layout.layout(styled, page_size: {200, 200}, margin: 0)
    text = Enum.filter(layout.boxes, &(Map.get(&1, :type) == :text))
    assert Enum.map_join(text, & &1.text) == "goABCDEFGHI"
    assert Enum.map(text, & &1.text) == ["go", "ABC", "DEF", "GHI"]
    assert Enum.uniq(Enum.map(tl(text), & &1.y)) |> length() == 1
    assert hd(text).y > hd(tl(text)).y
  end

  test "zero-width positioned spans do not wrap an overflowing unbroken word" do
    html =
      "<p style='margin:0;width:40pt;font-size:10pt;line-height:12pt'>ABCDEFGHI<span style='position:relative;left:1.5pt'></span>J</p>"

    assert {:ok, dom} = HtmlParser.parse_detailed(html)
    assert {:ok, styled} = Style.compute_detailed(dom, system_font_discovery: false)
    assert {:ok, layout} = Layout.layout(styled, page_size: {200, 200}, margin: 0)
    text = Enum.filter(layout.boxes, &(Map.get(&1, :type) == :text))
    assert Enum.map_join(text, & &1.text) == "ABCDEFGHIJ"
    assert length(Enum.uniq(Enum.map(text, & &1.y))) == 1
  end

  test "break-spaces uses selected-font widths across CJK, inline styles and Latin hyphens" do
    for {content, width, family, expected} <- [
          {"東京京都大阪漢字", 48, "CJK Ruby Fixture", ["東京京都", "大阪漢字"]},
          {"東京<span>京都</span>、大阪漢字", 48, "CJK Ruby Fixture", ["東京京", "都、大", "阪漢字"]},
          {"東京  京都", 48, "CJK Ruby Fixture", ["東京  京", "都"]},
          {"water-repellent", 42, "DejaVu Sans", ["water-", "repellent"]},
          {"prefix water<span>-repel</span><span>lent</span>", 115, "DejaVu Sans",
           ["prefix water-", "repellent"]}
        ] do
      html =
        "<p style='margin:0;white-space:break-spaces;width:#{width}pt;font-family:#{family};line-height:24pt'>#{content}</p>"

      assert {:ok, dom} = HtmlParser.parse_detailed(html)

      assert {:ok, styled} =
               Style.compute_detailed(dom, fonts: @fonts, system_font_discovery: false)

      assert {:ok, layout} = Layout.layout(styled, page_size: {240, 240}, margin: 0)
      text = Enum.filter(layout.boxes, &(Map.get(&1, :type) == :text))

      lines =
        text
        |> Enum.group_by(& &1.y)
        |> Enum.sort_by(fn {y, _} -> -y end)
        |> Enum.map(fn {_y, boxes} ->
          boxes |> Enum.sort_by(& &1.x) |> Enum.map_join(& &1.text)
        end)

      assert lines == expected
      assert Enum.map_join(text, & &1.text) == Regex.replace(~r/<[^>]+>/, content, "")
    end
  end
end
