alias NativeElixirPdfUtilities.TestSupport.Paths

defmodule NativeElixirPdfUtilities.HtmlToPdf.CjkLineBreakTest do
  use ExUnit.Case

  alias NativeElixirPdfUtilities.HtmlToPdf.{
    Font,
    HtmlParser,
    Layout,
    Pagination,
    RubyLayout,
    Style
  }

  alias NativeElixirPdfUtilities.Validators.{HtmlLineBreakValidator, RubyValidator, FontValidator}
  alias NativeElixirPdfUtilities.Limits

  @font Paths.fixture("html_to_pdf/browser_parity/assets/cjk-ruby-fixture.ttf")
  @fonts [%{family: "CJK Ruby Fixture", path: @font}]

  test "Unicode opportunities preserve CJK punctuation, NBSP, Latin words and emoji clusters" do
    for {text, expected} <- [
          {"東京（京都）、大阪", ["東", "京", "（京", "都）、", "大", "阪"]},
          {"漢字AlphaBeta中文", ["漢", "字", "AlphaBeta", "中", "文"]},
          {"A\u00A0B C", ["A\u00A0B ", "C"]},
          {"markup</b> path/to/file", ["markup</b> ", "path/to/file"]},
          {"path/東京", ["path/", "東", "京"]},
          {"e\u0301漢字", ["e\u0301", "漢", "字"]},
          {"👩‍👩‍👧‍👦漢字", ["👩‍👩‍👧‍👦", "漢", "字"]}
        ] do
      assert {:ok, ^expected} = HtmlLineBreakValidator.segments(text)
      assert Enum.join(expected) == text
    end

    assert {:ok, [{2, true}, {3, true}]} = HtmlLineBreakValidator.opportunities("a\nb")
    assert {:ok, []} = HtmlLineBreakValidator.segments("")
    assert {:ok, ["-", "repellent"]} = HtmlLineBreakValidator.segments("-repellent", "water")
    assert {:ok, false} = HtmlLineBreakValidator.boundary?("東京", "、大阪")
    assert {:ok, true} = HtmlLineBreakValidator.boundary?("東京", "京都")
  end

  test "line-break failures use diagnostics and the shared layout work limit" do
    for input <- [nil, 42, <<255>>] do
      assert {:error, {:invalid_layout, diagnostic}} = HtmlLineBreakValidator.opportunities(input)
      assert diagnostic.stage == :layout
      assert diagnostic.reason == :invalid_layout
      assert diagnostic.operation == :line_break
      assert diagnostic.module == HtmlLineBreakValidator
      assert diagnostic.message =~ "UTF-8"
    end

    assert {:error, {:invalid_layout, diagnostic}} =
             RubyValidator.em_metrics(%{
               font_face: %{type: :embedded, id: "unreadable", data: <<>>},
               font_size: 12.0
             })

    assert diagnostic.stage == :layout and diagnostic.operation == :measure_ruby
    assert diagnostic.message =~ "typographic metrics"

    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    Limits.install(Map.put(limits, :max_layout_text_work, 1))

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             HtmlLineBreakValidator.opportunities("東京")

    assert diagnostic.stage == :layout
    assert diagnostic.message =~ "max_layout_text_work"
  end

  test "the configured CJK subset contains supported outlines and fullwidth advances" do
    assert {:ok, registry} = Font.load_registry(fonts: @fonts, system_font_discovery: false)
    assert {:ok, _families, font} = Font.resolve("CJK Ruby Fixture", 400, :normal, registry)

    assert {:ok, %{typographic_height: %{ascent: 880, descent: 120}}} =
             FontValidator.prepare_text_metrics(font)

    assert {:ok, %{ascent: 11.25, descent: 1.5}} =
             RubyValidator.em_metrics(%{font_face: font, font_size: 12.0})

    assert {:ok, %{ascent: 6.0, descent: 0.75}} =
             RubyValidator.em_metrics(%{font_face: font, font_size: 6.0})

    assert Font.supports_text?(font, "東京京都大阪漢字とうきょうきょうとおおさかかんじ")
    assert_in_delta Font.text_width("東京京都", font, 12), 48, 0.001
    assert_in_delta Font.text_width("とうきょう", font, 6), 30, 0.001
    manifest = File.read!(Path.rootname(@font) <> ".json")
    assert manifest =~ "outline_sha256"
    assert manifest =~ Base.encode16(:crypto.hash(:sha256, File.read!(@font)), case: :lower)
  end

  test "normal long CJK ruby wraps base and reading proportionally and paginates intact" do
    html =
      "<p style='margin:0;width:48pt;line-height:30pt;font-family:CJK Ruby Fixture'><ruby>東京京都大阪漢字<rt>とうきょうきょうとおおさかかんじ</rt></ruby></p>"

    assert {:ok, dom} = HtmlParser.parse_detailed(html)

    assert {:ok, styled} =
             Style.compute_detailed(dom, fonts: @fonts, system_font_discovery: false)

    assert {:ok, layout} = Layout.layout(styled, page_size: {80, 50}, margin: 5)
    bases = Enum.filter(layout.boxes, &(Map.get(&1, :type) == :text and &1.font_size == 12.0))
    readings = Enum.filter(layout.boxes, &(Map.get(&1, :type) == :text and &1.font_size == 6.0))
    assert Enum.map(bases, & &1.text) == ["東京京都", "大阪漢字"]
    assert Enum.map_join(readings, & &1.text) == "とうきょうきょうとおおさかかんじ"
    assert length(Enum.uniq_by(bases, & &1.y)) == 2
    assert Enum.all?(bases, &(&1.width <= 48.001))
    assert {:ok, pages} = Pagination.paginate(layout)
    assert length(pages) == 2

    assert Enum.all?(pages, fn page ->
             Enum.any?(page.boxes, &(Map.get(&1, :font_size) == 12.0)) and
               Enum.any?(page.boxes, &(Map.get(&1, :font_size) == 6.0))
           end)
  end

  test "public rendering propagates exhausted Ruby fragment work and releases its render scope" do
    html =
      "<p style='margin:0;width:48pt;line-height:30pt;font-family:CJK Ruby Fixture'><ruby>東京京都大阪漢字<rt>とうきょうきょうとおおさかかんじ</rt></ruby></p>"

    options = [fonts: @fonts, system_font_discovery: false, page_size: {80, 50}, margin: 5]
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)

    assert {:ok, pdf} = NativeElixirPdfUtilities.HtmlToPdf.render(html, options)
    assert {:ok, %{pages: [first, second]}} = NativeElixirPdfUtilities.Text.extract_spans(pdf)
    assert Enum.map_join(first.spans, & &1.text) =~ "東京京都"
    assert Enum.map_join(second.spans, & &1.text) =~ "大阪漢字"

    # Initial Ruby measurement fits this budget. Measuring the proportional
    # fragment candidates exhausts the same cumulative layout work scope.
    Limits.install(Map.put(limits, :max_layout_text_work, 1_500))

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             NativeElixirPdfUtilities.HtmlToPdf.render(html, options)

    assert diagnostic.stage == :layout
    assert diagnostic.reason == :resource_limit_exceeded
    assert diagnostic.operation == :render
    assert diagnostic.module == NativeElixirPdfUtilities.Validators.HtmlValidator

    assert diagnostic.message ==
             "rendering exceeds max_layout_text_work (configured limit 1500)"

    Limits.install(limits)
    assert {:ok, recovered} = NativeElixirPdfUtilities.HtmlToPdf.render(html, options)
    assert {:ok, %{pages: pages}} = NativeElixirPdfUtilities.Text.extract_spans(recovered)

    assert Enum.map(pages, &Enum.map(&1.spans, fn span -> span.text end)) ==
             Enum.map([first, second], &Enum.map(&1.spans, fn span -> span.text end))
  end

  test "ordinary CJK breaks cross styled boundaries without detaching closing punctuation" do
    html =
      "<p style='margin:0;width:36pt;font-family:CJK Ruby Fixture;line-height:24pt'>東京<span>京都</span>、大阪漢字</p>"

    assert {:ok, dom} = HtmlParser.parse_detailed(html)

    assert {:ok, styled} =
             Style.compute_detailed(dom, fonts: @fonts, system_font_discovery: false)

    assert {:ok, layout} = Layout.layout(styled, page_size: {100, 150}, margin: 0)
    text = Enum.filter(layout.boxes, &(Map.get(&1, :type) == :text))
    assert Enum.map_join(text, & &1.text) == "東京京都、大阪漢字"
    lines = Enum.group_by(text, & &1.y)
    assert map_size(lines) >= 3

    assert Enum.all?(lines, fn {_y, boxes} ->
             boxes = Enum.sort_by(boxes, & &1.x)
             not String.starts_with?(Enum.map_join(boxes, & &1.text), "、")
           end)
  end

  test "normal Latin ruby remains atomic and nowrap CJK ruby preserves its complete reading" do
    for {content, extra_style} <- [
          {"AlphaBeta<rt>longreading</rt>", ""},
          {"東京京都大阪漢字<rt>とうきょうきょうとおおさかかんじ</rt>", ";white-space:nowrap"}
        ] do
      html =
        "<p style='margin:0;width:24pt;font-family:CJK Ruby Fixture#{extra_style}'><ruby>#{content}</ruby></p>"

      assert {:ok, dom} = HtmlParser.parse_detailed(html)

      assert {:ok, styled} =
               Style.compute_detailed(dom, fonts: @fonts, system_font_discovery: false)

      assert {:ok, layout} = Layout.layout(styled)
      bases = Enum.filter(layout.boxes, &(Map.get(&1, :type) == :text and &1.font_size == 12.0))
      assert length(Enum.uniq_by(bases, & &1.y)) == 1
    end

    assert {:ok, dom} =
             HtmlParser.parse_detailed(
               "<ruby style='font-family:CJK Ruby Fixture'>東京（京都）、大阪<rt>とうきょうきょうとおおさか</rt></ruby>"
             )

    assert {:ok, %{children: [ruby]}} =
             Style.compute_detailed(dom, fonts: @fonts, system_font_discovery: false)

    assert {:ok, [measured]} = RubyLayout.measure(ruby, nil)
    assert measured.source_group.break_counts == [1, 2, 4, 7, 8]
  end
end
