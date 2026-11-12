defmodule NativeElixirPdfUtilities.HtmlToPdf.TextSpacingTest do
  use ExUnit.Case, async: true
  alias NativeElixirPdfUtilities.{HtmlToPdf, Text}
  alias NativeElixirPdfUtilities.HtmlToPdf.{Font, HtmlParser, Layout, Style}
  alias NativeElixirPdfUtilities.Validators.CssTextSpacingValidator, as: Validator

  test "paragraph edges keep percentage indents and leave atomic last content intact" do
    alias NativeElixirPdfUtilities.HtmlToPdf.TextSpacing
    style = %{text_indent: %{length: {:percent, 0.1}, hanging: false, each_line: false}}
    [marker] = TextSpacing.start_line([%{text: "A", style: style}], 200)
    assert marker._inline_padding_width == 20
    atomic = %{type: :inline_block, text: "", style: %{}}

    [prepared] =
      TextSpacing.prepare_runs([atomic], %{hanging_punctuation: [:last]}, &Function.identity/1)

    assert prepared.type == :inline_block and prepared.text == ""
    assert prepared._paragraph_indent_style.hanging_punctuation == [:last]
  end

  test "percentage indentation and physical match-parent alignment retain their computed forms" do
    assert :ok =
             Validator.validate_style(
               %{text_indent: %{length: {:percent, 0.25}, hanging: false, each_line: true}},
               "p"
             )

    assert {:error, {:invalid_layout, %{source: "p"}}} =
             Validator.validate_style(
               %{text_indent: %{length: {:percent, :bad}, hanging: false, each_line: false}},
               "p"
             )

    assert {:ok, %{text_align_last: :center}} =
             Validator.prepare(
               %{_cascade_inherited: %{text_align_last: :center, direction: :rtl}},
               "text-align-last",
               "match-parent"
             )
  end

  test "word spacing resolves font units, inherits, cascades and resets independently" do
    assert "word-spacing" in Validator.properties()

    assert {:ok, dom} =
             HtmlParser.parse("""
             <div style="font-size:10pt;word-spacing:.5em;text-align:justify;text-align-last:center">
               A<span>B</span><b style="word-spacing:initial;text-align-last:initial">C</b></div>
             """)

    assert {:ok, %{children: [parent]}} =
             Style.compute_detailed(dom, system_font_discovery: false)

    assert parent.style.word_spacing == 5
    [_, span, bold] = parent.children
    assert span.style.word_spacing == 5
    assert bold.style.word_spacing == 0
    assert span.style.text_align_last == :center
    assert bold.style.text_align_last == :auto
  end

  test "percentage indents, atomic justification and padded hanging edges use the available line width" do
    alias NativeElixirPdfUtilities.HtmlToPdf.TextSpacing
    assert {:ok, style} = Validator.prepare(%{}, "text-indent", "25%")
    assert :ok = Validator.validate_style(style, "p")
    [marker] = TextSpacing.start_line([%{text: "A", style: style}], 100)
    assert marker._inline_padding_width == 25

    atomic = %{type: :inline_block, text: "", style: %{}, advance: 2}

    word = %{
      text: "A B",
      style: %{font_face: %{type: :built_in, family: "Helvetica", pdf_name: "Helvetica"}},
      advance: 3
    }

    style = %{text_align: :justify, text_align_last: :justify, text_justify: :inter_word}
    [kept, stretched] = TextSpacing.justify([atomic, word], style, 10, true, & &1.advance)
    assert kept == atomic and stretched.style.word_spacing == 5

    [overflow] =
      TextSpacing.justify([%{word | text: "AAAA", advance: 4}], style, 2, true, & &1.advance)

    assert overflow._line_alignment == :left
    assert TextSpacing.justify([], style, 0, true, & &1.advance) == []

    style = %{hanging_punctuation: [:first, :last]}

    text = %{
      text: "\"A\"",
      style: style,
      _paragraph_end_length: 3,
      _paragraph_offset: 0,
      _paragraph_first_line: true
    }

    assert TextSpacing.hanging_edges([text], 10, &String.length(&1.text)) == {1, 1}

    padding = %{
      type: :inline_positioning_marker,
      text: "",
      style: style,
      _inline_padding_width: 2,
      _paragraph_first_line: true
    }

    assert TextSpacing.hanging_edges([padding, text, padding], 10, &String.length(&1.text)) ==
             {0.0, 0.0}
  end

  test "word spacing changes fitting and paint advances without changing logical text" do
    for spacing <- [-2, 0, 5] do
      html =
        "<p style='margin:0;font-family:DejaVu Sans Mono;font-size:10pt;word-spacing:#{spacing}pt'>A B</p>"

      assert {:ok, dom} = HtmlParser.parse(html)
      assert {:ok, styled} = Style.compute_detailed(dom, system_font_discovery: false)
      assert {:ok, layout} = Layout.layout(styled)
      [box] = Enum.filter(layout.boxes, &(&1.type == :text))
      natural = Font.css_text_width("A B", box.font_face, 10)
      assert_in_delta box.annotation_width, natural + spacing, 0.001
      assert Map.get(box, :word_spacing, 0.0) == spacing
      assert {:ok, pdf} = HtmlToPdf.render(html, system_font_discovery: false)
      assert {:ok, "A B"} = Text.extract(pdf, layout: false)
    end
  end

  test "last-line alignment and forced line endings resolve logical directions" do
    for direction <- [:ltr, :rtl], last? <- [true, false] do
      style = %{text_align: :justify, direction: direction}

      assert Validator.alignment(style, last?) ==
               if(last?, do: if(direction == :rtl, do: :right, else: :left), else: :justify)

      assert Validator.alignment(Map.put(style, :text_align_last, :end), true) ==
               if(direction == :rtl, do: :left, else: :right)
    end

    html =
      "<p style='margin:0;width:100pt;font-family:DejaVu Sans Mono;font-size:10pt;text-align:justify;text-align-last:right'>A B C D E F G H I J K L<br>X Y</p>"

    assert {:ok, dom} = HtmlParser.parse(html)
    assert {:ok, styled} = Style.compute_detailed(dom, system_font_discovery: false)
    assert {:ok, layout} = Layout.layout(styled)
    boxes = Enum.filter(layout.boxes, &(&1.type == :text))
    last = List.last(boxes)
    assert last.text == "X Y"
    assert_in_delta last.x + last.annotation_width - hd(boxes).x, 100, 0.02
    assert {:ok, pdf} = HtmlToPdf.render(html, system_font_discovery: false)
    assert {:ok, text} = Text.extract(pdf, layout: false)
    assert String.replace(text, ~r/\s+/, "") == "ABCDEFGHIJKLXY"
  end

  test "spacing grammars reject duplicate and contradictory flags and fixed spaces are not separators" do
    for {property, values} <- [
          {"word-spacing", ["auto", "10%", "1pt 2pt"]},
          {"text-indent", ["", "auto", "hanging", "1pt 2pt", "1pt hanging hanging"]},
          {"text-align-last", ["stretch", "left right"]},
          {"text-justify", ["left", "auto none"]},
          {"hanging-punctuation", ["", "none first", "first first", "allow-end force-end"]}
        ],
        value <- values do
      refute match?({:ok, _}, Validator.prepare(%{}, property, value)), inspect({property, value})
    end

    assert Validator.word_count(" \u00A0\u1361\u{10100}\u{10101}\u{1039F}\u{1091F}") == 7
    assert Validator.word_count("\u2000\u2001\u2009\u200B\u3000") == 0

    assert {:ok, %{text_indent: %{length: length, each_line: true, hanging: true}}} =
             Validator.prepare(%{}, "text-indent", "hanging 25% each-line")

    assert {:ok, 25.0} =
             NativeElixirPdfUtilities.Validators.CssLengthValidator.resolve(length, 100, %{})

    assert {:ok, %{hanging_punctuation: [:first, :last, :allow_end]}} =
             Validator.prepare(%{}, "hanging-punctuation", "first last allow-end")

    assert :ok = Validator.validate_style(Validator.defaults(), "p")

    assert {:error, {:invalid_layout, diagnostic}} =
             Validator.validate_style(%{word_spacing: :bad}, "p")

    assert diagnostic.stage == :layout
    assert diagnostic.source == "p"
  end

  test "indentation reserves line width and follows hanging, forced-break and logical start rules" do
    for {indent, expected} <- [
          {"10pt", [10, 0, 0, 0]},
          {"10pt hanging", [0, 10, 10, 10]},
          {"10pt each-line", [10, 0, 10, 0]},
          {"10pt each-line hanging", [0, 10, 0, 10]},
          {"25% each-line", [10, 0, 10, 0]},
          {"-10pt", [-10, 0, 0, 0]}
        ] do
      html =
        "<p style='margin:0;width:40pt;font-family:DejaVu Sans Mono;font-size:10pt;text-indent:#{indent}'>AAAA BBBB<br>CCCC DDDD</p>"

      assert {:ok, dom} = HtmlParser.parse(html)
      assert {:ok, styled} = Style.compute_detailed(dom, system_font_discovery: false)
      assert {:ok, layout} = Layout.layout(styled)
      boxes = Enum.filter(layout.boxes, &(&1.type == :text))
      assert Enum.map(boxes, & &1.text) == ["AAAA", "BBBB", "CCCC", "DDDD"], indent
      for {box, x} <- Enum.zip(boxes, expected), do: assert_in_delta(box.x, x, 0.02, indent)
    end

    for direction <- ["ltr", "rtl"] do
      html =
        "<p dir='#{direction}' style='margin:0;width:100pt;font-size:10pt;text-indent:20pt'>אב</p>"

      assert {:ok, dom} = HtmlParser.parse(html)
      assert {:ok, styled} = Style.compute_detailed(dom, system_font_discovery: false)
      assert {:ok, layout} = Layout.layout(styled)
      [box] = Enum.filter(layout.boxes, &(&1.type == :text))

      if direction == "rtl",
        do: assert_in_delta(box.x + box.annotation_width, 80, 0.02),
        else: assert_in_delta(box.x, 20, 0.02)
    end
  end

  test "inter-character expansion preserves the selected ffi ligature and cluster ownership" do
    html =
      "<p style='margin:0;width:120pt;font-family:DejaVu Sans;font-size:12pt;text-align:justify;text-align-last:justify;text-justify:inter-character'>office</p>"

    assert {:ok, pdf} =
             HtmlToPdf.render(html, system_font_discovery: false, compress_streams: false)

    assert {:ok, "office"} = Text.extract(pdf, layout: false)
    assert pdf =~ "/ActualText"
    # The font's ffi remains one glyph. Independent font cmap/GSUB metrics
    # provide the four clusters o/ffi/c/e; only their boundaries may expand.
    assert {:ok, dom} = HtmlParser.parse(html)
    assert {:ok, styled} = Style.compute_detailed(dom, system_font_discovery: false)
    assert {:ok, layout} = Layout.layout(styled)
    [box] = Enum.filter(layout.boxes, &(&1.type == :text))
    assert_in_delta box.annotation_width, 120, 0.02
    assert box._justification_spacing > 0
  end

  test "hanging punctuation excludes only eligible edge glyphs from fitting and alignment" do
    for {text, control, align, expected_start, expected_lines} <- [
          {"(AAAA)", "first", "left", -1, 1},
          {"(AAAA)", "none", "left", 0, 1},
          {"(AAAA)", "last", "right", 1, 1},
          {"AAAA.", "force-end", "right", 2, 1},
          {"AAAA.", "allow-end", "right", 1, 1},
          {"AAAA, BBBB.", "force-end", "left", 0, 2}
        ] do
      html =
        "<p style='margin:0;width:40pt;font-family:DejaVu Sans Mono;font-size:10pt;hanging-punctuation:#{control};text-align:#{align}'>#{text}</p>"

      assert {:ok, dom} = HtmlParser.parse(html)
      assert {:ok, styled} = Style.compute_detailed(dom, system_font_discovery: false)
      assert {:ok, layout} = Layout.layout(styled)
      boxes = Enum.filter(layout.boxes, &(&1.type == :text))
      assert length(boxes) == expected_lines
      first = hd(boxes)
      glyph = Font.css_text_width("A", first.font_face, 10)

      expected =
        case expected_start do
          -1 -> -glyph
          0 -> 0
          1 -> 40 - first.annotation_width
          2 -> 40 - first.annotation_width + glyph
        end

      # The final closing bracket hangs to the right of the alignment edge.
      expected = if control == "last", do: expected + glyph, else: expected
      assert_in_delta first.x, expected, 0.025, control
    end

    html =
      "<p style='margin:0;width:30pt;font-family:DejaVu Sans Mono;font-size:10pt;hanging-punctuation:allow-end'>AAAA, BBBB</p>"

    assert {:ok, pdf} = HtmlToPdf.render(html, system_font_discovery: false)
    assert {:ok, text} = Text.extract(pdf, layout: false)
    assert String.replace(text, ~r/\s/u, "") == "AAAA,BBBB"
  end

  test "balanced and pretty static wrapping retain line count, parent width and source text" do
    for {mode, expected} <- [
          {"auto", ["AAAA BBBB CCCC", "DDDD"]},
          {"stable", ["AAAA BBBB CCCC", "DDDD"]},
          {"balance", ["AAAA BBBB", "CCCC DDDD"]},
          {"pretty", ["AAAA BBBB", "CCCC DDDD"]}
        ] do
      html =
        "<p style='margin:0;width:100pt;font-family:DejaVu Sans Mono;font-size:10pt;text-wrap:#{mode};text-align:right'>AAAA BBBB CCCC DDDD</p>"

      assert {:ok, dom} = HtmlParser.parse(html)
      assert {:ok, styled} = Style.compute_detailed(dom, system_font_discovery: false)
      assert {:ok, layout} = Layout.layout(styled)
      boxes = Enum.filter(layout.boxes, &(&1.type == :text))
      assert Enum.map(boxes, & &1.text) == expected, mode
      for box <- boxes, do: assert_in_delta(box.x + box.annotation_width, 100, 0.02)
      assert {:ok, pdf} = HtmlToPdf.render(html, system_font_discovery: false)
      assert {:ok, text} = Text.extract(pdf, layout: false)
      assert String.replace(text, ~r/\s/u, "") == "AAAABBBBCCCCDDDD"
    end

    assert {:ok, %{text_wrap_mode: :nowrap, text_wrap_style: :balance}} =
             NativeElixirPdfUtilities.Validators.CssWhiteSpaceValidator.prepare(
               %{},
               "text-wrap",
               "balance nowrap"
             )

    for value <- ["balance pretty", "wrap nowrap", "auto auto", "unknown"] do
      assert :error =
               NativeElixirPdfUtilities.Validators.CssWhiteSpaceValidator.prepare(
                 %{},
                 "text-wrap",
                 value
               )
    end
  end

  test "script-aware justification excludes joined letters and Korean tracking while keeping mixed CJK opportunities" do
    for {left, right, mode, language, expected} <- [
          {"東", "京", :auto, "ja", true},
          {"東", "A", :auto, "zh-Hant", true},
          {"東", " ", :auto, "ja", false},
          {"ก", "ข", :auto, "th", true},
          {"한", "글", :auto, "ko", false},
          {"東", "京", :auto, "ko-KR", false},
          {"A", "B", :auto, "en", false},
          {"A", "B", :inter_character, "en", true},
          {"ب", "ب", :inter_character, "ar", false},
          {"ب", "ا", :auto, "ar", false},
          {"A", "́", :inter_character, "en", false},
          {"—", "—", :inter_character, "ja", false},
          {"東", "京", :inter_word, "ja", false}
        ] do
      assert Validator.justification_boundary?(left, right, mode, language) == expected
    end

    assert {:ok, %{text_justify: :inter_character}} =
             Validator.prepare(%{}, "text-justify", "distribute")

    parent = %{direction: :rtl, text_align: :start, text_align_last: :end}
    assert Validator.parent_alignment(%{_cascade_inherited: parent}, :text_align) == :right

    assert {:ok, %{text_align_last: :left}} =
             Validator.prepare(%{_cascade_inherited: parent}, "text-align-last", "match-parent")

    html =
      "<p dir='rtl' style='margin:0;width:100pt;font-size:12pt;text-align:justify;text-align-last:justify;text-justify:inter-character'>سلام</p>"

    assert {:ok, dom} = HtmlParser.parse(html)
    assert {:ok, styled} = Style.compute_detailed(dom, system_font_discovery: false)
    assert {:ok, layout} = Layout.layout(styled)
    [box] = Enum.filter(layout.boxes, &(&1.type == :text))
    assert Map.get(box, :_justification_spacing, 0.0) == 0
    assert_in_delta box.x, (100 - box.annotation_width) / 2, 0.025
    assert {:ok, pdf} = HtmlToPdf.render(html, system_font_discovery: false)
    assert {:ok, "سلام"} = Text.extract(pdf, layout: false)
  end

  test "PDF spacing admission rejects invalid numbers and offsets with the shared diagnostic contract" do
    alias NativeElixirPdfUtilities.HtmlToPdf.PdfWriter

    box = %{
      type: :text,
      text: "AéB",
      x: 0,
      y: 10,
      font_size: 10,
      color: {0, 0, 0},
      font: "Helvetica"
    }

    assert Validator.valid_paint?(Map.put(box, :_justification_boundaries, [1, 3]))

    for extra <- [
          %{word_spacing: :bad},
          %{word_spacing: 1.0e100},
          %{_justification_spacing: :bad},
          %{_justification_boundaries: nil},
          %{_justification_boundaries: [1, 1]},
          %{_justification_boundaries: [3, 1]},
          %{_justification_boundaries: [2]},
          %{_justification_boundaries: [4]},
          %{_justification_boundaries: [-1]}
        ] do
      invalid = Map.merge(box, extra)
      refute Validator.valid_paint?(invalid)

      assert {:error, {:invalid_pdf_input, diagnostic}} =
               PdfWriter.render([%{size: {100, 100}, boxes: [invalid]}])

      assert diagnostic.stage == :pdf
      assert diagnostic.reason == :invalid_pdf_input
      assert is_binary(diagnostic.message)
    end
  end

  test "built-in PDF text and explicit embedded glyphs apply admitted word spacing" do
    alias NativeElixirPdfUtilities.HtmlToPdf.PdfWriter

    base = %{
      type: :text,
      text: "A B",
      x: 0,
      y: 10,
      font_size: 10,
      color: {0, 0, 0},
      word_spacing: 5
    }

    builtin = Map.put(base, :font, "Helvetica")

    assert {:ok, pdf} =
             PdfWriter.render([%{size: {100, 100}, boxes: [builtin]}], compress_streams: false)

    assert pdf =~ "5 Tw"
    assert {:ok, "A B"} = Text.extract(pdf, layout: false)

    assert {:ok, dom} = HtmlParser.parse("<p>A B</p>")

    assert {:ok, %{children: [paragraph]}} =
             Style.compute_detailed(dom, system_font_discovery: false)

    font = paragraph.style.font_face
    glyphs = Enum.map(String.to_charlist("A B"), &Map.fetch!(font.cmap, &1))
    embedded = Map.merge(base, %{font: Font.pdf_name(font), font_face: font, glyph_ids: glyphs})

    assert {:error, {:invalid_pdf_input, diagnostic}} =
             PdfWriter.render([%{size: {100, 100}, boxes: [%{embedded | font_size: 0}]}])

    assert diagnostic.stage == :pdf

    assert {:ok, pdf} =
             HtmlToPdf.render("<p style='word-spacing:5pt'>A&nbsp;B</p>",
               system_font_discovery: false
             )

    assert {:ok, "A\u00A0B"} = Text.extract(pdf, layout: false)

    for box <- [embedded, Map.put(embedded, :glyph_advances, [6.0, 3.0, 6.0])] do
      assert {:ok, pdf} =
               PdfWriter.render([%{size: {100, 100}, boxes: [box]}], compress_streams: false)

      assert pdf =~ "] TJ"
      assert {:ok, "A B"} = Text.extract(pdf, layout: false)
    end
  end
end
