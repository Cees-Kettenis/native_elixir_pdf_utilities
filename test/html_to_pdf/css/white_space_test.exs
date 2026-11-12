defmodule NativeElixirPdfUtilities.HtmlToPdf.WhiteSpaceTest do
  use ExUnit.Case, async: true
  alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, Layout, Style}
  alias NativeElixirPdfUtilities.Validators.{CssWhiteSpaceValidator, NativeGlyphSourceValidator}

  test "partial computed longhands inherit the other legacy whitespace axis" do
    assert CssWhiteSpaceValidator.compute_style(%{white_space: :pre, text_wrap_mode: :wrap}).white_space ==
             :pre_wrap

    assert CssWhiteSpaceValidator.compute_style(%{
             white_space: :pre,
             white_space_collapse: :collapse
           }).white_space == :nowrap

    assert CssWhiteSpaceValidator.compute_style(%{text_wrap_mode: :nowrap}).white_space == :nowrap
  end

  test "admitted legacy fitting projections trim a complete trailing separator run" do
    assert {:ok, dom} = HtmlParser.parse("<p style='white-space:pre;margin:0'>A   </p>")

    assert {:ok, %{children: [paragraph]} = styled} =
             Style.compute_detailed(dom, system_font_discovery: false)

    [text] = paragraph.children

    # Styled-tree callers may retain the collapse longhand while selecting the
    # legacy fitting mode. Both controls pass the shared style admission.
    paragraph = %{
      paragraph
      | style: Map.put(paragraph.style, :white_space, :normal),
        children: [%{text | style: Map.put(text.style, :white_space, :normal)}]
    }

    assert :ok = CssWhiteSpaceValidator.validate_style(paragraph.style, "p")
    assert {:ok, layout} = Layout.layout(%{styled | children: [paragraph]})
    [box] = Enum.filter(layout.boxes, &(&1.type == :text))
    assert box.text == "A"
    assert box.width > 0
  end

  test "edge trimming stops at preserved atoms and pre-wrap padding markers" do
    atomic = %{type: :inline_block, text: "", style: %{white_space: :normal}}
    assert CssWhiteSpaceValidator.trim_line_start([atomic]) == [atomic]

    marker = %{
      type: :inline_positioning_marker,
      text: "",
      style: %{white_space: :pre_wrap},
      _inline_padding_width: 2.0
    }

    text = %{text: "A  ", style: %{white_space: :pre_wrap}}

    measure = fn runs ->
      Enum.reduce(runs, 0, &(byte_size(&1.text) + Map.get(&1, :_inline_padding_width, 0) + &2))
    end

    assert CssWhiteSpaceValidator.alignment_width([text, marker], 1, false, measure) == 5

    assert CssWhiteSpaceValidator.trim_line_start([%{text: "  A", style: %{white_space: :pre}}]) ==
             [%{text: "  A", style: %{white_space: :pre}}]

    boundary = %{
      type: :inline_whitespace_boundary,
      text: "",
      edge: :after,
      controls: [:discard_after]
    }

    assert CssWhiteSpaceValidator.trim_runs([boundary, atomic], %{}) == [atomic]

    assert CssWhiteSpaceValidator.trim_runs([atomic], %{white_space_trim: [:discard_inner]}) == [
             atomic
           ]
  end

  test "the text-wrap-style longhand admits stable wrapping without changing collapse policy" do
    assert "text-wrap-style" in CssWhiteSpaceValidator.properties()

    assert {:ok, %{text_wrap_style: :stable, white_space: :pre}} =
             CssWhiteSpaceValidator.prepare(%{white_space: :pre}, "text-wrap-style", "stable")

    assert :error = CssWhiteSpaceValidator.prepare(%{}, "text-wrap-style", "unknown")
  end

  test "collapse and wrap longhands cascade independently of shorthand importance and inherit computed values" do
    html = """
    <style>div{white-space:pre-wrap!important;white-space-collapse:collapse!important;
      text-wrap-mode:nowrap}span{white-space-collapse:preserve-breaks;text-wrap-mode:nowrap}
      b{white-space:initial}i{white-space:inherit}</style>
    <div>A<span>B</span><b>C</b><i>D</i></div>
    """

    assert {:ok, dom} = HtmlParser.parse(html)

    assert {:ok, %{children: [parent]}} =
             Style.compute_detailed(dom, system_font_discovery: false)

    assert parent.style.white_space == :normal
    assert parent.style.white_space_collapse == :collapse
    assert parent.style.text_wrap_mode == :wrap
    [_, span, bold, italic] = parent.children
    assert span.style.white_space == :nowrap
    assert span.style.white_space_collapse == :preserve_breaks
    assert span.style.text_wrap_mode == :nowrap
    assert bold.style.white_space == :normal
    assert italic.style.white_space == :normal
    assert italic.style.white_space_collapse == :collapse
    assert italic.style.text_wrap_mode == :wrap

    for {value, expected} <- [
          {"preserve nowrap", :pre},
          {"wrap preserve-breaks", :pre_line},
          {"preserve-spaces", :pre_wrap},
          {"discard nowrap", :nowrap}
        ] do
      assert {:ok, model} = CssWhiteSpaceValidator.prepare(%{}, "white-space", value)
      assert model.white_space == expected
    end

    for value <- ["preserve collapse", "wrap nowrap", "wrap wrap", "unknown", ""] do
      assert :error = CssWhiteSpaceValidator.prepare(%{}, "white-space", value)
    end
  end

  test "preserve-spaces and discard share one normalization for ordinary and source-owned scalars" do
    for {collapse, expected} <- [
          {:preserve_spaces, "A   B C D"},
          {:discard, "ABCD"},
          {:collapse, "A B C D"},
          {:preserve_breaks, "A B\nC\nD"},
          {:preserve, "A \t B\nC\nD"},
          {:break_spaces, "A \t B\nC\nD"}
        ] do
      source = "A \t B\r\nC\nD"
      style = %{white_space_collapse: collapse}
      assert NativeGlyphSourceValidator.normalize_text(source, style) == expected

      characters =
        Enum.map(String.codepoints(source), &%{displayed: &1, fragments: [%{text: &1}]})

      selected = NativeGlyphSourceValidator.normalize_characters(characters, style)
      assert Enum.map_join(selected, & &1.displayed) == expected
      assert Enum.all?(selected, &(&1.displayed != ""))
    end

    assert NativeGlyphSourceValidator.normalize_text(" ", %{white_space_collapse: :discard}) == ""
  end

  test "Chinese and Japanese segment breaks avoid a word gap while Korean and Latin retain it" do
    for {language, source, expected} <- [
          {"ja", "東京\n京都", "東京京都"},
          {"zh-Hant", "東京 \r\n 京都", "東京京都"},
          {"und", "東京。\n京都", "東京。京都"},
          {"ko", "東京\n京都", "東京 京都"},
          {"ko", "한\n글", "한 글"},
          {"en", "One\nTwo", "One Two"},
          {"ja", "東京\nABC", "東京 ABC"}
        ] do
      style = %{white_space_collapse: :collapse, _text_language: language}
      assert NativeGlyphSourceValidator.normalize_text(source, style) == expected

      characters =
        Enum.map(String.codepoints(source), &%{displayed: &1, fragments: [%{text: &1}]})

      selected = NativeGlyphSourceValidator.normalize_characters(characters, style)
      assert Enum.map_join(selected, & &1.displayed) == expected
    end

    assert NativeGlyphSourceValidator.normalize_text("東京\n京都", %{
             white_space_collapse: :preserve_breaks,
             _text_language: "ja"
           }) == "東京\n京都"
  end

  test "number, fractional and length tab stops use the containing block space in inline, table, flex and grid consumers" do
    for {before, after_markup} <- [
          {"<div>", "</div>"},
          {"<table><tr><td>", "</td></tr></table>"},
          {"<div style='display:flex'><div>", "</div></div>"},
          {"<div style='display:grid;grid-template-columns:100pt'><div>", "</div></div>"}
        ],
        size <- ["4", "2.5", "20pt", "0"] do
      html =
        "#{before}<p style='font-family:DejaVu Sans Mono;font-size:10pt;line-height:14pt;margin:0;white-space:pre;tab-size:#{size}'>A\tB\tC</p>#{after_markup}"

      assert {:ok, dom} = HtmlParser.parse(html)
      assert {:ok, styled} = Style.compute_detailed(dom, system_font_discovery: false)
      assert {:ok, layout} = Layout.layout(styled)

      boxes =
        Enum.flat_map(layout.boxes, fn box ->
          if box.type == :glyph_group, do: box.boxes, else: [box]
        end)
        |> Enum.filter(&(Map.get(&1, :text) in ["A", "B", "C"]))

      assert length(boxes) == 3
      first = hd(boxes).x
      space = NativeElixirPdfUtilities.HtmlToPdf.Font.css_text_width(" ", hd(boxes).font_face, 10)

      step =
        case size do
          "4" -> 4 * space
          "2.5" -> 2.5 * space
          "20pt" -> 20
          "0" -> space
        end

      for {box, expected} <- Enum.zip(boxes, [0, step, 2 * step]) do
        assert_in_delta box.x - first,
                        expected,
                        0.02,
                        inspect({before, size, Enum.map(boxes, &Map.take(&1, [:text, :x]))})
      end
    end

    assert CssWhiteSpaceValidator.tab_advance(%{tab_size: {:length, 20.0}}, 18.0, 6.0) == 22.0
    assert CssWhiteSpaceValidator.tab_advance(%{tab_size: {:length, 20.0}}, 17.0, 6.0) == 3.0
  end

  test "tabs in larger inline fonts retain the block's stop measure and their original extraction" do
    alias NativeElixirPdfUtilities.{HtmlToPdf, Text}
    alias NativeElixirPdfUtilities.HtmlToPdf.Font

    html =
      "<p style='font-family:DejaVu Sans Mono;font-size:10pt;line-height:30pt;margin:0;white-space:pre;tab-size:4'><span style='font-size:20pt'>A\t</span>B\tC</p>"

    options = [system_font_discovery: false]
    assert {:ok, dom} = HtmlParser.parse(html)
    assert {:ok, styled} = Style.compute_detailed(dom, options)
    assert {:ok, layout} = Layout.layout(styled)
    [group] = Enum.filter(layout.boxes, &(&1.type == :glyph_group))
    [first, second, third] = Enum.filter(group.boxes, &(&1.text in ["A", "B", "C"]))
    space = Font.css_text_width(" ", second.font_face, 10)
    assert_in_delta second.x - first.x, 4 * space, 0.012
    assert_in_delta third.x - first.x, 8 * space, 0.012
    assert {:ok, pdf} = HtmlToPdf.render(html, options)
    assert {:ok, "A\tB\tC"} = Text.extract(pdf, layout: false)
  end

  test "a preserved tab moved by wrapping resolves its stop from the new line origin" do
    alias NativeElixirPdfUtilities.{HtmlToPdf, Text}
    alias NativeElixirPdfUtilities.HtmlToPdf.Font

    html =
      "<p style='font-family:DejaVu Sans Mono;font-size:10pt;line-height:14pt;margin:0;white-space:break-spaces;tab-size:4;width:31pt'>AAAA \tB</p>"

    assert {:ok, dom} = HtmlParser.parse(html)
    assert {:ok, styled} = Style.compute_detailed(dom, system_font_discovery: false)
    assert {:ok, layout} = Layout.layout(styled)

    boxes =
      Enum.flat_map(layout.boxes, fn box ->
        if box.type == :glyph_group, do: box.boxes, else: [box]
      end)

    first = Enum.find(boxes, &String.starts_with?(Map.get(&1, :text, ""), "AAAA"))
    last = Enum.find(boxes, &(Map.get(&1, :text) == "B"))
    assert last.y < first.y
    space = Font.css_text_width(" ", last.font_face, 10)
    assert_in_delta last.x - first.x, space * 4, 0.02
    assert {:ok, pdf} = HtmlToPdf.render(html, system_font_discovery: false)
    assert {:ok, "AAAA \tB"} = Text.extract(pdf, layout: false)
  end

  test "tab paragraph replacements retain forced newlines and remove trimmed whitespace-only lines" do
    alias NativeElixirPdfUtilities.{HtmlToPdf, Text}
    html = "<p style='white-space:pre;margin:0'>A\tB\nC</p>"
    assert {:ok, pdf} = HtmlToPdf.render(html, system_font_discovery: false)
    assert {:ok, "A\tB\nC"} = Text.extract(pdf, layout: false)
    following = "<p style='margin:0'>Following</p>"

    positions =
      for prefix <- [
            "",
            "<div style='white-space:pre;white-space-trim:discard-inner'>\n \n</div>"
          ] do
        assert {:ok, dom} = HtmlParser.parse(prefix <> following)
        assert {:ok, styled} = Style.compute_detailed(dom, system_font_discovery: false)
        assert {:ok, layout} = Layout.layout(styled)
        Enum.find(layout.boxes, &(Map.get(&1, :text) == "Following")).y
      end

    assert Enum.uniq(positions) |> length() == 1
  end

  test "invalid styled tab and whitespace controls return actionable diagnostics" do
    for style <- [
          nil,
          %{tab_size: -1},
          %{tab_size: {:number, -1}},
          %{tab_size: {:number, 1.0e100}},
          %{tab_size: {:length, {:percent, 0.5}}},
          %{tab_size: {:length, -1}},
          %{white_space: :unknown},
          %{white_space_collapse: :unknown},
          %{text_wrap_mode: :unknown}
        ] do
      assert {:error,
              {:invalid_layout,
               %{stage: :layout, operation: :layout, source: "p", message: message}}} =
               CssWhiteSpaceValidator.validate_style(style, "p")

      assert message =~ "whitespace"
    end

    for value <- ["-1", "-1pt", "50%", "calc(50%)", "unknown"] do
      refute match?({:ok, _}, CssWhiteSpaceValidator.prepare(%{}, "tab-size", value))
    end

    assert :error = CssWhiteSpaceValidator.prepare(%{}, "unknown", "normal")
  end

  test "inner trimming preserves block indentation and inline scopes trim only their admitted neighboring whitespace" do
    for {html, expected} <- [
          {"<p>A <span style='white-space-trim:discard-before discard-after discard-inner'>  B  </span> C</p>",
           "ABC"},
          {"<p>A <em> </em><span style='white-space:preserve nowrap discard-before discard-after discard-inner'>\t B </span> C</p>",
           "ABC"},
          {"<p style='white-space:pre'>A <span style='white-space-trim:discard-before discard-after'>B</span> C</p>",
           "A B C"},
          {"<pre style='white-space-trim:discard-inner'>\n\n  A\n  B\n\n</pre>", "  A  B"},
          {"<p style='white-space:pre;white-space-trim:discard-inner'>  A  </p>", "  A  "}
        ] do
      assert {:ok, dom} = HtmlParser.parse(html)
      assert {:ok, styled} = Style.compute_detailed(dom, system_font_discovery: false)
      assert {:ok, layout} = Layout.layout(styled)

      visible =
        Enum.flat_map(layout.boxes, fn box ->
          if box.type == :glyph_group, do: box.boxes, else: [box]
        end)
        |> Enum.filter(&(&1.type == :text))
        |> Enum.map_join(& &1.text)

      assert visible == expected, inspect({html, visible})
    end

    assert {:ok, dom} =
             HtmlParser.parse("<div style='white-space-trim:discard-inner'><span>A</span></div>")

    assert {:ok, %{children: [%{children: [span]}]}} =
             Style.compute_detailed(dom, system_font_discovery: false)

    assert Map.get(span.style, :white_space_trim, []) == []

    for value <- ["discard-before discard-before", "none discard-inner", "unknown", ""] do
      assert :error = CssWhiteSpaceValidator.prepare(%{}, "white-space-trim", value)
    end

    for value <- [nil, :discard_inner, [:unknown], [:discard_inner, :discard_inner]] do
      assert {:error, {:invalid_layout, _}} =
               CssWhiteSpaceValidator.validate_style(%{white_space_trim: value}, "span")
    end

    assert {:ok, %{white_space_trim: []}} =
             CssWhiteSpaceValidator.prepare(%{}, "white-space-trim", "none")

    assert :error =
             CssWhiteSpaceValidator.prepare(%{}, "white-space", "preserve none discard-inner")
  end

  test "preserved trailing spaces hang on soft wraps and conditionally on forced and final lines" do
    alias NativeElixirPdfUtilities.HtmlToPdf.Font
    alias NativeElixirPdfUtilities.Validators.HtmlLineBreakValidator

    for {mode, content, width, measured} <- [
          {"pre-wrap", "A   ", 40, "A   "},
          {"pre-wrap", "A   ", 12, "A"},
          {"pre-wrap", "A    BBBB", 40, "A"},
          {"pre-wrap", "A   <br>B", 40, "A   "},
          {"pre", "A   ", 40, "A   "},
          {"break-spaces", "A   ", 40, "A   "}
        ] do
      html =
        "<p style='font-family:DejaVu Sans Mono;font-size:10pt;line-height:14pt;margin:0;white-space:#{mode};text-align:right;width:#{width}pt'>#{content}</p>"

      assert {:ok, dom} = HtmlParser.parse(html)
      assert {:ok, styled} = Style.compute_detailed(dom, system_font_discovery: false)
      assert {:ok, layout} = Layout.layout(styled)
      first = Enum.find(layout.boxes, &(Map.get(&1, :text, "") |> String.starts_with?("A")))

      advance =
        Font.css_text_width(measured, first.font_face, 10)
        |> HtmlLineBreakValidator.snap_fragment_width()

      assert_in_delta first.x,
                      width - advance,
                      0.012,
                      inspect({mode, content, first.x, width - advance})

      assert String.ends_with?(first.text, " ")
    end
  end
end
