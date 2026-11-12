defmodule NativeElixirPdfUtilities.HtmlToPdf.InlineBidiTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.HtmlToPdf.{InlineBidi, UnicodeBidi, HtmlParser, Style}
  alias NativeElixirPdfUtilities.{HtmlToPdf, Text}

  alias NativeElixirPdfUtilities.Validators.{
    NativeGlyphSourceValidator,
    TextBidiValidator,
    CssValueValidator,
    CssCascadeValidator
  }

  test "an explicitly empty language cancels inherited shaping language" do
    assert {:ok, dom} = HtmlParser.parse("<div lang='he'><span lang=''>A</span></div>")

    assert {:ok, %{children: [parent]}} =
             Style.compute_detailed(dom, system_font_discovery: false)

    assert parent.style._text_language == "he"
    [child] = parent.children
    assert child.style._text_language == ""
    [text] = child.children
    assert text.style._text_language == ""
  end

  test "paragraph levels survive splitting and per-line visual allocation retains logical run order" do
    style = %{direction: :rtl, unicode_bidi: :normal}
    assert {:ok, runs} = InlineBidi.prepare([%{text: "אב abc גד", style: style}], style)
    assert Enum.map(runs, & &1.text) == ["אב ", "abc", " גד"]

    assert Enum.map(runs, fn run -> Enum.uniq(Enum.map(run.bidi_characters, & &1.level)) end) == [
             [1],
             [2],
             [1]
           ]

    positioned = InlineBidi.position(runs, 10, &String.length(&1.text))
    assert Enum.map(positioned, & &1._bidi_x) == [16, 13, 10]
    assert Enum.map_join(positioned, & &1.text) == "אב abc גד"
    middle = Enum.at(runs, 1) |> NativeGlyphSourceValidator.slice_run(1, "bc")
    assert Enum.map(middle.bidi_characters, & &1.level) == [2, 2]

    assert InlineBidi.position([middle], 0, &String.length(&1.text))
           |> hd()
           |> Map.fetch!(:_bidi_x) == 0

    assert {:ok, [plain]} = InlineBidi.prepare([%{text: "abc", style: %{}}], %{})
    refute Map.has_key?(plain, :bidi_characters)
    assert InlineBidi.position([plain], 0, &String.length(&1.text)) == [plain]
  end

  test "inline isolate and override scopes preserve owners and do not leak into siblings" do
    style = %{direction: :ltr}
    context = %{id: :isolated, mode: :isolate_override, direction: :rtl}

    runs = [
      %{text: "a ", style: style},
      %{text: "ABC", style: Map.put(style, :_bidi_contexts, [context])},
      %{text: " z", style: style}
    ]

    assert {:ok, prepared} = InlineBidi.prepare(runs, style)
    assert Enum.map(prepared, & &1.text) == ["a ", "ABC", " z"]
    assert Enum.map(Enum.at(prepared, 1).bidi_characters, & &1.level) == [3, 3, 3]
    positioned = InlineBidi.position(prepared, 0, &String.length(&1.text))
    assert Enum.map(positioned, & &1._bidi_x) == [0, 2, 5]
    assert Enum.map(List.last(positioned).bidi_characters, & &1.level) == [0, 0]
    assert TextBidiValidator.first_strong_direction("\u2066א\u2069a") == "ltr"
    assert TextBidiValidator.first_strong_direction("\u2066a\u2069א") == "rtl"
    assert TextBidiValidator.first_strong_direction("123") == nil
  end

  test "embedding, overrides and plaintext isolate their scalar levels from following siblings" do
    parent = %{direction: :ltr}

    for {mode, direction, text, expected} <- [
          {:embed, :ltr, "BC", 2},
          {:embed, :rtl, "BC", 2},
          {:bidi_override, :ltr, "BC", 2},
          {:bidi_override, :rtl, "BC", 1},
          {:plaintext, :ltr, "אב", 1}
        ] do
      scope = %{id: :scope, mode: mode, direction: direction}

      runs = [
        %{text: "a", style: parent},
        %{text: text, style: Map.put(parent, :_bidi_contexts, [scope])},
        %{text: "z", style: parent}
      ]

      assert {:ok, [before, selected, following]} = InlineBidi.prepare(runs, parent)
      assert Enum.map(selected.bidi_characters, & &1.level) == [expected, expected]
      assert Enum.map(before.bidi_characters, & &1.level) == [0]
      assert Enum.map(following.bidi_characters, & &1.level) == [0]
      assert Enum.map_join([before, selected, following], & &1.text) == "a" <> text <> "z"
    end

    parent = %{direction: :rtl, unicode_bidi: :plaintext}
    assert {:ok, [latin]} = InlineBidi.prepare([%{text: "abc", style: parent}], parent)
    assert Enum.map(latin.bidi_characters, & &1.level) == [0, 0, 0]
  end

  test "font work exhausted by a line-advance callback retains its resource diagnostic" do
    limits = NativeElixirPdfUtilities.Limits.effective()
    on_exit(fn -> NativeElixirPdfUtilities.Limits.install(limits) end)
    style = %{direction: :rtl}
    assert {:ok, runs} = InlineBidi.prepare([%{text: "אב", style: style}], style)
    NativeElixirPdfUtilities.Limits.install(%{limits | max_work: 3})

    assert {:render_resource_limit, {:error, {:resource_limit_exceeded, diagnostic}}} =
             catch_throw(
               InlineBidi.position(runs, 0, fn _ ->
                 send(self(), :line_advance_called)
                 NativeElixirPdfUtilities.Validators.FontValidator.reserve(:max_work, 2)
                 1
               end)
             )

    assert_received :line_advance_called
    assert diagnostic.stage == :font and diagnostic.message =~ "max_work"
  end

  test "all unicode-bidi values share CSS grammar, remain non-inherited and survive all" do
    initial = CssCascadeValidator.initial(%{})

    for value <- ~w(normal embed isolate bidi-override isolate-override plaintext) do
      assert {:ok, style} = CssValueValidator.validate_resolved(initial, "unicode-bidi", value)

      assert {:ok, inherited} =
               CssValueValidator.validate_resolved(
                 %{initial | unicode_bidi: style.unicode_bidi},
                 "all",
                 "initial"
               )

      assert inherited.unicode_bidi == style.unicode_bidi
    end

    assert {:ok, :isolate_override} = TextBidiValidator.prepare_css("isolate\\2d override")
    assert :error = TextBidiValidator.prepare_css("bogus")
    assert :error = TextBidiValidator.prepare_css("isolate normal")
    assert TextBidiValidator.valid_paint?(%{text: "אב", bidi_levels: [1, 1]})

    for levels <- [nil, [1], [1, 127], [1, :bad]] do
      refute TextBidiValidator.valid_paint?(%{text: "אב", bidi_levels: levels})
    end

    refute TextBidiValidator.valid_paint?(%{text: "a", bidi_direction: :bad})
  end

  test "HTML direction, bdi and bdo reach painting and retain logical extraction" do
    html = "<p dir=rtl>אב abc גד</p><p>a <bdo dir=rtl>ABC</bdo> z</p><p dir=auto>אב</p>"
    assert {:ok, dom} = HtmlParser.parse(html)
    assert {:ok, styled} = Style.compute_detailed(dom, system_font_discovery: false)

    assert Enum.any?(styled.children, fn node ->
             node.type == :element and Map.get(node.style, :direction) == :rtl
           end)

    assert {:ok, pdf} =
             HtmlToPdf.render(html, system_font_discovery: false, compress_streams: false)

    assert pdf =~ "/ActualText"
    assert {:ok, text} = Text.extract(pdf, layout: false)
    assert text =~ "אב abc גד"
    assert text =~ "a ABC z"
    assert {:ok, %{pages: [page]}} = Text.extract_spans(pdf)
    letters = Enum.filter(page.spans, &(&1.text in ["A", "B", "C"]))
    assert Enum.map(letters, & &1.text) == ["C", "B", "A"]
    [c, b, a] = letters
    assert c.x < b.x and b.x < a.x
    assert {:ok, %{levels: [1, 1]}} = UnicodeBidi.resolve("אב", :rtl)
  end
end
