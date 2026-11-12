defmodule NativeElixirPdfUtilities.HtmlToPdf.HyphenationTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.HtmlToPdf.{Hyphenator, RenderCache}
  alias NativeElixirPdfUtilities.Limits
  alias NativeElixirPdfUtilities.Validators.{HtmlValidator, HyphenationValidator}

  setup do
    original = Limits.effective()
    on_exit(fn -> Limits.install(original) end)
    :ok
  end

  test "pattern traversal preserves exhaustion after the word has been admitted" do
    alias NativeElixirPdfUtilities.TestSupport.ResourceBudget

    assert {:ok, dictionaries} =
             HyphenationValidator.prepare_dictionaries(%{
               "en" => %{patterns: ["a1b"], provenance: %{source: "Authored pattern"}}
             })

    assert {{:ok, _}, admitted} =
             ResourceBudget.measure(:max_work, fn ->
               HyphenationValidator.prepare_word("abcd", "en")
             end)

    Limits.install(Map.put(Limits.effective(), :max_work, admitted + 1))

    assert :checked =
             HtmlValidator.with_render_budget(fn ->
               assert {:error, {:resource_limit_exceeded, diagnostic}} =
                        Hyphenator.opportunities("abcd", dictionaries, "en")

               assert diagnostic.stage == :layout and diagnostic.message =~ "max_work"
               :checked
             end)
  end

  test "a matching dictionary with no word opportunity leaves source runs unchanged" do
    assert {:ok, dictionaries} =
             HyphenationValidator.prepare_dictionaries(%{
               "en" => %{
                 exceptions: ["hy-phen-ation"],
                 provenance: %{source: "Authored exception"}
               }
             })

    run = %{
      text: "plain",
      style: %{hyphens: :auto, _text_language: "en", _hyphenation_dictionaries: dictionaries}
    }

    assert {:ok, [^run]} = Hyphenator.prepare_runs([run])
    Limits.install(Map.put(Limits.effective(), :max_work, 0))

    assert :checked =
             HtmlValidator.with_render_budget(fn ->
               assert {:error, {:resource_limit_exceeded, diagnostic}} =
                        Hyphenator.opportunities("plain", dictionaries, "en")

               assert diagnostic.message =~ "max_work"
               :checked
             end)
  end

  test "hyphenation zones preserve used font-unit resolution failures" do
    alias NativeElixirPdfUtilities.Validators.{CssHyphenationValidator, CssLengthValidator}
    assert {:ok, zone} = CssLengthValidator.prepare("calc(100 * 1em)", :margin)
    Limits.install(Map.put(Limits.effective(), :max_numeric_magnitude, 1000))

    line = %{
      point: {make_ref(), 2},
      natural_width: 10,
      style: %{font_size: 12, hyphenate_limit_zone: zone}
    }

    assert {:render_resource_limit, {:error, {:resource_limit_exceeded, diagnostic}}} =
             catch_throw(CssHyphenationValidator.rejected_breaks([line, nil], 200))

    assert diagnostic.message =~ "max_numeric_magnitude"
  end

  test "automatic and unchanged source groups retain owners through generated and manual breaks" do
    alias NativeElixirPdfUtilities.Validators.CssHyphenationValidator

    assert {:ok, dictionaries} =
             HyphenationValidator.prepare_dictionaries(%{
               "en" => %{
                 exceptions: ["hy-phen-ation"],
                 provenance: %{source: "Authored exception"}
               }
             })

    style = %{hyphens: :auto, _text_language: "en", _hyphenation_dictionaries: dictionaries}

    source =
      Enum.map(
        String.codepoints("hyphenation"),
        &%{displayed: &1, fragments: [%{owner: :word, text: &1}]}
      )

    runs = [
      %{text: "hyphenation", style: style, source_glyph_characters: source},
      %{text: " unchanged ", style: %{hyphens: :manual}},
      %{text: "hy\u00ADphenation", style: style},
      %{text: "plain", style: style},
      %{text: "unknown", style: Map.put(style, :_text_language, "de")}
    ]

    assert {:ok, prepared} = Hyphenator.prepare_runs(runs)

    assert Enum.map(prepared, & &1.text) == [
             "hy\u00ADphen\u00ADation",
             " unchanged ",
             "hy\u00ADphenation",
             "plain",
             "unknown"
           ]

    assert Enum.filter(
             hd(prepared).source_glyph_characters,
             &(Map.get(hd(&1.fragments ++ [%{}]), :owner) == :word)
           ) == source

    assert Enum.all?(Enum.at(prepared, 1).hyphenation_characters, &(not &1.generated?))
    assert Hyphenator.discard_breaks([Enum.at(runs, 1)], MapSet.new()) == [Enum.at(runs, 1)]
    facts = [%{height: 10, break: %{point: :manual, style: %{hyphenate_limit_last: :none}}}]
    assert CssHyphenationValidator.rejected_fragmentainer_breaks(facts, 5) == MapSet.new()

    original = Limits.effective()

    for {allowance, stage} <- [{1, :layout}, {byte_size("hyphenation") + 1, :css}] do
      Limits.install(%{original | max_work: allowance})

      assert {:error, {:resource_limit_exceeded, diagnostic}} =
               HtmlValidator.with_render_budget(fn -> Hyphenator.prepare_runs([hd(runs)]) end)

      assert diagnostic.stage == stage and diagnostic.message =~ "max_work"
    end
  end

  test "typography declarations propagate numeric token limits from the shared value validators" do
    alias NativeElixirPdfUtilities.Validators.{
      CssTextShadowValidator,
      CssTextSpacingValidator,
      CssInitialLetterValidator
    }

    Limits.install(Map.put(Limits.effective(), :max_numeric_token_bytes, 3))

    for operation <- [
          fn -> CssTextShadowValidator.prepare(%{}, "text-shadow", "123ch 1pt") end,
          fn -> CssTextSpacingValidator.prepare(%{}, "text-indent", "123ch") end,
          fn -> CssInitialLetterValidator.prepare(%{}, "initial-letter", "calc(1234)") end
        ] do
      assert {:error, {:resource_limit_exceeded, diagnostic}} =
               HtmlValidator.with_render_budget(operation)

      assert diagnostic.reason == :resource_limit_exceeded
      assert is_atom(diagnostic.stage) and diagnostic.message =~ "max_numeric_token_bytes"
    end
  end

  test "dictionary validation retains resource failures inside an enclosing render scope" do
    original = Limits.effective()
    definition = %{"en" => %{patterns: ["a1b"], provenance: %{source: "Authored"}}}
    assert {:ok, model} = HyphenationValidator.prepare_dictionaries(definition)
    Limits.install(Map.put(original, :max_work, 1))

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             HtmlValidator.with_render_budget(fn ->
               HyphenationValidator.prepare_dictionaries(definition)
             end)

    assert diagnostic.message =~ "max_work"

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             HyphenationValidator.validate_prepared(model)

    assert diagnostic.message =~ "max_work"
    Limits.install(Map.put(original, :max_entries, 1))

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             HyphenationValidator.validate_prepared(Map.put(model, "de", model["en"]))

    assert diagnostic.stage == :layout and diagnostic.message =~ "max_entries"
  end

  test "Liang maximum weights, odd boundaries and start/end anchors have independently calculated positions" do
    provenance = %{source: "Authored arithmetic patterns", version: "1"}

    assert {:ok, dictionaries} =
             HyphenationValidator.prepare_dictionaries(%{
               "en" => %{patterns: ["hy3phen", "phen3ation"], provenance: provenance},
               "en-GB" => %{
                 patterns: [".hy3", "a1tion."],
                 provenance: %{source: "Authored anchors"}
               }
             })

    assert {:ok, %{breaks: [2, 6], language: "en", provenance: ^provenance}} =
             Hyphenator.opportunities("HYPHENATION", dictionaries, "en-US-u-co-standard")

    assert {:ok, %{breaks: [2, 7], language: "en-gb"}} =
             Hyphenator.opportunities("hyphenation", dictionaries, "en-GB")

    assert {:ok, %{breaks: [2]}} = Hyphenator.opportunities("nation", dictionaries, "en-GB")
    assert {:ok, %{breaks: []}} = Hyphenator.opportunities("nations", dictionaries, "en-GB")
    assert {:ok, %{breaks: []}} = Hyphenator.opportunities("shyphen", dictionaries, "en-GB")

    assert {:ok, %{breaks: [], language: nil, provenance: nil}} =
             Hyphenator.opportunities("hyphenation", dictionaries, "de")

    for {patterns, expected} <- [{["ab3c", "ab2c"], [2]}, {["ab3c", "ab4c"], []}] do
      assert {:ok, dictionaries} =
               HyphenationValidator.prepare_dictionaries(%{
                 "en" => %{patterns: patterns, provenance: provenance}
               })

      assert {:ok, %{breaks: ^expected}} = Hyphenator.opportunities("abcd", dictionaries, "en")
    end
  end

  test "exceptions, canonical graphemes, Turkish casing and language aliases retain original byte offsets" do
    assert {:ok, dictionaries} =
             HyphenationValidator.prepare_dictionaries(%{
               "en" => %{
                 patterns: ["hy3phen"],
                 exceptions: ["hyphenation", "com-pu-ter"],
                 provenance: %{source: "Authored exceptions"}
               },
               "fr" => %{patterns: ["é1t"], provenance: %{source: "Authored accent"}},
               "tr" => %{patterns: ["ı1r"], provenance: %{source: "Authored Turkish"}},
               "iw" => %{
                 patterns: [],
                 exceptions: [],
                 provenance: %{source: "Explicit empty dictionary"}
               }
             })

    assert {:ok, %{breaks: []}} = Hyphenator.opportunities("hyphenation", dictionaries, "en")
    assert {:ok, %{breaks: [3, 5]}} = Hyphenator.opportunities("COMPUTER", dictionaries, "en-US")
    assert {:ok, %{breaks: [2]}} = Hyphenator.opportunities("Été", dictionaries, "fr")
    assert {:ok, %{breaks: [3]}} = Hyphenator.opportunities("Été", dictionaries, "fr")
    assert {:ok, %{breaks: [1]}} = Hyphenator.opportunities("IRMAK", dictionaries, "tr-TR")

    assert {:ok, %{breaks: [], language: "he"}} =
             Hyphenator.opportunities("אב", dictionaries, "he")

    assert {:ok, %{}} = HyphenationValidator.prepare_dictionaries(%{})

    RenderCache.with_stylesheets(fn ->
      input = %{"en" => %{patterns: ["a1b"], provenance: %{source: "First"}}}
      assert {:ok, first} = HyphenationValidator.prepare_dictionaries(input)
      assert {:ok, ^first} = HyphenationValidator.prepare_dictionaries(input)

      assert {:ok, second} =
               HyphenationValidator.prepare_dictionaries(%{
                 "de" => %{exceptions: ["ab-cd"], provenance: %{source: "Child document"}}
               })

      assert Map.has_key?(second, "de") and not Map.has_key?(second, "en")
    end)
  end

  test "malformed dictionaries and words return shared actionable diagnostics" do
    valid = %{patterns: ["a1b"], provenance: %{source: "Authored"}}

    for input <- [
          nil,
          [],
          %{nil => valid},
          %{"" => valid},
          %{"en" => nil},
          %{"iw" => valid, "he" => valid},
          %{"en" => Map.put(valid, :unknown, true)},
          %{"en" => %{valid | provenance: nil}},
          %{"en" => %{valid | provenance: %{source: "", license: 1}}},
          %{"en" => %{valid | provenance: %{source: "test", unknown: "value"}}},
          %{"en" => %{valid | patterns: nil}},
          %{"en" => Map.put(valid, :exceptions, nil)}
        ] do
      assert {:error, {:invalid_options, diagnostic}} =
               HyphenationValidator.prepare_dictionaries(input)

      assert diagnostic.stage == :options and diagnostic.operation == :prepare_hyphenation
      assert diagnostic.module == HyphenationValidator and is_binary(diagnostic.message)
    end

    for pattern <- [nil, <<255>>, "a12b", "a.b", "a-b", "ABC", "é1t", "."] do
      assert {:error, {:invalid_options, %{source: "en"}}} =
               HyphenationValidator.prepare_dictionaries(%{
                 "en" => %{valid | patterns: [pattern]}
               })
    end

    for exceptions <- [[nil], [<<255>>], ["-word"], ["word-"], ["wo--rd"], ["wo-rd", "word"]] do
      assert {:error, {:invalid_options, _}} =
               HyphenationValidator.prepare_dictionaries(%{
                 "en" => Map.put(valid, :exceptions, exceptions)
               })
    end

    for word <- [nil, <<255>>, "", "two words", "hy-phen"] do
      assert {:error, {:invalid_text, %{operation: :hyphenate_word}}} =
               Hyphenator.opportunities(word, %{}, "en")
    end

    assert {:error, {:invalid_document, _}} = Hyphenator.opportunities("word", %{}, false)
  end

  test "dictionary allocation, input bytes and matching work use central limits" do
    valid = %{patterns: ["a1b", "b1c"], provenance: %{source: "Authored"}}
    original = Limits.effective()

    for {limit, value} <- [
          {:max_entries, 1},
          {:max_entries, 1},
          {:max_input_bytes, 1},
          {:max_entries, 1},
          {:max_work, 1}
        ] do
      Limits.install(Map.put(original, limit, value))

      assert {:error, {:resource_limit_exceeded, diagnostic}} =
               HyphenationValidator.prepare_dictionaries(%{"en" => valid, "de" => valid})

      assert diagnostic.message =~ Atom.to_string(limit)
    end

    Limits.install(original)
    assert {:ok, dictionaries} = HyphenationValidator.prepare_dictionaries(%{"en" => valid})
    Limits.install(Map.put(original, :max_work, 4))

    assert {:error, {:resource_limit_exceeded, _}} =
             Hyphenator.opportunities("abcd", dictionaries, "en")

    Limits.install(Map.put(original, :max_entries, 2))

    assert {:error, {:resource_limit_exceeded, _}} =
             Hyphenator.opportunities("abcd", dictionaries, "en")

    Limits.install(original)

    HtmlValidator.with_render_budget(fn ->
      assert {:ok, %{breaks: [1, 2]}} =
               Hyphenator.opportunities("abcd", dictionaries, "en")
    end)
  end

  test "automatic opportunities cross spans, shape taken hyphens and retain original PDF extraction" do
    alias NativeElixirPdfUtilities.{HtmlToPdf, Text}
    alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, Style, Layout}

    dictionaries = %{
      "en" => %{exceptions: ["hy-phen-ation"], provenance: %{source: "Authored integration word"}}
    }

    options = [hyphenation_dictionaries: dictionaries, system_font_discovery: false]

    for content <- [
          "hyphenation",
          "hy<span>phen</span>ation",
          "<span>hyphenation</span>",
          "<span><span>hyphenation</span></span>",
          "one hyphenation two hyphenation"
        ] do
      html = "<p lang='en' style='margin:0;width:40pt;hyphens:auto'>#{content}</p>"
      assert {:ok, dom} = HtmlParser.parse(html)
      assert {:ok, styled} = Style.compute_detailed(dom, options)
      assert {:ok, layout} = Layout.layout(styled, options)
      groups = Enum.filter(layout.boxes, &(&1.type == :glyph_group))
      assert length(groups) >= 3

      assert Enum.any?(groups, fn group ->
               Enum.any?(group.boxes, &String.contains?(&1.text, "‐"))
             end)

      assert {:ok, pdf} = HtmlToPdf.render(html, options)
      assert {:ok, text} = Text.extract(pdf, layout: false)
      expected = String.replace(content, ["<span>", "</span>"], "")
      assert text == expected
      refute text =~ "‐"
      refute text =~ "\u00AD"
    end
  end

  test "hyphenation CSS uses shared grammars, inherits controls and suppresses manual opportunities with none" do
    alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, Style, Layout}
    alias NativeElixirPdfUtilities.Validators.{CssHyphenationValidator, HtmlLineBreakValidator}
    assert "hyphenate-limit-zone" in CssHyphenationValidator.properties()
    assert :ok = CssHyphenationValidator.validate_style(CssHyphenationValidator.defaults(), "p")

    for {property, value, field, expected} <- [
          {"hyphens", "auto", :hyphens, :auto},
          {"hyphens", "manual", :hyphens, :manual},
          {"hyphens", "none", :hyphens, :none},
          {"hyphenate-character", "auto", :hyphenate_character, :auto},
          {"hyphenate-character", "'=='", :hyphenate_character, "=="},
          {"hyphenate-limit-chars", "auto", :hyphenate_limit_chars, {:auto, :auto, :auto}},
          {"hyphenate-limit-chars", "6 3", :hyphenate_limit_chars, {6, 3, 3}},
          {"hyphenate-limit-chars", "7 auto 4", :hyphenate_limit_chars, {7, :auto, 4}},
          {"hyphenate-limit-lines", "no-limit", :hyphenate_limit_lines, :no_limit},
          {"hyphenate-limit-lines", "calc(1 + 1)", :hyphenate_limit_lines, 2},
          {"hyphenate-limit-last", "always", :hyphenate_limit_last, :always},
          {"hyphenate-limit-last", "column", :hyphenate_limit_last, :column},
          {"hyphenate-limit-last", "page", :hyphenate_limit_last, :page},
          {"hyphenate-limit-last", "spread", :hyphenate_limit_last, :spread},
          {"hyphenate-limit-last", "none", :hyphenate_limit_last, :none},
          {"hyphenate-limit-zone", "3pt", :hyphenate_limit_zone, 3.0}
        ] do
      assert {:ok, style} = CssHyphenationValidator.prepare(%{}, property, value)
      assert style[field] == expected
      assert :ok = CssHyphenationValidator.validate_style(style, "p")
    end

    for {property, value} <- [
          {"hyphens", "always"},
          {"hyphenate-character", "none"},
          {"hyphenate-limit-chars", ""},
          {"hyphenate-limit-chars", "1 2 3 4"},
          {"hyphenate-limit-chars", "auto -1"},
          {"hyphenate-limit-lines", "-1"},
          {"hyphenate-limit-last", "never"},
          {"hyphenate-limit-zone", "-3pt"},
          {"unknown", "auto"}
        ] do
      assert CssHyphenationValidator.prepare(%{}, property, value) in [
               :error,
               {:error, :invalid_document}
             ]
    end

    assert {:error, {:invalid_layout, _}} = CssHyphenationValidator.validate_style(nil, "p")

    for controls <- [
          %{hyphens: :invalid},
          %{hyphenate_character: <<255>>},
          %{hyphenate_limit_chars: {1, 2, -1}},
          %{hyphenate_limit_chars: false},
          %{hyphenate_limit_lines: -1},
          %{hyphenate_limit_last: :invalid},
          %{hyphenate_limit_zone: :invalid}
        ] do
      assert {:error, {:invalid_layout, %{operation: :layout}}} =
               CssHyphenationValidator.validate_style(controls, "p")
    end

    assert {:ok, ["co\u00ADoperate"]} =
             HtmlLineBreakValidator.segments("co\u00ADoperate", "", %{hyphens: :none})

    for {mode, count} <- [{"none", 1}, {"manual", 2}] do
      html =
        "<p style='margin:0;width:45pt;hyphens:#{mode};hyphenate-character:\"==\"'>co&shy;operate</p>"

      assert {:ok, dom} = HtmlParser.parse(html)
      assert {:ok, styled} = Style.compute_detailed(dom, system_font_discovery: false)
      assert {:ok, layout} = Layout.layout(styled)
      groups = Enum.filter(layout.boxes, &(&1.type == :glyph_group))
      assert length(groups) == 1

      boxes =
        Enum.flat_map(layout.boxes, fn box ->
          if box.type == :glyph_group, do: box.boxes, else: [box]
        end)
        |> Enum.filter(&(&1.type == :text))

      assert length(Enum.uniq_by(boxes, & &1.y)) == count
      assert Enum.any?(boxes, &String.contains?(&1.text, "==")) == (mode == "manual")
    end
  end

  test "styled dictionary metadata cannot supply a forged trie or missing admission definition" do
    assert {:ok, model} =
             HyphenationValidator.prepare_dictionaries(%{
               "en" => %{patterns: ["a1b"], provenance: %{source: "Authored"}}
             })

    assert :ok = HyphenationValidator.validate_prepared(model)

    RenderCache.with_stylesheets(fn ->
      assert :ok = HyphenationValidator.validate_prepared(model)
      assert :ok = HyphenationValidator.validate_prepared(model)

      for forged <- [
            nil,
            %{"en" => %{}},
            put_in(model, ["en", :trie], %{}),
            put_in(model, ["en", :definition], %{patterns: nil})
          ] do
        assert {:error, {:invalid_layout, %{operation: :layout}}} =
                 HyphenationValidator.validate_prepared(forged)
      end
    end)
  end

  test "character limits count spacing marks while excluding nonspacing marks and intra-word punctuation" do
    alias NativeElixirPdfUtilities.Validators.CssHyphenationValidator
    assert CssHyphenationValidator.used_character_limits(%{}) == {5, 2, 2}

    assert CssHyphenationValidator.used_character_limits(%{hyphenate_limit_chars: {6, 3, 4}}) ==
             {6, 3, 4}

    for {word, point, limits, expected} <- [
          {"a'bcde", 3, {6, 2, 2}, []},
          {"a'bcde", 3, {5, 2, 3}, [3]},
          {"ábcde", 4, {6, 2, 2}, []},
          {"ábcde", 4, {5, 2, 3}, [4]},
          {"aःbcde", 5, {6, 3, 3}, [5]},
          {"aःbcde", 5, {6, 4, 3}, []}
        ] do
      assert {:ok, prepared} = HyphenationValidator.prepare_word(word, "und")

      assert CssHyphenationValidator.allowed_points(prepared, [point], %{
               hyphenate_limit_chars: limits
             }) == expected
    end
  end

  test "consecutive-line, last-full-line and unused-zone limits refit original source opportunities" do
    alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, Layout, Style}
    alias NativeElixirPdfUtilities.{HtmlToPdf, Text}

    options = [
      system_font_discovery: false,
      hyphenation_dictionaries: %{
        "en" => %{exceptions: ["hy-phen-ation"], provenance: %{source: "Authored limit word"}}
      }
    ]

    for {control, expected_hyphens} <- [
          {"", 2},
          {"hyphenate-limit-lines:1", 1},
          {"hyphenate-limit-lines:0", 0},
          {"hyphenate-limit-last:always", 0},
          {"hyphenate-limit-zone:40pt", 0},
          {"hyphenate-limit-zone:100%", 0},
          {"hyphenate-limit-zone:calc(100% - 0pt)", 0}
        ] do
      html = "<p lang='en' style='margin:0;width:40pt;hyphens:auto;#{control}'>hyphenation</p>"
      assert {:ok, dom} = HtmlParser.parse(html)
      assert {:ok, styled} = Style.compute_detailed(dom, options)
      assert {:ok, layout} = Layout.layout(styled, options)

      visible =
        Enum.flat_map(layout.boxes, fn box ->
          if box.type == :glyph_group, do: box.boxes, else: [box]
        end)
        |> Enum.filter(&(&1.type == :text))
        |> Enum.map_join(& &1.text)

      assert length(String.split(visible, "‐")) - 1 == expected_hyphens, control
      assert {:ok, pdf} = HtmlToPdf.render(html, options)
      assert {:ok, "hyphenation"} = Text.extract(pdf, layout: false)
    end
  end

  test "page and spread limits reserve complete hyphenated word chains and refit chains taller than a page" do
    alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, Layout, Pagination, Style}

    dictionaries = %{
      "en" => %{exceptions: ["hy-phen-ation"], provenance: %{source: "Authored page word"}}
    }

    for {control, height, filler, expected} <- [
          {"none", 56, 28, ["hy‐phen‐", "ation"]},
          {"page", 56, 28, ["", "hy‐phen‐ation"]},
          {"column", 56, 28, ["", "hy‐phen‐ation"]},
          {"spread", 56, 28, ["", "hy‐phen‐ation"]},
          {"spread", 56, 84, ["", "hy‐phen‐", "ation"]},
          {"page", 56, 84, ["", "", "hy‐phen‐ation"]},
          {"page", 28, 0, ["hyphenation"]}
        ] do
      options = [
        system_font_discovery: false,
        hyphenation_dictionaries: dictionaries,
        page_size: {100, height},
        margin: 0
      ]

      html = """
      <body style="margin:0"><div style="height:#{filler}pt"></div>
      <p lang="en" style="margin:0;width:40pt;line-height:14pt;font-size:12pt;orphans:1;widows:1;
        hyphens:auto;hyphenate-limit-last:#{control}">hyphenation</p></body>
      """

      assert {:ok, dom} = HtmlParser.parse(html)
      assert {:ok, styled} = Style.compute_detailed(dom, options)
      assert {:ok, layout} = Layout.layout(styled, options)
      assert {:ok, pages} = Pagination.paginate(layout, options)

      visible =
        Enum.map(pages, fn page ->
          Enum.flat_map(page.boxes, fn box ->
            if box.type == :glyph_group, do: box.boxes, else: [box]
          end)
          |> Enum.filter(&(&1.type == :text))
          |> Enum.map_join(& &1.text)
        end)

      assert visible == expected, inspect({control, height, filler, visible})
    end
  end

  test "zone measures the preceding complete words rather than the already hyphenated line" do
    alias NativeElixirPdfUtilities.Validators.CssHyphenationValidator

    point = {make_ref(), 4}
    make_line = fn style -> %{point: point, style: style, natural_width: 30.0} end

    for {zone, rejected?} <- [{0.0, false}, {19.0, false}, {20.0, true}, {{:percent, 0.4}, true}] do
      line = make_line.(%{hyphenate_limit_zone: zone})
      rejected = CssHyphenationValidator.rejected_breaks([line, nil], 50.0)
      assert MapSet.member?(rejected, point) == rejected?
    end

    assert MapSet.size(
             CssHyphenationValidator.rejected_breaks(
               [
                 make_line.(%{hyphenate_limit_lines: 1}),
                 nil,
                 make_line.(%{hyphenate_limit_lines: 1})
               ],
               50.0
             )
           ) == 0

    for control <- [:none, :always, :column, :page, :spread],
        boundary <- [:column, :page],
        page <- [1, 2] do
      expected =
        control in [:always, :column] or
          (boundary == :page and (control == :page or (control == :spread and page == 1)))

      assert CssHyphenationValidator.forbids_fragment_end?(control, boundary, page) == expected
    end
  end
end
