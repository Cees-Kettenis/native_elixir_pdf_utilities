defmodule NativeElixirPdfUtilities.HtmlToPdf.CssParserBoundariesTest do
  use ExUnit.Case, async: false

  alias NativeElixirPdfUtilities.Limits
  alias NativeElixirPdfUtilities.HtmlToPdf.{CssParser, HtmlParser, Style}

  alias NativeElixirPdfUtilities.Validators.{
    CssCalculationValidator,
    CssColorValidator,
    HtmlValidator
  }

  setup do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    :ok
  end

  test "comments cannot create property, identifier, dimension, hash or function tokens" do
    for declaration <- ["co/**/lor:red", "--cu/**/stom:1pt"] do
      for parse <- [
            &CssParser.parse_declarations_detailed/1,
            &CssParser.prepare_source_stylesheet("p{" <> &1 <> "}")
          ] do
        assert {:error, {:invalid_css, detail}} = parse.(declaration)
        assert detail.stage == :css and detail.reason == :invalid_css
        assert detail.source == declaration
      end
    end

    for value <- ["1/**/0pt", "1/**/pt", "min/**/(1pt,2pt)", "mi/**/n(1pt,2pt)"] do
      html = "<p style='width:#{value}'>Text</p>"
      assert {:ok, dom} = HtmlParser.parse_detailed(html)
      assert {:error, {:invalid_css, detail}} = Style.compute_detailed(dom)
      assert detail.source =~ "width:"
    end

    for value <- ["r/**/ed", "#/**/123", "//**/*garbage*/red"] do
      assert :error = CssColorValidator.parse_function_color(value, nil)
    end

    assert {:ok, document} = HtmlParser.parse_document_detailed("<p>Text</p>")

    for prepare <- [&CssParser.prepare_stylesheet/1, &CssParser.prepare_source_stylesheet/1] do
      assert {:ok, prepared} = prepare.("p{color://**/*garbage*/red}")
      assert hd(prepared.rules).declarations != [{"color", "red"}]
    end

    assert {:error, {:invalid_css, _}} =
             Style.with_source_style_contexts(
               document,
               ["p{color://**/*garbage*/red}"],
               HtmlValidator.new_css_budget(),
               fn _, _, _ -> :ok end
             )

    for selector <- ["p/**/a", ".a/**/b", "#/**/a", "p:nth-child/**/(1)", ~S|p\31 /**/b|] do
      assert {:error, {:invalid_css, _}} = CssParser.prepare_stylesheet(selector <> "{color:red}")

      assert {:error, {:invalid_css, _}} =
               CssParser.prepare_source_stylesheet(selector <> "{color:red}")
    end
  end

  test "comments separate values without adding descendant or math whitespace" do
    css = "/* é */ p/**/.a/**/:first-child{margin:1pt/**/2pt;color:rgb(1/**/2/**/3)}"
    assert {:ok, ordinary} = CssParser.prepare_stylesheet(css)
    assert {:ok, authored} = CssParser.prepare_source_stylesheet(css)
    assert [rule] = ordinary.rules
    assert [%{parts: [%{tag: "p", classes: ["a"], combinator: nil}]}] = rule.selectors
    assert [{"margin", "1pt 2pt"}, {"color", "rgb(1 2 3)"}] = rule.declarations

    assert Enum.map(authored.rules, &Map.take(&1, [:selectors, :declarations, :order])) ==
             ordinary.rules

    for record <- hd(authored.rules).source_declarations do
      source = record.source
      assert binary_part(css, source.offset, source.length) == source.text
    end

    for selector <- ["./**/a", ":/**/first-child", "p/**/ >/**/ .a"] do
      assert {:ok, _} = CssParser.prepare_stylesheet(selector <> "{color:red}")
    end

    for value <- [
          "calc(1pt +/**/1pt)",
          "calc(1pt/**/+ 1pt)",
          "calc(2pt/**/- 1pt)",
          "calc(2pt/* ordinary */- 1pt)",
          "calc(2pt/**/-/**/ 1pt)",
          "calc(1/**/pt + 1pt)"
        ] do
      assert :error = CssCalculationValidator.prepare(value, %{"pt" => 1})
      assert {:ok, dom} = HtmlParser.parse_detailed("<p style='width:#{value}'>Text</p>")
      assert {:error, {:invalid_css, _}} = Style.compute_detailed(dom)
      assert {:ok, document} = HtmlParser.parse_document_detailed("<p>Text</p>")

      for selector <- ["p", ".absent"] do
        assert {:error, {:invalid_css, detail}} =
                 Style.with_source_style_contexts(
                   document,
                   [selector <> "{width:#{value}}"],
                   HtmlValidator.new_css_budget(),
                   fn _, _, _ -> :ok end
                 )

        assert detail.source == "width:#{value}"
      end
    end

    for value <- ["calc(1pt +/**/ 1pt)", "calc(1pt/**/ + 1pt)"] do
      assert {:ok, expression} = CssCalculationValidator.prepare(value, %{"pt" => 1})
      assert {:ok, 2.0} = CssCalculationValidator.resolve(expression, 0)
    end
  end

  test "strings, escaped comment markers and adjacent comments retain lexical boundaries" do
    assert HtmlValidator.normalize_css_comments(~S|"/* ", '/* */', \/*x*/, 1/**//**/pt|) ==
             ~S|"/* ", '/* */', \/*x*/, 1/**/pt|

    assert HtmlValidator.normalize_css_comments("/* first */color/**/:red/* last */", :value) ==
             "color:red"

    assert HtmlValidator.normalize_css_comments("/* only */") == ""

    for url <- [
          "url(icons/a/*b*/x.svg)",
          ~S|u\72l(icons/a/*b*/x.svg)|,
          ~S|\u\r\l(icons/a/*b*/x.svg)|,
          "url(/* gap */'x')"
        ] do
      assert HtmlValidator.normalize_css_comments(url) == url
    end

    assert HtmlValidator.normalize_css_comments("width:1/* unfinished") == "width:1/* unfinished"
    unfinished = String.duplicate("/*x", 2_000)
    assert HtmlValidator.normalize_css_comments(unfinished) == unfinished
    assert HtmlValidator.normalize_css_comments("x/**/" <> unfinished) == "x" <> unfinished

    assert CssParser.strip_comments(~S|"/* string */"/* gap */red|, " ") == ~S|"/* string */" red|
    assert CssParser.strip_comments(~S|a\b/**/c|, " ") == ~S|a\b c|
    assert CssParser.strip_comments("x/* unfinished", " ") == "x/* unfinished"

    assert {:error, {:invalid_css, _}} =
             CssParser.parse_detailed("p{} @media screen{p{color:red}} broken")

    assert {:ok, [{"content", ~S|"/* "|}]} =
             CssParser.parse_declarations_detailed(~S|content:"/* "|)

    assert HtmlValidator.split_css_comma(~S|"a,b",calc(1pt + 2pt),'x(y)'|) ==
             [~S|"a,b"|, "calc(1pt + 2pt)", "'x(y)'"]

    assert HtmlValidator.split_css_comma("1pt,,2pt,", true) == ["1pt", "", "2pt", ""]
  end

  test "font fallback candidates keep their original comment spans" do
    css = "/* original */@font-face{font-family:X;src:local/**/(X),url(x.ttf)}"
    assert {:ok, prepared} = CssParser.prepare_source_stylesheet(css)
    assert [font] = prepared.authored_fonts
    assert [local, url] = font.candidates
    assert local.source.text == "local/**/(X)" and local.supported == false
    assert url.source.text == "url(x.ttf)" and url.supported == true
    assert [reference] = url.source_references
    assert reference.source.text == "url(x.ttf)"

    for source <- [local.source, url.source, reference.source] do
      assert binary_part(css, source.offset, source.length) == source.text
    end
  end

  test "ordinary and authored failures name original comment bytes and locations" do
    css = "/* before */\np { co/* authored */lor:red }"

    for parse <- [&CssParser.parse_detailed/1, &CssParser.prepare_source_stylesheet/1] do
      assert {:error, {:invalid_css, detail}} = parse.(css)
      assert detail.source == "co/* authored */lor:red"
      assert detail.line == 2 and detail.column == 5

      assert detail.message ==
               ~s(line 2: declaration "co/* authored */lor:red" is invalid or unsupported)
    end
  end

  test "collection order and bounded growth survive large declarations selectors parts and values" do
    for count <- [200, 800] do
      declarations = Enum.map_join(1..count, ";", &"--item#{&1}:#{&1}")
      assert {:ok, parsed} = CssParser.parse_declarations_detailed(declarations)
      assert length(parsed) == count
      assert hd(parsed) == {"--item1", "1"}
      assert List.last(parsed) == {"--item#{count}", "#{count}"}
      selectors = Enum.map_join(1..count, ",", &".item#{&1}")
      assert {:ok, %{rules: [rule]}} = CssParser.prepare_stylesheet(selectors <> "{color:red}")
      assert length(rule.selectors) == count
      assert hd(hd(rule.selectors).parts).classes == ["item1"]
      assert hd(List.last(rule.selectors).parts).classes == ["item#{count}"]
      parts = Enum.map_join(1..count, " > ", &"p.item#{&1}")
      assert {:ok, %{rules: [rule]}} = CssParser.prepare_stylesheet(parts <> "{color:red}")
      assert [selector] = rule.selectors
      assert length(selector.parts) == count
      assert hd(selector.parts).combinator == nil
      assert List.last(selector.parts).classes == ["item#{count}"]
      values = Enum.map_join(1..count, " ", &"#{&1}pt")
      assert length(HtmlValidator.split_css_tokens(values)) == count

      assert {:ok, {:min, sizes}} =
               HtmlValidator.parse_css_size("min(" <> String.replace(values, " ", ",") <> ")")

      assert length(sizes) == count
      assert hd(sizes) == 1.0 and List.last(sizes) == count * 1.0
    end

    assert {:ok, %{rules: [rule]}} =
             CssParser.prepare_stylesheet(
               "p.a.b[x][y]:first-child:last-child:not(.c):not(.d)::before{color:red}"
             )

    [part] = hd(rule.selectors).parts
    assert part.classes == ["a", "b"]
    assert part.attributes == [{:present, "x"}, {:present, "y"}]
    assert Enum.map(part.negations, & &1.classes) == [["c"], ["d"]]
  end

  test "declaration and selector collection work grows with source size" do
    for parse <- [
          fn n ->
            CssParser.parse_declarations_detailed(Enum.map_join(1..n, ";", &"--x#{&1}:#{&1}"))
          end,
          fn n ->
            CssParser.prepare_stylesheet(Enum.map_join(1..n, ",", &".x#{&1}") <> "{color:red}")
          end,
          fn n -> HtmlValidator.split_css_comma(Enum.map_join(1..n, ",", &"#{&1}pt")) end
        ] do
      # Warm module loading before measuring the process's own work.
      parse.(10)

      [small, large] =
        for n <- [500, 2_000] do
          {:reductions, before_count} = Process.info(self(), :reductions)
          result = parse.(n)
          assert match?({:ok, _}, result) or is_list(result)
          {:reductions, after_count} = Process.info(self(), :reductions)
          after_count - before_count
        end

      assert large < small * 6
    end
  end

  test "min depth is checked before recursion through applied and authored declarations" do
    Limits.install(Map.put(Limits.effective(), :max_css_calculation_depth, 4))

    assert {:ok, {:min, _}} =
             HtmlValidator.parse_css_size(
               String.duplicate("min(", 4) <> "1pt" <> String.duplicate(")", 4)
             )

    nested = String.duplicate("min(", 100) <> "1pt" <> String.duplicate(")", 100)

    assert {:error, {:resource_limit_exceeded, detail}} =
             HtmlValidator.with_render_budget(fn -> HtmlValidator.parse_css_size(nested) end)

    assert detail.message =~ "max_css_calculation_depth"

    for selector <- ["p", ".absent"], media <- ["print", "screen"] do
      css = "@media #{media}{#{selector}{width:#{nested}}}"
      assert {:ok, dom} = HtmlParser.parse_document_detailed("<p>Text</p>")

      assert {:error, {:resource_limit_exceeded, detail}} =
               Style.with_source_style_contexts(dom, [css], HtmlValidator.new_css_budget(), fn _,
                                                                                               _,
                                                                                               _ ->
                 :ok
               end)

      assert detail.message =~ "max_css_calculation_depth"
      assert detail.stage == :style and detail.reason == :resource_limit_exceeded
    end
  end

  test "nested minmax is rejected as a leaf while ordinary tracks remain supported" do
    nested = String.duplicate("minmax(1pt,", 100) <> "1fr" <> String.duplicate(")", 100)

    for property <- [
          "grid-template-columns",
          "grid-template-rows",
          "grid-auto-columns",
          "grid-auto-rows"
        ],
        selector <- ["p", ".absent"] do
      css = selector <> "{#{property}:#{nested}}"
      assert {:ok, dom} = HtmlParser.parse_document_detailed("<p>Text</p>")

      assert {:error, {:invalid_css, detail}} =
               Style.with_source_style_contexts(dom, [css], HtmlValidator.new_css_budget(), fn _,
                                                                                               _,
                                                                                               _ ->
                 :ok
               end)

      assert detail.source == "#{property}:#{nested}"
    end

    for value <- ["minmax(1pt,,2fr)", "minmax(1pt,)", "minmax(,2fr)"] do
      assert {:ok, dom} =
               HtmlParser.parse_detailed("<p style='grid-template-columns:#{value}'>Text</p>")

      assert {:error, {:invalid_css, _}} = Style.compute_detailed(dom)
    end

    assert {:ok, dom} =
             HtmlParser.parse_detailed(
               "<p style='grid-template-columns:minmax(1pt,2fr)'>Text</p>"
             )

    assert {:ok, _} = Style.compute_detailed(dom)
  end
end
