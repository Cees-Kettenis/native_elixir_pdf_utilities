defmodule NativeElixirPdfUtilities.HtmlToPdf.ElixirPatternCompatibilityTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.Validators.HtmlPatternValidator

  test "Unicode-v sets retain character and finite-string set semantics" do
    for {source, value, mismatch} <- [
          {"[a-cx-z]+", "abz", false},
          {"[^a-c]+", "xyz", false},
          {"[^a-c]+", "ax", true},
          {"[]", "a", true},
          {"[^]", "\n", false},
          {"[ ]", " ", false},
          {"[\\q{ab|cd}&&\\q{cd|ef}]", "cd", false},
          {"[\\q{ab|cd}--\\q{cd}]", "ab", false},
          {"[\\q{ab|cd}--\\q{cd}]", "cd", true},
          {"[\\q{|a}]", "", false},
          {"[\\p{ASCII}--[A-Z]]+", "abc123", false},
          {"\\p{Basic_Emoji}", "😀", false},
          {"\\p{RGI_Emoji_Flag_Sequence}", "🇲🇾", false},
          {"\\p{Emoji_Keycap_Sequence}", "1️⃣", false},
          {"[\\u0061-\\u0063]+", "abc", false},
          {~S"[\u{61}-\u{7a}]+", "abc", false},
          {~S"[\uD83D\uDE00-\uD83D\uDE01]", "😀", false},
          {~S"[\uD800\u0041-\uDFFF]", "a", false},
          {~S"[\x41-\x5a]+", "ABC", false},
          {~S"[\cA-\cC]", "\u0002", false},
          {~S"[\t-\r]", "\n", false},
          {~S"[\!-\#]", "!", false},
          {~S"\u{D800}", "a", true},
          {~S"[\uD800-\uDFFF]", "a", true}
        ] do
      assert {:ok, %{compiled?: true, mismatch: ^mismatch}} =
               HtmlPatternValidator.initial_pattern(source, [value]),
             source
    end
  end

  test "whole-value matching preserves captures, assertions and Unicode escapes" do
    for {source, value, mismatch} <- [
          {"(?<pair>ab)\\k<pair>", "abab", false},
          {"(a|(b))\\2c", "ac", false},
          {"(?:ab){2,3}?", "ababab", false},
          {"a{2,}", "aaa", false},
          {"(?<=a)b", "b", true},
          {"(?!b)a", "a", false},
          {"a(?<!b)", "a", false},
          {"a?b+c*", "bbcc", false},
          {"^a$", "a", false},
          {"a(?=$)\\n", "a\n", true},
          {"\\uD83D\\uDE00", "😀", false},
          {"\\uD800", "a", true},
          {"\\x41\\cJ", "A\n", false},
          {"\\0", "\0", false},
          {"\\d\\w\\s", "3a\u00A0", false},
          {"\\D\\W\\S", "漢😀a", false},
          {"\\w+", "漢", true},
          {"\\bword\\b", "word", false},
          {".", "\u2028", true},
          {"\\.", ".", false}
        ] do
      assert {:ok, %{compiled?: true, mismatch: ^mismatch}} =
               HtmlPatternValidator.initial_pattern(source, [value]),
             source
    end
  end

  test "exact Unicode properties and aliases include vocabulary independent of OTP" do
    for {source, value, mismatch} <- [
          {~S"\p{Script=Greek}+", "αβ", false},
          {~S"\p{sc=Grek}+", "αβ", false},
          {~S"\p{Script_Extensions=Hiragana}", "ー", false},
          {~S"\p{scx=Hira}", "ー", false},
          {~S"\p{General_Category=Decimal_Number}+", "١2", false},
          {~S"\p{gc=Nd}+", "١2", false},
          {~S"\p{ASCII_Hex_Digit}+", "01aF", false},
          {~S"\p{AHex}+", "g", true},
          {~S"\p{Extended_Pictographic}", "😀", false},
          {~S"\p{Changes_When_NFKC_Casefolded}", "Ａ", false},
          {~S"\P{ASCII}+", "漢😀", false},
          {~S"[\p{Letter}&&\p{Script=Greek}]+", "αβ", false},
          {~S"\p{Any}", "😀", false},
          {~S"\p{Surrogate}", "a", true},
          {~S"\P{Surrogate}", "a", false},
          {~S"\p{Assigned}", "\u0378", true}
        ] do
      assert {:ok, %{compiled?: true, mismatch: ^mismatch}} =
               HtmlPatternValidator.initial_pattern(source, [value]),
             source
    end

    for source <- [
          ~S"\p{Greek}",
          ~S"\p{script=Greek}",
          ~S"\p{Script=greek}",
          ~S"\p{gc=decimal_number}",
          ~S"\p{ASCII=true}",
          ~S"\p{Age=17.0}",
          ~S"\p{Block=Basic_Latin}",
          ~S"\p{NFKC_Quick_Check=Yes}",
          ~S"\p{Basic_emoji}"
        ] do
      assert {:ok, %{compiled?: false, mismatch: false}} =
               HtmlPatternValidator.initial_pattern(source, ["α"]),
             source
    end
  end

  test "variable backwards assertions preserve capture direction, alternatives and reset semantics" do
    for {source, value, mismatch} <- [
          {~S"a+(?<=a+)b", "aab", false},
          {~S"[\uD800\u0061-z](?<=a)", "a", false},
          {~S"(?:a|b)(?<=a)(?!c)b", "ab", false},
          {~S"a{2}(?<=a{2,})b", "aab", false},
          {~S"a*?ab(?<=ab)", "aaab", false},
          {~S"(?:(?=(a))a)+(?<=a)\1", "aa", false},
          {~S"漢(?<=\W)\B", "漢", false},
          {~S"a+(?<!b+)b", "aab", false},
          {~S"a+(?<!a+)b", "aab", true},
          {~S"abc(?<=([ab]+)([bc]+))\1-\2", "abca-bc", false},
          {~S"abc(?<=([ab]+)([bc]+))\1-\2", "abcab-c", true},
          {~S"aa(?<=\1(a))", "aa", false},
          {~S"ba(?<=\1(a))", "ba", true},
          {~S"ab(?<=(a|ab))\1", "abab", false},
          {~S"a+(?<=a+?)b", "aab", false},
          {~S"(?<pair>a+)(?<=\k<pair>)b", "aab", false},
          {~S"(a|(b))+\2", "aba", false},
          {~S"😀(?<=\p{Extended_Pictographic})a", "😀a", false},
          {~S"a(?<=\b\w)\b", "a", false},
          {~S"-(?<=\W)-", "--", false},
          {~S"a(?<=^a)$", "a", false},
          {~S"a(?<!^b)$", "a", false},
          {~S"ab(?<=ab|cd)(?=c)c", "abc", false},
          {~S"a(?<=[\q{a|ba}])b", "ab", false},
          {~S"a(?<=a{1,2})b", "ab", false},
          {~S"a(?<=(a?)*)", "a", false}
        ] do
      assert {:ok, %{compiled?: true, mismatch: ^mismatch}} =
               HtmlPatternValidator.initial_pattern(source, [value]),
             inspect({source, value})
    end
  end

  test "invalid ECMAScript Unicode-v syntax cannot acquire OTP-specific meanings" do
    for source <- [
          "a++",
          "a{z}",
          "a{2,1}",
          "a{2",
          "a}",
          "*a",
          "(abc",
          "(?i)a",
          "(*UTF)a",
          "[!!]",
          "[|]",
          "[-]",
          "[a&&b--c]",
          "[\\q{ab}--]",
          "\\j",
          "\\u{110000}",
          "\\P{RGI_Emoji}",
          "\\p{NoSuchProperty}",
          "[z-a]",
          "[a-\\p{ASCII}]",
          "[\\d-a]",
          "\\b+",
          "[\\B]",
          "\\u{+1}",
          "\\x+1",
          "\\2",
          "\\k<missing>"
        ] do
      assert {:ok, %{compiled?: false, mismatch: false}} =
               HtmlPatternValidator.initial_pattern(source, ["a"]),
             source
    end
  end

  test "nested syntax admission restores caller context on invalid input and second-pass exhaustion" do
    alias NativeElixirPdfUtilities.Validators.HtmlPatternSyntaxValidator
    limits = NativeElixirPdfUtilities.Limits.effective()
    key = {HtmlPatternSyntaxValidator, :syntax}
    Process.put(key, :enclosing_context)
    assert :invalid_pattern = HtmlPatternSyntaxValidator.prepare("\\1", limits)
    assert Process.get(key) == :enclosing_context
    source = "(a)+"
    assert {:ok, _, work} = HtmlPatternSyntaxValidator.prepare(source, limits)

    assert {:limit, :max_html_pattern_compile_work} =
             HtmlPatternSyntaxValidator.prepare_execution(source, %{
               limits
               | max_html_pattern_compile_work: work
             })

    assert Process.get(key) == :enclosing_context
  end

  test "valid native character expressions exceeding a fixed OTP bound retain diagnostics" do
    source = "[" <> String.duplicate("a", 20_000) <> "](?<=a)"

    assert {:error, {:unsupported_html, diagnostic}} =
             HtmlPatternValidator.initial_pattern(source, ["a"])

    assert diagnostic.stage == :html and diagnostic.operation == :initial_form_value
    assert diagnostic.message =~ "fixed engine bound"
  end
end
