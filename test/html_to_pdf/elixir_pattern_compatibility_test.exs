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
          {"[\\u0061-\\u0063]+", "abc", false}
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
          "\\p{NoSuchProperty}"
        ] do
      assert {:ok, %{compiled?: false, mismatch: false}} =
               HtmlPatternValidator.initial_pattern(source, ["a"]),
             source
    end
  end
end
