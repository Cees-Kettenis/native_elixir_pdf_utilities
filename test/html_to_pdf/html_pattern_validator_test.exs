defmodule NativeElixirPdfUtilities.Validators.HtmlPatternValidatorTest do
  use ExUnit.Case
  alias NativeElixirPdfUtilities.Limits
  alias NativeElixirPdfUtilities.Validators.{HtmlPatternValidator, HtmlUrlValidator}

  setup do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    :ok
  end

  test "Unicode-v intersections, subtraction, strings, properties and UTF-16 code points" do
    for {pattern, values, mismatch} <- [
          {"[[a-z]&&[^aeiou]]+", ["bcdf"], false},
          {"[[a-z]&&[^aeiou]]+", ["bade"], true},
          {"[\\p{ASCII}--[A-Z]]+", ["abc123"], false},
          {"[\\q{ab|cd}]+", ["abcdab"], false},
          {"[\\q{ab|cd}]+", ["abc"], true},
          {"\\p{Script=Greek}+", ["Ελληνικά"], false},
          {"\\p{RGI_Emoji}", ["👩‍👩‍👧‍👦"], false},
          {".", ["😀"], false},
          {".{2}", ["😀"], true},
          {"\\u{1F600}", ["😀"], false}
        ] do
      assert {:ok, %{compiled?: true, mismatch: ^mismatch}} =
               HtmlPatternValidator.initial_pattern(pattern, values)
    end
  end

  test "whole-value matching, backreferences, lookarounds and multiple values" do
    assert {:ok, %{compiled?: true, mismatch: false}} =
             HtmlPatternValidator.initial_pattern("(ab)\\1", ["abab"])

    assert {:ok, %{compiled?: true, mismatch: false}} =
             HtmlPatternValidator.initial_pattern("(?=abc)abc", ["abc"])

    assert {:ok, %{compiled?: true, mismatch: true}} =
             HtmlPatternValidator.initial_pattern("ab", ["abc", "ab"])

    assert {:ok, %{compiled?: true, mismatch: true}} =
             HtmlPatternValidator.initial_pattern("ab", ["zab"])

    assert {:ok, %{compiled?: true, mismatch: false}} =
             HtmlPatternValidator.initial_pattern("", [""])

    assert {:ok, %{compiled?: true, mismatch: true}} =
             HtmlPatternValidator.initial_pattern("", ["a"])
  end

  test "invalid authored v patterns are ignored before implicit anchors are applied" do
    for pattern <- [
          "[",
          ")|(",
          "[a-z|]",
          "[a&&b--c]",
          "\\p{NotAProperty}",
          "(?i)a",
          "[\\q{ab}--]"
        ] do
      assert {:ok, %{compiled?: false, mismatch: false}} =
               HtmlPatternValidator.initial_pattern(pattern, ["anything"])
    end
  end

  test "caller shape and Unicode failures return shared actionable diagnostics" do
    for {pattern, values} <- [{nil, ["a"]}, {"a", "a"}, {<<255>>, []}, {"a", [<<255>>]}] do
      assert {:error, {:invalid_document, diagnostic}} =
               HtmlPatternValidator.initial_pattern(pattern, values)

      assert diagnostic.stage == :html
      assert diagnostic.reason == :invalid_document
      assert diagnostic.operation == :initial_form_value
      assert diagnostic.message =~ "UTF-8"
    end
  end

  test "compiler fuel, allocation, recursion and native backtracking exhaust as diagnostics" do
    for {limit, ceiling, pattern, value} <- [
          {:max_html_pattern_compile_work, 8, "(?:abc|def){2}", "abcabc"},
          {:max_html_form_value_memory_bytes, 100, "(abc)+", "abc"},
          {:max_html_pattern_depth, 8,
           String.duplicate("(", 30) <> "a" <> String.duplicate(")", 30), "a"},
          {:max_html_pattern_match_work, 2_000, "(a+)+b", String.duplicate("a", 30)}
        ] do
      original = Limits.effective()
      Limits.install(Map.put(original, limit, ceiling))

      assert {:error, {:resource_limit_exceeded, diagnostic}} =
               HtmlPatternValidator.initial_pattern(pattern, [value])

      assert diagnostic.stage == :html
      assert diagnostic.operation == :initial_form_value
      assert diagnostic.message =~ Atom.to_string(limit)
      Limits.install(original)
    end

    assert {:ok, %{compiled?: true, mismatch: false}} =
             HtmlPatternValidator.initial_pattern("a+", ["aaa"])
  end

  test "Unicode string-property compilation charges its actual native allocations" do
    Limits.install(Map.put(Limits.effective(), :max_html_form_value_memory_bytes, 50_000))

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             HtmlPatternValidator.initial_pattern("\\p{RGI_Emoji}", ["👩‍👩‍👧‍👦"])

    assert diagnostic.message =~ "max_html_form_value_memory_bytes"
  end

  test "early native allocation exhaustion preserves the caller and the actual limit diagnostic" do
    Limits.install(Map.put(Limits.effective(), :max_html_form_value_memory_bytes, 100))
    flags = Process.info(self(), :trap_exit)
    marker = make_ref()
    send(self(), {:caller_message, marker})

    for _ <- 1..8 do
      assert {:error, {:resource_limit_exceeded, diagnostic}} =
               HtmlPatternValidator.initial_pattern("a+", [String.duplicate("a", 500_000)])

      assert diagnostic.stage == :html
      assert diagnostic.reason == :resource_limit_exceeded
      assert diagnostic.operation == :initial_form_value
      assert diagnostic.message =~ "max_html_form_value_memory_bytes"
    end

    assert Process.info(self(), :trap_exit) == flags
    assert_receive {:caller_message, ^marker}
    refute_receive {:EXIT, _port, _reason}
  end

  test "character scans consume interpreter fuel even without exponential backtracking" do
    Limits.install(Map.put(Limits.effective(), :max_html_pattern_match_work, 100))

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             HtmlPatternValidator.initial_pattern(".*", [String.duplicate("😀", 1000)])

    assert diagnostic.message =~ "max_html_pattern_match_work"
  end

  test "elapsed exhaustion terminates the helper and leaves later calls usable" do
    original = Limits.effective()

    Limits.install(
      Map.merge(original, %{
        max_html_form_value_timeout_ms: 0,
        max_html_pattern_match_work: 1_000_000_000
      })
    )

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             HtmlPatternValidator.initial_pattern("(a+)+b", [String.duplicate("a", 50)])

    assert diagnostic.message =~ "max_html_form_value_timeout_ms"
    Limits.install(original)

    assert {:ok, %{compiled?: true, mismatch: false}} =
             HtmlPatternValidator.initial_pattern("a", ["a"])
  end

  test "native transport failure preserves the shared diagnostic contract" do
    assert {:error, {:form_value_backend_unavailable, diagnostic}} =
             NativeElixirPdfUtilities.Validators.HtmlFormValueValidator.result({:exit, 80})

    assert diagnostic.stage == :html
    assert diagnostic.operation == :initial_form_value
    assert diagnostic.message =~ "Elixir"
  end

  test "input size, value count and cumulative render work have validator-owned limits" do
    original = Limits.effective()

    for {limit, ceiling, pattern, values} <- [
          {:max_html_pattern_bytes, 2, "abc", ["abc"]},
          {:max_html_form_value_bytes, 2, "a", ["aaaa"]},
          {:max_html_pattern_values, 1, "a", ["a", "a"]}
        ] do
      Limits.install(Map.put(original, limit, ceiling))

      assert {:error, {:resource_limit_exceeded, diagnostic}} =
               HtmlPatternValidator.initial_pattern(pattern, values)

      assert diagnostic.operation == :initial_form_value
      assert diagnostic.message =~ Atom.to_string(limit)
    end

    Limits.install(Map.put(original, :max_html_pattern_bytes, 3))

    assert {:error, {:resource_limit_exceeded, _}} =
             NativeElixirPdfUtilities.Validators.HtmlValidator.with_render_budget(fn ->
               assert {:ok, %{compiled?: true}} =
                        HtmlPatternValidator.initial_pattern("ab", ["ab"])

               HtmlPatternValidator.initial_pattern("ab", ["ab"])
             end)
  end

  test "WHATWG absolute URL inputs parse without base or resource authorization" do
    for value <- [
          "https://example.org/a?q=1#x",
          "mailto:user@example.org",
          "data:text/plain,hello",
          "custom:opaque",
          "http://127.1/",
          "http://0x7f000001/",
          "http://0177.0.0.1/",
          "http://4294967295/",
          "http://1.2.65535/",
          "http://127.0.0.1./",
          "http://[::1]/",
          "file:///tmp/a",
          "https://例え.テスト/"
        ] do
      assert {:ok, true} = HtmlUrlValidator.valid_initial_input_url?(value)
    end

    for value <- [
          "relative/path",
          "//example.org",
          "https://",
          "http://[::::]/",
          "http://example.org:99999/",
          "http://999.999.999.999/",
          "http://256.1.1.1/",
          "http://1.2.65536/",
          "http://4294967296/",
          "http://1.2.3.4.5/",
          "http://09/",
          "http://example.123/",
          "http://1..2/",
          "http://1.0x+1.2/",
          ""
        ] do
      assert {:ok, false} = HtmlUrlValidator.valid_initial_input_url?(value)
    end

    assert {:error, {:invalid_document, _}} = HtmlUrlValidator.valid_initial_input_url?(<<255>>)
  end
end
