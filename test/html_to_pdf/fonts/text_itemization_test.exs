defmodule NativeElixirPdfUtilities.HtmlToPdf.TextItemizationTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.HtmlToPdf.UnicodeTextData
  alias NativeElixirPdfUtilities.Validators.TextItemizationValidator, as: Itemization
  alias NativeElixirPdfUtilities.Limits

  setup do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    :ok
  end

  test "combining marks and supplementary emoji retain scalar owners across inline pieces" do
    pieces = [
      %{text: "a", owner: :base},
      %{text: "\u0301", owner: :accent},
      %{text: "👩", owner: :first_emoji},
      %{text: "\u200D💻", owner: :second_emoji},
      %{text: "𠀀", owner: :han}
    ]

    assert {:ok, plan} = Itemization.prepare(pieces)
    assert plan.text == "á👩‍💻𠀀"
    assert Enum.map(plan.clusters, & &1.text) == ["á", "👩‍💻", "𠀀"]
    assert Enum.map(plan.clusters, &{&1.start, &1.end}) == [{0, 3}, {3, 14}, {14, 18}]
    assert Enum.map(plan.clusters, &{&1.scalar_start, &1.scalar_end}) == [{0, 2}, {2, 5}, {5, 6}]
    assert Enum.map(hd(plan.clusters).fragments, & &1.owner) == [:base, :accent]

    assert Enum.map(Enum.at(plan.clusters, 1).fragments, & &1.owner) == [
             :first_emoji,
             :second_emoji,
             :second_emoji
           ]

    assert List.last(plan.clusters).script == "Hani"
    assert List.last(plan.clusters).fragments == [%{owner: :han, offset: 0, text: "𠀀"}]
  end

  test "script extensions and languages produce contiguous runs without splitting a grapheme" do
    assert {:ok, plan} =
             Itemization.prepare([
               %{text: "́Б", language: "sr-Cyrl"},
               %{text: "بِ", language: "ar"},
               %{text: "कि", language: "hi"},
               %{text: "A", language: "en"}
             ])

    assert Enum.map(plan.runs, &{&1.script, &1.language}) == [
             {"Cyrl", "sr-cyrl"},
             {"Arab", "ar"},
             {"Deva", "hi"},
             {"Latn", "en"}
           ]

    assert Enum.map(plan.clusters, & &1.script) == ["Cyrl", "Cyrl", "Arab", "Deva", "Latn"]
    assert List.last(plan.runs).end == byte_size(plan.text)
    assert {:ok, %{clusters: [], runs: [], text: ""}} = Itemization.prepare("")
    assert {:ok, %{runs: [%{script: "Zyyy"}]}} = Itemization.prepare("🙂")
    assert {:ok, %{runs: [%{script: "Arab"}]}} = Itemization.prepare("بـ")
  end

  test "transformed scalar fragments and variation selectors preserve extraction ownership" do
    characters = [
      %{displayed: "A", fragments: [%{owner: :original, offset: 0, text: "a"}]},
      %{displayed: "\uFE0F", fragments: [%{scalar_owner: :field, offset: 1, text: "\uFE0F"}]}
    ]

    assert {:ok, %{clusters: [cluster]}} = Itemization.prepare_characters(characters, "en")
    assert cluster.text == "A️"
    assert cluster.fragments == Enum.flat_map(characters, & &1.fragments)

    assert Enum.map(cluster.scalars, & &1.source) ==
             Enum.map(characters, &Map.put(&1, :language, "en"))
  end

  test "display projections admit exact scalars and bidi mirrors while rejecting missing or unrelated source text" do
    characters = [%{displayed: ")", fragments: [%{owner: :punctuation, text: "(", offset: 0}]}]
    assert {:ok, mirrored} = Itemization.prepare_projection("(", characters, "he")
    assert mirrored.text == ")"
    assert hd(mirrored.clusters).fragments == hd(characters).fragments
    assert {:ok, _} = Itemization.prepare_projection(")", characters, "he")
    assert {:ok, %{text: ""}} = Itemization.prepare_projection("", [], "und")

    for {source, selected} <- [
          {"A", characters},
          {"", characters},
          {"((", characters},
          {"(", []},
          {<<255>>, characters},
          {nil, characters},
          {[%{text: "("}], characters},
          {"(", [%{displayed: "bad", fragments: []}]}
        ] do
      assert {:error, {:invalid_text, diagnostic}} =
               Itemization.prepare_projection(source, selected, "und")

      assert diagnostic.stage == :font
      assert is_binary(diagnostic.message)
    end
  end

  test "pinned properties include joining, Indic position, RTL defaults and paired brackets" do
    assert {"Latn", :L, :Lu, 0, :U, :Other, :NA, :R} = UnicodeTextData.properties(?A)
    assert {"Arab", :AL, :Lo, 0, :D, _, _, _} = UnicodeTextData.properties(0x628)

    assert {"Deva", :L, :Mc, 0, :U, :Vowel_Dependent, :Left, _} =
             UnicodeTextData.properties(0x93F)

    assert {"Zzzz", :R, :Cn, _, _, _, _, _} = UnicodeTextData.properties(0x590)
    assert {"Zzzz", :BN, :Cn, _, _, _, _, _} = UnicodeTextData.properties(0x10FFFF)
    assert "Latn" in Tuple.to_list(UnicodeTextData.scripts(0x301))
    assert {"Latn"} = UnicodeTextData.scripts(?A)
    assert UnicodeTextData.mirror(?() == ?)
    assert UnicodeTextData.mirror(?A) == ?A
    assert UnicodeTextData.bracket(?() == {?), :open}
    assert UnicodeTextData.bracket(?)) == {?(, :close}
    assert UnicodeTextData.bracket(?A) == nil
  end

  test "invalid UTF-8, pieces, scalar fragments and language produce shared diagnostics" do
    for value <- [nil, 123, [nil], [%{text: nil}], <<0xED, 0xA0, 0x80>>, [%{text: <<255>>}]] do
      assert {:error, {:invalid_text, diagnostic}} = Itemization.prepare(value)
      assert diagnostic.stage == :font
      assert diagnostic.operation == :itemize_text
      assert diagnostic.module == Itemization
      assert is_binary(diagnostic.message)
    end

    for value <- [nil, [%{displayed: "ab", fragments: []}], [%{displayed: "a", fragments: nil}]] do
      assert {:error, {:invalid_text, %{operation: :itemize_text}}} =
               Itemization.prepare_characters(value)
    end

    for fragment <- [
          nil,
          :owner,
          %{},
          %{text: "ab", offset: 0, owner: :source},
          %{text: "a", offset: -1, owner: :source},
          %{text: "a", offset: 0}
        ] do
      assert {:error, {:invalid_text, %{operation: :itemize_text, message: message}}} =
               Itemization.prepare_characters([%{displayed: "a", fragments: [fragment]}])

      assert message =~ "source fragments"
    end

    assert {:error, {:invalid_document, _}} = Itemization.prepare([%{text: "a", language: false}])

    for language <- [nil, 1, []] do
      assert {:error, {:invalid_document, %{operation: :prepare_language_identifier}}} =
               Itemization.prepare("a", language)

      assert {:error, {:invalid_document, %{operation: :prepare_language_identifier}}} =
               Itemization.prepare_characters([], language)
    end

    assert {:error, {:invalid_encoding, _}} = Itemization.prepare("a", <<255>>)

    assert {:ok, %{runs: [%{language: "not a tag"}]}} =
             Itemization.prepare([%{text: "a", language: "not a tag"}])

    assert {:ok, %{runs: [%{language: ""}]}} = Itemization.prepare("a", "")
  end

  test "input and cluster allocation charge the shared shaping allowances" do
    original = Limits.effective()

    for {limit, value} <- [{:max_work, 1}, {:max_work, 1}] do
      Limits.install(Map.put(original, limit, value))
      assert {:error, {:resource_limit_exceeded, diagnostic}} = Itemization.prepare("abc")
      assert diagnostic.stage == :font
      assert diagnostic.message =~ Atom.to_string(limit)
    end
  end
end
