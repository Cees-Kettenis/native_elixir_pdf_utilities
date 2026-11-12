defmodule NativeElixirPdfUtilities.Validators.NativeGlyphSourceValidatorTest do
  use ExUnit.Case, async: true
  alias NativeElixirPdfUtilities.Validators.NativeGlyphSourceValidator, as: Source

  test "trimming retains paragraph offsets even without a parallel scalar list" do
    run = %{text: "  ABC ", style: %{}, _paragraph_end_length: 6, _paragraph_offset: 10}
    assert %{text: "ABC", _paragraph_offset: 12} = Source.trim_run(run, "ABC")
    assert %{text: "", _paragraph_offset: 10} = Source.trim_run(run, "")
  end

  test "tabs preserve bidi and hyphenation owners while expanding their displayed scalars" do
    bidi = [%{codepoint: ?A, level: 0}, %{codepoint: 9, level: 2}, %{codepoint: ?B, level: 0}]
    hyphens = Enum.map(bidi, &Map.merge(&1, %{generated?: false, owner: :word}))
    run = %{text: "A\tB", style: %{}, bidi_characters: bidi, hyphenation_characters: hyphens}
    expanded = Source.expand_tabs(run, "A  B", [2])
    assert Enum.map(expanded.bidi_characters, & &1.codepoint) == [65, 32, 32, 66]
    assert Enum.map(expanded.bidi_characters, & &1.level) == [0, 2, 2, 0]
    assert Enum.all?(expanded.hyphenation_characters, &(&1.owner == :word and not &1.generated?))
  end

  test "discretionary display normalizes paragraph separators and inherits the preceding bidi level" do
    records = [
      %{displayed: "A", fragments: [%{level: 1, owner: :before}]},
      %{displayed: "\u2028", fragments: [%{level: 1, owner: :separator}]},
      %{displayed: "\u00AD", fragments: [%{level: 0, owner: :soft}]}
    ]

    selected = Source.display_characters(records, %{discretionary?: true, text: "A -"})
    assert Enum.map(selected, & &1.displayed) == ["A", " ", "-"]
    assert List.last(selected).fragments == [%{level: 1, owner: :soft}]

    only_soft = [%{displayed: "\u00AD", fragments: [%{level: 2, owner: :soft}]}]

    assert [%{displayed: "-", fragments: [%{level: 2}]}] =
             Source.display_characters(only_soft, %{discretionary?: true, text: "-"})
  end
end
