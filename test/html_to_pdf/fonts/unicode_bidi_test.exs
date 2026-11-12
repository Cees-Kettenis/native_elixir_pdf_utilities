defmodule NativeElixirPdfUtilities.HtmlToPdf.UnicodeBidiTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.HtmlToPdf.UnicodeBidi
  alias NativeElixirPdfUtilities.Limits
  alias NativeElixirPdfUtilities.TestSupport.Paths

  setup do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    :ok
  end

  test "compact official Unicode 17 cases retain independent levels and visual order" do
    fixture =
      Paths.project("test/fixtures/html_to_pdf/typography/bidi_vectors.json")
      |> File.read!()
      |> :json.decode()

    for vector <- fixture["vectors"] do
      text = vector["points"] |> Enum.map(&String.to_integer(&1, 16)) |> List.to_string()
      direction = %{0 => :ltr, 1 => :rtl, 2 => :auto}[vector["direction"]]
      assert {:ok, resolved} = UnicodeBidi.resolve(text, direction)

      assert hd(resolved.paragraphs).level == vector["base"],
             "Unicode source line #{vector["line"]}"

      assert resolved.order == vector["order"], "Unicode source line #{vector["line"]}"

      for {expected, actual} <- Enum.zip(vector["levels"], resolved.levels), expected != "x" do
        assert actual == String.to_integer(expected), "Unicode source line #{vector["line"]}"
      end
    end
  end

  test "paragraph separators independently resolve auto direction and trailing whitespace" do
    assert {:ok, resolved} = UnicodeBidi.resolve("אב  \nabc\n")
    assert Enum.map(resolved.paragraphs, & &1.level) == [1, 0]
    assert resolved.levels == [1, 1, 1, 1, 1, 0, 0, 0, 0]
    assert resolved.order == [4, 3, 2, 1, 0, 5, 6, 7, 8]
    assert {:ok, empty} = UnicodeBidi.resolve("")
    assert empty.order == [] and empty.paragraphs == []
    assert {:ok, removed} = UnicodeBidi.resolve("\u202A\u202C")
    assert removed.order == []
  end

  test "line fitting resets its own trailing spaces without changing paragraph resolution" do
    assert {:ok, resolved} = UnicodeBidi.resolve("א  a", :ltr)
    assert resolved.levels == [1, 0, 0, 0]
    assert {:ok, line} = UnicodeBidi.line(resolved, 0, 3)
    assert line.order == [0, 1, 2]
    assert {:ok, line} = UnicodeBidi.line(resolved, 1, 1)
    assert line.order == []

    for {first, last} <- [{-1, 2}, {0, 5}, {3, 2}, {false, 2}] do
      assert {:error, {:invalid_text, %{operation: :reorder_bidi_line}}} =
               UnicodeBidi.line(resolved, first, last)
    end
  end

  test "isolates, overrides and protocol overflow bounds remain balanced and bounded" do
    # Explicit embedding depth 125 is fixed by Unicode, not a tunable ceiling.
    for prefix <- ["\u202A", "\u202B", "\u202D", "\u202E", "\u2066", "\u2067", "\u2068"] do
      text = String.duplicate(prefix, 130) <> "אa" <> String.duplicate("\u202C\u2069", 130)
      assert {:ok, resolved} = UnicodeBidi.resolve(text)
      assert Enum.all?(resolved.levels, &(&1 in 0..126))
    end

    assert {:ok, isolate} = UnicodeBidi.resolve("\u2068\u2067א\u2069a\u2069b")
    assert hd(isolate.paragraphs).level == 0

    assert {:ok, brackets} =
             UnicodeBidi.resolve(String.duplicate("(", 64) <> "אב" <> String.duplicate(")", 64))

    assert length(brackets.order) == 130
  end

  test "embedding controls within an overflowed isolate cannot change the active embedding level" do
    # UAX9 X2 and X5a bound explicit levels at 125. Even LRI levels reach
    # 124; additional isolate and embedding initiators remain inert until PDI.
    text = String.duplicate("\u2066", 64) <> "\u202AA\u202C" <> String.duplicate("\u2069", 64)
    assert {:ok, resolved} = UnicodeBidi.resolve(text, :ltr)
    [letter] = Enum.filter(resolved.records, &(&1.codepoint == ?A))
    assert letter.level == 124
    assert Enum.all?(resolved.levels, &(&1 in 0..126))
  end

  test "invalid direction, text and depleted work return actionable diagnostics" do
    assert {:error, {:invalid_text, %{operation: :resolve_bidi}}} = UnicodeBidi.resolve("a", :bad)
    assert {:error, {:invalid_text, %{stage: :font}}} = UnicodeBidi.resolve(<<255>>)
    Limits.install(%{Limits.effective() | max_work: 100})

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             UnicodeBidi.resolve(String.duplicate("א", 30))

    assert diagnostic.message =~ "max_work"
  end
end
