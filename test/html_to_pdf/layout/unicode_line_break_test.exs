defmodule NativeElixirPdfUtilities.HtmlToPdf.UnicodeLineBreakTest do
  use ExUnit.Case, async: true
  alias NativeElixirPdfUtilities.HtmlToPdf.{UnicodeLineBreak, UnicodeTextData}
  alias NativeElixirPdfUtilities.TestSupport.Paths

  test "pinned Unicode 17 vectors retain their independent rule and property expectations" do
    corpus =
      Paths.fixture("html_to_pdf/typography/line_break_vectors.json")
      |> File.read!()
      |> :json.decode()

    assert corpus["unicode"] == "17.0.0"
    assert length(corpus["vectors"]) > 100

    for row <- corpus["vectors"] do
      text = Enum.map_join(row["points"], &<<String.to_integer(&1, 16)::utf8>>)
      actual = UnicodeLineBreak.opportunities(text)
      assert Enum.map(actual, &elem(&1, 0)) == row["breaks"], "Unicode source line #{row["line"]}"
    end
  end

  test "hard breaks, empty text and the ordinary-word shortcut preserve byte offsets and mandatory flags" do
    assert UnicodeLineBreak.opportunities("") == [{0, true}]
    assert UnicodeLineBreak.opportunities(" A1  B2 ") == [{1, false}, {5, false}, {8, true}]

    assert UnicodeLineBreak.opportunities("a\r\nb\nc\u0085d\u2028e") ==
             [{3, true}, {5, true}, {8, true}, {12, true}, {13, true}]

    assert UnicodeLineBreak.opportunities("🇳🇱🇫🇷") == [{8, false}, {16, true}]
    assert UnicodeTextData.line_break(?A) == {:AL, false}
    assert UnicodeTextData.line_break(0x4E00) == {:ID, true}
    assert UnicodeTextData.line_break(0xF0000) == {:XX, false}
  end

  test "ASCII numeric labels retain generic Unicode boundaries around punctuation and hyphens" do
    # An appended AL scalar forces the general path. It cannot change any
    # earlier boundary in this subset; the original end remains mandatory.
    assert UnicodeTextData.line_break(0xE9) == {:AL, false}
    alphabet = ["A", "1", " ", "-", ".", ",", ":"]

    words =
      Enum.reduce(1..4, [""], fn _, words ->
        Enum.flat_map(words, fn word -> Enum.map(alphabet, &(word <> &1)) end)
      end)

    for word <- ["", "A-1", "-A", "A--B", "A .1", "12,345.67", "A: B" | words] do
      size = byte_size(word)

      reference =
        UnicodeLineBreak.opportunities(word <> "é")
        |> Enum.filter(fn {offset, _} -> offset < size end)
        |> Kernel.++([{size, true}])

      assert UnicodeLineBreak.opportunities(word) == reference, inspect(word)
    end
  end
end
