defmodule NativeElixirPdfUtilities.HtmlToPdf.UnicodeGraphemeTest do
  use ExUnit.Case, async: true
  alias NativeElixirPdfUtilities.HtmlToPdf.UnicodeGrapheme
  alias NativeElixirPdfUtilities.TestSupport.Paths

  test "complete Unicode 17 extended grapheme corpus agrees at every UTF-8 boundary" do
    data =
      Paths.fixture("html_to_pdf/typography/grapheme_break_vectors.json")
      |> File.read!()
      |> :json.decode()

    assert data["unicode"] == "17.0.0"
    assert length(data["vectors"]) == 766

    for vector <- data["vectors"] do
      text =
        Enum.map_join(vector["points"], fn point -> <<String.to_integer(point, 16)::utf8>> end)

      clusters = UnicodeGrapheme.graphemes(text)

      {starts, total} =
        Enum.map_reduce(clusters, 0, fn cluster, offset ->
          {offset, offset + byte_size(cluster)}
        end)

      assert starts ++ [total] == vector["breaks"], "grapheme conformance line #{vector["line"]}"
      assert Enum.join(clusters) == text

      assert UnicodeGrapheme.next(text) ==
               {hd(clusters),
                binary_part(
                  text,
                  byte_size(hd(clusters)),
                  byte_size(text) - byte_size(hd(clusters))
                )}
    end
  end

  test "empty, ASCII CRLF and Unicode 17 Indic and emoji clusters share the same API" do
    assert UnicodeGrapheme.graphemes("") == []
    assert UnicodeGrapheme.next("") == nil
    assert UnicodeGrapheme.graphemes("A\r\nB") == ["A", "\r\n", "B"]
    assert UnicodeGrapheme.graphemes("क्‍क 🇳🇱👩🏽‍🚀") == ["क्‍क", " ", "🇳🇱", "👩🏽‍🚀"]
  end

  test "first-letter selection uses the same pinned typographic units and punctuation" do
    alias NativeElixirPdfUtilities.Validators.CssInitialLetterValidator
    assert CssInitialLetterValidator.first_letter("\"Á!B") == "\"Á!"
    assert CssInitialLetterValidator.first_letter("'क्‍क'Z") == "'क्‍क'"
    assert CssInitialLetterValidator.first_letter("'🇳🇱'Z") == "'🇳🇱'"
    assert CssInitialLetterValidator.first_letter("!?") == "!?"
    assert CssInitialLetterValidator.first_letter("") == ""
  end
end
