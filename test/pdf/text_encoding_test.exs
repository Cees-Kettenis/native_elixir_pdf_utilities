defmodule NativeElixirPdfUtilities.Pdf.TextEncodingTest do
  use ExUnit.Case, async: true

  alias NativeElixirPdfUtilities.Pdf.{GlyphName, TextEncoding}

  test "reports supported encodings and rejects invalid character requests" do
    assert TextEncoding.supported?("WinAnsiEncoding")
    refute TextEncoding.supported?("Unknown")
    refute TextEncoding.supported?(nil)

    assert TextEncoding.character("WinAnsiEncoding", 65, %{}) == {:ok, "A"}
    assert TextEncoding.character("Unknown", 65, %{}) == :error
    assert TextEncoding.character("WinAnsiEncoding", 300, %{}) == :error
    assert TextEncoding.character("StandardEncoding", 0, %{}) == :error
  end

  test "resolves Adobe glyph names and their algorithmic forms" do
    assert GlyphName.to_unicode("Aacute") == {:ok, "Á"}
    assert GlyphName.to_unicode("Aacute.alt") == {:ok, "Á"}
    assert GlyphName.to_unicode("A_B") == {:ok, "AB"}
    assert GlyphName.to_unicode("uni00410042") == {:ok, "AB"}
    assert GlyphName.to_unicode("u1F600") == {:ok, "😀"}
    assert TextEncoding.glyph("Aacute") == {:ok, "Á"}

    assert GlyphName.to_unicode("A_unknownGlyph") == :error
    assert GlyphName.to_unicode("uni") == :error
    assert GlyphName.to_unicode("uniD800") == :error
    assert GlyphName.to_unicode("uD800") == :error
    assert GlyphName.to_unicode("u110000") == :error
    assert GlyphName.to_unicode("unknownGlyph") == :error
    assert GlyphName.to_unicode("madeup") == :error
    assert GlyphName.to_unicode(123) == :error
    assert GlyphName.to_unicode(<<255>>) == :error
  end

  test "decodes valid Unicode glyph names at scalar and digit-width boundaries" do
    for {name, text} <- [
          {"u", "u"},
          {"u0000", <<0>>},
          {"u0042", "B"},
          {"u00042", "B"},
          {"u000042", "B"},
          {"uD7FF", <<0xD7FF::utf8>>},
          {"uE000", <<0xE000::utf8>>},
          {"u10FFFF", <<0x10FFFF::utf8>>},
          {"uni0000D7FFE000FFFF", <<0, 0xD7FF::utf8, 0xE000::utf8, 0xFFFF::utf8>>},
          {"uni20AC0308", "€\u0308"},
          {"Aacute_uni20AC0308_u1040C.alternate", "Á€\u0308𐐌"}
        ] do
      assert GlyphName.to_unicode(name) == {:ok, text}, name
    end
  end

  test "rejects Unicode-like custom names outside Adobe's mapping grammar" do
    for name <- [
          "uu0042",
          "uniuni0042",
          "u+0042",
          "u-0000",
          "u42",
          "u004",
          "u0000042",
          "u1f600",
          "u004G",
          "uu",
          "uni20ac",
          "uni+042",
          "uni-000",
          "uni0041004",
          "uni0041004G",
          "uniD801DC0C",
          "uni0041D800",
          "uDFFF",
          "u110000",
          "A_uu0042",
          "A_",
          ".notdef"
        ] do
      assert GlyphName.to_unicode(name) == :error, name
    end
  end
end
