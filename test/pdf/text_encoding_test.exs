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
end
