Code.require_file(NativeElixirPdfUtilities.TestSupport.Paths.support("open_type_fixture.ex"))

defmodule NativeElixirPdfUtilities.HtmlToPdf.SyriacShapingTest do
  use ExUnit.Case, async: true
  alias NativeElixirPdfUtilities.HtmlToPdf.{Font, TextShaper}
  alias NativeElixirPdfUtilities.TestSupport.OpenTypeFixture, as: Fixture

  test "Syriac contextual forms and transparent marks match an independent HarfBuzz oracle" do
    # System libharfbuzz shaped these test-owned bytes independently. Glyphs
    # are in logical order; distinct advances expose wrong form selection.
    # Font SHA256: 4832d2205d163074860d8e93fbbbf1173e49166a822b4e0f9ca6799969943426
    data = Fixture.syriac_font()
    assert byte_size(data) < 7000
    assert {:ok, [font]} = Font.load_configured_faces([%{family: "Authored Syriac", data: data}])

    for {text, expected} <- [
          {"ܐ", [{8, 424}]},
          {"ܒܐ", [{51, 553}, {15, 445}]},
          {"ܗܐ", [{10, 430}, {22, 466}]},
          {"ܕܐ", [{11, 433}, {29, 487}]},
          {"ܖܐ", [{13, 439}, {29, 487}]},
          {"ܪܐ", [{12, 436}, {29, 487}]},
          {"ܯܐ", [{14, 442}, {29, 487}]},
          {"ܒܐܒ", [{51, 553}, {43, 529}, {9, 427}]},
          {"ܗܐܒ", [{10, 430}, {8, 424}, {9, 427}]},
          {"ܕܐܒ", [{11, 433}, {8, 424}, {9, 427}]},
          {"ܒܐܐ", [{51, 553}, {43, 529}, {22, 466}]},
          {"ܗܐܐ", [{10, 430}, {8, 424}, {22, 466}]},
          {"ܕܐܐ", [{11, 433}, {8, 424}, {22, 466}]},
          {"ܒܒܒ", [{51, 553}, {37, 511}, {16, 448}]},
          {"ܒܗܒ", [{51, 553}, {17, 451}, {9, 427}]},
          {"ܒܐ ܒ", [{51, 553}, {15, 445}, {59, 200}, {9, 427}]},
          {"ܒܐܰܒ", [{51, 553}, {43, 529}, {58, 0}, {9, 427}]},
          {"ܗܰܐ", [{10, 430}, {58, 0}, {22, 466}]},
          {"ܕܰܐ", [{11, 433}, {58, 0}, {29, 487}]},
          {"ܒ‌ܐ", [{9, 427}, {8, 424}]},
          {"ܒ‍ܐ", [{51, 553}, {15, 445}]},
          {"ܒܐ‌ܒ", [{51, 553}, {15, 445}, {9, 427}]},
          {"ܒܐ‍ܒ", [{51, 553}, {43, 529}, {16, 448}]}
        ] do
      assert {:ok, shaped} = TextShaper.shape(text, font)
      glyphs = Enum.reject(shaped.glyphs, & &1.ignorable?)
      assert Enum.map(glyphs, &{&1.glyph, &1.x_advance}) == expected, text
      assert shaped.advance == Enum.sum(Enum.map(expected, &elem(&1, 1)))
      assert shaped.text == text
    end

    assert {:ok, shaped} =
             TextShaper.shape([%{text: "ܒ", owner: :first}, %{text: "ܐܒ", owner: :second}], font)

    assert Enum.map(shaped.glyphs, & &1.glyph) == [51, 43, 9]
    assert Enum.map(shaped.glyphs, &hd(&1.fragments).owner) == [:first, :second, :second]
  end
end
