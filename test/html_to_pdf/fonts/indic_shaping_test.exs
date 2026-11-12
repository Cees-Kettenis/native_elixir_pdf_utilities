Code.require_file(NativeElixirPdfUtilities.TestSupport.Paths.support("open_type_fixture.ex"))

defmodule NativeElixirPdfUtilities.HtmlToPdf.IndicShapingTest do
  use ExUnit.Case, async: true
  alias NativeElixirPdfUtilities.HtmlToPdf.{Font, TextShaper}
  alias NativeElixirPdfUtilities.TestSupport.OpenTypeFixture, as: Fixture

  test "Indic preprocessing permits a ccmp lookup to remove the complete syllable" do
    delete =
      Fixture.join(
        fn [coverage, sequence] -> Fixture.words([1, coverage, 1, sequence]) end,
        [Fixture.coverage([1]), Fixture.words([0])]
      )

    font =
      Fixture.font([%{type: 2, subtables: [delete]}], [], features: [{"ccmp", [0]}])
      |> Map.put(:cmap, %{0x915 => 1})

    assert {:ok, shaped} = TextShaper.shape("क", font)
    assert shaped.glyphs == [] and shaped.advance == 0
    assert shaped.text == "क"
  end

  test "unsubstituted Sinhala reph candidates retain nominal order with and without a joiner" do
    font = Fixture.font() |> Map.put(:cmap, %{0xDBB => 1, 0xDCA => 2, 0xD9A => 3, 0x200D => 4})

    for text <- ["ර්ක", "ර්‍ක"] do
      assert {:ok, shaped} = TextShaper.shape(text, font)
      assert Enum.map(Enum.reject(shaped.glyphs, & &1.ignorable?), & &1.glyph) == [1, 2, 3]
      assert shaped.text == text
    end
  end

  test "Indic syllable stages and positions match an independent HarfBuzz oracle without font assets" do
    data = Fixture.indic_font()

    assert byte_size(data) < 6_000
    assert {:ok, [font]} = Font.load_configured_faces([%{family: "Indic oracle", data: data}])

    # System libharfbuzz shaped the same authored bytes independently. Rows give
    # visible glyphs and complete advances; two GPOS adjustments are checked below.
    for {text, ids, advances} <- [
          {"कि", [4, 1], [200, 500]},
          {"क्षि", [4, 20], [200, 500]},
          {"र्कि", [4, 1, 21], [200, 500, 0]},
          {"क्नि", [4, 22, 6], [200, 300, 500]},
          {"क्‍नि", [4, 22, 6], [200, 300, 500]},
          {"क्‌नि", [1, 2, 4, 6], [500, 0, 200, 500]},
          {"क्रि", [4, 1, 24], [200, 500, 0]},
          {"किकि", [4, 1, 4, 1], [200, 500, 200, 500]},
          {"क्", [1, 2], [500, 0]},
          {"क्‍", [22], [300]},
          {"क्‌", [1, 2], [500, 0]},
          {"र्का", [1, 9, 21], [500, 500, 0]},
          {"र्कं", [1, 21, 10], [500, 0, 500]},
          {"र्कः", [1, 21, 11], [500, 0, 500]}
        ] do
      assert {:ok, shaped} = TextShaper.shape(text, font)
      glyphs = Enum.reject(shaped.glyphs, & &1.ignorable?)
      assert Enum.map(glyphs, & &1.glyph) == ids
      assert Enum.map(glyphs, & &1.x_advance) == advances
      assert shaped.advance == Enum.sum(advances)
      assert shaped.text == text

      for glyph <- glyphs do
        expected =
          case glyph.glyph do
            4 -> {20, -30}
            21 -> {-350, 700}
            _ -> {0, 0}
          end

        assert {glyph.x_offset, glyph.y_offset} == expected
      end
    end

    assert {:ok, shaped} =
             TextShaper.shape([%{text: "क्", owner: :left}, %{text: "षि", owner: :right}], font)

    assert Enum.map(shaped.glyphs, & &1.glyph) == [4, 20]
    assert Enum.map(List.last(shaped.glyphs).fragments, & &1.owner) == [:left, :left, :right]
    assert TextShaper.cluster_boundaries(shaped) == [0, 12]
  end

  test "split matras remain separately reorderable even when the font covers their composed scalar" do
    font = Fixture.font() |> Map.put(:cmap, %{0x995 => 1, 0x9CB => 2, 0x9C7 => 3, 0x9BE => 4})
    assert {:ok, shaped} = TextShaper.shape([%{text: "কো", owner: :word}], font)
    assert Enum.map(shaped.glyphs, & &1.glyph) == [3, 1, 4]

    assert Enum.map(shaped.glyphs, & &1.fragments) == [
             [%{owner: :word, text: "ো", offset: 1}],
             [%{owner: :word, text: "ক", offset: 0}],
             [%{owner: :word, text: "ো", offset: 1}]
           ]

    assert shaped.text == "কো"
    assert TextShaper.cluster_boundaries(shaped) == [0, 6]
  end
end
