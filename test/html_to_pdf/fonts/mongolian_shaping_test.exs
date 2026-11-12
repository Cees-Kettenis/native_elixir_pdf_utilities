Code.require_file(NativeElixirPdfUtilities.TestSupport.Paths.support("open_type_fixture.ex"))

defmodule NativeElixirPdfUtilities.HtmlToPdf.MongolianShapingTest do
  use ExUnit.Case, async: true
  alias NativeElixirPdfUtilities.HtmlToPdf.{Font, TextShaper}
  alias NativeElixirPdfUtilities.TestSupport.OpenTypeFixture, as: Fixture

  test "joining masks constrain every ligature component and selectors inherit their base form" do
    # Independently shaped with system libharfbuzz. These small tables exercise
    # legacy GSUB selectors without adding a binary font or runtime dependency.
    for {tag, first, component, expected} <- [
          {"init", 2, 3, [1, 3]},
          {"init", 1, 2, [4, 3]},
          {"liga", 2, 3, [1, 4]}
        ] do
      set =
        Fixture.join(fn [offset] -> Fixture.words([1, offset]) end, [
          Fixture.words([4, 2, component])
        ])

      ligature =
        Fixture.join(fn [covered, set] -> Fixture.words([1, covered, 1, set]) end, [
          Fixture.coverage([first]),
          set
        ])

      data =
        Fixture.outline_font([%{type: 4, subtables: [ligature]}], [],
          script: "mong",
          features: [{tag, [0]}],
          cmap: %{0x1820 => 1, 0x180B => 2, 0x1821 => 3, 32 => 59},
          widths: %{1 => 500, 2 => 0, 3 => 500, 4 => 800, 59 => 200}
        )

      assert {:ok, [font]} =
               Font.load_configured_faces([%{family: "Authored Mongolian", data: data}])

      assert {:ok, shaped} = TextShaper.shape("ᠠ᠋ᠡ", font)
      assert Enum.map(Enum.reject(shaped.glyphs, & &1.ignorable?), & &1.glyph) == expected
      assert shaped.advance == if(4 in expected, do: 1300, else: 1000)
      assert shaped.text == "ᠠ᠋ᠡ"
    end
  end
end
