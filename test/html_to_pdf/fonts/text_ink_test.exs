Code.require_file(NativeElixirPdfUtilities.TestSupport.Paths.support("open_type_fixture.ex"))

defmodule NativeElixirPdfUtilities.HtmlToPdf.TextInkTest do
  use ExUnit.Case, async: true
  alias NativeElixirPdfUtilities.HtmlToPdf.{Font, ShapedTextPaint, TextInk}
  alias NativeElixirPdfUtilities.TestSupport.Paths
  alias NativeElixirPdfUtilities.Validators.FontValidator

  test "readable ink bounds do not hide a truncated contour program" do
    alias NativeElixirPdfUtilities.TestSupport.{FontFixture, OpenTypeFixture}
    data = OpenTypeFixture.outline_font([], [])
    # A complete one-contour header has readable bounds but no end-point array.
    header = <<1::signed-16, 0::signed-16, 0::signed-16, 100::signed-16, 100::signed-16>>

    data =
      FontFixture.rebuild(data, %{
        "glyf" => header,
        "loca" => for(id <- 0..64, into: <<>>, do: <<if(id < 2, do: 0, else: 10)::32>>)
      })

    assert {:ok, [font]} =
             Font.load_configured_faces([%{family: "Truncated contour", data: data}])

    pieces = [%{text: "A", style: %{font_face: font, font_size: 12}}]

    FontValidator.with_budget(fn ->
      assert {:ok, ink} = TextInk.measure(pieces, false)
      assert length(ink.points) == 4
      assert {:error, {reason, diagnostic}} = TextInk.measure(pieces, :paths)
      assert is_atom(reason) and diagnostic.stage == :font
      assert is_binary(diagnostic.message) and diagnostic.message != ""
    end)

    for decoration <- [
          "text-decoration:underline",
          "text-shadow:1pt 1pt 2pt blue",
          "initial-letter:2;initial-letter-wrap:all"
        ] do
      assert {:error, {_, diagnostic}} =
               NativeElixirPdfUtilities.HtmlToPdf.render(
                 "<p style='font-family:Truncated'><span style='#{decoration}'>A</span></p>",
                 fonts: [%{family: "Truncated", data: data}],
                 system_font_discovery: false
               )

      assert diagnostic.stage == :font
      assert is_atom(diagnostic.operation)
      assert is_binary(diagnostic.message)
    end
  end

  test "measurement and source painting retain the selected face's invalid-metrics diagnostic" do
    alias NativeElixirPdfUtilities.Validators.NativeGlyphSourceValidator, as: Source
    original = NativeElixirPdfUtilities.TestSupport.OpenTypeFixture.font()
    assert [{65, 1, advance}] = Font.css_glyph_advances("A", %{original | widths: []}, 12)
    assert_in_delta advance, 6, 0.0001
    font = original |> Map.put(:widths, [:bad]) |> Map.put(:shaping_language, "en")

    run = %{
      text: "A",
      style: %{font_face: font, font_size: 12},
      source_glyph_characters: [%{displayed: "A", fragments: []}]
    }

    FontValidator.with_budget(fn ->
      assert {:error, {reason, diagnostic}} = Font.css_text_width("A", font, 12)
      assert diagnostic.stage == :font and is_atom(reason)
      assert diagnostic.message =~ "metrics"

      for operation <- [
            fn -> Source.cluster_boundaries(run) end,
            fn -> Source.paint_runs(run, font, 0) end
          ] do
        assert {:render_resource_limit, {:error, {^reason, source_diagnostic}}} =
                 catch_throw(operation.())

        assert source_diagnostic.stage == :font and source_diagnostic.message =~ "metrics"
      end
    end)
  end

  test "built-in font fallback ink retains piece advances in its nominal em rectangle" do
    pieces =
      for text <- ["AB", "C"],
          do: %{text: text, style: %{font_face: %{type: :built_in}, font_size: 10}}

    assert {:ok, ink} = TextInk.measure(pieces, false)
    assert ink.advance == 18

    for {{x, y}, {expected_x, expected_y}} <-
          Enum.zip(ink.points, [{12.0, -2.0}, {18.0, 6.6}, {0.0, -2.0}, {12.0, 6.6}]) do
      assert_in_delta x, expected_x, 0.00001
      assert_in_delta y, expected_y, 0.00001
    end

    assert ink.glyphs == [] and ink.edges == []
  end

  test "uncovered glyphs and faces without outline tables return their original font diagnostics" do
    font = NativeElixirPdfUtilities.TestSupport.OpenTypeFixture.font()
    style = %{font_face: font, font_size: 12}

    FontValidator.with_budget(fn ->
      assert {:error, {:unsupported_glyph, diagnostic}} =
               TextInk.measure([%{text: "a", style: style}], false)

      assert diagnostic.stage == :font and diagnostic.message =~ "uncovered shaping cluster"
      assert {:error, {_, diagnostic}} = TextInk.measure([%{text: "A", style: style}], true)
      assert diagnostic.stage == :font and is_binary(diagnostic.message)
    end)
  end

  test "Arabic mark ink uses the independent HarfBuzz attachment in visual order" do
    {:ok, [font]} =
      Font.load_configured_faces([
        %{family: "Ink", path: Paths.project("priv/fonts/dejavu/DejaVuSans.ttf")}
      ])

    style = %{font_face: font, font_size: 12, direction: :rtl}

    FontValidator.with_budget(fn ->
      assert {:ok, ink} = TextInk.measure([%{text: "بَّ", style: style}], false)
      assert length(ink.glyphs) == 2
      mark = Enum.find(ink.glyphs, &(&1.codepoints != [0x628]))
      assert {:ok, bounds} = Font.glyph_bounds(font)
      {left, bottom, right, top} = elem(bounds, 6020)
      # The existing independent HarfBuzz oracle gives glyph 6020 at +388,-200
      # font units, before the RTL base. The base advances 1928 units.
      assert_in_delta Enum.min(Enum.map(mark.points, &elem(&1, 0))),
                      (388 + left) * 12 / 2048,
                      0.001

      assert_in_delta Enum.max(Enum.map(mark.points, &elem(&1, 0))),
                      (388 + right) * 12 / 2048,
                      0.001

      assert_in_delta Enum.min(Enum.map(mark.points, &elem(&1, 1))),
                      (-200 + bottom) * 12 / 2048,
                      0.001

      assert_in_delta Enum.max(Enum.map(mark.points, &elem(&1, 1))),
                      (-200 + top) * 12 / 2048,
                      0.001

      assert_in_delta ink.advance, 1928 * 12 / 2048, 0.001

      # The public geometry validator must include both the attached mark and
      # the base, using the same independently measured glyph offsets.
      {base_left, base_bottom, base_right, base_top} = elem(bounds, 1366)

      expected =
        {min(base_left, left + 388), min(base_bottom, bottom - 200), max(base_right, right + 388),
         max(base_top, top - 200)}

      box = %{text: "بَّ", font_face: font, font_size: 12, bidi_direction: :rtl, x: 20, y: 30}
      assert {:ok, actual} = FontValidator.prepare_text_ink_rectangle(box)

      for {value, expected, origin} <-
            Enum.zip([Tuple.to_list(actual), Tuple.to_list(expected), [20, 30, 20, 30]]) do
        assert_in_delta value, origin + expected * 12 / 2048, 0.001
      end
    end)
  end

  test "ink and native paint share word spacing, cluster spacing and justification" do
    {:ok, [font]} =
      Font.load_configured_faces([
        %{family: "Ink", path: Paths.project("priv/fonts/dejavu/DejaVuSans.ttf")}
      ])

    style = %{font_face: font, font_size: 12, letter_spacing: 2, word_spacing: 4}

    box = %{
      text: "A B",
      font_face: font,
      font_size: 12,
      letter_spacing: 2,
      word_spacing: 4,
      _justification_spacing: 3,
      _justification_boundaries: [2]
    }

    FontValidator.with_budget(fn ->
      assert {:ok, plan} = ShapedTextPaint.plan(box)
      third = List.last(plan.glyphs)
      # Design advances A=1401 and space=651; two 2pt gaps, one 4pt
      # word expansion and the 3pt boundary adjustment precede B.
      expected = (1401 + 651) * 12 / 2048 + 4 + 4 + 3
      assert_in_delta third.paint_x, expected, 0.001
      assert {:ok, ink} = TextInk.measure([%{text: box.text, box: box, style: style}], :paths)
      glyph = Enum.find(ink.glyphs, &(&1.codepoints == [?B]))
      assert {:ok, bounds} = Font.glyph_bounds(font)
      {left, _, _, _} = elem(bounds, Map.fetch!(font.cmap, ?B))

      assert_in_delta Enum.min(Enum.map(glyph.points, &elem(&1, 0))),
                      expected + left * 12 / 2048,
                      0.001

      assert glyph.commands != [] and glyph.edges == []
      assert_in_delta ink.advance, plan.advance, 0.0001
    end)
  end
end
