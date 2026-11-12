Code.require_file(NativeElixirPdfUtilities.TestSupport.Paths.support("open_type_fixture.ex"))

defmodule NativeElixirPdfUtilities.HtmlToPdf.OpenTypeLayoutVariationTest do
  use ExUnit.Case, async: true
  alias NativeElixirPdfUtilities.HtmlToPdf.TextShaper
  alias NativeElixirPdfUtilities.HtmlToPdf.Font
  alias NativeElixirPdfUtilities.TestSupport.{FontCorpus, FontFixture}
  alias NativeElixirPdfUtilities.TestSupport.OpenTypeFixture, as: Fixture
  alias NativeElixirPdfUtilities.Validators.OpenTypeLayoutValidator

  import NativeElixirPdfUtilities.TestSupport.OpenTypeFixture,
    only: [join: 2, words: 1, coverage: 1]

  # Offsets and F2DOT14 boundaries are authored directly from the OpenType 1.9.1
  # FeatureVariations/ConditionSet/FeatureTableSubstitution table definitions.
  defp variations(records, version \\ {1, 0}) do
    children =
      Enum.flat_map(records, fn {conditions, substitution} -> [conditions, substitution] end)

    join(
      fn offsets ->
        {major, minor} = version

        words([major, minor]) <>
          <<length(records)::32>> <>
          for offset <- offsets, into: <<>>, do: <<offset::32>>
      end,
      children
    )
  end

  defp conditions(format, axis, low, high) do
    join(
      fn [offset] -> <<1::16, offset::32>> end,
      [<<format::16, axis::16, low::signed-16, high::signed-16>>]
    )
  end

  defp substitutions(indices, lookups, version \\ {1, 0}, params? \\ false) do
    target =
      words([if(params?, do: 4 + 2 * length(lookups), else: 0), length(lookups)] ++ lookups) <>
        if(params?, do: words([1]), else: <<>>)

    join(
      fn offsets ->
        {major, minor} = version

        words([major, minor, length(indices)]) <>
          (Enum.zip(indices, offsets)
           |> Enum.map(fn {index, offset} -> <<index::16, offset::32>> end)
           |> IO.iodata_to_binary())
      end,
      List.duplicate(target, length(indices))
    )
  end

  defp font(variations, coordinates \\ [0.0]) do
    lookups =
      for target <- [2, 3, 4] do
        %{
          type: 1,
          subtables: [join(fn [offset] -> words([2, offset, 1, target]) end, [coverage([1])])]
        }
      end

    Fixture.font(lookups, [], features: [{"test", [0]}], variations: variations)
    |> Map.put(:normalized_coordinates, coordinates)
  end

  test "configured variable fonts retain feature variations through public loading and instance selection" do
    variable = FontCorpus.path("selected-variable.ttf") |> File.read!()
    {_, source_tables} = FontFixture.tables(variable)

    lookups =
      for target <- [2, 3] do
        %{
          type: 1,
          subtables: [join(fn [offset] -> words([2, offset, 1, target]) end, [coverage([1])])]
        }
      end

    records = variations([{conditions(1, 0, 8192, 16384), substitutions([0], [1])}])

    data =
      Fixture.outline_font(lookups, [],
        features: [{"ccmp", [0]}],
        variations: records,
        widths: %{2 => 600, 3 => 900}
      )
      |> FontFixture.rebuild(%{"fvar" => source_tables["fvar"]})

    assert {:ok, [normal]} =
             Font.load_configured_faces([%{family: "Feature variations", data: data}])

    assert {:ok, default} = TextShaper.shape("A", normal)
    assert hd(default.glyphs).glyph == 2
    assert_in_delta Font.text_width("A", normal, 12), 7.2, 1.0e-8

    assert {:ok, heavy} =
             NativeElixirPdfUtilities.Validators.FontValidator.prepare_font_instance(
               normal,
               %{"wght" => 900},
               nil
             )

    assert {:ok, selected} = TextShaper.shape("A", heavy)
    assert hd(selected.glyphs).glyph == 3
    assert Font.text_width("A", heavy, 12) == 10.8
    assert normal.normalized_coordinates == [0.0, 0.0]
    assert heavy.normalized_coordinates == [1.0, 0.0]
  end

  test "GSUB variations select inclusive normalized intervals and the first matching condition set" do
    first = conditions(1, 0, 8192, 16384)
    second = conditions(1, 0, 0, 16384)

    data =
      variations([
        {first, substitutions([0], [1], {1, 0}, true)},
        {second, substitutions([0], [2])}
      ])

    for {coordinate, expected} <- [{-1.0, 2}, {-0.01, 2}, {0.0, 4}, {0.49, 4}, {0.5, 3}, {1.0, 3}] do
      font = font(data, [coordinate])
      assert {:ok, shaped} = TextShaper.shape("A", font, features: %{"test" => 1})
      assert Enum.map(shaped.glyphs, & &1.glyph) == [expected]
      assert shaped.advance == 500
      assert {:ok, layout} = OpenTypeLayoutValidator.prepare(font)
      assert hd(layout.gsub.variations).conditions == [{0, 0.5, 1.0}]
    end
  end

  test "GPOS variations replace a feature's lookup list without changing source clusters" do
    data = variations([{conditions(1, 0, 8192, 16384), substitutions([0], [1])}])

    lookups =
      for advance <- [10, 40] do
        %{
          type: 1,
          subtables: [join(fn [offset] -> words([1, offset, 4, advance]) end, [coverage([1])])]
        }
      end

    base = Fixture.font([], lookups, features: [{"test", [0]}], variations: data)

    for {coordinate, advance} <- [{0.0, 510}, {0.5, 540}] do
      assert {:ok, shaped} =
               TextShaper.shape(
                 [%{text: "A", owner: :source}],
                 Map.put(base, :normalized_coordinates, [coordinate]),
                 features: %{"test" => 1}
               )

      assert [%{glyph: 1, x_advance: ^advance, fragments: [%{owner: :source}]}] = shaped.glyphs
    end

    assert {:ok, %{gsub: %{variations: []}}} = OpenTypeLayoutValidator.prepare(font(nil))
  end

  test "malformed variation versions, normalized ranges and references diagnose the owning table" do
    valid_conditions = conditions(1, 0, 0, 16384)
    valid_substitutions = substitutions([0], [1])

    invalid = [
      {variations([{valid_conditions, valid_substitutions}], {2, 0}),
       "FeatureVariations version"},
      {variations([{valid_conditions, substitutions([0], [1], {2, 0})}]),
       "FeatureTableSubstitution version"},
      {variations([{valid_conditions, substitutions([1], [1])}]), "unavailable feature"},
      {variations([{valid_conditions, substitutions([0], [3])}]), "unavailable lookup"},
      {variations([{valid_conditions, substitutions([0, 0], [1])}]), "unordered or repeated"}
    ]

    invalid =
      invalid ++
        for values <- [
              {0, 0, 0, 16384},
              {1, 1, 0, 16384},
              {1, 0, -16385, 0},
              {1, 0, 0, 16385},
              {1, 0, 1, -1}
            ] do
          {format, axis, low, high} = values

          {variations([{conditions(format, axis, low, high), valid_substitutions}]),
           "variation condition"}
        end

    for {data, message} <- invalid do
      assert {:error, {:invalid_font, diagnostic}} = OpenTypeLayoutValidator.prepare(font(data))
      assert diagnostic.stage == :font and diagnostic.operation == :prepare_opentype_layout
      assert diagnostic.source =~ "GSUB byte"
      assert diagnostic.message =~ message

      assert {:error, {:invalid_font, _}} =
               TextShaper.shape("A", font(data), features: %{"test" => 1})
    end
  end
end
