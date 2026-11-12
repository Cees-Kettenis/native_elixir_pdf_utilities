Code.require_file(NativeElixirPdfUtilities.TestSupport.Paths.support("open_type_fixture.ex"))

defmodule NativeElixirPdfUtilities.HtmlToPdf.OpenTypeGdefTest do
  use ExUnit.Case, async: true
  alias NativeElixirPdfUtilities.HtmlToPdf.TextShaper
  alias NativeElixirPdfUtilities.TestSupport.OpenTypeFixture, as: Fixture
  alias NativeElixirPdfUtilities.Validators.OpenTypeLayoutValidator

  import NativeElixirPdfUtilities.TestSupport.OpenTypeFixture,
    only: [join: 2, words: 1, coverage: 1]

  defp attachments(points) do
    join(
      fn [covered, target] -> words([covered, 1, target]) end,
      [coverage([1]), words([length(points) | points])]
    )
  end

  defp carets(values) do
    target = join(fn offsets -> words([length(offsets) | offsets]) end, values)
    join(fn [covered, target] -> words([covered, 1, target]) end, [coverage([2]), target])
  end

  defp definition(options \\ []) do
    classes = words([1, 1, 4, 1, 2, 3, 3])
    mark_classes = words([1, 3, 2, 1, 2])

    sets =
      join(fn [covered] -> <<Keyword.get(options, :set_format, 1)::16, 1::16, covered::32>> end, [
        coverage([3])
      ])

    device = words([0, 0, 0x8000])
    adjusted = join(fn [device] -> words([3, 200, device]) end, [device])
    caret = carets([words([1, 100]), words([2, 3]), adjusted])

    store =
      <<1::16, 12::32, 1::16, 22::32, 1::16, 1::16, 0::signed-16, 16384::signed-16,
        16384::signed-16, 1::16, 1::16, 1::16, 0::16, 15::signed-16>>

    join(
      fn [classes, attachments, carets, mark_classes, sets, store] ->
        words([
          Keyword.get(options, :major, 1),
          3,
          classes,
          attachments,
          carets,
          mark_classes,
          sets
        ]) <> <<store::32>>
      end,
      [
        classes,
        Keyword.get(options, :attachments, attachments([1, 4])),
        Keyword.get(options, :carets, caret),
        mark_classes,
        sets,
        Keyword.get(options, :store, store)
      ]
    )
  end

  defp font(gdef) do
    single = join(fn [covered] -> words([1, covered, 4, 10]) end, [coverage([1, 3, 4])])

    Fixture.font([], [%{type: 1, flags: 16, mark_set: 0, subtables: [single]}], gdef: gdef)
    |> Map.merge(%{normalized_coordinates: [0.5], cmap: %{?A => 1, 0x301 => 3, 0x302 => 4}})
  end

  test "GDEF supplies contour attachments, ligature carets, mark sets and evaluated variation deltas" do
    for data <- [
          words([1, 0, 0, 0, 0, 0]),
          words([1, 2, 0, 0, 0, 0, 0]),
          words([1, 3, 0, 0, 0, 0, 0]) <> <<0::32>>
        ] do
      assert {:ok, %{gdef: %{carets: %{}, attachments: %{}, mark_sets: {}}}} =
               OpenTypeLayoutValidator.prepare(Fixture.font([], [], gdef: data))
    end

    font = font(definition())
    assert {:ok, %{gdef: gdef}} = OpenTypeLayoutValidator.prepare(font)
    assert gdef.classes == %{1 => 1, 2 => 2, 3 => 3, 4 => 3}
    assert gdef.mark_classes == %{3 => 1, 4 => 2}
    assert gdef.attachments == %{1 => [1, 4]}
    assert gdef.mark_sets == {MapSet.new([3])}

    assert gdef.carets[2] == [
             %{value: 100, point: nil, device: nil},
             %{value: nil, point: 3, device: nil},
             %{value: 200, point: nil, device: %{variation_index: {0, 0}, delta: 7.5}}
           ]

    assert elem(gdef.store, 0).values == {7.5}

    for {text, advances} <- [{"Á", [510, 10]}, {"Â", [510, 0]}] do
      assert {:ok, shaped} = TextShaper.shape(text, font, features: %{"test" => 1})
      assert Enum.map(shaped.glyphs, & &1.x_advance) == advances
    end
  end

  test "attachment classes filter mark positioning and ignored marks do not interrupt pair matching" do
    single = join(fn [covered] -> words([1, covered, 4, 10]) end, [coverage([1, 3, 4])])

    for {flags, expected} <- [{256, [510, 10, 0]}, {512, [510, 0, 10]}] do
      font =
        Fixture.font([], [%{type: 1, flags: flags, subtables: [single]}], gdef: definition())
        |> Map.put(:normalized_coordinates, [0.5])
        |> Map.put(:cmap, %{?A => 1, 0x301 => 3, 0x302 => 4})

      assert {:ok, result} = TextShaper.shape("Á̂", font, features: %{"test" => 1})
      assert Enum.map(result.glyphs, & &1.x_advance) == expected
    end

    pair =
      join(
        fn [covered, pairs] -> words([1, covered, 4, 0, 1, pairs]) end,
        [coverage([1]), words([1, 2, -40])]
      )

    font =
      Fixture.font([], [%{type: 2, flags: 256, subtables: [pair]}], gdef: definition())
      |> Map.put(:normalized_coordinates, [0.5])
      |> Map.put(:cmap, %{?A => 1, ?B => 2, 0x302 => 4})

    assert {:ok, result} = TextShaper.shape("ÂB", font, features: %{"test" => 1})
    assert Enum.map(result.glyphs, & &1.x_advance) == [460, 0, 500]
  end

  test "mark attachment skips bases ignored by the lookup before finding an earlier mark" do
    marks = join(fn [anchor] -> words([1, 0, anchor]) end, [words([1, 10, 20])])
    bases = join(fn [anchor] -> words([1, anchor]) end, [words([1, 100, 200])])

    attachment =
      join(
        fn [mark, base, marks, bases] -> words([1, mark, base, 1, marks, bases]) end,
        [coverage([4]), coverage([3]), marks, bases]
      )

    font =
      Fixture.font([], [%{type: 6, flags: 2, subtables: [attachment]}], gdef: definition())
      |> Map.put(:normalized_coordinates, [0.5])
      |> Map.put(:cmap, %{?A => 1, 0x301 => 3, 0x302 => 4})

    assert {:ok, result} = TextShaper.shape("́Â", font, features: %{"test" => 1})

    assert Enum.map(result.glyphs, &{&1.glyph, &1.x_offset, &1.y_offset}) == [
             {3, 0, 0},
             {1, 0, 0},
             {4, -410, 180}
           ]
  end

  test "GDEF rejects inconsistent coverage, contour order, unsupported carets and invalid stores" do
    mismatched = join(fn [covered] -> words([covered, 0]) end, [coverage([1])])

    cases = [
      {definition(major: 2), "GDEF version"},
      {definition(set_format: 2), "mark glyph set format"},
      {definition(attachments: mismatched), "attachment list count"},
      {definition(attachments: attachments([4, 1])), "attachment point indices"},
      {definition(carets: mismatched), "ligature caret count"},
      {definition(carets: carets([words([4, 100])])), "ligature caret format"},
      {definition(store: <<0::16, 12::32, 0::16>>), "ItemVariationStore format"}
    ]

    for {data, message} <- cases do
      assert {:error, {:invalid_font, diagnostic}} = OpenTypeLayoutValidator.prepare(font(data))
      assert diagnostic.stage == :font and diagnostic.message =~ message
      assert is_atom(diagnostic.module) and is_atom(diagnostic.operation)
    end
  end
end
