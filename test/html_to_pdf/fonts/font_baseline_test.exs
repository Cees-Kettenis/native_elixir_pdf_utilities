Code.require_file(NativeElixirPdfUtilities.TestSupport.Paths.support("open_type_fixture.ex"))

defmodule NativeElixirPdfUtilities.HtmlToPdf.FontBaselineTest do
  use ExUnit.Case, async: true
  alias NativeElixirPdfUtilities.Validators.{FontBaselineValidator, OpenTypeLayoutValidator}
  alias NativeElixirPdfUtilities.HtmlToPdf.Font.Sfnt
  alias NativeElixirPdfUtilities.TestSupport.{FontFixture, OpenTypeFixture}
  import NativeElixirPdfUtilities.TestSupport.OpenTypeFixture, only: [join: 2, words: 1]

  defp baseline(tags, coordinates, options \\ []) do
    values =
      join(
        fn offsets ->
          words([Keyword.get(options, :default, 0), length(coordinates)] ++ offsets)
        end,
        coordinates
      )

    default_extent = Keyword.get(options, :extents)
    language_extent = Keyword.get(options, :language)
    language? = language_extent != nil

    script =
      join(
        fn [values, default_extent, language_extent] ->
          words([values, default_extent, if(language?, do: 1, else: 0)]) <>
            if(language?, do: "URD " <> words([language_extent]), else: <<>>)
        end,
        [
          if(Keyword.get(options, :no_values, false), do: nil, else: values),
          default_extent,
          language_extent
        ]
      )

    scripts =
      join(
        fn [script] -> words([1]) <> Keyword.get(options, :script, "latn") <> words([script]) end,
        [script]
      )

    axis =
      join(fn [tags, scripts] -> words([tags, scripts]) end, [
        if(tags == nil, do: nil, else: words([length(tags)]) <> Enum.join(tags)),
        scripts
      ])

    store = Keyword.get(options, :store)

    if Keyword.get(options, :minor, 0) == 1 do
      join(
        fn [horizontal, vertical, store] ->
          <<1::16, 1::16, horizontal::16, vertical::16, store::32>>
        end,
        [axis, if(Keyword.get(options, :vertical, false), do: axis), store]
      )
    else
      join(fn [horizontal, vertical] -> words([1, 0, horizontal, vertical]) end, [
        axis,
        if(Keyword.get(options, :vertical, false), do: axis)
      ])
    end
  end

  test "middle baseline fallback retains an unreadable x-height glyph diagnostic" do
    font = Map.put(OpenTypeFixture.font(), :cmap, %{?x => 1, ?A => 2})
    parent = %{font_face: font, font_size: 12, line_height: 15}
    style = Map.put(parent, :vertical_align, {:baseline_controls, :middle, 0.0})

    assert {:error, {_, diagnostic}} =
             NativeElixirPdfUtilities.Validators.CssBaselineValidator.resolve(style, parent, "A")

    assert diagnostic.stage == :font
    assert is_atom(diagnostic.operation) and is_binary(diagnostic.message)
  end

  test "BASE coordinates retain both axes, signed design values, contour references and pixel devices" do
    device = words([12, 12, 3]) <> <<-2::signed-8, 0>>

    data =
      baseline(
        ["hang", "ideo", "romn"],
        [<<1::16, 700::signed-16>>, words([3, 100, 6]) <> device, words([2, 0, 1, 2])],
        default: 2,
        vertical: true
      )

    assert {:ok, model} = FontBaselineValidator.prepare(data, %{glyph_count: 64, coordinates: []})
    assert model.horizontal == model.vertical
    script = model.horizontal.scripts["latn"]
    assert script.default == "romn"
    assert script.values["hang"].value == 700
    assert script.values["ideo"].device == %{start_size: 12, end_size: 12, deltas: [-2]}
    assert script.values["romn"].glyph == 1 and script.values["romn"].point == 2
    assert script.extents == nil and script.languages == %{}
  end

  test "script and language extents keep optional feature-specific coordinates" do
    extent =
      join(
        fn [minimum, maximum, feature_minimum, feature_maximum] ->
          words([minimum, maximum, 1]) <> "sups" <> words([feature_minimum, feature_maximum])
        end,
        [<<1::16, -250::signed-16>>, words([1, 900]), nil, words([1, 1000])]
      )

    data = baseline(["romn"], [words([1, 0])], extents: extent, language: extent)
    assert {:ok, model} = FontBaselineValidator.prepare(data, %{glyph_count: 64, coordinates: []})
    script = model.horizontal.scripts["latn"]
    assert script.extents == script.languages["URD "]
    assert script.extents.min.value == -250 and script.extents.max.value == 900
    assert script.extents.features["sups"].min == nil
    assert script.extents.features["sups"].max.value == 1000
    assert model.vertical == nil

    font = OpenTypeFixture.font()
    {scaler, tables} = FontFixture.tables(font.data)
    font = %{font | data: Sfnt.rebuild(scaler, Map.to_list(Map.put(tables, "BASE", data)))}
    assert {:ok, selected} = FontBaselineValidator.coordinates(font, :horizontal, ["latn"], 12)
    expected = %{min: -250, max: 900, features: %{"sups" => %{min: nil, max: 1000}}}
    assert selected.extents == expected
    assert selected.languages["URD "] == expected
  end

  test "BASE variation indices use their local store independently of GDEF" do
    store =
      join(fn [regions, items] -> <<1::16, regions::32, 1::16, items::32>> end, [
        words([1, 1, 0, 16384, 16384]),
        words([1, 1, 1, 0, 200])
      ])

    coordinate = words([3, 400, 6, 0, 0, 0x8000])
    data = baseline(["hang"], [coordinate], minor: 1, store: store)

    for {normalized, delta} <- [{0.0, 0.0}, {1.0, 200.0}] do
      assert {:ok, model} =
               FontBaselineValidator.prepare(data, %{
                 glyph_count: 64,
                 coordinates: [normalized],
                 gdef: %{store: :unrelated}
               })

      assert model.horizontal.scripts["latn"].values["hang"].device.delta == delta
    end
  end

  test "absent baseline data and optional empty axes do not invent metrics" do
    assert {:ok, %{horizontal: nil, vertical: nil}} = FontBaselineValidator.prepare(nil, %{})

    assert {:ok, model} =
             FontBaselineValidator.prepare(baseline(nil, [], no_values: true), %{
               glyph_count: 64,
               coordinates: []
             })

    assert model.horizontal.tags == []
    assert model.horizontal.scripts["latn"].values == %{}

    assert {:ok, _} =
             FontBaselineValidator.prepare(baseline([], []), %{glyph_count: 64, coordinates: []})
  end

  test "malformed BASE data returns bounded font diagnostics before layout" do
    language_data = baseline(["romn"], [words([1, 0])], language: words([0, 0, 0]))
    {language_offset, _} = :binary.match(language_data, "URD ")
    null_language = FontFixture.replace(language_data, language_offset + 4, <<0::16>>)

    invalid = [
      null_language,
      <<>>,
      words([2, 0, 0, 0]),
      words([1, 0, 65535, 0]),
      baseline(["romn", "hang"], [words([1, 0]), words([1, 700])]),
      baseline(["romn"], []),
      baseline(["romn"], [words([1, 0])], default: 1),
      baseline(["romn"], [nil]),
      baseline(["romn"], [words([5, 0])]),
      baseline(["romn"], [words([2, 0, 64, 0])]),
      baseline(["romn"], [words([3, 0, 6, 0, 0, 0x8000])]),
      baseline(["romn"], [words([3, 0, 4])])
    ]

    for data <- invalid do
      assert {:error, {:invalid_font, diagnostic}} =
               FontBaselineValidator.prepare(data, %{glyph_count: 64, coordinates: []})

      assert diagnostic.stage == :font and diagnostic.source =~ "BASE"
      assert diagnostic.message != ""
    end

    assert {:error, {:invalid_font, diagnostic}} = FontBaselineValidator.prepare(:invalid, %{})
    assert diagnostic.source == "BASE"
  end

  test "the selected font layout shares BASE admission with GSUB, GPOS and GDEF" do
    font = OpenTypeFixture.font()
    {scaler, tables} = FontFixture.tables(font.data)
    data = baseline(["romn"], [words([1, 20])])
    font = %{font | data: Sfnt.rebuild(scaler, Map.to_list(Map.put(tables, "BASE", data)))}
    assert {:ok, model} = OpenTypeLayoutValidator.prepare(font)
    assert model.baselines.horizontal.scripts["latn"].values["romn"].value == 20
  end

  test "selected baseline values evaluate devices and reuse script fallback without inventing absent baselines" do
    data =
      baseline(
        ["ideo", "romn"],
        [words([3, 100, 6, 12, 12, 3]) <> <<-2::signed-8, 0>>, words([3, 0, 0])],
        default: 1
      )

    font = OpenTypeFixture.font()
    {scaler, tables} = FontFixture.tables(font.data)
    font = %{font | data: Sfnt.rebuild(scaler, Map.to_list(Map.put(tables, "BASE", data)))}

    assert {:ok, selected} =
             FontBaselineValidator.coordinates(font, :horizontal, ["arab", "latn"], 12)

    assert selected.default == "romn" and selected.values["romn"] == 0
    assert_in_delta selected.values["ideo"], 100 - 2 * 1000 / 12, 0.00001
    assert {:ok, selected} = FontBaselineValidator.coordinates(font, :horizontal, ["latn"], 13)
    assert selected.values["ideo"] == 100
    assert {:ok, selected} = FontBaselineValidator.coordinates(font, :horizontal, ["latn"])
    assert selected.values["ideo"] == 100
    assert {:ok, nil} = FontBaselineValidator.coordinates(font, :vertical, ["latn"])
    assert {:ok, nil} = FontBaselineValidator.coordinates(font, :horizontal, ["arab"])

    assert {:ok, nil} =
             FontBaselineValidator.coordinates(%{type: :built_in}, :horizontal, ["latn"])

    for {face, axis, scripts, size} <- [
          {font, :other, ["latn"], nil},
          {font, :horizontal, ["bad"], nil},
          {font, :horizontal, ["latn"], 0},
          {%{}, :horizontal, ["latn"], nil}
        ] do
      assert {:error, {:invalid_font, diagnostic}} =
               FontBaselineValidator.coordinates(face, axis, scripts, size)

      assert diagnostic.source == "BASE" and diagnostic.stage == :font
    end
  end

  test "empty axes and invalid local variation data retain their BASE source" do
    assert {:ok, %{horizontal: nil, vertical: nil}} =
             FontBaselineValidator.prepare(words([1, 0, 0, 0]), %{
               glyph_count: 64,
               coordinates: []
             })

    assert {:ok, _} =
             FontBaselineValidator.prepare(baseline(["romn"], [words([1, 0])], minor: 1), %{
               glyph_count: 64,
               coordinates: []
             })

    assert {:error, {:invalid_font, diagnostic}} =
             FontBaselineValidator.prepare(
               baseline(["romn"], [words([1, 0])], minor: 1, store: <<0>>),
               %{glyph_count: 64, coordinates: []}
             )

    assert diagnostic.stage == :font and diagnostic.source =~ "BASE byte"
    device = %{start_size: 0, end_size: 0, deltas: [1]}

    assert NativeElixirPdfUtilities.Validators.OpenTypeDataValidator.device_adjustment(
             device,
             0.00000001,
             1000
           ) == 0
  end

  test "initial-letter alignment uses BASE ideographic edges and hanging coordinates" do
    alias NativeElixirPdfUtilities.Validators.CssInitialLetterValidator
    tags = ["hang", "icfb", "icft", "ideo", "romn"]

    data =
      baseline(tags, Enum.map([700, -200, 800, -150, 20], &<<1::16, &1::signed-16>>),
        default: 4,
        vertical: true
      )

    font = OpenTypeFixture.font()
    {scaler, tables} = FontFixture.tables(font.data)
    font = %{font | data: Sfnt.rebuild(scaler, Map.to_list(Map.put(tables, "BASE", data)))}

    for {face, mode, expected_over, expected_under} <- [
          {font, :hanging, 0.7, 0.02},
          {font, :ideographic, 0.8, -0.2},
          {Map.put(font, :_vertical_text, %{mode: :vertical_rl}), :ideographic, 0.8, -0.2}
        ] do
      assert {:ok, {over, under}} =
               CssInitialLetterValidator.alignment_metrics(face, mode, 12, "Latn")

      assert_in_delta over, expected_over, 0.000000000001
      assert_in_delta under, expected_under, 0.000000000001
    end

    assert {:ok, {over, under}} = CssInitialLetterValidator.alignment_metrics(font, :hanging, 12)
    assert_in_delta over, 0.6, 0.000000000001
    assert under == 0
    assert {:ok, selected} = FontBaselineValidator.coordinates(font, :horizontal, ["latn"])
    assert selected.values["ideo"] == -150
  end

  test "inline baseline controls use script-specific BASE coordinates relative to Roman origins" do
    alias NativeElixirPdfUtilities.Validators.CssBaselineValidator

    for {script, text} <- [{"latn", "A"}, {"arab", "ب"}] do
      data =
        baseline(
          ["hang", "ideo", "idtp", "math", "romn"],
          Enum.map([700, -200, 800, 300, 20], &<<1::16, &1::signed-16>>),
          script: script,
          vertical: true
        )

      font = OpenTypeFixture.font()
      {scaler, tables} = FontFixture.tables(font.data)
      font = %{font | data: Sfnt.rebuild(scaler, Map.to_list(Map.put(tables, "BASE", data)))}

      for writing <- [:horizontal_tb, :vertical_rl],
          {metric, offset} <- [
            {"hanging", 6.8},
            {"ideographic", -2.2},
            {"central", 2.8},
            {"mathematical", 2.8},
            {"alphabetic", 0}
          ] do
        parent = %{font_face: font, font_size: 20, line_height: 24, writing_mode: writing}
        child = %{parent | font_size: 10}
        assert {:ok, admitted} = CssBaselineValidator.prepare(child, "vertical-align", metric)
        assert {:ok, resolved} = CssBaselineValidator.resolve(admitted, parent, text)
        actual = if resolved.vertical_align == :baseline, do: 0, else: resolved.vertical_align
        assert_in_delta actual, offset, 0.000000001
      end

      if script == "arab" do
        parent = %{font_face: font, font_size: 20, line_height: 24}

        assert {:ok, child} =
                 CssBaselineValidator.prepare(
                   %{parent | font_size: 10},
                   "vertical-align",
                   "hanging"
                 )

        assert {:ok, resolved} = CssBaselineValidator.resolve(child, parent, "A")
        assert_in_delta resolved.vertical_align, 6, 0.000000001
      end

      parent = %{font_face: font, font_size: 20, line_height: 24, dominant_baseline: :hanging}
      child = %{parent | font_size: 10, dominant_baseline: :alphabetic}

      for {value, extra} <- [{"baseline", 0}, {"2pt", 2}, {"sub", -4.74609375}] do
        assert {:ok, admitted} = CssBaselineValidator.prepare(child, "vertical-align", value)
        assert {:ok, resolved} = CssBaselineValidator.resolve(admitted, parent, text)
        assert_in_delta resolved.vertical_align, 6.8 + extra, 0.000000001
      end

      parent = %{parent | dominant_baseline: :alphabetic}
      child = %{child | dominant_baseline: :hanging}
      assert {:ok, resolved} = CssBaselineValidator.resolve(child, parent, text)
      assert resolved.vertical_align == 0
    end
  end
end
