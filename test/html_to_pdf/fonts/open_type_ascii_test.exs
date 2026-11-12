Code.require_file(NativeElixirPdfUtilities.TestSupport.Paths.support("open_type_fixture.ex"))

defmodule NativeElixirPdfUtilities.HtmlToPdf.OpenTypeAsciiTest do
  use ExUnit.Case, async: true
  alias NativeElixirPdfUtilities.{HtmlToPdf, Text}
  alias NativeElixirPdfUtilities.HtmlToPdf.{Font, TextShaper}
  alias NativeElixirPdfUtilities.TestSupport.{FontFixture, OpenTypeFixture}
  alias NativeElixirPdfUtilities.Validators.{OpenTypeAsciiValidator, TextShapingValidator}

  defp pair_table(sets, second_format \\ 0) do
    records =
      Enum.map(sets, fn {_first, pairs} ->
        OpenTypeFixture.words(
          [length(pairs)] ++
            Enum.flat_map(pairs, fn {second, delta} ->
              [second, delta] ++ if(second_format == 0, do: [], else: [0])
            end)
        )
      end)

    OpenTypeFixture.join(
      fn [covered | offsets] ->
        OpenTypeFixture.words([1, covered, 4, second_format, length(sets)] ++ offsets)
      end,
      [OpenTypeFixture.coverage(Enum.map(sets, &elem(&1, 0))) | records]
    )
  end

  test "default composition, localization and required ASCII features reach measurement and painting" do
    single =
      OpenTypeFixture.join(
        fn [covered] -> OpenTypeFixture.words([2, covered, 1, 9]) end,
        [OpenTypeFixture.coverage([1])]
      )

    for {tag, required} <- [{"ccmp", 65535}, {"locl", 65535}, {"test", 0}] do
      data =
        OpenTypeFixture.outline_font([%{type: 1, subtables: [single]}], [],
          features: [{tag, [0]}],
          required: required,
          widths: %{9 => 900}
        )

      assert {:ok, [font]} = Font.load_configured_faces([%{family: "ASCII #{tag}", data: data}])
      assert TextShapingValidator.required?(font, "A")
      refute TextShapingValidator.required?(font, "B")
      assert Font.text_width("A", font, 12) == 10.8
      assert Font.text_width("B", font, 12) == 6.0
      assert {:ok, shaped} = TextShaper.shape("A", font)
      assert hd(shaped.glyphs).glyph == 9
      assert {:ok, info} = OpenTypeAsciiValidator.prepare(font)
      key = if required == 0, do: {:required, tag}, else: tag
      assert info.flags["Latn"][key] == MapSet.new([1])

      disabled = Map.put(font, :feature_plan, %{features: %{tag => 0}})
      assert TextShapingValidator.required?(disabled, "A") == (required == 0)
      assert Font.text_width("A", disabled, 12) == if(required == 0, do: 10.8, else: 6.0)
      html = "<p style='font-family:Authored;font-size:12pt'>AB</p>"

      assert {:ok, pdf} =
               HtmlToPdf.render(html,
                 system_font_discovery: false,
                 fonts: [%{family: "Authored", data: data}]
               )

      assert {:ok, "AB"} = Text.extract(pdf, layout: false)
    end
  end

  test "language-specific ASCII rules and overrides retain independent admission proofs" do
    single =
      OpenTypeFixture.join(
        fn [covered] -> OpenTypeFixture.words([2, covered, 1, 9]) end,
        [OpenTypeFixture.coverage([1])]
      )

    data =
      OpenTypeFixture.outline_font([%{type: 1, subtables: [single]}], [],
        features: [{"locl", [0]}],
        language: "TRK ",
        widths: %{9 => 900}
      )

    assert {:ok, [font]} = Font.load_configured_faces([%{family: "ASCII language", data: data}])
    alias NativeElixirPdfUtilities.Validators.FontValidator

    FontValidator.with_budget(fn ->
      english = Map.put(font, :shaping_language, "en")
      turkish = Map.put(font, :shaping_language, "tr")
      refute TextShapingValidator.required?(english, "AB")
      assert TextShapingValidator.required?(turkish, "AB")
      upper = Map.put(font, :shaping_language, "TR")
      assert TextShapingValidator.required?(upper, "AB")
      assert_in_delta Font.text_width("AB", upper, 12), 16.8, 1.0e-8

      refute TextShapingValidator.required?(turkish, "B")
      assert_in_delta Font.text_width("AB", english, 12), 12.0, 1.0e-8
      assert_in_delta Font.text_width("AB", turkish, 12), 16.8, 1.0e-8
      override = Map.put(english, :feature_plan, %{language: "TRK ", features: %{}})
      assert TextShapingValidator.required?(override, "AB")
      assert_in_delta Font.text_width("AB", override, 12), 16.8, 1.0e-8
      disabled = Map.put(override, :feature_plan, %{language: "TRK ", features: %{"locl" => 0}})
      refute TextShapingValidator.required?(disabled, "AB")
      assert_in_delta Font.text_width("AB", disabled, 12), 12.0, 1.0e-8
      refute TextShapingValidator.required?(english, "AB")
    end)
  end

  test "ASCII ligatures outside presentation forms and GPOS-only kerning use the shared shaper" do
    set =
      OpenTypeFixture.join(
        fn [offset] -> OpenTypeFixture.words([1, offset]) end,
        [OpenTypeFixture.words([9, 2, 2])]
      )

    ligature =
      OpenTypeFixture.join(
        fn [covered, set] -> OpenTypeFixture.words([1, covered, 1, set]) end,
        [OpenTypeFixture.coverage([1]), set]
      )

    pair =
      OpenTypeFixture.join(
        fn [covered, set] -> OpenTypeFixture.words([1, covered, 4, 0, 1, set]) end,
        [OpenTypeFixture.coverage([1]), OpenTypeFixture.words([1, 2, 0xFF9C])]
      )

    for {substitutions, positions, options} <- [
          {[%{type: 4, subtables: [ligature]}], [],
           [features: [{"liga", [0]}], widths: %{9 => 900}]},
          {[], [%{type: 2, subtables: [pair]}], [gpos_features: [{"kern", [0]}]]}
        ] do
      data =
        OpenTypeFixture.outline_font(substitutions, positions, options)
        |> FontFixture.rebuild(%{"kern" => nil})

      assert {:ok, [font]} = Font.load_configured_faces([%{family: "ASCII default", data: data}])
      assert TextShapingValidator.required?(font, "AB")
      assert Font.text_width("AB", font, 12) == 10.8
      disabled = Map.put(font, :feature_plan, %{features: %{"kern" => 0, "liga" => 0}})
      refute TextShapingValidator.required?(disabled, "AB")
      assert Font.text_width("AB", disabled, 12) == 12

      NativeElixirPdfUtilities.Validators.FontValidator.with_budget(fn ->
        # Projection reuse must distinguish active features and still reject
        # an edited physical model when its caller retains the old font ID.
        refute TextShapingValidator.required?(disabled, "AB")
        assert TextShapingValidator.required?(font, "AB")
        refute TextShapingValidator.required?(font, "Z")
        refute TextShapingValidator.required?(disabled, "AB")
        edited = %{disabled | cmap: Map.put(disabled.cmap, ?A, 2)}
        assert TextShapingValidator.required?(edited, "AB")
      end)
    end
  end

  test "kerning equivalence combines lookups, omitted pairs and duplicate feature records" do
    ab = %{type: 2, subtables: [pair_table([{1, [{2, -50}]}])]}
    cd = %{type: 2, subtables: [pair_table([{3, [{4, -100}]}])]}

    for {lookups, features, legacy, expected, general?} <- [
          {[ab, cd], [{"kern", [0, 1]}], %{{1, 2} => -50, {3, 4} => -100}, 10.8 + 0.6, false},
          {[ab, ab], [{"kern", [0, 1]}], %{{1, 2} => -100}, 10.8, false},
          {[ab, ab], [{"kern", [0]}, {"kern", [1]}], %{{1, 2} => -50}, 10.8, true},
          {[cd], [{"kern", [0]}], %{{1, 2} => -100}, 12.0, true},
          {[ab], [{"kern", []}], %{{1, 2} => -100}, 10.8, false}
        ] do
      data = OpenTypeFixture.outline_font([], lookups, gpos_features: features)

      assert {:ok, [font]} =
               Font.load_configured_faces([
                 %{family: "Combined kern #{inspect(features)} #{inspect(legacy)}", data: data}
               ])

      font = Map.put(font, :kerning, legacy)
      assert {:ok, info} = OpenTypeAsciiValidator.prepare(font)
      assert MapSet.member?(Map.get(info.flags["Latn"], "kern", MapSet.new()), 1) == general?
      assert TextShapingValidator.required?(font, "AB") == general?
      assert_in_delta Font.text_width("AB", font, 12), expected, 1.0e-8
    end
  end

  test "IgnoreMarks kerning retains the legacy plan only for nonmark ASCII glyphs" do
    pair = pair_table([{1, [{2, -50}]}])

    data =
      OpenTypeFixture.outline_font([], [%{type: 2, flags: 8, subtables: [pair]}],
        gpos_features: [{"kern", [0]}]
      )

    assert {:ok, [font]} =
             Font.load_configured_faces([%{family: "ASCII IgnoreMarks", data: data}])

    font = Map.put(font, :kerning, %{{1, 2} => -50})
    refute TextShapingValidator.required?(font, "AB")
    assert_in_delta Font.text_width("AB", font, 12), 11.4, 1.0e-8
    assert {:ok, shaped} = TextShaper.shape("AB", font)
    assert shaped.advance == 950

    classes = OpenTypeFixture.words([1, 1, 1, 3])

    gdef =
      OpenTypeFixture.join(fn [classes] -> OpenTypeFixture.words([1, 0, classes, 0, 0, 0]) end, [
        classes
      ])

    marked =
      OpenTypeFixture.outline_font([], [%{type: 2, flags: 8, subtables: [pair]}],
        gpos_features: [{"kern", [0]}],
        gdef: gdef
      )

    assert {:ok, [font]} =
             Font.load_configured_faces([%{family: "ASCII marked base", data: marked}])

    font = Map.put(font, :kerning, %{{1, 2} => -50})
    assert TextShapingValidator.required?(font, "AB")
    assert_in_delta Font.text_width("AB", font, 12), 12.0, 1.0e-8
  end

  test "a presentation ligature feeding a later lookup retains its ASCII trigger" do
    set =
      OpenTypeFixture.join(
        fn [rule] -> OpenTypeFixture.words([1, rule]) end,
        [OpenTypeFixture.words([3, 2, 2])]
      )

    ligature =
      OpenTypeFixture.join(
        fn [covered, set] -> OpenTypeFixture.words([1, covered, 1, set]) end,
        [OpenTypeFixture.coverage([1]), set]
      )

    single =
      OpenTypeFixture.join(
        fn [covered] -> OpenTypeFixture.words([2, covered, 1, 9]) end,
        [OpenTypeFixture.coverage([3])]
      )

    data =
      OpenTypeFixture.outline_font(
        [
          %{type: 4, subtables: [ligature]},
          %{type: 1, subtables: [single]}
        ],
        [],
        features: [{"clig", [1]}, {"liga", [0]}],
        cmap: %{?f => 1, ?i => 2, 0xFB01 => 3},
        widths: %{9 => 900}
      )

    assert {:ok, [font]} =
             Font.load_configured_faces([%{family: "ASCII chained ligature", data: data}])

    assert font.latin_ligatures == %{"fi" => 0xFB01}
    assert {:ok, admission} = OpenTypeAsciiValidator.prepare(font)
    assert MapSet.member?(admission.flags["Latn"]["clig"], 1)
    assert TextShapingValidator.required?(font, "fi")
    assert_in_delta Font.text_width("fi", font, 12), 10.8, 1.0e-8
  end

  test "a required kern feature remains active when the caller disables optional kerning" do
    data =
      OpenTypeFixture.outline_font([], [%{type: 2, subtables: [pair_table([{1, [{2, -50}]}])]}],
        gpos_features: [{"kern", [0]}],
        required: 0
      )

    assert {:ok, [font]} =
             Font.load_configured_faces([%{family: "Required ASCII kern", data: data}])

    font =
      font
      |> Map.put(:kerning, %{{1, 2} => -50})
      |> Map.put(:feature_plan, %{features: %{"kern" => 0}})

    assert TextShapingValidator.required?(font, "AB")
    assert_in_delta Font.text_width("AB", font, 12), 11.4, 1.0e-8
  end

  test "second value formats and lookup flags cannot inherit a legacy kerning proof" do
    table = pair_table([{1, [{2, -50}]}, {2, [{3, -50}]}], 4)
    ignored = pair_table([{1, [{2, -100}]}])

    for {lookup, text, legacy, width} <- [
          {%{type: 2, subtables: [table]}, "ABC", %{{1, 2} => -50, {2, 3} => -50}, 17.4},
          {%{type: 2, flags: 2, subtables: [ignored]}, "AB", %{{1, 2} => -100}, 12.0}
        ] do
      data = OpenTypeFixture.outline_font([], [lookup], gpos_features: [{"kern", [0]}])

      assert {:ok, [font]} =
               Font.load_configured_faces([%{family: "Kern scan semantics", data: data}])

      font = Map.put(font, :kerning, legacy)
      assert TextShapingValidator.required?(font, text)
      assert_in_delta Font.text_width(text, font, 12), width, 1.0e-8
    end
  end

  test "a malformed edited face cannot poison a valid face's cached layout analysis" do
    alias NativeElixirPdfUtilities.HtmlToPdf.FontCache
    alias NativeElixirPdfUtilities.Validators.FontValidator
    data = OpenTypeFixture.outline_font([], [])

    assert {:ok, [font]} =
             Font.load_configured_faces([%{family: "Fingerprint recovery", data: data}])

    FontValidator.with_budget(fn ->
      assert {:error, {:invalid_font, _}} = FontCache.ascii_layout(%{font | data: <<"OTTO", 0>>})
      assert Font.text_width("A", font, 12) == 6
    end)

    assert {:ok, %{compatible?: true}} = FontCache.ascii_layout(font)
    assert Font.text_width("A", font, 12) == 6
  end
end
