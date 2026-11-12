Code.require_file(NativeElixirPdfUtilities.TestSupport.Paths.support("open_type_fixture.ex"))
Code.require_file(NativeElixirPdfUtilities.TestSupport.Paths.support("font_fixture.ex"))

defmodule NativeElixirPdfUtilities.HtmlToPdf.OpenTypeLayoutTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.HtmlToPdf.TextShaper
  alias NativeElixirPdfUtilities.Limits
  alias NativeElixirPdfUtilities.TestSupport.{FontFixture, OpenTypeFixture, Paths}
  alias NativeElixirPdfUtilities.Validators.{FontValidator, OpenTypeLayoutValidator}
  alias NativeElixirPdfUtilities.Validators.OpenTypeDataValidator, as: Data

  import NativeElixirPdfUtilities.TestSupport.OpenTypeFixture,
    only: [join: 2, words: 1, coverage: 1]

  setup do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    :ok
  end

  test "operation layout memo checks source bytes even when a face identity is reused" do
    font = OpenTypeFixture.font()

    FontValidator.with_budget(fn ->
      assert {:ok, first} = OpenTypeLayoutValidator.for_font(font)
      assert OpenTypeLayoutValidator.for_font(font) == {:ok, first}

      assert {:error, {:invalid_font, diagnostic}} =
               OpenTypeLayoutValidator.for_font(%{font | data: <<0>>})

      assert diagnostic.stage == :font
      assert OpenTypeLayoutValidator.for_font(font) == {:ok, first}
      assert {:error, {:invalid_font, _}} = OpenTypeLayoutValidator.for_font(nil)
      assert {:ok, ^first} = OpenTypeLayoutValidator.for_font(Map.delete(font, :id))
    end)
  end

  test "every bundled face admits its actual substitution and positioning tables" do
    for path <- Path.wildcard(Paths.project("priv/fonts/**/*.ttf")) do
      assert {:ok, layout} = OpenTypeLayoutValidator.prepare(%{data: File.read!(path)})
      assert layout.glyph_count > 0
      assert is_map(layout.gsub.scripts) and is_tuple(layout.gsub.features)
      assert is_map(layout.gpos.scripts) and is_tuple(layout.gpos.lookups)
    end

    assert {:ok, %{gsub: nil, gpos: nil, gdef: %{store: {}}}} =
             OpenTypeLayoutValidator.prepare(OpenTypeFixture.font())

    assert {:error, {:invalid_font, %{operation: :prepare_opentype_layout}}} =
             OpenTypeLayoutValidator.prepare(nil)
  end

  test "layout records reject unordered tags, invalid references and inconsistent extension types" do
    single = join(fn [covered] -> words([2, covered, 1, 2]) end, [coverage([1])])

    multiple =
      join(fn [covered, sequence] -> words([1, covered, 1, sequence]) end, [
        coverage([1]),
        words([1, 2])
      ])

    extension = fn type, data ->
      join(fn [offset] -> words([1, type]) <> <<offset::32>> end, [data])
    end

    lookup = %{type: 1, subtables: [single]}

    for {lookups, options, message} <- [
          {[%{lookup | type: 0}], [], "lookup type"},
          {[Map.put(lookup, :flags, 16)], [], "mark filtering set"},
          {[%{type: 7, subtables: [extension.(7, single)]}], [], "nested extension"},
          {[%{type: 7, subtables: [words([2, 1]) <> <<8::32>> <> single]}], [],
           "extension lookup format"},
          {[%{type: 7, subtables: [extension.(1, single), extension.(2, multiple)]}], [],
           "same lookup type"},
          {[lookup], [features: [{"test", [1]}]], "unavailable lookup"},
          {[lookup], [required: 1], "unavailable feature"},
          {[lookup], [language: "BAD\0"], "language tags"}
        ] do
      assert {:error, {:invalid_font, diagnostic}} =
               OpenTypeLayoutValidator.prepare(OpenTypeFixture.font(lookups, [], options))

      assert diagnostic.stage == :font and diagnostic.operation == :prepare_opentype_layout
      assert diagnostic.message =~ message
    end

    font = OpenTypeFixture.font([lookup])
    {_, tables} = FontFixture.tables(font.data)
    <<1::16, 0::16, _scripts::16, features::16, lookups::16, _::binary>> = tables["GSUB"]
    system = words([4, 0, 0, 65535, 1, 0])

    for tags <- [["zzzz", "aaaa"], ["aaaa", "aaaa"]] do
      scripts =
        join(
          fn offsets ->
            words([2]) <>
              (Enum.zip(tags, offsets)
               |> Enum.map(fn {tag, offset} -> tag <> words([offset]) end)
               |> IO.iodata_to_binary())
          end,
          [system, system]
        )

      gsub =
        join(fn [s, f, l] -> words([1, 0, s, f, l]) end, [
          scripts,
          binary_part(tables["GSUB"], features, lookups - features),
          binary_part(tables["GSUB"], lookups, byte_size(tables["GSUB"]) - lookups)
        ])

      invalid = %{font | data: FontFixture.rebuild(font.data, %{"GSUB" => gsub})}
      assert {:error, {:invalid_font, diagnostic}} = OpenTypeLayoutValidator.prepare(invalid)
      assert diagnostic.message =~ "tags are unordered or repeated"
    end
  end

  test "coverage ranges, classes, device packing and anchors have independently calculated values" do
    FontValidator.with_budget(fn ->
      data = words([2, 2, 1, 3, 0, 8, 9, 3])
      assert Data.coverage(%{data: data, tag: "test", glyph_count: 20}, 0) == [1, 2, 3, 8, 9]
      context = %{data: words([1, 4, 3, 0, 2, 1]), tag: "test", glyph_count: 20}
      assert Data.classes(context, 0, 3) == %{4 => 0, 5 => 2, 6 => 1}
      context = %{data: words([2, 2, 1, 2, 1, 5, 6, 2]), tag: "test", glyph_count: 20}
      assert Data.classes(context, 0, 3) == %{1 => 1, 2 => 1, 5 => 2, 6 => 2}

      for {format, raw, expected} <- [
            {1, <<1::2, -2::signed-2, -1::signed-2, 0::2, 0::8>>, [1, -2, -1, 0]},
            {2, <<7::4, -8::signed-4, -1::signed-4, 0::4>>, [7, -8, -1, 0]},
            {3, <<127::8, -128::signed-8, -1::signed-8, 0::8>>, [127, -128, -1, 0]}
          ] do
        context = %{data: words([10, 13, format]) <> raw, tag: "test"}
        assert Data.device(context, 0, 0) == nil
        context = %{context | data: <<0::16>> <> context.data}
        assert Data.device(context, 0, 2).deltas == expected
      end

      for {raw, point} <- [
            {words([1, 30, -20]), nil},
            {words([2, 30, -20, 3]), 3},
            {words([3, 30, -20, 0, 0]), nil}
          ] do
        context = %{data: <<0::16>> <> raw, tag: "test"}

        assert Data.anchor(context, 0, 2) == %{
                 x: 30,
                 y: -20,
                 point: point,
                 x_device: nil,
                 y_device: nil
               }

        assert Data.anchor(context, 0, 0) == nil
      end
    end)
  end

  test "GSUB rejects inconsistent formats and counts before constructing substitution maps" do
    mismatched = fn format ->
      join(fn [covered] -> words([format, covered, 0]) end, [coverage([1])])
    end

    empty_alternate =
      join(
        fn [covered, sequence] -> words([1, covered, 1, sequence]) end,
        [coverage([1]), words([0])]
      )

    invalid_ligature = join(fn [rule] -> words([1, rule]) end, [words([2, 0])])

    invalid_ligature =
      join(
        fn [covered, set] -> words([1, covered, 1, set]) end,
        [coverage([1]), invalid_ligature]
      )

    reverse = join(fn [covered] -> words([1, covered, 0, 0, 0]) end, [coverage([1])])

    for {kind, data, expected} <- [
          {1, words([3, 0, 0]), "single substitution format"},
          {1, mismatched.(2), "single substitute count"},
          {2, words([2, 0, 0]), "multiple or alternate substitution format"},
          {2, mismatched.(1), "substitute sequence count"},
          {3, empty_alternate, "alternate set must not be empty"},
          {4, words([2, 0, 0]), "ligature substitution format"},
          {4, mismatched.(1), "ligature set count"},
          {4, invalid_ligature, "ligature must consume"},
          {8, words([2, 0]), "reverse chaining substitution format"},
          {8, reverse, "reverse substitute count"}
        ] do
      font = OpenTypeFixture.font([%{type: kind, subtables: [data]}])
      assert {:error, {:invalid_font, diagnostic}} = OpenTypeLayoutValidator.prepare(font)
      assert diagnostic.stage == :font and diagnostic.source =~ "GSUB byte"
      assert diagnostic.operation == :prepare_opentype_layout and diagnostic.message =~ expected
    end
  end

  test "GPOS rejects malformed value arrays, pairs and attachment matrices before layout" do
    single = join(fn [covered] -> words([2, covered, 0, 0]) end, [coverage([1])])
    cursive = join(fn [covered] -> words([1, covered, 0]) end, [coverage([1])])
    pair = join(fn [covered] -> words([1, covered, 0, 0, 0]) end, [coverage([1])])
    marks = join(fn [anchor] -> words([1, 0, anchor]) end, [words([1, 0, 0])])

    attachment = fn marks, bases ->
      join(
        fn [mark_coverage, base_coverage, marks, bases] ->
          words([1, mark_coverage, base_coverage, 1, marks, bases])
        end,
        [coverage([1]), coverage([2]), marks, bases]
      )
    end

    empty_components = join(fn [record] -> words([1, record]) end, [words([0])])

    for {kind, data, expected} <- [
          {1, words([3, 0, 0]), "single positioning format"},
          {1, single, "position value count"},
          {3, cursive, "cursive anchor count"},
          {2, pair, "pair set count"},
          {2, words([2, 0, 0, 0, 0, 0, 0, 1]), "pair class counts"},
          {2, words([3, 0, 0, 0]), "pair positioning format"},
          {4, words([1, 0, 0, 0, 0, 0]), "invalid attachment format"},
          {4, attachment.(words([0]), words([0])), "mark record count"},
          {4, attachment.(words([1, 1, 0]), words([0])), "invalid mark class"},
          {4, attachment.(marks, words([0])), "attachment target count"},
          {5, attachment.(marks, empty_components), "requires a component"}
        ] do
      font = OpenTypeFixture.font([], [%{type: kind, subtables: [data]}])
      assert {:error, {:invalid_font, diagnostic}} = OpenTypeLayoutValidator.prepare(font)
      assert diagnostic.stage == :font and diagnostic.source =~ "GPOS byte"
      assert diagnostic.operation == :prepare_opentype_layout and diagnostic.message =~ expected
    end
  end

  test "context formats one and two preserve encoded action order and class zero" do
    single = join(fn [covered] -> words([2, covered, 1, 9]) end, [coverage([2])])
    rule = words([2, 1, 2, 1, 1])
    rule_set = join(fn [rule] -> words([1, rule]) end, [rule])
    glyph = join(fn [covered, set] -> words([1, covered, 1, set]) end, [coverage([1]), rule_set])
    classes = words([1, 1, 2, 1, 2])

    class =
      join(fn [covered, classes, set] -> words([2, covered, classes, 2, 0, set]) end, [
        coverage([1]),
        classes,
        rule_set
      ])

    for context <- [glyph, class] do
      font =
        OpenTypeFixture.font(
          [%{type: 5, subtables: [context]}, %{type: 1, subtables: [single]}],
          [],
          features: [{"test", [0]}]
        )

      assert {:ok, admission} =
               NativeElixirPdfUtilities.Validators.OpenTypeAsciiValidator.prepare(font)

      assert MapSet.member?(admission.flags["Latn"]["test"], 1)
      assert {:ok, result} = TextShaper.shape("ABC", font, features: %{"test" => 1})
      assert Enum.map(result.glyphs, & &1.glyph) == [1, 9, 3]
      assert {:ok, result} = TextShaper.shape("ACC", font, features: %{"test" => 1})
      assert Enum.map(result.glyphs, & &1.glyph) == [1, 3, 3]
    end
  end

  test "context actions retain their targets when a nested ligature consumes a target or cursor" do
    ligature =
      join(fn [covered, set] -> words([1, covered, 1, set]) end, [
        coverage([1]),
        join(fn [rule] -> words([1, rule]) end, [words([9, 2, 2])])
      ])

    single = join(fn [covered] -> words([2, covered, 1, 8]) end, [coverage([2])])

    for {count, actions} <- [{1, [0, 1]}, {2, [0, 1, 1, 2]}] do
      context =
        join(
          fn offsets -> words([3, count, div(length(actions), 2)] ++ offsets ++ actions) end,
          Enum.map(1..count, &coverage([&1]))
        )

      font =
        OpenTypeFixture.font(
          [
            %{type: 5, subtables: [context]},
            %{type: 4, subtables: [ligature]},
            %{type: 1, subtables: [single]}
          ],
          [],
          features: [{"test", [0]}]
        )

      assert {:ok, result} = TextShaper.shape("ABC", font, features: %{"test" => 1})
      assert Enum.map(result.glyphs, & &1.glyph) == [9, 3]
      assert hd(result.glyphs).codepoints == [?A, ?B]
      assert List.last(result.glyphs).codepoints == [?C]
      assert result.advance == 1000
    end
  end

  test "ignored marks inside a ligature attach to their original logical components" do
    ligature =
      join(fn [covered, set] -> words([1, covered, 1, set]) end, [
        coverage([1]),
        join(fn [rule] -> words([1, rule]) end, [words([9, 2, 2])])
      ])

    marks = join(fn [anchor] -> words([1, 0, anchor]) end, [words([1, 10, 20])])

    components =
      join(
        fn [one, two] -> words([2, one, two]) end,
        [words([1, 100, 200]), words([1, 300, 400])]
      )

    targets = join(fn [anchors] -> words([1, anchors]) end, [components])

    attachment =
      join(
        fn [mark, base, marks, targets] -> words([1, mark, base, 1, marks, targets]) end,
        [coverage([3]), coverage([9]), marks, targets]
      )

    font =
      OpenTypeFixture.font(
        [%{type: 4, flags: 8, subtables: [ligature]}],
        [%{type: 5, subtables: [attachment]}]
      )
      |> Map.put(:cmap, %{?A => 1, ?B => 2, 0x301 => 3})

    assert {:ok, result} = TextShaper.shape("ÁB́", font, features: %{"test" => 1})

    # Both marks follow the final 500-unit ligature advance. Their anchors
    # remain at component one (100,200) and component two (300,400).
    assert Enum.map(result.glyphs, &{&1.glyph, &1.x_advance, &1.x_offset, &1.y_offset}) == [
             {9, 500, 0, 0},
             {3, 0, -410, 180},
             {3, 0, -210, 380}
           ]

    assert result.text == "ÁB́"
  end

  test "reverse substitutions require both backtrack and lookahead coverage" do
    reverse =
      join(
        fn [covered, back, look] -> words([1, covered, 1, back, 1, look, 1, 8]) end,
        [coverage([2]), coverage([1]), coverage([3])]
      )

    font = OpenTypeFixture.font([%{type: 8, subtables: [reverse]}])

    for {text, expected} <- [{"ABC", [1, 8, 3]}, {"ABD", [1, 2, 4]}, {"DBC", [4, 2, 3]}] do
      assert {:ok, result} = TextShaper.shape(text, font, features: %{"test" => 1})
      assert Enum.map(result.glyphs, & &1.glyph) == expected
    end
  end

  test "all chaining formats match nearest backtrack and lookahead without consuming them" do
    single = join(fn [covered] -> words([2, covered, 1, 9]) end, [coverage([2])])
    rule = words([1, 1, 1, 1, 3, 1, 0, 1])
    rule_set = join(fn [rule] -> words([1, rule]) end, [rule])
    glyph = join(fn [covered, set] -> words([1, covered, 1, set]) end, [coverage([2]), rule_set])
    classes = words([1, 1, 3, 1, 2, 3])

    class =
      join(
        fn [covered, classes, set] ->
          words([2, covered, classes, classes, classes, 3, 0, 0, set])
        end,
        [coverage([2]), classes, rule_set]
      )

    cover =
      join(fn [covered, back, look] -> words([3, 1, back, 1, covered, 1, look, 1, 0, 1]) end, [
        coverage([2]),
        coverage([1]),
        coverage([3])
      ])

    for context <- [glyph, class, cover] do
      font =
        OpenTypeFixture.font(
          [%{type: 6, subtables: [context]}, %{type: 1, subtables: [single]}],
          [],
          features: [{"test", [0]}]
        )

      assert {:ok, admission} =
               NativeElixirPdfUtilities.Validators.OpenTypeAsciiValidator.prepare(font)

      assert MapSet.member?(admission.flags["Latn"]["test"], 2)
      assert {:ok, result} = TextShaper.shape("ABCDBC", font, features: %{"test" => 1})
      assert Enum.map(result.glyphs, & &1.glyph) == [1, 9, 3, 4, 2, 3]
    end
  end

  test "class pair positioning includes zero classes and complete single value arrays" do
    classes = words([1, 1, 2, 1, 1])

    pair =
      join(
        fn [covered, classes] ->
          words([2, covered, 4, 0, classes, classes, 2, 2, 0, -10, -20, -30])
        end,
        [coverage([1, 2]), classes]
      )

    single = join(fn [covered] -> words([2, covered, 1, 2, 20, 40]) end, [coverage([1, 2])])

    font =
      OpenTypeFixture.font([], [%{type: 2, subtables: [pair]}, %{type: 1, subtables: [single]}])

    assert {:ok, result} = TextShaper.shape("ABCA", font, features: %{"test" => 1})

    assert Enum.map(result.glyphs, &{&1.x_advance, &1.x_offset}) == [
             {470, 20},
             {480, 40},
             {500, 0},
             {500, 20}
           ]
  end

  test "cursive attachments align both anchors and honor the lookup's cross-stream flag" do
    cursive =
      join(fn [covered, exit, entry] -> words([1, covered, 2, 0, exit, entry, 0]) end, [
        coverage([1, 2]),
        words([1, 400, 100]),
        words([1, 50, 20])
      ])

    for {flag, offsets} <- [{0, [0, 80]}, {1, [-80, 0]}] do
      font = OpenTypeFixture.font([], [%{type: 3, flags: flag, subtables: [cursive]}])

      assert {:ok, admission} =
               NativeElixirPdfUtilities.Validators.OpenTypeAsciiValidator.prepare(font)

      assert admission.flags["Latn"]["test"] == MapSet.new([1])
      assert {:ok, result} = TextShaper.shape("AB", font, features: %{"test" => 1})
      assert Enum.map(result.glyphs, & &1.x_advance) == [350, 500]
      assert Enum.map(result.glyphs, & &1.y_offset) == offsets
      assert {:ok, unmatched} = TextShaper.shape("AC", font, features: %{"test" => 1})
      assert Enum.map(unmatched.glyphs, & &1.x_advance) == [500, 500]
    end
  end

  test "mark to base, ligature component and mark anchors resolve after final advances" do
    marks = join(fn [anchor] -> words([1, 0, anchor]) end, [words([1, 10, 20])])
    bases = join(fn [anchor] -> words([1, anchor]) end, [words([1, 100, 200])])

    attachments = fn target ->
      join(
        fn [mark_coverage, base_coverage, marks, bases] ->
          words([1, mark_coverage, base_coverage, 1, marks, bases])
        end,
        [coverage([2]), coverage([target]), marks, bases]
      )
    end

    cmap = %{?A => 1, 0x301 => 2, 0x302 => 3}

    font =
      OpenTypeFixture.font([], [%{type: 4, subtables: [attachments.(1)]}]) |> Map.put(:cmap, cmap)

    assert {:ok, result} = TextShaper.shape("Á", font, features: %{"test" => 1})

    assert Enum.map(result.glyphs, &{&1.x_advance, &1.x_offset, &1.y_offset}) == [
             {500, 0, 0},
             {0, -410, 180}
           ]

    # A ligature attachment has one anchor matrix per logical component.
    component_anchors =
      join(fn [one, two] -> words([2, one, two]) end, [words([1, 100, 200]), words([1, 300, 400])])

    ligatures = join(fn [attach] -> words([1, attach]) end, [component_anchors])

    ligature =
      join(
        fn [covered, target, marks, bases] -> words([1, covered, target, 1, marks, bases]) end,
        [coverage([2]), coverage([1]), marks, ligatures]
      )

    font = OpenTypeFixture.font([], [%{type: 5, subtables: [ligature]}]) |> Map.put(:cmap, cmap)
    assert {:ok, result} = TextShaper.shape("Á", font, features: %{"test" => 1})
    assert List.last(result.glyphs).x_offset == -210
    assert List.last(result.glyphs).y_offset == 380

    upper_marks = join(fn [anchor] -> words([1, 0, anchor]) end, [words([1, 5, 10])])

    upper =
      join(
        fn [mark_coverage, base_coverage, marks, bases] ->
          words([1, mark_coverage, base_coverage, 1, marks, bases])
        end,
        [coverage([3]), coverage([2]), upper_marks, bases]
      )

    font =
      OpenTypeFixture.font([], [
        %{type: 4, subtables: [attachments.(1)]},
        %{type: 6, subtables: [upper]}
      ])
      |> Map.put(:cmap, cmap)

    assert {:ok, result} = TextShaper.shape("Á̂", font, features: %{"test" => 1})

    assert Enum.map(result.glyphs, &{&1.x_offset, &1.y_offset}) == [
             {0, 0},
             {-410, 180},
             {-315, 370}
           ]
  end

  test "contextual positioning can invoke single and pair lookups through extensions" do
    single = join(fn [covered] -> words([1, covered, 4, -40]) end, [coverage([2])])
    context = join(fn [covered] -> words([3, 1, 1, covered, 0, 1]) end, [coverage([2])])
    extension = <<1::16, 1::16, 8::32, single::binary>>

    font =
      OpenTypeFixture.font(
        [],
        [%{type: 7, subtables: [context]}, %{type: 9, subtables: [extension]}],
        features: [{"test", [0]}]
      )

    assert {:ok, result} = TextShaper.shape("ABC", font, features: %{"test" => 1})
    assert Enum.map(result.glyphs, & &1.x_advance) == [500, 460, 500]
  end

  test "context lookups reject missing rule sets, empty inputs, invalid actions and unknown formats" do
    empty_rule = join(fn [rule] -> words([1, rule]) end, [words([0, 0])])

    for {kind, subtable, message} <- [
          {5, join(fn [cov] -> words([1, cov, 0]) end, [coverage([1])]), "rule-set count"},
          {5,
           join(fn [cov, classes] -> words([2, cov, classes, 1, 0]) end, [
             coverage([1]),
             words([1, 1, 1, 1])
           ]), "no class rule set"},
          {5, words([3, 0, 0]), "must not be empty"},
          {6, words([3, 0, 0, 0, 0]), "must not be empty"},
          {5, join(fn [cov, rules] -> words([1, cov, 1, rules]) end, [coverage([1]), empty_rule]),
           "must not be empty"},
          {6, join(fn [cov, rules] -> words([1, cov, 1, rules]) end, [coverage([1]), empty_rule]),
           "must not be empty"},
          {5, join(fn [cov] -> words([3, 1, 1, cov, 1, 0]) end, [coverage([1])]),
           "unavailable input or lookup"},
          {5, words([4]), "unsupported contextual lookup format"}
        ] do
      font = OpenTypeFixture.font([%{type: kind, subtables: [subtable]}])
      assert {:error, {:invalid_font, diagnostic}} = OpenTypeLayoutValidator.prepare(font)
      assert diagnostic.stage == :font and diagnostic.operation == :prepare_opentype_layout
      assert diagnostic.source =~ "GSUB byte" and diagnostic.message =~ message
    end
  end

  test "class ranges and anchor formats are admitted before positioning consumes them" do
    bad_classes = words([2, 1, 2, 1, 1])

    pair =
      join(fn [cov, classes] -> words([2, cov, 0, 0, classes, classes, 2, 2]) end, [
        coverage([1]),
        bad_classes
      ])

    cursive =
      join(fn [cov, anchor] -> words([1, cov, 1, anchor, 0]) end, [
        coverage([1]),
        words([4, 0, 0])
      ])

    for {kind, subtable, message} <- [
          {2, pair, "invalid class range"},
          {3, cursive, "unsupported anchor format"}
        ] do
      assert {:error, {:invalid_font, diagnostic}} =
               OpenTypeLayoutValidator.prepare(
                 OpenTypeFixture.font([], [%{type: kind, subtables: [subtable]}])
               )

      assert diagnostic.stage == :font and diagnostic.source =~ "GPOS byte" and
               diagnostic.message =~ message
    end

    assert Data.device_adjustment(%{delta: -25}, 12, 1000) == -25
    assert Data.device_adjustment(%{delta: 10}, nil, 2048) == 10
  end

  test "malformed table fields return shared diagnostics, never pattern-match exceptions" do
    source =
      OpenTypeFixture.font([
        %{type: 1, subtables: [join(fn [covered] -> words([1, covered, 1]) end, [coverage([1])])]}
      ])

    {_, tables} = FontFixture.tables(source.data)
    gsub = tables["GSUB"]

    for {offset, bytes} <- [
          {0, words([2])},
          {2, words([2])},
          {4, words([0])},
          {6, words([65535])},
          {8, words([9])},
          {10, words([65535])},
          {12, "\0bad"}
        ] do
      data =
        FontFixture.rebuild(source.data, %{"GSUB" => FontFixture.replace(gsub, offset, bytes)})

      assert {:error, {:invalid_font, diagnostic}} =
               OpenTypeLayoutValidator.prepare(%{source | data: data})

      assert diagnostic.stage == :font and diagnostic.operation == :prepare_opentype_layout
      assert diagnostic.source =~ "GSUB byte"
    end

    for size <- 0..(byte_size(gsub) - 1) do
      data = FontFixture.rebuild(source.data, %{"GSUB" => binary_part(gsub, 0, size)})

      assert {:error, {:invalid_font, _}} =
               OpenTypeLayoutValidator.prepare(%{source | data: data})
    end

    data = FontFixture.rebuild(source.data, %{"maxp" => nil})

    assert {:error, {:invalid_font, %{source: "maxp byte 0"}}} =
             OpenTypeLayoutValidator.prepare(%{source | data: data})

    data = FontFixture.rebuild(source.data, %{"maxp" => words([1, 0, 0])})

    assert {:error, {:invalid_font, %{source: "maxp byte 4"}}} =
             OpenTypeLayoutValidator.prepare(%{source | data: data})
  end

  test "variation sequence tables reject conflicting sources, overlapping ranges and surrogate mappings" do
    original = OpenTypeFixture.font()

    variation = fn defaults, nondefaults ->
      size = 21 + byte_size(defaults || <<>>) + byte_size(nondefaults || <<>>)

      join(
        fn [default, nondefault] ->
          <<14::16, size::32, 1::32, 0xFE0F::24, default::32, nondefault::32>>
        end,
        [defaults, nondefaults]
      )
    end

    empty_nondefaults = variation.(<<1::32, ?A::24, 0::8>>, nil)
    cmap = words([0, 1, 0, 5]) <> <<12::32>> <> empty_nondefaults

    assert {:ok, layout} =
             OpenTypeLayoutValidator.prepare(%{
               original
               | data: FontFixture.rebuild(original.data, %{"cmap" => cmap})
             })

    assert layout.sequences[0xFE0F].nondefaults == %{}
    assert layout.sequences[0xFE0F].defaults == MapSet.new([?A])

    empty = variation.(nil, nil)

    conflict =
      words([0, 2, 0, 5]) <>
        <<20::32>> <> words([0, 5]) <> <<20 + byte_size(empty)::32>> <> empty <> empty

    repeated_range = variation.(<<2::32, ?A::24, 0::8, ?A::24, 0::8>>, nil)
    surrogate = variation.(nil, <<1::32, 0xD800::24, 1::16>>)

    for {cmap, message} <- [
          {conflict, "conflicting Unicode variation sequence tables"},
          {words([0, 1, 0, 5]) <> <<12::32>> <> repeated_range,
           "invalid default variation range"},
          {words([0, 1, 0, 5]) <> <<12::32>> <> surrogate, "not a Unicode scalar"}
        ] do
      assert {:error, {:invalid_font, diagnostic}} =
               OpenTypeLayoutValidator.prepare(%{
                 original
                 | data: FontFixture.rebuild(original.data, %{"cmap" => cmap})
               })

      assert diagnostic.stage == :font and diagnostic.source =~ "cmap byte" and
               diagnostic.message =~ message
    end
  end

  test "position devices retain signed pixel adjustments and reject unavailable variation deltas" do
    FontValidator.with_budget(fn ->
      context = %{data: words([0, 10, 10, 1, 0x4000]), tag: "GPOS", glyph_count: 64}
      {value, finish} = Data.value(context, 0, 0, 0)
      assert value.devices == %{} and finish == 0
      context = %{context | data: words([2, 10, 10, 1, 0x4000])}
      {value, 2} = Data.value(context, 0, 0, 0x10)
      assert value.devices.x_placement.deltas == [1]
      assert Data.device_adjustment(value.devices.x_placement, 10, 1000) == 100
      assert Data.device_adjustment(value.devices.x_placement, 9, 1000) == 0
      context = %{data: words([0, 0, 0, 0x8000]), tag: "GPOS", gdef: %{store: {}}}

      assert {:invalid_open_type, {:error, {:invalid_font, diagnostic}}} =
               catch_throw(Data.device(context, 0, 2))

      assert diagnostic.stage == :font and
               diagnostic.message =~ "unavailable item variation delta"
    end)
  end

  test "variation selectors admit default and nondefault glyphs without storing extra fonts" do
    defaults = <<1::32, ?B::24, 0::8>>
    nondefaults = <<1::32, ?A::24, 7::16>>
    size = 21 + byte_size(defaults) + byte_size(nondefaults)

    variation =
      join(
        fn [default, nondefault] ->
          <<14::16, size::32, 1::32, 0xFE0F::24, default::32, nondefault::32>>
        end,
        [defaults, nondefaults]
      )

    cmap = words([0, 1, 0, 5]) <> <<12::32, variation::binary>>
    original = OpenTypeFixture.font()
    font = %{original | data: FontFixture.rebuild(original.data, %{"cmap" => cmap})}
    assert {:ok, layout} = OpenTypeLayoutValidator.prepare(font)
    assert layout.sequences[0xFE0F].nondefaults == %{?A => 7}
    assert MapSet.member?(layout.sequences[0xFE0F].defaults, ?B)
    assert {:ok, result} = TextShaper.shape("A️B️C️", font)
    assert Enum.map(result.glyphs, & &1.glyph) == [7, 2, 3]
    assert Enum.map(result.glyphs, & &1.codepoints) == [[?A, 0xFE0F], [?B, 0xFE0F], [?C, 0xFE0F]]
    assert NativeElixirPdfUtilities.HtmlToPdf.Font.supports_text?(font, "A️")
    font = %{font | cmap: Map.delete(font.cmap, ?A)}
    assert NativeElixirPdfUtilities.Validators.FontRenderingValidator.supports_text?(font, "A️")

    for {offset, bytes} <- [
          {14, <<20::32>>},
          {18, <<99::32>>},
          {22, <<0x110000::24>>},
          {25, <<1::32>>},
          {29, <<1::32>>},
          {37, <<?A::24>>},
          {48, words([64])}
        ] do
      data =
        FontFixture.rebuild(original.data, %{"cmap" => FontFixture.replace(cmap, offset, bytes)})

      assert {:error, {:invalid_font, %{source: source}}} =
               OpenTypeLayoutValidator.prepare(%{original | data: data})

      assert source =~ "cmap"
    end
  end

  test "ZWJ clusters choose complete substitution coverage and retain supplementary sources" do
    rule = words([9, 3, 0, 2])
    set = join(fn [rule] -> words([1, rule]) end, [rule])
    ligature = join(fn [covered, set] -> words([1, covered, 1, set]) end, [coverage([1]), set])

    font =
      OpenTypeFixture.font([%{type: 4, subtables: [ligature]}])
      |> Map.put(:cmap, %{0x1F468 => 1, 0x1F466 => 2})
      |> Map.put(:feature_plan, %{features: %{"test" => 1}, language: :normal})

    assert {:ok, result} =
             TextShaper.shape([%{text: "👨", owner: :parent}, %{text: "‍👦", owner: :child}], font)

    assert Enum.map(result.glyphs, & &1.glyph) == [9]
    assert Enum.map(hd(result.glyphs).fragments, & &1.owner) == [:parent, :child, :child]
    assert NativeElixirPdfUtilities.Validators.FontRenderingValidator.supports_text?(font, "👨‍👦")

    refute NativeElixirPdfUtilities.Validators.FontRenderingValidator.supports_text?(
             %{font | cmap: Map.delete(font.cmap, 0x1F466)},
             "👨‍👦"
           )
  end
end
