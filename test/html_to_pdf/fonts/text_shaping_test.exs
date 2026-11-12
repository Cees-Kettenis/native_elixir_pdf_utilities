Code.require_file(NativeElixirPdfUtilities.TestSupport.Paths.support("open_type_fixture.ex"))

defmodule NativeElixirPdfUtilities.HtmlToPdf.TextShapingTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.HtmlToPdf.{Font, TextShaper}
  alias NativeElixirPdfUtilities.Limits
  alias NativeElixirPdfUtilities.TestSupport.{OpenTypeFixture, Paths}
  alias NativeElixirPdfUtilities.TestSupport.OpenTypeFixture, as: Fixture

  import NativeElixirPdfUtilities.TestSupport.OpenTypeFixture,
    only: [join: 2, words: 1, coverage: 1]

  setup do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    :ok
  end

  test "physical face admission shares metadata variants and rechecks every edited metric" do
    alias NativeElixirPdfUtilities.Validators.{FontValidator, ResourceBudgetValidator}
    original = Map.put(Fixture.font(), :kerning, %{{1, 2} => -20})

    FontValidator.with_budget(fn ->
      assert FontValidator.validate_embedded_face(original) == :ok
      remaining = ResourceBudgetValidator.remaining(:max_work)

      assert FontValidator.validate_embedded_face(Map.put(original, :shaping_language, "en")) ==
               :ok

      assert ResourceBudgetValidator.remaining(:max_work) == remaining

      for {key, value} <- [
            {:data, nil},
            {:pdf_name, "invalid name"},
            {:units_per_em, 0},
            {:widths, [:bad]},
            {:default_width, -1},
            {:cmap, nil},
            {:ascent, 70_000},
            {:descent, -70_000},
            {:bbox, {0, 0, 0, 70_000}},
            {:kerning, %{{64, 1} => -20}}
          ] do
        assert FontValidator.validate_embedded_face(Map.put(original, key, value)) == :error
      end

      assert FontValidator.validate_embedded_face(original) == :ok
      assert FontValidator.validate_embedded_face(nil) == :error
    end)
  end

  test "shaping consumers propagate selected-face diagnostics and retain plain overflow markers" do
    alias NativeElixirPdfUtilities.HtmlToPdf.{ShapedTextPaint, VerticalText, UnicodeTextData}

    alias NativeElixirPdfUtilities.Validators.{
      FontRenderingValidator,
      FontValidator,
      TextShapingValidator
    }

    original = Fixture.font()
    bad = original |> Map.put(:widths, [:bad]) |> Map.put(:shaping_language, "en")

    box = %{
      type: :text,
      text: "A",
      font_face: bad,
      font_size: 12,
      x: 0,
      y: 20,
      width: 6,
      color: {0, 0, 0}
    }

    assert {:error, {reason, diagnostic}} =
             ShapedTextPaint.prepare([%{size: {100, 100}, boxes: [box]}])

    assert diagnostic.stage == :font and diagnostic.message =~ "metrics"

    assert {:font_resource_limit, {:error, {^reason, propagated}}} =
             catch_throw(FontRenderingValidator.supports_text?(bad, "A"))

    assert propagated.stage == :font and propagated.message =~ "metrics"

    for {text, orientation} <- [{"A", :upright}, {"（", :mixed}] do
      if text == "（", do: assert(elem(UnicodeTextData.properties(0xFF08), 7) == :Tr)

      vertical =
        Map.put(bad, :_vertical_text, %{
          mode: :vertical_rl,
          orientation: orientation,
          combine: :none
        })

      assert {:error, {^reason, diagnostic}} = VerticalText.shape(text, vertical, [])
      assert diagnostic.stage == :font and diagnostic.message =~ "metrics"
    end

    configured =
      original |> Map.merge(%{id: "invalid-configured-layout", data: <<0>>, source: :configured})

    assert {:font_resource_limit, {:error, {_, diagnostic}}} =
             catch_throw(TextShapingValidator.required?(configured, "A"))

    assert diagnostic.stage == :font and is_binary(diagnostic.message)

    marker =
      %{box | font_face: original} |> Map.merge(%{_text_line: make_ref(), _overflow_marker: true})

    page = %{size: {100, 100}, boxes: [marker]}
    assert {:ok, [^page], %{}} = ShapedTextPaint.prepare([page])

    assert FontValidator.with_budget(fn -> TextShapingValidator.required?(original, "A") end) ==
             false
  end

  test "bundled font glyphs and advances match an independent HarfBuzz oracle" do
    {:ok, [font]} =
      Font.load_configured_faces([
        %{family: "Oracle", path: Paths.project("priv/fonts/dejavu/DejaVuSans.ttf")}
      ])

    # Measured with system libharfbuzz against these unchanged bundled bytes.
    # Each row is in logical order: glyph, advance, x offset, y offset.
    for {text, expected} <- [
          {"office", [{82, 1253, 0, 0}, {5044, 1980, 0, 0}, {70, 1126, 0, 0}, {72, 1260, 0, 0}]},
          {"AVATAR",
           [
             {36, 1270, 0, 0},
             {57, 1270, 0, 0},
             {36, 1242, 0, 0},
             {55, 1092, 0, 0},
             {36, 1401, 0, 0},
             {53, 1423, 0, 0}
           ]},
          {"á", [{163, 1255, 0, 0}]},
          {"שָׁלוֹם",
           [
             {1344, 1451, 0, 0},
             {1314, 0, 0, 0},
             {1305, 0, 0, 0},
             {1331, 1164, 0, 0},
             {1324, 558, 0, 0},
             {1306, 0, 0, 0},
             {1332, 1359, 0, 0}
           ]},
          {"بَّ", [{1366, 1928, 0, 0}, {6020, 0, 388, -200}]},
          {"بَّ", [{1366, 1928, 0, 0}, {6020, 0, 388, -200}]},
          {"سلام", [{5293, 1716, 0, 0}, {5366, 1222, 0, 0}, {1390, 1268, 0, 0}]},
          {"بِسْمِ",
           [
             {5259, 570, 0, 0},
             {1401, 0, -213, -350},
             {5294, 1827, 0, 0},
             {1403, 0, 138, -300},
             {5340, 1363, 0, 0},
             {1401, 0, -272, -600}
           ]}
        ] do
      assert {:ok, shaped} = TextShaper.shape(text, font)

      assert Enum.map(shaped.glyphs, &{&1.glyph, &1.x_advance, &1.x_offset, &1.y_offset}) ==
               expected

      assert shaped.text == text
      assert shaped.advance == Enum.sum(Enum.map(expected, &elem(&1, 1)))
    end

    assert {:ok, plain} = TextShaper.shape("office", font, features: %{"liga" => 0})
    assert length(plain.glyphs) == 6
    assert {:ok, no_kern} = TextShaper.shape("AV", font, features: %{"kern" => 0})
    assert Enum.map(no_kern.glyphs, & &1.x_advance) == [1401, 1401]
    assert {:ok, empty} = TextShaper.shape("", font)
    assert empty.glyphs == [] and empty.advance == 0
  end

  test "single, deletion, multiple and alternate lookups retain order and source owners" do
    single = join(fn [covered] -> words([1, covered, 1]) end, [coverage([1])])

    multiple =
      join(fn [covered, sequence] -> words([1, covered, 1, sequence]) end, [
        coverage([2]),
        words([2, 3, 4])
      ])

    alternate =
      join(fn [covered, sequence] -> words([1, covered, 1, sequence]) end, [
        coverage([3]),
        words([2, 5, 6])
      ])

    deletion =
      join(fn [covered, sequence] -> words([1, covered, 1, sequence]) end, [
        coverage([4]),
        words([0])
      ])

    font =
      Fixture.font([
        %{type: 1, subtables: [single]},
        %{type: 2, subtables: [multiple]},
        %{type: 3, subtables: [alternate]},
        %{type: 2, subtables: [deletion]}
      ])

    assert {:ok, admission} =
             NativeElixirPdfUtilities.Validators.OpenTypeAsciiValidator.prepare(font)

    assert MapSet.subset?(MapSet.new([1, 2, 3, 4]), admission.flags["Latn"]["test"])

    assert {:ok, shaped} =
             TextShaper.shape([%{text: "A", owner: :one}], font, features: %{"test" => 2})

    assert Enum.map(shaped.glyphs, & &1.glyph) == [6]
    assert hd(shaped.glyphs).fragments == [%{owner: :one, text: "A", offset: 0}]
    assert {:ok, absent} = TextShaper.shape("B", font, features: %{"test" => 10})
    assert Enum.map(absent.glyphs, & &1.glyph) == [3]
    assert {:ok, untouched} = TextShaper.shape("A", font)
    assert Enum.map(untouched.glyphs, & &1.glyph) == [1]
  end

  test "deleting a leading glyph retains the next stable target and original source cluster" do
    deletion =
      join(
        fn [covered, sequence] -> words([1, covered, 1, sequence]) end,
        [coverage([1]), words([0])]
      )

    font = Fixture.font([%{type: 2, subtables: [deletion]}])
    assert {:ok, shaped} = TextShaper.shape("AB", font, features: %{"test" => 1})
    assert [%{glyph: 2, cluster_start: 1, cluster_end: 2}] = shaped.glyphs
    assert shaped.text == "AB" and length(shaped.clusters) == 2
    assert {:ok, empty} = TextShaper.shape("A", font, features: %{"test" => 1})
    assert empty.glyphs == [] and empty.text == "A" and length(empty.clusters) == 1
  end

  test "font-dependent canonical normalization keeps each contributing scalar's exact owner" do
    font = Fixture.font() |> Map.put(:cmap, %{0x1EAD => 4, ?a => 1, 0x302 => 2, 0x323 => 3})

    assert {:ok, shaped} =
             TextShaper.shape([%{text: "â", owner: :letter}, %{text: "̣", owner: :mark}], font)

    assert [glyph] = shaped.glyphs
    assert glyph.glyph == 4
    assert Enum.map(glyph.fragments, &{&1.owner, &1.text}) == [{:letter, "â"}, {:mark, "̣"}]

    font = %{font | cmap: %{?a => 1, 0x301 => 2}}
    assert {:ok, shaped} = TextShaper.shape([%{text: "á", owner: :one_scalar}], font)
    assert Enum.map(shaped.glyphs, & &1.glyph) == [1, 2]

    assert Enum.map(shaped.glyphs, & &1.fragments) ==
             List.duplicate([%{owner: :one_scalar, text: "á", offset: 0}], 2)

    assert {:ok, repeated} =
             TextShaper.shape(
               [
                 %{text: "á", owner: :composed},
                 %{text: "́", owner: :following}
               ],
               font
             )

    assert Enum.map(repeated.glyphs, & &1.glyph) == [1, 2, 2]

    assert Enum.map(repeated.glyphs, & &1.fragments) == [
             [%{owner: :composed, text: "á", offset: 0}],
             [%{owner: :composed, text: "á", offset: 0}],
             [%{owner: :following, text: "́", offset: 0}]
           ]

    assert shaped.text == "á"
    assert {:ok, shaped} = TextShaper.shape("", font)
    assert NativeElixirPdfUtilities.HtmlToPdf.TextShaper.cluster_boundaries(shaped) == [0]
  end

  test "ligatures preserve the cluster extent and every inline source owner" do
    ligature = words([9, 3, 2, 3])
    set = join(fn [offset] -> words([1, offset]) end, [ligature])
    subtable = join(fn [covered, set] -> words([1, covered, 1, set]) end, [coverage([1]), set])
    font = Fixture.font([%{type: 4, subtables: [subtable]}])
    pieces = [%{text: "A", owner: :left}, %{text: "BC", owner: :right}]
    assert {:ok, shaped} = TextShaper.shape(pieces, font, features: %{"test" => 1})
    assert Enum.map(shaped.glyphs, & &1.glyph) == [9]
    assert hd(shaped.glyphs).cluster_start == 0
    assert hd(shaped.glyphs).cluster_end == 3
    assert Enum.map(hd(shaped.glyphs).fragments, & &1.owner) == [:left, :right, :right]
    assert {:ok, partial} = TextShaper.shape("ABD", font, features: %{"test" => 1})
    assert Enum.map(partial.glyphs, & &1.glyph) == [1, 2, 4]
  end

  test "context actions target original inputs after an earlier multiple substitution" do
    multiple =
      join(fn [covered, sequence] -> words([1, covered, 1, sequence]) end, [
        coverage([1]),
        words([2, 7, 8])
      ])

    single = join(fn [covered] -> words([1, covered, 2]) end, [coverage([2])])

    context =
      join(fn [a, b] -> words([3, 2, 2, a, b, 0, 1, 1, 2]) end, [coverage([1]), coverage([2])])

    font =
      Fixture.font(
        [
          %{type: 5, subtables: [context]},
          %{type: 2, subtables: [multiple]},
          %{type: 1, subtables: [single]}
        ],
        [],
        features: [{"test", [0]}]
      )

    assert {:ok, shaped} = TextShaper.shape("ABC", font, features: %{"test" => 1})
    assert Enum.map(shaped.glyphs, & &1.glyph) == [7, 8, 4, 3]
    assert {:ok, nonmatch} = TextShaper.shape("AC", font, features: %{"test" => 1})
    assert Enum.map(nonmatch.glyphs, & &1.glyph) == [1, 3]
  end

  test "reverse chaining scans from the end, and extension offsets use their own base" do
    reverse =
      join(fn [covered, back, look] -> words([1, covered, 1, back, 1, look, 1, 9]) end, [
        coverage([2]),
        coverage([1]),
        coverage([3])
      ])

    extension = <<1::16, 8::16, 8::32, reverse::binary>>
    font = Fixture.font([%{type: 7, subtables: [extension]}])

    assert {:ok, admission} =
             NativeElixirPdfUtilities.Validators.OpenTypeAsciiValidator.prepare(font)

    assert admission.flags["Latn"]["test"] == MapSet.new([2])
    missing_context = %{font | cmap: %{?B => 2}}

    assert {:ok, admission} =
             NativeElixirPdfUtilities.Validators.OpenTypeAsciiValidator.prepare(missing_context)

    assert admission.flags["Latn"] == %{}
    assert {:ok, shaped} = TextShaper.shape("ABCABC", font, features: %{"test" => 1})
    assert Enum.map(shaped.glyphs, & &1.glyph) == [1, 9, 3, 1, 9, 3]
  end

  test "position values accumulate and pair records control where scanning resumes" do
    single = join(fn [covered] -> words([1, covered, 3, 20, -10]) end, [coverage([1])])
    pair_set = words([1, 1, -50])

    pair =
      join(fn [covered, pair] -> words([1, covered, 4, 0, 1, pair]) end, [coverage([1]), pair_set])

    font = Fixture.font([], [%{type: 1, subtables: [single]}, %{type: 2, subtables: [pair]}])
    assert {:ok, shaped} = TextShaper.shape("AAA", font, features: %{"test" => 1})

    assert Enum.map(shaped.glyphs, &{&1.x_advance, &1.x_offset, &1.y_offset}) == [
             {450, 20, -10},
             {450, 20, -10},
             {500, 20, -10}
           ]

    pair_set = words([1, 1, -50, 10])

    pair =
      join(fn [covered, pair] -> words([1, covered, 4, 1, 1, pair]) end, [coverage([1]), pair_set])

    font = Fixture.font([], [%{type: 2, subtables: [pair]}])
    assert {:ok, shaped} = TextShaper.shape("AAA", font, features: %{"test" => 1})

    assert Enum.map(shaped.glyphs, &{&1.x_advance, &1.x_offset}) == [
             {450, 0},
             {500, 10},
             {500, 0}
           ]
  end

  test "script, language and required features select one language system" do
    single = join(fn [covered] -> words([2, covered, 1, 9]) end, [coverage([1])])

    font =
      Fixture.font([%{type: 1, subtables: [single]}], [],
        script: "latn",
        language: "TRK ",
        required: 0
      )

    assert {:ok, turkish} = TextShaper.shape("A", font, language: "tr", features: %{"test" => 0})
    assert hd(turkish.glyphs).glyph == 9
    assert {:ok, english} = TextShaper.shape("A", font, language: "en", features: %{"test" => 1})
    assert hd(english.glyphs).glyph == 1
    font = Map.put(font, :feature_plan, %{language: "TRK ", features: %{}})
    assert {:ok, overridden} = TextShaper.shape("A", font)
    assert hd(overridden.glyphs).glyph == 9
    assert {:ok, vertical} = TextShaper.shape("AB", font, direction: :ttb)
    assert Enum.map(vertical.glyphs, &{&1.x_advance, &1.y_advance}) == [{0, -1000}, {0, -1000}]
  end

  test "source-character options cannot replace unrelated logical text or bypass its UTF-8 admission" do
    font = Fixture.font()
    characters = [%{displayed: "A", fragments: [%{owner: :first, text: "A", offset: 0}]}]
    assert {:ok, shaped} = TextShaper.shape("A", font, characters: characters)
    assert shaped.text == "A"
    assert hd(shaped.glyphs).fragments == hd(characters).fragments

    for text <- ["B", "AA", "", nil, <<255>>] do
      assert {:error, {:invalid_text, diagnostic}} =
               TextShaper.shape(text, font, characters: characters)

      assert diagnostic.stage == :font
      assert diagnostic.message =~ "source"
    end
  end

  test "HTML language selects localized forms for measurement and painting across inherited and reset spans" do
    alias NativeElixirPdfUtilities.HtmlToPdf.{
      HtmlParser,
      Style,
      FontFallback,
      Layout,
      Pagination,
      PdfWriter
    }

    alias NativeElixirPdfUtilities.Validators.{
      HtmlValidator,
      NativeGlyphSourceValidator,
      WriterValidator
    }

    single = join(fn [covered] -> words([2, covered, 1, 9]) end, [coverage([1])])

    data =
      Fixture.outline_font([%{type: 1, subtables: [single]}], [],
        script: "latn",
        language: "TRK ",
        features: [{"locl", [0]}],
        widths: %{1 => 500, 9 => 700}
      )

    options = [fonts: [%{family: "Localized", data: data}], system_font_discovery: false]

    html =
      "<p lang='tr' style='font-family:Localized'>A<span lang='en'>A</span><span lang='tr'>A</span><span lang=''>A</span></p>"

    HtmlValidator.with_render_budget(fn ->
      assert {:ok, dom} = HtmlParser.parse(html)
      assert {:ok, styled} = Style.compute_detailed(dom, options)

      assert {:ok, resolved} =
               styled |> NativeGlyphSourceValidator.prepare_tree() |> FontFallback.resolve(:error)

      assert {:ok, layout} = Layout.layout(resolved, options)
      assert {:ok, pages} = Pagination.paginate(layout, options)
      assert {:ok, context} = WriterValidator.prepare(pages, options)
      boxes = hd(context.pages).boxes |> Enum.filter(&(&1.type == :text))
      assert Enum.map(boxes, &Map.get(&1.font_face, :shaping_language)) == ["tr", "en", "tr", nil]

      assert Enum.map(boxes, &Map.get(&1, :glyph_ids, [&1.font_face.cmap[?A]])) == [
               [9],
               [1],
               [9],
               [1]
             ]

      assert Enum.map(boxes, &Font.text_width("A", &1.font_face, 10)) == [7.0, 5.0, 7.0, 5.0]
      assert {:ok, pdf} = PdfWriter.render(pages)
      assert {:ok, "AAAA"} = NativeElixirPdfUtilities.Text.extract(pdf, layout: false)
    end)
  end

  test "partial advances use the selected default and orphan selectors keep logical source text" do
    font = %{Fixture.font() | widths: [600], default_width: 700}
    assert {:ok, shaped} = TextShaper.shape("A", font)
    assert [%{glyph: 1, x_advance: 700}] = shaped.glyphs
    assert shaped.advance == 700
    assert {:ok, shaped} = TextShaper.shape("\uFE0FA", Fixture.font())
    assert [%{glyph: 0, ignorable?: true}, %{glyph: 1, ignorable?: false}] = shaped.glyphs
    assert shaped.text == "\uFE0FA" and shaped.advance == 500
  end

  test "cyclic positioning and excessive attachment depth return recoverable font diagnostics" do
    cursive =
      join(
        fn [covered, entry, exit] ->
          words([1, covered, 3, entry, exit, entry, exit, entry, exit])
        end,
        [coverage([1, 2, 3]), words([1, 50, 20]), words([1, 400, 100])]
      )

    left = %{type: 3, flags: 0, subtables: [cursive]}
    right = %{left | flags: 1}
    font = Fixture.font([], [left, right])

    assert {:error, {:invalid_font, diagnostic}} =
             TextShaper.shape("ABC", font, features: %{"test" => 1})

    assert diagnostic.stage == :font and diagnostic.operation == :shape_text
    assert diagnostic.message =~ "cyclic attachment"
    assert diagnostic.source == "GPOS attachment"
    font = Fixture.font([], [right])
    assert {:ok, shaped} = TextShaper.shape("ABC", font, features: %{"test" => 1})
    assert shaped.advance == 1200
    Limits.install(%{Limits.effective() | max_depth: 1})

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             TextShaper.shape("ABC", font, features: %{"test" => 1})

    assert diagnostic.stage == :font and diagnostic.message =~ "max_depth"
  end

  test "edited nominal glyphs, malformed metrics and missing visible output return font diagnostics" do
    alias NativeElixirPdfUtilities.Validators.TextShapingValidator
    font = Fixture.font()
    assert TextShapingValidator.required?(Map.put(font, :shaping_language, 123), "A")

    assert {:error, {:invalid_font, diagnostic}} =
             TextShaper.shape("A", put_in(font, [:cmap, ?A], 64))

    assert diagnostic.stage == :font and diagnostic.operation == :shape_text
    assert diagnostic.message =~ "nominal glyph"
    assert {:error, {:invalid_font, diagnostic}} = TextShaper.shape("A", %{font | widths: [:bad]})
    assert diagnostic.message =~ "valid selected font metrics"
    assert {:ok, uncovered} = TextShaper.shape("a", font)

    assert {:error, {:unsupported_glyph, diagnostic}} =
             TextShapingValidator.validate_visible_glyphs(uncovered.glyphs)

    assert diagnostic.stage == :font and diagnostic.operation == :shape_text
    assert diagnostic.message =~ "uncovered shaping cluster"
  end

  test "invalid inputs and exhausted shaping limits return recoverable diagnostics" do
    font = OpenTypeFixture.font()

    for {text, font, options} <- [
          {"a", nil, []},
          {<<255>>, font, []},
          {"A", font, %{}},
          {"A", font, [bogus: true]},
          {"A", font, [script: "bad"]},
          {"A", font, [direction: :bad]},
          {"A", font, [ppem: 0]},
          {"A", font, [features: nil]},
          {"A", font, [features: %{"bad" => 1}]},
          {"A", font, [features: %{"test" => -1}]}
        ] do
      assert {:error, {_reason, %{stage: :font, message: message}}} =
               TextShaper.shape(text, font, options)

      assert is_binary(message)
    end

    original = Limits.effective()

    for limit <- [:max_work] do
      Limits.install(Map.put(original, limit, 1))
      assert {:error, {:resource_limit_exceeded, %{stage: :font}}} = TextShaper.shape("ABC", font)
    end

    Limits.install(original)
    context = join(fn [covered] -> words([3, 1, 1, covered, 0, 0]) end, [coverage([1])])
    font = Fixture.font([%{type: 5, subtables: [context]}])

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             TextShaper.shape("A", font, features: %{"test" => 1})

    assert diagnostic.message =~ "max_depth"
  end
end
