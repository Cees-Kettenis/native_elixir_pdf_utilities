Code.require_file(NativeElixirPdfUtilities.TestSupport.Paths.support("open_type_fixture.ex"))

defmodule NativeElixirPdfUtilities.HtmlToPdf.TextEmphasisTest do
  use ExUnit.Case, async: true
  alias NativeElixirPdfUtilities.{HtmlToPdf, Text}
  alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, Layout, Style}
  alias NativeElixirPdfUtilities.Validators.CssTextEmphasisValidator

  test "emphasis painting preserves a source glyph-plan failure after mark metrics succeed" do
    assert {:ok, dom} = HtmlParser.parse(~s|<p style="text-emphasis:'*'">A</p>|)

    assert {:ok, %{children: [paragraph]} = styled} =
             Style.compute_detailed(dom, system_font_discovery: false)

    assert {:ok, layout} = Layout.layout(styled)
    box = Enum.find(layout.boxes, &(Map.get(&1, :text) == "A"))
    box = Map.update!(box, :font_face, &Map.put(&1, :widths, [:bad]))

    assert {:render_resource_limit, {:error, {reason, diagnostic}}} =
             catch_throw(
               NativeElixirPdfUtilities.Validators.FontValidator.with_budget(fn ->
                 NativeElixirPdfUtilities.HtmlToPdf.TextEmphasis.boxes(box, paragraph.style, %{})
               end)
             )

    assert is_atom(reason) and diagnostic.stage == :font
    assert diagnostic.message =~ "metrics"
  end

  test "emphasis adds ruby forms while preserving explicit feature-setting precedence" do
    alias NativeElixirPdfUtilities.TestSupport.OpenTypeFixture, as: Fixture

    single =
      Fixture.join(fn [coverage] -> Fixture.words([2, coverage, 1, 8]) end, [
        Fixture.coverage([2])
      ])

    data = Fixture.outline_font([%{type: 1, subtables: [single]}], [], features: [{"ruby", [0]}])

    for {declaration, expected} <- [
          {"", 8},
          {"font-variant-east-asian:full-width", 8},
          {"font-feature-settings:'ruby' 0", 2}
        ] do
      html = "<p style=\"font:18pt/36pt Ruby;text-emphasis-style:'B';#{declaration}\">ABC</p>"
      assert {:ok, dom} = HtmlParser.parse(html)

      assert {:ok, styled} =
               Style.compute_detailed(dom,
                 fonts: [%{family: "Ruby", data: data}],
                 system_font_discovery: false
               )

      NativeElixirPdfUtilities.Validators.FontValidator.with_budget(fn ->
        mark = NativeElixirPdfUtilities.HtmlToPdf.TextEmphasis.metrics(hd(styled.children).style)

        assert {:ok, shaped} =
                 NativeElixirPdfUtilities.HtmlToPdf.Font.css_glyph_positions(
                   mark.text,
                   mark.font,
                   mark.size
                 )

        assert Enum.map(shaped.glyphs, & &1.glyph) == [expected]
      end)
    end
  end

  test "blank custom marks retain empty ink and missing typographic tables use the selected face metrics" do
    alias NativeElixirPdfUtilities.TestSupport.{OpenTypeFixture, FontFixture}

    data =
      OpenTypeFixture.outline_font([], [],
        cmap: Map.put(Map.new(Enum.with_index(?A..?Z, 1)), 32, 63)
      )
      |> FontFixture.rebuild(%{"OS/2" => nil})

    assert {:ok, dom} =
             HtmlParser.parse("<p style=\"font:18pt/36pt Bare;text-emphasis-style:' '\">ABC</p>")

    assert {:ok, styled} =
             Style.compute_detailed(dom,
               fonts: [%{family: "Bare", data: data}],
               system_font_discovery: false
             )

    style = hd(styled.children).style

    assert {:error, {_, diagnostic}} =
             HtmlToPdf.render("<p style=\"text-emphasis-style:'🫨'\">A</p>",
               system_font_discovery: false
             )

    assert diagnostic.stage == :font and diagnostic.operation == :shape_text
    assert diagnostic.message =~ "uncovered shaping cluster"

    NativeElixirPdfUtilities.Validators.FontValidator.with_budget(fn ->
      mark = NativeElixirPdfUtilities.HtmlToPdf.TextEmphasis.metrics(style)
      assert mark.size == 9 and mark.ascent > 18 and mark.center == 0
    end)
  end

  test "orthogonal emphasis retains physical sides and keeps vertical marks upright" do
    # CSS Text Decoration 3, 5 May 2022, sections 3.1 and 3.4:
    # https://www.w3.org/TR/2022/CRD-css-text-decor-3-20220505/#text-emphasis-position-property
    for {mode, side, over?} <- [
          {:vertical_rl, :right, true},
          {:vertical_rl, :left, false},
          {:vertical_lr, :right, false},
          {:vertical_lr, :left, true}
        ] do
      html =
        "<p style='writing-mode:#{String.replace(Atom.to_string(mode), "_", "-")};font:18pt/48pt sans-serif;text-emphasis-style:filled dot;text-emphasis-position:over #{side};width:48pt;height:80pt;margin:0'>AB</p>"

      assert {:ok, dom} = HtmlParser.parse(html)
      assert {:ok, styled} = Style.compute_detailed(dom, system_font_discovery: false)
      assert {:ok, layout} = Layout.layout(styled, margin: 0)
      [plane] = Enum.filter(layout.boxes, &(Map.get(&1, :role) == :writing_mode))
      [marks] = Enum.filter(plane.boxes, &(Map.get(&1, :role) == :text_emphasis))
      assert length(marks.boxes) == 2

      for mark <- marks.boxes do
        assert mark.text_baseline_offset > 0 == over?

        assert mark.font_face._vertical_text == %{
                 mode: mode,
                 orientation: :upright,
                 combine: :none,
                 emphasis: true
               }

        assert {:ok, plan} =
                 NativeElixirPdfUtilities.HtmlToPdf.Font.css_glyph_positions(
                   mark.text,
                   mark.font_face,
                   mark.font_size
                 )

        assert hd(plan.glyphs).glyph_basis ==
                 if(mode == :vertical_lr, do: {0, -1, -1, 0}, else: {0, 1, -1, 0})

        assert_in_delta hd(plan.glyphs).y_offset,
                        if(mode == :vertical_lr,
                          do: mark.font_size / 2,
                          else: -mark.font_size / 2
                        ),
                        0.0001
      end
    end
  end

  test "custom grapheme marks center their shaped composition rather than nominal component bounds" do
    assert {:ok, dom} =
             HtmlParser.parse(
               "<p style='font:18pt/36pt sans-serif;text-emphasis-style:\"Á\"'>ABC</p>"
             )

    assert {:ok, styled} = Style.compute_detailed(dom, system_font_discovery: false)
    style = hd(styled.children).style

    NativeElixirPdfUtilities.Validators.FontValidator.with_budget(fn ->
      mark = NativeElixirPdfUtilities.HtmlToPdf.TextEmphasis.metrics(style)
      assert {:ok, bounds} = NativeElixirPdfUtilities.HtmlToPdf.Font.glyph_bounds(mark.font)
      # DejaVu's ccmp selection composes A + acute into the Aacute glyph.
      {left, _, right, _} = elem(bounds, mark.font.cmap[0xC1])
      assert_in_delta mark.center, (left + right) / 2 * 9 / mark.font.units_per_em, 0.00001
    end)
  end

  test "emphasis grammar resets style and color while preserving independent position" do
    assert {:ok, style} =
             CssTextEmphasisValidator.prepare(
               %{text_emphasis_position: {:under, :left}},
               "text-emphasis",
               "open triangle red"
             )

    assert style.text_emphasis_style == {:open, :triangle}
    assert style.text_emphasis_color == {1, 0, 0}
    assert style.text_emphasis_position == {:under, :left}

    assert {:ok, %{text_emphasis_style: {:custom, "a"}}} =
             CssTextEmphasisValidator.prepare(%{}, "text-emphasis-style", "\"ab\"")

    for value <- ["filled open", "dot circle", "none dot", "unknown"] do
      assert :error = CssTextEmphasisValidator.prepare(%{}, "text-emphasis-style", value)
    end

    assert {:ok, %{text_emphasis_position: {:under, :left}}} =
             CssTextEmphasisValidator.prepare(%{}, "text-emphasis-position", "left under")

    assert :error = CssTextEmphasisValidator.prepare(%{}, "text-emphasis-position", "left right")
    assert :ok = CssTextEmphasisValidator.validate_style(style, "p")

    assert {:error, {:invalid_layout, diagnostic}} =
             CssTextEmphasisValidator.validate_style(
               %{text_emphasis_style: {:filled, :unknown}},
               "p"
             )

    assert diagnostic.stage == :layout
    for cp <- [?A, ?#, ?%, ?&, ?@], do: assert(CssTextEmphasisValidator.receives?(cp))
    for cp <- [32, 10, ?,, ?., 0x1361, 0x200D], do: refute(CssTextEmphasisValidator.receives?(cp))
  end

  test "keyword and custom emphasis marks share the prepared-style grammar" do
    assert "text-emphasis" in CssTextEmphasisValidator.properties()

    for {value, expected} <- [
          {"none", :none},
          {"filled", {:filled, :auto}},
          {"double-circle", {:filled, :double_circle}},
          {"sesame", {:filled, :sesame}}
        ] do
      assert {:ok, style} = CssTextEmphasisValidator.prepare(%{}, "text-emphasis-style", value)
      assert style.text_emphasis_style == expected
      assert :ok = CssTextEmphasisValidator.validate_style(style, "span")
    end

    assert :error = CssTextEmphasisValidator.prepare(%{}, "unknown", "filled")

    assert :ok =
             CssTextEmphasisValidator.validate_style(
               %{text_emphasis_style: {:custom, "Á"}},
               "span"
             )

    for style <- [
          %{text_emphasis_style: {:custom, "AB"}},
          %{text_emphasis_style: :bad},
          %{text_emphasis_position: :bad}
        ] do
      assert {:error, {:invalid_layout, diagnostic}} =
               CssTextEmphasisValidator.validate_style(style, "span")

      assert diagnostic.stage == :layout and diagnostic.source == "span"
    end
  end

  test "hand-styled built-in PDF fonts diagnose unavailable emphasis metrics" do
    assert {:error, {:invalid_document, diagnostic}} =
             CssTextEmphasisValidator.validate_style(
               %{
                 font_face: %{type: :built_in},
                 font_size: 12,
                 text_emphasis_style: {:filled, :dot}
               },
               "span"
             )

    assert diagnostic.stage == :font and diagnostic.source == "span"
    assert diagnostic.message =~ "embedded font"
  end

  test "marks follow graphemes across ligatures and inline splits without entering extraction" do
    html =
      "<p style='font:18pt/32pt sans-serif;text-emphasis:filled dot blue'>ffi<span>Á B.</span></p>"

    assert {:ok, dom} = HtmlParser.parse(html)
    assert {:ok, styled} = Style.compute_detailed(dom, system_font_discovery: false)
    assert {:ok, layout} = Layout.layout(styled, margin: 0)
    groups = Enum.filter(layout.boxes, &(Map.get(&1, :role) == :text_emphasis))
    marks = Enum.flat_map(groups, & &1.boxes)
    assert length(marks) == 5
    assert Enum.all?(marks, &(&1.text == "•" and &1.font_size == 9 and &1.color == {0, 0, 1}))
    assert {:ok, pdf} = HtmlToPdf.render(html, system_font_discovery: false)
    assert {:ok, "ffiÁ B."} = Text.extract(pdf, layout: false)
  end

  test "under marks enlarge the line's descent while over marks use the normalized em box" do
    for {position, offset, distance} <- [{"over", 16.5, 36.0}, {"under", -12.0, 37.5}] do
      html =
        "<div style='font:18pt/36pt sans-serif;text-emphasis:filled dot;text-emphasis-position:#{position}'><p style='margin:0'>ABC</p><p style='margin:0'>DEF</p></div>"

      assert {:ok, dom} = HtmlParser.parse(html)
      assert {:ok, styled} = Style.compute_detailed(dom, system_font_discovery: false)
      assert {:ok, layout} = Layout.layout(styled, margin: 0)
      first = Enum.find(layout.boxes, &(Map.get(&1, :text) == "ABC"))
      second = Enum.find(layout.boxes, &(Map.get(&1, :text) == "DEF"))
      assert_in_delta first.y - second.y, distance, 0.0001

      marks =
        layout.boxes
        |> Enum.filter(&(Map.get(&1, :role) == :text_emphasis))
        |> Enum.flat_map(& &1.boxes)

      assert length(marks) == 6
      assert Enum.all?(marks, &(&1.text_baseline_offset == offset))
    end
  end

  test "zero-size emphasis retains the ordinary zero-size layout without marks" do
    assert {:ok, dom} =
             HtmlParser.parse("<p style='font-size:0;text-emphasis:filled circle'>ABC</p>")

    assert {:ok, styled} = Style.compute_detailed(dom, system_font_discovery: false)
    assert {:ok, layout} = Layout.layout(styled, margin: 0)
    refute Enum.any?(layout.boxes, &(Map.get(&1, :role) == :text_emphasis))

    assert NativeElixirPdfUtilities.HtmlToPdf.TextEmphasis.metrics(%{
             font_size: 0,
             font_face: %{type: :built_in},
             text_emphasis_style: {:filled, :dot}
           }) == nil
  end
end
