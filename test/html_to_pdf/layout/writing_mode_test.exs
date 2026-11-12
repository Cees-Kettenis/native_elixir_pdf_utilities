Code.require_file(NativeElixirPdfUtilities.TestSupport.Paths.support("open_type_fixture.ex"))

defmodule NativeElixirPdfUtilities.HtmlToPdf.WritingModeTest do
  use ExUnit.Case, async: true
  alias NativeElixirPdfUtilities.{HtmlToPdf, Text}
  alias NativeElixirPdfUtilities.HtmlToPdf.{Font, HtmlParser, Layout, Pagination, Style}
  alias NativeElixirPdfUtilities.TestSupport.OpenTypeFixture
  alias NativeElixirPdfUtilities.Validators.{CssWritingModeValidator, WriterValidator}

  test "orientation and combination grammar shares computed-style and paint admission" do
    assert "text-combine-upright" in CssWritingModeValidator.properties()

    for mode <- [:none, :all] do
      assert {:ok, %{text_combine_upright: ^mode}} =
               CssWritingModeValidator.prepare(%{}, "text-combine-upright", Atom.to_string(mode))
    end

    assert {:ok, %{text_orientation: :upright}} =
             CssWritingModeValidator.prepare(%{}, "text-orientation", "upright")

    assert {:ok, %{text_combine_upright: {:digits, 2}}} =
             CssWritingModeValidator.prepare(%{}, "text-combine-upright", "digits")

    assert {:ok, %{text_combine_upright: {:digits, 4}}} =
             CssWritingModeValidator.prepare(%{}, "text-combine-upright", "digits 4")

    assert :error = CssWritingModeValidator.prepare(%{}, "text-combine-upright", "digits 5")
    assert :error = CssWritingModeValidator.prepare(%{}, "text-orientation", "vertical")
    assert :ok = CssWritingModeValidator.validate_style(%{}, "p")

    assert {:error, {:invalid_layout, %{stage: :layout, source: "p"}}} =
             CssWritingModeValidator.validate_style(%{text_orientation: :invalid}, "p")

    refute CssWritingModeValidator.valid_paint?(%{glyph_basis: {0, 0, 0, 0}})
    refute CssWritingModeValidator.valid_paint?(%{glyph_basis: :invalid})
  end

  test "upright metrics and substitutions supply logical fitting advances" do
    data = OpenTypeFixture.vertical_font()
    assert {:ok, [font]} = Font.load_configured_faces([%{family: "Vertical", data: data}])

    vertical =
      Map.put(font, :_vertical_text, %{mode: :vertical_rl, orientation: :upright, combine: :none})

    assert_in_delta Font.text_width("AB", vertical, 12), 29.76, 0.0001
    assert {:ok, plan} = Font.css_glyph_positions("AB", vertical, 12)
    assert Enum.map(plan.glyphs, & &1.glyph) == [4, 2]
    assert_in_delta plan.advance, 29.76, 0.0001
    assert Enum.all?(plan.glyphs, &(&1.glyph_basis == {0, 1, -1, 0}))
    assert Enum.map(plan.glyphs, & &1.cluster_start) == [0, 1]
    sideways = put_in(vertical._vertical_text.orientation, :sideways)
    assert_in_delta Font.text_width("AB", sideways, 12), 16.8, 0.0001
  end

  test "orthogonal inline flex and grid resolve percentage and intrinsic inline sizes in the page's block axis" do
    for {display, size} <- [
          {"inline-flex", "50%"},
          {"inline-grid", "min-content"},
          {"inline-grid", "max-content"},
          {"inline-flex", "fit-content(18pt)"},
          {"inline-grid", "calc(50% - 10pt)"}
        ] do
      html =
        "<span style='display:#{display};grid-template-columns:1fr;writing-mode:vertical-rl;inline-size:#{size};font:10pt/12pt DejaVu Sans Mono'>AB CD</span>"

      assert {:ok, dom} = HtmlParser.parse(html)
      assert {:ok, styled} = Style.compute_detailed(dom, system_font_discovery: false)
      assert {:ok, layout} = Layout.layout(styled, page_size: {200, 180}, margin: 0)
      [group] = Enum.filter(layout.boxes, &(Map.get(&1, :role) == :writing_mode))
      font = hd(styled.children).style.font_face

      expected =
        case size do
          "50%" -> 90
          "min-content" -> Font.css_text_width("AB", font, 10)
          "max-content" -> Font.css_text_width("AB CD", font, 10)
          "fit-content(18pt)" -> 18
          "calc(50% - 10pt)" -> 80
        end

      assert_in_delta group.writing_extent.height, expected, 0.02
      assert group.writing_extent.width > 0

      if size == "min-content" do
        text = Enum.filter(group.boxes, &(&1.type == :text))
        assert Enum.map(text, & &1.text) == ["AB", "CD"]
        assert length(Enum.uniq_by(text, & &1.y)) == 2
      end
    end
  end

  test "orthogonal atomic boxes honor top, center and bottom alignment after preceding block content" do
    for alignment <- ["top", "center", "bottom"] do
      html =
        "<div style='font:8pt/40pt sans-serif'><div style='height:12pt'></div><span style='display:inline-grid;grid-template-columns:1fr;writing-mode:vertical-rl;inline-size:min-content;vertical-align:#{alignment};font:10pt/12pt DejaVu Sans Mono'>AB CD</span></div>"

      assert {:ok, dom} = HtmlParser.parse(html)
      assert {:ok, styled} = Style.compute_detailed(dom, system_font_discovery: false)
      assert {:ok, layout} = Layout.layout(styled, page_size: {200, 180}, margin: 0)
      [group] = Enum.filter(layout.boxes, &(Map.get(&1, :role) == :writing_mode))
      height = group.writing_extent.height

      expected =
        case alignment do
          "top" -> 168
          "center" -> 168 - 20 + height / 2
          "bottom" -> 168 - 40 + height
        end

      assert_in_delta group.y + height, expected, 0.0001
    end
  end

  test "contextual vertical alternates retain their neighboring transformed characters" do
    data = OpenTypeFixture.contextual_vertical_font()
    assert byte_size(data) < 6000

    assert {:ok, [font]} =
             Font.load_configured_faces([%{family: "Contextual vertical", data: data}])

    font =
      Map.put(font, :_vertical_text, %{mode: :vertical_rl, orientation: :mixed, combine: :none})

    assert {:ok, plan} = Font.css_glyph_positions("（（", font, 12)
    assert Enum.map(plan.glyphs, & &1.glyph) == [4, 1]
    assert Enum.all?(plan.glyphs, &(&1.glyph_basis == {0, 1, -1, 0}))
    assert Enum.map(plan.glyphs, & &1.cluster_start) == [0, 3]
    assert_in_delta plan.advance, 24, 0.0001
  end

  test "mixed Tr and Tu characters retain upright nominal forms when an alternate is absent" do
    alias NativeElixirPdfUtilities.HtmlToPdf.UnicodeTextData
    assert elem(UnicodeTextData.properties(0x3008), 7) == :Tr
    assert elem(UnicodeTextData.properties(0x3009), 7) == :Tr
    assert elem(UnicodeTextData.properties(0x3001), 7) == :Tu

    single =
      OpenTypeFixture.join(
        fn [coverage] -> OpenTypeFixture.words([2, coverage, 1, 4]) end,
        [OpenTypeFixture.coverage([1])]
      )

    source =
      OpenTypeFixture.outline_font([%{type: 1, subtables: [single]}], [],
        cmap: %{0x3008 => 1, 0x3009 => 2, 0x3001 => 3},
        features: [{"vert", [0]}],
        widths: %{1 => 600, 2 => 800, 3 => 500, 4 => 1000}
      )

    assert {:ok, [font]} = Font.load_configured_faces([%{family: "Vertical forms", data: source}])

    mixed =
      Map.put(font, :_vertical_text, %{mode: :vertical_rl, orientation: :mixed, combine: :none})

    assert {:ok, plan} = Font.css_glyph_positions("〈〉、", mixed, 12)
    [opening, closing, comma] = plan.glyphs
    assert opening.glyph == 4 and opening.glyph_basis == {0, 1, -1, 0}
    assert closing.glyph == 2 and closing.glyph_basis == {0, 1, -1, 0}
    assert comma.glyph == 3 and comma.glyph_basis == {0, 1, -1, 0}
    assert_in_delta closing.x_advance, 12, 0.0001
    assert_in_delta comma.x_advance, 12, 0.0001

    upright = put_in(mixed._vertical_text.orientation, :upright)
    assert {:ok, plan} = Font.css_glyph_positions("〈〉", upright, 12)
    assert Enum.all?(plan.glyphs, &(&1.glyph_basis == {0, 1, -1, 0}))
    assert_in_delta plan.advance, 24, 0.0001
  end

  test "vertical and sideways blocks reuse physical scopes and retain source extraction" do
    data = OpenTypeFixture.vertical_font()

    for {mode, basis} <- [
          {"vertical-rl", {0, -1, 1, 0}},
          {"vertical-lr", {0, -1, -1, 0}},
          {"sideways-rl", {0, -1, 1, 0}},
          {"sideways-lr", {0, 1, -1, 0}}
        ] do
      html =
        "<div style='writing-mode:#{mode};width:36pt;height:72pt;font:12pt Vertical;text-orientation:upright'>AB</div>"

      options = [fonts: [%{family: "Vertical", data: data}], system_font_discovery: false]
      assert {:ok, dom} = HtmlParser.parse(html)
      assert {:ok, styled} = Style.compute_detailed(dom, options)
      assert {:ok, layout} = Layout.layout(styled, page_size: {200, 200}, margin: 0)
      [group] = layout.boxes
      assert group.role == :writing_mode
      assert group.writing_extent == %{width: 36.0, height: 72.0}
      assert Tuple.to_list(group.transform) |> Enum.take(4) |> List.to_tuple() == basis
      assert {:ok, pages} = Pagination.paginate(layout)
      assert {:ok, prepared} = WriterValidator.prepare(pages, [])
      assert Enum.any?(hd(prepared.pages).boxes, &(&1.type == :drawing_group_start))
      assert {:ok, pdf} = HtmlToPdf.render(html, options ++ [page_size: {200, 200}, margin: 0])
      assert {:ok, "AB"} = Text.extract(pdf, layout: false)
    end
  end

  test "combined glyphs occupy one em and preserve a complete source cluster" do
    assert {:ok, [font]} =
             Font.load_configured_faces([
               %{family: "Combined vertical", data: OpenTypeFixture.vertical_font()}
             ])

    font =
      Map.put(font, :_vertical_text, %{mode: :vertical_rl, orientation: :mixed, combine: :all})

    assert {:ok, shaped} = Font.css_glyph_positions("AB", font, 12)
    assert_in_delta shaped.advance, 12.0, 0.0001
    assert_in_delta hd(shaped.glyphs).x_advance, 12.0, 0.0001
    assert List.last(shaped.glyphs).x_advance == 0.0
    assert Enum.all?(shaped.glyphs, &(&1.cluster_start == 0 and &1.cluster_end == 2))

    assert Enum.all?(shaped.glyphs, fn glyph ->
             {a, scale, c, d} = glyph.glyph_basis
             a == 0 and c == -1 and d == 0 and abs(scale - 5 / 7) < 0.0001
           end)

    assert {:ok, empty} = Font.css_glyph_positions("", font, 12)
    assert empty.glyphs == [] and empty.advance == 0
  end

  test "digit combination stays within its requested count and rotates longer left-to-right columns" do
    data = OpenTypeFixture.outline_font([], [], cmap: %{?1 => 1, ?2 => 2, ?3 => 3})
    assert {:ok, [font]} = Font.load_configured_faces([%{family: "Vertical digits", data: data}])

    for mode <- [:vertical_rl, :vertical_lr] do
      selected =
        Map.put(font, :_vertical_text, %{mode: mode, orientation: :mixed, combine: {:digits, 2}})

      assert {:ok, combined} = Font.css_glyph_positions("12", selected, 12)
      # FreeType's 16.16 multiplier for a 1000-unit em at 16 CSS px is
      # 67109; the quantized one-em advance is 1048578 / 65536 CSS px.
      assert_in_delta combined.advance, 12.000022888183594, 1.0e-8
      assert Enum.all?(combined.glyphs, &(&1.cluster_start == 0 and &1.cluster_end == 2))
      assert {:ok, longer} = Font.css_glyph_positions("123", selected, 12)
      assert_in_delta longer.advance, 18.00003433227539, 1.0e-8

      if mode == :vertical_lr do
        assert Enum.all?(longer.glyphs, &(&1.glyph_basis == {1, 0, 0, -1}))
      else
        assert Enum.all?(longer.glyphs, &(not Map.has_key?(&1, :glyph_basis)))
      end
    end
  end

  test "orthogonal descendants restore horizontal text and remain within a vertical link scope" do
    data = OpenTypeFixture.vertical_font()

    options = [
      fonts: [%{family: "Vertical", data: data}],
      system_font_discovery: false,
      page_size: {180, 120},
      margin: 0,
      compress_streams: false
    ]

    html = """
    <div style="writing-mode:vertical-rl;width:48pt;height:72pt;font:12pt Vertical">
    <a href="https://example.org/"><span style="writing-mode:horizontal-tb;display:inline-block;width:20pt;height:12pt">AB</span>C</a>
    </div>
    """

    assert {:ok, pdf} = HtmlToPdf.render(html, options)
    assert pdf =~ "/Subtype /Link"
    assert {:ok, text} = Text.extract(pdf, layout: false)
    assert String.replace(text, ~r/\s/u, "") == "ABC"
    assert pdf =~ "0 1 -1 0"
  end

  test "upright used direction controls logical edges without changing computed direction" do
    assert {:ok, dom} =
             HtmlParser.parse(
               "<div style='writing-mode:vertical-rl;direction:rtl;text-orientation:upright;margin-inline-start:3pt'>AB</div>"
             )

    assert {:ok, %{children: [node]}} = Style.compute_detailed(dom, system_font_discovery: false)
    assert node.style.direction == :rtl
    assert node.style.margin.top == 3.0
    assert node.style.margin.bottom == 0.0
    assert node.style.font_face._vertical_text.orientation == :upright
  end

  test "color glyph artwork keeps its upright basis and projected link region" do
    data =
      NativeElixirPdfUtilities.TestSupport.FontCorpus.path("selected-colr.ttf") |> File.read!()

    html =
      "<div style='font:12pt Color;writing-mode:vertical-rl;text-orientation:upright;width:30pt;height:60pt'><a href='https://example.org/'>Hx0</a></div>"

    assert {:ok, pdf} =
             HtmlToPdf.render(html,
               fonts: [%{family: "Color", data: data}],
               system_font_discovery: false,
               page_size: {120, 100},
               margin: 0,
               compress_streams: false
             )

    assert pdf =~ "/Subtype /Image"
    assert pdf =~ "/Subtype /Link"
    assert {:ok, "Hx0"} = Text.extract(pdf, layout: false)
  end

  test "separate vertical paragraphs retain their physical heights across pages" do
    html =
      String.duplicate(
        "<div style='writing-mode:vertical-rl;width:24pt;height:60pt;font-size:12pt'>AB</div>",
        3
      )

    assert {:ok, pdf} =
             HtmlToPdf.render(html,
               page_size: {120, 100},
               margin: 0,
               system_font_discovery: false
             )

    assert {:ok, text} = Text.extract(pdf, layout: false)
    assert String.replace(text, ~r/\s/u, "") == "ABABAB"
    assert {:ok, pages} = NativeElixirPdfUtilities.Info.page_count(pdf)
    assert pages == 3
  end
end
