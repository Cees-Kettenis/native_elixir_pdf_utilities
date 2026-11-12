defmodule NativeElixirPdfUtilities.HtmlToPdf.RubyControlsTest do
  use ExUnit.Case, async: true
  alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, RubyLayout, Style}
  alias NativeElixirPdfUtilities.TestSupport.Paths
  alias NativeElixirPdfUtilities.Validators.{NativeGlyphSourceValidator, RubyValidator}

  defp measure(controls, children) do
    html = "<ruby style=\"font:12pt 'Ruby CJK';#{controls}\">#{children}</ruby>"
    assert {:ok, dom} = HtmlParser.parse_detailed(html)

    assert {:ok, %{children: [ruby]}} =
             Style.compute_detailed(dom,
               fonts: [
                 %{
                   family: "Ruby CJK",
                   path: Paths.fixture("html_to_pdf/browser_parity/assets/cjk-ruby-fixture.ttf")
                 }
               ],
               system_font_discovery: false
             )

    assert {:ok, measured} = RubyLayout.measure(ruby, nil)
    {ruby, measured}
  end

  test "ruby preserves explicit bidi isolation and upgrades overrides to isolated overrides" do
    for {mode, expected} <- [
          {:bidi_override, :isolate_override},
          {:isolate, :isolate},
          {:isolate_override, :isolate_override}
        ] do
      assert RubyValidator.compute_style(%{_ruby_container: true, unicode_bidi: mode}).unicode_bidi ==
               expected
    end
  end

  test "ruby controls use one finite authored and computed grammar" do
    for {property, value, key, expected} <- [
          {"ruby-align", "space-between", :ruby_align, :space_between},
          {"ruby-position", "under alternate", :ruby_position, [:alternate, :under]},
          {"ruby-position", "alternate", :ruby_position, [:alternate, :over]},
          {"ruby-position", "inter-character", :ruby_position, [:inter_character]},
          {"ruby-merge", "merge", :ruby_merge, :merge},
          {"ruby-overhang", "none", :ruby_overhang, :none}
        ] do
      assert {:ok, result} = RubyValidator.prepare_property(%{}, property, value)
      assert result[key] == expected
      assert :ok = RubyValidator.validate_style(result, "ruby")
    end

    assert "ruby-merge" in RubyValidator.properties()

    assert {:ok, %{ruby_merge: :merge}} =
             RubyValidator.prepare_property(%{}, "ruby-merge", "collapse")

    assert {:error, {:invalid_layout, %{source: "ruby"}}} =
             RubyValidator.validate_style(%{ruby_align: :invalid}, "ruby")

    assert :error = RubyValidator.prepare_property(%{}, "ruby-align", "justify")

    assert {:error, {:invalid_layout, %{source: "ruby", message: message}}} =
             RubyValidator.validate_style(%{ruby_position: [:invalid]}, "ruby")

    assert message =~ "recompute"
  end

  test "over and under annotations reserve the corresponding baseline side" do
    {_, [over]} = measure("ruby-position:over", "京<rt>東京</rt>")
    {_, [under]} = measure("ruby-position:under", "京<rt>東京</rt>")
    reading = fn measured -> Enum.find(measured.boxes, &(Map.get(&1, :text) == "東京")) end
    assert reading.(over).y > 0
    assert reading.(under).y < 0
    assert over.ascent > under.ascent
    assert under.descent > over.descent
  end

  test "alignment distributes complete CJK clusters with the specified edge spaces" do
    for {mode, edge, spacing} <- [
          {"start", 0.0, 0.0},
          {"center", 12.0, 0.0},
          {"space-between", 0.0, 24.0},
          {"space-around", 6.0, 12.0}
        ] do
      {_, [measured]} =
        measure(
          "ruby-align:#{mode};ruby-overhang:none",
          "東京<rt style='font-size:12pt'>東京京都</rt>"
        )

      base = Enum.find(measured.boxes, &(Map.get(&1, :text) == "東京"))
      assert_in_delta measured.width, 48, 0.001
      assert_in_delta base.x, edge, 0.02
      assert_in_delta Map.get(base, :_justification_spacing, 0.0), spacing, 0.02
      assert measured.overhang == 0
    end
  end

  test "collapse merges adjacent reading levels without discarding base or reading owners" do
    {_, separate} = measure("ruby-merge:separate", "京<rt>東京</rt>都<rt>京都</rt>")
    {_, [collapsed]} = measure("ruby-merge:merge", "京<rt>東京</rt>都<rt>京都</rt>")
    assert length(separate) == 2
    assert Enum.map_join(collapsed.source_group.base, & &1.text) == "京都"
    assert Enum.map_join(hd(collapsed.source_group.annotations).children, & &1.text) == "東京京都"
  end

  test "fragmented text retains original scalar owners on both sides of a ruby split" do
    {ruby, _} = measure("", "東京京都<rt>東京京都</rt>")

    owner =
      %{type: :text, text: "東京京都", node_id: make_ref(), style: ruby.style}
      |> NativeGlyphSourceValidator.prepare_tree()

    {before, after_group} =
      RubyValidator.partition_group(%{base: [owner], annotations: []}, 2, [])

    assert hd(before.base).text == "東京"
    assert hd(after_group.base).text == "京都"

    fragments =
      hd(after_group.base).style._source_glyph_characters |> Enum.flat_map(& &1.fragments)

    assert Enum.map(fragments, & &1.offset) == [2, 3]
    assert Enum.all?(fragments, &(&1.owner.text == "東京京都"))
  end

  test "inter-character reading creates an upright vertical column to the right" do
    {ruby, [measured]} =
      measure("ruby-position:inter-character;ruby-overhang:none", "京<rt>とう</rt>")

    reading = Enum.find(ruby.children, &(Map.get(&1, :tag) == "rt"))
    assert reading.style.writing_mode == :vertical_rl
    assert reading.style._ruby_inter_character
    group = Enum.find(measured.boxes, &(Map.get(&1, :role) == :writing_mode))
    assert group.x >= measured.base_width
    assert measured.width > measured.base_width
    assert_in_delta group.writing_extent.height, 12, 0.001
    assert measured.overhang == 0

    assert {:ok, pdf} =
             NativeElixirPdfUtilities.HtmlToPdf.render(
               "<ruby style=\"font:12pt 'Ruby CJK';ruby-position:inter-character\">京<rt>とう</rt></ruby>",
               fonts: [
                 %{
                   family: "Ruby CJK",
                   path: Paths.fixture("html_to_pdf/browser_parity/assets/cjk-ruby-fixture.ttf")
                 }
               ],
               system_font_discovery: false
             )

    assert {:ok, text} = NativeElixirPdfUtilities.Text.extract(pdf, layout: false)
    assert String.replace(text, ~r/\s/u, "") == "京とう"
  end

  test "start-aligned inter-character reading uses the base top rather than its center" do
    {ruby, [measured]} =
      measure("ruby-position:inter-character;ruby-align:start;ruby-overhang:none", "京<rt>とう</rt>")

    group = Enum.find(measured.boxes, &(Map.get(&1, :role) == :writing_mode))
    assert {:ok, base_em} = RubyValidator.em_metrics(ruby.style)
    assert_in_delta group.y + group.writing_extent.height, base_em.ascent, 0.001
    assert group.x >= measured.base_width
  end

  test "merging three nested bases retains the isolation already issued for the first pair" do
    {_, [measured]} =
      measure(
        "ruby-merge:merge",
        "<span>京</span><rt>東京</rt><span>都</span><rt>京都</rt><span>京</span><rt>東京</rt>"
      )

    assert Enum.map_join(measured.source_group.base, fn node ->
             case node do
               %{text: text} -> text
               %{children: children} -> Enum.map_join(children, & &1.text)
             end
           end) == "京都京"

    contexts = Enum.map(measured.source_group.base, & &1.style._bidi_contexts)
    assert Enum.all?(contexts, &(length(&1) == 2))
    assert contexts |> Enum.map(&hd/1) |> Enum.uniq() |> length() == 1
    assert length(Enum.uniq(contexts)) == 3

    for node <- measured.source_group.base do
      assert hd(node.children).style._bidi_contexts == node.style._bidi_contexts
    end
  end

  test "merging keeps the original base boundaries isolated from ligature formation" do
    {_, [measured]} =
      measure("font-family:'DejaVu Sans';ruby-merge:merge", "f<rt>a</rt>i<rt>b</rt>")

    base = Enum.filter(measured.boxes, &(Map.get(&1, :text) in ["f", "i"]))
    assert Enum.map(base, & &1.text) == ["f", "i"]
    assert Enum.all?(base, &Map.has_key?(&1, :bidi_levels))
  end

  test "alternate annotation containers reserve both sides of the same base" do
    {_, [measured]} =
      measure("ruby-position:alternate over", """
      <span style='display:ruby-base-container'><span style='display:ruby-base'>京</span></span><span style='display:ruby-text-container'><span style='display:ruby-text;font-size:6pt'>東京</span></span><span style='display:ruby-text-container'><span style='display:ruby-text;font-size:6pt'>京都</span></span>
      """)

    over = Enum.find(measured.boxes, &(Map.get(&1, :text) == "東京"))
    under = Enum.find(measured.boxes, &(Map.get(&1, :text) == "京都"))
    assert over.y > 0
    assert under.y < 0
  end
end
