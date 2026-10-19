defmodule NativeElixirPdfUtilities.HtmlToPdf.SourceGlyphAndCalculationLimitsTest do
  use ExUnit.Case, async: false

  alias NativeElixirPdfUtilities.{HtmlToPdf, Text}
  alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, HtmlTree, ResourceContext}

  alias NativeElixirPdfUtilities.Validators.{
    FontValidator,
    HtmlValidator
  }

  test "discretionary paint retains exact original scalar glyph owners across transparent spans" do
    for separator <- ["\u00AD", "\u200B"] do
      html =
        "<p style='width:40pt;font-family:DejaVu Sans;font-size:12pt'><span>co#{separator}</span><span>operate</span></p>"

      assert {:ok, dom} = HtmlParser.parse_document_detailed(html)
      tree = HtmlTree.from_document(dom)
      budget = HtmlValidator.new_image_budget()

      assert {:ok, _prepared} =
               FontValidator.with_budget(fn ->
                 ResourceContext.run([], budget, fn _ ->
                   HtmlToPdf.prepare_document(dom, [system_font_discovery: false], budget)
                 end)
               end)

      assert {:ok, styled} =
               NativeElixirPdfUtilities.HtmlToPdf.Style.compute_detailed(dom,
                 system_font_discovery: false
               )

      [body] = styled.children
      [paragraph] = body.children
      [first_span, _second_span] = paragraph.children
      [node] = first_span.children

      original =
        Enum.find(
          Map.values(tree.nodes),
          &(Map.get(&1, :type) == :text and &1.text == "co#{separator}")
        )

      validator = NativeElixirPdfUtilities.Validators.NativeGlyphSourceValidator
      characters = validator.source_characters(original, original.text)

      run =
        validator.prepare_run(
          %{node | style: Map.put(node.style, :_source_glyph_characters, characters)},
          node.text
        )

      face = node.style.font_face

      assert {:ok, plan} =
               NativeElixirPdfUtilities.Validators.HtmlLineBreakValidator.display_plan(
                 run.text,
                 face,
                 true
               )

      painted = validator.paint_runs(run, face, 0, plan)
      assert Enum.map_join(painted, & &1.text) == if(separator == "\u00AD", do: "co‐", else: "co")

      for glyph <- painted, fragment <- glyph.source_fragments do
        assert fragment.owner.node_id == original.node_id
        assert Enum.at(String.codepoints(original.text), fragment.offset) == fragment.text
        assert glyph.glyph_ids == [Map.fetch!(face.cmap, glyph.displayed_unicode)]
      end

      assert {:ok, pdf} = HtmlToPdf.render(html, system_font_discovery: false)
      assert {:ok, text} = Text.extract(pdf, layout: false)
      assert String.replace(text, ~r/\s/u, "") == "co#{separator}operate"
      assert pdf =~ "/FontFile2" and pdf =~ "/ActualText"
    end
  end

  test "source replacement still validates original styled SVG glyphs after vendor integration" do
    html = """
    <body><?start name='source'>
    <svg width='120' height='40'><text x='2' y='25' font-family='DejaVu Sans'>𐍈</text></svg>
    <?end?><template for='source'><p>Replacement</p></template></body>
    """

    assert {:error, {:unsupported_html, diagnostic}} =
             HtmlToPdf.render(html, system_font_discovery: false, unsupported_glyphs: :error)

    assert diagnostic.reason == :unsupported_html
    assert diagnostic.stage in [:font, :layout]
    assert is_atom(diagnostic.module) and is_atom(diagnostic.operation)
    assert is_binary(diagnostic.source) and diagnostic.source != ""
    assert diagnostic.message =~ "original text"
  end

  test "public rendering retains the shared CSS calculation depth failure after source preparation" do
    limits = NativeElixirPdfUtilities.Limits.effective()

    try do
      NativeElixirPdfUtilities.Limits.install(Map.put(limits, :max_css_calculation_depth, 2))

      assert {:error, {:resource_limit_exceeded, diagnostic}} =
               HtmlToPdf.render("<p style='width:calc(min(max(1pt,2pt),3pt))'>Bounded</p>")

      assert diagnostic.reason == :resource_limit_exceeded
      assert diagnostic.stage == :style
      assert diagnostic.message =~ "max_css_calculation_depth"
      assert diagnostic.module == HtmlValidator
    after
      NativeElixirPdfUtilities.Limits.install(limits)
    end
  end
end
