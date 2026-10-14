defmodule NativeElixirPdfUtilities.HtmlToPdf.MathNativeGlyphSourceTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.{HtmlToPdf, Text}
  alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, HtmlTree, ResourceContext}

  alias NativeElixirPdfUtilities.Validators.{
    FontValidator,
    HtmlValidator,
    TemplateRendererContextValidator
  }

  test "native Math text and annotation CSS boxes bind actual glyphs to exact scalar owners" do
    for content <- [
          "<mtext><span style='background:yellow'>Before Before ffi</span></mtext>",
          "<annotation encoding='text/plain' style='display:inline-block;width:32px'>Repeat Repeat</annotation>",
          "<annotation-xml encoding='text/html' style='display:inline-flex'><span>A</span><span>B</span></annotation-xml>",
          "<mtext><span style='white-space:pre-wrap;width:64px;display:inline-block'>A\tB\r\nRepeat Repeat</span></mtext>",
          "<mtext><svg width='120' height='28'><text x='1' y='20' font-family='DejaVu Sans'>Alpha<tspan font-weight='bold'>Beta</tspan></text></svg></mtext>"
        ] do
      html = "<math>#{content}<mn>1</mn></math>"
      assert {:ok, dom} = HtmlParser.parse_document_detailed(html)
      tree = HtmlTree.from_document(dom)
      budget = HtmlValidator.new_image_budget()

      assert {:ok, prepared} =
               FontValidator.with_budget(fn ->
                 ResourceContext.run([], budget, fn _ ->
                   HtmlToPdf.prepare_document(dom, [system_font_discovery: false], budget)
                 end)
               end)

      assert {:ok, runs} =
               TemplateRendererContextValidator.validate_prepared_math_glyph_runs(
                 prepared.pages,
                 tree
               )

      for owner <- Map.values(tree.nodes), Map.get(owner, :type) == :text do
        parent = tree.nodes[owner.parent_id]

        if parent.namespace == :svg do
          refute Map.has_key?(runs, owner.node_id)
        else
          assert Map.has_key?(runs, owner.node_id)

          bound =
            runs[owner.node_id]
            |> Enum.uniq_by(& &1.source_offset)
            |> Enum.sort_by(& &1.source_offset)

          assert Enum.map_join(bound, & &1.authored_text) |> String.replace(~r/\s/u, "") ==
                   String.replace(owner.text, ~r/\s/u, "")

          for run <- bound do
            assert run.owner.source == owner.source
            assert run.owner.document_id == owner.document_id
            assert run.owner.tree_scope_id == owner.tree_scope_id
            assert [gid] = run.glyph_ids
            assert gid > 0

            assert run.paint_basis == :native_text_painter
            assert gid == Map.fetch!(run.font_face.cmap, run.displayed_unicode)
          end
        end
      end

      original =
        "<body><?start name='A'>#{html}<?end?><template for='A'><p>Good</p></template></body>"

      assert {:ok, pdf} = HtmlToPdf.render(original, system_font_discovery: false)
      assert {:ok, "Good"} = Text.extract(pdf)
    end
  end

  test "wrapped native Math fragments keep mixed-font glyph owners and original text once" do
    html =
      "<math><mtext style='display:inline-block;width:40pt;white-space:normal'><span style='font-family:DejaVu Sans;font-size:12pt'>Alpha beta gamma</span><span style='font-family:STIX Two Math;font-size:10pt'> delta Omega</span></mtext><mn>2</mn></math>"

    assert {:ok, dom} = HtmlParser.parse_document_detailed(html)
    tree = HtmlTree.from_document(dom)
    budget = HtmlValidator.new_image_budget()

    assert {:ok, prepared} =
             FontValidator.with_budget(fn ->
               ResourceContext.run([], budget, fn _ ->
                 HtmlToPdf.prepare_document(dom, [system_font_discovery: false], budget)
               end)
             end)

    assert {:ok, runs} =
             TemplateRendererContextValidator.validate_prepared_math_glyph_runs(
               prepared.pages,
               tree
             )

    originals =
      for source <- Map.values(tree.nodes),
          source.type == :text,
          source.text in ["Alpha beta gamma", " delta Omega"],
          do: source

    assert length(originals) == 2

    collect = fn collect, box ->
      [box | Enum.flat_map(Map.get(box, :boxes, []), &collect.(collect, &1))]
    end

    leaves =
      for page <- prepared.pages,
          box <- page.boxes,
          leaf <- collect.(collect, box),
          Map.get(leaf, :type) == :text,
          do: leaf

    for source <- originals do
      painted = runs[source.node_id]
      fragments = Enum.filter(leaves, &(Map.get(&1, :_math_actual_source) == source.text))
      assert length(Enum.uniq_by(fragments, & &1.y)) > 1

      for run <- painted do
        assert [gid] = run.glyph_ids
        assert gid == Map.fetch!(run.font_face.cmap, run.displayed_unicode)
        assert run.owner.source == source.source
        assert run.paint_basis == :native_text_painter
      end
    end

    assert originals
           |> Enum.flat_map(&runs[&1.node_id])
           |> Enum.map(& &1.font_face.id)
           |> Enum.uniq()
           |> length() == 2

    assert {:ok, pdf} = HtmlToPdf.render(html, system_font_discovery: false)
    assert {:ok, "Alpha beta gamma delta Omega2"} = Text.extract(pdf, layout: false)
    assert pdf =~ "/FontFile2"
  end

  test "native painted source fragments retain their owner alongside SVG image artwork" do
    html =
      "<math><mtext><span style='background:yellow'>Native</span><svg width='70' height='24'><text x='1' y='18' font-family='DejaVu Sans'>Svg</text></svg></mtext></math>"

    assert {:ok, dom} = HtmlParser.parse_document_detailed(html)
    tree = HtmlTree.from_document(dom)
    budget = HtmlValidator.new_image_budget()

    assert {:ok, prepared} =
             FontValidator.with_budget(fn ->
               ResourceContext.run([], budget, fn _ ->
                 HtmlToPdf.prepare_document(dom, [system_font_discovery: false], budget)
               end)
             end)

    assert {:ok, foreign} = HtmlParser.parse_document_detailed(html)
    foreign_tree = HtmlTree.from_document(foreign)

    assert {:error, {:invalid_document, diagnostic}} =
             TemplateRendererContextValidator.validate_prepared_math_glyph_runs(
               prepared.pages,
               foreign_tree
             )

    assert diagnostic.stage == :font
    assert diagnostic.operation == :template_renderer_context
    assert diagnostic.source == "Native"
    assert diagnostic.message =~ "original text source identity"

    assert {:ok, _} =
             TemplateRendererContextValidator.validate_prepared_math_glyph_runs(
               prepared.pages,
               tree
             )
  end
end
