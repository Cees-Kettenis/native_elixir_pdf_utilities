defmodule NativeElixirPdfUtilities.HtmlToPdf.MathGlyphSourceTest do
  use ExUnit.Case, async: false

  alias NativeElixirPdfUtilities.{HtmlToPdf, Text}
  alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, HtmlTree, ResourceContext}

  alias NativeElixirPdfUtilities.Validators.{
    FontValidator,
    HtmlValidator,
    TemplateRendererContextValidator
  }

  test "Math paint glyphs bind each original character to its selected font and GID" do
    html = "<math><mi>x</mi><mfrac><mn>12</mn><mn>3</mn></mfrac><mo stretchy='true'>(</mo></math>"
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

    owners = Enum.filter(Map.values(tree.nodes), &(Map.get(&1, :type) == :text))

    for owner <- owners do
      assert Map.has_key?(runs, owner.node_id)
      bound = Enum.sort_by(runs[owner.node_id], & &1.source_offset)
      assert Enum.map_join(bound, & &1.authored_text) == owner.text

      for run <- bound do
        assert run.owner.source == owner.source
        assert run.owner.document_id == owner.document_id
        assert run.owner.tree_scope_id == owner.tree_scope_id
        assert run.policy == :actual_math_paint_glyph_observed
        assert run.font_face.type == :embedded
        assert [gid] = run.glyph_ids
        assert gid > 0
        assert is_integer(run.displayed_unicode)
        assert run.paint_box.width > 0
      end
    end

    [x] = runs[Enum.find(owners, &(&1.text == "x")).node_id]
    assert x.displayed_unicode == 0x1D465
    assert x.glyph_ids == [Map.fetch!(x.font_face.cmap, x.displayed_unicode)]
    assert {:ok, pdf} = HtmlToPdf.render(html, system_font_discovery: false)
    assert {:ok, extracted} = Text.extract(pdf, layout: false)
    assert String.replace(extracted, ~r/\s/u, "") == "x123("
  end

  test "Latin ligature painting retains all consumed source characters" do
    html = "<math style='font-family:DejaVu Sans'><mn>ffi</mn></math>"
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

    owner = Enum.find(Map.values(tree.nodes), &(Map.get(&1, :text) == "ffi"))
    bound = Enum.sort_by(runs[owner.node_id], & &1.source_offset)
    assert Enum.map_join(bound, & &1.authored_text) == "ffi"
    assert Enum.map(bound, & &1.source_offset) == [0, 1, 2]
    assert Enum.all?(bound, &(&1.resolved_text == "ﬃ"))

    assert Enum.uniq(Enum.map(bound, & &1.glyph_ids)) == [
             [bound |> hd() |> Map.fetch!(:font_face) |> Map.fetch!(:cmap) |> Map.fetch!(0xFB03)]
           ]
  end

  test "removed MathML checks its authored font before replacement" do
    font =
      Path.join(
        :code.priv_dir(:native_elixir_pdf_utilities),
        "fonts/stix/STIXTwoMath-Regular.ttf"
      )

    html =
      "<body><?start name='A'><style>@font-face{font-family:OriginalMath;src:url('original-math.ttf')}</style><math style='font-family:OriginalMath'><mi>x</mi><mn>12</mn></math><?end?><template for='A'><p>Good</p></template></body>"

    assert {:ok, pdf} =
             HtmlToPdf.render(html,
               system_font_discovery: false,
               assets: %{"original-math.ttf" => {:file, font}},
               compress_streams: false
             )

    assert {:ok, expected} =
             HtmlToPdf.render("<p>Good</p>",
               system_font_discovery: false,
               compress_streams: false
             )

    assert pdf == expected
    assert {:ok, "Good"} = Text.extract(pdf)
  end

  test "an aggregate Math ActualText and a hidden original owner cannot satisfy glyph coverage" do
    html =
      "<body><?start name='A'><math><mphantom><mi>x</mi></mphantom><mi>y</mi></math><?end?><template for='A'><p>Good</p></template></body>"

    assert {:error, {:unsupported_html, diagnostic}} =
             HtmlToPdf.render(html, system_font_discovery: false)

    assert diagnostic.stage == :font
    assert diagnostic.operation == :parse
    assert diagnostic.source == "x"
    assert diagnostic.message =~ "unprepared original text context"
  end

  test "Math collector rejects a glyph fragment borrowed from another document" do
    assert {:ok, dom} = HtmlParser.parse_document_detailed("<math><mi>x</mi></math>")
    assert {:ok, foreign} = HtmlParser.parse_document_detailed("<math><mi>x</mi></math>")

    owner =
      Enum.find(Map.values(HtmlTree.from_document(foreign).nodes), &(Map.get(&1, :type) == :text))

    box = %{math_source_fragments: [%{owner: owner, text: "x", offset: 0}]}

    assert {:error, {:invalid_document, diagnostic}} =
             TemplateRendererContextValidator.validate_prepared_math_glyph_runs(
               [%{boxes: [box]}],
               HtmlTree.from_document(dom)
             )

    assert diagnostic.stage == :font
    assert diagnostic.operation == :template_renderer_context
    assert diagnostic.source == "x"
    assert diagnostic.message =~ "original text source identity"
  end
end
