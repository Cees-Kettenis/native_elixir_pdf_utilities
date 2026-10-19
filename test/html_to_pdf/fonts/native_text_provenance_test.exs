defmodule NativeElixirPdfUtilities.HtmlToPdf.NativeTextProvenanceTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.{Forms, HtmlToPdf}
  alias NativeElixirPdfUtilities.HtmlToPdf.{Font, HtmlParser, HtmlTree}
  alias NativeElixirPdfUtilities.Pdf.Reader

  alias NativeElixirPdfUtilities.Validators.{
    FormValidator,
    HtmlValidator,
    NativeGlyphSourceValidator
  }

  test "parsed DOM scalars survive whitespace, slicing, tabs and selected font ligatures" do
    assert {:ok, document} = HtmlParser.parse_document_detailed("<p> A\tffi\r\nΩ </p>")
    tree = HtmlTree.from_document(document)
    owner = Enum.find(Map.values(tree.nodes), &(&1.type == :text))
    characters = NativeGlyphSourceValidator.source_characters(owner, owner.text)
    assert Enum.map_join(characters, & &1.displayed) == owner.text

    assert Enum.all?(characters, fn character ->
             hd(character.fragments).owner.node_id == owner.node_id
           end)

    data = File.read!("priv/fonts/dejavu/DejaVuSans.ttf")
    assert {:ok, [face]} = Font.load_configured_faces([%{family: "Actual", data: data}])

    HtmlValidator.with_render_budget(fn ->
      node = Map.put(owner, :style, %{white_space: :pre_wrap})

      node =
        NativeGlyphSourceValidator.prepare_tree(%{children: [node]})
        |> Map.fetch!(:children)
        |> hd()

      run = NativeGlyphSourceValidator.prepare_run(node, owner.text)
      run = NativeGlyphSourceValidator.trim_run(run, "A\tffi\nΩ")
      expanded = NativeGlyphSourceValidator.expand_tabs(run, "A  ffi\nΩ", [2])
      assert Enum.map_join(expanded.source_glyph_characters, & &1.displayed) == expanded.text
      sliced = NativeGlyphSourceValidator.slice_run(expanded, 3, "ffi")
      runs = NativeGlyphSourceValidator.paint_runs(sliced, face, 0)

      assert Enum.map_join(runs, fn glyph -> Enum.map_join(glyph.source_fragments, & &1.text) end) ==
               "ffi"

      for glyph <- runs do
        assert glyph.glyph_ids == [face.cmap[glyph.displayed_unicode]]
        assert glyph.field_scalar_fragments == []
      end

      collapsed =
        NativeGlyphSourceValidator.normalize_characters(characters, %{white_space: :normal})

      assert Enum.map_join(collapsed, & &1.displayed) ==
               NativeGlyphSourceValidator.normalize_text(owner.text, %{white_space: :normal})

      assert NativeGlyphSourceValidator.trim_run(run, "").source_glyph_characters == []
      merged = NativeGlyphSourceValidator.merge_runs(sliced, sliced)
      assert merged.text == "ffiffi"
    end)
  end

  test "current parsed field scalar carries field fragments independently of a DOM owner" do
    assert {:ok, pdf} =
             HtmlToPdf.render("<input name='current' value='ffiZ'>", system_font_discovery: false)

    assert {:ok, [%{value: "ffiZ"}]} = Forms.fields(pdf)
    assert {:ok, context} = Reader.read_validated(pdf)
    assert {:ok, %{fields: [field]}} = FormValidator.inspect_document(context)
    [widget] = field.widgets

    authority = %{
      source_pdf_sha256: :crypto.hash(:sha256, pdf),
      field_ref: field.ref,
      widget_ref: widget.ref,
      field_key: field.name,
      scalar: field.value
    }

    characters = NativeGlyphSourceValidator.field_scalar_characters(authority)
    assert Enum.map_join(characters, & &1.displayed) == field.value
    assert Enum.map(characters, &hd(&1.fragments).offset) == [0, 1, 2, 3]

    assert Enum.all?(characters, fn character ->
             not Map.has_key?(hd(character.fragments), :owner)
           end)

    [face] = widget.source_editor.fonts
    assert {:ok, [font]} = Font.load_configured_faces([face])

    glyphs =
      NativeGlyphSourceValidator.paint_runs(%{source_glyph_characters: characters}, font, 0)

    assert Enum.all?(glyphs, &(&1.source_fragments == []))

    assert Enum.map_join(glyphs, fn glyph ->
             Enum.map_join(glyph.field_scalar_fragments, & &1.text)
           end) == field.value
  end

  test "discretionary breaks select the font hyphen while omitted source markers remain unpainted" do
    assert {:ok, document} = HtmlParser.parse_document_detailed("<p>co&#xAD;</p>")
    tree = HtmlTree.from_document(document)
    owner = Enum.find(Map.values(tree.nodes), &(&1.type == :text))

    assert {:ok, [face]} =
             Font.load_configured_faces([
               %{family: "Actual", data: File.read!("priv/fonts/dejavu/DejaVuSans.ttf")}
             ])

    HtmlValidator.with_render_budget(fn ->
      node = NativeGlyphSourceValidator.prepare_tree(Map.put(owner, :style, %{}))
      run = NativeGlyphSourceValidator.prepare_run(node, owner.text)

      for taken <- [false, true] do
        assert {:ok, plan} =
                 NativeElixirPdfUtilities.Validators.HtmlLineBreakValidator.display_plan(
                   run.text,
                   face,
                   taken
                 )

        glyphs = NativeGlyphSourceValidator.paint_runs(run, face, 0, plan)
        assert Enum.map_join(glyphs, & &1.text) == plan.text
        assert Enum.all?(glyphs, &(&1.glyph_ids == [face.cmap[&1.displayed_unicode]]))

        if taken do
          assert List.last(glyphs).source_fragments |> Enum.map_join(& &1.text) == "\u00AD"
        else
          assert Enum.map_join(glyphs, fn glyph ->
                   Enum.map_join(glyph.source_fragments, & &1.text)
                 end) == "co"
        end
      end

      ordinary = %{text: "co", style: %{}}
      assert NativeGlyphSourceValidator.prepare_run(%{style: %{}}, "co") == ordinary
      assert NativeGlyphSourceValidator.slice_run(ordinary, 1, "o").text == "o"
      assert NativeGlyphSourceValidator.trim_run(ordinary, "o").text == "o"
      assert NativeGlyphSourceValidator.expand_tabs(ordinary, "co", []) == ordinary
      assert NativeGlyphSourceValidator.merge_runs(ordinary, ordinary).text == "coco"
      assert NativeGlyphSourceValidator.paint_runs(ordinary, face, 0) == []
      assert NativeGlyphSourceValidator.source_characters(%{text: "co"}, "co") == nil

      assert NativeGlyphSourceValidator.prepare_tree(%{type: :comment, text: "co"}) == %{
               type: :comment,
               text: "co"
             }

      assert NativeGlyphSourceValidator.normalize_characters(run.source_glyph_characters, %{
               white_space: :pre_line
             }) == run.source_glyph_characters
    end)
  end
end
