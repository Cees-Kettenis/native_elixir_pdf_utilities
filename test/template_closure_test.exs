defmodule NativeElixirPdfUtilities.TemplateClosureTest do
  use ExUnit.Case, async: false
  Code.require_file("support/png_fixture.ex", __DIR__)

  alias NativeElixirPdfUtilities.HtmlToPdf
  alias NativeElixirPdfUtilities.HtmlToPdf.{Font, HtmlParser, HtmlTree}

  alias NativeElixirPdfUtilities.Validators.{
    TemplatePatchValidator,
    TemplateRendererContextValidator
  }

  test "actual candidate resource callbacks reject a caller copy with mismatched stored identities" do
    alias NativeElixirPdfUtilities.Validators.TemplateExecutionValidator

    png =
      NativeElixirPdfUtilities.TestSupport.PngFixture.build(1, 1, 2, 8, 0, fn _, _ ->
        [255, 0, 0]
      end)

    resolver = fn _ ->
      frame = Process.get({TemplateExecutionValidator, :execution})

      if is_map(frame) and frame.mode == :execute and frame.busy and is_map(frame.capture) do
        tree = frame.capture.tree

        assert {:ok, _} =
                 TemplateExecutionValidator.original_prefix_provenance(tree, frame.capture.offset)

        invalid = put_in(tree.nodes[tree.root].node_id, make_ref())

        assert {:error, {:invalid_document, diagnostic}} =
                 TemplateExecutionValidator.original_prefix_provenance(
                   invalid,
                   frame.capture.offset
                 )

        assert diagnostic.reason == :invalid_document and diagnostic.stage == :html
        assert diagnostic.module == TemplateExecutionValidator
        assert diagnostic.operation == :template_candidate_projection
        assert diagnostic.source == "source_prefix"
        assert diagnostic.message =~ "canonical stored node identities"
        send(self(), :actual_candidate_identity_checked)
      end

      {:ok, png}
    end

    source =
      "<body><?start name='A'><p>Original</p><?end?><template for='A'><img src='candidate.png'><p>Kept</p></template></body>"

    assert {:ok, pdf} =
             HtmlToPdf.render(source, system_font_discovery: false, asset_resolver: resolver)

    assert {:ok, "Kept"} = NativeElixirPdfUtilities.Text.extract(pdf)
    assert_received :actual_candidate_identity_checked
    assert Process.get({TemplateExecutionValidator, :execution}) == nil
  end

  test "SVG image metadata with HTML integration content has no PDF glyph obligation during replacement" do
    source =
      "<body><?start name='A'><svg width='40' height='20'><desc>Original <span>Metadata</span></desc><rect width='40' height='20' fill='red'/></svg><?end?><template for='A'><p>Kept</p></template></body>"

    assert {:ok, pdf} = HtmlToPdf.render(source, system_font_discovery: false)
    assert {:ok, "Kept"} = NativeElixirPdfUtilities.Text.extract(pdf)
  end

  test "template source ownership stays separate from genuinely rendered page furniture" do
    source =
      "<body><?start name='A'><p>Discarded</p><?end?><template for='A'><p>Kept</p></template></body>"

    assert {:ok, pdf} =
             HtmlToPdf.render(source,
               system_font_discovery: false,
               page_size: {240, 160},
               margin: 20,
               page_furniture: [
                 header:
                   "<style>.header{color:#123456}</style><div class='header' style='font-size:8pt;line-height:10pt;margin:0'>Furniture {{page}}</div>",
                 footer:
                   "<div style='font-size:8pt;line-height:10pt;margin:0'>Footer {{page}} of {{pages}}</div>"
               ]
             )

    assert {:ok, text} = NativeElixirPdfUtilities.Text.extract(pdf, layout: false)
    for value <- ["Kept", "Furniture 1", "Footer 1 of 1"], do: assert(text =~ value)
    refute text =~ "Discarded"
    assert Process.get({TemplateRendererContextValidator, :preparation}) == nil

    assert Process.get(
             {NativeElixirPdfUtilities.Validators.TemplateExecutionValidator, :execution}
           ) == nil
  end

  test "composition entry points diagnose invalid source trees through the canonical validator" do
    for source <- [nil, %{type: :element, children: []}, %{type: :document, children: nil}] do
      assert {:error, {:invalid_document, diagnostic}} =
               NativeElixirPdfUtilities.HtmlToPdf.HtmlComposition.compose(source)

      assert diagnostic.stage == :html
      assert diagnostic.reason == :invalid_document
      assert diagnostic.module == NativeElixirPdfUtilities.Validators.HtmlCompositionValidator
      assert diagnostic.operation == :compose
      assert diagnostic.message =~ "document or document fragment"
    end
  end

  test "ordinary template attributes use the default composition context" do
    assert :ok = TemplatePatchValidator.validate_attributes(%{"id" => "ordinary"})

    assert {:error, {:unsupported_html, diagnostic}} =
             TemplatePatchValidator.validate_attributes(%{"for" => "target"})

    assert diagnostic.stage == :html
    assert diagnostic.reason == :unsupported_html
    assert diagnostic.module == TemplatePatchValidator
    assert diagnostic.message =~ "validated parser composition contexts"
  end

  test "Math observations retain Elixir text ownership without SVG painter metadata" do
    source = "<math><mi>x</mi></math>"
    assert {:ok, dom} = HtmlParser.parse_document_detailed(source)
    tree = HtmlTree.from_document(dom)
    budget = NativeElixirPdfUtilities.Validators.HtmlValidator.new_image_budget()

    assert {:ok, prepared} =
             NativeElixirPdfUtilities.Validators.FontValidator.with_budget(fn ->
               NativeElixirPdfUtilities.HtmlToPdf.ResourceContext.run([], budget, fn _ ->
                 HtmlToPdf.prepare_document(dom, [system_font_discovery: false], budget)
               end)
             end)

    assert {:ok, records} =
             TemplateRendererContextValidator.validate_prepared_math_glyph_runs(
               prepared.pages,
               tree
             )

    [run] = records |> Map.values() |> List.flatten()
    assert run.authored_text == "x"
    assert run.paint_basis == :native_text_painter
    assert run.glyph_ids == [Map.fetch!(run.font_face.cmap, run.displayed_unicode)]

    for key <- [:glyph_matrix, :pdf_operand, :source_pdf_sha256, :paint_observation] do
      refute Map.has_key?(run, key)
    end

    box =
      prepared.pages
      |> Stream.unfold(fn pending ->
        case pending do
          [] -> nil
          [box | rest] -> {box, Map.get(box, :boxes, []) ++ rest}
        end
      end)
      |> Enum.find(&(Map.get(&1, :math_source_fragments, []) != []))

    [fragment] = box.math_source_fragments
    altered = %{box | math_source_fragments: [%{fragment | offset: 1}]}

    assert {:error, {:invalid_document, diagnostic}} =
             TemplateRendererContextValidator.validate_prepared_math_glyph_runs([altered], tree)

    assert diagnostic.stage == :font
    assert diagnostic.reason == :invalid_document
    assert diagnostic.module == TemplateRendererContextValidator
    assert diagnostic.operation == :template_renderer_context
    assert diagnostic.source == "x"
    assert diagnostic.line > 0 and diagnostic.column > 0
    assert diagnostic.message =~ "original Unicode source fragment"
  end

  test "source diagnostics retain the original context when a prepared node loses its location" do
    assert {:ok, dom} = HtmlParser.parse_document_detailed("<p>Owned</p>")
    owner = Enum.find(Map.values(HtmlTree.from_document(dom).nodes), &(&1.type == :text))
    borrowed = %{owner | source: nil, document_id: make_ref()}

    for stage <- [:font, :style] do
      assert {:error, {:invalid_document, diagnostic}} =
               TemplateRendererContextValidator.validate_prepared_source(borrowed, owner, stage)

      assert diagnostic.stage == stage
      assert diagnostic.reason == :invalid_document
      assert diagnostic.module == TemplateRendererContextValidator
      assert diagnostic.operation == :template_renderer_context
      assert diagnostic.source == "Owned"
      assert diagnostic.line > 0 and diagnostic.column > 0
      assert diagnostic.message != ""
    end

    parent = Map.fetch!(HtmlTree.from_document(dom).nodes, owner.parent_id)

    assert {:error, {:invalid_document, diagnostic}} =
             TemplateRendererContextValidator.validate_prepared_glyph_runs(
               [%{parent | children: [borrowed]}],
               HtmlTree.from_document(dom)
             )

    assert diagnostic.stage == :font
    assert diagnostic.operation == :template_renderer_context
    assert diagnostic.source == "Owned"
    assert diagnostic.message =~ "original text source identity"
  end

  test "responsive width selection propagates the actual sizes limit diagnostic before fetching" do
    source = "<img src='fallback.png' srcset='candidate.png 100w' sizes='calc(1px/0)'>"

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             HtmlToPdf.render(source,
               system_font_discovery: false,
               asset_resolver: fn _ ->
                 flunk("a sizes limit failure must precede asset loading")
               end
             )

    assert diagnostic.stage == :style
    assert diagnostic.reason == :resource_limit_exceeded
    assert diagnostic.message != ""
  end

  test "an authored zero page dimension preserves the renderer's public layout failure" do
    for size <- ["0pt 100pt", "100pt 0pt"] do
      assert {:error, {:invalid_page_size, diagnostic}} =
               HtmlToPdf.render("<style>@page{size:#{size}}</style><p>Actual</p>",
                 system_font_discovery: false
               )

      assert diagnostic.stage == :layout
      assert diagnostic.reason == :invalid_page_size
      assert diagnostic.module == HtmlToPdf
      assert diagnostic.operation == :render
      assert diagnostic.message == "layout failed: invalid_page_size"
    end
  end

  test "candidate capture diagnoses an actual insertion plan which has not been consumed" do
    prefix = "<div><?marker name='A'></div>"
    token = "<template for='A'>"
    assert {:ok, dom} = HtmlParser.parse_detailed(prefix)
    tree = HtmlTree.from_document(dom)
    source = prefix <> token
    root = tree.nodes[tree.root]

    tree = %{
      tree
      | nodes: Map.put(tree.nodes, tree.root, %{root | source: %{root.source | text: source}})
    }

    origin = %{
      source: %{text: token, line: 1, column: String.length(prefix) + 1},
      offset: byte_size(prefix),
      tokenizer_context: "div",
      ancestors: []
    }

    assert :checked =
             TemplatePatchValidator.with_parser_invocation(source, fn _ ->
               TemplatePatchValidator.with_insertion_context(
                 tree,
                 tree.root,
                 "A",
                 origin,
                 fn plan ->
                   assert {:error, {:invalid_document, diagnostic}} =
                            TemplatePatchValidator.with_candidate_input(plan, tree, fn _ ->
                              flunk("unconsumed plans cannot capture candidate input")
                            end)

                   assert diagnostic.stage == :html
                   assert diagnostic.module == TemplatePatchValidator
                   assert diagnostic.message =~ "consumed insertion plan"

                   assert {:error, {:invalid_document, policy_diagnostic}} =
                            TemplatePatchValidator.with_renderer_policy_evidence(
                              plan,
                              tree,
                              fn _ ->
                                flunk("an insertion plan cannot supply renderer evidence")
                              end
                            )

                   assert policy_diagnostic.stage == :html
                   assert policy_diagnostic.module == TemplatePatchValidator
                   assert policy_diagnostic.operation == :parse
                   assert policy_diagnostic.message =~ "issued policy handle"
                   :checked
                 end
               )
             end)
  end

  test "font observations outside a preparation preserve the actual loaded font without issuing evidence" do
    assert {:ok, registry} = Font.load_registry(system_font_discovery: false)
    [loaded | _] = registry.fallback

    assert TemplateRendererContextValidator.observe_font_load(
             make_ref(),
             :fallback,
             0,
             0,
             %{},
             loaded
           ) == loaded

    refute Map.has_key?(loaded, :__template_font_outcome__)
    assert Process.get({TemplateRendererContextValidator, :preparation}) == nil
    entry = %{css: "p{color:red}"}
    assert TemplateRendererContextValidator.stylesheet_owner(%{}, entry) == entry
  end
end
