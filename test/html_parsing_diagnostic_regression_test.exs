defmodule NativeElixirPdfUtilities.Validators.HtmlParsingDiagnosticRegressionTest do
  use ExUnit.Case, async: false

  Code.require_file("support/png_fixture.ex", __DIR__)
  alias NativeElixirPdfUtilities.TestSupport.PngFixture
  alias NativeElixirPdfUtilities.Limits

  alias NativeElixirPdfUtilities.HtmlToPdf.{
    AssetLoader,
    CssParser,
    FontFallback,
    HtmlParser,
    HtmlTree,
    ResourceContext,
    Style
  }

  alias NativeElixirPdfUtilities.Validators.{
    CssValueValidator,
    HtmlTreeValidator,
    HtmlValidator,
    TemplatePatchValidator,
    TemplateRendererContextValidator
  }

  setup do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    :ok
  end

  test "malformed and oversized diagnostic CSS preserves the shared validator failure" do
    assert [record] = CssValueValidator.stylesheet_declaration_sources("p { width: bananas }")
    assert record.source.text == "width: bananas"

    for css <- [nil, <<255>>, "p {"] do
      assert {:error, {:invalid_css, diagnostic}} = CssParser.style_declaration_records(css)
      assert diagnostic.stage == :css and diagnostic.reason == :invalid_css
      assert is_binary(diagnostic.message) and diagnostic.message != ""

      assert {:error, {:invalid_css, ^diagnostic}} =
               CssValueValidator.stylesheet_declaration_sources(css)

      assert {:invalid_css, ^diagnostic} =
               CssValueValidator.declaration_error(css, {:error, {:invalid_css, diagnostic}})
    end

    Limits.install(Map.put(Limits.effective(), :max_css_source_bytes, 1))

    assert {:error, {:invalid_css, diagnostic}} =
             CssValueValidator.stylesheet_declaration_sources("p { width: bananas }")

    assert diagnostic.stage == :css and diagnostic.message =~ "max_css_source_bytes"
  end

  test "wrong patch parent and missing end marker are recoverable without changing the tree" do
    assert {:ok, document} =
             HtmlParser.parse_detailed("<div><?start name='A'><p>Old</p><?end></div>")

    tree = HtmlTree.from_document(document)
    assert {:ok, target} = HtmlTreeValidator.prepare_content_patch(tree, tree.root, "A")
    assert :ok = HtmlTreeValidator.validate_patch_boundaries(tree, target, true)

    missing_end = %{tree | nodes: Map.delete(tree.nodes, target.end)}

    for {input, target} <- [
          {tree, %{target | parent: make_ref()}},
          {missing_end, target},
          {tree, %{target | end: make_ref()}},
          {tree, %{target | removed: []}}
        ] do
      assert {:error, {:invalid_document, diagnostic}} =
               HtmlTreeValidator.validate_patch_boundaries(input, target, true)

      assert diagnostic.stage == :html and diagnostic.operation == :content_patch
      assert diagnostic.module == HtmlTreeValidator
      assert diagnostic.message =~ "boundaries or removed range"
    end

    assert HtmlTree.fetch(tree, tree.root) == document
  end

  test "insertion handles cannot substitute for consumed renderer-policy handles" do
    prefix = "<?marker name='A'><p>Original</p>"
    token = "<template for='A'>"
    source = prefix <> token
    assert {:ok, document} = HtmlParser.parse_detailed(prefix)
    tree = HtmlTree.from_document(document)
    tree = put_in(tree.nodes[tree.root].source, %{text: source, line: 1, column: 1})

    origin = %{
      source: %{text: token, line: 1, column: String.length(prefix) + 1},
      offset: byte_size(prefix),
      tokenizer_context: :document,
      ancestors: []
    }

    assert :ok = TemplatePatchValidator.validate_attributes(%{})

    assert {:error, {:unsupported_html, attributes}} =
             TemplatePatchValidator.validate_attributes(%{"for" => "A"})

    assert attributes.stage == :html and attributes.module == TemplatePatchValidator
    assert attributes.message =~ "validated parser composition contexts"

    assert :checked =
             TemplatePatchValidator.with_parser_invocation(source, fn _ ->
               TemplatePatchValidator.with_insertion_context(
                 tree,
                 tree.root,
                 "A",
                 origin,
                 fn plan ->
                   TemplatePatchValidator.with_planned_insertion(plan, tree, fn _ ->
                     assert {:error, {:invalid_document, diagnostic}} =
                              TemplatePatchValidator.with_renderer_policy_evidence(
                                plan,
                                tree,
                                fn _ ->
                                  flunk("insertion handle was used as renderer evidence")
                                end
                              )

                     assert diagnostic.stage == :html and diagnostic.operation == :parse
                     assert diagnostic.module == TemplatePatchValidator

                     assert diagnostic.message ==
                              "renderer evidence requires its issued policy handle"

                     assert {:error, {:unsupported_html, blocked}} =
                              TemplatePatchValidator.authorize_mutation(plan, tree)

                     assert blocked.source == token and blocked.line == 1
                     assert blocked.column == origin.source.column and blocked.operation == :parse
                     assert blocked.message =~ "no original source has been discarded"
                     :checked
                   end)
                 end
               )
             end)

    refute Process.get({TemplatePatchValidator, :invocation})
  end

  test "prepared source and glyph invariants diagnose altered identity without issuing evidence" do
    assert {:ok, document} = HtmlParser.parse_detailed("<p>Original</p>")
    tree = HtmlTree.from_document(document)
    [paragraph] = document.children
    [text] = paragraph.children
    altered = put_in(paragraph.source.text, "<p forged>")

    assert {:error, {:invalid_document, diagnostic}} =
             TemplateRendererContextValidator.validate_prepared_source(altered, paragraph, :style)

    assert diagnostic.stage == :style and diagnostic.operation == :template_renderer_context
    assert diagnostic.module == TemplateRendererContextValidator
    assert diagnostic.source == "<p forged>" and diagnostic.line == 1 and diagnostic.column == 1
    assert diagnostic.message =~ "different original node or source scope"

    assert {:error, {:invalid_document, wrong_type}} =
             TemplateRendererContextValidator.validate_prepared_source(
               %{text | type: :element},
               text,
               :font
             )

    assert wrong_type.stage == :font and wrong_type.source == "Original"
    assert wrong_type.message == "normal font run lost its original text source identity"

    assert {:ok, styled} = Style.compute_detailed(document, system_font_discovery: false)
    assert {:ok, resolved} = FontFallback.resolve(styled)
    [prepared] = resolved.children
    [run] = prepared.children
    changed_run = %{run | parent_id: make_ref()}

    assert {:error, {:invalid_document, glyph}} =
             TemplateRendererContextValidator.validate_prepared_glyph_runs(
               [%{prepared | children: [changed_run]}],
               tree
             )

    assert glyph.stage == :font and glyph.operation == :template_renderer_context
    assert glyph.module == TemplateRendererContextValidator
    assert glyph.source == text.source.text and glyph.line == text.source.line
    assert glyph.column == text.source.column
    assert glyph.message == "normal font run lost its original text source identity"

    for malformed <- [
          Map.delete(run, :source),
          %{run | source: nil},
          %{run | source: "lost metadata"},
          %{run | source: %{text: 42}},
          %{run | source: %{text: "wrong", line: 0, column: 1}}
        ] do
      assert {:error, {:invalid_document, missing_source}} =
               TemplateRendererContextValidator.validate_prepared_glyph_runs(
                 [%{prepared | children: [malformed]}],
                 tree
               )

      assert missing_source.stage == :font and
               missing_source.operation == :template_renderer_context

      assert missing_source.module == TemplateRendererContextValidator
      assert missing_source.message == "normal font run lost its original text source identity"
      assert missing_source.source == text.source.text and missing_source.line == text.source.line
      assert missing_source.column == text.source.column

      assert {:error, {:invalid_document, altered_style}} =
               TemplateRendererContextValidator.validate_prepared_source(malformed, text, :style)

      assert altered_style.stage == :style and altered_style.source == text.source.text
      assert altered_style.line == text.source.line and altered_style.column == text.source.column
    end

    assert {:error, {:invalid_document, no_context}} =
             TemplateRendererContextValidator.validate_prepared_source(
               %{run | source: nil, parent_id: make_ref()},
               %{text | source: nil},
               :font
             )

    assert no_context.message == "normal font run lost its original text source identity"
    refute Map.has_key?(no_context, :source)
    refute Map.has_key?(no_context, :line)
    refute Map.has_key?(no_context, :column)

    assert {:ok, records} =
             TemplateRendererContextValidator.validate_prepared_glyph_runs(
               resolved.children,
               tree
             )

    assert records[text.node_id] |> hd() |> Map.fetch!(:authored_text) == "Original"
    refute Process.get({TemplateRendererContextValidator, :preparation})
  end

  test "private owner markers cannot survive outside an active issued preparation" do
    assert {:ok, document} = HtmlParser.parse_detailed("<p>Original</p>")
    [paragraph] = document.children
    opts = [__template_source_owner__: make_ref(), system_font_discovery: false]

    assert TemplateRendererContextValidator.source_owner(paragraph, opts) ==
             [system_font_discovery: false]

    assert :ok = TemplateRendererContextValidator.observe(paragraph, %{})
    refute Process.get({TemplateRendererContextValidator, :preparation})
  end

  test "an actual issued marker expires and cannot bind a later resource observation" do
    png = PngFixture.build(1, 1, 2, 8, 0, fn _, _ -> [255, 0, 0] end)
    prefix = "<?marker name='A'><p>Original</p><img src='pixel.png'>"
    token = "<template for='A'>"
    source = prefix <> token
    assert {:ok, document} = HtmlParser.parse_detailed(prefix)
    tree = HtmlTree.from_document(document)
    tree = put_in(tree.nodes[tree.root].source, %{text: source, line: 1, column: 1})

    origin = %{
      source: %{text: token, line: 1, column: String.length(prefix) + 1},
      offset: byte_size(prefix),
      tokenizer_context: :document,
      ancestors: []
    }

    opts = [system_font_discovery: false, assets: %{"pixel.png" => {:bytes, png}}]

    prepare = fn options, consumer ->
      ResourceContext.run(
        [authorization_root: System.tmp_dir!()],
        HtmlValidator.new_image_budget(),
        fn _ ->
          TemplatePatchValidator.with_parser_invocation(source, fn _ ->
            TemplatePatchValidator.with_insertion_context(tree, tree.root, "A", origin, fn plan ->
              TemplatePatchValidator.with_planned_insertion(plan, tree, fn _ ->
                TemplatePatchValidator.with_renderer_policy_context(plan, tree, options, consumer)
              end)
            end)
          end)
        end
      )
    end

    Code.ensure_loaded!(AssetLoader)
    assert :erlang.trace_pattern({AssetLoader, :resolve, 4}, true, [:local]) == 1

    try do
      task =
        Task.async(fn ->
          receive do
            :prepare -> prepare.(opts, fn _ -> :prepared end)
          end
        end)

      :erlang.trace(task.pid, true, [:call])
      send(task.pid, :prepare)
      assert :prepared = Task.await(task, 10_000)

      assert_receive {:trace, _, :call,
                      {AssetLoader, :resolve, ["pixel.png", :image, issued_opts, _]}}

      assert is_reference(issued_opts[:__template_source_owner__])
      [_, paragraph, _] = document.children
      cleaned = TemplateRendererContextValidator.source_owner(paragraph, issued_opts)
      refute Keyword.has_key?(cleaned, :__template_source_owner__)
      refute Process.get({TemplateRendererContextValidator, :preparation})

      resolver = fn %{reference: "pixel.png"} ->
        replay = [
          assets: %{"replayed.png" => {:bytes, png}},
          __template_source_owner__: issued_opts[:__template_source_owner__]
        ]

        assert {:ok, ^png} = AssetLoader.resolve("replayed.png", :image, replay)
        {:ok, png}
      end

      second_opts = opts |> Keyword.delete(:assets) |> Keyword.put(:asset_resolver, resolver)

      assert :checked =
               prepare.(second_opts, fn policy ->
                 TemplatePatchValidator.with_renderer_policy_evidence(policy, tree, fn evidence ->
                   [replayed] =
                     Enum.filter(evidence.references, &(&1.reference == "replayed.png"))

                   assert replayed.owner == :unbound_global_preparation
                   [normal] = Enum.filter(evidence.references, &(&1.reference == "pixel.png"))
                   assert normal.owner.tag == "img" and normal.encoded_source == png
                   assert length(evidence.pending_checks) == 7
                   :checked
                 end)
               end)

      refute Process.get({TemplateRendererContextValidator, :preparation})
    after
      :erlang.trace_pattern({AssetLoader, :resolve, 4}, false, [:local])
    end
  end

  test "noframes placement uses the same native content-model authority" do
    for context <- [:document, :after_doctype, :after_head, "html", "head", "body", "frameset"] do
      assert HtmlValidator.valid_html_child?(context, "noframes")
    end

    refute HtmlValidator.valid_html_child?("div", "noframes")

    assert {:error, {:unsupported_html, diagnostic}} =
             HtmlParser.parse_document_detailed(
               "<body><div><noframes>Fallback</noframes></div></body>"
             )

    assert diagnostic.stage == :html and diagnostic.reason == :unsupported_html
    assert diagnostic.source == "<noframes>" and diagnostic.line == 1 and diagnostic.column == 12
    assert diagnostic.message =~ "noframes"
  end
end
