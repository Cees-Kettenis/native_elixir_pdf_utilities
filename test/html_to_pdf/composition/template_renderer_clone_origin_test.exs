alias NativeElixirPdfUtilities.TestSupport.Paths

defmodule NativeElixirPdfUtilities.Validators.TemplateRendererCloneOriginTest do
  use ExUnit.Case, async: false
  Code.require_file(Paths.support("png_fixture.ex"))

  alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, HtmlTree, ResourceContext, Style}
  alias NativeElixirPdfUtilities.TestSupport.PngFixture

  alias NativeElixirPdfUtilities.Validators.{
    HtmlValidator,
    TemplatePatchValidator,
    TemplateRendererContextValidator
  }

  @opts [system_font_discovery: false, forms: :static]

  test "selected-content clone glyphs retain their lexical source and fresh placement" do
    source =
      "<select style='appearance:base-select'><button><selectedcontent></selectedcontent></button><option selected><span style='text-transform:uppercase'>Choice</span></option></select>"

    assert :checked =
             prepare(source, @opts, fn plan, tree, evidence ->
               clones =
                 Enum.filter(evidence.glyph_runs, fn {id, _} ->
                   Map.has_key?(Map.get(evidence, :clone_origins, %{}), id)
                 end)

               assert [{clone_id, [run]}] = clones
               original = tree.nodes[run.original_owner.node_id]
               assert original.text == "Choice"
               assert run.resolved_text == "CHOICE"
               assert run.authored_text == "Choice"
               assert clone_id != original.node_id
               assert run.owner.node_id == clone_id
               assert run.owner.source == original.source
               assert run.clone_origin.original.node_id == original.node_id
               assert run.clone_origin.placement.parent_id != original.parent_id
               assert run.clone_origin.policy == :actual_normal_clone_origin_chain
               [own | _] = run.declaration_contexts
               assert own.owner.node_id != original.parent_id
               assert [declaration] = own.declarations
               assert declaration.source.owner.node_id == original.parent_id
               assert declaration.source.text == "text-transform:uppercase"
               assert declaration.context_kind == :composition_clone
               assert declaration.preparation_scope == :original_prefix
               assert declaration.clone_origin.original.node_id == original.parent_id
               assert declaration.clone_origin.placement.node_id == own.owner.node_id
               assert declaration.context.node_id == own.owner.node_id
               assert declaration.original_context.node_id == original.parent_id

               assert {:error, {:unsupported_html, diagnostic}} =
                        TemplatePatchValidator.authorize_mutation(plan, tree)

               assert diagnostic.message =~ "original_source_policy"
               :checked
             end)

    assert Process.get({TemplateRendererContextValidator, :preparation}) == nil
  end

  test "successful clone resource loading retains encoded bytes and original owner without borrowing permission" do
    png = PngFixture.build(1, 1, 2, 8, 0, fn _, _ -> [255, 0, 0] end)

    source =
      "<select style='appearance:base-select'><button><selectedcontent></selectedcontent></button><option selected><img src='pixel.png' alt='Chosen'></option></select>"

    opts = Keyword.put(@opts, :assets, %{"pixel.png" => {:bytes, png}})

    assert :checked =
             prepare(source, opts, fn _plan, tree, evidence ->
               cloned = Enum.find(evidence.references, &is_map(Map.get(&1, :clone_origin)))
               assert is_map(cloned)
               assert cloned.reference == "pixel.png"
               assert cloned.encoded_source == png
               assert cloned.authority_basis == :ordinary_renderer_options
               assert cloned.descriptor == nil
               assert cloned.authorization_context == ResourceContext.current()
               assert cloned.owner.node_id != cloned.original_owner.node_id

               assert cloned.original_owner.source ==
                        tree.nodes[cloned.original_owner.node_id].source

               assert cloned.resource_context == ResourceContext.current()
               assert cloned.clone_origin.placement.node_id == cloned.owner.node_id
               :checked
             end)
  end

  test "nested cloned shadow selection resolves each immediate clone chain" do
    source =
      "<select style='appearance:base-select'><button><selectedcontent></selectedcontent></button><option><span><template shadowrootmode=open shadowrootclonable><style>select::picker-icon{display:none}</style><select style='appearance:base-select'><button><selectedcontent></selectedcontent></button><option selected><strong>Inner choice</strong></option></select></template>Outer light</span></option></select>"

    assert :checked =
             prepare(source, @opts, fn _plan, tree, evidence ->
               twice =
                 Enum.filter(Map.get(evidence, :clone_origins, %{}), fn {_id, binding} ->
                   Map.has_key?(
                     Map.get(evidence, :clone_origins, %{}),
                     binding.immediate_origin_id
                   )
                 end)

               assert twice != []

               assert Enum.all?(twice, fn {_id, binding} ->
                        Map.has_key?(tree.nodes, binding.original.node_id)
                      end)

               inner =
                 Enum.find(evidence.glyph_runs, fn {id, runs} ->
                   Map.has_key?(Map.get(evidence, :clone_origins, %{}), id) and
                     hd(runs).authored_text == "Inner choice"
                 end)

               assert {_id, [run]} = inner
               assert run.original_owner.source.text == "Inner choice"
               :checked
             end)
  end

  test "resource callback reentry cannot issue clone source ownership or replace outer bindings" do
    source =
      "<select style='appearance:base-select'><button><selectedcontent></selectedcontent></button><option selected><img src='pixel.png'><span>Choice</span></option></select>"

    png = PngFixture.build(1, 1, 2, 8, 0, fn _, _ -> [255, 0, 0] end)

    {:ok, foreign} =
      HtmlParser.parse_document_detailed(
        "<style>select::picker-icon{display:none}</style><select style='appearance:base-select'><button><selectedcontent></selectedcontent></button><option><span>Foreign</span></option></select>"
      )

    resolver = fn _request ->
      before =
        Map.get(
          Process.get({TemplateRendererContextValidator, :preparation}),
          :clone_bindings,
          %{}
        )

      assert {:ok, _} = Style.compute_detailed(foreign, @opts, HtmlValidator.new_image_budget())

      assert Map.get(
               Process.get({TemplateRendererContextValidator, :preparation}),
               :clone_bindings,
               %{}
             ) ==
               before

      {:ok, png}
    end

    assert :checked =
             prepare(source, Keyword.put(@opts, :asset_resolver, resolver), fn _plan,
                                                                               _tree,
                                                                               evidence ->
               assert Enum.all?(Map.get(evidence, :clone_origins, %{}), fn {_id, binding} ->
                        binding.original.source.text != "Foreign"
                      end)

               assert Enum.any?(evidence.glyph_runs, fn {id, _runs} ->
                        Map.has_key?(Map.get(evidence, :clone_origins, %{}), id)
                      end)

               :checked
             end)

    assert Process.get({TemplateRendererContextValidator, :preparation}) == nil
  end

  test "clone fallback failure keeps the unsupported source diagnostic and cleans invocation state" do
    source =
      "<select style='appearance:base-select'><button><selectedcontent></selectedcontent></button><option selected><span>&#x10FFFF;</span></option></select>"

    assert {:error, {:unsupported_glyph, diagnostic}} =
             prepare(source, Keyword.put(@opts, :unsupported_glyphs, :error), fn _, _, _ ->
               flunk("unsupported clone glyph issued evidence")
             end)

    assert diagnostic.stage == :font and diagnostic.operation == :resolve_fonts
    assert diagnostic.source == <<0x10FFFF::utf8>>
    assert diagnostic.message =~ "U+10FFFF"
    assert Process.get({TemplateRendererContextValidator, :preparation}) == nil

    assert :checked =
             prepare("<p>After</p>", @opts, fn _, _, evidence ->
               assert Map.get(evidence, :clone_origins, %{}) == %{}
               :checked
             end)
  end

  test "cloned shadow fonts retain the exact declaration provider in source observations" do
    mono = File.read!(Paths.project("priv/fonts/dejavu/DejaVuSansMono.ttf"))

    source =
      "<select style='appearance:base-select'><button><selectedcontent></selectedcontent></button><option selected><span><template shadowrootmode=open shadowrootclonable><style>@font-face{font-family:Scoped;src:url(mono.ttf)}span{font-family:Scoped}</style><span>Scoped choice</span></template></span></option></select>"

    opts = Keyword.put(@opts, :assets, %{"mono.ttf" => {:bytes, mono}})

    assert :checked =
             prepare(source, opts, fn _plan, _tree, evidence ->
               {_id, [run]} =
                 Enum.find(evidence.glyph_runs, fn {_id, runs} ->
                   hd(runs).authored_text == "Scoped choice" and
                     Map.has_key?(hd(runs), :clone_origin)
                 end)

               assert run.font_face.data == mono
               assert is_map(run.font_source_outcome)
               assert run.font_source_outcome.basis == :validated_css_candidate
               assert run.font_source_outcome.shadow_scope_id == run.owner.tree_scope_id
               assert run.font_source_outcome.encoded_source == mono
               :checked
             end)
  end

  defp prepare(authored, opts, consumer) do
    prefix =
      "<head><style>select::picker-icon{display:none}</style></head><body><div><?marker name='A'>#{authored}</div>"

    token = "<template for='A'>"
    {:ok, dom} = HtmlParser.parse_document_detailed(prefix <> "</body>")
    [html] = dom.children
    [_, body] = html.children
    tree = HtmlTree.from_document(dom)
    source = prefix <> token
    owner = tree.nodes[tree.root]

    tree = %{
      tree
      | nodes:
          Map.put(tree.nodes, tree.root, %{owner | source: %{text: source, line: 1, column: 1}})
    }

    origin = %{
      source: %{text: token, line: 1, column: String.length(prefix) + 1},
      offset: byte_size(prefix),
      tokenizer_context: "body",
      ancestors: []
    }

    ResourceContext.run([], HtmlValidator.new_image_budget(), fn _ ->
      TemplatePatchValidator.with_parser_invocation(source, fn _ ->
        TemplatePatchValidator.with_insertion_context(tree, body.node_id, "A", origin, fn plan ->
          TemplatePatchValidator.with_planned_insertion(plan, tree, fn _ ->
            TemplatePatchValidator.with_renderer_policy_context(plan, tree, opts, fn policy ->
              TemplatePatchValidator.with_renderer_policy_evidence(policy, tree, fn evidence ->
                consumer.(plan, tree, evidence)
              end)
            end)
          end)
        end)
      end)
    end)
  end
end
