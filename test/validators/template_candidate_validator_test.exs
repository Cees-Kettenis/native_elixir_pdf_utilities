alias NativeElixirPdfUtilities.TestSupport.Paths

defmodule NativeElixirPdfUtilities.Validators.TemplateCandidateValidatorTest do
  use ExUnit.Case, async: false

  Code.require_file(Paths.support("png_fixture.ex"))
  alias NativeElixirPdfUtilities.TestSupport.PngFixture
  alias NativeElixirPdfUtilities.{Diagnostics, FileReader, Limits}

  alias NativeElixirPdfUtilities.HtmlToPdf.{
    AssetLoader,
    Font,
    HtmlParser,
    HtmlTree,
    ResourceContext
  }

  alias NativeElixirPdfUtilities.Validators.{
    HtmlCssSourceValidator,
    HtmlValidator,
    ResourceValidator,
    TemplateRendererContextValidator,
    TemplateCandidateValidator,
    TemplatePatchValidator
  }

  setup do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    :ok
  end

  test "read-only capture preserves invalid original source and exact candidate identities in a separate target scope" do
    {source, tree, parent, origin} =
      fixture(
        "<div><?start name='A'><p style='width:bananas'>Original</p><?end?></div>",
        "\r\n<p id='new' style='color:red'>𝑥 &amp; New</p>"
      )

    assert :checked =
             with_plan(source, tree, parent, origin, fn plan ->
               TemplatePatchValidator.with_candidate_projection(plan, tree, fn handle ->
                 TemplatePatchValidator.with_candidate_evidence(handle, tree, fn evidence ->
                   [id] =
                     Enum.filter(
                       evidence.inserted_roots,
                       &(evidence.prospective.nodes[&1][:tag] == "p")
                     )

                   original = evidence.original.nodes[id]
                   prospective = evidence.prospective.nodes[id]
                   assert original.source.text == "<p id='new' style='color:red'>"
                   assert original.source.line == 2 and original.source.column == 1
                   assert original.parent_id == evidence.original_fragment
                   assert original.tree_scope_id == evidence.original_fragment

                   assert prospective.source == original.source and
                            prospective.node_id == original.node_id

                   assert prospective.parent_id != original.parent_id
                   assert prospective.tree_scope_id != original.tree_scope_id
                   assert prospective.document_id == original.document_id
                   assert evidence.source_bindings[id].original.parent_id == original.parent_id

                   assert evidence.source_bindings[id].prospective.parent_id ==
                            prospective.parent_id

                   assert HtmlTree.fetch(evidence.prospective, id).children
                          |> hd()
                          |> Map.fetch!(:text) == "𝑥 & New"

                   invalid =
                     Enum.find(
                       Map.values(evidence.original.nodes),
                       &(&1[:attributes]["style"] == "width:bananas")
                     )

                   assert invalid.parent_id != nil and invalid.source.text =~ "bananas"
                   assert evidence.prospective.nodes[invalid.node_id].parent_id == nil

                   assert evidence.content_model_policy ==
                            :validated_native_parse_in_observed_target_context

                   assert evidence.style_policy ==
                            :requires_actual_prospective_cascade_preparation

                   assert evidence.font_policy == :requires_actual_prospective_glyph_preparation

                   assert evidence.resource_policy ==
                            :requires_actual_prospective_authorized_outcomes

                   assert length(evidence.pending_checks) == 7

                   assert {:error, {:unsupported_html, diagnostic}} =
                            TemplatePatchValidator.authorize_mutation(plan, tree)

                   assert diagnostic.message =~ "original_source_policy"

                   target_parent = evidence.prospective_scope.parent

                   assert tree.nodes[target_parent].children !=
                            evidence.prospective.nodes[target_parent].children

                   :checked
                 end)
               end)
             end)
  end

  test "table parsing creates an implied tbody without inventing an authored source identity" do
    {source, tree, parent, origin} =
      fixture("<table><?marker name='A'></table>", "<tr><td>New</td></tr>")

    assert :checked =
             with_plan(source, tree, parent, origin, fn plan ->
               TemplatePatchValidator.with_candidate_projection(plan, tree, fn handle ->
                 TemplatePatchValidator.with_candidate_evidence(handle, tree, fn evidence ->
                   [group] = evidence.inserted_roots
                   assert evidence.prospective.nodes[group].tag == "tbody"
                   assert evidence.prospective.nodes[group].source.text == ""
                   refute Map.has_key?(evidence.source_bindings, group)
                   [row] = HtmlTree.children(evidence.prospective, group)
                   assert row.tag == "tr" and row.source.text == "<tr>"

                   assert evidence.original.nodes[row.node_id].parent_id ==
                            evidence.original_fragment

                   assert row.parent_id == group
                   [cell] = row.children
                   assert cell.node_id == evidence.original.nodes[cell.node_id].node_id
                   assert cell.source == evidence.original.nodes[cell.node_id].source
                   :checked
                 end)
               end)
             end)
  end

  test "original executable URL fails before projection and target content rejects invalid prospective children" do
    {source, tree, parent, origin} =
      fixture(
        "<div><?marker name='A'></div>",
        "\r\n<p style='background-image:url(\"javascript:run()\")'>Invalid</p>"
      )

    assert {:error, {:unsupported_html, diagnostic}} =
             with_plan(source, tree, parent, origin, fn plan ->
               TemplatePatchValidator.with_candidate_projection(plan, tree, fn _ ->
                 flunk("invalid original URL projected")
               end)
             end)

    assert diagnostic.stage == :css and diagnostic.operation == :source_references
    assert diagnostic.module == NativeElixirPdfUtilities.Validators.CssReferenceValidator
    assert diagnostic.message =~ "Active URL schemes"
    assert diagnostic.source == "url(\"javascript:run()\")"
    assert diagnostic.line == 2 and diagnostic.column == 28

    {source, tree, parent, origin} =
      fixture("<select><?marker name='A'></select>", "<p>Not an option</p>")

    assert {:error, {:unsupported_html, diagnostic}} =
             with_plan(source, tree, parent, origin, fn plan ->
               TemplatePatchValidator.with_candidate_projection(plan, tree, fn _ ->
                 flunk("invalid select projected")
               end)
             end)

    assert diagnostic.stage == :html and diagnostic.source == "<p>"

    assert diagnostic.line == 1 and
             diagnostic.column == origin.source.column + String.length(origin.source.text)
  end

  test "future styles and missing targets cannot supply a prospective context" do
    {source, tree, parent, origin} = fixture("<div><?marker name='A'></div>", "<p>New</p>")

    assert :checked =
             with_plan(source, tree, parent, origin, fn plan ->
               TemplatePatchValidator.with_candidate_projection(plan, tree, fn handle ->
                 TemplatePatchValidator.with_candidate_evidence(handle, tree, fn evidence ->
                   assert binary_part(
                            source,
                            evidence.captured_finish,
                            byte_size(source) - evidence.captured_finish
                          ) == "<style>p{color:yellow}</style></body>"

                   refute Enum.any?(
                            Map.values(evidence.original.nodes),
                            &(&1[:source][:text] == "<style>" and
                                &1[:source][:column] > origin.source.column)
                          )

                   refute Enum.any?(
                            Map.values(evidence.prospective.nodes),
                            &(&1[:tag] == "template" and &1[:attributes]["for"] == "A")
                          )

                   :checked
                 end)
               end)
             end)

    {source, tree, parent, origin} = fixture("<div>No marker</div>", "<p>Still authored</p>")

    assert {:error, {:unsupported_html, diagnostic}} =
             with_plan(source, tree, parent, origin, fn plan ->
               TemplatePatchValidator.with_candidate_projection(plan, tree, fn _ ->
                 flunk("future target authority")
               end)
             end)

    assert diagnostic.module == TemplateCandidateValidator
    assert diagnostic.message =~ "no earlier target" and diagnostic.source == "<template for='A'>"
  end

  test "projection evidence is invocation-owned, single-use, expires and restores nested frames" do
    {source, tree, parent, origin} = fixture("<div><?marker name='A'></div>", "<p>New</p>")

    assert :checked =
             with_plan(source, tree, parent, origin, fn plan ->
               expired =
                 TemplatePatchValidator.with_candidate_projection(plan, tree, fn handle ->
                   assert {:error, {:invalid_document, _}} =
                            TemplatePatchValidator.with_candidate_evidence(
                              make_ref(),
                              tree,
                              fn _ -> flunk("forged") end
                            )

                   TemplatePatchValidator.with_candidate_projection(plan, tree, fn inner ->
                     assert {:error, {:invalid_document, diagnostic}} =
                              TemplatePatchValidator.with_candidate_evidence(handle, tree, fn _ ->
                                flunk("wrong active frame")
                              end)

                     assert diagnostic.message =~ "different active capture"

                     assert :inner =
                              TemplatePatchValidator.with_candidate_evidence(inner, tree, fn _ ->
                                :inner
                              end)
                   end)

                   assert :checked =
                            TemplatePatchValidator.with_candidate_evidence(handle, tree, fn _ ->
                              :checked
                            end)

                   assert {:error, {:invalid_document, diagnostic}} =
                            TemplatePatchValidator.with_candidate_evidence(handle, tree, fn _ ->
                              flunk("reused")
                            end)

                   assert diagnostic.message =~ "already been consumed"
                   handle
                 end)

               assert {:error, {:invalid_document, diagnostic}} =
                        TemplatePatchValidator.with_candidate_evidence(expired, tree, fn _ ->
                          flunk("expired")
                        end)

               assert diagnostic.message =~ "expired"

               error = Diagnostics.error(:html, :unsupported_html, "consumer stopped")

               assert ^error =
                        TemplatePatchValidator.with_candidate_projection(plan, tree, fn handle ->
                          TemplatePatchValidator.with_candidate_evidence(handle, tree, fn _ ->
                            error
                          end)
                        end)

               assert_raise RuntimeError, "candidate stopped", fn ->
                 TemplatePatchValidator.with_candidate_projection(plan, tree, fn handle ->
                   TemplatePatchValidator.with_candidate_evidence(handle, tree, fn _ ->
                     raise "candidate stopped"
                   end)
                 end)
               end

               assert Process.get({TemplatePatchValidator, :candidate_projection}) == nil
               :checked
             end)
  end

  test "changed source trees, policy configuration and shared budgets reject projection evidence" do
    {source, tree, parent, origin} = fixture("<div><?marker name='A'></div>", "<p>New</p>")
    limits = Limits.effective()

    assert :checked =
             with_plan(source, tree, parent, origin, fn plan ->
               TemplatePatchValidator.with_candidate_projection(plan, tree, fn handle ->
                 root = tree.nodes[tree.root]

                 changed = %{
                   tree
                   | nodes:
                       Map.put(tree.nodes, tree.root, %{
                         root
                         | source: %{root.source | text: root.source.text <> "changed"}
                       })
                 }

                 assert {:error, {:invalid_document, _}} =
                          TemplatePatchValidator.with_candidate_evidence(handle, changed, fn _ ->
                            flunk("changed tree")
                          end)

                 Limits.install(Map.put(limits, :max_css_numeric_magnitude, 1))

                 assert {:error, {:invalid_document, diagnostic}} =
                          TemplatePatchValidator.with_candidate_evidence(handle, tree, fn _ ->
                            flunk("changed limits")
                          end)

                 assert diagnostic.message =~ "configuration changed"
                 Limits.install(limits)

                 assert :checked =
                          TemplatePatchValidator.with_candidate_evidence(handle, tree, fn _ ->
                            :checked
                          end)
               end)

               :checked
             end)

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             with_plan(source, tree, parent, origin, fn plan ->
               Limits.install(Map.put(limits, :max_html_tree_work, 1))

               TemplatePatchValidator.with_candidate_projection(plan, tree, fn _ ->
                 flunk("unbounded capture")
               end)
             end)

    assert diagnostic.stage == :html and diagnostic.message =~ "max_html_tree_work"
    Limits.install(limits)
  end

  test "nested replacement remains diagnosed and no caller candidate map can issue context" do
    {source, tree, parent, origin} =
      fixture("<div><?marker name='A'></div>", "<template for='A'><p>Nested</p></template>")

    assert {:error, {:unsupported_html, diagnostic}} =
             with_plan(source, tree, parent, origin, fn plan ->
               TemplatePatchValidator.with_candidate_projection(plan, tree, fn _ ->
                 flunk("nested mutation bypass")
               end)
             end)

    assert diagnostic.stage == :html and diagnostic.operation == :parse
    assert diagnostic.source == "<template for='A'>"
    assert diagnostic.column == origin.source.column + String.length(origin.source.text)
    assert diagnostic.message =~ "inert fallback"

    {source, tree, parent, origin} = fixture("<div><?marker name='A'></div>", "<p>New</p>")

    assert :checked =
             with_plan(source, tree, parent, origin, fn plan ->
               assert {:ok, captured} = HtmlParser.capture_template_candidate(plan, tree)
               [id] = captured.inserted_roots
               prospective = captured.projected.nodes[id]

               forged = %{
                 captured
                 | projected: %{
                     captured.projected
                     | nodes:
                         Map.put(captured.projected.nodes, id, %{
                           prospective
                           | source: %{prospective.source | text: "<p forged>"}
                         })
                   }
               }

               context = TemplatePatchValidator.with_candidate_input(plan, tree, & &1.plan)

               assert {:error, {:unsupported_html, binding_error}} =
                        TemplateCandidateValidator.prepare(forged, tree, context)

               assert binding_error.stage == :html and
                        binding_error.operation == :template_candidate_projection

               assert binding_error.source == "<p forged>" and
                        binding_error.message =~ "exact authored token identity"

               assert {:error, {:invalid_document, _}} =
                        TemplatePatchValidator.with_candidate_projection(captured, tree, fn _ ->
                          flunk("raw candidate receipt")
                        end)

               :checked
             end)
  end

  test "CSS provenance bridges only a consumed live candidate without rewriting lexical parents" do
    {source, tree, parent, origin} =
      fixture("<div><?marker name='A'></div>", "<p style='width:2px'>New</p>")

    escaped =
      with_plan(source, tree, parent, origin, fn plan ->
        TemplatePatchValidator.with_candidate_projection(plan, tree, fn handle ->
          assert {:error, {:invalid_document, unconsumed}} =
                   HtmlCssSourceValidator.prepare_candidate_projection(handle, tree)

          assert unconsumed.operation == :parse
          assert unconsumed.message == "source projection requires a consumed candidate frame"

          assert :checked =
                   TemplatePatchValidator.with_candidate_evidence(handle, tree, fn candidate ->
                     [id] = candidate.inserted_roots
                     original = candidate.original.nodes[id]
                     prospective = candidate.prospective.nodes[id]
                     refute original.parent_id == prospective.parent_id
                     refute original.tree_scope_id == prospective.tree_scope_id

                     assert {:error, {:invalid_document, wrong_lexical_context}} =
                              HtmlCssSourceValidator.prepare_prefix(
                                HtmlTree.fetch(candidate.prospective, candidate.prospective.root),
                                candidate.captured_finish
                              )

                     assert wrong_lexical_context.stage == :html
                     assert wrong_lexical_context.message =~ "original"

                     assert {:ok, bridge} =
                              HtmlCssSourceValidator.prepare_candidate_projection(handle, tree)

                     assert bridge.policy ==
                              :original_lexical_sources_with_distinct_prospective_placements

                     assert bridge.captured_finish == candidate.captured_finish
                     assert bridge.original_scope == candidate.original_scope
                     assert bridge.prospective_scope == candidate.prospective_scope
                     assert bridge.placements[id].original.parent_id == original.parent_id
                     assert bridge.placements[id].original.tree_scope_id == original.tree_scope_id
                     assert bridge.placements[id].prospective.parent_id == prospective.parent_id

                     assert bridge.placements[id].prospective.tree_scope_id ==
                              prospective.tree_scope_id

                     assert bridge.inline[id].owner.parent_id == original.parent_id
                     assert bridge.inline[id].owner.tree_scope_id == original.tree_scope_id
                     assert bridge.records[id] == bridge.original_provenance.records[id]
                     [record] = bridge.records[id]
                     assert record.declaration == {"width", "2px"}
                     assert record.source.text == "width:2px"
                     assert bridge.insertion_contexts[id] == :template_insertion_context
                     assert length(bridge.pending_checks) == 7
                     refute Map.has_key?(bridge, :certificate)
                     :checked
                   end)

          handle
        end)
      end)

    assert {:error, {:invalid_document, expired}} =
             HtmlCssSourceValidator.prepare_candidate_projection(escaped, tree)

    assert expired.operation == :parse and expired.message =~ "expired"

    assert {:error, {:invalid_document, forged}} =
             HtmlCssSourceValidator.prepare_candidate_projection(%{source: source}, tree)

    assert forged.message =~ "forged"
  end

  test "prospective composition clones keep separate lexical and execution source contexts" do
    authored =
      "<select style='appearance:base-select'><button><selectedcontent></selectedcontent></button><option selected><span style='width:2rem;text-transform:uppercase'>Choice</span></option></select>"

    {source, tree, parent, origin} =
      fixture("<div style='font-size:20px'><?marker name='A'></div>", authored)

    opts = [system_font_discovery: false, forms: :static]

    assert :checked =
             ResourceContext.run([], HtmlValidator.new_image_budget(), fn _ ->
               with_plan(source, tree, parent, origin, fn plan ->
                 TemplatePatchValidator.with_candidate_projection(plan, tree, fn handle ->
                   TemplatePatchValidator.with_prospective_renderer_policy_context(
                     handle,
                     tree,
                     opts,
                     fn policy ->
                       TemplatePatchValidator.with_renderer_policy_evidence(
                         policy,
                         tree,
                         fn evidence ->
                           assert evidence.preparation_scope == :prospective_insertion

                           cloned =
                             Enum.find_value(evidence.glyph_runs, fn {id, runs} ->
                               if Map.has_key?(evidence.clone_origins, id) and
                                    hd(runs).authored_text == "Choice",
                                  do: hd(runs)
                             end)

                           assert cloned.resolved_text == "CHOICE"
                           [own | _] = cloned.declaration_contexts
                           assert own.owner.tag == "span"
                           assert [width, transform] = own.declarations
                           assert width.declaration == {"width", "2rem"}
                           assert transform.declaration == {"text-transform", "uppercase"}

                           for declaration <- own.declarations do
                             assert declaration.context_kind == :composition_clone
                             assert declaration.preparation_scope == :prospective_insertion
                             assert declaration.context.node_id == own.owner.node_id

                             assert declaration.clone_origin.placement.node_id ==
                                      own.owner.node_id

                             original = declaration.clone_origin.original
                             assert declaration.original_context == original
                             assert declaration.source.owner.node_id == original.node_id
                             refute declaration.context.node_id == original.node_id

                             assert binary_part(
                                      source,
                                      declaration.source.offset,
                                      declaration.source.length
                                    ) == declaration.source.text
                           end

                           assert :original_source_policy in evidence.pending_checks

                           assert {:error, {:unsupported_html, diagnostic}} =
                                    TemplatePatchValidator.authorize_mutation(plan, tree)

                           assert diagnostic.message =~ "original_source_policy"
                           :checked
                         end
                       )
                     end
                   )
                 end)
               end)
             end)
  end

  test "cloned detached shadow origins without prospective placements stay unbound synthetic sources" do
    authored =
      "<select style='appearance:base-select'><button><selectedcontent></selectedcontent></button><option selected><template shadowrootmode=open shadowrootclonable><style>b{color:red}</style><b>Shadow</b></template></option></select>"

    {source, tree, parent, origin} =
      fixture("<div style='font-size:20px'><?marker name='A'></div>", authored)

    opts = [system_font_discovery: false, forms: :static]

    assert :checked =
             ResourceContext.run([], HtmlValidator.new_image_budget(), fn _ ->
               with_plan(source, tree, parent, origin, fn plan ->
                 TemplatePatchValidator.with_candidate_projection(plan, tree, fn handle ->
                   TemplatePatchValidator.with_prospective_renderer_policy_context(
                     handle,
                     tree,
                     opts,
                     fn policy ->
                       TemplatePatchValidator.with_renderer_policy_evidence(
                         policy,
                         tree,
                         fn evidence ->
                           assert evidence.preparation_scope == :prospective_insertion

                           unbound =
                             Enum.find(Map.get(evidence, :clone_origins, %{}), fn {_id, binding} ->
                               binding.original == :unbound_synthetic_source
                             end)

                           assert unbound != nil
                           {clone_id, binding} = unbound
                           assert binding.preparation_origin.type == :document_fragment
                           assert binding.policy == :actual_normal_clone_origin_chain

                           assert Enum.any?(Map.get(evidence, :clone_origins, %{}), fn {id, other} ->
                                    id != clone_id and is_map(other.original)
                                  end)

                           # The cloned detached style keeps its authored owner while
                           # placed inside the prospective composition.
                           cloned_style =
                             Enum.find(Map.get(evidence, :clone_origins, %{}), fn {_id, other} ->
                               is_map(other.original) and other.preparation_origin.tag == "style"
                             end)

                           assert cloned_style != nil
                           :checked
                         end
                       )
                     end
                   )
                 end)
               end)
             end)
  end

  test "ordinary prospective preparation observes cascade glyphs and authorized bytes under distinct placement" do
    png = PngFixture.build(1, 1, 2, 8, 0, fn _, _ -> [255, 0, 0] end)

    {source, tree, parent, origin} =
      fixture(
        "<div style='font-size:20px'><?marker name='A'></div>",
        "<style>p{font-size:24px;color:red}</style><p style='width:2px'>A漢</p><img src='pixel.png'>"
      )

    opts = [system_font_discovery: false, assets: %{"pixel.png" => {:bytes, png}}]

    assert :checked =
             ResourceContext.run(
               [authorization_root: System.tmp_dir!()],
               HtmlValidator.new_image_budget(),
               fn resource ->
                 with_plan(source, tree, parent, origin, fn plan ->
                   TemplatePatchValidator.with_candidate_projection(plan, tree, fn handle ->
                     TemplatePatchValidator.with_prospective_renderer_policy_context(
                       handle,
                       tree,
                       opts,
                       fn policy ->
                         TemplatePatchValidator.with_renderer_policy_evidence(
                           policy,
                           tree,
                           fn evidence ->
                             assert evidence.preparation_scope == :prospective_insertion

                             assert evidence.prospective_policy ==
                                      :ordinary_prospective_style_and_generic_fallback_observed

                             assert evidence.original_parent ==
                                      :requires_separate_original_preparation

                             assert evidence.parent_declaration_contexts.original ==
                                      :requires_separate_original_preparation

                             paragraph =
                               Enum.find_value(evidence.nodes, fn {_id, entry} ->
                                 if entry.node[:tag] == "p", do: entry
                               end)

                             assert paragraph.font.font_size == 18.0
                             placement = evidence.source_placements[paragraph.node.node_id]

                             refute placement.original.parent_id ==
                                      placement.prospective.parent_id

                             assert paragraph.node.parent_id == placement.prospective.parent_id

                             declarations =
                               evidence.declaration_contexts[{paragraph.node.node_id, nil}]

                             assert Enum.any?(
                                      declarations,
                                      &(&1.declaration == {"font-size", "24px"} and
                                          &1.source.text == "font-size:24px")
                                    )

                             assert Enum.any?(declarations, &(&1.declaration == {"color", "red"}))

                             refute Enum.any?(
                                      declarations,
                                      &(&1.declaration == {"color", "yellow"})
                                    )

                             runs = evidence.glyph_runs |> Map.values() |> List.flatten()

                             paragraph_runs =
                               Enum.filter(runs, &(&1.owner.parent_id == paragraph.node.node_id))

                             assert Enum.any?(
                                      paragraph_runs,
                                      &(&1.authored_text == "A漢" and &1.resolved_text != "A漢")
                                    )

                             assert Enum.all?(
                                      paragraph_runs,
                                      &(&1.font_size == 18.0 and
                                          &1.original_owner.node_id == &1.owner.node_id)
                                    )

                             [reference] =
                               Enum.filter(evidence.references, &(&1.reference == "pixel.png"))

                             assert reference.encoded_source == png and
                                      reference.source_bytes == byte_size(png)

                             assert reference.policy == :loaded_in_prospective_preparation
                             assert reference.authority_basis == :ordinary_renderer_options

                             assert reference.owner.tag == "img" and
                                      reference.original_owner.tag == "img"

                             refute reference.owner.parent_id ==
                                      reference.original_owner.parent_id

                             assert reference.resource_context == resource
                             assert length(evidence.pending_checks) == 7

                             assert {:error, {:unsupported_html, denied}} =
                                      TemplatePatchValidator.authorize_mutation(plan, tree)

                             assert denied.message =~ "original_source_policy"
                             :checked
                           end
                         )
                       end
                     )
                   end)
                 end)
               end
             )
  end

  test "prospective observations leave implied source ownership unresolved" do
    png = PngFixture.build(1, 1, 2, 8, 0, fn _, _ -> [255, 0, 0] end)

    {source, tree, parent, origin} =
      fixture(
        "<style>tbody{background-image:url('pixel.png')}</style><table><?marker name='A'></table>",
        "<tr><td>New</td></tr>"
      )

    opts = [system_font_discovery: false, assets: %{"pixel.png" => {:bytes, png}}]

    assert :checked =
             ResourceContext.run(
               [authorization_root: System.tmp_dir!()],
               HtmlValidator.new_image_budget(),
               fn _ ->
                 with_plan(source, tree, parent, origin, fn plan ->
                   TemplatePatchValidator.with_candidate_projection(plan, tree, fn handle ->
                     TemplatePatchValidator.with_prospective_renderer_policy_context(
                       handle,
                       tree,
                       opts,
                       fn policy ->
                         TemplatePatchValidator.with_renderer_policy_evidence(
                           policy,
                           tree,
                           fn evidence ->
                             group =
                               Enum.find_value(evidence.nodes, fn {_id, entry} ->
                                 if entry.node[:tag] == "tbody", do: entry
                               end)

                             assert group.node.source.text == ""
                             refute Map.has_key?(evidence.source_placements, group.node.node_id)

                             assert Enum.all?(
                                      evidence.declaration_contexts[{group.node.node_id, nil}],
                                      &(&1.original_context == :unbound_synthetic_source)
                                    )

                             [resource] =
                               Enum.filter(evidence.references, &(&1.reference == "pixel.png"))

                             assert resource.owner.tag == "tbody"
                             assert resource.original_owner == :unbound_synthetic_source
                             assert resource.encoded_source == png
                             assert length(evidence.pending_checks) == 7
                             :checked
                           end
                         )
                       end
                     )
                   end)
                 end)
               end
             )
  end

  test "changed policy during ordinary preparation is diagnosed before any receipt is issued" do
    png = PngFixture.build(1, 1, 2, 8, 0, fn _, _ -> [255, 0, 0] end)

    {source, tree, parent, origin} =
      fixture("<div><?marker name='A'></div>", "<img src='pixel.png'>")

    limits = Limits.effective()

    resolver = fn %{reference: "pixel.png"} ->
      Limits.install(
        Map.put(limits, :max_css_numeric_magnitude, limits.max_css_numeric_magnitude + 1)
      )

      {:ok, png}
    end

    opts = [system_font_discovery: false, asset_resolver: resolver]

    assert :checked =
             ResourceContext.run(
               [authorization_root: System.tmp_dir!()],
               HtmlValidator.new_image_budget(),
               fn _ ->
                 with_plan(source, tree, parent, origin, fn plan ->
                   try do
                     assert {:error, {:invalid_document, diagnostic}} =
                              TemplatePatchValidator.with_candidate_projection(
                                plan,
                                tree,
                                fn handle ->
                                  TemplatePatchValidator.with_prospective_renderer_policy_context(
                                    handle,
                                    tree,
                                    opts,
                                    fn _ ->
                                      flunk("changed preparation policy issued a receipt")
                                    end
                                  )
                                end
                              )

                     assert diagnostic.stage == :html and diagnostic.operation == :parse
                     assert diagnostic.message =~ "changed during execution"

                     assert diagnostic.source == origin.source.text and
                              diagnostic.line == origin.source.line

                     assert diagnostic.column == origin.source.column

                     refute Process.get(
                              {NativeElixirPdfUtilities.Validators.TemplateRendererContextValidator,
                               :preparation}
                            )
                   after
                     Limits.install(limits)
                   end

                   assert {:error, {:unsupported_html, _}} =
                            TemplatePatchValidator.authorize_mutation(plan, tree)

                   :checked
                 end)
               end
             )
  end

  test "prospective receipts expire and restore active frames after callbacks fail" do
    {source, tree, parent, origin} = fixture("<div><?marker name='A'></div>", "<p>New</p>")
    opts = [system_font_discovery: false]
    reject = fn _ -> flunk("invalid receipt consumed") end

    assert {:error, {:invalid_document, forged}} =
             TemplatePatchValidator.with_prospective_renderer_policy_context(
               %{prepared: true},
               tree,
               opts,
               reject
             )

    assert forged.operation == :parse and forged.message =~ "forged"

    escaped =
      ResourceContext.run(
        [authorization_root: System.tmp_dir!()],
        HtmlValidator.new_image_budget(),
        fn _ ->
          with_plan(source, tree, parent, origin, fn plan ->
            assert {:error, {:invalid_document, wrong_kind}} =
                     TemplatePatchValidator.with_prospective_renderer_policy_context(
                       plan,
                       tree,
                       opts,
                       reject
                     )

            assert wrong_kind.message =~ "projection handle"

            TemplatePatchValidator.with_candidate_projection(plan, tree, fn handle ->
              for fail <- [:diagnostic, :exception] do
                # Each actual preparation owns a fresh projection, never reuses consumed capture.
                TemplatePatchValidator.with_candidate_projection(plan, tree, fn nested ->
                  runner = fn _policy ->
                    case fail do
                      :diagnostic ->
                        Diagnostics.error(:style, :invalid_document, "review callback stopped")

                      :exception ->
                        raise "review callback failed"
                    end
                  end

                  if fail == :diagnostic do
                    assert {:error, {:invalid_document, diagnostic}} =
                             TemplatePatchValidator.with_prospective_renderer_policy_context(
                               nested,
                               tree,
                               opts,
                               runner
                             )

                    assert diagnostic.stage == :style and
                             diagnostic.message == "review callback stopped"
                  else
                    assert_raise RuntimeError, "review callback failed", fn ->
                      TemplatePatchValidator.with_prospective_renderer_policy_context(
                        nested,
                        tree,
                        opts,
                        runner
                      )
                    end
                  end

                  refute Process.get({TemplatePatchValidator, :renderer_policy})
                  assert Process.get({TemplatePatchValidator, :candidate_projection}) == nested
                end)

                assert Process.get({TemplatePatchValidator, :candidate_projection}) == handle
              end

              TemplatePatchValidator.with_prospective_renderer_policy_context(
                handle,
                tree,
                opts,
                fn policy ->
                  assert :checked =
                           TemplatePatchValidator.with_renderer_policy_evidence(
                             policy,
                             tree,
                             fn _ -> :checked end
                           )

                  assert {:error, {:invalid_document, reused}} =
                           TemplatePatchValidator.with_renderer_policy_evidence(
                             policy,
                             tree,
                             reject
                           )

                  assert reused.message =~ "already been consumed"
                  policy
                end
              )
            end)
          end)
        end
      )

    assert {:error, {:invalid_document, expired}} =
             TemplatePatchValidator.with_renderer_policy_evidence(escaped, tree, reject)

    assert expired.message =~ "expired"
    refute Process.get({TemplatePatchValidator, :candidate_projection})
    refute Process.get({TemplatePatchValidator, :renderer_policy})
  end

  test "valid parent resource context cannot stand in for the current issued child before IO" do
    {source, tree, parent, origin} =
      fixture("<div><?marker name='A'></div>", "<img src='pixel.png'>")

    opts = [
      system_font_discovery: false,
      asset_resolver: fn _ -> flunk("stale parent scope reached resource work") end
    ]

    assert :checked =
             ResourceContext.run(
               [authorization_root: System.tmp_dir!()],
               HtmlValidator.new_image_budget(),
               fn resource ->
                 {:ok, _, child} =
                   ResourceValidator.authorize_document({:srcdoc, source}, resource)

                 with_plan(source, tree, parent, origin, fn plan ->
                   TemplatePatchValidator.with_candidate_projection(plan, tree, fn handle ->
                     TemplatePatchValidator.with_candidate_evidence(handle, tree, fn _ ->
                       context =
                         TemplatePatchValidator.with_candidate_input(plan, tree, & &1.plan)

                       ResourceContext.with_document(child, fn ->
                         assert ResourceValidator.validate_context(resource) == :ok

                         assert {:error, {:invalid_options, diagnostic}} =
                                  TemplateRendererContextValidator.prepare_candidate(
                                    handle,
                                    tree,
                                    context,
                                    opts,
                                    resource
                                  )

                         assert diagnostic.stage == :options
                         assert diagnostic.operation == :template_renderer_context
                         assert diagnostic.module == TemplateRendererContextValidator
                         assert diagnostic.source == "resource_context"

                         assert diagnostic.message ==
                                  "renderer preparation requires the current issued resource context"

                         :checked
                       end)
                     end)
                   end)
                 end)
               end
             )
  end

  test "public parser cannot reach read-only projection and evidence capture performs no asset or font IO" do
    {source, tree, parent, origin} = fixture("<div><?marker name='A'></div>", "<p>New</p>")

    traced = [
      {HtmlParser, :capture_template_candidate, 2},
      {AssetLoader, :resolve, 4},
      {Font, :load_registry, 1},
      {FileReader, :read, 2}
    ]

    Enum.each(traced, fn {module, _, _} -> Code.ensure_loaded!(module) end)
    Enum.each(traced, fn mfa -> assert :erlang.trace_pattern(mfa, true, [:local]) == 1 end)
    caller = self()

    task =
      Task.async(fn ->
        receive do
          :run -> :ok
        end

        assert {:error, {:unsupported_html, _}} = HtmlParser.parse_document_detailed(source)
        send(caller, :public_done)

        receive do
          :capture -> :ok
        end

        with_plan(source, tree, parent, origin, fn plan ->
          TemplatePatchValidator.with_candidate_projection(plan, tree, fn handle ->
            TemplatePatchValidator.with_candidate_evidence(handle, tree, fn _ -> :checked end)
          end)
        end)
      end)

    try do
      :erlang.trace(task.pid, true, [:call])
      send(task.pid, :run)
      assert_receive :public_done
      refute_receive {:trace, _, :call, {HtmlParser, :capture_template_candidate, _}}
      send(task.pid, :capture)
      assert :checked = Task.await(task, 10_000)
      assert_receive {:trace, _, :call, {HtmlParser, :capture_template_candidate, _}}
      refute_receive {:trace, _, :call, {AssetLoader, :resolve, _}}
      refute_receive {:trace, _, :call, {Font, :load_registry, _}}
      refute_receive {:trace, _, :call, {FileReader, :read, _}}
    after
      Enum.each(traced, &:erlang.trace_pattern(&1, false, [:local]))
    end
  end

  defp fixture(target, body) do
    prefix = "<head><style>p{color:blue}</style></head><body>#{target}"
    token = "<template for='A'>"
    source = prefix <> token <> body <> "</template><style>p{color:yellow}</style></body>"
    assert {:ok, dom} = HtmlParser.parse_document_detailed(prefix <> "</body>")
    [html] = dom.children
    [_, body_node] = html.children
    tree = HtmlTree.from_document(dom)
    root = tree.nodes[tree.root]

    tree = %{
      tree
      | nodes:
          Map.put(tree.nodes, tree.root, %{root | source: %{text: source, line: 1, column: 1}})
    }

    origin = %{
      source: %{text: token, line: 1, column: String.length(prefix) + 1},
      offset: byte_size(prefix),
      tokenizer_context: "body",
      ancestors: []
    }

    {source, tree, body_node.node_id, origin}
  end

  defp with_plan(source, tree, parent, origin, consumer) do
    TemplatePatchValidator.with_parser_invocation(source, fn _ ->
      TemplatePatchValidator.with_insertion_context(tree, parent, "A", origin, fn plan ->
        TemplatePatchValidator.with_planned_insertion(plan, tree, fn _ -> consumer.(plan) end)
      end)
    end)
  end
end
