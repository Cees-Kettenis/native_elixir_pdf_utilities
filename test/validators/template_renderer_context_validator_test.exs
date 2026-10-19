alias NativeElixirPdfUtilities.TestSupport.Paths

defmodule NativeElixirPdfUtilities.Validators.TemplateRendererContextValidatorTest do
  use ExUnit.Case, async: false

  Code.require_file(Paths.support("png_fixture.ex"))

  alias NativeElixirPdfUtilities.{Diagnostics, Limits}

  alias NativeElixirPdfUtilities.HtmlToPdf.{
    AssetLoader,
    Font,
    HtmlParser,
    HtmlTree,
    ResourceContext,
    Style
  }

  alias NativeElixirPdfUtilities.TestSupport.PngFixture

  alias NativeElixirPdfUtilities.Validators.{
    FontValidator,
    HtmlValidator,
    ResourceValidator,
    TemplatePatchValidator,
    TemplateRendererContextValidator
  }

  setup do
    root =
      Path.join(
        System.tmp_dir!(),
        "native-pdf-template-policy-#{System.unique_integer([:positive])}"
      )

    File.mkdir_p!(root)
    limits = Limits.effective()

    on_exit(fn ->
      Limits.install(limits)
      File.rm_rf!(root)
    end)

    png = PngFixture.build(1, 1, 2, 8, 0, fn _, _ -> [255, 0, 0] end)
    %{root: root, opts: [system_font_discovery: false, assets: %{"pixel.png" => {:bytes, png}}]}
  end

  test "issued context contains primary faces, decoded assets and effective page geometry",
       %{root: root, opts: opts} do
    {source, tree, parent, origin} = fixture()

    font_data =
      File.read!(
        Path.join(:code.priv_dir(:native_elixir_pdf_utilities), "fonts/dejavu/DejaVuSans.ttf")
      )

    opts =
      Keyword.merge(opts,
        fonts: [%{family: "ReceiptFont", data: font_data}],
        default_font: "ReceiptFont"
      )

    caller = self()

    traced = [
      {Font, :load_registry, 1},
      {Font, :resolve, 4},
      {AssetLoader, :resolve_local, 4},
      {File, :read, 1},
      {File, :stat, 1},
      {File, :lstat, 1}
    ]

    Enum.each(traced, fn {module, _, _} -> Code.ensure_loaded!(module) end)
    Enum.each(traced, fn mfa -> assert :erlang.trace_pattern(mfa, true, [:local]) == 1 end)

    task =
      Task.async(fn ->
        ResourceContext.run(
          [authorization_root: root],
          HtmlValidator.new_image_budget(),
          fn resource ->
            with_plan(source, tree, parent, origin, fn plan, context ->
              TemplatePatchValidator.with_renderer_policy_context(plan, tree, opts, fn policy ->
                send(caller, {:prepared, self()})

                receive do
                  :consume -> :ok
                end

                assert :checked =
                         TemplatePatchValidator.with_renderer_policy_evidence(
                           policy,
                           tree,
                           fn evidence ->
                             assert evidence.source.text == source

                             assert evidence.original == context.original and
                                      evidence.insertion == context.insertion

                             assert evidence.original_parent.node.node_id == parent

                             assert evidence.insertion_parent.node.node_id ==
                                      context.target.parent

                             assert evidence.insertion_parent.node.tree_scope_id ==
                                      tree.nodes[context.target.parent].tree_scope_id

                             face = evidence.insertion_parent.font.font_face

                             assert face.family == "ReceiptFont" and face.data == font_data and
                                      face.source == :configured

                             assert face.units_per_em > 0 and length(face.widths) > 0
                             assert evidence.insertion_parent.font._font_registry.embedded != []

                             image =
                               Enum.find_value(evidence.nodes, fn {_id, entry} ->
                                 Map.get(entry.resources, :image)
                               end)

                             assert image.width_px == 1 and image.height_px == 1 and
                                      image.format == :png

                             assert evidence.page.page_size == {200.0, 300.0}

                             assert evidence.page.margins == %{
                                      top: 10.0,
                                      right: 10.0,
                                      bottom: 10.0,
                                      left: 10.0
                                    }

                             assert evidence.page.requested_media.width >
                                      evidence.page.page_size |> elem(0)

                             assert evidence.font_policy ==
                                      :prepared_primary_faces_and_loaded_metrics

                             assert evidence.resource_policy == :prepared_active_style_outcomes

                             assert evidence.pending_selection == [
                                      :glyph_fallback,
                                      :final_math_faces,
                                      :final_svg_faces
                                    ]

                             assert evidence.pending_checks == context.pending_checks and
                                      length(evidence.pending_checks) == 7

                             refute Map.has_key?(evidence, :certificate)
                             assert ResourceValidator.validate_context(resource) == :ok
                             :checked
                           end
                         )

                assert {:error, {:invalid_document, _}} =
                         TemplatePatchValidator.with_renderer_policy_evidence(
                           policy,
                           tree,
                           fn _ -> flunk("reused evidence") end
                         )

                assert {:error, {:unsupported_html, diagnostic}} =
                         TemplatePatchValidator.authorize_mutation(plan, tree)

                assert diagnostic.message =~ "selected_fonts_and_metrics"
                :ok
              end)
            end)
          end
        )
      end)

    try do
      assert_receive {:prepared, pid}, 10_000
      :erlang.trace(pid, true, [:call])
      send(pid, :consume)
      assert :ok = Task.await(task, 10_000)
      refute_receive {:trace, _, :call, _}
    after
      Enum.each(traced, &:erlang.trace_pattern(&1, false, [:local]))
    end
  end

  test "evidence expires and rejects forgery, changed tree, nested invocation and retained-budget reset",
       %{root: root, opts: opts} do
    {source, tree, parent, origin} = fixture()
    keys = MapSet.new(Process.get_keys())

    escaped =
      ResourceContext.run(
        [authorization_root: root],
        HtmlValidator.new_image_budget(),
        fn resource ->
          with_plan(source, tree, parent, origin, fn plan, _ ->
            TemplatePatchValidator.with_renderer_policy_context(plan, tree, opts, fn policy ->
              reject = fn _ -> flunk("invalid evidence consumed") end

              assert {:error, {:invalid_document, _}} =
                       TemplatePatchValidator.with_renderer_policy_evidence(
                         %{prepared: policy},
                         tree,
                         reject
                       )

              changed = put_in(tree.nodes[tree.root].source.text, source <> " ")

              assert {:error, {:invalid_document, _}} =
                       TemplatePatchValidator.with_renderer_policy_evidence(
                         policy,
                         changed,
                         reject
                       )

              assert {:error, {:invalid_document, _}} =
                       TemplatePatchValidator.with_parser_invocation(source, fn _ ->
                         TemplatePatchValidator.with_renderer_policy_evidence(
                           policy,
                           tree,
                           reject
                         )
                       end)

              assert {:error, {:invalid_document, _}} =
                       Task.async(fn ->
                         TemplatePatchValidator.with_renderer_policy_evidence(
                           policy,
                           tree,
                           reject
                         )
                       end)
                       |> Task.await()

              assert {:error, {:invalid_document, _}} =
                       TemplatePatchValidator.with_planned_insertion(policy, tree, reject)

              assert {:error, {:invalid_document, _}} =
                       TemplatePatchValidator.authorize_mutation(policy, tree)

              font_key = {FontValidator, :budget}
              previous = Process.get(font_key)
              Process.delete(font_key)

              assert {:error, {:invalid_document, _}} =
                       FontValidator.with_budget(fn ->
                         refute Process.get(font_key).budget_id == previous.budget_id

                         TemplatePatchValidator.with_renderer_policy_evidence(
                           policy,
                           tree,
                           reject
                         )
                       end)

              Process.put(font_key, previous)

              {:ok, _, child} = ResourceValidator.authorize_document({:srcdoc, ""}, resource)

              assert {:error, {:invalid_document, _}} =
                       ResourceContext.with_document(child, fn ->
                         TemplatePatchValidator.with_renderer_policy_evidence(
                           policy,
                           tree,
                           reject
                         )
                       end)

              scope_key = {ResourceContext, resource.scope}
              registration = Process.get(scope_key)
              Process.delete(scope_key)

              assert {:error, {:invalid_options, stale_diagnostic}} =
                       TemplatePatchValidator.with_renderer_policy_evidence(policy, tree, reject)

              assert stale_diagnostic.module == ResourceValidator
              assert stale_diagnostic.message == "resource context is outside its immutable scope"
              Process.put(scope_key, registration)

              image_counters = for i <- 1..3, do: :atomics.get(resource.image_budget, i)
              assert Enum.any?(image_counters, &(&1 > 0))
              for i <- 1..3, do: :atomics.put(resource.image_budget, i, 0)

              assert {:error, {:invalid_document, _}} =
                       TemplatePatchValidator.with_renderer_policy_evidence(policy, tree, reject)

              for {used, i} <- Enum.with_index(image_counters, 1),
                  do: :atomics.put(resource.image_budget, i, used)

              assert :nested_checked =
                       TemplatePatchValidator.with_renderer_policy_context(
                         plan,
                         tree,
                         Keyword.put(opts, :margin, 20),
                         fn nested_policy ->
                           assert {:error, {:invalid_document, _}} =
                                    TemplatePatchValidator.with_renderer_policy_evidence(
                                      policy,
                                      tree,
                                      reject
                                    )

                           TemplatePatchValidator.with_renderer_policy_evidence(
                             nested_policy,
                             tree,
                             fn nested_evidence ->
                               assert nested_evidence.page.margins.top == 20.0
                               :nested_checked
                             end
                           )
                         end
                       )

              assert :checked =
                       TemplatePatchValidator.with_renderer_policy_evidence(policy, tree, fn _ ->
                         :checked
                       end)

              policy
            end)
          end)
        end
      )

    assert {:error, {:invalid_document, _}} =
             TemplatePatchValidator.with_renderer_policy_evidence(escaped, tree, fn _ ->
               flunk("expired context")
             end)

    assert MapSet.new(Process.get_keys()) == keys
  end

  test "issuer requires consumed plan and independently issued resource context before preparation",
       %{opts: opts} do
    {source, tree, parent, origin} = fixture()

    assert {:error, {:invalid_options, diagnostic}} =
             with_plan(source, tree, parent, origin, fn plan, _ ->
               TemplatePatchValidator.with_renderer_policy_context(plan, tree, opts, fn _ ->
                 flunk("unissued resources")
               end)
             end)

    assert diagnostic.module == ResourceValidator

    assert {:error, {:invalid_document, _}} =
             TemplatePatchValidator.with_parser_invocation(source, fn _ ->
               TemplatePatchValidator.with_insertion_context(tree, parent, "A", origin, fn plan ->
                 TemplatePatchValidator.with_renderer_policy_context(plan, tree, opts, fn _ ->
                   flunk("unconsumed plan")
                 end)
               end)
             end)

    assert TemplateRendererContextValidator.observe(%{}, %{}) == :ok
    assert Process.get({TemplateRendererContextValidator, :preparation}) == nil
  end

  test "normal preparation failures never mint policy context and callbacks always clean up", %{
    root: root,
    opts: opts
  } do
    {source, tree, parent, origin} = fixture()
    error = Diagnostics.error(:css, :invalid_css, "consumer stopped", source: "checked context")

    ResourceContext.run([authorization_root: root], HtmlValidator.new_image_budget(), fn _ ->
      with_plan(source, tree, parent, origin, fn plan, _ ->
        assert {:error, {:invalid_document, _}} =
                 TemplatePatchValidator.with_renderer_policy_context(
                   plan,
                   tree,
                   %{prepared: :fake},
                   fn _ -> flunk("raw prepared map") end
                 )

        assert {:error, {:invalid_page_size, page_diagnostic}} =
                 TemplatePatchValidator.with_renderer_policy_context(
                   plan,
                   tree,
                   Keyword.put(opts, :page_size, {-1, 1}),
                   fn _ -> flunk("bad page") end
                 )

        assert page_diagnostic.stage == :layout
        assert page_diagnostic.message == "layout failed: invalid_page_size"

        assert {:error, {:invalid_document, _}} =
                 TemplatePatchValidator.with_renderer_policy_context(
                   plan,
                   tree,
                   Keyword.put(opts, :assets, %{"pixel.png" => {:bytes, "invalid PNG"}}),
                   fn _ -> flunk("bad decoded resource") end
                 )

        assert {:error, {:invalid_document, missing_diagnostic}} =
                 TemplatePatchValidator.with_renderer_policy_context(
                   plan,
                   tree,
                   Keyword.put(opts, :stylesheets, [{:file, Path.join(root, "missing.css")}]),
                   fn _ -> flunk("missing stylesheet evidence") end
                 )

        assert missing_diagnostic.stage == :style
        assert missing_diagnostic.reason == :invalid_document
        assert is_binary(missing_diagnostic.message)

        assert Process.get({TemplateRendererContextValidator, :preparation}) == nil

        assert ^error =
                 TemplatePatchValidator.with_renderer_policy_context(
                   plan,
                   tree,
                   opts,
                   fn policy ->
                     TemplatePatchValidator.with_renderer_policy_evidence(policy, tree, fn _ ->
                       error
                     end)
                   end
                 )

        assert_raise RuntimeError, "context consumer failed", fn ->
          TemplatePatchValidator.with_renderer_policy_context(plan, tree, opts, fn _ ->
            raise "context consumer failed"
          end)
        end

        assert Process.get({TemplateRendererContextValidator, :preparation}) == nil
        :ok
      end)
    end)
  end

  test "root supplied bytes and issued child local authorization keep their distinct authority",
       %{root: root, opts: opts} do
    reference = "https://example.test/pixel.png"
    {source, tree, parent, origin} = fixture(reference)
    {:bytes, png} = opts[:assets]["pixel.png"]
    caller = self()

    opts =
      Keyword.merge(opts,
        assets: %{reference => {:bytes, png}},
        asset_resolver: fn _ ->
          send(caller, :resolver_called)
          {:ok, png}
        end
      )

    assert :checked =
             ResourceContext.run(
               [authorization_root: root],
               HtmlValidator.new_image_budget(),
               fn resource ->
                 with_plan(source, tree, parent, origin, fn plan, _ ->
                   assert :checked =
                            TemplatePatchValidator.with_renderer_policy_context(
                              plan,
                              tree,
                              opts,
                              fn policy ->
                                TemplatePatchValidator.with_renderer_policy_evidence(
                                  policy,
                                  tree,
                                  fn evidence ->
                                    assert evidence.resource_authority.basis ==
                                             :ordinary_root_renderer_options

                                    assert evidence.resource_authority.policy ==
                                             :requires_complete_per_reference_evidence

                                    assert evidence.resource_authority.scope == resource.scope

                                    assert evidence.resource_authority.document ==
                                             resource.document_id

                                    assert :active_resource_authorization_and_contents in evidence.pending_checks

                                    :checked
                                  end
                                )
                              end
                            )

                   {:ok, _, child} =
                     ResourceValidator.authorize_document({:srcdoc, source}, resource)

                   assert {:error, {:resource_denied, diagnostic}} =
                            ResourceContext.with_document(child, fn ->
                              TemplatePatchValidator.with_renderer_policy_context(
                                plan,
                                tree,
                                opts,
                                fn _ -> flunk("unauthorized child prepared evidence") end
                              )
                            end)

                   assert diagnostic.stage == :resource and diagnostic.module == ResourceValidator
                   assert diagnostic.source == reference
                   :checked
                 end)
               end
             )

    refute_receive :resolver_called
    assert Process.get({TemplateRendererContextValidator, :preparation}) == nil
  end

  test "visited invalid authored styles fail normal preparation while inert resources remain explicitly unresolved",
       %{root: root, opts: opts} do
    {source, tree, parent, origin} =
      fixture("pixel.png", "<p style='width:bananas'>Invalid original</p>", "")

    assert {:error, {:invalid_css, diagnostic}} =
             ResourceContext.run(
               [authorization_root: root],
               HtmlValidator.new_image_budget(),
               fn _ ->
                 with_plan(source, tree, parent, origin, fn plan, _ ->
                   TemplatePatchValidator.with_renderer_policy_context(plan, tree, opts, fn _ ->
                     flunk("invalid original style issued")
                   end)
                 end)
               end
             )

    assert diagnostic.stage == :css
    assert diagnostic.source == "width:bananas"

    {source, tree, parent, origin} =
      fixture(
        "pixel.png",
        "<template><img src='https://example.test/inert.png'></template><p>Active</p>"
      )

    assert :checked =
             ResourceContext.run(
               [authorization_root: root],
               HtmlValidator.new_image_budget(),
               fn _ ->
                 with_plan(source, tree, parent, origin, fn plan, _ ->
                   TemplatePatchValidator.with_renderer_policy_context(
                     plan,
                     tree,
                     opts,
                     fn policy ->
                       TemplatePatchValidator.with_renderer_policy_evidence(
                         policy,
                         tree,
                         fn evidence ->
                           inert =
                             Enum.find_value(tree.nodes, fn {id, node} ->
                               if node[:attributes]["src"] == "https://example.test/inert.png",
                                 do: id
                             end)

                           assert is_reference(inert)
                           refute Map.has_key?(evidence.nodes, inert)
                           assert :unvisited_authored_resources in evidence.pending_resources
                           assert :original_source_policy in evidence.pending_checks

                           assert {:error, {:unsupported_html, _}} =
                                    TemplatePatchValidator.authorize_mutation(plan, tree)

                           :checked
                         end
                       )
                     end
                   )
                 end)
               end
             )
  end

  test "source-only style traversal does not invoke the ordinary preparation observer" do
    observer = {TemplateRendererContextValidator, :observe, 2}
    Code.ensure_loaded!(TemplateRendererContextValidator)
    assert :erlang.trace_pattern(observer, true, [:local]) == 1

    task =
      Task.async(fn ->
        receive do
          :source_only -> :ok
        end

        assert {:ok, dom} =
                 HtmlParser.parse_document_detailed(
                   "<p style='font-family:Missing;width:3pt'>Original</p>"
                 )

        assert {:ok, result} =
                 Style.with_source_style_contexts(dom, [], HtmlValidator.new_css_budget(), fn _,
                                                                                              _,
                                                                                              _ ->
                   :ok
                 end)

        assert :font_selection in result.runtime_obligations
        :ok
      end)

    try do
      :erlang.trace(task.pid, true, [:call])
      send(task.pid, :source_only)
      assert :ok = Task.await(task)
      refute_receive {:trace, _, :call, {TemplateRendererContextValidator, :observe, _}}
    after
      :erlang.trace_pattern(observer, false, [:local])
    end
  end

  test "font budget identities survive nested reuse and change on a fresh scope" do
    key = {FontValidator, :budget}
    assert Process.get(key) == nil

    first =
      FontValidator.with_budget(fn ->
        id = Process.get(key).budget_id
        assert is_reference(id)
        assert ^id = FontValidator.with_budget(fn -> Process.get(key).budget_id end)
        id
      end)

    assert Process.get(key) == nil
    second = FontValidator.with_budget(fn -> Process.get(key).budget_id end)
    refute first == second

    assert_raise RuntimeError, "font scope failed", fn ->
      FontValidator.with_budget(fn -> raise "font scope failed" end)
    end

    assert Process.get(key) == nil
  end

  test "live stylesheet preparation binds authored owners and leaves synthetic styles unbound",
       %{opts: _opts} do
    key = {TemplateRendererContextValidator, :preparation}

    assert {:ok, dom} =
             HtmlParser.parse_document_detailed(
               "<head><style>p{color:blue}</style></head><body><p>Body</p></body>"
             )

    tree = HtmlTree.from_document(dom)
    style = Enum.find(Map.values(tree.nodes), &(&1[:tag] == "style"))

    identity = [
      :node_id,
      :document_id,
      :tree_scope_id,
      :parent_id,
      :source,
      :attributes,
      :tag,
      :namespace
    ]

    # A composition clone of the authored style element keeps its payload and
    # gains a fresh placement identity.
    clone = %{style | node_id: make_ref()}
    authored = %{Map.take(style, identity) | node_id: make_ref()}
    entry = %{css: "p{color:blue}", source_kind: :embedded}

    ResourceContext.run([], HtmlValidator.new_image_budget(), fn current ->
      FontValidator.with_budget(fn ->
        preparation = %{
          resource_frame: nil,
          resource_context: current,
          tree: tree,
          clone_nodes: %{},
          clone_bindings: %{},
          source_projection: nil,
          preparation_limits: Limits.effective(),
          font_budget_id: Process.get({FontValidator, :budget}).budget_id
        }

        Process.put(key, preparation)

        try do
          # An authored stylesheet keeps its own original owner.
          owned = TemplateRendererContextValidator.stylesheet_owner(style, entry)
          assert owned.__template_stylesheet_owner__ == Map.take(style, identity)

          # A composition clone's stylesheet binds its authored origin.
          Process.put(key, %{
            preparation
            | clone_nodes: %{clone.node_id => clone},
              clone_bindings: %{clone.node_id => %{original: authored}}
          })

          assert TemplateRendererContextValidator.stylesheet_owner(clone, entry).__template_stylesheet_owner__ ==
                   Map.take(authored, identity)

          # A prospective synthetic style without a prefix placement stays unbound.
          Process.put(key, %{
            preparation
            | clone_nodes: %{clone.node_id => clone},
              clone_bindings: %{},
              source_projection: %{placements: %{}}
          })

          assert TemplateRendererContextValidator.stylesheet_owner(clone, entry) == entry
        after
          Process.delete(key)
        end
      end)
    end)
  end

  test "an out-of-range CSS font candidate ordinal cannot mint candidate ownership", %{
    opts: _opts
  } do
    key = {TemplateRendererContextValidator, :preparation}

    ResourceContext.run([], HtmlValidator.new_image_budget(), fn current ->
      FontValidator.with_budget(fn ->
        ticket = make_ref()

        provider = %{
          authored: %{candidates: [%{role: :first_candidate}]},
          owner: nil,
          resolution_base: nil,
          shadow_scope_id: nil,
          shadow_host_id: nil,
          resource_context: current
        }

        preparation = %{
          resource_frame: nil,
          resource_context: current,
          font_providers: %{ticket => provider},
          font_candidate_frame: nil,
          references: [],
          preparation_limits: Limits.effective(),
          font_budget_id: Process.get({FontValidator, :budget}).budget_id
        }

        Process.put(key, preparation)

        try do
          config = %{family: "Chosen", data: ["bytes"]}

          assert {:ok, {^config, issued}} =
                   TemplateRendererContextValidator.with_font_candidate(ticket, 0, fn ->
                     {:ok, config}
                   end)

          assert issued.provider == provider and issued.candidate.role == :first_candidate

          assert {:error, {:invalid_document, diagnostic}} =
                   TemplateRendererContextValidator.with_font_candidate(ticket, 99, fn ->
                     {:ok, config}
                   end)

          assert diagnostic.stage == :font and diagnostic.reason == :invalid_document
          assert diagnostic.operation == :template_renderer_context
          assert diagnostic.module == TemplateRendererContextValidator
          assert diagnostic.message =~ "live normal preparation source owner and policy context"
        after
          Process.delete(key)
        end
      end)
    end)
  end

  test "a font load observation with a forged configuration stays unbound", %{opts: _opts} do
    key = {TemplateRendererContextValidator, :preparation}

    ResourceContext.run([], HtmlValidator.new_image_budget(), fn current ->
      FontValidator.with_budget(fn ->
        ticket = make_ref()
        provider = %{shadow_scope_id: nil, shadow_host_id: nil, resource_context: current}
        candidate = %{provider: provider, candidate: %{role: :first_candidate}, references: []}
        config = %{family: "Real", path: ["/fonts/real.ttf"]}

        frame = %{
          ticket: ticket,
          caller_count: 0,
          recipes: [%{provider: ticket, candidates: [candidate], config: config}],
          context: current
        }

        preparation = %{
          resource_frame: nil,
          resource_context: current,
          font_registry_frame: frame,
          font_outcomes: %{},
          preparation_limits: Limits.effective(),
          font_budget_id: Process.get({FontValidator, :budget}).budget_id
        }

        Process.put(key, preparation)

        try do
          loaded = %{family: "Real", weight: 400, style: :normal, data: "bytes"}

          observed =
            TemplateRendererContextValidator.observe_font_load(
              ticket,
              :configured,
              0,
              0,
              config,
              loaded
            )

          assert observed.family == "Real"
          assert is_reference(observed.__template_font_outcome__)
          assert Process.get(key).font_outcomes != %{}

          forged = %{family: "Forged", weight: 400, style: :normal, data: "bytes"}

          assert TemplateRendererContextValidator.observe_font_load(
                   ticket,
                   :configured,
                   0,
                   0,
                   %{family: "Forged"},
                   forged
                 ) == forged

          assert map_size(Process.get(key).font_outcomes) == 1
        after
          Process.delete(key)
        end
      end)
    end)
  end

  test "prepared glyph declaration chains stop at tree scope boundaries", %{opts: _opts} do
    document = make_ref()
    outer_scope = make_ref()
    inner_scope = make_ref()
    body_id = make_ref()
    div_id = make_ref()
    text_id = make_ref()
    source = %{line: 1, text: "Chained", column: 1}

    text_identity = %{
      type: :text,
      node_id: text_id,
      document_id: document,
      tree_scope_id: outer_scope,
      parent_id: div_id,
      source: source,
      attributes: nil,
      tag: nil,
      namespace: nil
    }

    tree = %HtmlTree{
      root: body_id,
      nodes: %{
        text_id => Map.merge(text_identity, %{text: "Chained"}),
        div_id => %{
          type: :element,
          node_id: div_id,
          document_id: document,
          tree_scope_id: outer_scope,
          parent_id: body_id,
          source: source,
          attributes: %{},
          tag: "div",
          namespace: :html,
          children: [text_id]
        },
        body_id => %{
          type: :element,
          node_id: body_id,
          document_id: document,
          tree_scope_id: inner_scope,
          parent_id: nil,
          source: source,
          attributes: %{},
          tag: "body",
          namespace: :html,
          children: [div_id]
        }
      }
    }

    runs = [
      Map.merge(text_identity, %{
        text: "Chained",
        style: %{font_face: %{family: "DejaVu Sans"}, font_size: 12.0}
      })
    ]

    assert {:ok, collected} =
             TemplateRendererContextValidator.validate_prepared_glyph_runs(runs, tree)

    assert [_run] = collected[text_id]
    [run] = collected[text_id]
    assert run.authored_text == "Chained"
    assert [%{owner: owner, declarations: []}] = run.declaration_contexts
    assert owner.node_id == div_id
    assert owner.tree_scope_id == outer_scope
  end

  defp fixture(
         reference \\ "pixel.png",
         authored \\ "<p>Original</p>",
         stylesheet \\ "@page{size:200pt 300pt;margin:10pt}"
       ) do
    prefix =
      "<head><style>#{stylesheet}</style></head><body><div><?marker name='A'>#{authored}<img src='#{reference}'></div>"

    token = "<template for='A'>"
    assert {:ok, dom} = HtmlParser.parse_document_detailed(prefix <> "</body>")
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

    {source, tree, body.node_id, origin}
  end

  defp with_plan(source, tree, parent, origin, consumer) do
    TemplatePatchValidator.with_parser_invocation(source, fn _ ->
      TemplatePatchValidator.with_insertion_context(tree, parent, "A", origin, fn plan ->
        TemplatePatchValidator.with_planned_insertion(plan, tree, fn context ->
          consumer.(plan, context)
        end)
      end)
    end)
  end
end
