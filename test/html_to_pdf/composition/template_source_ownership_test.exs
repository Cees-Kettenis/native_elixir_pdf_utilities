alias NativeElixirPdfUtilities.TestSupport.Paths

defmodule NativeElixirPdfUtilities.Validators.TemplateSourceOwnershipTest do
  use ExUnit.Case, async: false

  Code.require_file(Paths.support("png_fixture.ex"))

  alias NativeElixirPdfUtilities.{FileReader, Limits}

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
    HtmlValidator,
    ResourceValidator,
    TemplatePatchValidator,
    TemplateRendererContextValidator
  }

  setup do
    limits = Limits.effective()

    root =
      Path.join(
        System.tmp_dir!(),
        "native-pdf-template-evidence-#{System.unique_integer([:positive])}"
      )

    File.mkdir_p!(root)
    png = PngFixture.build(1, 1, 2, 8, 0, fn _, _ -> [255, 0, 0] end)

    on_exit(fn ->
      Limits.install(limits)
      File.rm_rf!(root)
    end)

    %{
      root: root,
      png: png,
      opts: [system_font_discovery: false, assets: %{"pixel.png" => {:bytes, png}}]
    }
  end

  test "normal fallback retains authored Unicode, original IDs and replacement outcomes",
       %{root: root, opts: opts} do
    {source, tree, parent, origin} = fixture("<p>A𝑥B漢</p>")
    opts = Keyword.put(opts, :default_font, "DejaVu Sans, STIX Two Math")

    assert :checked =
             in_scope(root, source, tree, parent, origin, fn plan ->
               TemplatePatchValidator.with_renderer_policy_context(plan, tree, opts, fn policy ->
                 TemplatePatchValidator.with_renderer_policy_evidence(policy, tree, fn evidence ->
                   {id, runs} =
                     Enum.find(evidence.glyph_runs, fn {_id, runs} ->
                       hd(runs).authored_text == "A𝑥B漢"
                     end)

                   assert is_reference(id)
                   assert Enum.map_join(runs, & &1.resolved_text) == "A𝑥B�"

                   assert Enum.map(runs, & &1.font_face.family) == [
                            "DejaVu Sans",
                            "STIX Two Math",
                            "DejaVu Sans"
                          ]

                   assert hd(runs).owner.source.text == "A𝑥B漢"

                   assert hd(runs).owner.node_id == id and
                            hd(runs).owner.tree_scope_id == tree.nodes[id].tree_scope_id

                   primary = evidence.nodes[tree.nodes[id].parent_id].font.font_face
                   refute Font.supports_text?(primary, "𝑥")
                   assert Font.supports_text?(Enum.at(runs, 1).font_face, "𝑥")

                   assert Enum.all?(
                            runs,
                            &(&1.authored_text == "A𝑥B漢" and
                                &1.policy == :normal_generic_text_fallback_observed)
                          )

                   assert evidence.prospective_policy == :requires_actual_insertion_preparation

                   assert :final_math_faces in evidence.pending_selection and
                            :final_svg_faces in evidence.pending_selection

                   assert {:error, {:unsupported_html, diagnostic}} =
                            TemplatePatchValidator.authorize_mutation(plan, tree)

                   assert diagnostic.message =~ "selected_fonts_and_metrics"
                   :checked
                 end)
               end)
             end)

    assert {:error, {:unsupported_glyph, diagnostic}} =
             in_scope(root, source, tree, parent, origin, fn plan ->
               TemplatePatchValidator.with_renderer_policy_context(
                 plan,
                 tree,
                 Keyword.put(opts, :unsupported_glyphs, :error),
                 fn _ -> flunk("unsupported glyph evidence") end
               )
             end)

    assert diagnostic.stage == :font and diagnostic.source == "漢"
    assert diagnostic.operation == :resolve_fonts
    assert diagnostic.message =~ "U+6F22"
  end

  test "transformed original text remains separate from synthetic generated text", %{
    root: root,
    opts: opts
  } do
    {source, tree, parent, origin} =
      fixture("<p style='text-transform:uppercase'>abc</p>", "p::before{content:'generated'}")

    assert :checked =
             in_scope(root, source, tree, parent, origin, fn plan ->
               TemplatePatchValidator.with_renderer_policy_context(plan, tree, opts, fn policy ->
                 TemplatePatchValidator.with_renderer_policy_evidence(policy, tree, fn evidence ->
                   runs = evidence.glyph_runs |> Map.values() |> List.flatten()

                   assert Enum.any?(
                            runs,
                            &(&1.authored_text == "abc" and &1.resolved_text == "ABC" and
                                &1.text_transform == :uppercase)
                          )

                   refute Enum.any?(runs, &(&1.authored_text == "generated"))
                   assert :glyph_fallback in evidence.pending_selection
                   assert :generated_content in evidence.pending_glyph_owners
                   :checked
                 end)
               end)
             end)
  end

  test "encoded source bytes and source owner retain root or issued-child authority", %{
    root: root,
    png: png,
    opts: opts
  } do
    {source, tree, parent, origin} = fixture("<p>Original</p><img src='pixel.png'>")

    for child? <- [false, true] do
      assert :checked =
               ResourceContext.run(
                 [authorization_root: root],
                 HtmlValidator.new_image_budget(),
                 fn context ->
                   work = fn ->
                     with_plan(source, tree, parent, origin, fn plan ->
                       TemplatePatchValidator.with_renderer_policy_context(
                         plan,
                         tree,
                         opts,
                         fn policy ->
                           TemplatePatchValidator.with_renderer_policy_evidence(
                             policy,
                             tree,
                             fn evidence ->
                               [record] = Enum.filter(evidence.references, &(&1.kind == :image))

                               assert record.reference == "pixel.png" and
                                        record.encoded_source == png and
                                        record.source_bytes == byte_size(png)

                               assert record.owner.tag == "img" and
                                        record.owner.source.text == "<img src='pixel.png'>"

                               assert record.owner.document_id ==
                                        tree.nodes[tree.root].document_id

                               assert record.policy == :loaded_in_original_preparation

                               assert record.prospective_policy ==
                                        :requires_actual_insertion_preparation

                               if child? do
                                 assert record.authority_basis == :issued_local_dependency
                                 assert record.descriptor == {:bytes, png}
                                 assert record.resource_context.depth == 1
                               else
                                 assert record.authority_basis == :ordinary_renderer_options

                                 assert record.descriptor == nil and
                                          record.resource_context.depth == 0
                               end

                               assert ResourceValidator.validate_context(record.resource_context) ==
                                        :ok

                               assert length(evidence.pending_checks) == 7
                               :checked
                             end
                           )
                         end
                       )
                     end)
                   end

                   if child? do
                     {:ok, _, child} =
                       ResourceValidator.authorize_document({:srcdoc, source}, context)

                     ResourceContext.with_document(child, work)
                   else
                     work.()
                   end
                 end
               )
    end
  end

  test "cached font asset results retain encoded evidence without another source read", %{
    root: root,
    opts: opts
  } do
    path = Path.join(root, "proof.ttf")

    data =
      File.read!(
        Path.join(:code.priv_dir(:native_elixir_pdf_utilities), "fonts/dejavu/DejaVuSans.ttf")
      )

    File.write!(path, data)

    css =
      "@font-face{font-family:One;src:url('proof.ttf')}@font-face{font-family:Two;src:url('proof.ttf')}"

    {source, tree, parent, origin} = fixture("<p>Original</p>", css)
    opts = Keyword.put(opts, :assets, %{"proof.ttf" => {:file, path}})
    Code.ensure_loaded!(FileReader)
    assert :erlang.trace_pattern({FileReader, :read, 2}, true, [:local]) == 1

    task =
      Task.async(fn ->
        receive do
          :prepare -> :ok
        end

        in_scope(root, source, tree, parent, origin, fn plan ->
          TemplatePatchValidator.with_renderer_policy_context(plan, tree, opts, fn policy ->
            TemplatePatchValidator.with_renderer_policy_evidence(policy, tree, fn evidence ->
              records =
                Enum.filter(
                  evidence.references,
                  &(&1.kind == :font and &1.reference == "proof.ttf")
                )

              assert length(records) == 2

              assert Enum.all?(
                       records,
                       &(&1.encoded_source == data and &1.owner == :unbound_global_preparation and
                           &1.authority_basis == :ordinary_renderer_options)
                     )

              :checked
            end)
          end)
        end)
      end)

    try do
      :erlang.trace(task.pid, true, [:call])
      send(task.pid, :prepare)
      assert :checked = Task.await(task, 10_000)
      assert_receive {:trace, _, :call, {FileReader, :read, [^path, _]}}
      refute_receive {:trace, _, :call, {FileReader, :read, [^path, _]}}
    after
      :erlang.trace_pattern({FileReader, :read, 2}, false, [:local])
    end
  end

  test "resolver reentry cannot borrow the outer consuming element owner", %{
    root: root,
    png: png,
    opts: opts
  } do
    {source, tree, parent, origin} =
      fixture("<p>Original</p><img src='https://example.invalid/outer.png'><img src='pixel.png'>")

    forged_owner = Enum.find(Map.values(tree.nodes), &(&1[:tag] == "p"))

    resolver = fn %{reference: "https://example.invalid/outer.png", kind: :image} ->
      for marker <- [forged_owner.node_id, make_ref()] do
        assert {:ok, ^png} =
                 AssetLoader.resolve("nested.png", :image,
                   assets: %{"nested.png" => {:bytes, png}},
                   __template_source_owner__: marker
                 )
      end

      nested_opts =
        TemplateRendererContextValidator.source_owner(forged_owner,
          assets: %{"nested-issued.png" => {:bytes, png}}
        )

      assert {:ok, ^png} = AssetLoader.resolve("nested-issued.png", :image, nested_opts)
      {:ok, png}
    end

    assert :checked =
             in_scope(root, source, tree, parent, origin, fn plan ->
               TemplatePatchValidator.with_renderer_policy_context(
                 plan,
                 tree,
                 Keyword.put(opts, :asset_resolver, resolver),
                 fn policy ->
                   TemplatePatchValidator.with_renderer_policy_evidence(
                     policy,
                     tree,
                     fn evidence ->
                       nested =
                         Enum.filter(
                           evidence.references,
                           &String.starts_with?(&1.reference, "nested")
                         )

                       assert length(nested) == 3
                       assert Enum.all?(nested, &(&1.owner == :unbound_global_preparation))

                       [outer] =
                         Enum.filter(
                           evidence.references,
                           &(&1.reference == "https://example.invalid/outer.png")
                         )

                       [following] =
                         Enum.filter(evidence.references, &(&1.reference == "pixel.png"))

                       assert outer.owner.tag == "img" and
                                outer.owner.attributes["src"] == outer.reference

                       assert following.owner.tag == "img" and
                                following.owner.attributes["src"] == "pixel.png"

                       assert Enum.all?(evidence.references, &(&1.encoded_source == png))
                       assert length(evidence.pending_checks) == 7
                       :checked
                     end
                   )
                 end
               )
             end)
  end

  test "resource frames restore after callback exceptions and preparation cleanup", %{
    root: root,
    png: png,
    opts: opts
  } do
    {source, tree, parent, origin} = fixture("<img src='https://example.invalid/outer.png'>")
    owner = Enum.find(Map.values(tree.nodes), &(&1[:tag] == "img"))
    key = {__MODULE__, :owner_opts}

    try do
      resolver = fn _ ->
        assert_raise RuntimeError, "resource failure", fn ->
          TemplateRendererContextValidator.with_resource_observation("throw", :image, [], fn ->
            raise "resource failure"
          end)
        end

        Process.put(key, TemplateRendererContextValidator.source_owner(owner, []))
        {:ok, png}
      end

      assert :checked =
               in_scope(root, source, tree, parent, origin, fn plan ->
                 TemplatePatchValidator.with_renderer_policy_context(
                   plan,
                   tree,
                   Keyword.put(opts, :asset_resolver, resolver),
                   fn policy ->
                     TemplatePatchValidator.with_renderer_policy_evidence(
                       policy,
                       tree,
                       fn evidence ->
                         [record] = evidence.references
                         assert record.owner.node_id == owner.node_id
                         refute Keyword.has_key?(Process.get(key), :__template_source_owner__)
                         :checked
                       end
                     )
                   end
                 )
               end)

      assert {:ok, ^png} = AssetLoader.resolve("pixel.png", :image, opts)
      assert Process.get({TemplateRendererContextValidator, :preparation}) == nil
    after
      Process.delete(key)
    end
  end

  test "reentrant Style leaves outer observations unchanged", %{
    root: root,
    opts: opts,
    png: png
  } do
    {source, tree, parent, origin} =
      fixture("<p>Proof</p><img src='https://example.invalid/outer.png'>")

    resolver = fn _ ->
      nested_opts =
        Keyword.merge(opts,
          assets: %{"https://example.invalid/outer.png" => {:bytes, png}},
          stylesheets: [{:css, "p{font-size:99px}"}]
        )

      assert {:ok, nested} = Style.compute_detailed(HtmlTree.fetch(tree, tree.root), nested_opts)

      paragraph =
        nested.children
        |> Stream.unfold(fn
          [] -> nil
          [node | pending] -> {node, Map.get(node, :children, []) ++ pending}
        end)
        |> Enum.find(&(&1[:tag] == "p"))

      assert paragraph.style.font_size == 74.25
      {:ok, png}
    end

    assert :checked =
             in_scope(root, source, tree, parent, origin, fn plan ->
               TemplatePatchValidator.with_renderer_policy_context(
                 plan,
                 tree,
                 Keyword.put(opts, :asset_resolver, resolver),
                 fn policy ->
                   TemplatePatchValidator.with_renderer_policy_evidence(
                     policy,
                     tree,
                     fn evidence ->
                       paragraph = Enum.find(Map.values(evidence.nodes), &(&1.node.tag == "p"))
                       assert paragraph.font.font_size == 12.0

                       [outer] =
                         Enum.filter(
                           evidence.references,
                           &(&1.reference == "https://example.invalid/outer.png" and
                               is_map(&1.owner))
                         )

                       assert outer.owner.tag == "img" and outer.encoded_source == png
                       assert length(evidence.pending_checks) == 7

                       assert {:error, {:unsupported_html, diagnostic}} =
                                TemplatePatchValidator.authorize_mutation(plan, tree)

                       assert diagnostic.message =~ "selected_fonts_and_metrics"
                       :checked
                     end
                   )
                 end
               )
             end)

    assert Process.get({TemplateRendererContextValidator, :preparation}) == nil
  end

  test "changed effective policy limits reject replay and caller options cannot forge source ownership",
       %{root: root, opts: opts} do
    {source, tree, parent, origin} = fixture("<p>Original</p>")
    limits = Limits.effective()

    assert :checked =
             in_scope(root, source, tree, parent, origin, fn plan ->
               assert {:error, {:invalid_options, diagnostic}} =
                        TemplatePatchValidator.with_renderer_policy_context(
                          plan,
                          tree,
                          Keyword.put(opts, :__template_source_owner__, parent),
                          fn _ -> flunk("caller source ownership") end
                        )

               assert diagnostic.stage == :options and
                        diagnostic.module == TemplateRendererContextValidator

               assert diagnostic.source == "__template_source_owner__" and
                        diagnostic.message =~ "remove"

               TemplatePatchValidator.with_renderer_policy_context(plan, tree, opts, fn policy ->
                 for key <- [:max_css_numeric_magnitude, :max_font_source_bytes] do
                   Limits.install(Map.put(limits, key, 1))

                   assert {:error, {:invalid_document, changed}} =
                            TemplatePatchValidator.with_renderer_policy_evidence(
                              policy,
                              tree,
                              fn _ -> flunk("changed Limits replay") end
                            )

                   assert changed.message =~ "configuration changed"
                   Limits.install(limits)
                 end

                 assert :checked =
                          TemplatePatchValidator.with_renderer_policy_evidence(
                            policy,
                            tree,
                            fn _ -> :checked end
                          )

                 :checked
               end)
             end)
  end

  defp fixture(authored, css \\ "") do
    prefix = "<head><style>#{css}</style></head><body><div><?marker name='A'>#{authored}</div>"
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

  defp in_scope(root, source, tree, parent, origin, consumer) do
    ResourceContext.run([authorization_root: root], HtmlValidator.new_image_budget(), fn _ ->
      with_plan(source, tree, parent, origin, consumer)
    end)
  end

  defp with_plan(source, tree, parent, origin, consumer) do
    TemplatePatchValidator.with_parser_invocation(source, fn _ ->
      TemplatePatchValidator.with_insertion_context(tree, parent, "A", origin, fn plan ->
        TemplatePatchValidator.with_planned_insertion(plan, tree, fn _ -> consumer.(plan) end)
      end)
    end)
  end
end
