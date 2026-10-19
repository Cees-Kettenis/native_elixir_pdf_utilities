alias NativeElixirPdfUtilities.TestSupport.Paths

defmodule NativeElixirPdfUtilities.Validators.TemplateRendererSourceContextTest do
  use ExUnit.Case, async: false

  Code.require_file(Paths.support("png_fixture.ex"))

  alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, HtmlTree, ResourceContext, Style}
  alias NativeElixirPdfUtilities.TestSupport.PngFixture

  alias NativeElixirPdfUtilities.Validators.{
    HtmlCssSourceValidator,
    HtmlValidator,
    TemplatePatchValidator,
    TemplateRendererContextValidator
  }

  setup do
    root =
      Path.join(
        System.tmp_dir!(),
        "native-pdf-template-source-#{System.unique_integer([:positive])}"
      )

    File.mkdir_p!(root)
    png = PngFixture.build(1, 1, 2, 8, 0, fn _, _ -> [255, 0, 0] end)
    on_exit(fn -> File.rm_rf!(root) end)

    %{
      root: root,
      png: png,
      opts: [system_font_discovery: false, assets: %{"pixel.png" => {:bytes, png}}]
    }
  end

  test "fallback runs retain exact inline and inherited original declaration contexts", %{
    root: root,
    opts: opts
  } do
    {source, tree, parent, origin} =
      fixture(
        "<section style='font-family:DejaVu Sans, STIX Two Math'><p style='font-size:18px;text-transform:uppercase'>A𝑥漢</p></section>",
        "#target{font-size:20px}"
      )

    assert :checked =
             prepare(root, source, tree, parent, origin, opts, fn plan, evidence ->
               runs = evidence.glyph_runs |> Map.values() |> List.flatten()
               assert Enum.map_join(runs, & &1.resolved_text) == "A𝑥�"
               assert Enum.any?(runs, &(&1.font_face.family == "STIX Two Math"))
               contexts = hd(runs).declaration_contexts
               [own | _] = contexts
               assert own.owner.tag == "p"

               assert Enum.map(own.declarations, & &1.source.text) == [
                        "font-size:18px",
                        "text-transform:uppercase"
                      ]

               assert Enum.all?(
                        own.declarations,
                        &(&1.source.provenance == :original_html and
                            &1.source.owner.node_id == own.owner.node_id)
                      )

               inherited = Enum.find(contexts, &(&1.owner.tag == "section"))
               assert [family] = inherited.declarations
               assert family.source.text == "font-family:DejaVu Sans, STIX Two Math"
               assert family.context.node_id == inherited.owner.node_id
               assert family.selection_policy == :requires_final_winning_declaration

               assert evidence.parent_declaration_contexts.policy ==
                        :observed_existing_parent_context_only

               [prospective | _] = evidence.parent_declaration_contexts.prospective
               assert prospective.owner.attributes["id"] == "target"
               assert [size] = prospective.declarations

               assert size.source.text == "font-size:20px" and
                        tree.nodes[size.source.owner.node_id].tag == "style"

               assert {:error, {:unsupported_html, diagnostic}} =
                        TemplatePatchValidator.authorize_mutation(plan, tree)

               assert diagnostic.message =~ "insertion_style_context"
               :checked
             end)
  end

  test "identical embedded CSS keeps distinct source owners and background candidates without future or inert borrowing",
       %{root: root, opts: opts, png: png} do
    css = "p{background-image:url('pixel.png');font-family:DejaVu Sans}"

    sheets =
      css <>
        "</style><style>" <>
        css <>
        "@media print and (min-width:1px){p{font-size:18px}}@media screen{p{font-size:99px}}"

    {source, tree, parent, origin} =
      fixture(
        "<p>Proof</p><template><style>p{font-size:99px}</style><p style='font-size:99px'>Inert</p></template>",
        sheets
      )

    future = "<style>p{font-family:STIX Two Math}</style>"
    source = source <> future
    owner = tree.nodes[tree.root]

    tree = %{
      tree
      | nodes: Map.put(tree.nodes, tree.root, %{owner | source: %{owner.source | text: source}})
    }

    assert :checked =
             prepare(root, source, tree, parent, origin, opts, fn _, evidence ->
               [background] = Enum.filter(evidence.references, &(&1.kind == :background_image))
               assert background.encoded_source == png and background.owner.tag == "p"
               assert length(background.defining_declaration_candidates) == 2
               candidates = background.defining_declaration_candidates

               assert Enum.all?(
                        candidates,
                        &(&1.source.text == "background-image:url('pixel.png')" and
                            &1.source.provenance == :original_html)
                      )

               assert length(Enum.uniq_by(candidates, & &1.source.owner.node_id)) == 2
               assert length(Enum.uniq_by(candidates, & &1.source.offset)) == 2

               assert Enum.all?(candidates, fn declaration ->
                        [reference] = declaration.references

                        reference.decoded == "pixel.png" and
                          reference.source.text == "url('pixel.png')"
                      end)

               assert background.defining_declaration_policy ==
                        :requires_final_winning_declaration

               runs = evidence.glyph_runs |> Map.values() |> List.flatten()
               assert Enum.map_join(runs, & &1.resolved_text) == "Proof"

               assert Enum.all?(
                        runs,
                        &(&1.font_face.family == "DejaVu Sans" and &1.font_size == 13.5)
                      )

               own = hd(hd(runs).declaration_contexts)

               assert Enum.count(own.declarations, &(&1.declaration == {"font-size", "18px"})) ==
                        1

               refute Enum.any?(own.declarations, &(&1.declaration == {"font-size", "99px"}))

               inert =
                 Enum.find(
                   Map.values(tree.nodes),
                   &(&1[:tag] == "p" and &1[:attributes]["style"] == "font-size:99px")
                 )

               assert [inert_declaration] = evidence.authored_prefix.inline[inert.node_id]
               assert inert_declaration.source.text == "font-size:99px"
               assert inert_declaration.context_policy == :requires_applicable_original_context
               refute Map.has_key?(evidence.declaration_contexts, {inert.node_id, nil})
               assert evidence.authored_prefix.finish == origin.offset
               assert evidence.authored_prefix.policy == :original_prefix_provenance_only
               assert evidence.authored_prefix.completion == :requires_remaining_original_source
               assert length(evidence.pending_checks) == 7
               :checked
             end)
  end

  test "reentrant normal Style cannot overwrite outer node or declaration observations", %{
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

      assert {:ok, _} = Style.compute_detailed(HtmlTree.fetch(tree, tree.root), nested_opts)
      {:ok, png}
    end

    assert :checked =
             prepare(
               root,
               source,
               tree,
               parent,
               origin,
               Keyword.put(opts, :asset_resolver, resolver),
               fn _, evidence ->
                 paragraph = Enum.find(Map.values(evidence.nodes), &(&1.node.tag == "p"))
                 assert paragraph.font.font_size == 12.0
                 assert evidence.declaration_contexts[{paragraph.node.node_id, nil}] == []

                 [record] =
                   Enum.filter(
                     evidence.references,
                     &(&1.reference == "https://example.invalid/outer.png" and is_map(&1.owner))
                   )

                 assert record.owner.tag == "img"
                 assert length(evidence.pending_checks) == 7
                 :checked
               end
             )

    assert Process.get({TemplateRendererContextValidator, :preparation}) == nil
  end

  test "prefix ownership rejects future nodes and invalid offsets using shared diagnostics" do
    source = "<p style='font-size:18px'>Before</p><p style='font-size:20px'>After</p>"
    assert {:ok, dom} = HtmlParser.parse_document_detailed(source)
    {finish, _} = :binary.match(source, "<p style='font-size:20px'>")

    assert {:error, {:invalid_document, diagnostic}} =
             HtmlCssSourceValidator.prepare_prefix(dom, finish)

    assert diagnostic.stage == :html and diagnostic.module == HtmlCssSourceValidator
    assert diagnostic.source == "<p style='font-size:20px'>"
    assert diagnostic.operation == :source_style_contexts

    for invalid <- [-1, byte_size(source) + 1, :caller] do
      assert {:error, {:invalid_document, invalid_diagnostic}} =
               HtmlCssSourceValidator.prepare_prefix(dom, invalid)

      assert invalid_diagnostic.message =~ "original byte offset"
    end
  end

  test "source binding failures retain shared actionable diagnostics without issuing evidence", %{
    root: root
  } do
    assert {:ok, dom} =
             HtmlParser.parse_document_detailed("<style>p{font-size:18px}</style><p>Proof</p>")

    assert {:ok, provenance} = HtmlCssSourceValidator.prepare(dom)
    tree = HtmlTree.from_document(dom)
    paragraph = Enum.find(Map.values(tree.nodes), &(&1[:tag] == "p"))
    stylesheet = Enum.find(Map.values(tree.nodes), &(&1[:tag] == "style"))

    assert {:ok, prepared} =
             NativeElixirPdfUtilities.HtmlToPdf.CssParser.prepare_stylesheet("p{font-size:18px}")

    [authored] = provenance.embedded

    assert {:ok, source_prepared} =
             NativeElixirPdfUtilities.HtmlToPdf.CssParser.prepare_source_stylesheet(
               authored.css,
               authored.descriptor
             )

    [record] = hd(source_prepared.rules).source_declarations
    key = {TemplateRendererContextValidator, :preparation}

    ResourceContext.run(
      [authorization_root: root],
      HtmlValidator.new_image_budget(),
      fn current ->
        Process.put(key, %{
          resource_frame: nil,
          resource_context: current,
          tree: tree,
          provenance: provenance,
          cascades: %{}
        })

        try do
          entry = %{
            css: authored.css,
            __template_stylesheet_owner__: %{
              stylesheet
              | source: %{stylesheet.source | text: "<style forged>"}
            }
          }

          assert {:error, {:invalid_document, owner_error}} =
                   TemplateRendererContextValidator.bind_stylesheet(entry, prepared, nil)

          assert owner_error.stage == :css and owner_error.message =~ "original HTML source owner"
          assert owner_error.operation == :template_renderer_context

          entry = %{css: authored.css, __template_stylesheet_owner__: stylesheet}

          mismatch = %{
            prepared
            | rules: [%{hd(prepared.rules) | declarations: [{"font-size", "99px"}]}]
          }

          assert {:error, {:invalid_document, rule_error}} =
                   TemplateRendererContextValidator.bind_stylesheet(entry, mismatch, nil)

          assert rule_error.message =~ "actual prepared stylesheet cascade"

          matched = %{declaration: {"font-size", "99px"}, inline: false, source_record: record}

          assert {:error, {:invalid_document, declaration_error}} =
                   TemplateRendererContextValidator.observe_cascade(paragraph, nil, [matched])

          assert declaration_error.source == "font-size:18px" and declaration_error.line == 1
          assert declaration_error.column == 10 and declaration_error.stage == :css
          assert declaration_error.operation == :template_renderer_context
          assert Process.get(key).cascades == %{}
        after
          Process.delete(key)
        end
      end
    )
  end

  test "unavailable font assets propagate source diagnostics without candidate ownership",
       %{root: root, opts: opts} do
    css = "@font-face{font-family:Chosen;src:url('missing.ttf')}p{font-family:Chosen}"
    {source, tree, parent, origin} = fixture("<p>Chosen</p>", css)
    opts = Keyword.put(opts, :asset_resolver, fn _ -> {:error, :unavailable} end)

    assert {:error, {reason, diagnostic}} =
             prepare(root, source, tree, parent, origin, opts, fn _, _ ->
               flunk("unavailable font source must not issue evidence")
             end)

    assert diagnostic.reason == reason
    assert diagnostic.stage == :asset
    assert diagnostic.source == "missing.ttf"
    assert diagnostic.message != ""
    assert Process.get({TemplateRendererContextValidator, :preparation}) == nil
  end

  test "a font loader policy change cannot issue expired candidate evidence", %{
    root: root,
    opts: opts
  } do
    alias NativeElixirPdfUtilities.Limits
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    data = File.read!("priv/fonts/dejavu/DejaVuSans.ttf")
    css = "@font-face{font-family:Chosen;src:url('chosen.ttf')}p{font-family:Chosen}"
    {source, tree, parent, origin} = fixture("<p>Chosen</p>", css)

    opts =
      Keyword.put(opts, :asset_resolver, fn _ ->
        Limits.install(Map.update!(limits, :max_html_depth, &(&1 + 1)))
        {:ok, data}
      end)

    assert {:error, {reason, diagnostic}} =
             prepare(root, source, tree, parent, origin, opts, fn _, _ ->
               flunk("expired font loading policy must not issue evidence")
             end)

    assert diagnostic.reason == reason
    assert diagnostic.module == TemplateRendererContextValidator
    assert diagnostic.stage == :font
    assert diagnostic.message =~ "font"
    assert Process.get({TemplateRendererContextValidator, :preparation}) == nil
  end

  test "font provider tickets expire when source preparation finishes", %{
    root: root,
    opts: opts
  } do
    data = File.read!("priv/fonts/dejavu/DejaVuSans.ttf")
    css = "@font-face{font-family:Chosen;src:url('chosen.ttf')}p{font-family:Chosen}"
    {source, tree, parent, origin} = fixture("<p>Chosen</p>", css)

    opts =
      Keyword.put(opts, :asset_resolver, fn _ ->
        preparation = Process.get({TemplateRendererContextValidator, :preparation})
        [provider] = Map.keys(preparation.font_providers)
        send(self(), {:issued_provider, provider})
        {:ok, data}
      end)

    assert :checked =
             prepare(root, source, tree, parent, origin, opts, fn _, evidence ->
               assert Enum.any?(evidence.glyph_runs, fn {_, runs} ->
                        Enum.any?(runs, &(&1.authored_text == "Chosen"))
                      end)

               :checked
             end)

    assert_received {:issued_provider, provider}
    assert is_reference(provider)
    assert Process.get({TemplateRendererContextValidator, :preparation}) == nil
    config = %{family: "Chosen", data: [data]}

    assert {:ok, {^config, nil}} =
             TemplateRendererContextValidator.with_font_candidate(provider, 0, fn ->
               {:ok, config}
             end)

    assert {:error, {:invalid_document, diagnostic}} =
             TemplateRendererContextValidator.font_registry_context(provider)

    assert diagnostic.stage == :font and diagnostic.reason == :invalid_document
    assert diagnostic.operation == :template_renderer_context
    assert diagnostic.module == TemplateRendererContextValidator
    assert diagnostic.message =~ "live normal preparation source owner"
  end

  test "a resource callback cannot issue font candidate source ownership",
       %{root: root, opts: opts, png: png} do
    data = File.read!("priv/fonts/dejavu/DejaVuSans.ttf")
    css = "@font-face{font-family:Chosen;src:url('chosen.ttf')}p{font-family:Chosen}"
    {source, tree, parent, origin} = fixture("<p>Chosen</p><img src='pixel.png'>", css)
    config = %{family: "Chosen", data: [data]}

    resolver = fn _ ->
      preparation = Process.get({TemplateRendererContextValidator, :preparation})
      [ticket] = Map.keys(preparation.font_providers)
      assert is_map(preparation.resource_frame)

      assert {:ok, {^config, nil}} =
               TemplateRendererContextValidator.with_font_candidate(ticket, 99, fn ->
                 {:ok, config}
               end)

      failed = {:error, :actual_candidate_failure}

      assert ^failed =
               TemplateRendererContextValidator.with_font_candidate(ticket, 0, fn -> failed end)

      assert Process.get({TemplateRendererContextValidator, :preparation}).font_candidate_frame ==
               nil

      send(self(), :actual_font_candidate_checked)
      {:ok, png}
    end

    opts =
      opts
      |> Keyword.put(:assets, %{"chosen.ttf" => {:bytes, data}})
      |> Keyword.put(:asset_resolver, resolver)

    assert :checked =
             prepare(root, source, tree, parent, origin, opts, fn _plan, evidence ->
               assert Enum.any?(evidence.glyph_runs, fn {_id, runs} ->
                        Enum.any?(runs, &(&1.authored_text == "Chosen"))
                      end)

               :checked
             end)

    assert_received :actual_font_candidate_checked
    assert Process.get({TemplateRendererContextValidator, :preparation}) == nil
  end

  test "font selection keeps fallback candidate ordinals and original descriptor sources",
       %{root: root, opts: opts} do
    data =
      File.read!(
        Path.join(:code.priv_dir(:native_elixir_pdf_utilities), "fonts/dejavu/DejaVuSans.ttf")
      )

    css = "@font-face{font-family:Chosen;src:url('bad.ttf'),url('good.ttf')}p{font-family:Chosen}"
    {source, tree, parent, origin} = fixture("<p>Chosen</p>", css)

    opts =
      Keyword.put(opts, :assets, %{
        "bad.ttf" => {:bytes, "invalid font"},
        "good.ttf" => {:bytes, data}
      })

    assert :checked =
             prepare(root, source, tree, parent, origin, opts, fn _, evidence ->
               [run] = evidence.glyph_runs |> Map.values() |> List.flatten()
               outcome = run.font_source_outcome
               assert outcome.basis == :validated_css_candidate
               assert outcome.selected_candidate == 1
               assert outcome.candidate.candidate.candidate_index == 1
               assert outcome.candidate.candidate.declaration_index == 1
               assert outcome.candidate.candidate.source.text == "url('good.ttf')"
               assert outcome.candidate.provider.owner.tag == "style"

               assert outcome.candidate.provider.authored.source.text ==
                        "@font-face{font-family:Chosen;src:url('bad.ttf'),url('good.ttf')}"

               assert outcome.encoded_source == data
               assert [reference] = outcome.candidate.references
               assert reference.reference == "good.ttf" and reference.encoded_source == data
               assert reference.authority_basis == :ordinary_renderer_options
               assert reference.resource_context == outcome.resource_context
               :checked
             end)
  end

  test "identical bytes in distinct CSS descriptors retain distinct successful providers", %{
    root: root,
    opts: opts
  } do
    data =
      File.read!(
        Path.join(:code.priv_dir(:native_elixir_pdf_utilities), "fonts/dejavu/DejaVuSans.ttf")
      )

    css =
      "@font-face{font-family:One;src:url('same.ttf')}@font-face{font-family:Two;src:url('same.ttf')}p{font-family:One}span{font-family:Two}"

    {source, tree, parent, origin} = fixture("<p>One</p><span>Two</span>", css)
    opts = Keyword.put(opts, :assets, %{"same.ttf" => {:bytes, data}})

    assert :checked =
             prepare(root, source, tree, parent, origin, opts, fn _, evidence ->
               outcomes =
                 evidence.glyph_runs
                 |> Map.values()
                 |> List.flatten()
                 |> Enum.map(& &1.font_source_outcome)

               assert Enum.sort(Enum.map(outcomes, & &1.family)) == ["One", "Two"]

               assert Enum.all?(
                        outcomes,
                        &(&1.encoded_source == data and &1.selected_candidate == 0)
                      )

               assert length(
                        Enum.uniq(Enum.map(outcomes, & &1.candidate.provider.authored.source))
                      ) == 2

               assert length(Enum.uniq(Enum.map(outcomes, & &1.config_ordinal))) == 2
               :checked
             end)
  end

  test "caller font ownership remains explicit and opaque registry handles cannot be forged", %{
    root: root,
    opts: opts
  } do
    data =
      File.read!(
        Path.join(:code.priv_dir(:native_elixir_pdf_utilities), "fonts/dejavu/DejaVuSans.ttf")
      )

    {source, tree, parent, origin} = fixture("<p style='font-family:Caller'>Caller</p>")
    opts = Keyword.put(opts, :fonts, [%{family: "Caller", data: data}])

    assert :checked =
             prepare(root, source, tree, parent, origin, opts, fn _, evidence ->
               [run] = evidence.glyph_runs |> Map.values() |> List.flatten()
               assert run.font_source_outcome.basis == :configured
               assert run.font_source_outcome.candidate == nil
               assert run.font_source_outcome.encoded_source == data

               assert {:error, {:invalid_document, diagnostic}} =
                        TemplateRendererContextValidator.font_registry_context(make_ref())

               assert diagnostic.stage == :font and
                        diagnostic.operation == :template_renderer_context

               assert diagnostic.message =~ "live normal preparation"
               :checked
             end)

    assert {:ok, registry} = NativeElixirPdfUtilities.HtmlToPdf.Font.load_registry(opts)
    refute Enum.any?(registry.fallback, &Map.has_key?(&1, :__template_font_outcome__))
  end

  test "reentrant caller font loading cannot borrow the CSS provider or normal registry owner", %{
    root: root,
    opts: opts
  } do
    data =
      File.read!(
        Path.join(:code.priv_dir(:native_elixir_pdf_utilities), "fonts/dejavu/DejaVuSans.ttf")
      )

    css = "@font-face{font-family:Outer;src:url('outer.ttf')}p{font-family:Outer}"
    {source, tree, parent, origin} = fixture("<p>Outer</p>", css)

    resolver = fn %{reference: "outer.ttf", kind: :font} ->
      assert {:ok, nested} =
               NativeElixirPdfUtilities.HtmlToPdf.Font.load_registry(
                 fonts: [%{family: "Nested", data: data}],
                 system_font_discovery: false
               )

      refute Enum.any?(nested.fallback, &Map.has_key?(&1, :__template_font_outcome__))

      assert {:error, {:invalid_document, _}} =
               TemplateRendererContextValidator.font_registry_context(make_ref())

      {:ok, data}
    end

    opts = Keyword.put(opts, :asset_resolver, resolver)

    assert :checked =
             prepare(root, source, tree, parent, origin, opts, fn _, evidence ->
               [run] = evidence.glyph_runs |> Map.values() |> List.flatten()
               assert run.font_source_outcome.family == "Outer"

               refute Enum.any?(evidence.font_load_outcomes, fn {_, outcome} ->
                        outcome.family == "Nested"
                      end)

               :checked
             end)

    assert Process.get({NativeElixirPdfUtilities.Validators.FontValidator, :registry_loads}) ==
             nil

    assert Process.get({NativeElixirPdfUtilities.Validators.FontValidator, :source_owner}) == nil
    assert Process.get({TemplateRendererContextValidator, :preparation}) == nil
  end

  defp fixture(authored, css \\ "") do
    prefix =
      "<head><style>#{css}</style></head><body><div id='target'><?marker name='A'>#{authored}</div>"

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

  defp prepare(root, source, tree, parent, origin, opts, consumer) do
    ResourceContext.run([authorization_root: root], HtmlValidator.new_image_budget(), fn _ ->
      TemplatePatchValidator.with_parser_invocation(source, fn _ ->
        TemplatePatchValidator.with_insertion_context(tree, parent, "A", origin, fn plan ->
          TemplatePatchValidator.with_planned_insertion(plan, tree, fn _ ->
            TemplatePatchValidator.with_renderer_policy_context(plan, tree, opts, fn policy ->
              TemplatePatchValidator.with_renderer_policy_evidence(policy, tree, fn evidence ->
                consumer.(plan, evidence)
              end)
            end)
          end)
        end)
      end)
    end)
  end
end
