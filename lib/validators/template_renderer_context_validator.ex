defmodule NativeElixirPdfUtilities.Validators.TemplateRendererContextValidator do
  @moduledoc false

  alias NativeElixirPdfUtilities.Diagnostics

  alias NativeElixirPdfUtilities.HtmlToPdf.{
    CssParser,
    Font,
    FontFallback,
    HtmlTree,
    PageGeometry,
    ResourceContext,
    Style
  }

  alias NativeElixirPdfUtilities.Validators.{
    HtmlCssSourceValidator,
    HtmlValidator,
    PrintMediaValidator,
    ResourceValidator
  }

  @source_identity [
    :node_id,
    :document_id,
    :tree_scope_id,
    :parent_id,
    :source,
    :attributes,
    :tag,
    :namespace
  ]

  @doc false
  @spec prepare(HtmlTree.t(), map(), term(), ResourceContext.t()) ::
          {:ok, map()} | {:error, {atom(), map()}}
  def prepare(tree, plan, opts, resource_context) do
    prepare_context(tree, plan, opts, resource_context, nil)
  end

  @doc false
  @spec prepare_candidate(term(), HtmlTree.t(), map(), term(), ResourceContext.t()) ::
          {:ok, map()} | {:error, {atom(), map()}}
  def prepare_candidate(handle, prefix_tree, plan, opts, resource_context) do
    with :ok <- ResourceValidator.validate_context(resource_context),
         {:ok, projection} <-
           HtmlCssSourceValidator.prepare_candidate_projection(handle, prefix_tree) do
      NativeElixirPdfUtilities.Validators.TemplatePatchValidator.with_consumed_candidate_frame(
        handle,
        prefix_tree,
        fn candidate ->
          plan = Map.put(plan, :source_offset, candidate.captured_finish)
          prepare_context(candidate.prospective, plan, opts, resource_context, projection)
        end
      )
    end
  end

  defp prepare_context(tree, plan, opts, resource_context, projection) do
    result =
      with :ok <- ResourceValidator.validate_context(resource_context),
           :ok <-
             if(ResourceContext.current() == resource_context,
               do: :ok,
               else:
                 Diagnostics.error(
                   :options,
                   :invalid_options,
                   "renderer preparation requires the current issued resource context",
                   module: __MODULE__,
                   operation: :template_renderer_context,
                   source: "resource_context"
                 )
             ),
           {:ok, _controls} <-
             HtmlValidator.prepare_style_input(
               HtmlTree.fetch(tree, tree.root),
               opts,
               Font.normalize_options(opts)
             ),
           :ok <-
             if(Keyword.has_key?(opts, :__template_source_owner__),
               do:
                 Diagnostics.error(
                   :options,
                   :invalid_options,
                   "template resource source ownership is issued internally; remove __template_source_owner__",
                   module: __MODULE__,
                   operation: :template_renderer_context,
                   source: "__template_source_owner__"
                 ),
               else: :ok
             ),
           {:ok, requested_media} <- PrintMediaValidator.renderer_context(opts),
           {:ok, sheets} <- Style.prepare_stylesheets(HtmlTree.fetch(tree, tree.root), opts),
           {:ok, provenance} <-
             if(projection,
               do: {:ok, projection},
               else:
                 HtmlCssSourceValidator.prepare_prefix(
                   HtmlTree.fetch(tree, tree.root),
                   plan.source_offset
                 )
             ) do
        page_options =
          Enum.reduce(sheets, [], fn entry, acc ->
            PageGeometry.merge_page_options(acc, entry.prepared.page_options)
          end)

        effective_opts =
          page_options
          |> Keyword.merge(opts)
          |> Keyword.put_new(:print_viewport, {requested_media.width, requested_media.height})

        key = {__MODULE__, :preparation}
        previous = Process.get(key)

        Process.put(key, %{
          tree: tree,
          nodes: %{},
          references: [],
          owners: %{},
          resource_frame: nil,
          font_providers: %{},
          font_recipes: [],
          font_candidate_frame: nil,
          font_registry_frame: nil,
          font_outcomes: %{},
          preparation_limits: NativeElixirPdfUtilities.Limits.effective(),
          font_budget_id:
            Process.get({NativeElixirPdfUtilities.Validators.FontValidator, :budget}).budget_id,
          provenance: provenance,
          source_projection: projection,
          cascades: %{},
          resource_context: resource_context
        })

        try do
          with {:ok, styled} <-
                 Style.compute_detailed(
                   HtmlTree.fetch(tree, tree.root),
                   effective_opts,
                   resource_context.image_budget
                 ),
               {:ok, resolved} <-
                 FontFallback.resolve(
                   styled,
                   Keyword.get(effective_opts, :unsupported_glyphs, :replace)
                 ),
               {:ok, glyph_runs} <-
                 validate_prepared_glyph_runs(resolved.children, tree, Process.get(key).cascades),
               glyph_runs =
                 (if projection do
                    Map.new(glyph_runs, fn {id, runs} ->
                      runs =
                        Enum.map(runs, fn run ->
                          HtmlValidator.reserve_render_resource(:max_html_tree_work, 1, :font)
                          placement = Map.get(projection.placements, run.owner.node_id)

                          Map.put(
                            run,
                            :original_owner,
                            if(placement, do: placement.original, else: :unbound_synthetic_source)
                          )
                        end)

                      {id, runs}
                    end)
                  else
                    glyph_runs
                  end),
               glyph_runs =
                 Map.new(glyph_runs, fn {id, runs} ->
                   {id,
                    Enum.map(runs, fn run ->
                      HtmlValidator.reserve_render_resource(:max_html_tree_work, 1, :font)
                      ticket = Map.get(run.font_face, :__template_font_outcome__)
                      outcome = Map.get(Process.get(key).font_outcomes, ticket)
                      Map.put(run, :font_source_outcome, outcome || :unbound_later_or_system_face)
                    end)}
                 end),
               page_size =
                 PageGeometry.normalize_page_size(Keyword.get(effective_opts, :page_size, :a4)),
               margins = PageGeometry.normalize_margins(Keyword.get(effective_opts, :margin, 0)),
               :ok <-
                 HtmlValidator.validate_layout_input(styled, effective_opts, page_size, margins),
               {:ok, effective_media} <- PrintMediaValidator.context(effective_opts) do
            {:ok, page_size} = page_size
            {:ok, margins} = margins
            nodes = Process.get(key).nodes

            {:ok,
             %{
               source: tree.nodes[tree.root].source,
               preparation_scope:
                 if(projection, do: :prospective_insertion, else: :original_prefix),
               source_placements: if(projection, do: projection.placements, else: %{}),
               original: plan.original,
               insertion: plan.insertion,
               nodes: nodes,
               authored_prefix: %{
                 finish: plan.source_offset,
                 inline: provenance.records,
                 embedded:
                   Enum.map(provenance.embedded, &Map.take(&1, [:css, :node, :insertion_context])),
                 insertion_contexts: provenance.insertion_contexts,
                 policy: :original_prefix_provenance_only,
                 completion: :requires_remaining_original_source
               },
               declaration_contexts: Process.get(key).cascades,
               glyph_runs: glyph_runs,
               parent_declaration_contexts: %{
                 original:
                   if(projection,
                     do: :requires_separate_original_preparation,
                     else:
                       declaration_chain(tree, plan.original.parent, Process.get(key).cascades)
                   ),
                 prospective:
                   declaration_chain(tree, plan.insertion.parent, Process.get(key).cascades),
                 policy:
                   if(projection,
                     do: :actual_prospective_parent_cascade_observed,
                     else: :observed_existing_parent_context_only
                   )
               },
               glyph_policy: :normal_generic_text_fallback_observed,
               pending_glyph_owners: [:generated_content, :control_text, :unvisited_authored_text],
               references: Process.get(key).references |> Enum.reverse(),
               font_load_outcomes: Process.get(key).font_outcomes,
               prospective_policy:
                 if(projection,
                   do: :ordinary_prospective_style_and_generic_fallback_observed,
                   else: :requires_actual_insertion_preparation
                 ),
               original_parent:
                 if(projection,
                   do: :requires_separate_original_preparation,
                   else: Map.get(nodes, plan.original.parent, :not_prepared_by_normal_style)
                 ),
               insertion_parent:
                 Map.get(nodes, plan.insertion.parent, :not_prepared_by_normal_style),
               page: %{
                 requested_media: requested_media,
                 effective_media: effective_media,
                 page_size: page_size,
                 margins: margins,
                 policy: :validated_effective_renderer_geometry
               },
               font_policy: :prepared_primary_faces_and_loaded_metrics,
               resource_policy: :prepared_active_style_outcomes,
               resource_authority: %{
                 scope: resource_context.scope,
                 document: resource_context.document_id,
                 tree_scope: resource_context.tree_scope_id,
                 depth: resource_context.depth,
                 basis:
                   if(resource_context.depth > 0,
                     do: :issued_local_child,
                     else: :ordinary_root_renderer_options
                   ),
                 policy: :requires_complete_per_reference_evidence
               },
               pending_selection: [:glyph_fallback, :final_math_faces, :final_svg_faces],
               pending_resources: [:deferred_embedded_viewports, :unvisited_authored_resources],
               pending_checks: plan.pending_checks
             }}
          end
        after
          if previous, do: Process.put(key, previous), else: Process.delete(key)
        end
      end

    case result do
      {:ok, _evidence} ->
        result

      {:error, {_reason, _diagnostic}} = error ->
        error

      {:error, reason} ->
        Diagnostics.error(
          :style,
          reason,
          "ordinary renderer policy preparation failed: #{reason}",
          module: __MODULE__,
          operation: :template_renderer_context
        )
    end
  end

  @doc false
  @spec observe(map(), map()) :: :ok | {:error, {atom(), map()}}
  def observe(node, style) do
    case Process.get({__MODULE__, :preparation}) do
      nil ->
        :ok

      %{resource_frame: frame} when not is_nil(frame) ->
        :ok

      %{tree: tree, nodes: nodes, resource_context: expected} = preparation ->
        HtmlValidator.reserve_render_resource(:max_html_tree_work, 1, :style)
        original = Map.get(tree.nodes, Map.get(node, :node_id))

        binding = @source_identity

        cond do
          ResourceContext.current() != expected or original == nil ->
            :ok

          true ->
            with :ok <- validate_prepared_source(node, original, :style) do
              evidence = %{
                node: Map.take(original, binding),
                font: Map.take(style, [:font_face, :_font_registry, :font_families, :font_size]),
                resources: Map.take(style, [:image, :background_image, :svg_image, :svg_scene]),
                style: style,
                policy: :ordinary_style_prepared
              }

              Process.put({__MODULE__, :preparation}, %{
                preparation
                | nodes: Map.put(nodes, node.node_id, evidence)
              })

              :ok
            end
        end
    end
  end

  @doc false
  @spec validate_prepared_source(map(), map(), :style | :font) ::
          :ok | {:error, {atom(), map()}}
  def validate_prepared_source(node, original, stage) do
    if Map.take(node, @source_identity) == Map.take(original, @source_identity) and
         (stage != :font or (Map.get(node, :type) == :text and Map.get(original, :type) == :text)) do
      :ok
    else
      message =
        if stage == :font,
          do: "normal font run lost its original text source identity",
          else: "prepared style belongs to a different original node or source scope"

      # Selecting diagnostic context does not validate or confer source ownership.
      source =
        Enum.find_value([Map.get(node, :source), Map.get(original, :source)], %{}, fn source ->
          case source do
            %{text: text, line: line, column: column}
            when is_binary(text) and is_integer(line) and line > 0 and is_integer(column) and
                   column > 0 ->
              %{text: text, line: line, column: column}

            _ ->
              nil
          end
        end)

      Diagnostics.error(stage, :invalid_document, message,
        module: __MODULE__,
        operation: :template_renderer_context,
        source: Map.get(source, :text),
        line: Map.get(source, :line),
        column: Map.get(source, :column)
      )
    end
  end

  @doc false
  @spec stylesheet_owner(map(), map()) :: map()
  def stylesheet_owner(node, entry) do
    case Process.get({__MODULE__, :preparation}) do
      %{resource_frame: nil, resource_context: expected, tree: tree} = preparation ->
        original = Map.get(tree.nodes, Map.get(node, :node_id))

        if ResourceContext.current() == expected && original &&
             Map.take(original, @source_identity) == Map.take(node, @source_identity) do
          owner =
            if preparation.source_projection,
              do:
                (case Map.get(preparation.source_projection.placements, node.node_id) do
                   %{original: owner} -> owner
                   _ -> :unbound_synthetic_source
                 end),
              else: original

          if is_map(owner),
            do: Map.put(entry, :__template_stylesheet_owner__, Map.take(owner, @source_identity)),
            else: entry
        else
          entry
        end

      _ ->
        entry
    end
  end

  @doc false
  @spec bind_stylesheet(map(), map(), term()) :: {:ok, map()} | {:error, {atom(), map()}}
  def bind_stylesheet(entry, prepared, media_context) do
    case Process.get({__MODULE__, :preparation}) do
      %{resource_frame: nil, resource_context: expected} = preparation ->
        if ResourceContext.current() == expected do
          owner = Map.get(entry, :__template_stylesheet_owner__)

          descriptor =
            if owner do
              Enum.find_value(preparation.provenance.embedded, fn authored ->
                HtmlValidator.reserve_render_resource(:max_css_work, 1, :css)

                if Map.take(authored.node, @source_identity) == Map.take(owner, @source_identity),
                  do: authored.descriptor
              end)
            end

          HtmlValidator.reserve_render_resource(:max_css_work, length(prepared.rules), :css)

          with :ok <-
                 if(owner == nil or descriptor != nil,
                   do: :ok,
                   else:
                     Diagnostics.error(
                       :css,
                       :invalid_document,
                       "prepared stylesheet lost its original HTML source owner",
                       module: __MODULE__,
                       operation: :template_renderer_context
                     )
                 ),
               {:ok, authored} <-
                 CssParser.prepare_source_stylesheet(entry.css, descriptor, media_context),
               :ok <-
                 if(
                   Enum.map(prepared.rules, & &1.declarations) ==
                     Enum.map(authored.rules, & &1.declarations) and
                     prepared.font_faces == authored.font_faces,
                   do: :ok,
                   else:
                     Diagnostics.error(
                       :css,
                       :invalid_document,
                       "source declarations do not match the actual prepared stylesheet cascade",
                       module: __MODULE__,
                       operation: :template_renderer_context
                     )
                 ) do
            rules =
              Enum.zip_with(prepared.rules, authored.rules, fn rule, source_rule ->
                HtmlValidator.reserve_render_resource(:max_css_work, 1, :css)
                Map.put(rule, :source_declarations, source_rule.source_declarations)
              end)

            providers =
              authored.authored_fonts
              |> Enum.filter(& &1.active)
              |> Enum.map(fn font ->
                HtmlValidator.reserve_render_resource(:max_css_work, 1, :css)
                ticket = make_ref()

                provider = %{
                  authored: font,
                  owner: owner,
                  resolution_base: Map.get(entry, :base_url),
                  shadow_scope_id: Map.get(entry, :shadow_scope_id),
                  shadow_host_id: Map.get(entry, :shadow_host_id),
                  resource_context: expected
                }

                current = Process.get({__MODULE__, :preparation})

                Process.put(
                  {__MODULE__, :preparation},
                  Map.put(
                    current,
                    :font_providers,
                    Map.put(Map.get(current, :font_providers, %{}), ticket, provider)
                  )
                )

                ticket
              end)

            {:ok, Map.put(%{prepared | rules: rules}, :font_source_tickets, providers)}
          end
        else
          {:ok, prepared}
        end

      _ ->
        {:ok, prepared}
    end
  end

  @doc false
  @spec with_font_candidate(term(), non_neg_integer(), (-> term())) :: term()
  def with_font_candidate(ticket, ordinal, runner) do
    key = {__MODULE__, :preparation}
    preparation = Process.get(key)

    provider =
      if is_map(preparation), do: Map.get(Map.get(preparation, :font_providers, %{}), ticket)

    if provider && live_font_preparation?(preparation) && preparation.font_candidate_frame == nil do
      candidates =
        Enum.filter(
          provider.authored.candidates,
          &(&1.role in [:first_candidate, :fallback_candidate])
        )

      candidate = Enum.at(candidates, ordinal)
      HtmlValidator.reserve_render_resource(:max_css_work, length(candidates) + 1, :font)

      if candidate do
        frame = %{provider: ticket, candidate: candidate, context: preparation.resource_context}
        Process.put(key, %{preparation | font_candidate_frame: frame})

        try do
          case runner.() do
            {:ok, config} ->
              current = Process.get(key)

              if live_font_preparation?(current) do
                {:ok,
                 {config,
                  %{
                    provider: provider,
                    candidate: candidate,
                    references:
                      Enum.filter(current.references, fn reference ->
                        HtmlValidator.reserve_render_resource(:max_html_tree_work, 1, :font)
                        Map.get(reference, :font_candidate) == frame
                      end)
                  }}}
              else
                font_source_failure()
              end

            result ->
              result
          end
        after
          current = Process.get(key)
          Process.put(key, %{current | font_candidate_frame: nil})
        end
      else
        font_source_failure()
      end
    else
      case runner.() do
        {:ok, config} -> {:ok, {config, nil}}
        result -> result
      end
    end
  end

  @doc false
  @spec record_font_recipe(term(), [term()], map()) :: :ok
  def record_font_recipe(provider, candidates, config) do
    key = {__MODULE__, :preparation}
    preparation = Process.get(key)

    if is_map(preparation) && live_font_preparation?(preparation) &&
         Map.has_key?(preparation.font_providers, provider) &&
         Enum.all?(candidates, &is_map/1) do
      HtmlValidator.reserve_render_resource(:max_css_work, length(candidates) + 1, :font)
      recipe = %{provider: provider, candidates: candidates, config: config}
      Process.put(key, %{preparation | font_recipes: preparation.font_recipes ++ [recipe]})
    end

    :ok
  end

  @doc false
  @spec with_font_registry(keyword(), [map()], (-> term())) :: term()
  def with_font_registry(opts, css_fonts, runner) do
    key = {__MODULE__, :preparation}
    preparation = Process.get(key)

    if is_map(preparation) && live_font_preparation?(preparation) &&
         preparation.font_registry_frame == nil &&
         Enum.map(preparation.font_recipes, & &1.config) == css_fonts do
      ticket = make_ref()

      frame = %{
        ticket: ticket,
        caller_count: length(Keyword.get(opts, :fonts, [])),
        recipes: preparation.font_recipes,
        context: preparation.resource_context
      }

      Process.put(key, %{preparation | font_registry_frame: frame})

      try do
        NativeElixirPdfUtilities.Validators.FontValidator.with_source_owner(ticket, runner)
      after
        current = Process.get(key)
        Process.put(key, %{current | font_registry_frame: nil})
      end
    else
      runner.()
    end
  end

  @doc false
  @spec font_registry_context(term()) :: {:ok, map()} | {:error, {atom(), map()}}
  def font_registry_context(ticket) do
    preparation = Process.get({__MODULE__, :preparation})

    if is_map(preparation) && live_font_preparation?(preparation) &&
         is_map(preparation.font_registry_frame) &&
         preparation.font_registry_frame.ticket == ticket do
      {:ok, preparation.font_registry_frame}
    else
      font_source_failure()
    end
  end

  @doc false
  @spec observe_font_load(term(), atom(), non_neg_integer(), non_neg_integer(), map(), map()) ::
          map()
  def observe_font_load(ticket, source, ordinal, selected, config, loaded) do
    key = {__MODULE__, :preparation}

    with {:ok, frame} <- font_registry_context(ticket) do
      preparation = Process.get(key)

      recipe =
        if source == :configured && ordinal >= frame.caller_count,
          do: Enum.at(frame.recipes, ordinal - frame.caller_count)

      candidate = if recipe && recipe.config == config, do: Enum.at(recipe.candidates, selected)

      if recipe && candidate == nil do
        loaded
      else
        HtmlValidator.reserve_render_resource(:max_html_tree_work, 1, :font)
        outcome_ticket = make_ref()

        outcome = %{
          source: source,
          config_ordinal: ordinal,
          selected_candidate: selected,
          candidate: candidate,
          family: loaded.family,
          weight: loaded.weight,
          style: loaded.style,
          encoded_source: loaded.data,
          resource_context: preparation.resource_context,
          limits: preparation.preparation_limits,
          basis: if(candidate, do: :validated_css_candidate, else: source),
          shadow_scope_id: if(candidate, do: candidate.provider.shadow_scope_id),
          policy: :successful_normal_font_parse_and_embedding
        }

        Process.put(key, %{
          preparation
          | font_outcomes: Map.put(preparation.font_outcomes, outcome_ticket, outcome)
        })

        Map.put(loaded, :__template_font_outcome__, outcome_ticket)
      end
    else
      _ -> loaded
    end
  end

  # A provider belongs to this ordinary preparation, its live authority and exact policy ceilings.
  defp live_font_preparation?(preparation) do
    is_map(preparation) && Map.get(preparation, :resource_frame) == nil &&
      ResourceContext.current() == preparation.resource_context &&
      ResourceValidator.validate_context(preparation.resource_context) == :ok &&
      Map.get(preparation, :preparation_limits) == NativeElixirPdfUtilities.Limits.effective() &&
      Map.get(
        Process.get({NativeElixirPdfUtilities.Validators.FontValidator, :budget}) || %{},
        :budget_id
      ) ==
        Map.get(preparation, :font_budget_id)
  end

  defp font_source_failure do
    Diagnostics.error(
      :font,
      :invalid_document,
      "CSS font candidate requires its live normal preparation source owner and policy context",
      module: __MODULE__,
      operation: :template_renderer_context
    )
  end

  @doc false
  @spec observe_cascade(map(), atom() | nil, [map()]) :: :ok | {:error, {atom(), map()}}
  def observe_cascade(node, pseudo, matched) do
    key = {__MODULE__, :preparation}

    case Process.get(key) do
      %{resource_frame: nil, resource_context: expected} = preparation ->
        original = Map.get(preparation.tree.nodes, Map.get(node, :node_id))

        if ResourceContext.current() == expected && original &&
             Map.take(original, @source_identity) == Map.take(node, @source_identity) do
          inline = Map.get(preparation.provenance.records, node.node_id, [])

          result =
            Enum.reduce_while(matched, {:ok, []}, fn matched, {:ok, records} ->
              HtmlValidator.reserve_render_resource(:max_css_work, 1, :css)

              record =
                if matched.inline,
                  do: Enum.at(inline, matched.declaration_index),
                  else: Map.get(matched, :source_record)

              if record && record.declaration != matched.declaration do
                {:halt,
                 Diagnostics.error(
                   :css,
                   :invalid_document,
                   "normal cascade declaration does not match its exact original source record",
                   module: __MODULE__,
                   operation: :template_renderer_context,
                   source: record.source.text,
                   line: record.source.line,
                   column: record.source.column
                 )}
              else
                declaration = %{
                  declaration: matched.declaration,
                  source: if(record, do: record.source, else: :unbound_author_source),
                  references: if(record, do: record.references, else: []),
                  cascade:
                    Map.take(matched, [
                      :important,
                      :inline,
                      :specificity,
                      :order,
                      :declaration_index
                    ]),
                  context: Map.take(original, @source_identity),
                  original_context:
                    if(preparation.source_projection,
                      do:
                        case Map.get(preparation.source_projection.placements, original.node_id) do
                          %{original: owner} -> owner
                          _ -> :unbound_synthetic_source
                        end,
                      else: Map.take(original, @source_identity)
                    ),
                  pseudo: pseudo,
                  policy: :normal_author_cascade_observed,
                  selection_policy: :requires_final_winning_declaration
                }

                {:cont, {:ok, [declaration | records]}}
              end
            end)

          case result do
            {:ok, records} ->
              Process.put(key, %{
                preparation
                | cascades:
                    Map.put(preparation.cascades, {node.node_id, pseudo}, Enum.reverse(records))
              })

              :ok

            error ->
              error
          end
        else
          :ok
        end

      _ ->
        :ok
    end
  end

  @doc false
  @spec source_owner(map(), keyword()) :: keyword()
  def source_owner(node, opts) do
    marker =
      case Process.get({__MODULE__, :preparation}) do
        %{tree: tree, resource_frame: nil} = preparation ->
          owner = Map.get(tree.nodes, Map.get(node, :node_id))

          if owner && Map.take(owner, @source_identity) == Map.take(node, @source_identity) do
            HtmlValidator.reserve_render_resource(:max_html_tree_work, 1, :style)
            marker = make_ref()

            Process.put({__MODULE__, :preparation}, %{
              preparation
              | owners: Map.put(preparation.owners, marker, Map.take(owner, @source_identity))
            })

            marker
          end

        _ ->
          nil
      end

    if marker,
      do: Keyword.put(opts, :__template_source_owner__, marker),
      else: Keyword.delete(opts, :__template_source_owner__)
  end

  @doc false
  @spec with_resource_observation(term(), atom(), keyword(), (-> term())) :: term()
  def with_resource_observation(reference, kind, opts, runner) do
    key = {__MODULE__, :preparation}

    case Process.get(key) do
      nil ->
        runner.()

      preparation ->
        previous = preparation.resource_frame
        marker = Keyword.get(opts, :__template_source_owner__)
        current = ResourceContext.current()

        owner =
          if previous == nil and current == preparation.resource_context and
               ResourceValidator.validate_context(current) == :ok,
             do: Map.get(preparation.owners, marker)

        frame = %{
          reference: reference,
          kind: kind,
          marker: marker,
          owner: owner,
          context: current,
          observed: false
        }

        Process.put(key, %{preparation | resource_frame: frame})

        try do
          runner.()
        after
          current_preparation = Process.get(key)
          Process.put(key, %{current_preparation | resource_frame: previous})
        end
    end
  end

  @doc false
  @spec observe_resource(term(), atom(), term(), atom(), term(), keyword()) :: :ok
  @spec observe_resource(term(), atom(), term(), atom(), term(), keyword(), term()) :: :ok
  def observe_resource(
        reference,
        kind,
        result,
        basis,
        descriptor,
        opts,
        authorization_context \\ nil
      ) do
    case {Process.get({__MODULE__, :preparation}), result} do
      {%{resource_context: expected, resource_frame: frame} = preparation, {:ok, bytes}} ->
        current = ResourceContext.current()

        if current == expected and is_map(frame) and not frame.observed and
             frame.reference == reference and frame.kind == kind and frame.context == current and
             frame.marker == Keyword.get(opts, :__template_source_owner__) and
             ResourceValidator.validate_context(current) == :ok do
          HtmlValidator.reserve_render_resource(:max_html_tree_work, 1, :asset)
          owner = frame.owner

          record = %{
            reference: reference,
            kind: kind,
            encoded_source: bytes,
            source_bytes: byte_size(bytes),
            descriptor: descriptor,
            authority_basis: basis,
            authorization_context: authorization_context || current,
            resource_context: current,
            font_candidate: Map.get(preparation, :font_candidate_frame),
            owner: if(owner, do: owner, else: :unbound_global_preparation),
            original_owner:
              if(owner && preparation.source_projection,
                do:
                  case Map.get(preparation.source_projection.placements, owner.node_id) do
                    %{original: owner} -> owner
                    _ -> :unbound_synthetic_source
                  end,
                else: if(owner, do: owner, else: :unbound_global_preparation)
              ),
            policy:
              if(preparation.source_projection,
                do: :loaded_in_prospective_preparation,
                else: :loaded_in_original_preparation
              ),
            authored_owner_policy:
              if(kind == :image and owner,
                do: :html_consuming_element_binding,
                else: :requires_authored_css_or_candidate_binding
              ),
            prospective_policy: :requires_actual_insertion_preparation,
            defining_declaration_candidates:
              if(kind == :background_image and owner,
                do:
                  preparation.cascades
                  |> Map.get({owner.node_id, nil}, [])
                  |> Enum.filter(fn declaration ->
                    HtmlValidator.reserve_render_resource(
                      :max_css_work,
                      1 + length(declaration.references),
                      :css
                    )

                    Enum.any?(declaration.references, &(&1.decoded == reference))
                  end),
                else: []
              ),
            defining_declaration_policy: :requires_final_winning_declaration
          }

          Process.put({__MODULE__, :preparation}, %{
            preparation
            | references: [record | preparation.references],
              resource_frame: %{frame | observed: true}
          })
        end

        :ok

      _ ->
        :ok
    end
  end

  @doc false
  @spec validate_prepared_glyph_runs([map()], HtmlTree.t()) ::
          {:ok, map()} | {:error, {atom(), map()}}
  @spec validate_prepared_glyph_runs([map()], HtmlTree.t(), map()) ::
          {:ok, map()} | {:error, {atom(), map()}}
  def validate_prepared_glyph_runs(nodes, tree, cascades \\ %{}) do
    HtmlValidator.with_render_budget(fn -> collect_glyph_runs(nodes, tree, cascades, %{}, 0) end)
  end

  defp declaration_chain(tree, id, cascades) do
    Stream.unfold(id, fn id ->
      HtmlValidator.reserve_render_resource(:max_html_tree_work, 1, :css)

      case Map.get(tree.nodes, id) do
        %{type: :element, parent_id: parent} = node ->
          next =
            case Map.get(tree.nodes, parent) do
              %{tree_scope_id: scope} when scope == node.tree_scope_id -> parent
              _ -> nil
            end

          {{id, Map.get(cascades, {id, nil}, [])}, next}

        _ ->
          nil
      end
    end)
    |> Enum.map(fn {id, records} ->
      HtmlValidator.reserve_render_resource(:max_css_work, length(records), :css)
      %{owner: Map.take(tree.nodes[id], @source_identity), declarations: records}
    end)
  end

  defp collect_glyph_runs(nodes, tree, cascades, records, depth) do
    HtmlValidator.check_render_resource(:max_html_depth, depth, :font)

    Enum.reduce_while(nodes, {:ok, records}, fn node, {:ok, records} ->
      HtmlValidator.reserve_render_resource(:max_html_tree_work, 1, :font)

      result =
        case node do
          %{type: :text} ->
            original = Map.get(tree.nodes, Map.get(node, :node_id))

            cond do
              original == nil ->
                {:ok, records}

              true ->
                with :ok <- validate_prepared_source(node, original, :font) do
                  run = %{
                    owner: Map.take(original, @source_identity),
                    original_owner: Map.take(original, @source_identity),
                    authored_text: original.text,
                    resolved_text: node.text,
                    font_face: node.style.font_face,
                    font_size: node.style.font_size,
                    text_transform: Map.get(node.style, :text_transform, :none),
                    declaration_contexts: declaration_chain(tree, original.parent_id, cascades),
                    policy: :normal_generic_text_fallback_observed
                  }

                  {:ok, Map.update(records, original.node_id, [run], &[run | &1])}
                end
            end

          %{type: :element, children: children} ->
            collect_glyph_runs(children, tree, cascades, records, depth + 1)
        end

      case result do
        {:ok, records} -> {:cont, {:ok, records}}
        error -> {:halt, error}
      end
    end)
    |> case do
      {:ok, records} when depth == 0 ->
        {:ok, Map.new(records, fn {id, runs} -> {id, Enum.reverse(runs)} end)}

      result ->
        result
    end
  end
end
