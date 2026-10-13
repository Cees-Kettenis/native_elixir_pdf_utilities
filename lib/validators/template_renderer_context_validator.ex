defmodule NativeElixirPdfUtilities.Validators.TemplateRendererContextValidator do
  @moduledoc false

  alias NativeElixirPdfUtilities.Diagnostics

  alias NativeElixirPdfUtilities.HtmlToPdf.{
    Font,
    FontFallback,
    HtmlTree,
    PageGeometry,
    ResourceContext,
    Style
  }

  alias NativeElixirPdfUtilities.Validators.{
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
    result =
      with :ok <- ResourceValidator.validate_context(resource_context),
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
           {:ok, sheets} <- Style.prepare_stylesheets(HtmlTree.fetch(tree, tree.root), opts) do
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
               {:ok, glyph_runs} <- validate_prepared_glyph_runs(resolved.children, tree),
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
               original: plan.original,
               insertion: plan.insertion,
               nodes: nodes,
               glyph_runs: glyph_runs,
               glyph_policy: :normal_generic_text_fallback_observed,
               pending_glyph_owners: [:generated_content, :control_text, :unvisited_authored_text],
               references: Process.get(key).references |> Enum.reverse(),
               prospective_policy: :requires_actual_insertion_preparation,
               original_parent:
                 Map.get(nodes, plan.original.parent, :not_prepared_by_normal_style),
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
  def observe_resource(reference, kind, result, basis, descriptor, opts) do
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
            resource_context: current,
            owner: if(owner, do: owner, else: :unbound_global_preparation),
            policy: :loaded_in_original_preparation,
            authored_owner_policy:
              if(kind == :image and owner,
                do: :html_consuming_element_binding,
                else: :requires_authored_css_or_candidate_binding
              ),
            prospective_policy: :requires_actual_insertion_preparation
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
  def validate_prepared_glyph_runs(nodes, tree) do
    HtmlValidator.with_render_budget(fn -> collect_glyph_runs(nodes, tree, %{}, 0) end)
  end

  defp collect_glyph_runs(nodes, tree, records, depth) do
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
                    authored_text: original.text,
                    resolved_text: node.text,
                    font_face: node.style.font_face,
                    font_size: node.style.font_size,
                    text_transform: Map.get(node.style, :text_transform, :none),
                    policy: :normal_generic_text_fallback_observed
                  }

                  {:ok, Map.update(records, original.node_id, [run], &[run | &1])}
                end
            end

          %{type: :element, children: children} ->
            collect_glyph_runs(children, tree, records, depth + 1)
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
