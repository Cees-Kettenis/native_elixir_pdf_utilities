defmodule NativeElixirPdfUtilities.Validators.CssSourceContextValidator do
  @moduledoc false

  alias NativeElixirPdfUtilities.Validators.HtmlValidator

  @doc """
  Collects completed declaration checks and outstanding original-context obligations.

  The internal runner receives prepared stylesheet entries, inline records and
  an observer for successful CssValueValidator checks. Every authored declaration
  keeps its exact source and source owner. The observer records the original
  node identity and pseudo-element without copying styles or granting authority.
  Missing contexts remain unresolved; this is neither a removed-source
  certificate nor resource authorization. Nested invocations have separate
  collectors and retain the enclosing invocation's resource reservations.
  """
  @spec with_context_obligations([map()], map(), function()) ::
          {:ok, term(), [map()]} | {:error, {atom(), map()}}
  def with_context_obligations(entries, provenance, runner) do
    HtmlValidator.with_render_budget(fn ->
      {entries, stylesheet_records} =
        entries
        |> Enum.with_index()
        |> Enum.map_reduce([], fn {entry, index}, collected ->
          {authored, records} =
            Enum.map_reduce(entry.prepared.authored_rules, [], fn rule, records ->
              HtmlValidator.reserve_render_resource(:max_css_work, 1, :css)

              reason =
                cond do
                  entry[:insertion_context] -> entry.insertion_context
                  not entry.prepared.stylesheet_applicable -> :inapplicable_stylesheet
                  not rule.active -> :inactive_media
                  true -> :unmatched_original_context
                end

              {declarations, obligations} =
                prepare_records(rule.source_declarations, {:stylesheet, index}, reason)

              {%{rule | source_declarations: declarations}, [obligations | records]}
            end)

          by_source = Map.new(authored, &{&1.source, &1.source_declarations})

          rules =
            Enum.map(entry.prepared.rules, fn rule ->
              HtmlValidator.reserve_render_resource(:max_css_work, 1, :css)
              %{rule | source_declarations: Map.fetch!(by_source, rule.source)}
            end)

          prepared = %{entry.prepared | authored_rules: authored, rules: rules}
          {%{entry | prepared: prepared}, [Enum.reverse(records) | collected]}
        end)

      {inline, inline_records} =
        provenance.records
        |> Enum.sort_by(fn {_id, records} ->
          case records do
            [record | _] -> record.source.offset
            [] -> 0
          end
        end)
        |> Enum.map_reduce([], fn {id, records}, collected ->
          reason = provenance.insertion_contexts[id] || :unvisited_original_context
          {records, obligations} = prepare_records(records, {:inline, id}, reason)
          {{id, records}, [obligations | collected]}
        end)

      key = {__MODULE__, make_ref()}
      Process.put(key, %{})

      observe = fn declaration, node, pseudo ->
        case declaration do
          %{context_id: id} ->
            HtmlValidator.reserve_render_resource(:max_css_work, 1, :css)
            owner = Map.take(node, [:node_id, :document_id, :tree_scope_id, :parent_id])

            context = %{
              owner: owner,
              source: Map.get(node, :source),
              tag: node.tag,
              pseudo: pseudo
            }

            identity = {Map.get(node, :node_id, Map.get(node, :_selector_id)), pseudo}
            contexts = Process.get(key)
            prior = Map.get(contexts, id, %{})
            Process.put(key, Map.put(contexts, id, Map.put(prior, identity, context)))
            :ok

          _ ->
            :ok
        end
      end

      try do
        with {:ok, result} <- runner.(entries, Map.new(inline), observe) do
          checked = Process.get(key)

          obligations =
            Enum.map(
              List.flatten(Enum.reverse(stylesheet_records) ++ Enum.reverse(inline_records)),
              fn record ->
                HtmlValidator.reserve_render_resource(:max_css_work, 1, :css)
                matches = Map.get(checked, record.context_id, %{})
                HtmlValidator.reserve_render_resource(:max_css_work, map_size(matches), :css)
                contexts = Map.values(matches)

                context =
                  case contexts do
                    [] -> %{status: :unresolved, reason: record.reason, contexts: []}
                    _ -> %{status: :checked_in_original_context, contexts: contexts}
                  end

                record |> Map.drop([:context_id, :reason]) |> Map.merge(context)
              end
            )

          {:ok, result, obligations}
        end
      after
        Process.delete(key)
      end
    end)
  end

  defp prepare_records(records, origin, reason) do
    Enum.map_reduce(records, [], fn record, obligations ->
      HtmlValidator.reserve_render_resource(:max_css_work, 1, :css)
      id = make_ref()
      prepared = Map.put(record, :context_id, id)

      obligation = %{
        context_id: id,
        origin: origin,
        declaration: record.declaration,
        source: record.source,
        reason: reason
      }

      {prepared, [obligation | obligations]}
    end)
    |> then(fn {records, obligations} -> {records, Enum.reverse(obligations)} end)
  end
end
