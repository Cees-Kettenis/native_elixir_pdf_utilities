defmodule NativeElixirPdfUtilities.Validators.CssSourceContextValidator do
  @moduledoc false

  alias NativeElixirPdfUtilities.Validators.{CssValueValidator, HtmlValidator}

  @doc """
  Collects completed declaration checks and outstanding original-context obligations.

  The internal runner receives prepared stylesheet entries, inline records and
  an observer for successful CssValueValidator checks. Every authored declaration
  keeps its exact source and source owner. The observer records the original
  node identity and pseudo-element without copying styles or granting authority.
  Context-free literals use canonical value validation with the same budget.
  They carry no element contexts or invented owners. The four-argument runner
  also observes actual selector matches before computation. Unapplied stylesheet
  values use canonical symbolic syntax/type and URL policy with no resource reads.
  Matching declarations awaiting computation remain unresolved. This grants no source-removal or resource authorization. Nested invocations have separate
  collectors and retain the enclosing invocation's resource reservations.
  """
  @spec with_context_obligations([map()], map(), function()) ::
          {:ok, term(), [map()]} | {:error, {atom(), map()}}
  @spec with_context_obligations([map()], map(), function(), :atomics.atomics_ref()) ::
          {:ok, term(), [map()]} | {:error, {atom(), map()}}
  def with_context_obligations(
        entries,
        provenance,
        runner,
        budget \\ HtmlValidator.new_css_budget()
      ) do
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
      selection_key = {key, :selection}
      Process.put(selection_key, MapSet.new())

      select = fn declaration, _node, _pseudo ->
        case declaration do
          %{context_id: id} ->
            HtmlValidator.reserve_render_resource(:max_css_work, 1, :css)
            Process.put(selection_key, MapSet.put(Process.get(selection_key), id))

          _ ->
            :ok
        end

        :ok
      end

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
        selected_runner? = is_function(runner, 4)

        execution =
          if selected_runner?,
            do: runner.(entries, Map.new(inline), observe, select),
            else: runner.(entries, Map.new(inline), observe)

        with {:ok, result} <- execution do
          checked = Process.get(key)
          selected = Process.get(selection_key)

          obligations =
            List.flatten(Enum.reverse(stylesheet_records) ++ Enum.reverse(inline_records))
            |> Enum.reduce_while({:ok, []}, fn record, {:ok, records} ->
              HtmlValidator.reserve_render_resource(:max_css_work, 1, :css)
              matches = Map.get(checked, record.context_id, %{})
              HtmlValidator.reserve_render_resource(:max_css_work, map_size(matches), :css)
              contexts = Map.values(matches)
              original = Map.drop(record, [:context_id, :reason])

              case contexts do
                [] ->
                  case CssValueValidator.validate_source_literal(record, budget) do
                    :ok ->
                      literal =
                        Map.merge(original, %{
                          status: :checked_without_element_context,
                          contexts: []
                        })

                      {:cont, {:ok, [literal | records]}}

                    :requires_element_context ->
                      value =
                        if selected_runner? and match?({:stylesheet, _}, record.origin) and
                             record.reason in [
                               :unmatched_original_context,
                               :inactive_media,
                               :inapplicable_stylesheet
                             ] and
                             not MapSet.member?(selected, record.context_id),
                           do: CssValueValidator.prepare_unapplied_declaration(record, budget),
                           else: :requires_element_context

                      case value do
                        {:ok, symbolic} ->
                          prepared =
                            Map.merge(original, %{
                              status: :checked_symbolically,
                              contexts: [],
                              source_value: symbolic,
                              selection: %{kind: :unapplied_stylesheet, basis: record.reason}
                            })

                          {:cont, {:ok, [prepared | records]}}

                        :requires_element_context ->
                          pending =
                            Map.merge(original, %{
                              status: :unresolved,
                              reason: record.reason,
                              contexts: []
                            })

                          {:cont, {:ok, [pending | records]}}

                        error ->
                          {:halt, error}
                      end

                    error ->
                      {:halt, error}
                  end

                _ ->
                  checked =
                    Map.merge(original, %{
                      status: :checked_in_original_context,
                      contexts: contexts
                    })

                  {:cont, {:ok, [checked | records]}}
              end
            end)

          with {:ok, records} <- obligations do
            {:ok, result, Enum.reverse(records)}
          end
        end
      after
        Process.delete(key)
        Process.delete(selection_key)
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
