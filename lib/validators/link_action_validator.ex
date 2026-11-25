defmodule NativeElixirPdfUtilities.Validators.LinkActionValidator do
  @moduledoc false

  alias NativeElixirPdfUtilities.{Diagnostics, Limits}
  alias NativeElixirPdfUtilities.Validators.{MergeValidator, OutlineValidator, PdfValidator}

  @doc "Resolves local link names and relative URIs against the catalog of each merge input."
  @spec normalize(PdfValidator.document(), map(), [MergeValidator.object_context()]) ::
          {:ok, [MergeValidator.object_context()]} | {:error, {atom(), map()}}
  def normalize(document, catalog, objects) do
    with {:ok, base} <- catalog_uri_base(document, catalog) do
      state = %{catalog: catalog, names: nil, base: base, actions: %{}}

      {objects, state} =
        Enum.map_reduce(objects, state, fn object, state ->
          {value, state} = normalize_links(document, object.value, state)
          {override(object, value), state}
        end)

      {:ok,
       Enum.map(objects, fn object ->
         case Map.fetch(state.actions, {object.obj, object.gen}) do
           {:ok, value} -> override(object, value)
           :error -> object
         end
       end)}
    end
  catch
    {:link_error, error} -> error
  end

  defp override(object, value) do
    if value == object.value, do: object, else: Map.put(object, :value_override, value)
  end

  defp normalize_links(document, value, state) do
    case value do
      dictionary when is_map(dictionary) ->
        if link?(document, dictionary) do
          rewrite_link(document, dictionary, state)
        else
          Enum.reduce(dictionary, {dictionary, state}, fn {key, original}, {result, state} ->
            {value, state} = normalize_links(document, original, state)
            {if(value === original, do: result, else: Map.put(result, key, value)), state}
          end)
        end

      values when is_list(values) ->
        Enum.map_reduce(values, state, &normalize_links(document, &1, &2))

      _ ->
        {value, state}
    end
  end

  @doc "Filters one page's annotations and shares rewritten indirect actions across the selection."
  @spec select(PdfValidator.document(), PdfValidator.value(), map()) ::
          {:ok, PdfValidator.value(), map()} | {:error, {atom(), map()}}
  def select(document, annotations, state) do
    case annotations do
      nil ->
        {:ok, nil, state}

      annotations when is_list(annotations) ->
        {kept, state} =
          Enum.reduce(annotations, {[], state}, fn annotation, {kept, state} ->
            case select_annotation(document, annotation, state) do
              {:omit, state} -> {kept, state}
              {annotation, state} -> {[annotation | kept], state}
            end
          end)

        {:ok, if(kept == [], do: nil, else: Enum.reverse(kept)), state}

      _ ->
        annotation_error("page Annots entry must resolve to an array")
    end
  catch
    {:link_error, error} -> error
  end

  defp select_annotation(document, annotation, state) do
    case PdfValidator.resolve(document, annotation) do
      {:ok, dictionary} when is_map(dictionary) ->
        if link?(document, dictionary) do
          {link, state} = rewrite_link(document, dictionary, state)

          case {annotation, link} do
            {_, :omit} ->
              {:omit, state}

            {{:ref, ref}, link} when link != dictionary ->
              {annotation, put_in(state.annotations[ref], link)}

            {{:ref, _}, _} ->
              {annotation, state}

            _ ->
              {link, state}
          end
        else
          {annotation, state}
        end

      {:error, {:resource_limit_exceeded, _}} = error ->
        throw({:link_error, error})

      _ ->
        annotation_error("page annotation must resolve to a dictionary")
    end
  end

  defp link?(document, dictionary) do
    case PdfValidator.resolved_name?(document, dictionary["Subtype"], "Link") do
      {:error, _} = error -> throw({:link_error, error})
      result -> result
    end
  end

  defp rewrite_link(document, link, state) do
    case PdfValidator.link_destination(document, link) do
      {:ok, {:destination, target}} ->
        rewrite_target(document, link, "Dest", target, state)

      {:error, _} = error ->
        throw({:link_error, error})

      _ ->
        case Map.fetch(link, "A") do
          {:ok, value} ->
            {action, state} = rewrite_action(document, value, state, 0)
            {if(action == :omit, do: :omit, else: Map.put(link, "A", action)), state}

          :error ->
            {link, state}
        end
    end
  end

  # Only /A and /Next are action edges. Record each indirect object before
  # visiting its children so shared actions and cycles keep their identity.
  defp rewrite_action(document, value, state, depth) do
    if depth > Limits.get(:max_depth) do
      throw(
        {:link_error,
         Diagnostics.error(
           :limits,
           :resource_limit_exceeded,
           "link action chain exceeds max_depth",
           operation: :merge,
           module: __MODULE__
         )}
      )
    end

    case value do
      {:ref, ref} ->
        case Map.fetch(state.actions, ref) do
          {:ok, :omit} ->
            {:omit, state}

          {:ok, _} ->
            {value, state}

          :error ->
            original = link_value!(PdfValidator.resolve(document, value))
            state = put_in(state.actions[ref], original)
            {action, state} = rewrite_action(document, original, state, depth + 1)
            {if(action == :omit, do: :omit, else: value), put_in(state.actions[ref], action)}
        end

      actions when is_list(actions) ->
        {rewritten, state} =
          Enum.map_reduce(actions, state, &rewrite_action(document, &1, &2, depth + 1))

        kept = Enum.reject(rewritten, &(&1 == :omit))
        {if(kept == [] and actions != [], do: :omit, else: kept), state}

      action when is_map(action) ->
        base = Map.get(state, :base)

        {action, state} =
          case link_value!(PdfValidator.resolve(document, action["S"])) do
            {:name, "GoTo"} ->
              target = link_value!(PdfValidator.resolve(document, action["D"]))
              rewrite_target(document, action, "D", target, state)

            {:name, "URI"} when not is_nil(base) ->
              {rebase_uri(document, base, action), state}

            _ ->
              {action, state}
          end

        case action do
          %{"Next" => next} ->
            {next, state} = rewrite_action(document, next, state, depth + 1)

            {if(next == :omit,
               do: Map.delete(action, "Next"),
               else: Map.put(action, "Next", next)
             ), state}

          _ ->
            {action, state}
        end

      _ ->
        {value, state}
    end
  end

  defp rewrite_target(document, dictionary, key, target, state) do
    case state do
      %{selected: selected, pages: pages} ->
        case target do
          [{:ref, ref} | _] ->
            cond do
              Map.has_key?(selected, ref) -> {dictionary, state}
              Map.has_key?(pages, ref) -> {:omit, state}
              true -> annotation_error("link destination references an unknown page")
            end

          {kind, _} when kind in [:name, :string, :hex] ->
            {:omit, state}

          _ ->
            annotation_error("link destination is malformed")
        end

      %{catalog: catalog, names: names} ->
        case target do
          {kind, _} when kind in [:name, :string, :hex] ->
            names =
              names ||
                link_value!(
                  named_destination_result(OutlineValidator.named_destinations(document, catalog))
                )

            resolved =
              link_value!(
                named_destination_result(
                  OutlineValidator.resolve_named_destination(document, target, names)
                )
              )

            {if(resolved, do: Map.put(dictionary, key, resolved), else: dictionary),
             %{state | names: names}}

          _ ->
            {dictionary, state}
        end
    end
  end

  defp annotation_error(message) do
    throw(
      {:link_error,
       Diagnostics.error(:annotations, :invalid_pdf_input, message,
         operation: :assemble,
         module: __MODULE__
       )}
    )
  end

  defp catalog_uri_base(document, catalog) do
    with {:ok, uri} <- PdfValidator.resolve(document, catalog["URI"]),
         true <- is_map(uri),
         {:ok, {kind, bytes}} when kind in [:string, :hex] <-
           PdfValidator.resolve(document, uri["Base"]),
         {:ok, %URI{scheme: scheme} = base} when is_binary(scheme) <- URI.new(bytes) do
      {:ok, base}
    else
      {:error, {_reason, _diagnostic}} = error -> error
      _ -> {:ok, nil}
    end
  end

  # Name-tree failures belong to annotation import. Preserve resolution errors
  # from the link dictionary itself, including cycles in explicit destinations.
  defp named_destination_result(result) do
    case result do
      {:error, {reason, diagnostic}} ->
        stage = if reason == :resource_limit_exceeded, do: :limits, else: :annotations

        {:error,
         {reason, Map.merge(diagnostic, %{stage: stage, operation: :merge, module: __MODULE__})}}

      success ->
        success
    end
  end

  defp rebase_uri(document, base, action) do
    with {:ok, {kind, bytes}} when kind in [:string, :hex] <-
           PdfValidator.resolve(document, action["URI"]),
         {:ok, %URI{scheme: nil} = relative} <- URI.new(bytes) do
      Map.put(action, "URI", {kind, base |> URI.merge(relative) |> URI.to_string()})
    else
      {:error, {_, _}} = error -> throw({:link_error, error})
      _ -> action
    end
  end

  defp link_value!(result) do
    case result do
      {:ok, value} -> value
      {:error, _} = error -> throw({:link_error, error})
    end
  end
end
