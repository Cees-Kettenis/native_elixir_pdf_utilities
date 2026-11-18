defmodule NativeElixirPdfUtilities.HtmlToPdf.ContainerQuery do
  @moduledoc false

  alias NativeElixirPdfUtilities.Diagnostics
  alias NativeElixirPdfUtilities.HtmlToPdf.RenderCache

  alias NativeElixirPdfUtilities.Validators.{
    CssContainerQueryValidator,
    CssLogicalValidator,
    HtmlValidator
  }

  @doc "Styles against measured content boxes until the selected query geometry is stable."
  @spec settle((reference(), map() -> term()), (map() -> term())) :: term()
  def settle(style, layout) do
    scope = make_ref()

    try do
      settle(style, layout, scope, %{}, %{})
    after
      Process.delete(scope)
      Process.delete({scope, :zoom})
    end
  end

  defp settle(style, layout, scope, geometry, visited) do
    if Map.has_key?(visited, geometry) do
      Diagnostics.error(:style, :invalid_document, "CSS container queries did not settle",
        module: __MODULE__,
        operation: :compute_container_queries,
        source: "@container"
      )
    else
      HtmlValidator.check_render_resource(:max_depth, map_size(visited), :style)
      Process.put(scope, :unused)
      Process.delete({scope, :zoom})

      with {:ok, tree} <- style.(scope, geometry) do
        tree =
          if Process.get({scope, :zoom}),
            do: NativeElixirPdfUtilities.HtmlToPdf.CssZoom.project(tree, tree.document_zoom),
            else: tree

        if Process.put(scope, %{}) == :required do
          with {:ok, _layout} <- layout.(tree) do
            measured = Process.get(scope)

            if measured == geometry do
              {:ok, tree}
            else
              settle(style, layout, scope, measured, Map.put(visited, geometry, true))
            end
          end
        else
          {:ok, tree}
        end
      end
    end
  end

  @doc "Registers a principal size-query box in its own document's layout pass."
  @spec register(map(), map(), term()) :: map()
  def register(style, context, id) do
    case {style, context} do
      {%{container_type: type}, %{query_scope: scope}} when type in [:size, :inline_size] ->
        if CssContainerQueryValidator.eligible_axis?(style, :inline_size) do
          Process.put(scope, :required)
          Map.put(style, :_query_container, {scope, id})
        else
          style
        end

      _ ->
        style
    end
  end

  @doc "Stores the final used content-box size before paint clips or transforms."
  @spec measure(map(), number(), number()) :: :ok
  def measure(style, width, height) do
    case style do
      %{_query_container: {scope, id}} when is_reference(scope) ->
        store_measurement(scope, id, width, height)

      _ ->
        :ok
    end
  end

  defp store_measurement(scope, id, width, height) do
    case Process.get(scope) do
      geometry when is_map(geometry) ->
        Process.put(
          scope,
          Map.put(geometry, id, %{width: max(width, 0.0), height: max(height, 0.0)})
        )

        :ok

      _ ->
        :ok
    end
  end

  @doc "Filters conditional selectors in their existing cascade order."
  @spec matches?([list(map())], map(), [map()]) :: boolean()
  def matches?(conditions, context, ancestors) do
    styles = ancestor_styles(ancestors, context)
    sizes = Map.get(context, :query_sizes, %{})
    viewport = viewport(context)

    Enum.all?(conditions, fn alternatives ->
      Enum.any?(alternatives, fn query ->
        case Enum.find(styles, &CssContainerQueryValidator.eligible?(query, &1)) do
          nil ->
            false

          style ->
            CssContainerQueryValidator.evaluate(
              query.expression,
              style,
              geometry(style, sizes),
              viewport
            ) == true
        end
      end)
    end)
  end

  @doc "Captures a separate nearest eligible axis for each container-relative unit."
  @spec unit_context(map(), map(), [map()]) :: map()
  def unit_context(style, context, ancestors) do
    case context do
      %{query_sizes: sizes} when map_size(sizes) > 0 ->
        styles = ancestor_styles(ancestors, context)

        units =
          Enum.reduce(
            [{"cqw", :width}, {"cqh", :height}, {"cqi", :inline_size}, {"cqb", :block_size}],
            %{},
            fn {unit, axis}, units ->
              candidate = Enum.find(styles, &CssContainerQueryValidator.eligible_axis?(&1, axis))

              case candidate && geometry(candidate, sizes) do
                nil ->
                  units

                geometry ->
                  dimension =
                    Map.get_lazy(geometry, axis, fn ->
                      Map.fetch!(geometry, Map.fetch!(CssLogicalValidator.axes(candidate), axis))
                    end)

                  Map.put(units, unit, dimension / 75)
              end
            end
          )

        if units == %{}, do: style, else: Map.put(style, :_query_units, units)

      _ ->
        style
    end
  end

  defp ancestor_styles(ancestors, context) do
    Enum.flat_map(ancestors, fn ancestor ->
      case RenderCache.lookup(context.cache, :cascade_styles, ancestor._selector_id) do
        {:ok, {:ok, style}} -> [style]
        :error -> []
      end
    end)
  end

  defp geometry(style, sizes) do
    case style do
      %{_query_container: {_scope, id}} -> Map.get(sizes, id)
      _ -> nil
    end
  end

  defp viewport(context) do
    case Map.get(context, :position_viewport) do
      viewport when is_map(viewport) -> viewport
      _ -> %{width: 0.0, height: 0.0}
    end
  end
end
