defmodule NativeElixirPdfUtilities.HtmlToPdf.CounterState do
  @moduledoc false

  alias NativeElixirPdfUtilities.Validators.HtmlValidator

  # Scopes select the visible instance by name. Values remain shared across
  # descendants so restoring an outer scope retains increments made inside it.
  @empty %{scope: %{}, values: %{}, created: %{}}
  @type t :: %{scope: map(), values: map(), created: map()}

  @doc "Returns an empty counter scope."
  @spec new() :: t()
  def new, do: @empty

  @doc "Inherits parent names, missing sibling names, and the latest instance values."
  @spec inherit(t(), t() | nil, t()) :: t()
  def inherit(parent, sibling, latest) do
    if latest.values == %{} do
      parent
    else
      scope =
        if sibling == nil or sibling.scope == parent.scope do
          parent.scope
        else
          HtmlValidator.reserve_render_resource(
            :max_css_work,
            map_size(sibling.scope) + map_size(parent.scope),
            :style
          )

          Map.merge(sibling.scope, parent.scope)
        end

      scope =
        case Map.fetch(latest.scope, :_quote_depth) do
          {:ok, creator} -> Map.put_new(scope, :_quote_depth, creator)
          :error -> scope
        end

      if scope == latest.scope do
        latest
      else
        # Keep values for visible instances and enclosing scopes only. A child
        # scope that has ended must not retain every reset it ever performed.
        HtmlValidator.reserve_render_resource(
          :max_css_work,
          map_size(parent.values) + if(sibling == nil, do: 0, else: map_size(sibling.values)) +
            map_size(scope),
          :style
        )

        keys =
          Map.keys(parent.values) ++ if(sibling == nil, do: [], else: Map.keys(sibling.values))

        keys = Enum.reduce(scope, keys, fn {name, creator}, keys -> [{name, creator} | keys] end)
        values = Map.take(latest.values, keys)

        created =
          Enum.reduce(values, %{}, fn {{name, creator}, _value}, acc ->
            Map.update(acc, creator, %{name => creator}, &Map.put(&1, name, creator))
          end)

        %{scope: scope, values: values, created: created}
      end
    end
  end

  @doc "Applies validated reset and increment declarations in their authored order."
  @spec apply(map(), t(), t(), term()) :: t()
  def apply(style, counters, parent, creator) do
    counters =
      Enum.reduce(Map.get(style, :counter_reset, []), counters, fn {name, value}, state ->
        old = Map.get(state.scope, name)

        state =
          if old != nil and old != creator and not Map.has_key?(parent.values, {name, old}) do
            names = state.created |> Map.fetch!(old) |> Map.delete(name)

            created =
              if names == %{},
                do: Map.delete(state.created, old),
                else: Map.put(state.created, old, names)

            %{state | values: Map.delete(state.values, {name, old}), created: created}
          else
            state
          end

        put(state, name, creator, value)
      end)

    Enum.reduce(Map.get(style, :counter_increment, []), counters, fn {name, amount}, state ->
      owner = Map.get(state.scope, name, creator)
      put(state, name, owner, value(state, name) + amount)
    end)
  end

  @doc "Includes instances created by a pseudo-element without exporting child scopes."
  @spec include_created(t(), t(), term()) :: t()
  def include_created(counters, latest, creator) do
    additions = Map.get(latest.created, creator, %{})

    if additions == %{} do
      counters
    else
      HtmlValidator.reserve_render_resource(:max_css_work, map_size(additions), :style)
      %{counters | scope: Map.merge(counters.scope, additions)}
    end
  end

  @doc "Returns the visible instance value, or zero for an absent name."
  @spec value(t(), term()) :: integer()
  def value(counters, name) do
    Map.get(counters.values, {name, Map.get(counters.scope, name)}, 0)
  end

  @doc "Sets the document-wide quotation depth."
  @spec quote_depth(t(), non_neg_integer()) :: t()
  def quote_depth(counters, depth), do: put(counters, :_quote_depth, :document, depth)

  defp put(counters, name, creator, value) do
    HtmlValidator.reserve_render_resource(:max_css_work, 1, :style)

    %{
      scope: Map.put(counters.scope, name, creator),
      values: Map.put(counters.values, {name, creator}, value),
      created:
        Map.update(counters.created, creator, %{name => creator}, &Map.put(&1, name, creator))
    }
  end
end
