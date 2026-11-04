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

  @doc "Applies validated resets, implicit list numbering, increments and sets."
  @spec apply(map(), t(), t(), term()) :: t()
  @spec apply(map(), t(), t(), term(), :element | :pseudo) :: t()
  def apply(style, counters, parent, creator, kind \\ :element) do
    if HtmlValidator.counter_box?(style, kind),
      do: apply_box(style, counters, parent, creator),
      else: counters
  end

  defp apply_box(style, counters, parent, creator) do
    counters =
      Enum.reduce(Map.get(style, :counter_reset, []), counters, fn {name, value}, state ->
        {value, creator} =
          case value do
            {:reversed, value} -> {value, {:reversed, creator}}
            {:list, value} -> {value, {:list, creator}}
            value -> {value, creator}
          end

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

    counters =
      if Map.get(style, :display) == :list_item and
           not List.keymember?(Map.get(style, :counter_reset, []), "list-item", 0) do
        case {Map.get(style, :_list_owner), Map.get(counters.scope, "list-item")} do
          {{:auto, owner}, {kind, previous}}
          when kind in [:list, :reversed] and previous != owner ->
            put(counters, "list-item", {:list, owner}, 0)

          {{_kind, owner}, nil} ->
            put(counters, "list-item", {:list, owner}, 0)

          _ ->
            counters
        end
      else
        counters
      end

    step =
      case Map.get(counters.scope, "list-item") do
        {:reversed, _owner} -> -1
        _ -> 1
      end

    counters =
      Enum.reduce(increments(style, step), counters, fn {name, amount}, state ->
        owner = Map.get(state.scope, name, creator)
        put(state, name, owner, value(state, name) + amount)
      end)

    Enum.reduce(Map.get(style, :counter_set, []), counters, fn {name, value}, state ->
      put(state, name, Map.get(state.scope, name, creator), value)
    end)
  end

  @doc false
  @spec reversed_initial_value(map(), t(), map(), :element | :pseudo) :: map()
  def reversed_initial_value(style, counters, calculation, kind) do
    if not calculation.done and HtmlValidator.counter_box?(style, kind) and
         Map.get(counters.scope, "list-item") == calculation.owner do
      negated =
        Enum.reduce(increments(style, -1), 0, fn {name, amount}, total ->
          if name == "list-item", do: total - amount, else: total
        end)

      last = if negated == 0, do: calculation.last, else: negated
      set = List.keyfind(Enum.reverse(Map.get(style, :counter_set, [])), "list-item", 0)

      case set do
        {"list-item", value} ->
          %{calculation | num: calculation.num + value, last: last, done: true}

        nil ->
          %{calculation | num: calculation.num + negated, last: last}
      end
    else
      calculation
    end
  end

  defp increments(style, step) do
    increments = Map.get(style, :counter_increment, [])

    if Map.get(style, :display) == :list_item and
         not List.keymember?(increments, "list-item", 0),
       do: [{"list-item", step} | increments],
       else: increments
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
