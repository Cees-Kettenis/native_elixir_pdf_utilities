defmodule NativeElixirPdfUtilities.Validators.ResourceBudgetValidator do
  @moduledoc false
  alias NativeElixirPdfUtilities.{Diagnostics, Limits}
  @scope {__MODULE__, :scope}

  @doc "Shares resource consumption across nested stages and loops in one operation or standalone preparation."
  @spec with_budget((-> result)) :: result when result: term()
  def with_budget(operation) do
    case Process.get(@scope) do
      nil ->
        scope = make_ref()
        Process.put(@scope, scope)

        try do
          operation.()
        after
          Enum.each(Process.delete(scope) || [], &Process.delete({scope, &1}))
          Process.delete(@scope)
        end

      _ ->
        operation.()
    end
  end

  @doc "Identifies the current allowance for render-local admission caches."
  @spec scope() :: reference() | nil
  def scope, do: Process.get(@scope)

  @doc "Reserves admitted resource units against the operation's shared allowance."
  @spec reserve(Limits.key(), non_neg_integer(), atom(), keyword()) ::
          :ok | {:error, {atom(), Diagnostics.diagnostic()}}
  def reserve(limit, amount, stage, context \\ []) do
    reserve_counter(limit, limit, amount, stage, context)
  end

  @doc "Reserves a distinct subtotal against an existing ceiling without consuming another resource."
  @spec reserve_counter(atom(), Limits.key(), non_neg_integer(), atom(), keyword()) ::
          :ok | {:error, {atom(), Diagnostics.diagnostic()}}
  def reserve_counter(counter_name, limit, amount, stage, context) do
    scope =
      case :erlang.get(@scope) do
        :undefined -> nil
        scope -> scope
      end

    counter = {scope, counter_name}
    previous = if scope != nil, do: :erlang.get(counter), else: :undefined
    total = if previous == :undefined, do: amount, else: previous + amount
    maximum = Limits.get(limit)

    if total <= maximum do
      if scope != nil and amount != 0 do
        if previous == :undefined, do: Process.put(scope, [counter_name | Process.get(scope, [])])
        :erlang.put(counter, total)
      end

      :ok
    else
      Diagnostics.error(
        stage,
        :resource_limit_exceeded,
        "operation exceeds #{limit} (configured limit #{maximum})",
        context
      )
    end
  end

  @doc "Returns the remaining shared allowance before a bounded allocation or read."
  @spec remaining(Limits.key()) :: non_neg_integer()
  def remaining(limit) do
    scope = Process.get(@scope)
    max(Limits.get(limit) - Process.get({scope, limit}, 0), 0)
  end

  @doc "Returns the operation's cumulative reservations for validating stage receipts."
  @spec snapshot() :: map() | nil
  def snapshot do
    case Process.get(@scope) do
      nil ->
        nil

      scope ->
        Enum.reduce(Process.get(scope, []), %{}, fn limit, counters ->
          Map.put(counters, limit, Process.get({scope, limit}))
        end)
    end
  end

  @doc "Carries an active allowance through a synchronous bounded worker and restores its consumed totals to the waiting caller."
  @spec with_worker(((-> term()) -> term()), (-> term())) :: term()
  def with_worker(dispatch, fun) do
    case snapshot() do
      nil ->
        dispatch.(fun)

      before ->
        receipt = make_ref()

        case dispatch.(fn ->
               restore_counters(before)
               result = fun.()
               {receipt, result, snapshot()}
             end) do
          {^receipt, result, totals} ->
            restore_counters(totals)
            result

          failure ->
            failure
        end
    end
  end

  defp restore_counters(totals) do
    scope = Process.get(@scope)
    Process.put(scope, Map.keys(totals))
    Enum.each(totals, fn {limit, used} -> Process.put({scope, limit}, used) end)
  end

  @doc "Identifies an active allowance when an explicitly passed standalone budget needs to join it."
  @spec active?() :: boolean()
  def active?, do: Process.get(@scope) != nil
end
