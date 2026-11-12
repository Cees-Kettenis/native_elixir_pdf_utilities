defmodule NativeElixirPdfUtilities.TestSupport.ResourceBudget do
  @moduledoc "Measures complete stage and operation costs for shared-budget boundary tests."
  alias NativeElixirPdfUtilities.{Limits, Operation}
  alias NativeElixirPdfUtilities.Validators.ResourceBudgetValidator

  @doc "Returns a stage's result and consumption without replacing an enclosing resource scope."
  @spec measure(Limits.key(), (-> result)) :: {result, non_neg_integer()} when result: term()
  def measure(key, operation) do
    ResourceBudgetValidator.with_budget(fn ->
      before = ResourceBudgetValidator.remaining(key)
      result = operation.()
      {result, before - ResourceBudgetValidator.remaining(key)}
    end)
  end

  @doc "Measures nested public operations in one disposable worker, including their reader and writer stages."
  @spec measure_operation(Limits.key(), (-> result)) ::
          {result, non_neg_integer()} | {:error, {atom(), map()}}
        when result: term()
  def measure_operation(key, operation) do
    Operation.run(__MODULE__, :measure_resource_budget, fn -> measure(key, operation) end)
  end
end
