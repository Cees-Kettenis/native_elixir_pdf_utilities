defmodule NativeElixirPdfUtilities.Validators.CollectionValidator do
  @moduledoc false

  @doc "Counts a proper list up to its owning validator's configured ceiling, without enumeration or copying."
  @spec bounded_length(term(), non_neg_integer()) ::
          {:ok, non_neg_integer()} | :invalid | :limit_exceeded
  def bounded_length(values, maximum) do
    count(values, maximum, 0)
  end

  defp count(values, maximum, length) do
    case values do
      [] -> {:ok, length}
      [_value | rest] when length < maximum -> count(rest, maximum, length + 1)
      [_value | _rest] -> :limit_exceeded
      _ -> :invalid
    end
  end
end
