defmodule NativeElixirPdfUtilities.Validators.LimitsValidator do
  @moduledoc false

  alias NativeElixirPdfUtilities.Limits

  @max_configured_integer 9_223_372_036_854_775_807

  @doc false
  @spec validate(term()) :: {:ok, Limits.t()} | {:error, String.t()}
  def validate(config) do
    case Keyword.keyword?(config) do
      true ->
        keys = Keyword.keys(config)
        known_keys = Limits.keys()
        unknown_keys = Enum.reject(keys, &(&1 in known_keys))

        cond do
          unknown_keys != [] ->
            {:error, "unknown resource limit keys: #{inspect(Enum.uniq(unknown_keys))}"}

          length(keys) != length(Enum.uniq(keys)) ->
            {:error, "resource limit keys must not be repeated"}

          true ->
            limits = Map.merge(Limits.defaults(), Map.new(config))

            case Enum.find(limits, fn {_key, value} ->
                   not is_integer(value) or value <= 0 or value > @max_configured_integer
                 end) do
              {key, value} ->
                {:error,
                 "resource limit #{inspect(key)} must be a positive integer no greater than #{@max_configured_integer}; got: #{inspect(value)}"}

              nil ->
                # BEAM receive timeouts have a fixed unsigned 32-bit millisecond range.
                # This is a runtime invariant, independent of configurable resource budgets.
                timeout =
                  Enum.find(
                    [:max_operation_timeout_ms, :operation_memory_check_interval_ms],
                    &(Map.fetch!(limits, &1) > 4_294_967_295)
                  )

                if timeout,
                  do:
                    {:error,
                     "resource limit #{inspect(timeout)} exceeds the BEAM receive timeout range"},
                  else: {:ok, limits}
            end
        end

      false ->
        {:error, "resource limits must be configured as a keyword list"}
    end
  end
end
