defmodule NativeElixirPdfUtilities.Validators.CssCalculationValidator do
  @moduledoc "MathML compatibility interface to the shared CSS calculation validator."
  alias NativeElixirPdfUtilities.Diagnostics
  alias NativeElixirPdfUtilities.Validators.CssMathValidator

  @typedoc "A shared validated length-percentage calculation."
  @type calculation :: CssMathValidator.length_calculation()

  @doc "Prepares length-percentage arithmetic through the shared CSS grammar."
  @spec prepare(term(), term()) ::
          {:ok, calculation()} | :error | {:error, Diagnostics.detailed_error()}
  def prepare(value, scales), do: CssMathValidator.prepare_length(value, scales)

  @doc "Returns measured unit dependencies after the shared grammar validates their type."
  @spec referenced_units(term()) :: {:ok, [String.t()]} | {:error, Diagnostics.detailed_error()}
  def referenced_units(value) do
    case CssMathValidator.referenced_length_units(value) do
      :error -> {:ok, []}
      result -> result
    end
  end

  @doc "Resolves the shared calculation against its actual percentage basis in points."
  @spec resolve(term(), term()) :: {:ok, number()} | {:error, Diagnostics.detailed_error()}
  def resolve(calculation, basis), do: CssMathValidator.resolve_length(calculation, basis)
end
