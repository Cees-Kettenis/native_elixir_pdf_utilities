defmodule NativeElixirPdfUtilities.Validators.HtmlPatternValidator do
  @moduledoc """
  Evaluates static HTML patterns with ECMAScript Unicode-v and UTF-16 semantics.

  Invalid authored patterns are ignored, as required by HTML. Valid patterns
  match the entire initial value. Compilation, recursion, BEAM memory,
  character scans and bounded OTP matching consume configured Limits. Exhaustion is a
  diagnostic, never an invented mismatch. No general script engine is involved.
  """
  alias NativeElixirPdfUtilities.Diagnostics
  alias NativeElixirPdfUtilities.HtmlToPdf.FormValueBackend
  alias NativeElixirPdfUtilities.Validators.HtmlFormValueValidator

  @doc "Compiles the authored Unicode-v pattern and tests every supplied sanitized initial value."
  @spec initial_pattern(term(), term()) ::
          {:ok, %{compiled?: boolean(), mismatch: boolean()}}
          | {:error, {atom(), Diagnostics.diagnostic()}}
  def initial_pattern(pattern, values) do
    with {:ok, request} <- HtmlFormValueValidator.prepare(:pattern, pattern, values),
         {:ok, state} <- HtmlFormValueValidator.result(FormValueBackend.run(request)) do
      {:ok, %{compiled?: state != :invalid_pattern, mismatch: state == :mismatched}}
    end
  end
end
