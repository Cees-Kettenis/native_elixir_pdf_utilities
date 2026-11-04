defmodule NativeElixirPdfUtilities.DiagnosticsTest do
  use ExUnit.Case

  alias NativeElixirPdfUtilities.Diagnostics

  test "builds minimal and contextual diagnostics" do
    assert Diagnostics.diagnostic(:parse, :invalid_input, "bad input") == %{
             stage: :parse,
             reason: :invalid_input,
             message: "bad input"
           }

    assert Diagnostics.diagnostic(:css, :invalid_css, "bad css",
             operation: :render,
             module: __MODULE__,
             line: 2,
             column: 4,
             source: "p >"
           ) == %{
             stage: :css,
             reason: :invalid_css,
             message: "bad css",
             operation: :render,
             module: __MODULE__,
             line: 2,
             column: 4,
             source: "p >"
           }
  end

  test "adds missing context without replacing existing context" do
    diagnostic = %{stage: :file, reason: :enoent, message: "missing", operation: :read}

    assert Diagnostics.with_context(diagnostic,
             operation: :extract_file,
             module: __MODULE__,
             source: "/tmp/missing.pdf"
           ) == %{
             stage: :file,
             reason: :enoent,
             message: "missing",
             operation: :read,
             module: __MODULE__,
             source: "/tmp/missing.pdf"
           }

    assert Diagnostics.with_context(diagnostic) == diagnostic
  end

  test "normalizes invalid diagnostic context" do
    assert Diagnostics.with_context(:not_a_diagnostic, operation: :render) == %{
             stage: :unknown,
             reason: :unknown_error,
             message: "operation failed",
             operation: :render
           }
  end
end
