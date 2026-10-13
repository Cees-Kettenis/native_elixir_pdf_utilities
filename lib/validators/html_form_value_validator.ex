defmodule NativeElixirPdfUtilities.Validators.HtmlFormValueValidator do
  @moduledoc false
  alias NativeElixirPdfUtilities.{Diagnostics, Limits}
  alias NativeElixirPdfUtilities.Validators.HtmlValidator

  @doc "Prepares bounded UTF-8 input for the byte-only native initial form-value parsers."
  @spec prepare(term(), term(), term()) ::
          {:ok, map()} | {:error, {atom(), Diagnostics.diagnostic()}}
  def prepare(operation, source, values) do
    cond do
      operation not in [:pattern, :url] or not is_binary(source) or not String.valid?(source) or
        not is_list(values) or not Enum.all?(values, &(is_binary(&1) and String.valid?(&1))) ->
        failure(
          :invalid_document,
          "initial form values require valid UTF-8 strings and a list of values"
        )

      true ->
        HtmlValidator.reserve_render_resource(:max_html_pattern_values, length(values), :html)

        source_limit =
          if operation == :pattern, do: :max_html_pattern_bytes, else: :max_html_form_value_bytes

        HtmlValidator.reserve_render_resource(source_limit, byte_size(source), :html)

        HtmlValidator.reserve_render_resource(
          :max_html_form_value_bytes,
          Enum.reduce(values, 0, &(byte_size(&1) + &2)),
          :html
        )

        encoded_source = [<<byte_size(source)::unsigned-big-32>>, source]

        payload =
          if operation == :pattern do
            [
              <<1>>,
              encoded_source,
              <<length(values)::unsigned-big-32>>,
              Enum.map(values, &[<<byte_size(&1)::unsigned-big-32>>, &1])
            ]
          else
            [<<2>>, encoded_source]
          end

        payload = IO.iodata_to_binary(payload)

        ceilings = [
          Limits.get(:max_html_pattern_compile_work),
          Limits.get(:max_html_pattern_match_work),
          Limits.get(:max_html_form_native_allocation_bytes),
          Limits.get(:max_html_pattern_depth),
          byte_size(payload),
          Limits.get(:max_html_form_native_timeout_ms)
        ]

        {:ok,
         %{
           payload: payload,
           ceilings: Enum.map(ceilings, &Integer.to_string/1),
           timeout: List.last(ceilings)
         }}
    end
  catch
    {:render_resource_limit, {:error, {reason, diagnostic}}} ->
      {:error, {reason, Diagnostics.with_context(diagnostic, operation: :initial_form_value)}}
  end

  @doc "Interprets native parser state and charges its actual compiler, VM and allocation work."
  @spec result(term()) :: {:ok, atom()} | {:error, {atom(), Diagnostics.diagnostic()}}
  def result(native) do
    case native do
      {:ok,
       <<state, compile::unsigned-big-64, matches::unsigned-big-64, allocated::unsigned-big-64>>}
      when state in 0..4 ->
        HtmlValidator.reserve_render_resource(:max_html_pattern_compile_work, compile, :html)
        HtmlValidator.reserve_render_resource(:max_html_pattern_match_work, matches, :html)

        HtmlValidator.reserve_render_resource(
          :max_html_form_native_allocation_bytes,
          allocated,
          :html
        )

        {:ok, elem({:invalid_pattern, :matched, :mismatched, :valid_url, :invalid_url}, state)}

      {:exit, status} when status in 73..78 ->
        limit =
          elem(
            {:max_html_form_native_allocation_bytes, :max_html_pattern_compile_work,
             :max_html_pattern_match_work, :max_html_pattern_depth, :max_html_form_value_bytes,
             :max_html_form_native_timeout_ms},
            status - 73
          )

        failure(
          :resource_limit_exceeded,
          "initial form-value parsing exceeds #{limit} (configured limit #{Limits.get(limit)}); simplify the pattern/value or increase the limit"
        )

      {:exit, 79} ->
        failure(
          :resource_limit_exceeded,
          "native regex bytecode has fixed 65,535 capture-group and loop operand bounds; reduce the pattern group or loop count"
        )

      :timeout ->
        failure(
          :resource_limit_exceeded,
          "initial form-value parsing exceeds max_html_form_native_timeout_ms; simplify the pattern/value or increase the limit"
        )

      _ ->
        failure(
          :native_form_backend_unavailable,
          "native initial form-value parser failed; rebuild the pinned native/svg_backend artifacts"
        )
    end
  catch
    {:render_resource_limit, {:error, {reason, diagnostic}}} ->
      {:error, {reason, Diagnostics.with_context(diagnostic, operation: :initial_form_value)}}
  end

  defp failure(reason, message),
    do:
      Diagnostics.error(:html, reason, message,
        module: __MODULE__,
        operation: :initial_form_value
      )
end
