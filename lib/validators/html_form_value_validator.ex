defmodule NativeElixirPdfUtilities.Validators.HtmlFormValueValidator do
  @moduledoc false
  alias NativeElixirPdfUtilities.{Diagnostics, Limits}

  alias NativeElixirPdfUtilities.Validators.{
    HtmlValidator,
    HtmlPatternSyntaxValidator,
    HtmlUrlValidator
  }

  @doc "Prepares bounded UTF-8 input and an immutable Elixir form-value work budget."
  @spec prepare(term(), term(), term()) ::
          {:ok, map()} | {:error, {atom(), Diagnostics.diagnostic()}}
  def prepare(operation, source, values) do
    if operation in [:pattern, :url] and is_binary(source) and String.valid?(source) and
         is_list(values) and Enum.all?(values, &(is_binary(&1) and String.valid?(&1))) do
      HtmlValidator.reserve_render_resource(:max_html_pattern_values, length(values), :html)

      limit =
        if operation == :pattern, do: :max_html_pattern_bytes, else: :max_html_form_value_bytes

      HtmlValidator.reserve_render_resource(limit, byte_size(source), :html)

      HtmlValidator.reserve_render_resource(
        :max_html_form_value_bytes,
        Enum.reduce(values, 0, &(byte_size(&1) + &2)),
        :html
      )

      {:ok,
       %{
         operation: operation,
         source: source,
         values: values,
         limits: Limits.effective(),
         timeout: Limits.get(:max_html_form_value_timeout_ms)
       }}
    else
      failure(
        :invalid_document,
        "initial form values require valid UTF-8 strings and a list of values"
      )
    end
  catch
    {:render_resource_limit, {:error, {reason, diagnostic}}} ->
      {:error, {reason, Diagnostics.with_context(diagnostic, operation: :initial_form_value)}}
  end

  @doc "Evaluates an admitted request in its disposable BEAM worker."
  @spec evaluate(map()) ::
          {:ok, atom(), map()} | {:limit, atom()} | {:unsupported_pattern, binary()}
  def evaluate(request) do
    limits = request.limits

    if elem(Process.info(self(), :memory), 1) > limits.max_html_form_value_memory_bytes,
      do: throw({:form_limit, :max_html_form_value_memory_bytes})

    {state, compile, match_work} =
      case request.operation do
        :url ->
          work = byte_size(request.source)

          if work > limits.max_html_pattern_compile_work,
            do: throw({:form_limit, :max_html_pattern_compile_work})

          {if(HtmlUrlValidator.absolute_input_url?(request.source),
             do: :valid_url,
             else: :invalid_url
           ), work, 0}

        :pattern ->
          case HtmlPatternSyntaxValidator.prepare(request.source, limits) do
            :invalid_pattern ->
              {:invalid_pattern, byte_size(request.source), 0}

            {:limit, limit} ->
              throw({:form_limit, limit})

            {:ok, expression, compile} ->
              if byte_size(expression) > limits.max_html_form_value_memory_bytes,
                do: throw({:form_limit, :max_html_form_value_memory_bytes})

              case :re.compile("(*NO_START_OPT)(*NO_AUTO_POSSESS)" <> expression, [:unicode]) do
                {:ok, regex} ->
                  {mismatch, work} =
                    Enum.reduce(request.values, {false, 0}, fn value, {mismatch, used} ->
                      used = used + byte_size(value) + 1

                      if used > limits.max_html_pattern_match_work,
                        do: throw({:form_limit, :max_html_pattern_match_work})

                      options = [
                        :report_errors,
                        {:capture, :none},
                        {:match_limit, max(limits.max_html_pattern_match_work - used, 1)},
                        {:match_limit_recursion, max(limits.max_html_pattern_depth, 1)}
                      ]

                      case :re.run(value, regex, options) do
                        :match ->
                          {mismatch, used}

                        :nomatch ->
                          {true, used}

                        {:error, :match_limit} ->
                          throw({:form_limit, :max_html_pattern_match_work})

                        {:error, :match_limit_recursion} ->
                          throw({:form_limit, :max_html_pattern_depth})
                      end
                    end)

                  {if(mismatch, do: :mismatched, else: :matched), compile, work}

                {:error, {message, _offset}} ->
                  message = to_string(message)

                  if String.contains?(message, [
                       "too many capturing",
                       "number too big",
                       "regular expression is too large"
                     ]) do
                    throw({:unsupported_pattern, message})
                  else
                    {:invalid_pattern, compile, 0}
                  end
              end
          end
      end

    memory = elem(Process.info(self(), :memory), 1)

    if memory > limits.max_html_form_value_memory_bytes,
      do: throw({:form_limit, :max_html_form_value_memory_bytes})

    {:ok, state, %{compile: compile, matches: match_work, memory: memory}}
  catch
    {:form_limit, limit} -> {:limit, limit}
    {:unsupported_pattern, message} -> {:unsupported_pattern, message}
  end

  @doc "Charges completed worker work and returns recoverable form-value diagnostics."
  @spec result(term()) :: {:ok, atom()} | {:error, {atom(), Diagnostics.diagnostic()}}
  def result(result) do
    case result do
      {:ok, state, %{compile: compile, matches: matches, memory: memory}}
      when state in [:invalid_pattern, :matched, :mismatched, :valid_url, :invalid_url] ->
        HtmlValidator.reserve_render_resource(:max_html_pattern_compile_work, compile, :html)
        HtmlValidator.reserve_render_resource(:max_html_pattern_match_work, matches, :html)
        HtmlValidator.reserve_render_resource(:max_html_form_value_memory_bytes, memory, :html)
        {:ok, state}

      {:limit, limit} ->
        failure(
          :resource_limit_exceeded,
          "initial form-value parsing exceeds #{limit} (configured limit #{Limits.get(limit)}); simplify the pattern/value or increase the limit"
        )

      :timeout ->
        failure(
          :resource_limit_exceeded,
          "initial form-value parsing exceeds max_html_form_value_timeout_ms; simplify the pattern/value or increase the limit"
        )

      {:unsupported_pattern, message} ->
        failure(
          :unsupported_html,
          "the OTP pattern compiler cannot represent this pattern: #{message}; simplify the pattern. This is a fixed engine bound, not an invalid authored HTML pattern"
        )

      _ ->
        failure(
          :form_value_backend_unavailable,
          "Elixir initial form-value worker failed; retry the operation and inspect the worker failure"
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
