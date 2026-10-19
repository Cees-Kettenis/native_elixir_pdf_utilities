defmodule NativeElixirPdfUtilities.Validators.HtmlFormValueValidator do
  @moduledoc false
  alias NativeElixirPdfUtilities.{Diagnostics, Limits}
  alias NativeElixirPdfUtilities.HtmlToPdf.FormPatternMatcher

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

    # Install the GC cap only after the initial observation, so sub-minimum
    # budgets return their named limit rather than an invalid BEAM option.
    # Shared binaries count, but native :re allocations do not. BEAM permits
    # no cap smaller than its minimum heap, and checks this cap during GC.
    Process.flag(:max_heap_size, %{
      size:
        max(
          div(limits.max_html_form_value_memory_bytes, :erlang.system_info(:wordsize)),
          elem(:erlang.system_info(:min_heap_size), 1)
        ),
      kill: true,
      error_logger: false,
      include_shared_binaries: true
    })

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
          case HtmlPatternSyntaxValidator.prepare_execution(request.source, limits) do
            :invalid_pattern ->
              {:invalid_pattern, byte_size(request.source), 0}

            {:limit, limit} ->
              throw({:form_limit, limit})

            {:ok, expression, compile} ->
              expression_bytes =
                if is_binary(expression),
                  do: byte_size(expression),
                  else: :erlang.external_size(expression)

              if expression_bytes > limits.max_html_form_value_memory_bytes,
                do: throw({:form_limit, :max_html_form_value_memory_bytes})

              {mismatch, work} =
                FormPatternMatcher.match_values(expression, request.values, limits)

              {if(mismatch, do: :mismatched, else: :matched), compile, work}
          end
      end

    memory = elem(Process.info(self(), :memory), 1)

    # result/1 reserves this observation against the shared render allowance.
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
          "the OTP pattern compiler reached a fixed engine bound for this valid ECMAScript pattern: #{message}; simplify the pattern or use a supported equivalent. This is not an invalid authored HTML pattern and the constraint was not ignored"
        )

      _ ->
        failure(
          :form_value_backend_unavailable,
          "Elixir initial form-value worker failed or was terminated by its GC-enforced BEAM heap limit; inspect the worker failure or increase max_html_form_value_memory_bytes. Native regex allocations are outside that heap limit"
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
