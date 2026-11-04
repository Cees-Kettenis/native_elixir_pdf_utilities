defmodule NativeElixirPdfUtilities.HtmlToPdf.FormPatternMatcher do
  @moduledoc false

  @doc "Matches admitted OTP expressions or native backwards assertions against one shared work allowance."
  @spec match_values(binary() | map(), [binary()], map()) :: {boolean(), non_neg_integer()}
  def match_values(prepared, values, limits) do
    key = {__MODULE__, :work}
    previous = Process.put(key, {0, limits})

    try do
      expression =
        case prepared do
          %{tree: tree} ->
            {tree, _cache} = compile_node(tree, %{})
            %{prepared | tree: tree}

          source ->
            case :re.compile("(*NO_START_OPT)(*NO_AUTO_POSSESS)" <> source, [:unicode]) do
              {:ok, regex} -> regex
              {:error, {message, _offset}} -> throw({:unsupported_pattern, to_string(message)})
            end
        end

      mismatch =
        Enum.reduce(values, false, fn value, mismatch ->
          charge(byte_size(value) + 1)

          matched? =
            case expression do
              %{tree: tree, names: names} ->
                characters = String.codepoints(value)
                offsets = Enum.scan(characters, 0, &(byte_size(&1) + &2))

                context = %{
                  value: value,
                  characters: List.to_tuple(characters),
                  offsets: List.to_tuple([0 | offsets]),
                  length: length(characters),
                  names: names,
                  limits: limits
                }

                finish = fn position, captures ->
                  if position == context.length, do: {:ok, captures}, else: :nomatch
                end

                match(tree, context, 0, 1, %{}, finish, 0) != :nomatch

              regex ->
                engine_match(value, regex, 1)
            end

          mismatch or not matched?
        end)

      {mismatch, elem(Process.get(key), 0)}
    after
      if is_nil(previous), do: Process.delete(key), else: Process.put(key, previous)
    end
  end

  defp compile_node(node, cache) do
    case node do
      {:chars, expression} ->
        case Map.fetch(cache, expression) do
          {:ok, regex} ->
            {{:character, regex}, cache}

          :error ->
            case :re.compile("(*NO_START_OPT)(*NO_AUTO_POSSESS)\\A(?:" <> expression <> ")\\z", [
                   :unicode
                 ]) do
              {:ok, regex} -> {{:character, regex}, Map.put(cache, expression, regex)}
              {:error, {message, _offset}} -> throw({:unsupported_pattern, to_string(message)})
            end
        end

      {:set, set} ->
        {character, cache} = compile_node({:chars, set.chars || "(?!)"}, cache)

        strings =
          set.strings
          |> Enum.map(&{&1, length(String.codepoints(&1))})
          |> Enum.sort_by(fn {value, size} -> {-size, value} end)

        {nonempty, empty} = Enum.split_with(strings, fn {_value, size} -> size > 0 end)

        alternatives =
          Enum.map(nonempty, &{:literal, &1}) ++ [character] ++ Enum.map(empty, &{:literal, &1})

        {{:alternate, alternatives}, cache}

      {:sequence, nodes} ->
        {nodes, cache} = Enum.map_reduce(nodes, cache, &compile_node/2)
        {{:sequence, nodes}, cache}

      {:alternate, nodes} ->
        {nodes, cache} = Enum.map_reduce(nodes, cache, &compile_node/2)
        {{:alternate, nodes}, cache}

      {:capture, index, child} ->
        {child, cache} = compile_node(child, cache)
        {{:capture, index, child}, cache}

      {:assert, direction, positive?, child} ->
        {child, cache} = compile_node(child, cache)
        {{:assert, direction, positive?, child}, cache}

      {:repeat, child, minimum, maximum, lazy?} ->
        indices = capture_indices(child)
        {child, cache} = compile_node(child, cache)
        {{:repeat, child, minimum, maximum, lazy?, indices}, cache}

      _ ->
        {node, cache}
    end
  end

  defp capture_indices(node) do
    case node do
      {:capture, index, child} -> [index | capture_indices(child)]
      {:sequence, children} -> Enum.flat_map(children, &capture_indices/1)
      {:alternate, children} -> Enum.flat_map(children, &capture_indices/1)
      {:assert, _direction, _positive?, child} -> capture_indices(child)
      {:repeat, child, _minimum, _maximum, _lazy?} -> capture_indices(child)
      _ -> []
    end
  end

  # Continuations retain normal backtracking outside assertions. An assertion
  # commits its first successful internal match before resuming the outer tree,
  # as required by ECMAScript CompileAssertion. A backwards assertion reverses
  # sequences and consuming atoms, but preserves authored alternative order.
  defp match(node, context, position, direction, captures, next, depth) do
    charge(1)

    if depth > context.limits.max_html_pattern_depth,
      do: throw({:form_limit, :max_html_pattern_depth})

    case node do
      {:sequence, nodes} ->
        nodes = if direction == -1, do: Enum.reverse(nodes), else: nodes
        match_sequence(nodes, context, position, direction, captures, next, depth)

      {:alternate, nodes} ->
        Enum.reduce_while(nodes, :nomatch, fn child, _result ->
          case match(child, context, position, direction, captures, next, depth + 1) do
            :nomatch -> {:cont, :nomatch}
            result -> {:halt, result}
          end
        end)

      {:character, regex} ->
        index = if direction == 1, do: position, else: position - 1

        if index >= 0 and index < context.length and
             engine_match(elem(context.characters, index), regex, 1),
           do: next.(position + direction, captures),
           else: :nomatch

      {:literal, {value, length}} ->
        match_literal(value, length, context, position, direction, captures, next)

      {:capture, index, child} ->
        finish = fn final_position, final_captures ->
          start = elem(context.offsets, min(position, final_position))
          stop = elem(context.offsets, max(position, final_position))
          value = binary_part(context.value, start, stop - start)
          charge(stop - start)

          next.(
            final_position,
            Map.put(final_captures, index, {value, abs(final_position - position)})
          )
        end

        match(child, context, position, direction, captures, finish, depth + 1)

      {:backref, reference} ->
        index = if is_binary(reference), do: Map.fetch!(context.names, reference), else: reference
        {value, length} = Map.get(captures, index, {"", 0})
        match_literal(value, length, context, position, direction, captures, next)

      {:assert, assertion_direction, positive?, child} ->
        found =
          match(
            child,
            context,
            position,
            assertion_direction,
            captures,
            fn _position, found -> {:ok, found} end,
            depth + 1
          )

        case {positive?, found} do
          {true, {:ok, found}} -> next.(position, found)
          {false, :nomatch} -> next.(position, captures)
          _ -> :nomatch
        end

      {:repeat, child, minimum, maximum, lazy?, indices} ->
        repeat(
          child,
          minimum,
          maximum,
          lazy?,
          indices,
          context,
          position,
          direction,
          captures,
          next,
          depth
        )

      {:boundary, positive?} ->
        left? = position > 0 and word_character?(elem(context.characters, position - 1))
        right? = position < context.length and word_character?(elem(context.characters, position))
        if left? != right? == positive?, do: next.(position, captures), else: :nomatch

      :start ->
        if position == 0, do: next.(position, captures), else: :nomatch

      :end ->
        if position == context.length, do: next.(position, captures), else: :nomatch
    end
  end

  defp match_sequence(nodes, context, position, direction, captures, next, depth) do
    case nodes do
      [] ->
        next.(position, captures)

      [node | remaining] ->
        finish = fn position, captures ->
          match_sequence(remaining, context, position, direction, captures, next, depth)
        end

        match(node, context, position, direction, captures, finish, depth + 1)
    end
  end

  defp repeat(
         child,
         minimum,
         maximum,
         lazy?,
         indices,
         context,
         position,
         direction,
         captures,
         next,
         depth
       ) do
    charge(1)

    more = fn ->
      if maximum == 0 do
        :nomatch
      else
        finish = fn final_position, final_captures ->
          if final_position == position and minimum == 0 do
            :nomatch
          else
            maximum = if maximum == :infinity, do: :infinity, else: maximum - 1

            repeat(
              child,
              max(minimum - 1, 0),
              maximum,
              lazy?,
              indices,
              context,
              final_position,
              direction,
              final_captures,
              next,
              depth + 1
            )
          end
        end

        match(child, context, position, direction, Map.drop(captures, indices), finish, depth + 1)
      end
    end

    if lazy? and minimum == 0 do
      case next.(position, captures) do
        :nomatch -> more.()
        result -> result
      end
    else
      case more.() do
        :nomatch when minimum == 0 -> next.(position, captures)
        result -> result
      end
    end
  end

  defp match_literal(value, length, context, position, direction, captures, next) do
    charge(byte_size(value) + 1)
    final_position = position + direction * length

    if final_position >= 0 and final_position <= context.length do
      start = elem(context.offsets, min(position, final_position))
      stop = elem(context.offsets, max(position, final_position))

      if binary_part(context.value, start, stop - start) == value,
        do: next.(final_position, captures),
        else: :nomatch
    else
      :nomatch
    end
  end

  defp word_character?(value) do
    case value do
      <<c>> -> c in ?a..?z or c in ?A..?Z or c in ?0..?9 or c == ?_
      _ -> false
    end
  end

  # OTP reports exhaustion but not the successful run's consumed counter.
  # Debit each entire granted allowance, including failed probes, from the one
  # request budget. Geometric retries avoid granting every list token a fresh
  # full budget. Anchoring prevents OTP restarting an allowance per position.
  defp engine_match(value, regex, allowance) do
    {used, limits} = Process.get({__MODULE__, :work})
    remaining = limits.max_html_pattern_match_work - used
    if remaining <= 0, do: throw({:form_limit, :max_html_pattern_match_work})
    allowance = min(allowance, remaining)
    charge(allowance)

    options = [
      :report_errors,
      {:capture, :none},
      {:match_limit, allowance},
      {:match_limit_recursion, max(limits.max_html_pattern_depth, 1)}
    ]

    case :re.run(value, regex, options) do
      :match -> true
      :nomatch -> false
      {:error, :match_limit} -> engine_match(value, regex, allowance * 2)
      {:error, :match_limit_recursion} -> throw({:form_limit, :max_html_pattern_depth})
    end
  end

  defp charge(work) do
    key = {__MODULE__, :work}
    {used, limits} = Process.get(key)

    if used + work > limits.max_html_pattern_match_work,
      do: throw({:form_limit, :max_html_pattern_match_work})

    if elem(Process.info(self(), :memory), 1) > limits.max_html_form_value_memory_bytes,
      do: throw({:form_limit, :max_html_form_value_memory_bytes})

    Process.put(key, {used + work, limits})
  end
end
