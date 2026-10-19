defmodule NativeElixirPdfUtilities.Validators.HtmlPatternSyntaxValidator do
  @moduledoc false
  alias NativeElixirPdfUtilities.HtmlToPdf.{UnicodeCharacterProperty, UnicodeStringProperty}

  @doc "Admits ECMAScript Unicode-v syntax and produces an OTP regular expression."
  @spec prepare(binary(), map()) ::
          {:ok, binary(), non_neg_integer()} | :invalid_pattern | {:limit, atom()}
  def prepare(source, limits) do
    case prepare_source(source, limits, false) do
      {:ok, expression, work, _native_required?} -> {:ok, expression, work}
      result -> result
    end
  end

  @doc "Prepares an OTP expression or native tree for backwards assertions and quantified capture resets."
  @spec prepare_execution(binary(), map()) ::
          {:ok, binary() | map(), non_neg_integer()} | :invalid_pattern | {:limit, atom()}
  def prepare_execution(source, limits) do
    native? = String.contains?(source, ["(?<=", "(?<!"])

    case prepare_source(source, limits, native?) do
      {:ok, _expression, work, true} when not native? ->
        # Only patterns needing ECMAScript capture resets pay for a second
        # syntax pass. Charge both passes against the immutable request budget.
        remaining = %{
          limits
          | max_html_pattern_compile_work: limits.max_html_pattern_compile_work - work
        }

        case prepare_source(source, remaining, true) do
          {:ok, tree, extra_work, _} -> {:ok, tree, work + extra_work}
          result -> result
        end

      {:ok, expression, work, _} ->
        {:ok, expression, work}

      result ->
        result
    end
  end

  defp prepare_source(source, limits, native?) do
    key = {__MODULE__, :budget}
    previous = Process.put(key, {0, limits})
    state_key = {__MODULE__, :syntax}

    previous_state =
      Process.put(state_key, %{
        native?: native?,
        native_required?: false,
        captures: 0,
        names: %{},
        refs: []
      })

    try do
      {expression, ""} = expression(source, [], 0)
      state = Process.get(state_key)

      if Enum.any?(state.refs, fn ref ->
           if is_integer(ref), do: ref > state.captures, else: not Map.has_key?(state.names, ref)
         end),
         do: throw(:invalid_pattern)

      prepared =
        if native?,
          do: %{tree: expression, names: state.names, captures: state.captures},
          else: "\\A(?:" <> expression <> ")\\z"

      {:ok, prepared, elem(Process.get(key), 0), state.native_required?}
    catch
      :invalid_pattern -> :invalid_pattern
      {:pattern_limit, limit} -> {:limit, limit}
    after
      if is_nil(previous), do: Process.delete(key), else: Process.put(key, previous)

      if is_nil(previous_state),
        do: Process.delete(state_key),
        else: Process.put(state_key, previous_state)
    end
  end

  defp expression(source, output, depth) do
    charge(1, depth)

    case source do
      "" ->
        if depth > 0, do: throw(:invalid_pattern)
        {join_output(output), ""}

      <<"[", rest::binary>> ->
        {set, rest} = class(rest, depth + 1)
        {atom, rest} = quantified(if(native?(), do: {:set, set}, else: set_regex(set)), rest)
        expression(rest, [atom | output], depth)

      <<"\\", rest::binary>> ->
        {escaped, rest} = escape(rest, false, depth)

        {atom, rest} =
          if escaped in ["\\b", "\\B", {:boundary, true}, {:boundary, false}],
            do: {escaped, rest},
            else: quantified(escaped, rest)

        expression(rest, [atom | output], depth)

      <<"(?", rest::binary>> ->
        previous_captures = Process.get({__MODULE__, :syntax}).captures

        {prefix, rest} =
          case rest do
            <<c, rest::binary>> when c in [?:, ?=, ?!] ->
              {"(?" <> <<c>>, rest}

            <<"<=", rest::binary>> ->
              {"(?<=", rest}

            <<"<!", rest::binary>> ->
              {"(?<!", rest}

            <<"<", rest::binary>> ->
              case String.split(rest, ">", parts: 2) do
                [name, rest] ->
                  if Regex.match?(~r/\A[A-Za-z_$][A-Za-z0-9_$]*\z/, name),
                    do: {"(?<" <> name <> ">", rest},
                    else: throw(:invalid_pattern)

                _ ->
                  throw(:invalid_pattern)
              end

            _ ->
              throw(:invalid_pattern)
          end

        capture =
          if String.starts_with?(prefix, "(?<") and prefix not in ["(?<=", "(?<!"],
            do: capture(String.slice(prefix, 3, byte_size(prefix) - 4)),
            else: nil

        {inside, tail} = group(rest, depth + 1)

        node =
          if native?() do
            case prefix do
              "(?:" -> inside
              "(?=" -> {:assert, 1, true, inside}
              "(?!" -> {:assert, 1, false, inside}
              "(?<=" -> {:assert, -1, true, inside}
              "(?<!" -> {:assert, -1, false, inside}
              _ -> {:capture, capture, inside}
            end
          else
            prefix <> inside <> ")"
          end

        {atom, tail} =
          if prefix == "(?:" or not is_nil(capture),
            do:
              quantified(
                node,
                tail,
                Process.get({__MODULE__, :syntax}).captures - previous_captures
              ),
            else: {node, tail}

        expression(tail, [atom | output], depth)

      <<"(", rest::binary>> ->
        previous_captures = Process.get({__MODULE__, :syntax}).captures
        capture = capture(nil)
        {inside, rest} = group(rest, depth + 1)
        node = if native?(), do: {:capture, capture, inside}, else: "(" <> inside <> ")"

        {atom, rest} =
          quantified(node, rest, Process.get({__MODULE__, :syntax}).captures - previous_captures)

        expression(rest, [atom | output], depth)

      <<")", rest::binary>> when depth > 0 ->
        {join_output(output), rest}

      <<c, _::binary>> when c in [?), ?]] ->
        throw(:invalid_pattern)

      <<".", rest::binary>> ->
        {atom, rest} = quantified("[^\\r\\n\\x{2028}\\x{2029}]", rest)
        expression(rest, [atom | output], depth)

      <<c, rest::binary>> when c in [?^, ?$, ?|] ->
        assertion =
          case c do
            ?^ -> "\\A"
            ?$ -> "\\z"
            ?| -> "|"
          end

        assertion =
          if native?(),
            do:
              case(c,
                do: (
                  ?^ -> :start
                  ?$ -> :end
                  ?| -> :or
                )
              ),
            else: assertion

        expression(rest, [assertion | output], depth)

      <<c, _::binary>> when c in [?*, ?+, ??, ?{, ?}] ->
        throw(:invalid_pattern)

      <<c::utf8, rest::binary>> ->
        {atom, rest} = quantified(<<c::utf8>>, rest)
        expression(rest, [atom | output], depth)
    end
  end

  defp group(source, depth), do: expression(source, [], depth)

  defp quantified(atom, source, captures \\ 0) do
    {quantifier, rest} =
      case source do
        <<c, rest::binary>> when c in [?*, ?+, ??] ->
          {<<c>>, rest}

        <<"{", rest::binary>> ->
          case String.split(rest, "}", parts: 2) do
            [counts, rest] ->
              if not Regex.match?(~r/\A[0-9]+(?:,[0-9]*)?\z/, counts), do: throw(:invalid_pattern)
              numbers = String.split(counts, ",")

              if Enum.any?(
                   numbers,
                   &(byte_size(&1) >
                       NativeElixirPdfUtilities.Limits.get(:max_html_numeric_token_bytes))
                 ),
                 do: throw({:pattern_limit, :max_html_numeric_token_bytes})

              case numbers do
                [minimum, maximum] when maximum != "" ->
                  if String.to_integer(minimum) > String.to_integer(maximum),
                    do: throw(:invalid_pattern)

                _ ->
                  :ok
              end

              {"{" <> counts <> "}", rest}

            _ ->
              throw(:invalid_pattern)
          end

        _ ->
          {"", source}
      end

    {lazy?, rest} =
      case rest do
        <<"?", rest::binary>> when quantifier != "" -> {true, rest}
        _ -> {false, rest}
      end

    if quantifier != "" and captures > 0 do
      key = {__MODULE__, :syntax}
      Process.put(key, %{Process.get(key) | native_required?: true})
    end

    if native?() do
      atom = if is_binary(atom), do: {:chars, atom}, else: atom

      node =
        case quantifier do
          "" ->
            atom

          "*" ->
            {:repeat, atom, 0, :infinity, lazy?}

          "+" ->
            {:repeat, atom, 1, :infinity, lazy?}

          "?" ->
            {:repeat, atom, 0, 1, lazy?}

          _ ->
            counts =
              quantifier
              |> String.trim_leading("{")
              |> String.trim_trailing("}")
              |> String.split(",")

            {minimum, maximum} =
              case counts do
                [n] -> {String.to_integer(n), String.to_integer(n)}
                [n, ""] -> {String.to_integer(n), :infinity}
                [n, m] -> {String.to_integer(n), String.to_integer(m)}
              end

            {:repeat, atom, minimum, maximum, lazy?}
        end

      {node, rest}
    else
      {atom <> quantifier <> if(lazy?, do: "?", else: ""), rest}
    end
  end

  defp escape(source, in_class?, depth) do
    charge(1, depth)

    case source do
      <<"u{", rest::binary>> ->
        case String.split(rest, "}", parts: 2) do
          [hex, tail] ->
            if not Regex.match?(~r/\A[0-9A-Fa-f]+\z/, hex), do: throw(:invalid_pattern)

            case Integer.parse(hex, 16) do
              {n, ""} when n in 0xD800..0xDFFF -> {"(?!)", tail}
              {n, ""} when n in 0..0x10FFFF -> {"\\x{" <> hex <> "}", tail}
              _ -> throw(:invalid_pattern)
            end

          _ ->
            throw(:invalid_pattern)
        end

      <<"u", hex::binary-size(4), tail::binary>> ->
        if not Regex.match?(~r/\A[0-9A-Fa-f]{4}\z/, hex), do: throw(:invalid_pattern)

        {codepoint, ""} = Integer.parse(hex, 16)

        case codepoint do
          n when n not in 0xD800..0xDFFF ->
            {"\\x{" <> hex <> "}", tail}

          high when high in 0xD800..0xDBFF ->
            case tail do
              <<"\\u", low_hex::binary-size(4), rest::binary>> ->
                case Integer.parse(low_hex, 16) do
                  {low, ""} when low in 0xDC00..0xDFFF ->
                    {"\\x{" <>
                       Integer.to_string(0x10000 + (high - 0xD800) * 1024 + low - 0xDC00, 16) <>
                       "}", rest}

                  _ ->
                    {"(?!)", tail}
                end

              _ ->
                {"(?!)", tail}
            end

          _low ->
            {"(?!)", tail}
        end

      <<kind, "{", rest::binary>> when kind in [?p, ?P] ->
        case String.split(rest, "}", parts: 2) do
          [name, tail] ->
            case UnicodeStringProperty.strings(name) do
              nil ->
                case UnicodeCharacterProperty.expression(name, kind == ?P) do
                  nil ->
                    throw(:invalid_pattern)

                  regex ->
                    charge(byte_size(regex), depth)
                    {regex, tail}
                end

              strings when kind == ?p ->
                charge(Enum.reduce(strings, 0, &(byte_size(&1) + &2)), depth)

                {if(native?(), do: {:set, from_strings(strings)}, else: strings_regex(strings)),
                 tail}

              _ ->
                throw(:invalid_pattern)
            end

          _ ->
            throw(:invalid_pattern)
        end

      <<c, rest::binary>> when c in ~c"dDsSwWbBfnrtv" ->
        value =
          case c do
            ?v ->
              "\\x{B}"

            ?s ->
              "[\\x{9}-\\x{D}\\x{20}\\x{A0}\\x{1680}\\x{2000}-\\x{200A}\\x{2028}\\x{2029}\\x{202F}\\x{205F}\\x{3000}\\x{FEFF}]"

            ?S ->
              "[^\\x{9}-\\x{D}\\x{20}\\x{A0}\\x{1680}\\x{2000}-\\x{200A}\\x{2028}\\x{2029}\\x{202F}\\x{205F}\\x{3000}\\x{FEFF}]"

            ?b when in_class? ->
              "\\x{8}"

            ?B when in_class? ->
              throw(:invalid_pattern)

            c when c in [?b, ?B] ->
              if native?(), do: {:boundary, c == ?b}, else: "\\" <> <<c>>

            _ ->
              "\\" <> <<c>>
          end

        {value, rest}

      <<"0", rest::binary>> ->
        if rest != "" and :binary.at(rest, 0) in ?0..?9, do: throw(:invalid_pattern)
        {"\\x{0}", rest}

      <<c, _::binary>> when c in ?1..?9 ->
        if in_class?, do: throw(:invalid_pattern)
        [_, number, rest] = Regex.run(~r/\A([1-9][0-9]*)(.*)\z/s, source)
        ref = String.to_integer(number)
        reference(ref)

        {if(native?(), do: {:backref, ref}, else: "(?(" <> number <> ")\\g{" <> number <> "}|)"),
         rest}

      <<"k<", rest::binary>> when not in_class? ->
        case String.split(rest, ">", parts: 2) do
          [name, tail] ->
            reference(name)

            {if(native?(),
               do: {:backref, name},
               else: "(?(<" <> name <> ">)\\k<" <> name <> ">|)"
             ), tail}

          _ ->
            throw(:invalid_pattern)
        end

      <<"x", hex::binary-size(2), rest::binary>> ->
        if not Regex.match?(~r/\A[0-9A-Fa-f]{2}\z/, hex), do: throw(:invalid_pattern)

        {"\\x" <> hex, rest}

      <<"c", letter, rest::binary>> when letter in ?A..?Z or letter in ?a..?z ->
        {"\\x{" <> Integer.to_string(rem(letter, 32), 16) <> "}", rest}

      <<c::utf8, rest::binary>> ->
        allowed = if in_class?, do: ~c"^$\\.*+?()[]{}|/-!#%&,:;<=>@`~", else: ~c"^$\\.*+?()[]{}|/"
        if c in allowed, do: {Regex.escape(<<c::utf8>>), rest}, else: throw(:invalid_pattern)

      _ ->
        throw(:invalid_pattern)
    end
  end

  defp class(source, depth) do
    {negative?, source} =
      if String.starts_with?(source, "^"),
        do: {true, binary_part(source, 1, byte_size(source) - 1)},
        else: {false, source}

    {set, rest} = class_parts(source, [], nil, depth)

    if negative? do
      if MapSet.size(set.strings) > 0, do: throw(:invalid_pattern)
      {%{chars: "(?!" <> char_regex(set) <> ")[\\s\\S]", strings: MapSet.new()}, rest}
    else
      {set, rest}
    end
  end

  defp class_parts(source, parts, operation, depth) do
    charge(1, depth)

    case source do
      <<"]", rest::binary>> ->
        {combine(parts, operation), rest}

      <<op::binary-size(2), rest::binary>> when op in ["&&", "--"] ->
        if parts == [] or (not is_nil(operation) and operation != op), do: throw(:invalid_pattern)
        {right, tail} = class_atom(rest, depth)
        left = combine(parts, operation)
        class_parts(tail, [set_operation(left, right, op)], op, depth)

      "" ->
        throw(:invalid_pattern)

      _ ->
        if not is_nil(operation), do: throw(:invalid_pattern)
        {set, rest} = class_atom(source, depth)
        class_parts(rest, [set | parts], operation, depth)
    end
  end

  defp class_atom(source, depth) do
    case source do
      <<"[", rest::binary>> ->
        class(rest, depth + 1)

      <<"\\q{", rest::binary>> ->
        case String.split(rest, "}", parts: 2) do
          [strings, tail] ->
            if String.contains?(strings, ["\\", "[", "]", "{"]), do: throw(:invalid_pattern)
            {from_strings(String.split(strings, "|")), tail}

          _ ->
            throw(:invalid_pattern)
        end

      <<"\\", kind, "{", rest::binary>> when kind in [?p, ?P] ->
        [name | _] = String.split(rest, "}", parts: 2)

        case UnicodeStringProperty.strings(name) do
          strings when is_list(strings) and kind == ?p ->
            [_, tail] = String.split(rest, "}", parts: 2)
            charge(Enum.reduce(strings, 0, &(byte_size(&1) + &2)), depth)
            {from_strings(strings), tail}

          _ ->
            {escaped, tail} = escape(<<kind, "{", rest::binary>>, true, depth)
            {%{chars: escaped, strings: MapSet.new()}, tail}
        end

      <<c::utf8, "-", rest::binary>> when c not in ~c"[]\\-" ->
        if c in ~c"(){}/|", do: throw(:invalid_pattern)
        {last, tail} = range_endpoint(rest, depth)

        {%{
           chars: range_expression(c, last),
           strings: MapSet.new()
         }, tail}

      <<"\\", rest::binary>> ->
        {escaped, tail} = escape(rest, true, depth)

        case tail do
          <<"-", rest::binary>> ->
            first = range_escape_scalar(source)
            {last, tail} = range_endpoint(rest, depth)
            {%{chars: range_expression(first, last), strings: MapSet.new()}, tail}

          _ ->
            {%{chars: escaped, strings: MapSet.new()}, tail}
        end

      <<c::utf8, rest::binary>> ->
        if c in ~c"()[]{}/|-", do: throw(:invalid_pattern)

        if byte_size(rest) > 0 and binary_part(rest, 0, 1) == <<c::utf8>> and
             c in ~c"!#$%&*+,.:;<=>?@^`~",
           do: throw(:invalid_pattern)

        {%{chars: Regex.escape(<<c::utf8>>), strings: MapSet.new()}, rest}

      _ ->
        throw(:invalid_pattern)
    end
  end

  defp range_endpoint(source, depth) do
    case source do
      <<"\\", rest::binary>> ->
        {_escaped, tail} = escape(rest, true, depth)
        {range_escape_scalar(source), tail}

      <<c::utf8, rest::binary>> when c not in ~c"()[]{}/|\\-" ->
        {c, rest}

      _ ->
        throw(:invalid_pattern)
    end
  end

  # Character-class ranges require single character endpoints even when a
  # property expression happens to contain only one character. Surrogates are
  # valid ECMAScript code points, but cannot occur in the admitted UTF-8 values.
  defp range_escape_scalar(source) do
    case source do
      <<"\\u{", rest::binary>> ->
        [hex | _] = String.split(rest, "}", parts: 2)
        String.to_integer(hex, 16)

      <<"\\u", hex::binary-size(4), tail::binary>> ->
        first = String.to_integer(hex, 16)

        case tail do
          <<"\\u", low::binary-size(4), _::binary>> when first in 0xD800..0xDBFF ->
            # read_escape already consumed this pair only after validating
            # the trailing low surrogate as part of the one range endpoint.
            last = String.to_integer(low, 16)
            0x10000 + (first - 0xD800) * 1024 + last - 0xDC00

          _ ->
            first
        end

      <<"\\x", hex::binary-size(2), _::binary>> ->
        String.to_integer(hex, 16)

      <<"\\c", letter, _::binary>> ->
        rem(letter, 32)

      <<"\\", c, _::binary>> when c in ~c"bfnrtv0" ->
        Map.fetch!(%{?b => 8, ?f => 12, ?n => 10, ?r => 13, ?t => 9, ?v => 11, ?0 => 0}, c)

      <<"\\", c::utf8, _::binary>> when c in ~c"^$\\.*+?()[]{}|/-!#%&,:;<=>@`~" ->
        c

      _ ->
        throw(:invalid_pattern)
    end
  end

  defp range_expression(first, last) do
    if first > last, do: throw(:invalid_pattern)

    # The surrogate interval is a format invariant, not a configurable limit.
    ranges =
      [{first, min(last, 0xD7FF)}, {max(first, 0xE000), last}]
      |> Enum.filter(fn {start, stop} -> start <= stop end)
      |> Enum.map_join(fn {start, stop} ->
        "\\x{" <> Integer.to_string(start, 16) <> "}-\\x{" <> Integer.to_string(stop, 16) <> "}"
      end)

    if ranges == "", do: "(?!)", else: "[" <> ranges <> "]"
  end

  defp from_strings(strings) do
    {characters, strings} =
      Enum.split_with(strings, fn s ->
        case s do
          <<_::utf8>> -> true
          _ -> false
        end
      end)

    %{
      chars:
        if(characters == [],
          do: nil,
          else: "[" <> Enum.map_join(characters, &Regex.escape/1) <> "]"
        ),
      strings: MapSet.new(strings)
    }
  end

  defp combine(parts, operation) do
    if not is_nil(operation) and length(parts) != 1, do: throw(:invalid_pattern)

    %{
      chars:
        case Enum.flat_map(parts, &if(is_nil(&1.chars), do: [], else: [&1.chars])) do
          [] -> nil
          chars -> "(?:" <> Enum.join(chars, "|") <> ")"
        end,
      strings: Enum.reduce(parts, MapSet.new(), &MapSet.union(&1.strings, &2))
    }
  end

  defp set_operation(left, right, op) do
    chars =
      if op == "&&",
        do: "(?=" <> char_regex(right) <> ")" <> char_regex(left),
        else: "(?!" <> char_regex(right) <> ")" <> char_regex(left)

    strings =
      if op == "&&",
        do: MapSet.intersection(left.strings, right.strings),
        else: MapSet.difference(left.strings, right.strings)

    %{chars: chars, strings: strings}
  end

  defp char_regex(set), do: set.chars || "(?!)"

  defp set_regex(set) do
    if MapSet.size(set.strings) == 0,
      do: char_regex(set),
      else: "(?:" <> strings_regex(MapSet.to_list(set.strings)) <> "|" <> char_regex(set) <> ")"
  end

  # A prefix trie keeps Unicode string properties within OTP's regex bytecode bound.
  defp strings_regex(strings) do
    groups =
      Enum.group_by(strings, fn s ->
        case s do
          "" -> ""
          <<c::utf8, _::binary>> -> <<c::utf8>>
        end
      end)

    alternatives =
      for {prefix, values} <- Enum.sort(groups), prefix != "" do
        tails =
          Enum.map(values, &binary_part(&1, byte_size(prefix), byte_size(&1) - byte_size(prefix)))

        Regex.escape(prefix) <> if(tails == [""], do: "", else: strings_regex(tails))
      end

    body =
      case alternatives do
        [] -> ""
        [one] -> one
        many -> "(?:" <> Enum.join(many, "|") <> ")"
      end

    if Map.has_key?(groups, "") and alternatives != [], do: "(?:" <> body <> ")?", else: body
  end

  defp native?(), do: Process.get({__MODULE__, :syntax}).native?

  defp capture(name) do
    key = {__MODULE__, :syntax}
    state = Process.get(key)
    if not is_nil(name) and Map.has_key?(state.names, name), do: throw(:invalid_pattern)
    index = state.captures + 1
    names = if is_nil(name), do: state.names, else: Map.put(state.names, name, index)
    Process.put(key, %{state | captures: index, names: names})
    index
  end

  defp reference(ref) do
    key = {__MODULE__, :syntax}
    state = Process.get(key)
    Process.put(key, %{state | refs: [ref | state.refs]})
  end

  defp join_output(output) do
    output = Enum.reverse(output)

    if native?() do
      # Empty alternatives participate in the language, including a leading or
      # trailing empty branch. Split explicitly rather than dropping separators.
      {branches, current} =
        Enum.reduce(output, {[], []}, fn node, {branches, current} ->
          if node == :or,
            do: {[{:sequence, Enum.reverse(current)} | branches], []},
            else: {branches, [node | current]}
        end)

      branches = Enum.reverse([{:sequence, Enum.reverse(current)} | branches])

      case branches do
        [one] -> one
        many -> {:alternate, many}
      end
    else
      IO.iodata_to_binary(output)
    end
  end

  defp charge(work, depth) do
    key = {__MODULE__, :budget}
    {used, limits} = Process.get(key)
    if depth > limits.max_html_pattern_depth, do: throw({:pattern_limit, :max_html_pattern_depth})

    if used + work > limits.max_html_pattern_compile_work,
      do: throw({:pattern_limit, :max_html_pattern_compile_work})

    Process.put(key, {used + work, limits})
  end
end
