defmodule NativeElixirPdfUtilities.Validators.HtmlPatternSyntaxValidator do
  @moduledoc false
  alias NativeElixirPdfUtilities.HtmlToPdf.UnicodeStringProperty

  @doc "Admits ECMAScript Unicode-v syntax and produces an OTP regular expression."
  @spec prepare(binary(), map()) ::
          {:ok, binary(), non_neg_integer()} | :invalid_pattern | {:limit, atom()}
  def prepare(source, limits) do
    key = {__MODULE__, :budget}
    previous = Process.put(key, {0, limits})

    try do
      {expression, ""} = expression(source, [], 0)
      {:ok, "\\A(?:" <> expression <> ")\\z", elem(Process.get(key), 0)}
    catch
      :invalid_pattern -> :invalid_pattern
      {:pattern_limit, limit} -> {:limit, limit}
    after
      if is_nil(previous), do: Process.delete(key), else: Process.put(key, previous)
    end
  end

  defp expression(source, output, depth) do
    charge(1, depth)

    case source do
      "" ->
        if depth > 0, do: throw(:invalid_pattern)
        {output |> Enum.reverse() |> IO.iodata_to_binary(), ""}

      <<"[", rest::binary>> ->
        {set, rest} = class(rest, depth + 1)
        {atom, rest} = quantified(set_regex(set), rest)
        expression(rest, [atom | output], depth)

      <<"\\", rest::binary>> ->
        {escaped, rest} = escape(rest, false, depth)
        {atom, rest} = quantified(escaped, rest)
        expression(rest, [atom | output], depth)

      <<"(?", rest::binary>> ->
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

        {inside, tail} = group(rest, depth + 1)

        {atom, tail} =
          if prefix == "(?:" or
               (String.starts_with?(prefix, "(?<") and prefix not in ["(?<=", "(?<!"]),
             do: quantified(prefix <> inside <> ")", tail),
             else: {prefix <> inside <> ")", tail}

        expression(tail, [atom | output], depth)

      <<"(", rest::binary>> ->
        {inside, rest} = group(rest, depth + 1)
        {atom, rest} = quantified("(" <> inside <> ")", rest)
        expression(rest, [atom | output], depth)

      <<")", rest::binary>> when depth > 0 ->
        {output |> Enum.reverse() |> IO.iodata_to_binary(), rest}

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

        expression(rest, [assertion | output], depth)

      <<c, _::binary>> when c in [?*, ?+, ??, ?{, ?}] ->
        throw(:invalid_pattern)

      <<c::utf8, rest::binary>> ->
        {atom, rest} = quantified(<<c::utf8>>, rest)
        expression(rest, [atom | output], depth)
    end
  end

  defp group(source, depth), do: expression(source, [], depth)

  defp quantified(atom, source) do
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

    case rest do
      <<"?", rest::binary>> when quantifier != "" -> {atom <> quantifier <> "?", rest}
      _ -> {atom <> quantifier, rest}
    end
  end

  defp escape(source, in_class?, depth) do
    charge(1, depth)

    case source do
      <<"u{", rest::binary>> ->
        case String.split(rest, "}", parts: 2) do
          [hex, tail] ->
            case Integer.parse(hex, 16) do
              {n, ""} when n in 0..0x10FFFF -> {"\\x{" <> hex <> "}", tail}
              _ -> throw(:invalid_pattern)
            end

          _ ->
            throw(:invalid_pattern)
        end

      <<"u", hex::binary-size(4), tail::binary>> ->
        case Integer.parse(hex, 16) do
          {n, ""} when n not in 0xD800..0xDFFF ->
            {"\\x{" <> hex <> "}", tail}

          {high, ""} when high in 0xD800..0xDBFF ->
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

          {low, ""} when low in 0xDC00..0xDFFF ->
            {"(?!)", tail}

          _ ->
            throw(:invalid_pattern)
        end

      <<kind, "{", rest::binary>> when kind in [?p, ?P] ->
        case String.split(rest, "}", parts: 2) do
          [name, tail] ->
            case UnicodeStringProperty.strings(name) do
              nil ->
                if not Regex.match?(~r/\A[A-Za-z_]+(?:=[A-Za-z_]+)?\z/, name),
                  do: throw(:invalid_pattern)

                {"\\" <> <<kind>> <> "{" <> name <> "}", tail}

              strings when kind == ?p ->
                charge(Enum.reduce(strings, 0, &(byte_size(&1) + &2)), depth)
                {strings_regex(strings), tail}

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
        {"(?(" <> number <> ")\\g{" <> number <> "}|)", rest}

      <<"k<", rest::binary>> when not in_class? ->
        case String.split(rest, ">", parts: 2) do
          [name, tail] -> {"(?(<" <> name <> ">)\\k<" <> name <> ">|)", tail}
          _ -> throw(:invalid_pattern)
        end

      <<"x", hex::binary-size(2), rest::binary>> ->
        case Integer.parse(hex, 16) do
          {_n, ""} -> {"\\x" <> hex, rest}
          _ -> throw(:invalid_pattern)
        end

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
        {last, tail} = range_endpoint(rest, depth)

        {%{
           chars: "[\\x{" <> Integer.to_string(c, 16) <> "}-" <> last <> "]",
           strings: MapSet.new()
         }, tail}

      <<"\\", rest::binary>> ->
        {escaped, tail} = escape(rest, true, depth)

        case tail do
          <<"-", rest::binary>> ->
            if String.starts_with?(escaped, [
                 "\\d",
                 "\\D",
                 "\\s",
                 "\\S",
                 "\\w",
                 "\\W",
                 "\\p",
                 "\\P",
                 "(?"
               ]),
               do: throw(:invalid_pattern)

            {last, tail} = range_endpoint(rest, depth)
            {%{chars: "[" <> escaped <> "-" <> last <> "]", strings: MapSet.new()}, tail}

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
        {escaped, tail} = escape(rest, true, depth)

        if String.starts_with?(escaped, [
             "\\d",
             "\\D",
             "\\s",
             "\\S",
             "\\w",
             "\\W",
             "\\p",
             "\\P",
             "(?"
           ]),
           do: throw(:invalid_pattern)

        {escaped, tail}

      <<c::utf8, rest::binary>> when c not in ~c"[]\\-" ->
        {"\\x{" <> Integer.to_string(c, 16) <> "}", rest}

      _ ->
        throw(:invalid_pattern)
    end
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

  defp charge(work, depth) do
    key = {__MODULE__, :budget}
    {used, limits} = Process.get(key)
    if depth > limits.max_html_pattern_depth, do: throw({:pattern_limit, :max_html_pattern_depth})

    if used + work > limits.max_html_pattern_compile_work,
      do: throw({:pattern_limit, :max_html_pattern_compile_work})

    Process.put(key, {used + work, limits})
  end
end
