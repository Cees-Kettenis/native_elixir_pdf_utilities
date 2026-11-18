defmodule NativeElixirPdfUtilities.Validators.CssGeneratedContentValidator do
  @moduledoc false
  alias NativeElixirPdfUtilities.Validators.{CssSyntaxValidator, HtmlValidator}

  @doc "Parses static generated content, retaining document-dependent functions for pagination."
  @spec prepare(String.t()) :: {:ok, list() | :none} | :error
  def prepare(source) do
    if String.downcase(String.trim(source)) in ["none", "normal"] do
      {:ok, :none}
    else
      values =
        source
        |> CssSyntaxValidator.components()
        |> Enum.reject(&(&1.kind == :comment or String.trim(&1.text) == ""))

      {visual, alternatives} = Enum.split_while(values, &(&1.text != "/"))

      alternative =
        case alternatives do
          [] -> {:ok, []}
          [_ | rest] -> parse_values(rest)
        end

      with {:ok, parts} <- parse_values(visual),
           true <- parts != [],
           {:ok, alternatives} <- alternative,
           true <-
             alternatives == [] or
               Enum.all?(alternatives, &(elem(&1, 0) in [:string, :attr, :counter, :counters])),
           true <- alternatives != [] or not Enum.any?(values, &(&1.text == "/")) do
        {:ok, if(alternatives == [], do: parts, else: parts ++ [{:alternative, alternatives}])}
      else
        _ -> :error
      end
    end
  end

  @doc "Checks where a prepared generated-content sequence may be used."
  @spec valid_context?(list() | :none, :body | :margin | :string_set | :bookmark) :: boolean()
  def valid_context?(parts, context) do
    case {parts, context} do
      {:none, _} ->
        true

      {[{:element, _, _}], :margin} ->
        true

      {parts, :string_set} ->
        Enum.all?(parts, &(elem(&1, 0) in [:string, :counter, :counters, :content, :attr]))

      {parts, :bookmark} ->
        Enum.all?(parts, &(elem(&1, 0) not in [:element, :image, :alternative, :leader]))

      {parts, _} ->
        Enum.all?(parts, &(elem(&1, 0) not in [:element, :content]))
    end
  end

  @doc "Validates prepared publishing tokens at computed-style boundaries."
  @spec valid_parts?(term()) :: boolean()
  def valid_parts?(parts) do
    case parts do
      parts when is_list(parts) and length(parts) >= 0 ->
        Enum.all?(parts, fn part ->
          case part do
            {kind, value} when kind in [:string, :attr, :image, :counter, :leader, :content] ->
              is_binary(value)

            {:attr, name, fallback} ->
              is_binary(name) and is_binary(fallback)

            {:counter, name, style} ->
              identifier?(name) and counter_style?(style)

            {:counters, name, separator, style} ->
              identifier?(name) and is_binary(separator) and counter_style?(style)

            {:target_counter, target, name, style} ->
              is_binary(target) and identifier?(name) and counter_style?(style)

            {:target_text, target, kind} ->
              is_binary(target) and kind in ~w(content before after first-letter)

            {kind, name, selection} when kind in [:string, :element] ->
              identifier?(name) and selection in ~w(first start last first-except)

            {:quote, kind} ->
              kind in [:open_quote, :close_quote, :no_open_quote, :no_close_quote]

            {:alternative, alternatives} ->
              case alternatives do
                [_ | _] when length(alternatives) >= 0 ->
                  Enum.all?(alternatives, fn p ->
                    is_tuple(p) and tuple_size(p) > 0 and
                      elem(p, 0) in [:string, :attr, :counter, :counters]
                  end) and valid_parts?(alternatives)

                _ ->
                  false
              end

            _ ->
              false
          end
        end)

      _ ->
        false
    end
  end

  defp parse_values(values) do
    Enum.reduce_while(values, {:ok, []}, fn value, {:ok, parts} ->
      case part(value) do
        {:ok, part} -> {:cont, {:ok, [part | parts]}}
        _ -> {:halt, :error}
      end
    end)
    |> case do
      {:ok, parts} -> {:ok, Enum.reverse(parts)}
      _ -> :error
    end
  end

  defp part(value) do
    text = value.text

    cond do
      value.kind == :string ->
        {:ok, {:string, HtmlValidator.decode_css_string(text)}}

      text in ~w(open-quote close-quote no-open-quote no-close-quote) ->
        {:ok, {:quote, text |> String.replace("-", "_") |> String.to_existing_atom()}}

      String.starts_with?(String.downcase(text), "url(") ->
        case HtmlValidator.parse_background_image_reference(text) do
          {:ok, source} when is_binary(source) -> {:ok, {:image, source}}
          _ -> :error
        end

      value.kind == :block and String.contains?(text, "(") and String.ends_with?(text, ")") ->
        [name, rest] = String.split(text, "(", parts: 2)
        body = binary_part(rest, 0, byte_size(rest) - 1)
        with {:ok, args} <- arguments(body), do: function(String.downcase(name), args)

      true ->
        :error
    end
  end

  defp arguments(body) do
    tokens = CssSyntaxValidator.components(body) |> Enum.reject(&(&1.kind == :comment))

    result =
      Enum.reduce_while(tokens, {:ok, [], ""}, fn token, {:ok, args, current} ->
        if token.text == "," do
          if String.trim(current) == "",
            do: {:halt, :error},
            else: {:cont, {:ok, args ++ [String.trim(current)], ""}}
        else
          {:cont, {:ok, args, current <> token.text}}
        end
      end)

    case result do
      {:ok, [], current} when current == "" ->
        {:ok, []}

      {:ok, args, current} ->
        if String.trim(current) == "", do: :error, else: {:ok, args ++ [String.trim(current)]}

      _ ->
        :error
    end
  end

  defp function(name, args) do
    case {name, args} do
      {"counter", [counter]} ->
        if identifier?(counter), do: {:ok, {:counter, counter}}, else: :error

      {"counter", [counter, style]} ->
        if identifier?(counter) and counter_style?(style),
          do: {:ok, {:counter, counter, style}},
          else: :error

      {"counters", [counter, separator]} ->
        function("counters", [counter, separator, "decimal"])

      {"counters", [counter, separator, style]} ->
        if identifier?(counter) and counter_style?(style) and
             String.starts_with?(separator, ["\"", "'"]),
           do: {:ok, {:counters, counter, HtmlValidator.decode_css_string(separator), style}},
           else: :error

      {"attr", [attribute]} ->
        attribute(attribute, "")

      {"attr", [attribute, fallback]} ->
        if String.starts_with?(fallback, ["\"", "'"]),
          do: attribute(attribute, HtmlValidator.decode_css_string(fallback)),
          else: :error

      {"leader", [pattern]} ->
        valid = pattern in ~w(dotted solid space) or String.starts_with?(pattern, ["\"", "'"])

        pattern =
          Map.get(
            %{"dotted" => ".", "solid" => "_", "space" => " "},
            pattern,
            HtmlValidator.decode_css_string(pattern)
          )

        if valid and pattern != "", do: {:ok, {:leader, pattern}}, else: :error

      {"target-counter", [target, counter]} ->
        function(name, [target, counter, "decimal"])

      {"target-counter", [target, counter, style]} ->
        if identifier?(counter) and counter_style?(style),
          do: {:ok, {:target_counter, target, counter, style}},
          else: :error

      {"target-text", [target]} ->
        {:ok, {:target_text, target, "content"}}

      {"target-text", [target, kind]} when kind in ~w(content before after first-letter) ->
        {:ok, {:target_text, target, kind}}

      {"string", [name]} ->
        function("string", [name, "first"])

      {"element", [name]} ->
        function("element", [name, "first"])

      {function, [name, selection]}
      when function in ["string", "element"] and selection in ~w(first start last first-except) ->
        if identifier?(name),
          do: {:ok, {String.to_existing_atom(function), name, selection}},
          else: :error

      {"content", []} ->
        {:ok, {:content, "text"}}

      {"content", [kind]} when kind in ~w(text before after first-letter) ->
        {:ok, {:content, kind}}

      _ ->
        :error
    end
  end

  defp attribute(source, fallback) do
    case String.split(source, ~r/\s+/u, trim: true) do
      [name] ->
        if Regex.match?(~r/^[a-zA-Z_][a-zA-Z0-9_:-]*$/u, name),
          do:
            if(fallback == "",
              do: {:ok, {:attr, String.downcase(name)}},
              else: {:ok, {:attr, String.downcase(name), fallback}}
            ),
          else: :error

      [name, type] when type in ~w(string raw-string) ->
        if Regex.match?(~r/^[a-zA-Z_][a-zA-Z0-9_:-]*$/u, name),
          do: {:ok, {:attr, String.downcase(name), fallback}},
          else: :error

      _ ->
        :error
    end
  end

  defp counter_style?(value) do
    is_binary(value) and
      (identifier?(value) or
         match?(
           {:ok, _},
           NativeElixirPdfUtilities.Validators.CssCounterStyleValidator.symbols(value)
         ))
  end

  defp identifier?(value),
    do: NativeElixirPdfUtilities.Validators.CssCounterStyleValidator.identifier?(value)
end
