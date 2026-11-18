defmodule NativeElixirPdfUtilities.Validators.CssAliasValidator do
  @moduledoc false

  alias NativeElixirPdfUtilities.Validators.{CssSyntaxValidator, HtmlValidator}

  @doc "Collects stylesheet-local aliases in source order."
  @spec definitions(String.t(), :media | :selector) :: map()
  def definitions(source, kind) do
    rule_name = if kind == :media, do: "custom-media", else: "custom-selector"

    if :binary.match(source, "@") != :nomatch and
         (String.contains?(source, "\\") or
            String.contains?(String.downcase(source, :ascii), rule_name)) do
      source
      |> CssSyntaxValidator.rules()
      |> Enum.reduce(%{}, fn rule, definitions ->
        with false <- rule.block,
             <<"@", prelude::binary>> <- String.trim(rule.selectors),
             {:ok, keyword, tail} <- HtmlValidator.parse_css_identifier(prelude),
             ^rule_name <- String.downcase(keyword, :ascii),
             {:ok, name, value} <- definition(String.trim(tail), kind) do
          Map.put(definitions, name, value)
        else
          _ -> definitions
        end
      end)
      |> remove_cycles(kind)
    else
      %{}
    end
  end

  defp remove_cycles(definitions, kind) do
    graph = :digraph.new([:private])

    try do
      Enum.each(definitions, fn {name, _} -> :digraph.add_vertex(graph, name) end)

      Enum.each(definitions, fn {name, value} ->
        dependencies = references(CssSyntaxValidator.components(value), kind, [])

        Enum.each(dependencies, fn dependency ->
          if Map.has_key?(definitions, dependency), do: :digraph.add_edge(graph, name, dependency)
        end)
      end)

      Map.drop(definitions, List.flatten(:digraph_utils.cyclic_strong_components(graph)))
    after
      :digraph.delete(graph)
    end
  end

  defp references(tokens, kind, result) do
    case {tokens, kind} do
      {[%{text: ":"}, %{kind: :token, text: candidate} | tail], :selector} ->
        references(tail, kind, reference(candidate, result))

      {[%{kind: :block, opener: "(", children: children} | tail], :media} ->
        inside = Enum.reject(children, &(&1.kind == :comment or String.trim(&1.text) == ""))

        result =
          case inside do
            [%{kind: :token, text: candidate}] -> reference(candidate, result)
            _ -> references(inside, kind, result)
          end

        references(tail, kind, result)

      {[%{kind: :block, opener: opener, children: children} | tail], :selector} ->
        result =
          if String.ends_with?(opener, "("), do: references(children, kind, result), else: result

        references(tail, kind, result)

      {[_ | tail], _} ->
        references(tail, kind, result)

      {[], _} ->
        result
    end
  end

  defp reference(candidate, result) do
    case HtmlValidator.parse_css_identifier(candidate) do
      {:ok, "--" <> _ = name, ""} -> [name | result]
      _ -> result
    end
  end

  defp definition(source, kind) do
    name_source =
      case {source, kind} do
        {<<":", rest::binary>>, :selector} -> rest
        {_, :selector} -> ""
        {source, :media} -> source
      end

    with {:ok, "--" <> suffix = name, tail} when suffix != "" <-
           HtmlValidator.parse_css_identifier(name_source),
         true <- tail != "" and String.trim_leading(tail) != tail,
         value when value != "" <- String.trim(tail),
         do: {:ok, name, value}
  end

  @doc "Replaces custom pseudo-classes by the specificity-preserving equivalent selector."
  @spec selector(String.t(), map()) :: String.t()
  def selector(source, aliases) do
    if map_size(aliases) == 0 or not String.contains?(source, ":"),
      do: source,
      else:
        selector_tokens(CssSyntaxValidator.components(source), aliases, [], [])
        |> IO.iodata_to_binary()
  end

  defp selector_tokens(components, aliases, stack, output) do
    case components do
      [%{text: ":"}, %{kind: :token, text: name} | tail] ->
        case HtmlValidator.parse_css_identifier(name) do
          {:ok, "--" <> _ = key, ""} ->
            HtmlValidator.check_render_resource(:max_depth, length(stack), :css)

            replacement =
              with {:ok, value} <- Map.fetch(aliases, key),
                   false <- key in stack do
                HtmlValidator.reserve_render_resource(
                  :max_decoded_bytes,
                  byte_size(value) + 5,
                  :css
                )

                [
                  ":is(",
                  selector_tokens(
                    CssSyntaxValidator.components(value),
                    aliases,
                    [key | stack],
                    []
                  ),
                  ")"
                ]
              else
                _ -> [":", name]
              end

            selector_tokens(tail, aliases, stack, [replacement | output])

          _ ->
            selector_tokens(tail, aliases, stack, [name, ":" | output])
        end

      [%{kind: :block, opener: opener, closed: true, children: children} = token | rest] ->
        replacement =
          if String.ends_with?(opener, "(") do
            [opener, selector_tokens(children, aliases, stack, []), ")"]
          else
            token.text
          end

        selector_tokens(rest, aliases, stack, [replacement | output])

      [%{text: text} | rest] ->
        selector_tokens(rest, aliases, stack, [text | output])

      [] ->
        Enum.reverse(output)
    end
  end
end
