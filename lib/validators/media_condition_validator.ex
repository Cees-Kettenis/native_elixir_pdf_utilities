defmodule NativeElixirPdfUtilities.Validators.MediaConditionValidator do
  @moduledoc "Parses media query boolean structure before evaluating feature predicates."

  alias NativeElixirPdfUtilities.Validators.HtmlValidator

  @doc "Evaluates a media query with parse-invalid propagation and bounded nesting."
  @spec evaluate(String.t(), map(), (String.t(), map() -> boolean() | :unknown)) :: boolean()
  def evaluate(query, context, feature) do
    case parse(query, 0, true) do
      {:ok, expression} -> resolve(expression, context, feature) == true
      :invalid -> false
    end
  end

  defp parse(query, depth, query?) do
    HtmlValidator.check_render_resource(:max_media_query_depth, depth, :style)
    source = HtmlValidator.trim_css_whitespace(query)

    tokens = NativeElixirPdfUtilities.Validators.CssMathValidator.component_tokens(source, false)
    valid? = match?({:ok, _}, tokens)

    terms =
      case tokens do
        {:ok, terms} ->
          Enum.map(terms, fn term ->
            case HtmlValidator.parse_css_identifier(term) do
              {:ok, identifier, ""} -> String.downcase(identifier, :ascii)
              _ -> term
            end
          end)

        _ ->
          []
      end

    cond do
      not valid? or terms == [] ->
        :invalid

      query? and hd(terms) in ["not", "only"] and length(terms) > 1 and
        match?({:ok, _, ""}, HtmlValidator.parse_css_identifier(Enum.at(terms, 1))) and
          Enum.at(terms, 1) not in ~w(not only and or) ->
        modifier = hd(terms)

        case parse(Enum.drop(terms, 1) |> Enum.join(" "), depth + 1, true) do
          {:ok, expression} ->
            {:ok, if(modifier == "not", do: {:not, expression}, else: expression)}

          _ ->
            :invalid
        end

      query? and match?({:ok, _, ""}, HtmlValidator.parse_css_identifier(hd(terms))) and
          hd(terms) not in ~w(not only and or) ->
        case terms do
          [type] ->
            {:ok, {:type, type}}

          [type, "and" | rest] when rest != [] ->
            case parse(Enum.join(rest, " "), depth + 1, false) do
              {:ok, {:or, _}} when length(rest) > 1 -> :invalid
              {:ok, condition} -> {:ok, {:and, [{:type, type}, condition]}}
              _ -> :invalid
            end

          _ ->
            :invalid
        end

      hd(terms) == "not" ->
        case terms do
          ["not", operand] ->
            case parse(operand, depth + 1, false) do
              {:ok, expression} -> {:ok, {:not, expression}}
              _ -> :invalid
            end

          _ ->
            :invalid
        end

      length(terms) > 1 ->
        operators =
          terms
          |> Enum.with_index()
          |> Enum.filter(fn {_term, index} -> rem(index, 2) == 1 end)
          |> Enum.map(&elem(&1, 0))

        if rem(length(terms), 2) == 1 and Enum.uniq(operators) in [["and"], ["or"]] do
          operands =
            terms
            |> Enum.with_index()
            |> Enum.filter(fn {_term, index} -> rem(index, 2) == 0 end)
            |> Enum.map(fn {operand, _} -> parse(operand, depth + 1, false) end)

          if Enum.all?(operands, &match?({:ok, _}, &1)),
            do:
              {:ok,
               {if(hd(operators) == "and", do: :and, else: :or), Enum.map(operands, &elem(&1, 1))}},
            else: :invalid
        else
          :invalid
        end

      String.starts_with?(source, "(") and String.ends_with?(source, ")") ->
        inside =
          source
          |> String.slice(1, String.length(source) - 2)
          |> HtmlValidator.trim_css_whitespace()

        case parse(inside, depth + 1, false) do
          {:ok, expression} -> {:ok, expression}
          :invalid -> if inside != "", do: {:ok, {:feature, inside}}, else: :invalid
        end

      match?({:ok, _, "(" <> _}, HtmlValidator.parse_css_identifier(source)) and
          String.ends_with?(source, ")") ->
        {:ok, {:feature, source}}

      true ->
        :invalid
    end
  end

  defp resolve(expression, context, feature) do
    case expression do
      {:type, type} ->
        type in ~w(all print)

      {:feature, value} ->
        feature.(value, context)

      {:not, child} ->
        case resolve(child, context, feature) do
          :unknown -> :unknown
          result -> not result
        end

      {:and, children} ->
        results = Enum.map(children, &resolve(&1, context, feature))

        cond do
          false in results -> false
          :unknown in results -> :unknown
          true -> true
        end

      {:or, children} ->
        results = Enum.map(children, &resolve(&1, context, feature))

        cond do
          true in results -> true
          :unknown in results -> :unknown
          true -> false
        end
    end
  end
end
