defmodule NativeElixirPdfUtilities.Validators.CssConditionalValueValidator do
  @moduledoc false

  alias NativeElixirPdfUtilities.Validators.{
    CssSupportsValidator,
    CssSyntaxValidator,
    HtmlValidator,
    MediaConditionValidator,
    PrintMediaValidator
  }

  @doc "Evaluates the shared static condition grammar used by if(), @when and @else."
  @spec matches?(String.t(), map() | nil | :unpositioned, map(), map(), map()) :: boolean()
  def matches?(source, media, aliases, properties, namespaces \\ %{}) do
    MediaConditionValidator.truth(
      source,
      %{},
      fn value, _ ->
        with {:ok, name, "(" <> tail} when byte_size(tail) > 0 <-
               HtmlValidator.parse_css_identifier(value),
             true <- String.ends_with?(tail, ")") do
          argument = binary_part(tail, 0, byte_size(tail) - 1)

          case String.downcase(name, :ascii) do
            "media" ->
              PrintMediaValidator.matches?(argument, media, aliases) or
                PrintMediaValidator.matches?("(" <> argument <> ")", media, aliases)

            "supports" ->
              CssSupportsValidator.matches?("(" <> argument <> ")", namespaces)

            "style" ->
              style_query(argument, properties)

            _ ->
              :unknown
          end
        else
          _ -> :unknown
        end
      end,
      false
    ) == true
  end

  defp style_query(source, properties) do
    case HtmlValidator.parse_css_identifier(String.trim(source)) do
      {:ok, "--" <> _ = name, tail} ->
        case String.trim(tail) do
          "" ->
            is_binary(Map.get(properties, name))

          <<":", value::binary>> ->
            case Map.get(properties, name) do
              property when is_binary(property) -> String.trim(property) == String.trim(value)
              _ -> false
            end

          _ ->
            :unknown
        end

      _ ->
        MediaConditionValidator.truth(
          source,
          properties,
          fn feature, properties ->
            case HtmlValidator.parse_css_identifier(feature) do
              {:ok, "--" <> _, _} -> style_query(feature, properties)
              _ -> :unknown
            end
          end,
          false
        )
    end
  end

  @doc "Parses the Values 5 branch grammar, preserving the order and opaque value tokens."
  @spec branches(String.t()) :: {:ok, [{String.t(), String.t()}]} | :error
  def branches(source) do
    parts = CssSyntaxValidator.split_trimmed(source, ";")
    parts = if List.last(parts) == "", do: Enum.drop(parts, -1), else: parts

    Enum.reduce_while(parts, {:ok, []}, fn branch, {:ok, acc} ->
      case CssSyntaxValidator.split_trimmed(branch, ":") do
        [test, value | rest] when test != "" ->
          {:cont, {:ok, [{test, Enum.join([value | rest], ":")} | acc]}}

        _ ->
          {:halt, :error}
      end
    end)
    |> case do
      {:ok, [_ | _] = branches} -> {:ok, Enum.reverse(branches)}
      _ -> :error
    end
  end
end
