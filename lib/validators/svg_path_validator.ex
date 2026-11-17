defmodule NativeElixirPdfUtilities.Validators.SvgPathValidator do
  @moduledoc "Canonical SVG path lexical grammar shared by SVG, CSS clipping and static motion paths."
  alias NativeElixirPdfUtilities.Validators.HtmlValidator

  @doc "Parses complete SVG path commands, retaining numeric values and compact arc flags without reinterpretation."
  @spec prepare(term()) :: {:ok, [{byte(), [number()]}]} | :error
  def prepare(source) do
    HtmlValidator.with_render_budget(fn ->
      if is_binary(source) and String.valid?(source),
        do: parse_path_commands(source, nil, true, []),
        else: :error
    end)
  end

  defp parse_path_commands(source, repeated_command, first?, operations) do
    source = skip_path_whitespace(source)

    case source do
      "" ->
        {:ok, Enum.reverse(operations)}

      <<command, rest::binary>> when command in ~c"MmZzLlHhVvCcSsQqTtAa" ->
        cond do
          first? and command not in ~c"Mm" ->
            :error

          command in ~c"Zz" ->
            HtmlValidator.reserve_render_resource(:max_entries, 1, :layout)
            parse_path_commands(rest, nil, false, [{command, []} | operations])

          true ->
            with {:ok, values, rest} <- parse_path_arguments(rest, command) do
              HtmlValidator.reserve_render_resource(:max_entries, 1, :layout)
              HtmlValidator.reserve_render_resource(:max_work, length(values) + 1, :layout)

              repeated =
                case command do
                  ?M -> ?L
                  ?m -> ?l
                  _ -> command
                end

              parse_path_commands(rest, repeated, false, [{command, values} | operations])
            end
        end

      _ ->
        if is_nil(repeated_command) do
          :error
        else
          with {:ok, rest} <- path_argument_separator(source, :optional),
               {:ok, values, rest} <- parse_path_arguments(rest, repeated_command) do
            HtmlValidator.reserve_render_resource(:max_entries, 1, :layout)
            HtmlValidator.reserve_render_resource(:max_work, length(values) + 1, :layout)

            parse_path_commands(rest, repeated_command, false, [
              {repeated_command, values} | operations
            ])
          end
        end
    end
  end

  defp parse_path_arguments(source, command) do
    arguments =
      case command do
        value when value in ~c"MmLlTt" -> [:number, :number]
        value when value in ~c"HhVv" -> [:number]
        value when value in ~c"Cc" -> [:number, :number, :number, :number, :number, :number]
        value when value in ~c"SsQq" -> [:number, :number, :number, :number]
        value when value in ~c"Aa" -> [:number, :number, :number, :flag, :flag, :number, :number]
      end

    arguments
    |> Enum.with_index()
    |> Enum.reduce_while({:ok, [], source}, fn {kind, index}, {:ok, values, remaining} ->
      separator =
        cond do
          index == 0 -> :command
          index == 3 and command in ~c"Aa" -> :required
          true -> :optional
        end

      result =
        with {:ok, remaining} <- path_argument_separator(remaining, separator),
             do: consume_path_argument(remaining, kind)

      case result do
        {:ok, value, remaining} -> {:cont, {:ok, [value | values], remaining}}
        :error -> {:halt, :error}
      end
    end)
    |> case do
      {:ok, values, rest} -> {:ok, Enum.reverse(values), rest}
      error -> error
    end
  end

  defp path_argument_separator(source, requirement) do
    trimmed = skip_path_whitespace(source)

    case {requirement, trimmed} do
      {:command, <<",", _rest::binary>>} -> :error
      {:command, _} -> {:ok, trimmed}
      {_, <<",", rest::binary>>} -> {:ok, skip_path_whitespace(rest)}
      {:required, _} when byte_size(source) == byte_size(trimmed) -> :error
      _ -> {:ok, trimmed}
    end
  end

  defp skip_path_whitespace(source) do
    case source do
      <<character, rest::binary>> when character in [9, 10, 12, 13, 32] ->
        skip_path_whitespace(rest)

      _ ->
        source
    end
  end

  defp consume_path_argument(source, kind) do
    case {kind, source} do
      {:flag, <<flag, rest::binary>>} when flag in ~c"01" ->
        {:ok, flag - ?0, rest}

      {:number, source} ->
        case Regex.run(~r/\A[+-]?(?:[0-9]+(?:\.[0-9]*)?|\.[0-9]+)(?:[eE][+-]?[0-9]+)?/, source,
               capture: :first
             ) do
          [token] ->
            # Retain the original SVG 1.1 trailing-dot spelling. Only its lexical
            # token is normalized for the existing canonical numeric parser.
            normalized = Regex.replace(~r/\.(?=[eE]|$)/, token, ".0")

            case HtmlValidator.parse_css_number(normalized) do
              {number, ""} ->
                {:ok, number,
                 binary_part(source, byte_size(token), byte_size(source) - byte_size(token))}

              _ ->
                :error
            end

          _ ->
            :error
        end

      _ ->
        :error
    end
  end
end
