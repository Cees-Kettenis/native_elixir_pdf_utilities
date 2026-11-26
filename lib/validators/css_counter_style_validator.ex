defmodule NativeElixirPdfUtilities.Validators.CssCounterStyleValidator do
  @moduledoc false
  alias NativeElixirPdfUtilities.Diagnostics
  alias NativeElixirPdfUtilities.Validators.{CssSyntaxValidator, HtmlValidator}

  @text_symbol Regex.compile!(
                 "\\A(?:#{HtmlValidator.css_string_pattern()}|#{HtmlValidator.css_identifier_pattern()})\\z",
                 "u"
               )

  @doc "Prepares visual counter-style definitions from a selected stylesheet."
  @spec prepare(String.t()) :: {:ok, map()} | {:error, term()}
  def prepare(css) do
    if not Regex.match?(~r/@counter-style/iu, css) do
      {:ok, %{}}
    else
      css
      |> CssSyntaxValidator.rules()
      |> Enum.filter(
        &String.starts_with?(String.downcase(String.trim(&1.selectors)), "@counter-style")
      )
      |> Enum.reduce_while({:ok, %{}}, fn rule, {:ok, styles} ->
        name =
          String.trim(String.slice(String.trim(rule.selectors), 14..-1//1))

        with true <- identifier?(name) and name not in ~w(none decimal disc),
             {:ok, definition} <- definition(rule.declarations) do
          {:cont, {:ok, Map.put(styles, name, definition)}}
        else
          _ ->
            {:halt,
             Diagnostics.error(:css, :invalid_css, "invalid visual @counter-style definition",
               source: rule.source
             )}
        end
      end)
    end
  end

  @doc "Checks a case-sensitive custom counter name."
  @spec identifier?(term()) :: boolean()
  def identifier?(name) do
    is_binary(name) and Regex.match?(~r/^(?:--|-?[_a-zA-Z\p{L}])[_a-zA-Z0-9\p{L}-]*$/u, name) and
      name not in ~w(inherit initial unset revert revert-layer default)
  end

  @doc "Validates descriptors and their system-specific requirements."
  @spec definition(String.t()) :: {:ok, map()} | :error
  def definition(source) do
    defaults = %{
      system: :symbolic,
      start: 1,
      symbols: [],
      additive_symbols: [],
      negative: {"-", ""},
      prefix: "",
      suffix: ". ",
      range: :auto,
      pad: {0, ""},
      fallback: "decimal",
      specified: MapSet.new()
    }

    result =
      Enum.reduce_while(CssSyntaxValidator.declarations(source), {:ok, defaults}, fn record,
                                                                                     {:ok, style} ->
        {property, value} =
          case record.declaration do
            {p, v} -> {p, v}
            {p, v, :important} -> {p, v}
          end

        case descriptor(property, value) do
          {:ok, fields} ->
            {:cont,
             {:ok,
              Map.merge(style, fields)
              |> Map.update!(:specified, &MapSet.union(&1, MapSet.new(Map.keys(fields))))}}

          :error ->
            {:halt, :error}
        end
      end)

    with {:ok, style} <- result,
         true <-
           (case style.system do
              :numeric -> length(style.symbols) >= 2
              :alphabetic -> length(style.symbols) >= 2
              :additive -> style.additive_symbols != []
              {:extends, _} -> true
              _ -> style.symbols != []
            end) do
      {:ok, style}
    else
      _ -> :error
    end
  end

  @doc "Prepares an anonymous symbols() counter style using the same visual-system rules."
  @spec symbols(String.t()) :: {:ok, map()} | :error
  def symbols(source) do
    case Regex.run(~r/^symbols\((.*)\)$/su, String.trim(source)) do
      [_, body] ->
        tokens =
          body
          |> CssSyntaxValidator.components()
          |> Enum.reject(&(&1.kind == :comment or String.trim(&1.text) in ["", ","]))
          |> Enum.map(& &1.text)

        {system, symbols} =
          case tokens do
            [system | symbols] when system in ~w(cyclic numeric alphabetic symbolic fixed) ->
              {system, symbols}

            symbols ->
              {"symbolic", symbols}
          end

        definition("system:" <> system <> ";symbols:" <> Enum.join(symbols, " ") <> ";suffix:' '")

      _ ->
        :error
    end
  end

  defp descriptor(property, source) do
    tokens =
      case {property, HtmlValidator.split_css_tokens(source)} do
        {"system", [keyword | arguments]} -> [String.downcase(keyword, :ascii) | arguments]
        {_, tokens} -> tokens
      end

    strings =
      Enum.map(tokens, fn token ->
        if String.starts_with?(token, ["\"", "'"]),
          do: HtmlValidator.decode_css_string(token),
          else: token
      end)

    case {property, tokens, strings} do
      {"system", [name], _} when name in ~w(cyclic numeric alphabetic symbolic additive fixed) ->
        {:ok, %{system: String.to_existing_atom(name)}}

      {"system", ["fixed", initial], _} ->
        case NativeElixirPdfUtilities.Validators.CssMathValidator.prepare_scalar(
               initial,
               :integer
             ) do
          {:ok, n} -> {:ok, %{system: :fixed, start: n}}
          _ -> :error
        end

      {"system", ["extends", name], _} ->
        if identifier?(name), do: {:ok, %{system: {:extends, name}}}, else: :error

      {"symbols", [_ | _], strings} ->
        if Enum.all?(strings, &(&1 != "")), do: {:ok, %{symbols: strings}}, else: :error

      {"additive-symbols", _, _} ->
        pairs =
          source
          |> CssSyntaxValidator.split(",")
          |> Enum.map(fn pair ->
            case HtmlValidator.split_css_tokens(pair) do
              [n, symbol] ->
                case NativeElixirPdfUtilities.Validators.CssMathValidator.prepare_scalar(
                       n,
                       :integer
                     ) do
                  {:ok, n} when n >= 0 -> {n, HtmlValidator.decode_css_string(symbol)}
                  _ -> nil
                end

              _ ->
                nil
            end
          end)

        weights = for {n, _} <- pairs, do: n

        if length(weights) == length(pairs) and weights == Enum.sort(Enum.uniq(weights), :desc),
          do: {:ok, %{additive_symbols: pairs}},
          else: :error

      {"negative", [_], [prefix]} ->
        {:ok, %{negative: {prefix, ""}}}

      {"negative", [_, _], [prefix, suffix]} ->
        {:ok, %{negative: {prefix, suffix}}}

      {"prefix", [_], [value]} ->
        {:ok, %{prefix: value}}

      {"suffix", [_], [value]} ->
        {:ok, %{suffix: value}}

      {"fallback", [name], _} ->
        if identifier?(name), do: {:ok, %{fallback: name}}, else: :error

      {"pad", [left, right], [left_text, right_text]} ->
        with :error <- pad(left, right, right_text), do: pad(right, left, left_text)

      {"range", ["auto"], _} ->
        {:ok, %{range: :auto}}

      {"range", _, _} ->
        ranges =
          source
          |> String.split(",")
          |> Enum.map(fn part ->
            case String.split(part) do
              [low, high] ->
                parse = fn s ->
                  case s do
                    "-infinite" ->
                      {:ok, :negative_infinity}

                    "infinite" ->
                      {:ok, :infinity}

                    _ ->
                      case NativeElixirPdfUtilities.Validators.CssMathValidator.prepare_scalar(
                             s,
                             :integer
                           ) do
                        {:ok, n} -> {:ok, n}
                        _ -> :error
                      end
                  end
                end

                with {:ok, a} <- parse.(low),
                     {:ok, b} <- parse.(high),
                     true <-
                       a != :infinity and b != :negative_infinity and
                         (a == :negative_infinity or b == :infinity or
                            (is_integer(a) and is_integer(b) and a <= b)),
                     do: {a, b}

              _ ->
                :error
            end
          end)

        if Enum.all?(ranges, &is_tuple/1), do: {:ok, %{range: ranges}}, else: :error

      # Speech descriptors have no effect on static visual PDF output.
      {"speak-as", [_ | _], _} ->
        {:ok, %{}}

      _ ->
        :error
    end
  end

  defp pad(width, symbol, symbol_text) do
    with true <- Regex.match?(@text_symbol, symbol),
         {:ok, n} when is_integer(n) and n >= 0 <-
           NativeElixirPdfUtilities.Validators.CssMathValidator.prepare_scalar(width, :integer) do
      HtmlValidator.check_render_resource(
        :max_output_bytes,
        n * max(byte_size(symbol_text), 1),
        :css
      )

      {:ok, %{pad: {n, symbol_text}}}
    else
      _ -> :error
    end
  end
end
