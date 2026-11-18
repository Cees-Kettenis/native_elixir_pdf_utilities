defmodule NativeElixirPdfUtilities.HtmlToPdf.CounterStyle do
  @moduledoc false

  @type style ::
          :decimal
          | :decimal_leading_zero
          | :lower_alpha
          | :upper_alpha
          | :lower_roman
          | :upper_roman
          | :disc
          | :circle
          | :square
          | :disclosure_open
          | :disclosure_closed
          | :none

  @doc false
  @spec marker(style(), integer()) :: String.t()
  def marker(style, ordinal) do
    case style do
      :none ->
        ""

      :disc ->
        "•"

      :circle ->
        "◦"

      :square ->
        "▪"

      :disclosure_open ->
        "▼"

      :disclosure_closed ->
        "▶"

      :decimal ->
        "#{ordinal}."

      :decimal_leading_zero ->
        sign = if ordinal < 0, do: "-", else: ""

        sign <>
          String.pad_leading(Integer.to_string(abs(ordinal)), 2 - byte_size(sign), "0") <> "."

      style when style in [:lower_alpha, :upper_alpha] and ordinal > 0 ->
        letters =
          Stream.unfold(ordinal, fn value ->
            if value == 0, do: nil, else: {97 + rem(value - 1, 26), div(value - 1, 26)}
          end)
          |> Enum.reverse()
          |> List.to_string()

        if(style == :upper_alpha, do: String.upcase(letters), else: letters) <> "."

      # CSS defines Roman styles only for 1..3999. This is a fixed counter-style range.
      style when style in [:lower_roman, :upper_roman] and ordinal in 1..3999 ->
        {letters, 0} =
          Enum.reduce(
            [
              {1000, "M"},
              {900, "CM"},
              {500, "D"},
              {400, "CD"},
              {100, "C"},
              {90, "XC"},
              {50, "L"},
              {40, "XL"},
              {10, "X"},
              {9, "IX"},
              {5, "V"},
              {4, "IV"},
              {1, "I"}
            ],
            {[], ordinal},
            fn {value, letters}, {parts, remaining} ->
              {[String.duplicate(letters, div(remaining, value)) | parts], rem(remaining, value)}
            end
          )

        letters = letters |> Enum.reverse() |> IO.iodata_to_binary()
        if(style == :lower_roman, do: String.downcase(letters), else: letters) <> "."

      _ ->
        "#{ordinal}."
    end
  end

  @doc "Formats a validated visual style, including marker affixes."
  @spec marker(style() | String.t() | {:custom_counter_style, map()}, integer(), map()) ::
          String.t()
  def marker(style, ordinal, definitions) do
    {style, definitions} =
      case style do
        {:custom_counter_style, definition} ->
          {:anonymous, Map.put(definitions, :anonymous, definition)}

        _ ->
          {style, definitions}
      end

    case Map.get(definitions, style) do
      nil ->
        if is_binary(style),
          do: format(style, ordinal, definitions) <> ". ",
          else: marker(style, ordinal)

      _ ->
        {definition, text} = representation(style, ordinal, definitions, %{})
        definition.prefix <> text <> definition.suffix
    end
  end

  @doc "Formats a counter value without list-marker prefix and suffix."
  @spec format(atom() | String.t(), integer(), map()) :: String.t()
  def format(style, ordinal, definitions) do
    {style, definitions} =
      if is_binary(style) and String.starts_with?(style, "symbols(") do
        {:ok, definition} =
          NativeElixirPdfUtilities.Validators.CssCounterStyleValidator.symbols(style)

        {:anonymous, Map.put(definitions, :anonymous, definition)}
      else
        {style, definitions}
      end

    if Map.has_key?(definitions, style) do
      {_definition, text} = representation(style, ordinal, definitions, %{})
      text
    else
      name = if is_atom(style), do: Atom.to_string(style), else: style

      digits = %{
        "arabic-indic" => "٠١٢٣٤٥٦٧٨٩",
        "persian" => "۰۱۲۳۴۵۶۷۸۹",
        "devanagari" => "०१२३४५६७८९",
        "bengali" => "০১২৩৪৫৬৭৮৯",
        "thai" => "๐๑๒๓๔๕๖๗๘๙",
        "tibetan" => "༠༡༢༣༤༥༦༧༨༩",
        "cjk-decimal" => "〇一二三四五六七八九",
        "full-width-decimal" => "０１２３４５６７８９"
      }

      cond do
        Map.has_key?(digits, name) ->
          mapping =
            Map.fetch!(digits, name)
            |> String.graphemes()
            |> Enum.with_index()
            |> Map.new(fn {d, n} -> {Integer.to_string(n), d} end)

          ordinal
          |> Integer.to_string()
          |> String.graphemes()
          |> Enum.map_join(&Map.get(mapping, &1, &1))

        name == "lower-greek" and ordinal > 0 ->
          encode(ordinal, String.graphemes("αβγδεζηθικλμνξοπρστυφχψω"), :alphabetic)

        true ->
          atom =
            Map.get(
              %{
                "lower-alpha" => :lower_alpha,
                "upper-alpha" => :upper_alpha,
                "lower-latin" => :lower_alpha,
                "upper-latin" => :upper_alpha,
                "lower-roman" => :lower_roman,
                "upper-roman" => :upper_roman,
                "decimal-leading-zero" => :decimal_leading_zero
              },
              name,
              if(is_atom(style), do: style, else: :decimal)
            )

          marker(atom, ordinal) |> String.trim_trailing(".")
      end
    end
  end

  @spec representation(term(), integer(), map(), map()) :: {map(), String.t()}
  defp representation(name, ordinal, definitions, visited) do
    alias NativeElixirPdfUtilities.Validators.HtmlValidator
    HtmlValidator.check_render_resource(:max_depth, map_size(visited), :style)
    default = %{prefix: "", suffix: ". "}

    if Map.has_key?(visited, name) or not Map.has_key?(definitions, name) do
      {default, format(name, ordinal, %{})}
    else
      definition = Map.fetch!(definitions, name)
      visited = Map.put(visited, name, true)

      definition =
        case definition.system do
          {:extends, parent} ->
            inherited = inherited_definition(parent, definitions, visited)

            Map.merge(
              inherited,
              Map.take(
                definition,
                MapSet.to_list(
                  MapSet.difference(
                    definition.specified,
                    MapSet.new([:system, :symbols, :additive_symbols])
                  )
                )
              )
            )

          _ ->
            definition
        end

      in_range =
        case definition.range do
          :auto ->
            case definition.system do
              :fixed ->
                ordinal >= definition.start and
                  ordinal < definition.start + length(definition.symbols)

              system when system in [:numeric, :cyclic] ->
                true

              :additive ->
                ordinal >= 0

              _ ->
                ordinal >= 1
            end

          ranges ->
            Enum.any?(ranges, fn {a, b} ->
              (a == :negative_infinity or ordinal >= a) and (b == :infinity or ordinal <= b)
            end)
        end

      text = if in_range, do: generate(definition, ordinal), else: nil

      if text == nil do
        representation(definition.fallback, ordinal, definitions, visited)
      else
        {left, right} =
          if ordinal < 0 and definition.system != :cyclic, do: definition.negative, else: {"", ""}

        {width, pad} = definition.pad
        count = max(width - String.length(text) - String.length(left) - String.length(right), 0)

        HtmlValidator.reserve_render_resource(
          :max_output_bytes,
          count * byte_size(pad) + byte_size(text),
          :style
        )

        {definition, left <> String.duplicate(pad, count) <> text <> right}
      end
    end
  end

  @spec inherited_definition(term(), map(), map()) :: map()
  defp inherited_definition(name, definitions, visited) do
    if Map.has_key?(visited, name) or not Map.has_key?(definitions, name) do
      default = %{
        system: :numeric,
        start: 1,
        symbols: String.graphemes("0123456789"),
        additive_symbols: [],
        negative: {"-", ""},
        prefix: "",
        suffix: ". ",
        range: :auto,
        pad: {0, ""},
        fallback: "decimal"
      }

      case name do
        name when name in ~w(lower-alpha lower-latin upper-alpha upper-latin lower-greek) ->
          symbols =
            if name == "lower-greek",
              do: "αβγδεζηθικλμνξοπρστυφχψω",
              else: "abcdefghijklmnopqrstuvwxyz"

          symbols =
            if String.starts_with?(name, "upper"), do: String.upcase(symbols), else: symbols

          %{default | system: :alphabetic, symbols: String.graphemes(symbols)}

        name when name in ~w(lower-roman upper-roman) ->
          %{default | system: {:builtin, name}, range: [{1, 3999}]}

        _ ->
          default
      end
    else
      d = Map.fetch!(definitions, name)

      case d.system do
        {:extends, parent} ->
          Map.merge(
            inherited_definition(parent, definitions, Map.put(visited, name, true)),
            Map.take(
              d,
              MapSet.to_list(
                MapSet.difference(d.specified, MapSet.new([:system, :symbols, :additive_symbols]))
              )
            )
          )

        _ ->
          d
      end
    end
  end

  defp generate(d, ordinal) do
    n = abs(ordinal)

    case d.system do
      {:builtin, name} ->
        format(name, n, %{})

      :cyclic ->
        Enum.at(d.symbols, Integer.mod(ordinal - 1, length(d.symbols)))

      :fixed ->
        if ordinal >= d.start, do: Enum.at(d.symbols, ordinal - d.start)

      :symbolic ->
        if n > 0 do
          count = div(n - 1, length(d.symbols)) + 1
          symbol = Enum.at(d.symbols, rem(n - 1, length(d.symbols)))

          NativeElixirPdfUtilities.Validators.HtmlValidator.reserve_render_resource(
            :max_output_bytes,
            count * byte_size(symbol),
            :style
          )

          String.duplicate(symbol, count)
        end

      :numeric ->
        encode(n, d.symbols, :numeric)

      :alphabetic ->
        if n > 0, do: encode(n, d.symbols, :alphabetic)

      :additive ->
        {parts, rest} =
          Enum.reduce(d.additive_symbols, {[], n}, fn {weight, symbol}, {parts, rest} ->
            count = if weight == 0, do: if(n == 0, do: 1, else: 0), else: div(rest, weight)

            NativeElixirPdfUtilities.Validators.HtmlValidator.reserve_render_resource(
              :max_output_bytes,
              count * byte_size(symbol),
              :style
            )

            {[String.duplicate(symbol, count) | parts],
             if(weight == 0, do: rest, else: rem(rest, weight))}
          end)

        text = parts |> Enum.reverse() |> IO.iodata_to_binary()
        if rest == 0 and text != "", do: text
    end
  end

  defp encode(n, symbols, system) do
    base = length(symbols)

    if n == 0 do
      hd(symbols)
    else
      Stream.unfold(n, fn value ->
        if value == 0,
          do: nil,
          else:
            (
              value = if system == :alphabetic, do: value - 1, else: value
              {Enum.at(symbols, rem(value, base)), div(value, base)}
            )
      end)
      |> Enum.reverse()
      |> IO.iodata_to_binary()
    end
  end
end
