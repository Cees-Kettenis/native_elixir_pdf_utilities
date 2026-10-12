defmodule NativeElixirPdfUtilities.Validators.PrintMediaValidator do
  @moduledoc """
  Print environment and media predicates shared by responsive images and future
  stylesheet selection. Viewport lengths are CSS pixels, independent of raster
  resolution. Defaults use the effective page area and a selection density of 1.
  This module does not change the existing stylesheet media cascade.
  """

  alias NativeElixirPdfUtilities.Diagnostics
  alias NativeElixirPdfUtilities.HtmlToPdf.PageGeometry
  alias NativeElixirPdfUtilities.Validators.HtmlValidator

  @forward_range ~r/\A(width|height|resolution|aspect-ratio)\s*(<=|>=|<|>|=)\s*(.+)\z/s
  @reverse_range ~r/\A(.+?)\s*(<=|>=|<|>|=)\s*(width|height|resolution|aspect-ratio)(?:\s*(<=|>=|<|>)\s*(.+))?\z/s

  @type context :: %{width: float(), height: float(), density: float(), type: :print}

  @doc "Validates explicit print viewport and deterministic image density options."
  @spec validate_options(keyword()) :: :ok | {:error, {atom(), map()}}
  def validate_options(opts) do
    viewport = Keyword.get(opts, :print_viewport)
    density = Keyword.get(opts, :image_selection_density, 1)

    valid_viewport? =
      case viewport do
        nil -> true
        {width, height} -> positive_number?(width) and positive_number?(height)
        _ -> false
      end

    if valid_viewport? and positive_number?(density) do
      :ok
    else
      Diagnostics.error(
        :options,
        :invalid_options,
        "print_viewport must be a positive {width, height} in CSS pixels and image_selection_density must be positive",
        module: __MODULE__,
        source: "print_viewport/image_selection_density"
      )
    end
  end

  @doc "Returns a validated print context from effective renderer options."
  @spec context(keyword()) :: {:ok, context()} | {:error, {atom(), map()}}
  def context(opts) do
    with :ok <- validate_options(opts) do
      viewport =
        case Keyword.get(opts, :print_viewport) do
          nil ->
            with {:ok, {width, height}} <-
                   PageGeometry.normalize_page_size(Keyword.get(opts, :page_size, :a4)),
                 {:ok, margins} <- PageGeometry.normalize_margins(Keyword.get(opts, :margin, 0)),
                 true <-
                   width > margins.left + margins.right and height > margins.top + margins.bottom do
              {:ok,
               {(width - margins.left - margins.right) / 0.75,
                (height - margins.top - margins.bottom) / 0.75}}
            else
              _ ->
                Diagnostics.error(
                  :options,
                  :invalid_options,
                  "responsive image selection requires a positive printable page area",
                  module: __MODULE__
                )
            end

          viewport ->
            {:ok, viewport}
        end

      with {:ok, {width, height}} <- viewport do
        {:ok,
         %{
           width: width * 1.0,
           height: height * 1.0,
           density: Keyword.get(opts, :image_selection_density, 1) * 1.0,
           type: :print
         }}
      end
    end
  end

  @doc "Evaluates a media query list against a validated print context. Unknown features do not match."
  @spec matches?(String.t(), context()) :: boolean()
  def matches?(query, context) do
    query =
      query
      |> NativeElixirPdfUtilities.HtmlToPdf.CssParser.strip_comments()
      |> HtmlValidator.trim_css_whitespace()
      |> String.downcase(:ascii)

    query == "" or Enum.any?(HtmlValidator.split_css_comma(query), &condition(&1, context, 0))
  end

  @doc "Resolves a nonnegative CSS source-size length to CSS pixels, or returns :error."
  @spec source_length(String.t(), context()) :: {:ok, float()} | :error
  def source_length(value, context) do
    value = String.downcase(HtmlValidator.trim_css_whitespace(value), :ascii)

    if Regex.match?(
         ~r/(?:[0-9](?:ex|ch|cap|ic|lh|rlh)\b)|\b(?:sin|cos|tan|asin|acos|atan|atan2|pow|sqrt|hypot|log|exp|abs|sign|round|mod|rem)\(/,
         value
       ) do
      throw(
        {:render_resource_limit,
         Diagnostics.error(
           :style,
           :unsupported_html,
           "source-size font metrics or advanced math are not implemented; use absolute lengths, viewport units, em/rem, calc/min/max/clamp",
           module: __MODULE__,
           source: value
         )}
      )
    end

    parsed =
      try do
        if length(top_level_terms(value, true)) == 1,
          do: length_value(value, context, 0),
          else: :error
      rescue
        ArithmeticError ->
          throw(
            {:render_resource_limit,
             Diagnostics.error(
               :style,
               :resource_limit_exceeded,
               "source-size arithmetic is outside the finite numeric range",
               module: __MODULE__,
               source: value
             )}
          )
      end

    case parsed do
      {:ok, value} when value >= 0 ->
        HtmlValidator.check_render_resource(:max_css_numeric_magnitude, value, :style)
        HtmlValidator.normalize_geometry_number(value)

      _ ->
        :error
    end
  end

  defp positive_number?(value) do
    case HtmlValidator.normalize_geometry_number(value) do
      {:ok, value} -> value > 0
      _ -> false
    end
  end

  defp condition(query, context, depth) do
    HtmlValidator.check_render_resource(:max_media_query_depth, depth, :style)
    query = HtmlValidator.trim_css_whitespace(query)
    terms = top_level_terms(query)

    cond do
      String.starts_with?(query, "not ") and
          not String.starts_with?(String.slice(query, 4..-1//1), "(") ->
        rest = String.slice(query, 4..-1//1)

        Regex.match?(~r/\A[-a-z]+(?:\s+and\s+\(.*\))?\z/s, rest) and
          not condition(rest, context, depth + 1)

      length(terms) > 1 and "or" in terms ->
        terms
        |> Enum.chunk_by(&(&1 == "or"))
        |> Enum.reject(&(&1 == ["or"]))
        |> Enum.any?(&condition(Enum.join(&1, " "), context, depth + 1))

      length(terms) > 1 and "and" in terms ->
        terms
        |> Enum.chunk_by(&(&1 == "and"))
        |> Enum.reject(&(&1 == ["and"]))
        |> Enum.all?(&condition(Enum.join(&1, " "), context, depth + 1))

      String.starts_with?(query, "not ") ->
        not condition(String.slice(query, 4..-1//1), context, depth + 1)

      String.starts_with?(query, "only ") ->
        rest = String.slice(query, 5..-1//1)

        Regex.match?(~r/\A[-a-z]+(?:\s+and\s+\(.*\))?\z/s, rest) and
          condition(rest, context, depth + 1)

      query in ~w(all print) ->
        true

      query in ~w(screen speech) ->
        false

      String.starts_with?(query, "(") and String.ends_with?(query, ")") ->
        inside = String.slice(query, 1, String.length(query) - 2)

        if String.starts_with?(inside, "(") or String.starts_with?(inside, "not "),
          do: condition(inside, context, depth + 1),
          else: feature(inside, context)

      true ->
        false
    end
  end

  # Spaces inside parenthesized components are retained for media conditions and math.
  defp top_level_terms(value, math? \\ false) do
    {terms, current, _depth} =
      value
      |> String.codepoints()
      |> Enum.reduce({[], [], 0}, fn char, {terms, current, depth} ->
        cond do
          char == "(" ->
            {terms, [char | current], depth + 1}

          char == ")" ->
            {terms, [char | current], depth - 1}

          math? and char in ["*", "/"] and depth == 0 ->
            terms =
              if current == [],
                do: terms,
                else: [current |> Enum.reverse() |> Enum.join() | terms]

            {[char | terms], [], depth}

          char in [" ", "\t", "\n", "\r", "\f"] and depth == 0 ->
            if current == [],
              do: {terms, [], depth},
              else: {[current |> Enum.reverse() |> Enum.join() | terms], [], depth}

          true ->
            {terms, [char | current], depth}
        end
      end)

    terms = if current == [], do: terms, else: [current |> Enum.reverse() |> Enum.join() | terms]
    Enum.reverse(terms)
  end

  defp feature(value, context) do
    value = HtmlValidator.trim_css_whitespace(value)

    case Regex.run(~r/\A([a-z-]+)\s*:\s*(.+)\z/s, value) do
      [_, "orientation", orientation] ->
        orientation == if(context.width > context.height, do: "landscape", else: "portrait")

      [_, name, expected] ->
        {comparison, name} =
          cond do
            String.starts_with?(name, "min-") -> {:min, String.slice(name, 4..-1//1)}
            String.starts_with?(name, "max-") -> {:max, String.slice(name, 4..-1//1)}
            true -> {:equal, name}
          end

        with {:ok, actual, expected} <- feature_values(name, expected, context) do
          compare(actual, expected, comparison)
        else
          _ -> false
        end

      _ ->
        range_feature(value, context)
    end
  end

  defp range_feature(value, context) do
    case Regex.run(@forward_range, value) do
      [_, name, operator, expected] ->
        with {:ok, actual, expected} <- feature_values(name, expected, context) do
          compare(actual, expected, operator)
        else
          _ -> false
        end

      _ ->
        case Regex.run(@reverse_range, value) do
          [_, expected, operator, name, second_operator, second_expected] ->
            with true <-
                   (operator in ["<", "<="] and second_operator in ["<", "<="]) or
                     (operator in [">", ">="] and second_operator in [">", ">="]),
                 {:ok, actual, first} <- feature_values(name, expected, context),
                 {:ok, _, second} <- feature_values(name, second_expected, context) do
              compare(first, actual, operator) and compare(actual, second, second_operator)
            else
              _ -> false
            end

          [_, expected, operator, name] ->
            with {:ok, actual, expected} <- feature_values(name, expected, context) do
              compare(expected, actual, operator)
            else
              _ -> false
            end

          _ ->
            value in ~w(width height resolution aspect-ratio color)
        end
    end
  end

  defp feature_values(name, expected, context) do
    case name do
      name when name in ~w(width height) ->
        with {:ok, expected} <- source_length(expected, context),
             do: {:ok, Map.fetch!(context, String.to_existing_atom(name)), expected}

      "resolution" ->
        case HtmlValidator.parse_css_number(expected) do
          {value, unit} when value >= 0 and unit in ~w(dppx dpi dpcm x) ->
            scale =
              case unit do
                "dpi" -> 1 / 96
                "dpcm" -> 2.54 / 96
                _ -> 1
              end

            {:ok, context.density, value * scale}

          _ ->
            :error
        end

      "aspect-ratio" ->
        case String.split(expected, "/")
             |> Enum.map(&HtmlValidator.parse_css_number(String.trim(&1))) do
          [{numerator, ""}, {denominator, ""}] when numerator >= 0 and denominator > 0 ->
            {:ok, context.width / context.height, numerator / denominator}

          _ ->
            :error
        end

      _ ->
        :error
    end
  end

  defp compare(actual, expected, operator) do
    case operator do
      op when op in [:min, ">="] -> actual >= expected
      op when op in [:max, "<="] -> actual <= expected
      ">" -> actual > expected
      "<" -> actual < expected
      _ -> actual == expected
    end
  end

  defp length_value(value, context, depth) do
    case math_value(value, context, depth) do
      {:ok, value, :length} -> {:ok, value}
      {:ok, value, :number} when value == 0 -> {:ok, value}
      _ -> :error
    end
  end

  defp math_value(value, context, depth) do
    HtmlValidator.check_render_resource(:max_media_query_depth, depth, :style)
    terms = top_level_terms(value, true)

    additive =
      terms |> Enum.with_index() |> Enum.filter(fn {t, _} -> t in ["+", "-"] end) |> List.last()

    multiplicative =
      terms |> Enum.with_index() |> Enum.filter(fn {t, _} -> t in ["*", "/"] end) |> List.last()

    case additive || multiplicative do
      {operator, index} ->
        with true <- index > 0 and index < length(terms) - 1,
             {:ok, left, left_kind} <-
               math_value(terms |> Enum.take(index) |> Enum.join(" "), context, depth + 1),
             {:ok, right, right_kind} <-
               math_value(terms |> Enum.drop(index + 1) |> Enum.join(" "), context, depth + 1) do
          case {operator, left_kind, right_kind} do
            {op, kind, kind} when op in ["+", "-"] ->
              {:ok, if(op == "+", do: left + right, else: left - right), kind}

            {"*", :number, kind} ->
              {:ok, left * right, kind}

            {"*", kind, :number} ->
              {:ok, left * right, kind}

            {"/", kind, :number} when right != 0 ->
              {:ok, left / right, kind}

            _ ->
              :error
          end
        else
          _ -> :error
        end

      nil ->
        case Regex.run(~r/\A(calc|min|max|clamp)\((.*)\)\z/s, value) do
          [_, function, arguments] ->
            parsed =
              arguments
              |> HtmlValidator.split_css_comma()
              |> Enum.map(&math_value(&1, context, depth + 1))

            kinds =
              Enum.map(parsed, fn item -> if match?({:ok, _, _}, item), do: elem(item, 2) end)

            if parsed != [] and Enum.uniq(kinds) in [[:length], [:number]] do
              numbers = Enum.map(parsed, &elem(&1, 1))

              result =
                case {function, numbers} do
                  {"min", [_ | _]} -> {:ok, Enum.min(numbers)}
                  {"max", [_ | _]} -> {:ok, Enum.max(numbers)}
                  {"clamp", [low, preferred, high]} -> {:ok, max(low, min(preferred, high))}
                  {"calc", [number]} -> {:ok, number}
                  _ -> :error
                end

              case result do
                {:ok, number} -> {:ok, number, hd(kinds)}
                _ -> :error
              end
            else
              :error
            end

          _ ->
            if String.starts_with?(value, "(") and String.ends_with?(value, ")") do
              math_value(String.slice(value, 1, String.length(value) - 2), context, depth + 1)
            else
              case HtmlValidator.parse_css_number(value) do
                {number, unit} ->
                  scale =
                    case unit do
                      unit when unit in ~w(vw svw lvw dvw vi svi lvi dvi) ->
                        context.width / 100

                      unit when unit in ~w(vh svh lvh dvh vb svb lvb dvb) ->
                        context.height / 100

                      unit when unit in ~w(vmin svmin lvmin dvmin) ->
                        min(context.width, context.height) / 100

                      unit when unit in ~w(vmax svmax lvmax dvmax) ->
                        max(context.width, context.height) / 100

                      unit when unit in ~w(em rem) ->
                        16.0

                      "px" ->
                        1.0

                      "pt" ->
                        1 / 0.75

                      "pc" ->
                        16.0

                      "in" ->
                        96.0

                      "cm" ->
                        96 / 2.54

                      "mm" ->
                        96 / 25.4

                      "q" ->
                        96 / 101.6

                      "" ->
                        1.0

                      _ ->
                        nil
                    end

                  with true <- is_number(scale),
                       {:ok, value} <- HtmlValidator.normalize_geometry_number(number * scale) do
                    {:ok, value, if(unit == "", do: :number, else: :length)}
                  else
                    _ -> :error
                  end

                _ ->
                  :error
              end
            end
        end
    end
  end
end
