defmodule NativeElixirPdfUtilities.HtmlToPdf.PageGeometry do
  @moduledoc """
  Normalizes page sizes and four-sided page margins for rendering stages.

  Numeric custom page-size tuples retain the renderer's compatibility rule:
  values up to `20 x 20` are inches and larger values are PDF points. CSS
  page-size strings always retain their declared units.
  """

  @type page_size_name ::
          :a5 | :a4 | :a3 | :b5 | :b4 | :jis_b5 | :jis_b4 | :letter | :legal | :ledger
  @type orientation :: :portrait | :landscape
  @type page_size_input ::
          page_size_name()
          | :"jis-b5"
          | :"jis-b4"
          | {page_size_name() | :"jis-b5" | :"jis-b4", orientation()}
          | {orientation(), page_size_name() | :"jis-b5" | :"jis-b4"}
          | {number(), number()}
          | String.t()
  @type margins :: %{
          top: float(),
          right: float(),
          bottom: float(),
          left: float()
        }
  @type margin_input :: number() | String.t() | map()

  @page_sizes %{
    a5: {419.53, 595.28},
    a4: {595.28, 841.89},
    a3: {841.89, 1190.55},
    b5: {498.90, 708.66},
    b4: {708.66, 1000.63},
    jis_b5: {515.91, 728.50},
    jis_b4: {728.50, 1031.81},
    letter: {612.0, 792.0},
    legal: {612.0, 1008.0},
    ledger: {792.0, 1224.0}
  }
  @page_size_names Map.keys(@page_sizes)
  @margin_sides [:top, :right, :bottom, :left]
  @number_pattern "(?:\\d+(?:\\.\\d*)?|\\.\\d+)(?:e[+-]?\\d+)?"
  @absolute_length_regex Regex.compile!(
                           "^\\+?(#{@number_pattern})(cm|mm|q|in|pc|pt|px)$",
                           "iu"
                         )

  @doc """
  Resolves a renderer page-size value to a positive `{width, height}` point tuple.
  """
  @spec normalize_page_size(term()) :: {:ok, {float(), float()}} | {:error, :invalid_page_size}
  def normalize_page_size(page_size) do
    case page_size do
      page_size when page_size in @page_size_names ->
        {:ok, Map.fetch!(@page_sizes, page_size)}

      page_size when page_size in [:"jis-b5", :"jis-b4"] ->
        normalized_name = if page_size == :"jis-b5", do: :jis_b5, else: :jis_b4
        {:ok, Map.fetch!(@page_sizes, normalized_name)}

      {page_size, orientation}
      when orientation in [:portrait, :landscape] ->
        oriented_page_size(page_size, orientation)

      {orientation, page_size}
      when orientation in [:portrait, :landscape] ->
        oriented_page_size(page_size, orientation)

      {width, height} when is_number(width) and is_number(height) and width > 0 and height > 0 ->
        case width <= 20 and height <= 20 do
          true -> {:ok, {width * 72.0, height * 72.0}}
          false -> {:ok, {width * 1.0, height * 1.0}}
        end

      page_size when is_binary(page_size) ->
        normalize_css_page_size(page_size)

      _ ->
        {:error, :invalid_page_size}
    end
  end

  @doc """
  Resolves a nonnegative renderer margin into top, right, bottom, and left points.

  Strings use CSS shorthand order and accept one to four absolute lengths.
  Margin maps may contain any of the `:top`, `:right`, `:bottom`, and `:left`
  keys; omitted sides default to zero.
  """
  @spec normalize_margins(term()) :: {:ok, margins()} | {:error, :invalid_margin}
  def normalize_margins(margin) do
    case margin do
      margin when is_number(margin) and margin >= 0 ->
        value = margin * 1.0
        {:ok, %{top: value, right: value, bottom: value, left: value}}

      margin when is_binary(margin) ->
        margin
        |> String.trim()
        |> String.split(~r/\s+/u, trim: true)
        |> normalize_margin_values()

      margin when is_map(margin) ->
        case Enum.reject(Map.keys(margin), &(&1 in @margin_sides)) do
          [] ->
            Enum.reduce_while(@margin_sides, {:ok, %{}}, fn side, {:ok, normalized} ->
              case normalize_margin_length(Map.get(margin, side, 0)) do
                {:ok, value} -> {:cont, {:ok, Map.put(normalized, side, value)}}
                :error -> {:halt, {:error, :invalid_margin}}
              end
            end)

          _unknown ->
            {:error, :invalid_margin}
        end

      _ ->
        {:error, :invalid_margin}
    end
  end

  @doc """
  Returns a scalar for uniform margins and the four-sided map for asymmetric margins.
  """
  @spec compact_margins(margins()) :: float() | margins()
  def compact_margins(%{top: top, right: right, bottom: bottom, left: left} = margins) do
    case top == right and top == bottom and top == left do
      true -> top
      false -> margins
    end
  end

  @doc """
  Merges a later page-margin declaration over an earlier declaration.

  Scalar and shorthand values replace all sides. A partial side map replaces
  only its declared sides, allowing `@page` longhands to cascade across rules
  and stylesheets.
  """
  @spec merge_margin_defaults(term(), term()) :: term()
  def merge_margin_defaults(previous, override) do
    case override do
      override when is_map(override) ->
        previous_margins =
          case previous do
            previous when is_map(previous) ->
              previous

            nil ->
              %{}

            previous ->
              case normalize_margins(previous) do
                {:ok, margins} -> margins
                {:error, :invalid_margin} -> %{}
              end
          end

        Map.merge(previous_margins, override)

      override ->
        override
    end
  end

  @doc """
  Merges extracted page options while cascading partial margin longhands by side.
  """
  @spec merge_page_options(keyword(), keyword()) :: keyword()
  def merge_page_options(previous, override) do
    merged = Keyword.merge(previous, override)

    case Keyword.fetch(override, :margin) do
      {:ok, override_margin} ->
        margin =
          previous
          |> Keyword.get(:margin)
          |> merge_margin_defaults(override_margin)

        Keyword.put(merged, :margin, margin)

      :error ->
        merged
    end
  end

  @doc """
  Converts an applicable CSS page-margin value to a renderer margin value.

  Valid CSS values that require unresolved percentages, relative units,
  functions, keywords, `auto`, or negative geometry return `nil` and remain
  compatibility no-ops.
  """
  @spec css_margin_option(String.t()) :: number() | String.t() | margins() | nil
  def css_margin_option(value) do
    values =
      value
      |> String.trim()
      |> String.downcase()
      |> String.split(~r/\s+/u, trim: true)

    case values do
      ["0"] ->
        0.0

      [value] ->
        case normalize_margin_length(value) do
          {:ok, _margin} -> value
          :error -> nil
        end

      values when length(values) in 2..4 ->
        case normalize_margin_values(values) do
          {:ok, margins} -> margins
          {:error, :invalid_margin} -> nil
        end

      _ ->
        nil
    end
  end

  @doc """
  Converts an applicable CSS page-size descriptor to a renderer page-size value.

  Existing A4 and Letter return values are retained. Valid values that need
  unresolved CSS context return `nil` and remain compatibility no-ops.
  """
  @spec css_page_size_option(String.t()) :: page_size_input() | nil
  def css_page_size_option(value) do
    normalized = value |> String.trim() |> String.downcase()
    tokens = String.split(normalized, ~r/\s+/u, trim: true)

    case tokens do
      ["a4"] -> :a4
      ["a4", "portrait"] -> :a4
      ["portrait", "a4"] -> :a4
      ["a4", "landscape"] -> {841.89, 595.28}
      ["landscape", "a4"] -> {841.89, 595.28}
      ["letter"] -> :letter
      ["letter", "portrait"] -> :letter
      ["portrait", "letter"] -> :letter
      ["letter", "landscape"] -> {792.0, 612.0}
      ["landscape", "letter"] -> {792.0, 612.0}
      [orientation] when orientation in ["portrait", "landscape"] -> normalized
      [page_size] -> if page_size_name(page_size), do: normalized, else: nil
      [first, second] -> css_two_token_page_size(first, second, normalized)
      _ -> nil
    end
  end

  @doc """
  Returns whether a four-sided margin leaves positive printable page geometry.
  """
  @spec valid_printable_area?({number(), number()}, margins()) :: boolean()
  def valid_printable_area?(page_size, margins) do
    case {page_size, margins} do
      {{page_width, page_height}, %{top: top, right: right, bottom: bottom, left: left}}
      when is_number(page_width) and is_number(page_height) and is_number(top) and
             is_number(right) and is_number(bottom) and is_number(left) ->
        page_width > 0 and page_height > 0 and
          Enum.all?([top, right, bottom, left], &(&1 >= 0)) and
          left + right < page_width and top + bottom < page_height

      _ ->
        false
    end
  end

  defp normalize_css_page_size(page_size) do
    tokens =
      page_size
      |> String.trim()
      |> String.downcase()
      |> String.split(~r/\s+/u, trim: true)

    case tokens do
      [orientation] when orientation in ["portrait", "landscape"] ->
        oriented_page_size(:a4, String.to_existing_atom(orientation))

      [page_size] ->
        case page_size_name(page_size) do
          nil -> {:error, :invalid_page_size}
          name -> {:ok, Map.fetch!(@page_sizes, name)}
        end

      [first, second] ->
        cond do
          page_size_name(first) && second in ["portrait", "landscape"] ->
            oriented_page_size(page_size_name(first), String.to_existing_atom(second))

          first in ["portrait", "landscape"] && page_size_name(second) ->
            oriented_page_size(page_size_name(second), String.to_existing_atom(first))

          true ->
            with {:ok, width} <- css_page_length(first),
                 {:ok, height} <- css_page_length(second) do
              {:ok, {width, height}}
            else
              :error -> {:error, :invalid_page_size}
            end
        end

      _ ->
        {:error, :invalid_page_size}
    end
  end

  defp oriented_page_size(page_size, orientation) do
    normalized_name =
      case page_size do
        :"jis-b5" -> :jis_b5
        :"jis-b4" -> :jis_b4
        page_size -> page_size
      end

    case Map.fetch(@page_sizes, normalized_name) do
      {:ok, {width, height}} ->
        case orientation do
          :portrait -> {:ok, {min(width, height), max(width, height)}}
          :landscape -> {:ok, {max(width, height), min(width, height)}}
        end

      :error ->
        {:error, :invalid_page_size}
    end
  end

  defp css_two_token_page_size(first, second, normalized) do
    cond do
      page_size_name(first) && second in ["portrait", "landscape"] ->
        normalized

      first in ["portrait", "landscape"] && page_size_name(second) ->
        normalized

      match?({:ok, _length}, css_page_length(first)) and
          match?({:ok, _length}, css_page_length(second)) ->
        normalized

      true ->
        nil
    end
  end

  defp normalize_margin_values(values) do
    with true <- length(values) in 1..4,
         parsed <- Enum.map(values, &normalize_margin_length/1),
         true <- Enum.all?(parsed, &match?({:ok, _margin}, &1)) do
      parsed_values = Enum.map(parsed, fn {:ok, value} -> value end)

      margins =
        case parsed_values do
          [all] ->
            %{top: all, right: all, bottom: all, left: all}

          [vertical, horizontal] ->
            %{top: vertical, right: horizontal, bottom: vertical, left: horizontal}

          [top, horizontal, bottom] ->
            %{top: top, right: horizontal, bottom: bottom, left: horizontal}

          [top, right, bottom, left] ->
            %{top: top, right: right, bottom: bottom, left: left}
        end

      {:ok, margins}
    else
      _ -> {:error, :invalid_margin}
    end
  end

  defp normalize_margin_length(value) do
    case value do
      value when is_number(value) and value >= 0 ->
        {:ok, value * 1.0}

      value when is_binary(value) ->
        normalized = value |> String.trim() |> String.downcase()

        case normalized do
          "0" -> {:ok, 0.0}
          _ -> css_absolute_length(normalized)
        end

      _ ->
        :error
    end
  end

  defp css_page_length(value) do
    case value do
      "0" -> :error
      _ -> css_absolute_length(value)
    end
  end

  defp css_absolute_length(value) do
    case Regex.run(@absolute_length_regex, value) do
      [_, number, unit] ->
        number = if String.starts_with?(number, "."), do: "0" <> number, else: number
        {number, ""} = Float.parse(number)

        {:ok, number * points_per_unit(String.downcase(unit))}

      _ ->
        :error
    end
  end

  defp page_size_name(name) do
    case name do
      "a5" -> :a5
      "a4" -> :a4
      "a3" -> :a3
      "b5" -> :b5
      "b4" -> :b4
      "jis-b5" -> :jis_b5
      "jis-b4" -> :jis_b4
      "letter" -> :letter
      "legal" -> :legal
      "ledger" -> :ledger
      _ -> nil
    end
  end

  defp points_per_unit(unit) do
    case unit do
      "pt" -> 1.0
      "px" -> 0.75
      "mm" -> 72.0 / 25.4
      "q" -> 72.0 / 101.6
      "cm" -> 72.0 / 2.54
      "in" -> 72.0
      "pc" -> 12.0
    end
  end
end
