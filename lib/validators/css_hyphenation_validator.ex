defmodule NativeElixirPdfUtilities.Validators.CssHyphenationValidator do
  @moduledoc false
  alias NativeElixirPdfUtilities.Diagnostics
  alias NativeElixirPdfUtilities.Validators.{CssLengthValidator, CssMathValidator, HtmlValidator}

  @properties ~w(hyphens hyphenate-character hyphenate-limit-chars hyphenate-limit-lines
                 hyphenate-limit-last hyphenate-limit-zone)
  @string Regex.compile!("\\A(?:" <> HtmlValidator.css_string_pattern() <> ")\\z", "u")

  @doc "Returns the static hyphenation controls owned by this validator."
  @spec properties() :: [binary()]
  def properties, do: @properties

  @doc "Returns initial computed values, retaining automatic character-limit keywords until layout."
  @spec defaults() :: map()
  def defaults,
    do: %{
      hyphens: :manual,
      hyphenate_character: :auto,
      hyphenate_limit_chars: {:auto, :auto, :auto},
      hyphenate_limit_lines: :no_limit,
      hyphenate_limit_last: :none,
      hyphenate_limit_zone: 0.0
    }

  @doc "Prepares a hyphenation declaration through the shared CSS numeric, string and length grammars."
  @spec prepare(map(), binary(), binary()) :: {:ok, map()} | :error | {:error, {atom(), map()}}
  def prepare(style, property, value) do
    normalized = String.trim(value) |> String.downcase(:ascii)

    result =
      case property do
        "hyphens" ->
          case normalized do
            "none" -> {:ok, :hyphens, :none}
            "manual" -> {:ok, :hyphens, :manual}
            "auto" -> {:ok, :hyphens, :auto}
            _ -> :error
          end

        "hyphenate-character" ->
          cond do
            normalized == "auto" ->
              {:ok, :hyphenate_character, :auto}

            Regex.match?(@string, String.trim(value)) ->
              {:ok, :hyphenate_character, HtmlValidator.decode_css_string(String.trim(value))}

            true ->
              :error
          end

        "hyphenate-limit-chars" ->
          tokens = HtmlValidator.split_css_tokens(normalized)

          if length(tokens) in 1..3 do
            Enum.reduce_while(tokens, {:ok, []}, fn token, {:ok, values} ->
              result = if token == "auto", do: {:ok, :auto}, else: integer(token, style)

              case result do
                {:ok, value} -> {:cont, {:ok, [value | values]}}
                error -> {:halt, error}
              end
            end)
            |> case do
              {:ok, reversed} ->
                [word | rest] = Enum.reverse(reversed)
                before = Enum.at(rest, 0, :auto)
                after_count = Enum.at(rest, 1, before)

                {:ok, :hyphenate_limit_chars, {word, before, after_count}}

              error ->
                error
            end
          else
            :error
          end

        "hyphenate-limit-lines" ->
          if normalized == "no-limit",
            do: {:ok, :hyphenate_limit_lines, :no_limit},
            else:
              with(
                {:ok, value} <- integer(normalized, style),
                do: {:ok, :hyphenate_limit_lines, value}
              )

        "hyphenate-limit-last" ->
          case normalized do
            "none" -> {:ok, :hyphenate_limit_last, :none}
            "always" -> {:ok, :hyphenate_limit_last, :always}
            "column" -> {:ok, :hyphenate_limit_last, :column}
            "page" -> {:ok, :hyphenate_limit_last, :page}
            "spread" -> {:ok, :hyphenate_limit_last, :spread}
            _ -> :error
          end

        "hyphenate-limit-zone" ->
          with {:ok, zone} <- CssLengthValidator.prepare(value, :padding),
               do: {:ok, :hyphenate_limit_zone, zone}

        _ ->
          :error
      end

    case result do
      {:ok, field, value} -> {:ok, Map.put(style, field, value)}
      error -> error
    end
  end

  @doc "Validates computed hyphenation controls on caller-supplied styled trees."
  @spec validate_style(map(), String.t()) :: :ok | {:error, {atom(), map()}}
  def validate_style(style, source) do
    valid? =
      case style do
        style
        when is_map(style) and not is_map_key(style, :hyphens) and
               not is_map_key(style, :hyphenate_character) and
               not is_map_key(style, :hyphenate_limit_chars) and
               not is_map_key(style, :hyphenate_limit_lines) and
               not is_map_key(style, :hyphenate_limit_last) and
               not is_map_key(style, :hyphenate_limit_zone) ->
          true

        %{
          hyphens: :manual,
          hyphenate_character: :auto,
          hyphenate_limit_chars: {:auto, :auto, :auto},
          hyphenate_limit_lines: :no_limit,
          hyphenate_limit_last: :none,
          hyphenate_limit_zone: zone
        }
        when zone == 0 ->
          true

        style when is_map(style) ->
          limits? =
            case Map.get(style, :hyphenate_limit_chars, {:auto, :auto, :auto}) do
              {word, before, after_count} ->
                Enum.all?(
                  [word, before, after_count],
                  &(&1 == :auto or (is_integer(&1) and &1 >= 0))
                )

              _ ->
                false
            end

          character = Map.get(style, :hyphenate_character, :auto)
          lines = Map.get(style, :hyphenate_limit_lines, :no_limit)

          Map.get(style, :hyphens, :manual) in [:none, :manual, :auto] and
            (character == :auto or (is_binary(character) and String.valid?(character))) and
            limits? and
            (lines == :no_limit or (is_integer(lines) and lines >= 0)) and
            Map.get(style, :hyphenate_limit_last, :none) in [
              :none,
              :always,
              :column,
              :page,
              :spread
            ] and
            CssLengthValidator.valid_edge?(Map.get(style, :hyphenate_limit_zone, 0.0), :padding)

        _ ->
          false
      end

    if valid? do
      :ok
    else
      Diagnostics.error(
        :layout,
        :invalid_layout,
        "styled text requires valid hyphenation controls",
        operation: :layout,
        module: __MODULE__,
        source: source
      )
    end
  end

  @doc "Resolves automatic character limits to the CSS suggested used values for this layout."
  @spec used_character_limits(map()) :: {non_neg_integer(), non_neg_integer(), non_neg_integer()}
  def used_character_limits(style) do
    {word, before, after_count} = Map.get(style, :hyphenate_limit_chars, {:auto, :auto, :auto})

    {if(word == :auto, do: 5, else: word), if(before == :auto, do: 2, else: before),
     if(after_count == :auto, do: 2, else: after_count)}
  end

  @doc "Filters admitted dictionary points using CSS character counts, excluding nonspacing marks and punctuation."
  @spec allowed_points(map(), [non_neg_integer()], map()) :: [non_neg_integer()]
  def allowed_points(word, points, style) do
    HtmlValidator.reserve_render_resource(:max_work, byte_size(word.text), :layout)
    {minimum, before, after_count} = used_character_limits(style)

    {counts, _, total} =
      for(<<codepoint::utf8 <- word.text>>, do: codepoint)
      |> Enum.reduce({%{0 => 0}, 0, 0}, fn codepoint, {counts, offset, total} ->
        category =
          elem(NativeElixirPdfUtilities.HtmlToPdf.UnicodeTextData.properties(codepoint), 2)

        offset = offset + byte_size(<<codepoint::utf8>>)
        total = total + if(category in [:Mn, :Pc, :Pd, :Pe, :Pf, :Pi, :Po, :Ps], do: 0, else: 1)
        {Map.put(counts, offset, total), offset, total}
      end)

    if total < minimum do
      []
    else
      Enum.filter(points, fn point ->
        count = Map.fetch!(counts, point)
        count >= before and total - count >= after_count
      end)
    end
  end

  @doc "Selects introduced line-end opportunities forbidden by admitted consecutive-line, last-line and zone controls."
  @spec rejected_breaks([map() | nil], number()) :: MapSet.t()
  def rejected_breaks(lines, width) do
    last_full_line = length(lines) - 2

    {rejected, _} =
      lines
      |> Enum.with_index()
      |> Enum.reduce({MapSet.new(), 0}, fn {line, index}, {rejected, consecutive} ->
        case line do
          nil ->
            {rejected, 0}

          %{point: point, style: style, natural_width: natural_width} ->
            HtmlValidator.reserve_render_resource(:max_work, 1, :layout)
            limit = Map.get(style, :hyphenate_limit_lines, :no_limit)

            zone =
              case CssLengthValidator.resolve(
                     Map.get(style, :hyphenate_limit_zone, 0.0),
                     width,
                     style
                   ) do
                {:ok, {:percent, ratio}} -> ratio * width
                {:ok, value} -> value
                error -> throw({:render_resource_limit, error})
              end

            forbidden? =
              (limit != :no_limit and consecutive >= limit) or
                (Map.get(style, :hyphenate_limit_last, :none) == :always and
                   index == last_full_line) or
                (zone > 0 and width - natural_width <= zone)

            if forbidden?,
              do: {MapSet.put(rejected, point), 0},
              else: {rejected, consecutive + 1}
        end
      end)

    rejected
  end

  @doc "Rejects introduced opportunities whose required unbroken line chain exceeds a complete fragmentainer."
  @spec rejected_fragmentainer_breaks([map()], number()) :: MapSet.t()
  def rejected_fragmentainer_breaks(lines, capacity) do
    {rejected, _, _} =
      Enum.reduce(lines, {MapSet.new(), [], 0.0}, fn line, {rejected, points, height} ->
        HtmlValidator.reserve_render_resource(:max_work, 1, :layout)
        height = height + line.height

        {rejected, points, height} =
          if points != [] and height > capacity,
            do: {Enum.reduce(points, rejected, &MapSet.put(&2, &1)), [], line.height},
            else: {rejected, points, height}

        case line.break do
          %{point: point, style: style} ->
            if Map.get(style, :hyphenate_limit_last, :none) != :none,
              do: {rejected, [point | points], height},
              else: {rejected, [], 0.0}

          nil ->
            {rejected, [], 0.0}
        end
      end)

    rejected
  end

  @doc "Determines whether an admitted last-line control forbids a hyphen at this fragment boundary."
  @spec forbids_fragment_end?(atom(), :column | :page, pos_integer()) :: boolean()
  def forbids_fragment_end?(control, boundary, page_number) do
    case control do
      :none -> false
      :always -> true
      :column -> true
      :page -> boundary == :page
      :spread -> boundary == :page and rem(page_number, 2) == 1
    end
  end

  defp integer(token, style) do
    with {:ok, value} <- CssMathValidator.prepare_scalar(token, :integer, 0),
         do: CssMathValidator.resolve_scalar(value, style)
  end
end
