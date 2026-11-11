defmodule NativeElixirPdfUtilities.Validators.CssFontValidator do
  @moduledoc false

  alias NativeElixirPdfUtilities.Validators.HtmlValidator

  @widths %{
    "ultra-condensed" => 50,
    "extra-condensed" => 62.5,
    "condensed" => 75,
    "semi-condensed" => 87.5,
    "normal" => 100,
    "semi-expanded" => 112.5,
    "expanded" => 125,
    "extra-expanded" => 150,
    "ultra-expanded" => 200
  }

  @doc "Prepares a CSS font weight, including relative weights against the inherited value."
  @spec prepare_weight(term(), number()) :: {:ok, number()} | :error
  def prepare_weight(value, inherited \\ 400) do
    case value do
      weight when is_number(weight) and weight >= 1 and weight <= 1000 ->
        {:ok, if(weight == trunc(weight), do: trunc(weight), else: weight)}

      value when is_binary(value) ->
        case String.downcase(String.trim(value), :ascii) do
          "normal" ->
            {:ok, 400}

          "bold" ->
            {:ok, 700}

          "bolder" ->
            {:ok,
             cond do
               inherited < 350 -> 400
               inherited < 550 -> 700
               true -> 900
             end}

          "lighter" ->
            {:ok,
             cond do
               inherited < 550 -> 100
               inherited < 750 -> 400
               true -> 700
             end}

          number ->
            case HtmlValidator.parse_css_number(number) do
              {weight, ""} ->
                prepare_weight(weight)

              _ ->
                :error
            end
        end

      _ ->
        :error
    end
  end

  @doc "Prepares the CSS font width keywords or a positive percentage."
  @spec prepare_width(term()) :: {:ok, number()} | :error
  def prepare_width(value) do
    case value do
      value when is_binary(value) ->
        normalized = String.downcase(String.trim(value), :ascii)

        case Map.fetch(@widths, normalized) do
          {:ok, width} ->
            {:ok, width}

          :error ->
            case HtmlValidator.parse_css_number(normalized) do
              {width, "%"} when width > 0 -> {:ok, width}
              _ -> :error
            end
        end

      width when is_number(width) and width > 0 ->
        {:ok, width}

      _ ->
        :error
    end
  end

  @doc "Prepares a static style and optional oblique angle range, in degrees."
  @spec prepare_style(term(), boolean()) :: {:ok, map()} | :error
  def prepare_style(value, descriptor? \\ false) do
    case value do
      value when value in [:normal, :italic] ->
        {:ok, %{style: value}}

      value when is_binary(value) ->
        case value |> String.downcase(:ascii) |> HtmlValidator.split_css_tokens() do
          ["normal"] ->
            {:ok, %{style: :normal}}

          ["italic"] ->
            {:ok, %{style: :italic}}

          ["oblique" | angles] when length(angles) <= 2 ->
            angles = Enum.map(angles, &prepare_angle/1)

            case angles do
              [] ->
                {:ok, %{style: :italic, oblique: {14, 14}}}

              [{:ok, angle}] ->
                {:ok, %{style: :italic, oblique: {angle, angle}}}

              [{:ok, first}, {:ok, last}] when descriptor? and first <= last ->
                {:ok, %{style: :italic, oblique: {first, last}}}

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

  defp prepare_angle(value) do
    case HtmlValidator.parse_css_number(value) do
      {number, unit} when unit in ["deg", "grad", "rad", "turn"] ->
        angle =
          case unit do
            "deg" -> number
            "grad" -> number * 0.9
            "rad" -> number * 180 / :math.pi()
            "turn" -> number * 360
          end

        if angle >= -90 and angle <= 90, do: {:ok, angle}, else: :error

      _ ->
        :error
    end
  end

  @doc "Prepares the style, weight and CSS3 width slots before a font shorthand's size."
  @spec prepare_shorthand_prefix([String.t()]) :: {:ok, map()} | :error
  def prepare_shorthand_prefix(tokens) do
    {slots, properties, valid?} =
      Enum.reduce(tokens, {0, %{}, true}, fn token, {slots, properties, valid?} ->
        angle = Map.get(properties, :awaiting_angle, false)

        cond do
          angle and match?({:ok, _}, prepare_angle(token)) ->
            {:ok, value} = prepare_angle(token)

            {slots,
             properties
             |> Map.put(:style, "oblique " <> token)
             |> Map.put(:oblique, value)
             |> Map.delete(:awaiting_angle), valid?}

          token == "normal" ->
            {slots + 1, Map.delete(properties, :awaiting_angle), valid?}

          token == "small-caps" and not Map.has_key?(properties, :caps) ->
            {slots + 1, properties |> Map.put(:caps, token) |> Map.delete(:awaiting_angle),
             valid?}

          token in ["italic", "oblique"] and not Map.has_key?(properties, :style) ->
            {slots + 1,
             properties |> Map.put(:style, token) |> Map.put(:awaiting_angle, token == "oblique"),
             valid?}

          Map.has_key?(@widths, token) and not Map.has_key?(properties, :width) ->
            {slots + 1, properties |> Map.put(:width, token) |> Map.delete(:awaiting_angle),
             valid?}

          not Map.has_key?(properties, :weight) and match?({:ok, _}, prepare_weight(token)) ->
            {slots + 1, properties |> Map.put(:weight, token) |> Map.delete(:awaiting_angle),
             valid?}

          true ->
            {slots + 1, properties, false}
        end
      end)

    if valid? and slots <= 4 do
      {:ok,
       %{
         style: Map.get(properties, :style, "normal"),
         weight: Map.get(properties, :weight, "normal"),
         width: Map.get(properties, :width, "normal"),
         caps: Map.get(properties, :caps, "normal")
       }}
    else
      :error
    end
  end

  @doc "Prepares an ascending descriptor range using the canonical scalar parser."
  @spec prepare_range(term(), (term() -> {:ok, number()} | :error)) ::
          {:ok, {number(), number()}} | :error
  def prepare_range(value, parser) do
    values = if is_binary(value), do: HtmlValidator.split_css_tokens(value), else: [value]

    case Enum.map(values, parser) do
      [{:ok, value}] -> {:ok, {value, value}}
      [{:ok, first}, {:ok, last}] when first <= last -> {:ok, {first, last}}
      _ -> :error
    end
  end

  @doc "Prepares the shared face selection metadata used by CSS and configured sources."
  @spec prepare_face_properties(map()) :: {:ok, map()} | :error
  def prepare_face_properties(face) do
    weight = Map.get(face, :weight_range, Map.get(face, :weight, 400))
    width = Map.get(face, :width_range, Map.get(face, :width, 100))
    weight = if is_tuple(weight), do: Tuple.to_list(weight), else: [weight]
    width = if is_tuple(width), do: Tuple.to_list(width), else: [width]

    with {:ok, {low, high}} <- face_range(weight, &prepare_absolute_weight/1),
         {:ok, {narrow, wide}} <- face_range(width, &prepare_width/1),
         {:ok, style} <- prepare_style(Map.get(face, :style, :normal), true),
         {:ok, oblique} <- face_oblique(Map.get(face, :oblique), style),
         {:ok, ranges} <- face_unicode_range(Map.get(face, :unicode_range)) do
      properties = Map.merge(%{weight: low}, style)

      properties =
        if low == high, do: properties, else: Map.put(properties, :weight_range, {low, high})

      properties =
        if narrow == 100 and wide == 100,
          do: properties,
          else: Map.put(properties, :width, narrow)

      properties =
        if narrow == wide, do: properties, else: Map.put(properties, :width_range, {narrow, wide})

      properties =
        if is_nil(oblique), do: properties, else: Map.put(properties, :oblique, oblique)

      {:ok, if(is_nil(ranges), do: properties, else: Map.put(properties, :unicode_range, ranges))}
    end
  end

  defp face_range(values, parser) do
    case values do
      [value] ->
        prepare_range(value, parser)

      [first, last] ->
        with {:ok, first} <- parser.(first),
             {:ok, last} <- parser.(last),
             true <- first <= last do
          {:ok, {first, last}}
        else
          _ -> :error
        end

      _ ->
        :error
    end
  end

  @doc "Prepares a descriptor weight, excluding relative property keywords."
  @spec prepare_absolute_weight(term()) :: {:ok, number()} | :error
  def prepare_absolute_weight(value) do
    if is_binary(value) and String.downcase(String.trim(value), :ascii) in ["bolder", "lighter"],
      do: :error,
      else: prepare_weight(value)
  end

  defp face_oblique(value, style) do
    case value do
      nil ->
        {:ok, Map.get(style, :oblique)}

      {first, last}
      when is_number(first) and is_number(last) and first >= -90 and last <= 90 and first <= last ->
        if style.style == :italic, do: {:ok, {first, last}}, else: :error

      _ ->
        :error
    end
  end

  defp face_unicode_range(value) do
    case value do
      nil ->
        {:ok, nil}

      value when is_binary(value) ->
        prepare_unicode_range(value)

      ranges when is_list(ranges) and ranges != [] ->
        maximum = NativeElixirPdfUtilities.Limits.get(:max_font_cmap_work)

        with {:ok, _} <-
               NativeElixirPdfUtilities.Validators.CollectionValidator.bounded_length(
                 ranges,
                 maximum
               ) do
          if Enum.all?(ranges, fn range ->
               case range do
                 {first, last} when is_integer(first) and is_integer(last) ->
                   first >= 0 and first <= last and last <= 0x10FFFF

                 _ ->
                   false
               end
             end), do: {:ok, Enum.sort(ranges)}, else: :error
        else
          :limit_exceeded ->
            NativeElixirPdfUtilities.Validators.FontValidator.check(
              :max_font_cmap_work,
              maximum + 1
            )

          :invalid ->
            :error
        end

      _ ->
        :error
    end
  end

  @doc "Prepares Unicode ranges without expanding their codepoints."
  @spec prepare_unicode_range(term()) :: {:ok, [{non_neg_integer(), non_neg_integer()}]} | :error
  def prepare_unicode_range(value) do
    case value do
      value when is_binary(value) ->
        value
        |> String.split(",")
        |> Enum.reduce_while({:ok, []}, fn token, {:ok, ranges} ->
          HtmlValidator.reserve_render_resource(:max_css_work, 1, :font)
          token = token |> String.trim() |> String.upcase(:ascii)

          parsed =
            case Regex.run(~r/\AU\+([0-9A-F]{1,6})(?:-([0-9A-F]{1,6}))?\z/, token) do
              [_, first] ->
                {:ok, String.to_integer(first, 16), String.to_integer(first, 16)}

              [_, first, last] ->
                {:ok, String.to_integer(first, 16), String.to_integer(last, 16)}

              _ ->
                case Regex.run(~r/\AU\+([0-9A-F]*)(\?+)\z/, token) do
                  [_, prefix, suffix] when byte_size(prefix) + byte_size(suffix) <= 6 ->
                    {:ok,
                     String.to_integer(prefix <> String.duplicate("0", byte_size(suffix)), 16),
                     String.to_integer(prefix <> String.duplicate("F", byte_size(suffix)), 16)}

                  _ ->
                    :error
                end
            end

          case parsed do
            {:ok, first, last} when first <= last and first <= 0x10FFFF ->
              {:cont, {:ok, [{first, min(last, 0x10FFFF)} | ranges]}}

            _ ->
              {:halt, :error}
          end
        end)
        |> case do
          {:ok, ranges} -> {:ok, Enum.sort(ranges)}
          error -> error
        end

      _ ->
        :error
    end
  end
end
