defmodule NativeElixirPdfUtilities.HtmlToPdf.Font.Selection do
  @moduledoc false

  @doc "Orders physical faces by CSS width, style and directional weight matching."
  @spec rank(map(), number(), :normal | :italic, map()) :: tuple()
  def rank(font, weight, style, selection \\ %{}) do
    width = Map.get(selection, :width, 100)

    {first, last} =
      Map.get(font, :width_range, {Map.get(font, :width, 100), Map.get(font, :width, 100)})

    width_rank = directional_range(first, last, width, width <= 100)
    {first, last} = Map.get(font, :weight_range, {font.weight, font.weight})

    weight_rank =
      cond do
        first <= weight and last >= weight ->
          {0, 0}

        weight >= 400 and weight <= 500 ->
          cond do
            first > weight and first <= 500 -> {1, first - weight}
            last < weight -> {2, weight - last}
            true -> {3, first - 500}
          end

        true ->
          directional_range(first, last, weight, weight < 400)
      end

    style_font =
      cond do
        style == :italic and Map.get(font, :variation_italic?, false) and
            Map.get(selection, :oblique) == nil ->
          Map.put(font, :style, :italic)

        Map.has_key?(font, :variation_oblique) ->
          font |> Map.put(:style, :italic) |> Map.put(:oblique, font.variation_oblique)

        true ->
          font
      end

    {width_rank, style_rank(style_font, style, Map.get(selection, :oblique)), weight_rank}
  end

  defp directional_range(first, last, desired, descending?) do
    cond do
      first <= desired and last >= desired -> {0, 0}
      last < desired -> {if(descending?, do: 1, else: 2), desired - last}
      true -> {if(descending?, do: 2, else: 1), first - desired}
    end
  end

  # CSS Fonts 4, 1 February 2024, §5.2. Oblique ranges are searched in
  # angle order before falling back to a different physical style.
  defp style_rank(font, style, oblique) do
    case {style, oblique, font.style, Map.get(font, :oblique)} do
      {:normal, _, :normal, _} ->
        {0, 0, 0}

      {:normal, _, _, {first, last}} ->
        {group, distance} = directional_range(first, last, 0, false)

        {case group do
           0 -> 0
           1 -> 1
           2 -> 3
         end, distance, 0}

      {:normal, _, :italic, nil} ->
        {2, 0, 0}

      {:italic, nil, :italic, nil} ->
        {0, 0, 0}

      {:italic, nil, _, {first, last}} ->
        {group, distance} = directional_range(first, last, 11, false)
        {if(last > 0, do: 1, else: 3), group, distance}

      {:italic, nil, :normal, _} ->
        {2, 0, 0}

      {:italic, angle, _, {first, last}} ->
        {first, last, desired} =
          if angle < 0, do: {-last, -first, -angle}, else: {first, last, angle}

        {group, distance} = directional_range(first, last, desired, desired < 11)
        {if(group == 0 or last > 0, do: 0, else: 2), group, distance}

      {:italic, angle, :italic, nil} ->
        {if(angle < 0, do: 3, else: 1), 0, 0}

      {:italic, angle, :normal, _} ->
        {group, distance} = directional_range(0, 0, abs(angle), abs(angle) < 11)
        {if(angle == 0, do: 0, else: 2), group, distance}
    end
  end
end
