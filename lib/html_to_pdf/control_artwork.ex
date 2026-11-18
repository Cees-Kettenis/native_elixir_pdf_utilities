defmodule NativeElixirPdfUtilities.HtmlToPdf.ControlArtwork do
  @moduledoc false

  @doc "Uses a control part's computed paint without altering the native control's value."
  @spec paint_parts([map()], map()) :: [map()]
  def paint_parts(boxes, style) do
    case style do
      %{_control_parts: parts} ->
        Enum.flat_map(boxes, fn box ->
          keys =
            case Map.get(box, :role) do
              :range_track -> [:slider_track, :track]
              :range_border -> [:slider_track, :track]
              :range_value -> [:slider_fill, :fill]
              :range_thumb -> [:slider_thumb, :thumb]
              :gauge_track -> [:track]
              :gauge_value -> [:fill]
              :gauge_indeterminate -> [:fill]
              :native_file_button -> [:file_selector_button]
              :color_swatch -> [:color_swatch]
              _ -> []
            end

          case Enum.find_value(keys, &Map.get(parts, &1)) do
            nil -> [box]
            %{display: :none} -> []
            %{visibility: visibility} when visibility in [:hidden, :collapse] -> []
            part -> [paint(box, part)]
          end
        end)

      _ ->
        boxes
    end
  end

  defp paint(box, part) do
    box =
      case Map.fetch(part, :background_color) do
        {:ok, color} when box.role != :range_border ->
          box |> Map.put(:fill_color, color) |> Map.delete(:gauge_gradient)

        _ ->
          box
      end

    box =
      Map.merge(
        box,
        Map.take(part, [:border_radius, :border_widths, :border_styles, :border_colors])
      )

    case Map.fetch(part, :border_widths) do
      {:ok, widths} ->
        box
        |> Map.put(:stroke_width, Enum.max(Map.values(widths)))
        |> Map.put(:stroke_color, Map.get(part, :border_color, part.color))

      :error ->
        box
    end
  end
end
