defmodule NativeElixirPdfUtilities.HtmlToPdf.ScrollbarPainter do
  @moduledoc false

  alias NativeElixirPdfUtilities.Validators.HtmlValidator

  @doc false
  @spec paint([map()]) :: [map()]
  def paint(parts) do
    Enum.map(parts, fn part ->
      box =
        part
        |> Map.drop([:kind, :direction, :paint_clip])
        |> Map.merge(%{
          type: HtmlValidator.reserve_layout_box(:rect),
          fill_color:
            if(part.kind == :track,
              do: {252 / 255, 252 / 255, 252 / 255},
              else: {139 / 255, 139 / 255, 139 / 255}
            ),
          stroke_color: nil,
          stroke_width: 0.0,
          role: :scrollbar
        })

      box =
        case part do
          %{kind: :arrow, direction: direction} ->
            Map.put(box, :shape, {:triangle, direction})

          _ ->
            box
        end

      if part.kind == :track do
        box
      else
        clip = Map.get(part, :paint_clip, Map.take(box, [:x, :y, :width, :height]))

        %{
          type: HtmlValidator.reserve_layout_box(:drawing_group),
          width: clip.width,
          height: clip.height,
          transform: {1, 0, 0, 1, clip.x, clip.y},
          clip: %{x: 0.0, y: 0.0, width: clip.width, height: clip.height},
          role: :scrollbar,
          boxes: [%{box | x: box.x - clip.x, y: box.y - clip.y}]
        }
      end
    end)
  end
end
