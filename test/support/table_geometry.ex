defmodule NativeElixirPdfUtilities.TestSupport.TableGeometry do
  @moduledoc "Logical source artwork inspection for tests of canonical table formation."
  alias NativeElixirPdfUtilities.HtmlToPdf.DrawingGeometry

  @doc "Returns each original cell's native artwork in table coordinates, before physical fragment clipping."
  @spec original_cell_artwork([map()]) :: [map()]
  def original_cell_artwork(boxes) do
    Enum.flat_map(boxes, fn box ->
      case box do
        %{table_span_fragment: %{offset: offset}, transform: {1, 0, 0, 1, x, y}, boxes: children} ->
          if offset == 0, do: DrawingGeometry.translate_boxes(children, x, y), else: []

        _ ->
          [box]
      end
    end)
  end
end
