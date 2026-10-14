defmodule NativeElixirPdfUtilities.TestSupport.MathPaintGeometry do
  @moduledoc "Compares complete selected glyph paint independently of document source identity."

  @doc "Returns every glyph paint field except source fragments, whose owners differ between parsed documents."
  @spec glyphs(map()) :: [map()]
  def glyphs(measured) do
    Enum.map(measured.glyphs, &Map.delete(&1, :source_fragments))
  end
end
