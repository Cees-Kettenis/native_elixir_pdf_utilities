defmodule NativeElixirPdfUtilities.HtmlToPdf.Font.Outline do
  @moduledoc false

  @doc "Converts admitted CFF glyph commands through their FontDICT and TopDICT matrices to OpenType design units."
  @spec cff_design_commands(map(), non_neg_integer(), pos_integer()) :: [tuple() | :close]
  def cff_design_commands(face, index, units) do
    [a, b, c, d, e, f] = face.font_matrix
    font = elem(face.fonts, elem(face.fd_select, index))

    [g, h, i, j, k, l] =
      if face.cid?, do: font.font_matrix || [1, 0, 0, 1, 0, 0], else: [1, 0, 0, 1, 0, 0]

    transform = fn x, y ->
      {units * (a * (g * x + i * y + k) + c * (h * x + j * y + l) + e),
       units * (b * (g * x + i * y + k) + d * (h * x + j * y + l) + f)}
    end

    Enum.map(elem(face.glyphs, index).commands, fn command ->
      case command do
        :close ->
          :close

        {kind, x, y} ->
          {x, y} = transform.(x, y)
          {kind, x, y}

        {:curve, a, b, c, d, e, f} ->
          {a, b} = transform.(a, b)
          {c, d} = transform.(c, d)
          {e, f} = transform.(e, f)
          {:curve, a, b, c, d, e, f}
      end
    end)
  end
end
