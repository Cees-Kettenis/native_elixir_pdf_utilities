defmodule NativeElixirPdfUtilities.HtmlToPdf.Font.TrueTypeGlyph do
  @moduledoc false
  import Bitwise

  @doc "Serializes admitted static outline points or composite records with their evaluated bounds."
  @spec serialize(map()) :: binary()
  def serialize(glyph) do
    if glyph.contours == 0 do
      <<>>
    else
      {a, b, c, d} = glyph.bbox

      body =
        if glyph.contours < 0 do
          Enum.with_index(glyph.components)
          |> Enum.map(fn {component, index} ->
            flags =
              (component.flags &&& bnot(1 ||| 32 ||| 256)) ||| 1 |||
                if(index < length(glyph.components) - 1, do: 32, else: 0)

            {x, y} = component.args
            # All arguments have already been checked against their signed or
            # unsigned 16-bit field by the instance validator.
            <<flags::16, component.id::16, x::16, y::16, component.transform::binary>>
          end)
        else
          {encoded, _} =
            Enum.map_reduce(glyph.points, {0, 0}, fn {x, y, flag}, {px, py} ->
              {{flag, <<x - px::signed-16>>, <<y - py::signed-16>>}, {x, y}}
            end)

          [
            for(n <- glyph.ends, do: <<n::16>>),
            <<0::16>>,
            for({f, _, _} <- encoded, do: <<f>>),
            for({_, x, _} <- encoded, do: x),
            for({_, _, y} <- encoded, do: y)
          ]
        end

      bytes =
        IO.iodata_to_binary([
          <<glyph.contours::signed-16, a::signed-16, b::signed-16, c::signed-16, d::signed-16>>,
          body
        ])

      bytes <> :binary.copy(<<0>>, rem(2 - rem(byte_size(bytes), 2), 2))
    end
  end
end
