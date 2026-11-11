defmodule NativeElixirPdfUtilities.Validators.FontOutlineValidator do
  @moduledoc false
  alias NativeElixirPdfUtilities.Validators.{
    FontValidator,
    TrueTypeSubsetValidator,
    TrueTypeGlyphValidator
  }

  alias NativeElixirPdfUtilities.HtmlToPdf.Font.Outline

  @doc "Prepares a selected outline as a bounded SVG path in font design coordinates."
  @spec prepare_path(map(), non_neg_integer()) :: {:ok, binary()} | {:error, {atom(), map()}}
  def prepare_path(font, id) do
    FontValidator.with_budget(fn ->
      if is_integer(id) and id >= 0 and id < length(font.widths) do
        case font do
          %{outline_format: :cff, cff: face} ->
            path =
              Outline.cff_design_commands(face, id, font.units_per_em)
              |> Enum.map(fn command ->
                case command do
                  :close ->
                    "Z"

                  {:move, x, y} ->
                    ["M", number(x), " ", number(y)]

                  {:line, x, y} ->
                    ["L", number(x), " ", number(y)]

                  {:curve, a, b, c, d, e, f} ->
                    ["C", Enum.map_join([a, b, c, d, e, f], " ", &number/1)]
                end
              end)

            {:ok, IO.iodata_to_binary(path)}

          _ ->
            with {:ok, outlines} <-
                   FontValidator.memo({:color_outlines, font.data}, fn ->
                     TrueTypeSubsetValidator.prepare_outlines(font.data)
                   end) do
              {glyph, _} =
                TrueTypeGlyphValidator.resolve(id, outlines.glyphs, %{}, MapSet.new(), 0, false)

              FontValidator.reserve(:max_font_color_work, length(glyph.points))

              {paths, _} =
                Enum.map_reduce(glyph.ends, {glyph.points, 0}, fn last, {points, first} ->
                  {contour, remaining} = Enum.split(points, last - first + 1)
                  {contour_path(contour), {remaining, last + 1}}
                end)

              {:ok, IO.iodata_to_binary(paths)}
            end
        end
      else
        NativeElixirPdfUtilities.Diagnostics.error(
          :font,
          :invalid_font,
          "color outline references a glyph outside maxp",
          module: __MODULE__,
          operation: :prepare_color_font,
          source: "color outline"
        )
      end
    end)
  catch
    {:invalid_glyph, error} -> error
  end

  defp contour_path(points) do
    first = hd(points)
    last = List.last(points)

    {start, remaining} =
      cond do
        Bitwise.band(elem(first, 2), 1) != 0 ->
          {first, tl(points)}

        Bitwise.band(elem(last, 2), 1) != 0 ->
          {last, Enum.drop(points, -1)}

        true ->
          {{(elem(first, 0) + elem(last, 0)) / 2, (elem(first, 1) + elem(last, 1)) / 2, 1},
           points}
      end

    {x, y, _} = start
    ["M", number(x), " ", number(y), quadratics(remaining ++ [start]), "Z"]
  end

  defp quadratics(points) do
    case points do
      [] ->
        []

      [{x, y, flag} | rest] ->
        if Bitwise.band(flag, 1) != 0 do
          ["L", number(x), " ", number(y), quadratics(rest)]
        else
          [{nx, ny, nflag} | tail] = rest

          {ex, ey, remaining} =
            if Bitwise.band(nflag, 1) != 0,
              do: {nx, ny, tail},
              else: {(x + nx) / 2, (y + ny) / 2, rest}

          ["Q", Enum.map_join([x, y, ex, ey], " ", &number/1), quadratics(remaining)]
        end
    end
  end

  defp number(value),
    do: if(is_integer(value), do: Integer.to_string(value), else: Float.to_string(value * 1.0))
end
