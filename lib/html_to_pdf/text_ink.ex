defmodule NativeElixirPdfUtilities.HtmlToPdf.TextInk do
  @moduledoc false
  alias NativeElixirPdfUtilities.HtmlToPdf.{ClipGeometry, Font, ShapedTextPaint}

  alias NativeElixirPdfUtilities.Validators.{
    FontOutlineValidator,
    FontValidator
  }

  # The same selected glyphs determine ink, descender exclusions and contours.
  # Subdivision uses the existing clip precision and shared work budget.
  @doc "Measures admitted inline pieces from their selected shaped glyphs and optional contours."
  @spec measure([map()], boolean() | :paths) :: {:ok, map()} | {:error, term()}
  def measure(pieces, contours?) do
    Enum.reduce_while(pieces, {:ok, %{advance: 0.0, points: [], edges: [], glyphs: []}}, fn piece,
                                                                                            {:ok,
                                                                                             geometry} ->
      font = piece.style.font_face
      size = piece.style.font_size

      if font.type == :built_in do
        advance =
          Font.css_text_width(piece.text, font, size, Map.get(piece.style, :letter_spacing, 0.0))

        points = [{geometry.advance, -0.2 * size}, {geometry.advance + advance, 0.66 * size}]

        {:cont,
         {:ok,
          %{geometry | advance: geometry.advance + advance, points: points ++ geometry.points}}}
      else
        box =
          Map.get(piece, :box, %{})
          |> Map.merge(%{
            text: piece.text,
            font_face: font,
            font_size: size,
            letter_spacing: Map.get(piece.style, :letter_spacing, 0.0),
            word_spacing: Map.get(piece.style, :word_spacing, 0.0)
          })

        box = Map.put_new(box, :bidi_direction, Map.get(piece.style, :direction, :auto))
        origin = geometry.advance

        with {:ok, plan} <- ShapedTextPaint.plan(box),
             {:ok, bounds} <- Font.glyph_bounds(font) do
          result =
            Enum.reduce_while(plan.glyphs, {:ok, geometry}, fn glyph, {:ok, geometry} ->
              scale = Font.used_size(font, size) / font.units_per_em

              {a, b, c, d} =
                NativeElixirPdfUtilities.HtmlToPdf.DrawingGeometry.glyph_basis(
                  glyph
                  |> Map.put(:font_face, font)
                )

              transform = fn x, y ->
                {origin + glyph.paint_x + scale * (a * x + c * y),
                 glyph.paint_y + Font.baseline_offset(font, size) + scale * (b * x + d * y)}
              end

              {left, bottom, right, top} = elem(bounds, glyph.glyph)

              points =
                if {left, bottom, right, top} == {0, 0, 0, 0},
                  do: [],
                  else:
                    Enum.map([{left, bottom}, {right, bottom}, {right, top}, {left, top}], fn {x,
                                                                                               y} ->
                      transform.(x, y)
                    end)

              result =
                if contours? != false and scale > 0 do
                  with {:ok, commands} <-
                         FontValidator.memo({:typographic_outline, font.data, glyph.glyph}, fn ->
                           FontOutlineValidator.prepare_commands(font, glyph.glyph)
                         end) do
                    commands =
                      Enum.map(commands, fn {operation, coordinates} ->
                        coordinates =
                          coordinates
                          |> Enum.chunk_every(2)
                          |> Enum.flat_map(fn [x, y] -> Tuple.to_list(transform.(x, y)) end)

                        {operation, coordinates}
                      end)

                    edges =
                      if contours? == true do
                        {:contour, edges} =
                          ClipGeometry.flatten_box(
                            commands,
                            1 / NativeElixirPdfUtilities.Limits.get(:clip_curve_precision)
                          )

                        edges
                      else
                        []
                      end

                    {:ok, {commands, edges}}
                  end
                else
                  {:ok, {[], []}}
                end

              case result do
                {:ok, {commands, edges}} ->
                  {:cont,
                   {:ok,
                    %{
                      geometry
                      | points: points ++ geometry.points,
                        edges: edges ++ geometry.edges,
                        glyphs: [
                          %{
                            codepoints: glyph.codepoints,
                            points: points,
                            edges: edges,
                            commands: commands
                          }
                          | geometry.glyphs
                        ]
                    }}}

                error ->
                  {:halt, error}
              end
            end)

          case result do
            {:ok, geometry} -> {:cont, {:ok, %{geometry | advance: origin + plan.advance}}}
            error -> {:halt, error}
          end
        else
          error -> {:halt, error}
        end
      end
    end)
  end
end
