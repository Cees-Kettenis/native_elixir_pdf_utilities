defmodule NativeElixirPdfUtilities.HtmlToPdf.VerticalText do
  alias NativeElixirPdfUtilities.HtmlToPdf.UnicodeGrapheme
  @moduledoc false
  alias NativeElixirPdfUtilities.HtmlToPdf.{Font, TextShaper, UnicodeTextData}
  alias NativeElixirPdfUtilities.Validators.FontValidator

  @doc "Shapes vertical inline text into the line fitter's logical inline and block coordinates."
  @spec shape(String.t(), map(), keyword()) :: {:ok, map()} | {:error, {atom(), map()}}
  def shape(text, font, options) do
    FontValidator.with_budget(fn ->
      policy = font._vertical_text
      FontValidator.reserve(:max_work, byte_size(text))

      pieces =
        text
        |> UnicodeGrapheme.graphemes()
        |> Enum.map(&{&1, orientation(&1, policy)})

      pieces =
        pieces
        |> Enum.chunk_by(&elem(&1, 1))
        |> Enum.map(fn clusters ->
          kind = elem(hd(clusters), 1)

          kind =
            if kind == :digits,
              do: if(length(clusters) <= elem(policy.combine, 1), do: :combine, else: :sideways),
              else: kind

          {Enum.map_join(clusters, &elem(&1, 0)), kind}
        end)

      FontValidator.reserve(:max_work, length(pieces))
      characters = Keyword.get(options, :characters)

      Enum.reduce_while(pieces, {:ok, [], [], [], 0, 0, 0}, fn {part, kind},
                                                               {:ok, runs, clusters, glyphs,
                                                                bytes, scalars, cluster_count} ->
        count = length(String.codepoints(part))

        selected =
          Keyword.put(
            options,
            :direction,
            if(kind == :upright,
              do: :ttb,
              else: if(kind == :combine, do: :ltr, else: Keyword.get(options, :direction, :auto))
            )
          )

        selected =
          if characters == nil,
            do: selected,
            else: Keyword.put(selected, :characters, Enum.slice(characters, scalars, count))

        case TextShaper.shape(part, font, selected) do
          {:ok, shaped} ->
            combined_scale =
              if shaped.advance > 0, do: min(1, font.units_per_em / shaped.advance), else: 1

            counter = if policy.mode == :vertical_lr, do: {0, -1, -1, 0}, else: {0, 1, -1, 0}

            {part_glyphs, _} =
              Enum.map_reduce(shaped.glyphs, {0, 0}, fn glyph, {pen, index} ->
                original_advance = glyph.x_advance

                # CSS emphasis is centered on the em box, independently of
                # the marked letter's horizontal advance. Keep GPOS offsets
                # while replacing the ordinary vertical half-advance origin.
                glyph =
                  if kind == :upright and Map.get(policy, :emphasis, false) do
                    widths = Font.glyph_widths(font, glyph.glyph)
                    width = elem(widths, glyph.glyph)
                    %{glyph | x_offset: glyph.x_offset - (font.units_per_em - width) / 2}
                  else
                    glyph
                  end

                projected =
                  case kind do
                    :upright ->
                      %{
                        x_advance: -glyph.y_advance,
                        y_advance: 0,
                        x_offset: -glyph.y_offset,
                        y_offset:
                          if(policy.mode == :vertical_lr,
                            do: -glyph.x_offset,
                            else: glyph.x_offset
                          ),
                        glyph_basis: counter,
                        vertical_center?: true
                      }

                    :combine ->
                      {a, b, c, d} = counter
                      # A combined composition occupies one em along the line;
                      # each source glyph keeps its own horizontal position inside it.
                      %{
                        x_advance: if(index == 0, do: font.units_per_em, else: 0),
                        y_advance: 0,
                        x_offset: font.ascent - if(index == 0, do: 0, else: font.units_per_em),
                        y_offset:
                          ((pen + glyph.x_offset) * combined_scale -
                             shaped.advance * combined_scale / 2) *
                            if(policy.mode == :vertical_lr, do: -1, else: 1),
                        glyph_basis: {a * combined_scale, b * combined_scale, c, d},
                        vertical_center?: true
                      }

                    :sideways ->
                      if policy.mode == :vertical_lr do
                        %{
                          x_advance: glyph.x_advance,
                          y_advance: glyph.y_advance,
                          x_offset: glyph.x_offset,
                          y_offset: -glyph.y_offset,
                          glyph_basis: {1, 0, 0, -1},
                          vertical_center?: false
                        }
                      else
                        %{vertical_center?: false}
                      end
                  end

                glyph =
                  glyph
                  |> Map.merge(projected)
                  |> Map.update!(:cluster_start, &(&1 + bytes))
                  |> Map.update!(:cluster_end, &(&1 + bytes))
                  |> Map.update!(:scalar_start, &(&1 + scalars))
                  |> Map.update!(:scalar_end, &(&1 + scalars))

                glyph =
                  if kind == :combine,
                    do: %{
                      glyph
                      | cluster_start: bytes,
                        cluster_end: bytes + byte_size(part),
                        scalar_start: scalars,
                        scalar_end: scalars + count
                    },
                    else: glyph

                {glyph, {pen + original_advance, index + 1}}
              end)

            part_clusters =
              Enum.map(shaped.clusters, fn cluster ->
                cluster
                |> Map.update!(:start, &(&1 + bytes))
                |> Map.update!(:end, &(&1 + bytes))
                |> Map.update!(:scalar_start, &(&1 + scalars))
                |> Map.update!(:scalar_end, &(&1 + scalars))
              end)

            run = %{
              script: "Zyyy",
              language: Map.get(font, :shaping_language, "und"),
              start: bytes,
              end: bytes + byte_size(part),
              cluster_start: cluster_count,
              cluster_end: cluster_count + length(part_clusters),
              direction: :ltr,
              glyphs: part_glyphs,
              advance: Enum.sum(Enum.map(part_glyphs, & &1.x_advance)),
              vertical_advance: 0
            }

            {:cont,
             {:ok, [run | runs], [part_clusters | clusters], [part_glyphs | glyphs],
              bytes + byte_size(part), scalars + count, cluster_count + length(part_clusters)}}

          error ->
            {:halt, error}
        end
      end)
      |> case do
        {:ok, runs, clusters, glyphs, _, _, _} ->
          glyphs = glyphs |> Enum.reverse() |> List.flatten()

          {:ok,
           %{
             text: text,
             runs: Enum.reverse(runs),
             clusters: clusters |> Enum.reverse() |> List.flatten(),
             glyphs: glyphs,
             units_per_em: font.units_per_em,
             vertical_advance: 0,
             advance: Enum.sum(Enum.map(glyphs, & &1.x_advance))
           }}

        error ->
          error
      end
    end)
  end

  # CSS Writing Modes 4 section 5.1.2 keeps Tr/Tu upright. Rotating a missing
  # Tr form is optional synthesis, not the expected fallback. Shaping the
  # complete upright run also preserves contextual vertical substitutions.
  defp orientation(cluster, policy) do
    <<first::utf8, _::binary>> = cluster
    property = elem(UnicodeTextData.properties(first), 7)

    cond do
      policy.mode in [:sideways_rl, :sideways_lr] or policy.orientation == :sideways ->
        :sideways

      policy.combine == :all ->
        :combine

      match?({:digits, _}, policy.combine) and Regex.match?(~r/\A[0-9]\z/, cluster) ->
        :digits

      policy.orientation == :upright or property in [:U, :Tu, :Tr] ->
        :upright

      true ->
        :sideways
    end
  end
end
