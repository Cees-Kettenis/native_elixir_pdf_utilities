defmodule NativeElixirPdfUtilities.Validators.OpenTypeVerticalValidator do
  @moduledoc false
  alias NativeElixirPdfUtilities.Validators.{FontValidator, OpenTypeDataValidator}
  alias OpenTypeDataValidator, as: Data

  @doc "Admits optional vertical metrics and CFF vertical origins from bounded sfnt tables."
  @spec prepare(map(), pos_integer()) :: {:ok, map()} | {:error, {atom(), map()}}
  def prepare(tables, glyph_count) do
    FontValidator.with_budget(fn ->
      header = Map.get(tables, "vhea")
      metrics = Map.get(tables, "vmtx")
      origins = if not Map.get(tables, :true_type?, false), do: Map.get(tables, "VORG")

      empty = %{
        metrics: nil,
        origins: nil,
        header: nil,
        outlines?: Map.get(tables, :outlines?, false)
      }

      if header == nil and metrics == nil and origins == nil do
        {:ok, empty}
      else
        if header == nil or metrics == nil,
          do:
            Data.invalid(
              %{tag: "vhea/vmtx", operation: :prepare_vertical_metrics, module: __MODULE__},
              "vertical fonts require both vhea and vmtx",
              0
            )

        context = %{
          data: header,
          tag: "vhea",
          glyph_count: glyph_count,
          operation: :prepare_vertical_metrics,
          module: __MODULE__
        }

        <<version::32, ascent::signed-16, descent::signed-16, gap::signed-16,
          _metrics::binary-size(14), reserved::64, format::signed-16, count::16>> =
          Data.read(context, 0, 36)

        # Versions, reserved fields and the unsigned maxp glyph index range
        # are OpenType format invariants, independent of resource policy.
        if version not in [0x10000, 0x11000] or reserved != 0 or format != 0 or
             count == 0 or count > glyph_count,
           do:
             Data.invalid(
               context,
               "invalid vertical header version, reserved fields or metric count",
               0
             )

        FontValidator.reserve(:max_entries, glyph_count)
        context = %{context | data: metrics, tag: "vmtx"}

        long =
          for <<advance::16, bearing::signed-16 <- Data.read(context, 0, count * 4)>>,
            do: {advance, bearing}

        {last_advance, _} = List.last(long)

        tail =
          for <<bearing::signed-16 <- Data.read(context, count * 4, (glyph_count - count) * 2)>>,
            do: {last_advance, bearing}

        vertical_origins =
          if origins == nil do
            nil
          else
            context = %{context | data: origins, tag: "VORG"}
            <<major::16, minor::16, default::signed-16, count::16>> = Data.read(context, 0, 8)

            if major != 1 or minor != 0 or count > glyph_count,
              do: Data.invalid(context, "invalid vertical origin version or glyph count", 0)

            FontValidator.reserve(:max_entries, count)

            records =
              for <<glyph::16, origin::signed-16 <- Data.read(context, 8, count * 4)>>,
                do: {Data.glyph(context, glyph, 8), origin}

            Data.ordered(
              context,
              Enum.map(records, &elem(&1, 0)),
              "vertical origin glyphs must be strictly increasing",
              8
            )

            %{default: default, overrides: Map.new(records)}
          end

        {:ok,
         %{
           metrics: List.to_tuple(long ++ tail),
           origins: vertical_origins,
           outlines?: empty.outlines?,
           header: %{ascent: ascent, descent: descent, line_gap: gap}
         }}
      end
    end)
  catch
    {:invalid_open_type, error} -> error
  end

  @doc "Resolves vertical origins from VORG, top bearings or centered outline extents when vertical tables are absent."
  @spec origins(map(), map()) :: {:ok, tuple() | nil} | {:error, {atom(), map()}}
  def origins(font, vertical) do
    if vertical.metrics == nil and not Map.get(vertical, :outlines?, false) do
      {:ok, nil}
    else
      FontValidator.with_budget(fn ->
        FontValidator.memo(
          {:vertical_origins, Map.get(font, :data), vertical, Map.get(font, :ascent),
           Map.get(font, :descent)},
          fn ->
            if vertical.origins != nil do
              count = tuple_size(vertical.metrics)
              FontValidator.reserve(:max_entries, count)
              FontValidator.reserve(:max_work, count)

              {:ok,
               vertical.metrics
               |> Tuple.to_list()
               |> Enum.with_index()
               |> Enum.map(fn {_, glyph} ->
                 Map.get(vertical.origins.overrides, glyph, vertical.origins.default)
               end)
               |> List.to_tuple()}
            else
              with {:ok, bounds} <- FontValidator.prepare_glyph_bounds(font) do
                count = tuple_size(bounds)
                FontValidator.reserve(:max_entries, count)
                FontValidator.reserve(:max_work, count)

                {:ok,
                 bounds
                 |> Tuple.to_list()
                 |> Enum.with_index()
                 |> Enum.map(fn {{_, bottom, _, top}, glyph} ->
                   if vertical.metrics == nil do
                     # OpenType shapers synthesize a vertical cell from horizontal
                     # ascent/descent and center each glyph's actual ink in it.
                     # The half-unit floor matches HarfBuzz's design-unit origin.
                     top + floor((font.ascent - font.descent + bottom - top) / 2)
                   else
                     {_, bearing} = elem(vertical.metrics, glyph)
                     bearing + top
                   end
                 end)
                 |> List.to_tuple()}
              end
            end
          end
        )
      end)
    end
  end
end
