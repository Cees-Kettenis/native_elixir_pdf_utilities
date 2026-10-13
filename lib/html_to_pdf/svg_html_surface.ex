defmodule NativeElixirPdfUtilities.HtmlToPdf.SvgHtmlSurface do
  @moduledoc false
  alias NativeElixirPdfUtilities.Limits
  alias NativeElixirPdfUtilities.HtmlToPdf.{SvgBackend, SvgFormComposition}

  alias NativeElixirPdfUtilities.Validators.{
    SvgHtmlSurfaceValidator,
    SvgFormSlotValidator,
    SvgValidator
  }

  @doc false
  @spec paint(binary(), binary(), map(), keyword(), map(), term()) ::
          {:ok, binary(), [map()]} | {:error, {atom(), map()}}
  def paint(probe_source, image_source, issued, options, context, budget) do
    with {:ok, probe} <- SvgBackend.probe_slots(probe_source, options, context, issued, budget),
         {:ok, _reader, slots} <- SvgFormSlotValidator.prepare(probe, issued),
         {:ok, inputs, placements} <-
           Enum.reduce_while(slots, {:ok, %{}, []}, fn slot, {:ok, inputs, placements} ->
             with {:ok, prepared} <- SvgHtmlSurfaceValidator.prepare(slot),
                  {:ok, png} <- SvgHtmlSurfaceValidator.render(prepared) do
               logical =
                 if prepared.glyphs > 0, do: prepared.logical, else: ""

               {:cont,
                {:ok,
                 Map.put(
                   inputs,
                   slot.id,
                   {png, slot.width * 1.0, slot.height * 1.0, logical, prepared.logical_clusters}
                 ),
                 [
                   Map.merge(slot, %{
                     group: prepared.group,
                     selected_html_spans: prepared.spans,
                     selected_html_glyphs: prepared.selected,
                     html_source: prepared.source
                   })
                   | placements
                 ]}}
             else
               error -> {:halt, error}
             end
           end),
         {:ok, skeleton} <-
           NativeElixirPdfUtilities.HtmlToPdf.SvgBackend.NativeBytes.vectorize_html_surfaces(
             image_source,
             context.fonts,
             inputs,
             Map.new(Map.get(context, :resources, %{}), fn {id, resource} ->
               {id, resource.data}
             end),
             {Keyword.fetch!(options, :width), Keyword.fetch!(options, :height)},
             {Limits.get(:max_aggregate_svg_surface_pixels), Limits.get(:max_svg_filter_work)}
           )
           |> SvgValidator.conversion_result(),
         groups = Map.new(placements, &{&1.id, &1.group}),
         {:ok, pdf, _normal} <-
           SvgFormComposition.compose(skeleton, issued, fn slot ->
             {:ok, Map.fetch!(groups, slot.id)}
           end) do
      {:ok, pdf, placements}
    end
  end
end
