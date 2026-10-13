defmodule NativeElixirPdfUtilities.HtmlToPdf.SvgFormComposition do
  @moduledoc false
  alias NativeElixirPdfUtilities.HtmlToPdf.PdfWriter
  alias NativeElixirPdfUtilities.Pdf.IncrementalWriter
  alias NativeElixirPdfUtilities.Validators.SvgFormSlotValidator

  @doc "Inserts genuine validated drawing groups into issued native SVG Form slots at their original paint positions."
  @spec compose(binary(), map(), (map() -> {:ok, map()} | {:error, {atom(), map()}})) ::
          {:ok, binary(), [map()]} | {:error, {atom(), map()}}
  def compose(source, issued, layout) do
    with {:ok, context, slots} <- SvgFormSlotValidator.prepare(source, issued),
         {:ok, objects, placements, _next} <-
           Enum.reduce_while(slots, {:ok, [], [], context.document.trailer["Size"]}, fn slot,
                                                                                        {:ok,
                                                                                         objects,
                                                                                         placements,
                                                                                         next} ->
             with {:ok, group} <- layout.(slot),
                  {:ok, added, [form], next} <-
                    PdfWriter.appearance_objects(
                      [%{size: {slot.width, slot.height}, boxes: [group]}],
                      next
                    ) do
               {id, generation} = slot.ref

               dictionary = %{
                 "Type" => {:name, "XObject"},
                 "Subtype" => {:name, "Form"},
                 "BBox" => [0, 0, slot.width, slot.height],
                 "Resources" => %{"XObject" => %{"NativeHtmlSurface" => form}}
               }

               replacement =
                 {id, generation, {:stream, dictionary, "q\n/NativeHtmlSurface Do\nQ"}}

               {:cont,
                {:ok, [replacement | Enum.reverse(added, objects)],
                 [Map.put(slot, :group, group) | placements], next}}
             else
               error -> {:halt, error}
             end
           end),
         {:ok, pdf} <-
           if(objects == [], do: {:ok, source}, else: IncrementalWriter.write(context, objects)) do
      {:ok, pdf, placements}
    end
  end
end
