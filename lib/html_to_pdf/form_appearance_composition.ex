defmodule NativeElixirPdfUtilities.HtmlToPdf.FormAppearanceComposition do
  @moduledoc false
  alias NativeElixirPdfUtilities.HtmlToPdf.PdfWriter
  alias NativeElixirPdfUtilities.Pdf.{IncrementalWriter, InfoCodec, Reader}
  alias NativeElixirPdfUtilities.Validators.{HtmlValidator, WriterValidator}

  @doc "Composes a genuine native PDF appearance through its actual affine placement and visible clip polygons, preserving its original page paint frame."
  @spec project_appearance(term(), term(), term(), term()) ::
          {:ok, map()} | {:error, {atom(), map()}}
  def project_appearance(appearance, matrix, polygons, bounds) do
    HtmlValidator.with_render_budget(fn ->
      with {:ok, projection} <-
             WriterValidator.prepare_appearance_projection(appearance, matrix, polygons, bounds),
           {:ok, source} <- PdfWriter.render([%{size: projection.source_size, boxes: []}]),
           {:ok, context} <- Reader.read_validated(source),
           {:ok, objects, [form], next} <-
             PdfWriter.appearance_objects(
               [projection.appearance],
               context.document.trailer["Size"]
             ) do
        [page] = context.document.pages
        {page_id, generation} = page.ref
        page_value = context.document.objects[page.ref].value
        {left, bottom, right, top} = projection.bounds
        [a, b, c, d, e, f] = Tuple.to_list(projection.matrix)

        number = fn value ->
          {:ok, encoded} = InfoCodec.serialize_value(value)
          IO.iodata_to_binary(encoded)
        end

        paths =
          Enum.map_join(projection.polygons, "\n", fn [{x, y} | corners] ->
            Enum.join([number.(x - left), number.(y - bottom), "m"], " ") <>
              Enum.map_join(corners, "", fn {x, y} ->
                "\n" <> Enum.join([number.(x - left), number.(y - bottom), "l"], " ")
              end) <> "\nh"
          end)

        transform = Enum.map_join([a, b, c, d, e - left, f - bottom], " ", number)
        stream = "q\n" <> paths <> "\nW n\n" <> transform <> " cm\n/NativeFieldAppearance Do\nQ"
        width = right - left
        height = top - bottom

        page_value =
          page_value
          |> Map.put("MediaBox", [0, 0, width, height])
          |> Map.put("Contents", {:ref, {next, 0}})
          |> Map.put("Resources", %{"XObject" => %{"NativeFieldAppearance" => form}})

        with {:ok, pdf} <-
               IncrementalWriter.write(context, [
                 {next, 0, {:stream, %{}, stream}},
                 {page_id, generation, {:value, page_value}} | objects
               ]) do
          {:ok,
           %{
             size: {width, height},
             boxes: [
               %{
                 type: :image,
                 x: 0,
                 y: 0,
                 width: width,
                 height: height,
                 image: %{format: :pdf_form, data: pdf, width_px: 1, height_px: 1}
               }
             ]
           }}
        end
      end
    end)
  end
end
