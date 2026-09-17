defmodule NativeElixirPdfUtilities.Pdf.AttachmentWriter do
  @moduledoc false
  alias NativeElixirPdfUtilities.Pdf.{IncrementalWriter, InfoCodec}
  alias NativeElixirPdfUtilities.Validators.AttachmentValidator

  @doc false
  @spec write(map(), map(), [map()]) :: {:ok, binary()} | {:error, {atom(), map()}}
  def write(context, existing, attachments) do
    {entries, objects, _next} =
      Enum.reduce(attachments, {existing.entries, [], context.document.trailer["Size"]}, fn file,
                                                                                            {entries,
                                                                                             objects,
                                                                                             id} ->
        stream = %{
          "Type" => {:name, "EmbeddedFile"},
          "Subtype" => {:name, file.mime_type},
          "Params" => %{"Size" => byte_size(file.bytes)}
        }

        spec = %{
          "Type" => {:name, "Filespec"},
          "F" => InfoCodec.encode_text(file.filename),
          "UF" => InfoCodec.encode_text(file.filename),
          "Desc" => InfoCodec.encode_text(file.description),
          "EF" => %{"F" => {:ref, {id, 0}}, "UF" => {:ref, {id, 0}}}
        }

        {[{file.filename, {:ref, {id + 1, 0}}} | entries],
         [{id, 0, {:stream, stream, file.bytes}}, {id + 1, 0, {:value, spec}} | objects], id + 2}
      end)

    pairs =
      entries
      |> Enum.map(fn {name, ref} -> {InfoCodec.encode_text(name), ref} end)
      |> Enum.sort_by(fn {{_, bytes}, _} -> bytes end)
      |> Enum.flat_map(fn {name, ref} -> [name, ref] end)

    names = Map.put(existing.names, "EmbeddedFiles", %{"Names" => pairs})
    catalog = Map.put(context.catalog, "Names", names)
    {id, generation} = context.catalog_ref

    with {:ok, output} <-
           IncrementalWriter.prepare(context, [{id, generation, {:value, catalog}} | objects]),
         :ok <- AttachmentValidator.validate_output(output) do
      {:ok, IO.iodata_to_binary(output)}
    end
  end
end
