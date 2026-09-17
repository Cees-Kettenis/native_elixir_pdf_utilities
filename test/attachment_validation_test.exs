defmodule NativeElixirPdfUtilities.AttachmentValidationTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.{Attachments, HtmlToPdf, Limits}
  alias NativeElixirPdfUtilities.Pdf.{Reader, IncrementalWriter}

  alias NativeElixirPdfUtilities.Validators.{
    AttachmentValidator,
    MimeValidator,
    ModificationValidator
  }

  setup do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    {:ok, pdf} = HtmlToPdf.render("<p>Attachment tests</p>")
    {:ok, attached} = Attachments.embed(pdf, [%{filename: "one.txt", bytes: "one"}])
    %{pdf: pdf, attached: attached}
  end

  test "validates every supported binary signature and MIME syntax" do
    for {bytes, mime} <- [
          {"GIF89a", "image/gif"},
          {"RIFF0000WEBP", "image/webp"},
          {"RIFF0000WAVE", "audio/wav"},
          {"BM", "image/bmp"},
          {<<"II", 42, 0>>, "image/tiff"},
          {<<"MM", 0, 42>>, "image/tiff"},
          {<<0, 0, 1, 0>>, "image/vnd.microsoft.icon"},
          {<<31, 139>>, "application/gzip"},
          {"ID3", "audio/mpeg"},
          {"OggS", "application/ogg"},
          {"0000ftypisom", "video/mp4"}
        ] do
      assert {:ok, ^mime} = MimeValidator.prepare(bytes, "file", nil)
    end

    ole = <<0xD0, 0xCF, 0x11, 0xE0, 0xA1, 0xB1, 0x1A, 0xE1>>
    assert {:ok, "application/vnd.ms-excel"} = MimeValidator.prepare(ole, "a.xls", nil)
    assert {:ok, "application/x-ole-storage"} = MimeValidator.prepare(ole, "a", nil)
    assert {:ok, "application/msword"} = MimeValidator.prepare(ole, "a", "application/msword")

    assert {:ok, "application/vnd.ms-excel"} =
             MimeValidator.prepare(ole, "a.xls", "application/x-ole-storage")

    assert {:ok, "image/avif"} = MimeValidator.prepare("0000ftypavif", "a.avif", "image/avif")
    assert {:ok, "application/octet-stream"} = MimeValidator.prepare("0000ftyp", "a", nil)

    assert {:error, _} = MimeValidator.prepare(ole, "a.pdf", nil)

    assert {:ok, "application/pdf"} =
             MimeValidator.prepare("%PDF-1.7", "a.pdf", "application/octet-stream")
  end

  test "bounded ZIP metadata rejects ambiguous, forged and truncated directories" do
    {:ok, {_, zip}} =
      :zip.create(
        ~c"a.zip",
        [{~c"[Content_Types].xml", ""}, {~c"word/document.xml", ""}, {~c"xl/workbook.xml", ""}],
        [:memory]
      )

    assert {:error, _} = MimeValidator.prepare(zip, "a.zip", nil)
    {:ok, {_, zip}} = :zip.create(~c"a.zip", [{~c"a", ""}, {~c"b", ""}], [:memory])
    Limits.install(Map.put(Limits.effective(), :max_mime_container_entries, 1))
    assert {:error, {:resource_limit_exceeded, _}} = MimeValidator.prepare(zip, "a.zip", nil)
    Limits.install(Map.put(Limits.defaults(), :max_mime_container_bytes, 1))
    assert {:error, {:resource_limit_exceeded, _}} = MimeValidator.prepare(zip, "a.zip", nil)
    Limits.install(Limits.defaults())
    {:ok, {_, duplicate}} = :zip.create(~c"a.zip", [{~c"a", ""}, {~c"a", ""}], [:memory])
    assert {:error, _} = MimeValidator.prepare(duplicate, "a.zip", nil)
    assert {:error, _} = MimeValidator.prepare("PK\x03\x04PK\x05\x06", "a.zip", nil)
    {offset, _} = :binary.match(zip, "PK\x01\x02")

    corrupted =
      binary_part(zip, 0, offset) <>
        "BAD!" <> binary_part(zip, offset + 4, byte_size(zip) - offset - 4)

    assert {:error, _} = MimeValidator.prepare(corrupted, "a.zip", nil)
    # Forge an entry's filename length while preserving the end directory record.
    prefix = binary_part(zip, 0, offset + 28)

    truncated =
      prefix <>
        <<65535::little-16>> <> binary_part(zip, offset + 30, byte_size(zip) - offset - 30)

    assert {:error, _} = MimeValidator.prepare(truncated, "a.zip", nil)
  end

  test "preserves nested name trees and unrelated catalog names", %{attached: pdf} do
    {:ok, context} = Reader.read_validated(pdf)
    {:ok, names} = Reader.dictionary(context.document, context.catalog["Names"])
    tree = names["EmbeddedFiles"]

    pdf =
      patch_catalog(pdf, %{
        "Names" =>
          Map.merge(names, %{
            "EmbeddedFiles" => %{"Kids" => [tree]},
            "Unrelated" => %{"Names" => []}
          })
      })

    assert {:ok, [%{filename: "one.txt"}]} = Attachments.list(pdf)
    assert {:ok, updated} = Attachments.embed(pdf, [%{filename: "two.txt", bytes: "two"}])
    assert {:ok, context} = Reader.read_validated(updated)
    assert context.catalog["Names"]["Unrelated"] == %{"Names" => []}
    assert {:ok, [_, _]} = Attachments.list(updated)
  end

  test "rejects malformed trees, cyclic references and attachment metadata", %{attached: pdf} do
    {:ok, context} = Reader.read_validated(pdf)
    {:ok, names} = Reader.dictionary(context.document, context.catalog["Names"])
    [name, spec] = names["EmbeddedFiles"]["Names"]

    for tree <- [
          %{"Names" => [name]},
          %{"Names" => [name, spec, name, spec]},
          %{"Names" => [1, spec]},
          %{"Names" => [{:hex, <<254, 255, 1>>}, spec]},
          %{"Kids" => 1},
          %{"Kids" => [{:ref, {999, 0}}]},
          %{"Names" => [name, spec], "Kids" => [%{}]},
          %{"Names" => [name, {:ref, {999, 0}}]}
        ] do
      assert {:error, {_, %{module: Attachments, operation: :list}}} =
               Attachments.list(patch_catalog(pdf, %{"Names" => %{"EmbeddedFiles" => tree}}))
    end

    {catalog_id, gen} = context.catalog_ref
    next = context.document.trailer["Size"]
    catalog = Map.put(context.catalog, "Names", %{"EmbeddedFiles" => {:ref, {next, 0}}})

    {:ok, cyclic} =
      IncrementalWriter.write(context, [
        {next, 0, {:value, %{"Kids" => [{:ref, {next, 0}}]}}},
        {catalog_id, gen, {:value, catalog}}
      ])

    assert {:error, _} = Attachments.list(cyclic)
    {:ok, spec_dict} = Reader.dictionary(context.document, spec)
    {:ref, {spec_id, spec_gen}} = spec

    for patch <- [%{"UF" => 1}, %{"Desc" => {:hex, <<254, 255, 1>>}}, %{"EF" => %{}}] do
      {:ok, bad} =
        IncrementalWriter.write(context, [
          {spec_id, spec_gen, {:value, Map.merge(spec_dict, patch)}}
        ])

      assert {:error, _} = Attachments.list(bad)
    end

    {:ref, {stream_id, stream_gen}} = spec_dict["EF"]["F"]

    for patch <- [%{"Params" => %{"Size" => -1}}, %{"Subtype" => 1}, %{"Subtype" => nil}] do
      stream = context.document.objects[{stream_id, stream_gen}]

      {:ok, updated} =
        IncrementalWriter.write(context, [
          {stream_id, stream_gen, {:stream, Map.merge(stream.value, patch), stream.stream}}
        ])

      if patch["Subtype"] == nil and Map.has_key?(patch, "Subtype"),
        do: assert({:ok, [%{mime_type: nil}]} = Attachments.list(updated)),
        else: assert({:error, _} = Attachments.list(updated))
    end
  end

  test "checks aggregate and incremental limits", %{pdf: pdf, attached: attached} do
    file = %{filename: "two.txt", bytes: "two"}
    Limits.install(Map.put(Limits.defaults(), :max_pdf_attachments, 1))
    assert {:error, _} = Attachments.embed(pdf, [file, %{file | filename: "three.txt"}])
    assert {:error, {:resource_limit_exceeded, _}} = Attachments.embed(attached, [file])
    Limits.install(Map.put(Limits.defaults(), :max_pdf_info_value_bytes, 1))
    assert {:error, {:resource_limit_exceeded, _}} = Attachments.embed(pdf, [file])
    Limits.install(Map.put(Limits.defaults(), :max_pdf_attachment_total_bytes, 2))
    assert {:error, {:resource_limit_exceeded, _}} = Attachments.embed(pdf, [file])
    assert {:error, {:resource_limit_exceeded, _}} = Attachments.list(attached)
    Limits.install(Map.put(Limits.defaults(), :max_pdf_name_tree_nodes, 1))
    {:ok, context} = Reader.read_validated(attached)
    {:ok, names} = Reader.dictionary(context.document, context.catalog["Names"])

    nested =
      patch_catalog(attached, %{
        "Names" => %{"EmbeddedFiles" => %{"Kids" => [names["EmbeddedFiles"]]}}
      })

    assert {:error, {:resource_limit_exceeded, _}} = Attachments.list(nested)
    Limits.install(Limits.defaults())
    {:ok, context} = Reader.read_validated(pdf)
    {:ok, existing} = AttachmentValidator.inspect_document(context)
    {:ok, prepared} = AttachmentValidator.prepare([file], [])
    Limits.install(Map.put(Limits.defaults(), :max_pdf_objects, context.document.trailer["Size"]))

    assert {:error, {:resource_limit_exceeded, _}} =
             AttachmentValidator.prepare_write(context, existing, prepared)

    Limits.install(Map.put(Limits.defaults(), :max_pdf_input_bytes, byte_size(pdf)))

    assert {:error, {:resource_limit_exceeded, _}} =
             AttachmentValidator.prepare_write(context, existing, prepared)
  end

  test "rejects modification of signed PDFs", %{pdf: pdf} do
    signed = patch_catalog(pdf, %{"Perms" => %{}})

    assert {:error, {:unsupported_form, _}} =
             Attachments.embed(signed, [%{filename: "a", bytes: "a"}])

    {:ok, context} = Reader.read_validated(pdf)
    context = put_in(context.document.objects[{999, 0}], %{value: 1})
    assert :ok = ModificationValidator.validate(context)
  end

  test "aggregate budget includes files embedded by earlier calls", %{attached: pdf} do
    Limits.install(Map.put(Limits.defaults(), :max_pdf_attachment_total_bytes, 5))

    assert {:error, {:resource_limit_exceeded, %{operation: :embed, message: message}}} =
             Attachments.embed(pdf, [%{filename: "two.txt", bytes: "two"}])

    assert message =~ "max_pdf_attachment_total_bytes"
    assert {:ok, _} = Attachments.embed(pdf, [%{filename: "two.txt", bytes: "tw"}])
  end

  test "name-tree traversal also bounds depth for an already parsed document", %{attached: pdf} do
    {:ok, context} = Reader.read_validated(pdf)
    {:ok, names} = Reader.dictionary(context.document, context.catalog["Names"])
    tree = %{"Kids" => [%{"Kids" => [names["EmbeddedFiles"]]}]}
    context = put_in(context.catalog["Names"], %{"EmbeddedFiles" => tree})
    Limits.install(Map.put(Limits.defaults(), :max_pdf_value_depth, 1))

    assert {:error, {:resource_limit_exceeded, %{message: message}}} =
             AttachmentValidator.inspect_document(context)

    assert message =~ "traversal limits"
  end

  defp patch_catalog(pdf, patch) do
    {:ok, context} = Reader.read_validated(pdf)
    {id, gen} = context.catalog_ref

    {:ok, pdf} =
      IncrementalWriter.write(context, [{id, gen, {:value, Map.merge(context.catalog, patch)}}])

    pdf
  end
end
