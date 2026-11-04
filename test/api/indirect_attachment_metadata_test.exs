defmodule NativeElixirPdfUtilities.IndirectAttachmentMetadataTest do
  use ExUnit.Case, async: true

  alias NativeElixirPdfUtilities.{Attachments, HtmlToPdf}
  alias NativeElixirPdfUtilities.Pdf.{IncrementalWriter, Reader}
  alias NativeElixirPdfUtilities.Validators.AttachmentValidator

  setup do
    assert {:ok, base} = HtmlToPdf.render("<p>Attachment</p>", system_font_discovery: false)

    assert {:ok, pdf} =
             Attachments.embed(base, [
               %{filename: "source.txt", bytes: "source", description: "Original"}
             ])

    assert {:ok, context} = Reader.read_validated(pdf)
    assert {:ok, %{files: [file]}} = AttachmentValidator.inspect_document(context)
    {:ref, spec_ref} = file.reference
    spec = context.document.objects[spec_ref].value
    {:ref, stream_ref} = spec["EF"]["F"]

    %{
      context: context,
      spec_ref: spec_ref,
      spec: spec,
      stream_ref: stream_ref,
      stream: context.document.objects[stream_ref]
    }
  end

  test "indirect filenames descriptions MIME names and sizes support listing and embedding",
       fixture do
    next = fixture.context.document.trailer["Size"]

    for {key, value} <- [
          {"F", {:string, "renamed.txt"}},
          {"UF", {:string, "renamed.txt"}},
          {"Desc", {:string, "Description"}},
          {"Subtype", {:name, "text/plain"}},
          {"Size", 6}
        ] do
      direct = metadata_pdf(fixture, key, value)
      indirect = metadata_pdf(fixture, key, {:ref, {next, 0}}, [{next, 0, {:value, value}}])
      assert {:ok, [expected]} = Attachments.list(direct)
      assert Attachments.list(indirect) == {:ok, [expected]}

      assert {:ok, appended} =
               Attachments.embed(indirect, [%{filename: "extra.txt", bytes: "extra"}])

      assert {:ok, files} = Attachments.list(appended)
      assert expected in files
      assert Enum.any?(files, &(&1.filename == "extra.txt"))
    end
  end

  test "optional null metadata uses the same defaults directly and through references", fixture do
    next = fixture.context.document.trailer["Size"]

    for {key, field, expected} <- [
          {"UF", :filename, "source.txt"},
          {"Desc", :description, ""},
          {"Subtype", :mime_type, nil},
          {"Size", :size, nil},
          {"Params", :size, nil}
        ] do
      for {value, extra} <- [{nil, []}, {{:ref, {next, 0}}, [{next, 0, {:value, nil}}]}] do
        candidate = metadata_pdf(fixture, key, value, extra)
        assert {:ok, [file]} = Attachments.list(candidate)
        assert Map.fetch!(file, field) == expected

        assert {:ok, appended} =
                 Attachments.embed(candidate, [%{filename: "extra.txt", bytes: "extra"}])

        assert {:ok, [_, _]} = Attachments.list(appended)
      end
    end
  end

  test "malformed resolved values and broken references preserve diagnostics", fixture do
    next = fixture.context.document.trailer["Size"]

    for {key, invalid} <- [{"F", 42}, {"UF", 42}, {"Desc", 42}, {"Subtype", 42}, {"Size", -1}] do
      candidate = metadata_pdf(fixture, key, {:ref, {next, 0}}, [{next, 0, {:value, invalid}}])
      assert {:error, {:invalid_attachment, diagnostic}} = Attachments.list(candidate)
      assert diagnostic.stage == :attachments
      assert diagnostic.reason == :invalid_attachment
      assert diagnostic.operation == :list
      assert diagnostic.module == Attachments
      assert diagnostic.message != ""
    end

    for key <- ["F", "UF", "Desc", "Subtype", "Size", "Params"],
        extra <- [[], [{next, 0, {:value, {:ref, {next, 0}}}}]] do
      candidate = metadata_pdf(fixture, key, {:ref, {next, 0}}, extra)

      for {operation, result} <- [
            list: Attachments.list(candidate),
            embed: Attachments.embed(candidate, [%{filename: "extra.txt", bytes: "extra"}])
          ] do
        assert {:error, {:invalid_pdf_input, diagnostic}} = result
        assert diagnostic.stage == :resolution
        assert diagnostic.reason == :invalid_pdf_input
        assert diagnostic.operation == operation
        assert diagnostic.module == Attachments
        assert diagnostic.message != ""
      end
    end
  end

  defp metadata_pdf(fixture, key, value, objects \\ []) do
    {id, generation, body} =
      case key do
        key when key in ["F", "UF", "Desc"] ->
          {id, generation} = fixture.spec_ref
          spec = if key == "F", do: Map.delete(fixture.spec, "UF"), else: fixture.spec

          spec = Map.put(spec, key, value)

          {id, generation, {:value, spec}}

        key ->
          {id, generation} = fixture.stream_ref

          dictionary =
            if key == "Size",
              do: put_in(fixture.stream.value, ["Params", "Size"], value),
              else: Map.put(fixture.stream.value, key, value)

          {id, generation, {:stream, dictionary, fixture.stream.stream}}
      end

    assert {:ok, pdf} =
             IncrementalWriter.write(fixture.context, [{id, generation, body} | objects])

    pdf
  end
end
