defmodule NativeElixirPdfUtilities.AttachmentsTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.{Attachments, HtmlToPdf, Limits}
  alias NativeElixirPdfUtilities.Pdf.Reader
  alias NativeElixirPdfUtilities.Validators.MimeValidator

  setup do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    {:ok, pdf} = HtmlToPdf.render("<p>Invoice</p>")
    %{pdf: pdf}
  end

  test "embeds unchanged binary data and preserves prior attachments", %{pdf: pdf} do
    assert {:ok, []} = Attachments.list(pdf)
    assert {:ok, ^pdf} = Attachments.embed(pdf, [])
    bytes = <<0, 255, 1, 2, 0>>
    assert {:ok, first} = Attachments.embed(pdf, [%{filename: "évidence.bin", bytes: bytes}])

    assert {:ok, result} =
             Attachments.embed(first, [
               %{filename: "data.CSV", bytes: "name\nCees", description: "Source"}
             ])

    assert {:ok, files} = Attachments.list(result)

    assert Enum.any?(
             files,
             &(&1.filename == "évidence.bin" and &1.mime_type == "application/octet-stream")
           )

    assert Enum.any?(
             files,
             &(&1.filename == "data.CSV" and &1.description == "Source" and &1.size == 9)
           )

    assert {:ok, document} = Reader.read(result)
    assert Enum.any?(document.objects, fn {_, obj} -> obj.stream == bytes end)

    assert {:error,
            {:invalid_attachment, %{operation: :embed, module: Attachments, message: message}}} =
             Attachments.embed(result, [%{filename: "data.CSV", bytes: "again"}])

    assert message =~ "already exists"
  end

  test "validates metadata and limits", %{pdf: pdf} do
    for attachment <- [
          nil,
          %{},
          %{filename: "../a", bytes: "a"},
          %{filename: "a", bytes: 1},
          %{filename: "a", bytes: "a", extra: true},
          %{filename: "a", bytes: "a", description: 1}
        ] do
      assert {:error, {:invalid_attachment, %{stage: :attachments, message: _}}} =
               Attachments.embed(pdf, [attachment])
    end

    assert {:error, _} = Attachments.embed(pdf, :bad)
    assert {:error, _} = Attachments.embed(pdf, [], unknown: true)

    assert {:error, _} =
             Attachments.embed(pdf, [%{filename: "a", bytes: "a"}, %{filename: "a", bytes: "a"}])

    Limits.install(Map.put(Limits.effective(), :max_pdf_attachment_bytes, 1))

    assert {:error, {:resource_limit_exceeded, %{stage: :limits}}} =
             Attachments.embed(pdf, [%{filename: "a", bytes: "ab"}])

    assert {:error, {_, %{operation: :list, module: Attachments}}} = Attachments.list("bad")
  end

  test "detects signatures and rejects conflicting type evidence" do
    for {bytes, filename, mime} <- [
          {<<137, "PNG", 13, 10, 26, 10>>, "image.PNG", "image/png"},
          {<<255, 216, 255>>, "photo.jpeg", "image/jpeg"},
          {"%PDF-1.7", "file.pdf", "application/pdf"},
          {"opaque", "file.unrecognized", "application/octet-stream"},
          {"a,b", "file.csv", "text/csv"}
        ] do
      assert {:ok, ^mime} = MimeValidator.prepare(bytes, filename, nil)
    end

    assert {:ok, "image/jpeg"} =
             MimeValidator.prepare(<<255, 216, 255>>, "photo.jpg", "image/jpg")

    assert {:error, {:invalid_mime_type, _}} = MimeValidator.prepare("%PDF-1.7", "photo.jpg", nil)
    assert {:error, _} = MimeValidator.prepare("%PDF-1.7", "a.pdf", "text/plain")
    assert {:error, _} = MimeValidator.validate("text/plain; charset=utf-8")
    assert {:error, _} = MimeValidator.validate(nil)

    assert {:ok, "application/custom"} =
             MimeValidator.prepare("opaque", "a", "application/custom")
  end

  test "distinguishes ZIP and Office containers without extracting entries" do
    {:ok, {_, zip}} = :zip.create(~c"a.zip", [{~c"hello.txt", "hello"}], [:memory])
    assert {:ok, "application/zip"} = MimeValidator.prepare(zip, "a.zip", nil)
    assert {:error, _} = MimeValidator.prepare(zip, "a.xlsx", nil)

    {:ok, {_, xlsx}} =
      :zip.create(
        ~c"a.zip",
        [{~c"[Content_Types].xml", "types"}, {~c"xl/workbook.xml", "book"}],
        [:memory]
      )

    assert {:ok, "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"} =
             MimeValidator.prepare(xlsx, "a.xlsx", nil)

    assert {:ok, "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"} =
             MimeValidator.prepare(xlsx, "a.zip", "application/zip")

    assert {:error, _} = MimeValidator.prepare("PK\x03\x04", "a.zip", nil)
  end
end
