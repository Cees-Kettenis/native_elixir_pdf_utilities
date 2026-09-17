defmodule NativeElixirPdfUtilities.Validators.MimeValidator do
  @moduledoc false
  alias NativeElixirPdfUtilities.{Diagnostics, Limits}

  # Signature coverage adapted from the caller's Sigportal.ImageUtils. Container
  # signatures deliberately identify containers rather than a particular document.
  @extensions %{
    ".png" => "image/png",
    ".jpg" => "image/jpeg",
    ".jpeg" => "image/jpeg",
    ".gif" => "image/gif",
    ".webp" => "image/webp",
    ".bmp" => "image/bmp",
    ".tif" => "image/tiff",
    ".tiff" => "image/tiff",
    ".ico" => "image/vnd.microsoft.icon",
    ".svg" => "image/svg+xml",
    ".pdf" => "application/pdf",
    ".zip" => "application/zip",
    ".gz" => "application/gzip",
    ".txt" => "text/plain",
    ".csv" => "text/csv",
    ".json" => "application/json",
    ".xml" => "application/xml",
    ".html" => "text/html",
    ".htm" => "text/html",
    ".md" => "text/markdown",
    ".mp3" => "audio/mpeg",
    ".wav" => "audio/wav",
    ".mp4" => "video/mp4",
    ".ogg" => "application/ogg",
    ".doc" => "application/msword",
    ".xls" => "application/vnd.ms-excel",
    ".ppt" => "application/vnd.ms-powerpoint",
    ".docx" => "application/vnd.openxmlformats-officedocument.wordprocessingml.document",
    ".xlsx" => "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
    ".pptx" => "application/vnd.openxmlformats-officedocument.presentationml.presentation"
  }
  @aliases %{
    "image/jpg" => "image/jpeg",
    "application/x-zip-compressed" => "application/zip",
    "application/x-gzip" => "application/gzip",
    "text/xml" => "application/xml",
    "image/x-icon" => "image/vnd.microsoft.icon"
  }

  @ole_types ["application/msword", "application/vnd.ms-excel", "application/vnd.ms-powerpoint"]
  @zip_types Enum.map([".docx", ".xlsx", ".pptx"], &Map.fetch!(@extensions, &1))

  @doc false
  @spec validate(term()) :: {:ok, String.t()} | {:error, {atom(), map()}}
  def validate(value) do
    case value do
      value when is_binary(value) ->
        if byte_size(value) <= Limits.get(:max_pdf_info_value_bytes) and
             Regex.match?(~r/\A[a-zA-Z0-9!#$&^_.+-]+\/[a-zA-Z0-9!#$&^_.+-]+\z/, value) do
          value = String.downcase(value)
          {:ok, Map.get(@aliases, value, value)}
        else
          error("MIME type must be a type/subtype without parameters")
        end

      _ ->
        error("MIME type must be a string")
    end
  end

  @doc false
  @spec prepare(binary(), String.t(), term()) :: {:ok, String.t()} | {:error, {atom(), map()}}
  def prepare(bytes, filename, supplied) do
    extension = Map.get(@extensions, String.downcase(Path.extname(filename)))
    detected = signature(bytes)

    with {:ok, supplied} <- if(is_nil(supplied), do: {:ok, nil}, else: validate(supplied)),
         {:ok, detected} <- container_type(detected, bytes, extension, supplied) do
      evidence = detected || extension

      cond do
        detected && extension && not compatible?(detected, extension) ->
          error("filename extension conflicts with detected MIME type #{detected}")

        supplied && supplied != "application/octet-stream" && evidence &&
            not compatible?(evidence, supplied) ->
          error("supplied MIME type conflicts with detected or filename type #{evidence}")

        true ->
          {:ok, evidence || supplied || "application/octet-stream"}
      end
    end
  end

  defp signature(bytes) do
    case bytes do
      <<137, "PNG", 13, 10, 26, 10, _::binary>> ->
        "image/png"

      <<255, 216, 255, _::binary>> ->
        "image/jpeg"

      <<"GIF", version::binary-size(3), _::binary>> when version in ["87a", "89a"] ->
        "image/gif"

      <<"RIFF", _::binary-size(4), "WEBP", _::binary>> ->
        "image/webp"

      <<"RIFF", _::binary-size(4), "WAVE", _::binary>> ->
        "audio/wav"

      <<"BM", _::binary>> ->
        "image/bmp"

      <<"II", 42, 0, _::binary>> ->
        "image/tiff"

      <<"MM", 0, 42, _::binary>> ->
        "image/tiff"

      <<0, 0, 1, 0, _::binary>> ->
        "image/vnd.microsoft.icon"

      <<0xD0, 0xCF, 0x11, 0xE0, 0xA1, 0xB1, 0x1A, 0xE1, _::binary>> ->
        :ole

      <<"PK", a, b, _::binary>> when {a, b} in [{3, 4}, {5, 6}, {7, 8}] ->
        :zip

      <<"%PDF-", _::binary>> ->
        "application/pdf"

      <<31, 139, _::binary>> ->
        "application/gzip"

      <<"ID3", _::binary>> ->
        "audio/mpeg"

      <<"OggS", _::binary>> ->
        "application/ogg"

      <<_::binary-size(4), "ftyp", brand::binary-size(4), _::binary>>
      when brand in ["isom", "iso2", "mp41", "mp42", "M4V ", "MSNV", "avc1"] ->
        "video/mp4"

      _ ->
        nil
    end
  end

  defp container_type(detected, bytes, extension, supplied) do
    case detected do
      :ole ->
        hint = extension || supplied

        if hint in @ole_types or
             hint in [nil, "application/octet-stream", "application/x-ole-storage"],
           do: {:ok, if(hint in @ole_types, do: hint, else: "application/x-ole-storage")},
           else: error("filename or supplied MIME type conflicts with an OLE container")

      :zip ->
        # Inspect only bounded archive metadata. Never extract or inflate entries.
        if byte_size(bytes) > Limits.get(:max_mime_container_bytes) do
          Diagnostics.error(
            :limits,
            :resource_limit_exceeded,
            "ZIP MIME inspection exceeds max_mime_container_bytes"
          )
        else
          with {:ok, names} <- zip_names(bytes) do
            candidates =
              for {entry, ext} <- [
                    {"word/document.xml", ".docx"},
                    {"xl/workbook.xml", ".xlsx"},
                    {"ppt/presentation.xml", ".pptx"}
                  ],
                  Map.has_key?(names, "[Content_Types].xml") and Map.has_key?(names, entry),
                  do: @extensions[ext]

            case candidates do
              [] -> {:ok, "application/zip"}
              [mime] -> {:ok, mime}
              _ -> error("ZIP contains conflicting Office document types")
            end
          end
        end

      mime ->
        {:ok, mime}
    end
  end

  defp compatible?(detected, hint) do
    detected == hint or
      (detected in @zip_types and hint == "application/zip") or
      (detected in @ole_types and hint == "application/x-ole-storage")
  end

  @spec zip_names(binary()) :: {:ok, %{optional(binary()) => true}} | {:error, {atom(), map()}}
  defp zip_names(bytes) do
    # ZIP fixes the end record at 22 bytes and its comment length at 16 bits.
    start = max(byte_size(bytes) - 65_557, 0)
    tail = binary_part(bytes, start, byte_size(bytes) - start)
    records = :binary.matches(tail, <<"PK", 5, 6>>)

    record =
      Enum.find_value(Enum.reverse(records), fn {offset, _} ->
        rest = binary_part(tail, offset + 4, byte_size(tail) - offset - 4)

        case rest do
          <<disk::little-16, directory_disk::little-16, disk_entries::little-16,
            entries::little-16, size::little-32, directory_offset::little-32,
            comment_size::little-16, comment::binary>>
          when byte_size(comment) == comment_size ->
            {disk, directory_disk, disk_entries, entries, size, directory_offset, start + offset}

          _ ->
            nil
        end
      end)

    case record do
      {0, 0, entries, entries, size, offset, ending}
      when entries < 65_535 and offset + size == ending ->
        if entries > Limits.get(:max_mime_container_entries) do
          Diagnostics.error(
            :limits,
            :resource_limit_exceeded,
            "ZIP MIME inspection exceeds max_mime_container_entries"
          )
        else
          zip_directory(binary_part(bytes, offset, size), entries, %{})
        end

      _ ->
        error(
          "ZIP directory is malformed; split archives and ZIP64 MIME inspection are unsupported"
        )
    end
  end

  @spec zip_directory(binary(), non_neg_integer(), %{optional(binary()) => true}) ::
          {:ok, %{optional(binary()) => true}} | {:error, {atom(), map()}}
  defp zip_directory(bytes, remaining, names) do
    case {remaining, bytes} do
      {0, <<>>} ->
        {:ok, names}

      {remaining,
       <<"PK", 1, 2, _::binary-size(24), name_size::little-16, extra_size::little-16,
         comment_size::little-16, _::binary-size(12), rest::binary>>}
      when remaining > 0 ->
        case rest do
          <<name::binary-size(^name_size), _::binary-size(^extra_size),
            _::binary-size(^comment_size), tail::binary>> ->
            if Map.has_key?(names, name),
              do: error("ZIP directory contains duplicate entry names"),
              else: zip_directory(tail, remaining - 1, Map.put(names, name, true))

          _ ->
            error("ZIP directory entry is truncated")
        end

      _ ->
        error("ZIP directory count or entry signature is malformed")
    end
  end

  defp error(message), do: Diagnostics.error(:attachments, :invalid_mime_type, message)
end
