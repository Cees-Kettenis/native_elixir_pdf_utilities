defmodule ManualWeb.Validator do
  @moduledoc """
  Validates browser form input before the manual application calls the library.
  """

  alias NativeElixirPdfUtilities.Diagnostics

  @page_sizes %{
    "a5" => :a5,
    "a4" => :a4,
    "a3" => :a3,
    "b5" => :b5,
    "b4" => :b4,
    "jis_b5" => :jis_b5,
    "jis_b4" => :jis_b4,
    "letter" => :letter,
    "legal" => :legal,
    "ledger" => :ledger
  }
  @info_fields %{
    "title" => :title,
    "author" => :author,
    "subject" => :subject,
    "keywords" => :keywords,
    "producer" => :producer,
    "creation_date" => :creation_date,
    "modification_date" => :modification_date
  }

  @type detailed_error :: {atom(), Diagnostics.diagnostic()}

  @doc "Reads one uploaded PDF into memory."
  @spec read_pdf(term(), atom()) :: {:ok, binary()} | {:error, detailed_error()}
  def read_pdf(upload, operation) do
    read_upload(upload, "PDF", operation)
  end

  @doc "Reads at least `minimum` uploaded PDFs into memory in browser order."
  @spec read_pdfs(term(), pos_integer(), atom()) ::
          {:ok, [binary()]} | {:error, detailed_error()}
  def read_pdfs(value, minimum, operation) do
    uploads =
      case value do
        uploads when is_list(uploads) -> uploads
        nil -> []
        upload -> [upload]
      end

    case is_integer(minimum) and minimum > 0 and length(uploads) >= minimum do
      true ->
        uploads
        |> Enum.reduce_while({:ok, []}, fn upload, {:ok, pdfs} ->
          case read_pdf(upload, operation) do
            {:ok, pdf} -> {:cont, {:ok, [pdf | pdfs]}}
            {:error, _} = upload_error -> {:halt, upload_error}
          end
        end)
        |> case do
          {:ok, pdfs} -> {:ok, Enum.reverse(pdfs)}
          {:error, _} = upload_error -> upload_error
        end

      false ->
        error(operation, "select at least #{minimum} PDF files")
    end
  end

  @doc "Selects uploaded HTML when present, otherwise validates pasted HTML."
  @spec read_html(term(), term()) :: {:ok, binary()} | {:error, detailed_error()}
  def read_html(upload, pasted_html) do
    case upload do
      %Plug.Upload{} -> read_upload(upload, "HTML", :html_to_pdf)
      _ -> required_text(pasted_html, "upload an HTML file or paste HTML", :html_to_pdf)
    end
  end

  @doc "Selects an uploaded tokenizer source when present, otherwise validates pasted bytes."
  @spec read_token_source(term(), term()) :: {:ok, binary()} | {:error, detailed_error()}
  def read_token_source(upload, pasted_source) do
    case upload do
      %Plug.Upload{} -> read_upload(upload, "tokenizer source", :tokenize)
      _ -> required_text(pasted_source, "upload a PDF or paste PDF syntax", :tokenize)
    end
  end

  @doc "Validates page geometry and glyph options from the HTML form."
  @spec html_options(map()) :: {:ok, keyword()} | {:error, detailed_error()}
  def html_options(params) do
    page_size = Map.get(@page_sizes, Map.get(params, "page_size"))

    orientation =
      case Map.get(params, "orientation") do
        "portrait" -> :portrait
        "landscape" -> :landscape
        _ -> nil
      end

    unsupported_glyphs =
      case Map.get(params, "unsupported_glyphs", "replace") do
        "replace" -> :replace
        "error" -> :error
        _ -> nil
      end

    cond do
      is_nil(page_size) ->
        error(:html_to_pdf, "select a supported page size")

      is_nil(orientation) ->
        error(:html_to_pdf, "select portrait or landscape orientation")

      is_nil(unsupported_glyphs) ->
        error(:html_to_pdf, "select a supported missing-glyph policy")

      true ->
        options = [page_size: {page_size, orientation}, unsupported_glyphs: unsupported_glyphs]

        case optional_text(Map.get(params, "margin")) do
          nil -> {:ok, options}
          margin -> {:ok, Keyword.put(options, :margin, margin)}
        end
    end
  end

  @doc "Validates the requested text extraction mode."
  @spec text_mode(term()) ::
          {:ok, {:text, keyword()} | {:spans, keyword()}} | {:error, detailed_error()}
  def text_mode(mode) do
    case mode do
      "text_layout" -> {:ok, {:text, [layout: true]}}
      "text_source" -> {:ok, {:text, [layout: false]}}
      "spans_source" -> {:ok, {:spans, [order: :source]}}
      "spans_visual" -> {:ok, {:spans, [order: :visual]}}
      _ -> error(:extract_text, "select a supported extraction mode")
    end
  end

  @doc "Builds an Info.put/2 patch from field values and removal checkboxes."
  @spec info_patch(map()) :: {:ok, map()} | {:error, detailed_error()}
  def info_patch(params) do
    case is_map(params) do
      true ->
        patch =
          Enum.reduce(@info_fields, %{}, fn {form_field, info_field}, patch ->
            case Map.get(params, "remove_#{form_field}") do
              "on" -> Map.put(patch, info_field, nil)
              _ -> maybe_put_text(patch, info_field, Map.get(params, form_field))
            end
          end)

        {:ok, patch}

      false ->
        error(:put_info, "metadata form input is malformed")
    end
  end

  @doc "Validates whether a generated PDF should open inline or download."
  @spec disposition(term()) :: {:ok, :inline | :attachment} | {:error, detailed_error()}
  def disposition(value) do
    case value do
      "attachment" -> {:ok, :attachment}
      "inline" -> {:ok, :inline}
      nil -> {:ok, :inline}
      _ -> error(:response, "output disposition must be inline or attachment")
    end
  end

  defp read_upload(upload, label, operation) do
    case upload do
      %Plug.Upload{path: path} when is_binary(path) ->
        case File.read(path) do
          {:ok, bytes} -> {:ok, bytes}
          {:error, reason} -> error(operation, "could not read #{label} upload: #{reason}")
        end

      _ ->
        error(operation, "select a #{label} file")
    end
  end

  defp required_text(value, message, operation) do
    case optional_text(value) do
      nil -> error(operation, message)
      text -> {:ok, text}
    end
  end

  defp optional_text(value) do
    case value do
      value when is_binary(value) ->
        case String.trim(value) do
          "" -> nil
          _ -> value
        end

      _ ->
        nil
    end
  end

  defp maybe_put_text(map, key, value) do
    case optional_text(value) do
      nil -> map
      text -> Map.put(map, key, text)
    end
  end

  defp error(operation, message) do
    Diagnostics.error(:manual_web, :invalid_input, message,
      operation: operation,
      module: __MODULE__
    )
  end
end
