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

    with {:ok, outlines} <- html_outlines(params) do
      cond do
        is_nil(page_size) ->
          error(:html_to_pdf, "select a supported page size")

        is_nil(orientation) ->
          error(:html_to_pdf, "select portrait or landscape orientation")

        is_nil(unsupported_glyphs) ->
          error(:html_to_pdf, "select a supported missing-glyph policy")

        true ->
          options = [page_size: {page_size, orientation}, unsupported_glyphs: unsupported_glyphs]

          options =
            case outlines do
              nil -> options
              outlines -> Keyword.put(options, :outlines, outlines)
            end

          case optional_text(Map.get(params, "margin")) do
            nil -> {:ok, options}
            margin -> {:ok, Keyword.put(options, :margin, margin)}
          end
      end
    end
  end

  @doc "Parses comma-separated page numbers and inclusive ranges such as 1,3-5."
  @spec page_selection(term(), boolean(), atom()) ::
          {:ok, [pos_integer() | Range.t()]} | {:error, detailed_error()}
  def page_selection(value, allow_empty, operation) do
    case {optional_text(value), allow_empty} do
      {nil, true} ->
        {:ok, []}

      {nil, false} ->
        error(operation, "enter at least one page number or range")

      {selection, _allow_empty} ->
        selection
        |> String.split(",", trim: true)
        |> Enum.map(&String.trim/1)
        |> parse_page_selectors(operation)
    end
  end

  @doc "Parses comma-separated inclusive page ranges such as 1-3,8-10."
  @spec page_ranges(term(), atom()) :: {:ok, [Range.t()]} | {:error, detailed_error()}
  def page_ranges(value, operation) do
    with {:ok, selectors} <- page_selection(value, false, operation),
         true <- Enum.all?(selectors, &match?(%Range{}, &1)) do
      {:ok, selectors}
    else
      false -> error(operation, "enter ranges in start-end form")
      {:error, _error} = selection_error -> selection_error
    end
  end

  @doc "Parses a required integer used by page and rotation operations."
  @spec integer(term(), atom(), String.t()) :: {:ok, integer()} | {:error, detailed_error()}
  def integer(value, operation, message) do
    case optional_text(value) do
      nil ->
        error(operation, message)

      value ->
        case Integer.parse(value) do
          {integer, ""} -> {:ok, integer}
          _ -> error(operation, message)
        end
    end
  end

  @doc "Parses exact outline items from the manual form's JSON representation."
  @spec outline_items(term(), atom()) :: {:ok, list()} | {:error, detailed_error()}
  def outline_items(value, operation) do
    case optional_text(value) do
      nil ->
        error(operation, "enter an outline JSON array")

      value ->
        case Jason.decode(value) do
          {:ok, items} when is_list(items) -> {:ok, normalize_outline_json(items)}
          _ -> error(operation, "outline data must be a valid JSON array")
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

  defp html_outlines(params) do
    case Map.get(params, "outlines_mode", "none") do
      "none" -> {:ok, nil}
      "headings" -> {:ok, :headings}
      "exact" -> outline_items(Map.get(params, "outlines"), :html_to_pdf)
      _ -> error(:html_to_pdf, "select a supported outline mode")
    end
  end

  defp parse_page_selectors(parts, operation) do
    case parts do
      [] ->
        error(operation, "enter at least one page number or range")

      parts ->
        parts
        |> Enum.reduce_while({:ok, []}, fn part, {:ok, selectors} ->
          selector =
            case Regex.run(~r/\A([0-9]+)\s*-\s*([0-9]+)\z/, part) do
              [_, first, last] ->
                with {first, ""} <- Integer.parse(first),
                     {last, ""} <- Integer.parse(last) do
                  {:ok, first..last//1}
                else
                  _ -> :error
                end

              nil ->
                case Integer.parse(part) do
                  {page, ""} -> {:ok, page}
                  _ -> :error
                end
            end

          case selector do
            {:ok, selector} -> {:cont, {:ok, [selector | selectors]}}
            :error -> {:halt, error(operation, "page selections must use 1,3-5 syntax")}
          end
        end)
        |> case do
          {:ok, selectors} -> {:ok, Enum.reverse(selectors)}
          {:error, _error} = selection_error -> selection_error
        end
    end
  end

  defp normalize_outline_json(value) do
    case value do
      items when is_list(items) ->
        Enum.map(items, &normalize_outline_json/1)

      item when is_map(item) ->
        Map.new(item, fn {key, value} ->
          case key do
            "title" -> {:title, value}
            "page" -> {:page, value}
            "open" -> {:open, value}
            "children" -> {:children, normalize_outline_json(value)}
            "view" -> {:view, normalize_outline_view(value)}
            key -> {key, value}
          end
        end)

      value ->
        value
    end
  end

  defp normalize_outline_view(value) do
    case value do
      "fit" -> :fit
      "fit_b" -> :fit_b
      ["fit_h", top] -> {:fit_h, top}
      ["fit_v", left] -> {:fit_v, left}
      ["fit_bh", top] -> {:fit_bh, top}
      ["fit_bv", left] -> {:fit_bv, left}
      ["fit_r", left, bottom, right, top] -> {:fit_r, left, bottom, right, top}
      ["xyz", left, top, zoom] -> {:xyz, left, top, zoom}
      value -> value
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
