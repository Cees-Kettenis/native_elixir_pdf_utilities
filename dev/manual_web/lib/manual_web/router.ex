defmodule ManualWeb.Router do
  @moduledoc """
  Local HTTP endpoints for manually exercising every public utility workflow.
  """

  use Plug.Router

  alias ManualWeb.OpenApi
  alias ManualWeb.Page
  alias ManualWeb.Validator
  alias NativeElixirPdfUtilities.Diagnostics
  alias NativeElixirPdfUtilities.HtmlToPdf
  alias NativeElixirPdfUtilities.Info
  alias NativeElixirPdfUtilities.Limits
  alias NativeElixirPdfUtilities.Merge
  alias NativeElixirPdfUtilities.Outlines
  alias NativeElixirPdfUtilities.Split
  alias NativeElixirPdfUtilities.Text
  alias NativeElixirPdfUtilities.Tokenizer
  alias NativeElixirPdfUtilities.Transform

  @brand_banner_path Path.expand("../../../../assets/readme-banner.svg", __DIR__)
  @external_resource @brand_banner_path
  @brand_banner File.read!(@brand_banner_path)
  @favicon_path Path.expand("../../priv/static/favicon.ico", __DIR__)
  @external_resource @favicon_path
  @favicon File.read!(@favicon_path)
  @request_length Limits.defaults().max_aggregate_merge_input_bytes

  plug(Plug.Logger)
  plug(:match)

  plug(Plug.Parsers,
    parsers: [:urlencoded, :multipart],
    pass: ["*/*"],
    length: @request_length
  )

  plug(:dispatch)

  get "/" do
    send_html(conn, 200, Page.index())
  end

  get "/openapi.json" do
    body = OpenApi.document() |> Jason.encode_to_iodata!(pretty: true) |> IO.iodata_to_binary()

    conn
    |> security_headers()
    |> put_resp_content_type("application/json")
    |> send_resp(200, body)
  end

  get "/brand-banner.svg" do
    send_asset(conn, @brand_banner, "image/svg+xml")
  end

  get "/favicon.ico" do
    send_asset(conn, @favicon, "image/x-icon")
  end

  post "/merge" do
    with {:ok, pdfs} <- Validator.read_pdfs(conn.params["pdfs"], 2, :merge),
         {:ok, disposition} <- Validator.disposition(conn.params["disposition"]),
         {:ok, merged} <- Merge.merge(pdfs) do
      send_pdf(conn, merged, "merged.pdf", disposition)
    else
      {:error, _} = operation_error -> send_error(conn, operation_error)
    end
  end

  post "/transform/pick" do
    with {:ok, pdf} <- Validator.read_pdf(conn.params["pdf"], :pick_pages),
         {:ok, pages} <- Validator.page_selection(conn.params["pages"], false, :pick_pages),
         {:ok, disposition} <- Validator.disposition(conn.params["disposition"]),
         {:ok, transformed} <- Transform.pick_pages(pdf, pages) do
      send_pdf(conn, transformed, "picked-pages.pdf", disposition)
    else
      {:error, _} = operation_error -> send_error(conn, operation_error)
    end
  end

  post "/transform/delete" do
    with {:ok, pdf} <- Validator.read_pdf(conn.params["pdf"], :delete_pages),
         {:ok, pages} <- Validator.page_selection(conn.params["pages"], true, :delete_pages),
         {:ok, disposition} <- Validator.disposition(conn.params["disposition"]),
         {:ok, transformed} <- Transform.delete_pages(pdf, pages) do
      send_pdf(conn, transformed, "deleted-pages.pdf", disposition)
    else
      {:error, _} = operation_error -> send_error(conn, operation_error)
    end
  end

  post "/transform/rotate" do
    with {:ok, pdf} <- Validator.read_pdf(conn.params["pdf"], :rotate_pages),
         {:ok, degrees} <-
           Validator.integer(conn.params["degrees"], :rotate_pages, "enter an integer rotation"),
         {:ok, pages} <- Validator.page_selection(conn.params["pages"], true, :rotate_pages),
         {:ok, disposition} <- Validator.disposition(conn.params["disposition"]),
         options = if(pages == [], do: [], else: [pages: pages]),
         {:ok, transformed} <- Transform.rotate_pages(pdf, degrees, options) do
      send_pdf(conn, transformed, "rotated-pages.pdf", disposition)
    else
      {:error, _} = operation_error -> send_error(conn, operation_error)
    end
  end

  post "/split/by-page" do
    with {:ok, pdf} <- Validator.read_pdf(conn.params["pdf"], :split_by_page),
         {:ok, outputs} <- Split.by_page(pdf),
         {:ok, archive} <- zip_outputs(outputs, "page") do
      send_archive(conn, archive, "split-by-page.zip")
    else
      {:error, _} = operation_error -> send_error(conn, operation_error)
    end
  end

  post "/split/by-ranges" do
    with {:ok, pdf} <- Validator.read_pdf(conn.params["pdf"], :split_by_ranges),
         {:ok, ranges} <- Validator.page_ranges(conn.params["ranges"], :split_by_ranges),
         {:ok, outputs} <- Split.by_ranges(pdf, ranges),
         {:ok, archive} <- zip_outputs(outputs, "range") do
      send_archive(conn, archive, "split-by-ranges.zip")
    else
      {:error, _} = operation_error -> send_error(conn, operation_error)
    end
  end

  post "/split/after-page" do
    with {:ok, pdf} <- Validator.read_pdf(conn.params["pdf"], :split_after_page),
         {:ok, page} <-
           Validator.integer(conn.params["page"], :split_after_page, "enter a split page"),
         {:ok, {before_pdf, after_pdf}} <- Split.after_page(pdf, page),
         {:ok, archive} <- zip_outputs([before_pdf, after_pdf], "part") do
      send_archive(conn, archive, "split-after-page.zip")
    else
      {:error, _} = operation_error -> send_error(conn, operation_error)
    end
  end

  post "/html-to-pdf" do
    with {:ok, html} <- Validator.read_html(conn.params["html_file"], conn.params["html"]),
         {:ok, options} <- Validator.html_options(conn.params),
         {:ok, disposition} <- Validator.disposition(conn.params["disposition"]),
         {:ok, pdf} <- HtmlToPdf.render(html, options) do
      send_pdf(conn, pdf, "rendered.pdf", disposition)
    else
      {:error, _} = operation_error -> send_error(conn, operation_error)
    end
  end

  post "/outlines" do
    with {:ok, pdf} <- Validator.read_pdf(conn.params["pdf"], :get_outlines),
         {:ok, outlines} <- Outlines.get(pdf) do
      send_html(conn, 200, Page.outline_result("PDF outlines", outlines))
    else
      {:error, _} = operation_error -> send_error(conn, operation_error)
    end
  end

  post "/outlines/detect" do
    with {:ok, pdf} <- Validator.read_pdf(conn.params["pdf"], :detect_outlines),
         {:ok, outlines} <- Outlines.detect(pdf) do
      send_html(conn, 200, Page.outline_result("Detected PDF outlines", outlines))
    else
      {:error, _} = operation_error -> send_error(conn, operation_error)
    end
  end

  post "/outlines/automatic" do
    with {:ok, pdf} <- Validator.read_pdf(conn.params["pdf"], :automatic_outlines),
         {:ok, disposition} <- Validator.disposition(conn.params["disposition"]),
         {:ok, updated} <- Outlines.automatic(pdf) do
      send_pdf(conn, updated, "automatic-outlines.pdf", disposition)
    else
      {:error, _} = operation_error -> send_error(conn, operation_error)
    end
  end

  post "/outlines/update" do
    with {:ok, pdf} <- Validator.read_pdf(conn.params["pdf"], :put_outlines),
         {:ok, outlines} <- Validator.outline_items(conn.params["outlines"], :put_outlines),
         {:ok, disposition} <- Validator.disposition(conn.params["disposition"]),
         {:ok, updated} <- Outlines.put(pdf, outlines) do
      send_pdf(conn, updated, "updated-outlines.pdf", disposition)
    else
      {:error, _} = operation_error -> send_error(conn, operation_error)
    end
  end

  post "/text" do
    with {:ok, pdf} <- Validator.read_pdf(conn.params["pdf"], :extract_text),
         {:ok, mode} <- Validator.text_mode(conn.params["mode"]) do
      extract_text_response(conn, pdf, mode)
    else
      {:error, _} = operation_error -> send_error(conn, operation_error)
    end
  end

  post "/info" do
    with {:ok, pdf} <- Validator.read_pdf(conn.params["pdf"], :get_info),
         {:ok, inspection} <- inspect_pdf(pdf) do
      send_html(conn, 200, Page.term_result("PDF information", inspection))
    else
      {:error, _} = operation_error -> send_error(conn, operation_error)
    end
  end

  post "/info/update" do
    with {:ok, pdf} <- Validator.read_pdf(conn.params["pdf"], :put_info),
         {:ok, patch} <- Validator.info_patch(conn.params),
         {:ok, disposition} <- Validator.disposition(conn.params["disposition"]),
         {:ok, updated} <- Info.put(pdf, patch) do
      send_pdf(conn, updated, "updated-info.pdf", disposition)
    else
      {:error, _} = operation_error -> send_error(conn, operation_error)
    end
  end

  post "/tokenize" do
    with {:ok, source} <-
           Validator.read_token_source(conn.params["pdf"], conn.params["source"]) do
      tokens = source |> Tokenizer.new() |> Tokenizer.tokenize_all_with_spans()
      send_html(conn, 200, Page.term_result("Tokenizer output", tokens))
    else
      {:error, _} = operation_error -> send_error(conn, operation_error)
    end
  end

  match _ do
    send_html(conn, 404, Page.text_result("Not found", "No manual check exists at this path."))
  end

  defp extract_text_response(conn, pdf, mode) do
    result =
      case mode do
        {:text, options} -> Text.extract(pdf, options)
        {:spans, options} -> Text.extract_spans(pdf, options)
      end

    case {mode, result} do
      {{:text, _options}, {:ok, text}} ->
        send_html(conn, 200, Page.text_result("Extracted text", text))

      {{:spans, _options}, {:ok, spans}} ->
        send_html(conn, 200, Page.json_result("Positioned text spans", spans))

      {_mode, {:error, _} = operation_error} ->
        send_error(conn, operation_error)
    end
  end

  defp inspect_pdf(pdf) do
    case Info.encrypted?(pdf) do
      {:ok, true} ->
        {:ok,
         %{
           encrypted?: true,
           metadata: :unavailable_without_decryption,
           page_count: :unavailable_without_decryption,
           page_sizes: :unavailable_without_decryption
         }}

      {:ok, false} ->
        with {:ok, metadata} <- Info.get(pdf),
             {:ok, page_count} <- Info.page_count(pdf),
             {:ok, page_sizes} <- Info.page_sizes(pdf) do
          {:ok,
           %{
             encrypted?: false,
             metadata: metadata,
             page_count: page_count,
             page_sizes: page_sizes
           }}
        end

      {:error, _} = operation_error ->
        operation_error
    end
  end

  defp send_pdf(conn, pdf, filename, disposition) do
    conn
    |> security_headers()
    |> put_resp_header("content-type", "application/pdf")
    |> put_resp_header("content-disposition", "#{disposition}; filename=\"#{filename}\"")
    |> send_resp(200, pdf)
  end

  defp zip_outputs(outputs, prefix) do
    files =
      outputs
      |> Enum.with_index(1)
      |> Enum.map(fn {pdf, index} ->
        {String.to_charlist("#{prefix}-#{index}.pdf"), pdf}
      end)

    case :zip.create(~c"outputs.zip", files, [:memory]) do
      {:ok, {_filename, archive}} ->
        {:ok, archive}

      {:error, reason} ->
        Diagnostics.error(
          :manual_web,
          :archive_error,
          "could not create split archive: #{reason}",
          operation: :split
        )
    end
  end

  defp send_archive(conn, archive, filename) do
    conn
    |> security_headers()
    |> put_resp_header("content-type", "application/zip")
    |> put_resp_header("content-disposition", "attachment; filename=\"#{filename}\"")
    |> send_resp(200, archive)
  end

  defp send_error(conn, operation_error) do
    send_html(conn, 422, Page.error_result(operation_error))
  end

  defp send_html(conn, status, body) do
    conn
    |> security_headers()
    |> put_resp_content_type("text/html")
    |> send_resp(status, body)
  end

  defp send_asset(conn, asset, content_type) do
    conn
    |> security_headers()
    |> put_resp_header("cache-control", "public, max-age=3600")
    |> put_resp_header("content-type", content_type)
    |> send_resp(200, asset)
  end

  defp security_headers(conn) do
    conn
    |> put_resp_header(
      "content-security-policy",
      "default-src 'none'; img-src 'self'; style-src 'unsafe-inline'; form-action 'self'; base-uri 'none'; frame-ancestors 'none'"
    )
    |> put_resp_header("referrer-policy", "no-referrer")
    |> put_resp_header("x-content-type-options", "nosniff")
  end
end
