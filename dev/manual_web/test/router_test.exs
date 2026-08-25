defmodule ManualWeb.RouterTest do
  use ExUnit.Case, async: true

  import Plug.Conn
  import Plug.Test

  alias ManualWeb.Router
  alias NativeElixirPdfUtilities.HtmlToPdf
  alias NativeElixirPdfUtilities.Info

  @router_options Router.init([])

  test "serves the interface on the root route" do
    response = request(:get, "/")

    assert response.status == 200
    assert response.resp_body =~ "src=\"/brand-banner.svg\""
    assert response.resp_body =~ "href=\"/favicon.ico\""
    refute response.resp_body =~ "Manual PDF checks"
    assert response.resp_body =~ "Merge PDFs"
    assert response.resp_body =~ "HTML to PDF"
    assert response.resp_body =~ "Extract text"
    assert response.resp_body =~ "Inspect PDF information"
    assert get_resp_header(response, "content-security-policy") != []
  end

  test "serves an OpenAPI description for every operation" do
    response = request(:get, "/openapi.json")
    document = Jason.decode!(response.resp_body)

    assert response.status == 200

    assert Map.keys(document["paths"]) |> Enum.sort() ==
             Enum.sort([
               "/",
               "/brand-banner.svg",
               "/favicon.ico",
               "/html-to-pdf",
               "/info",
               "/info/update",
               "/merge",
               "/openapi.json",
               "/text",
               "/tokenize"
             ])
  end

  test "serves the README banner and generated favicon" do
    banner = request(:get, "/brand-banner.svg")
    favicon = request(:get, "/favicon.ico")

    assert banner.status == 200
    assert get_resp_header(banner, "content-type") == ["image/svg+xml"]
    assert banner.resp_body =~ "Native Elixir PDF Utilities"

    assert favicon.status == 200
    assert get_resp_header(favicon, "content-type") == ["image/x-icon"]
    assert <<0, 0, 1, 0, 6, 0, _rest::binary>> = favicon.resp_body
  end

  test "renders uploaded or pasted HTML to an inline PDF" do
    response =
      post("/html-to-pdf", %{
        "html" => "<html><head><title>Manual</title></head><body><p>Rendered</p></body></html>",
        "page_size" => "a4",
        "orientation" => "portrait",
        "margin" => "20mm",
        "unsupported_glyphs" => "replace",
        "disposition" => "inline"
      })

    assert response.status == 200
    assert response.resp_body =~ "%PDF-"
    assert get_resp_header(response, "content-type") == ["application/pdf"]

    assert get_resp_header(response, "content-disposition") == [
             "inline; filename=\"rendered.pdf\""
           ]
  end

  test "merges uploaded PDFs and returns an attachment when requested" do
    first = rendered_pdf("First PDF")
    second = rendered_pdf("Second PDF")

    response =
      post("/merge", %{
        "pdfs" => [upload(first, "first.pdf"), upload(second, "second.pdf")],
        "disposition" => "attachment"
      })

    assert response.status == 200
    assert response.resp_body =~ "%PDF-"

    assert get_resp_header(response, "content-disposition") == [
             "attachment; filename=\"merged.pdf\""
           ]

    assert Info.page_count(response.resp_body) == {:ok, 2}
  end

  test "extracts plain text and positioned spans" do
    pdf = rendered_pdf("Manual extraction")

    text_response =
      post("/text", %{
        "pdf" => upload(pdf, "text.pdf"),
        "mode" => "text_layout"
      })

    assert text_response.status == 200
    assert text_response.resp_body =~ "Manual extraction"

    spans_response =
      post("/text", %{
        "pdf" => upload(pdf, "spans.pdf"),
        "mode" => "spans_visual"
      })

    assert spans_response.status == 200
    assert spans_response.resp_body =~ "Positioned text spans"
    assert spans_response.resp_body =~ "&quot;page_count&quot;"
    assert spans_response.resp_body =~ "Manual extraction"
  end

  test "inspects and updates PDF information" do
    pdf = rendered_pdf("Info text", title: "Original title", author: "Original author")

    inspection = post("/info", %{"pdf" => upload(pdf, "inspect.pdf")})

    assert inspection.status == 200
    assert inspection.resp_body =~ "Original title"
    assert inspection.resp_body =~ "page_count: 1"
    assert inspection.resp_body =~ "encrypted?: false"

    update =
      post("/info/update", %{
        "pdf" => upload(pdf, "update.pdf"),
        "title" => "Updated title",
        "remove_author" => "on",
        "modification_date" => "2026-08-25T14:30:00",
        "disposition" => "inline"
      })

    assert update.status == 200

    assert {:ok,
            %{
              title: "Updated title",
              author: nil,
              modification_date: ~N[2026-08-25 14:30:00]
            }} = Info.get(update.resp_body)
  end

  test "shows tokenizer tokens with byte spans" do
    response = post("/tokenize", %{"source" => "<< /Type /Example /Count 2 >>"})

    assert response.status == 200
    assert response.resp_body =~ "Tokenizer output"
    assert response.resp_body =~ ":dict_start"
    assert response.resp_body =~ "{:name, &quot;Type&quot;}"
    assert response.resp_body =~ "from: 0"
  end

  test "renders structured input errors and a not-found page" do
    invalid = post("/merge", %{"pdfs" => []})
    missing = request(:get, "/missing")

    assert invalid.status == 422
    assert invalid.resp_body =~ ":invalid_input"
    assert invalid.resp_body =~ "select at least 2 PDF files"
    assert missing.status == 404
  end

  defp request(method, path) do
    method
    |> conn(path)
    |> Router.call(@router_options)
  end

  defp post(path, params) do
    :post
    |> conn(path)
    |> Map.put(:params, params)
    |> Map.put(:body_params, params)
    |> Router.call(@router_options)
  end

  defp rendered_pdf(text, metadata \\ []) do
    {:ok, pdf} = HtmlToPdf.render("<p>#{text}</p>", metadata: metadata)
    pdf
  end

  defp upload(bytes, filename) do
    path =
      Path.join(
        System.tmp_dir!(),
        "manual-web-#{System.unique_integer([:positive, :monotonic])}-#{filename}"
      )

    File.write!(path, bytes)
    on_exit(fn -> File.rm(path) end)

    %Plug.Upload{path: path, filename: filename, content_type: "application/pdf"}
  end
end
