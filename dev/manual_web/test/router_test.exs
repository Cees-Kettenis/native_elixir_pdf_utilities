defmodule ManualWeb.RouterTest do
  use ExUnit.Case, async: true

  import Plug.Conn
  import Plug.Test

  alias ManualWeb.Router
  alias NativeElixirPdfUtilities.HtmlToPdf
  alias NativeElixirPdfUtilities.Info
  alias NativeElixirPdfUtilities.Outlines

  @router_options Router.init([])

  test "serves the interface on the root route" do
    response = request(:get, "/")

    assert response.status == 200
    assert response.resp_body =~ "src=\"/brand-banner.svg\""
    assert response.resp_body =~ "href=\"/favicon.ico\""
    refute response.resp_body =~ "Manual PDF checks"
    assert response.resp_body =~ "Merge PDFs"
    assert response.resp_body =~ "Transform pages"
    assert response.resp_body =~ "Split PDFs"
    assert response.resp_body =~ "HTML to PDF"
    assert response.resp_body =~ "PDF outlines and bookmarks"
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
               "/outlines",
               "/outlines/automatic",
               "/outlines/detect",
               "/outlines/update",
               "/split/after-page",
               "/split/by-page",
               "/split/by-ranges",
               "/text",
               "/tokenize",
               "/transform/delete",
               "/transform/pick",
               "/transform/rotate"
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
        "html" =>
          "<html><head><title>Manual</title></head><body><h1>Manual</h1><p>Rendered</p></body></html>",
        "page_size" => "a4",
        "orientation" => "portrait",
        "margin" => "20mm",
        "unsupported_glyphs" => "replace",
        "outlines_mode" => "headings",
        "disposition" => "inline"
      })

    assert response.status == 200
    assert response.resp_body =~ "%PDF-"
    assert get_resp_header(response, "content-type") == ["application/pdf"]

    assert get_resp_header(response, "content-disposition") == [
             "inline; filename=\"rendered.pdf\""
           ]

    assert {:ok, [%{title: "Manual"}]} = Outlines.get(response.resp_body)
  end

  test "merges uploaded PDFs and returns an attachment when requested" do
    first = outlined_page_pdf("First PDF")
    second = outlined_page_pdf("Second PDF")

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

    assert {:ok, outlines} = Outlines.get(response.resp_body)
    assert Enum.map(outlines, &{&1.title, &1.page}) == [{"First PDF", 1}, {"Second PDF", 2}]
  end

  test "picks, deletes, and rotates pages" do
    pdf = multi_page_pdf()

    picked =
      post("/transform/pick", %{
        "pdf" => upload(pdf, "pick.pdf"),
        "pages" => "3,1",
        "disposition" => "inline"
      })

    assert picked.status == 200
    assert Info.page_count(picked.resp_body) == {:ok, 2}

    assert {:ok, picked_outlines} = Outlines.get(picked.resp_body)
    assert Enum.map(picked_outlines, &{&1.title, &1.page}) == [{"First", 2}, {"Third", 1}]

    deleted =
      post("/transform/delete", %{
        "pdf" => upload(pdf, "delete.pdf"),
        "pages" => "2",
        "disposition" => "inline"
      })

    assert deleted.status == 200
    assert Info.page_count(deleted.resp_body) == {:ok, 2}
    assert {:ok, deleted_outlines} = Outlines.get(deleted.resp_body)
    assert Enum.map(deleted_outlines, & &1.title) == ["First", "Third"]

    rotated =
      post("/transform/rotate", %{
        "pdf" => upload(pdf, "rotate.pdf"),
        "degrees" => "90",
        "pages" => "2",
        "disposition" => "inline"
      })

    assert rotated.status == 200
    assert {:ok, sizes} = Info.page_sizes(rotated.resp_body)
    assert Enum.map(sizes, & &1.rotation) == [0, 90, 0]
  end

  test "downloads every split mode as a ZIP of valid PDFs" do
    pdf = multi_page_pdf()

    by_page = post("/split/by-page", %{"pdf" => upload(pdf, "by-page.pdf")})
    assert by_page.status == 200
    assert get_resp_header(by_page, "content-type") == ["application/zip"]
    assert by_page.resp_body |> unzip_pdfs() |> Enum.map(&pdf_page_count/1) == [1, 1, 1]

    by_ranges =
      post("/split/by-ranges", %{
        "pdf" => upload(pdf, "by-ranges.pdf"),
        "ranges" => "1-2,2-3"
      })

    assert by_ranges.status == 200
    assert by_ranges.resp_body |> unzip_pdfs() |> Enum.map(&pdf_page_count/1) == [2, 2]

    after_page =
      post("/split/after-page", %{
        "pdf" => upload(pdf, "after-page.pdf"),
        "page" => "1"
      })

    assert after_page.status == 200
    assert after_page.resp_body |> unzip_pdfs() |> Enum.map(&pdf_page_count/1) == [1, 2]
  end

  test "inspects, detects, automatically writes, and replaces outlines" do
    {:ok, pdf} =
      HtmlToPdf.render("""
      <h1>Detected report</h1>
      <p>A sufficiently long ordinary paragraph establishes the body text size.</p>
      <h2>Detected section</h2>
      <p>Another ordinary paragraph makes visual heading detection predictable.</p>
      """)

    inspection = post("/outlines", %{"pdf" => upload(pdf, "inspect-outlines.pdf")})
    assert inspection.status == 200
    assert inspection.resp_body =~ "PDF outlines"
    assert inspection.resp_body =~ "[]"

    detection = post("/outlines/detect", %{"pdf" => upload(pdf, "detect-outlines.pdf")})
    assert detection.status == 200
    assert detection.resp_body =~ "Detected report"
    assert detection.resp_body =~ "Detected section"

    automatic =
      post("/outlines/automatic", %{
        "pdf" => upload(pdf, "automatic-outlines.pdf"),
        "disposition" => "inline"
      })

    assert automatic.status == 200
    assert {:ok, [%{title: "Detected report"}]} = Outlines.get(automatic.resp_body)

    exact =
      post("/outlines/update", %{
        "pdf" => upload(multi_page_pdf(), "exact-outlines.pdf"),
        "outlines" =>
          ~s([{"title":"Overview","page":1,"view":["fit_h",90],"children":[{"title":"Details","page":2,"view":"fit_b"}]}]),
        "disposition" => "attachment"
      })

    assert exact.status == 200

    assert get_resp_header(exact, "content-disposition") == [
             "attachment; filename=\"updated-outlines.pdf\""
           ]

    assert {:ok, [%{title: "Overview", view: {:fit_h, 90}, children: [details]}]} =
             Outlines.get(exact.resp_body)

    assert details.title == "Details"
    assert details.view == :fit_b
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

  defp multi_page_pdf do
    {:ok, pdf} =
      HtmlToPdf.render(
        """
        <h1>First</h1><p>First page body text.</p>
        <div style="break-before: page"><h1>Second</h1><p>Second page body text.</p></div>
        <div style="break-before: page"><h1>Third</h1><p>Third page body text.</p></div>
        """,
        outlines: :headings
      )

    pdf
  end

  defp outlined_page_pdf(title) do
    {:ok, pdf} =
      HtmlToPdf.render("<h1>#{title}</h1><p>Body text for the generated page.</p>",
        outlines: :headings
      )

    pdf
  end

  defp unzip_pdfs(archive) do
    {:ok, files} = :zip.extract(archive, [:memory])
    Enum.map(files, fn {_filename, pdf} -> pdf end)
  end

  defp pdf_page_count(pdf) do
    {:ok, count} = Info.page_count(pdf)
    count
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
