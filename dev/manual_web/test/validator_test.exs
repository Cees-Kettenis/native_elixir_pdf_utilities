defmodule ManualWeb.ValidatorTest do
  use ExUnit.Case, async: true

  alias ManualWeb.Validator

  test "reads ordered PDF uploads and enforces the minimum count" do
    first = upload("first", "first.pdf", "application/pdf")
    second = upload("second", "second.pdf", "application/pdf")

    assert Validator.read_pdfs([first, second], 2, :merge) == {:ok, ["first", "second"]}

    assert {:error,
            {:invalid_input, %{stage: :manual_web, reason: :invalid_input, operation: :merge}}} =
             Validator.read_pdfs([first], 2, :merge)
  end

  test "prefers uploaded source files over pasted input" do
    html = upload("<h1>Uploaded</h1>", "document.html", "text/html")
    source = upload("/Uploaded 1", "source.pdf", "application/pdf")

    assert Validator.read_html(html, "<h1>Pasted</h1>") == {:ok, "<h1>Uploaded</h1>"}
    assert Validator.read_token_source(source, "/Pasted 2") == {:ok, "/Uploaded 1"}
    assert Validator.read_html(nil, "<h1>Pasted</h1>") == {:ok, "<h1>Pasted</h1>"}
    assert Validator.read_token_source(nil, "/Pasted 2") == {:ok, "/Pasted 2"}
  end

  test "normalizes HTML options and rejects unsupported choices" do
    assert {:ok,
            [
              margin: "20mm",
              page_size: {:letter, :landscape},
              unsupported_glyphs: :error
            ]} =
             Validator.html_options(%{
               "page_size" => "letter",
               "orientation" => "landscape",
               "margin" => "20mm",
               "unsupported_glyphs" => "error"
             })

    assert {:error, {:invalid_input, %{operation: :html_to_pdf}}} =
             Validator.html_options(%{"page_size" => "unknown"})
  end

  test "builds metadata patches with remove taking precedence" do
    assert Validator.info_patch(%{"Title" => "ignored"}) == {:ok, %{}}

    assert {:ok, %{title: nil, author: "Ada", modification_date: "D:202608"}} =
             Validator.info_patch(%{
               "title" => "Ignored",
               "remove_title" => "on",
               "author" => "Ada",
               "subject" => " ",
               "modification_date" => "D:202608"
             })
  end

  test "validates extraction modes and PDF response disposition" do
    assert Validator.text_mode("text_layout") == {:ok, {:text, [layout: true]}}
    assert Validator.text_mode("spans_visual") == {:ok, {:spans, [order: :visual]}}
    assert Validator.disposition(nil) == {:ok, :inline}
    assert Validator.disposition("attachment") == {:ok, :attachment}
    assert {:error, {:invalid_input, %{operation: :response}}} = Validator.disposition("other")
  end

  defp upload(bytes, filename, content_type) do
    path =
      Path.join(
        System.tmp_dir!(),
        "manual-web-#{System.unique_integer([:positive, :monotonic])}-#{filename}"
      )

    File.write!(path, bytes)
    on_exit(fn -> File.rm(path) end)

    %Plug.Upload{path: path, filename: filename, content_type: content_type}
  end
end
