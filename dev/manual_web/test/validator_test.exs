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

    assert {:ok, options} =
             Validator.html_options(%{
               "page_size" => "a4",
               "orientation" => "portrait",
               "unsupported_glyphs" => "replace",
               "outlines_mode" => "headings"
             })

    assert options[:outlines] == :headings

    assert {:ok, exact_options} =
             Validator.html_options(%{
               "page_size" => "a4",
               "orientation" => "portrait",
               "unsupported_glyphs" => "replace",
               "outlines_mode" => "exact",
               "outlines" => ~s([{"title":"Start","page":1,"view":"fit"}])
             })

    assert exact_options[:outlines] == [%{title: "Start", page: 1, view: :fit}]

    assert {:error, {:invalid_input, %{operation: :html_to_pdf}}} =
             Validator.html_options(%{"page_size" => "unknown"})

    assert {:error, {:invalid_input, %{operation: :html_to_pdf}}} =
             Validator.html_options(%{
               "page_size" => "a4",
               "orientation" => "portrait",
               "outlines_mode" => "unsupported"
             })
  end

  test "parses page selections, ranges, integers, and outline JSON" do
    assert Validator.page_selection("3, 1-2", false, :pick_pages) == {:ok, [3, 1..2//1]}
    assert Validator.page_selection("", true, :delete_pages) == {:ok, []}

    assert {:error, {:invalid_input, %{operation: :pick_pages}}} =
             Validator.page_selection("", false, :pick_pages)

    assert {:error, {:invalid_input, %{operation: :pick_pages}}} =
             Validator.page_selection("one", false, :pick_pages)

    assert Validator.page_ranges("1-2, 4-5", :split_by_ranges) ==
             {:ok, [1..2//1, 4..5//1]}

    assert {:error, {:invalid_input, %{operation: :split_by_ranges}}} =
             Validator.page_ranges("1,2-3", :split_by_ranges)

    assert Validator.integer("-90", :rotate_pages, "enter rotation") == {:ok, -90}

    assert {:error, {:invalid_input, %{operation: :rotate_pages}}} =
             Validator.integer("90deg", :rotate_pages, "enter rotation")

    json =
      ~s([{"title":"Report","page":1,"view":["xyz",null,90,null],"open":false,"children":[{"title":"Child","page":2,"view":"fit_b"}]}])

    assert {:ok, [item]} = Validator.outline_items(json, :put_outlines)
    assert item.view == {:xyz, nil, 90, nil}
    assert item.open == false
    assert hd(item.children).view == :fit_b

    assert {:error, {:invalid_input, %{operation: :put_outlines}}} =
             Validator.outline_items("{}", :put_outlines)
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
