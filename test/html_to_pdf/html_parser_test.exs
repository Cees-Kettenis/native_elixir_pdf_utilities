defmodule NativeElixirPdfUtilities.HtmlToPdf.HtmlParserTest do
  use ExUnit.Case

  alias NativeElixirPdfUtilities.HtmlToPdf.HtmlParser

  test "parse accepts a strict paragraph document" do
    assert HtmlParser.parse("<p>Hello &amp; PDF</p>") ==
             {:ok,
              %{
                type: :document,
                children: [
                  %{
                    type: :element,
                    tag: "p",
                    attributes: %{},
                    children: [%{type: :text, text: "Hello & PDF"}]
                  }
                ]
              }}
  end

  test "parse rejects unsupported markup" do
    assert HtmlParser.parse("<div>Hello</div>") == {:error, :unsupported_html}
    assert HtmlParser.parse(:not_html) == {:error, :invalid_html}
  end
end
