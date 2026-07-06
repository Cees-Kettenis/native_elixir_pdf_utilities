defmodule NativeElixirPdfUtilities.HtmlToPdf.HtmlParserTest do
  use ExUnit.Case

  alias NativeElixirPdfUtilities.HtmlToPdf.HtmlParser

  test "parse exposes the parser boundary while behavior is pending" do
    assert HtmlParser.parse("<p>Hello</p>") == {:error, :not_implemented}
  end
end
