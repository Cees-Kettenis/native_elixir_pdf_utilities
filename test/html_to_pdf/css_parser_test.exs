defmodule NativeElixirPdfUtilities.HtmlToPdf.CssParserTest do
  use ExUnit.Case

  alias NativeElixirPdfUtilities.HtmlToPdf.CssParser

  test "parse exposes the stylesheet parser boundary while behavior is pending" do
    assert CssParser.parse("p { color: red; }") == {:error, :not_implemented}
  end
end
