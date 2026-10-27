defmodule NativeElixirPdfUtilities.HtmlToPdf.HtmlContainerProfileTest do
  use ExUnit.Case, async: true

  alias NativeElixirPdfUtilities.HtmlToPdf.HtmlParser

  test "static containers preserve browser-admitted nesting without certifying HTML conformance" do
    for {html, parent, child_tags} <- [
          {"<address><h1>Heading</h1></address>", "address", ["h1"]},
          {"<hgroup><h1>First</h1><h2>Second</h2></hgroup>", "hgroup", ["h1", "h2"]},
          {"<header><footer>Footer</footer></header>", "header", ["footer"]}
        ] do
      assert {:ok, %{children: [%{tag: ^parent, children: children}]}} =
               HtmlParser.parse_detailed(html)

      assert Enum.map(children, & &1.tag) == child_tags
    end
  end
end
