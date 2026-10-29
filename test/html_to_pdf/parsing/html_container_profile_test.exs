defmodule NativeElixirPdfUtilities.HtmlToPdf.HtmlContainerProfileTest do
  use ExUnit.Case, async: true

  alias NativeElixirPdfUtilities.HtmlToPdf.HtmlParser

  test "summaries and fieldset legends retain heading groups and their subtitles" do
    for {parent, caption, attributes} <- [
          {"details", "summary", ""},
          {"details", "summary", " open"},
          {"fieldset", "legend", ""}
        ] do
      html =
        "<#{parent}#{attributes}><#{caption}><hgroup><h2>Heading</h2><p>Subtitle</p></hgroup></#{caption}><p>Body</p></#{parent}>"

      assert {:ok, %{children: [%{tag: ^parent, children: [label, body]}]}} =
               HtmlParser.parse_detailed(html)

      assert label.tag == caption
      assert [%{tag: "hgroup", children: [heading, subtitle]}] = label.children
      assert %{tag: "h2", children: [%{type: :text, text: "Heading"}]} = heading
      assert %{tag: "p", children: [%{type: :text, text: "Subtitle"}]} = subtitle
      assert body.tag == "p"
    end
  end

  test "caption content models still reject unrelated block elements" do
    for {parent, caption} <- [{"details", "summary"}, {"fieldset", "legend"}],
        tag <- ["div", "p"] do
      html = "<#{parent}><#{caption}><#{tag}>Invalid</#{tag}></#{caption}></#{parent}>"

      assert {:error, {:unsupported_html, diagnostic}} = HtmlParser.parse_detailed(html)
      assert diagnostic.stage == :html and diagnostic.reason == :unsupported_html
      assert diagnostic.source == "<#{tag}>"
      assert diagnostic.message =~ "unsupported or malformed"
      assert diagnostic.line == 1 and is_integer(diagnostic.column)
    end
  end

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
