defmodule NativeElixirPdfUtilities.HtmlToPdf.EmptyGeneratedContentStyleTest do
  use ExUnit.Case, async: true

  alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, Style}

  test "empty before and after content retains the styled pseudo-element" do
    for pseudo <- ["before", "after"], display <- ["inline", "block", "inline-block"] do
      html =
        "<style>p::#{pseudo}{content:'';display:#{display};width:30pt;height:20pt;background:red;border:2px solid blue}</style><p>Text</p>"

      assert {:ok, dom} = HtmlParser.parse_detailed(html)
      assert {:ok, %{children: [paragraph]}} = Style.compute_detailed(dom)
      generated = Enum.find(paragraph.children, &(Map.get(&1, :tag) == "::#{pseudo}"))

      assert generated != nil
      assert generated.style.width == 30.0 and generated.style.height == 20.0
      assert generated.style.background_color == {1.0, 0.0, 0.0}
      assert generated.style.border_widths.top == 1.5
      assert Enum.map(generated.children, & &1.text) == [""]
    end
  end

  test "generated content resolving to an empty string retains its box" do
    for content <- ["attr(data-missing)", "no-open-quote"] do
      html =
        "<style>p::before{content:#{content};display:block;width:30pt;height:20pt;background:red}</style><p>Text</p>"

      assert {:ok, dom} = HtmlParser.parse_detailed(html)
      assert {:ok, %{children: [paragraph]}} = Style.compute_detailed(dom)
      assert [%{tag: "::before", children: [%{text: ""}]} | _] = paragraph.children
    end
  end

  test "none, normal, omitted content and display none still suppress pseudo-elements" do
    for declaration <- [
          "content:none",
          "content:normal",
          "color:blue",
          "content:'';display:none"
        ] do
      html =
        "<style>p::before,p::after{display:block;width:30pt;height:20pt;background:red;#{declaration}}</style><p>Text</p>"

      assert {:ok, dom} = HtmlParser.parse_detailed(html)
      assert {:ok, %{children: [paragraph]}} = Style.compute_detailed(dom)
      assert [%{type: :text, text: "Text"}] = paragraph.children
    end
  end
end
