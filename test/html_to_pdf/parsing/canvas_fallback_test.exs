defmodule NativeElixirPdfUtilities.HtmlToPdf.CanvasFallbackTest do
  use ExUnit.Case, async: true

  alias NativeElixirPdfUtilities.{HtmlToPdf, Text}
  alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, Style}

  test "scripting-disabled canvas renders fallback without bitmap sizing or script execution" do
    html =
      "<p>Before <canvas width=999999999999999999999 height=bad>Fallback <b>bold</b><script type='application/json' src='/never.json'>draw()</script><template><img src='/never.png'>Hidden</template></canvas> After</p>"

    assert {:ok, dom} = HtmlParser.parse_detailed(html)
    assert {:ok, tree} = Style.compute_detailed(dom)
    [canvas] = Enum.filter(hd(tree.children).children, &(Map.get(&1, :tag) == "canvas"))
    assert canvas.style.display == :inline
    assert Map.get(canvas.style, :image) == nil
    assert Map.get(canvas.style, :width) in [nil, :auto]

    assert {:ok, pdf} =
             HtmlToPdf.render(html,
               asset_resolver: fn request -> flunk("unexpected resource #{inspect(request)}") end
             )

    assert {:ok, text} = Text.extract(pdf)
    assert text =~ "Before Fallback bold After"
    refute text =~ "draw"
    refute text =~ "Hidden"
  end

  test "canvas is transparent and permits only the specified interactive fallback descendants" do
    for content <- [
          "<a href='https://example.test/'>Link</a>",
          "<button>Button</button>",
          "<input type=checkbox>",
          "<input type=radio>",
          "<input type=button value=Button>",
          "<select multiple><option>Choice</option></select>",
          "<select size=2><option>Choice</option></select>",
          "<span><canvas>Nested</canvas></span>"
        ] do
      assert {:ok, _} = HtmlToPdf.render("<canvas>#{content}</canvas>"), content
    end

    for content <- [
          "<input>",
          "<input type=range>",
          "<textarea>Text</textarea>",
          "<select><option>Choice</option></select>",
          "<span><input type=text></span>"
        ] do
      assert {:error, {:unsupported_html, diagnostic}} =
               HtmlParser.parse_detailed("<canvas>#{content}</canvas>")

      assert diagnostic.stage == :html
      assert diagnostic.message != ""
    end

    assert {:ok, _} = HtmlParser.parse_detailed("<canvas><div>Block fallback</div></canvas>")

    assert {:error, {:unsupported_html, _}} =
             HtmlParser.parse_detailed("<p><canvas><div>Wrong context</div></canvas></p>")
  end

  test "manual DOM canvas follows the same fallback content model" do
    dom = %{
      type: :document,
      children: [
        %{
          type: :element,
          tag: "canvas",
          attributes: %{},
          children: [%{type: :element, tag: "input", attributes: %{}, children: []}]
        }
      ]
    }

    assert {:error, {:invalid_document, diagnostic}} = Style.compute_detailed(dom)
    assert diagnostic.stage == :style and diagnostic.source == "canvas"
    assert diagnostic.message =~ "fallback controls"
  end

  test "unassigned light-tree styles remain document styles in source order" do
    html =
      "<style>p{color:red}</style><div><style>p{color:blue}</style><template shadowrootmode=open><p>Shadow</p></template></div><p>Light</p>"

    assert {:ok, dom} = HtmlParser.parse_detailed(html)
    assert {:ok, tree} = Style.compute_detailed(dom)
    p = Enum.find(tree.children, &(Map.get(&1, :tag) == "p"))
    assert p.style.color == {0, 0, 1}
    shadow = hd(tree.children).children |> Enum.find(&(Map.get(&1, :tag) == "p"))
    assert shadow.style.color == {0, 0, 0}
  end
end
