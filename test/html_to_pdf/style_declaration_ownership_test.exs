defmodule NativeElixirPdfUtilities.HtmlToPdf.StyleDeclarationOwnershipTest do
  use ExUnit.Case, async: false

  alias NativeElixirPdfUtilities.HtmlToPdf.{CssParser, HtmlParser, Style}
  alias NativeElixirPdfUtilities.Validators.CssValueValidator

  test "unrelated valid page and font descriptors cannot blame an invalid inline style" do
    font_data =
      File.read!(
        Path.join(:code.priv_dir(:native_elixir_pdf_utilities), "fonts/dejavu/DejaVuSans.ttf")
      )

    for css <- [
          "@page{size:200pt 300pt;margin:10pt}",
          "@page{size:200pt 300pt;margin:10pt}@font-face{font-family:Unused;src:url('/unused.ttf')}"
        ] do
      html = "<style>#{css}</style><p style='width:bananas'>Invalid original</p>"
      assert {:ok, dom} = HtmlParser.parse_document_detailed(html)

      assert {:error, {:invalid_css, diagnostic}} =
               Style.compute_detailed(dom,
                 system_font_discovery: false,
                 assets: %{"/unused.ttf" => {:bytes, font_data}}
               )

      assert diagnostic.stage == :css and diagnostic.reason == :invalid_css
      assert diagnostic.source == "width:bananas"
      assert diagnostic.line == 1 and diagnostic.column == 1

      assert diagnostic.message ==
               ~s(line 1: declaration "width:bananas" is invalid or unsupported)
    end
  end

  test "parser owns style declaration boundaries with opaque braces and inactive media" do
    css =
      "@page{size:200pt 300pt}p::before{content:'@page{width:bananas}';color:red}@media screen{p{width:bananas}}@media print{p{height:4pt}}"

    assert {:ok, records} = CssParser.style_declaration_records(css)

    assert Enum.map(records, & &1.source.text) == [
             "content:'@page{width:bananas}'",
             "color:red",
             "height:4pt"
           ]

    assert CssValueValidator.declaration_error(css, records) == nil
  end

  test "same text in an earlier string cannot steal the failing declaration coordinates" do
    css = "@page{size:200pt 300pt}p{font-family:'width:bananas';color:red}\n  p{width:bananas}"
    assert {:ok, records} = CssParser.style_declaration_records(css)
    assert {:invalid_css, diagnostic} = CssValueValidator.declaration_error(css, records)
    assert diagnostic.source == "width:bananas"
    assert diagnostic.line == 2 and diagnostic.column == 5
    assert diagnostic.message == ~s(line 2: declaration "width:bananas" is invalid or unsupported)
  end

  test "public diagnostic traversal uses the same viewport media context as the actual cascade" do
    css = "@page{size:200pt 300pt}@media print and (min-width:1px){p{width:bananas}}"
    html = "<style>#{css}</style><p style='height:bananas'>Invalid original</p>"
    assert {:ok, dom} = HtmlParser.parse_document_detailed(html)

    for {viewport, source} <- [{{300, 400}, "width:bananas"}, {{0.5, 400}, "height:bananas"}] do
      assert {:error, {:invalid_css, diagnostic}} =
               Style.compute_detailed(dom, print_viewport: viewport, system_font_discovery: false)

      assert diagnostic.stage == :css and diagnostic.reason == :invalid_css
      assert diagnostic.source == source
      assert diagnostic.message == ~s(line 1: declaration "#{source}" is invalid or unsupported)
    end
  end
end
