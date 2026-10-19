defmodule NativeElixirPdfUtilities.HtmlToPdf.MathLinkEmissionTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.{HtmlToPdf, Text}
  alias NativeElixirPdfUtilities.Pdf.Reader

  test "enclosing URI links annotate inline Math glyphs and table paint" do
    for equation <- [
          "<math><mi>x</mi><mo>+</mo><mn>2</mn></math>",
          "<math><mfrac><mn>1</mn><mn>2</mn></mfrac></math>",
          "<math><mtable><mtr style='background:lightyellow'><mtd style='background:lightblue'><mn>12</mn></mtd></mtr></mtable></math>"
        ] do
      html = "<a href='https://example.test/equation'>#{equation}</a>"
      assert {:ok, pdf} = HtmlToPdf.render(html, system_font_discovery: false)
      assert {:ok, context} = Reader.read_validated(pdf)
      links = annotations(context)
      assert links != []

      for link <- links do
        assert link["A"]["URI"] == {:string, "https://example.test/equation"}
        [left, bottom, right, top] = link["Rect"]
        assert right > left and top > bottom
      end

      assert {:ok, text} = Text.extract(pdf)
      assert String.trim(text) != ""
    end
  end

  test "enclosing block equations retain fragment destinations after page breaks" do
    html =
      "<style>html,body{margin:0}p{margin:0}a{text-decoration:none}</style><p id=target>Destination</p><a href='#target'><math display=block style='break-before:page'><mfrac><mn>3</mn><mn>4</mn></mfrac></math></a>"

    assert {:ok, pdf} =
             HtmlToPdf.render(html,
               page_size: {180, 120},
               margin: 10,
               system_font_discovery: false
             )

    assert {:ok, context} = Reader.read_validated(pdf)
    assert length(context.pages) == 2
    links = annotations(context)
    assert links != []

    for link <- links do
      assert [{:ref, page}, {:name, "XYZ"}, _, _, nil] = link["Dest"]
      assert page == hd(context.pages).ref
    end
  end

  test "links inside Math table cell text use native child paint" do
    for href <- ["https://example.test/cell", "#destination"] do
      html =
        "<p id=destination>Target</p><math><mtable><mtr><mtd><mtext><a href='#{href}'>Cell link</a></mtext></mtd></mtr></mtable></math>"

      assert {:ok, pdf} = HtmlToPdf.render(html, system_font_discovery: false)
      assert {:ok, context} = Reader.read_validated(pdf)
      assert annotations(context) != []
      assert {:ok, text} = Text.extract(pdf)
      assert text =~ "Cell link"
    end
  end

  test "enclosing links respect the clipped equation viewport" do
    html =
      "<style>html,body{margin:0}a{text-decoration:none}</style><div style='width:18pt;height:18pt;overflow:hidden'><a href='https://example.test/clipped'><math><mn>123456789</mn></math></a></div>"

    assert {:ok, pdf} =
             HtmlToPdf.render(html,
               page_size: {180, 120},
               margin: 10,
               system_font_discovery: false
             )

    assert {:ok, context} = Reader.read_validated(pdf)
    links = annotations(context)
    assert links != []

    for link <- links do
      [left, bottom, right, top] = link["Rect"]
      assert right > left and top > bottom
      assert left >= 9.75 and right <= 28.25, inspect(link["Rect"])
      assert bottom >= 91.75 and top <= 110.25, inspect(link["Rect"])
    end
  end

  test "child-document equations preserve scoped destinations and viewport clipping" do
    child =
      "<style>html,body{margin:0}a{text-decoration:none}</style><p id=target style='margin:0'>Child target</p><div style='width:24pt;height:18pt;overflow:hidden'><a href='#target'><math><mn>123456</mn></math></a></div>"

    encoded =
      child
      |> String.replace("&", "&amp;")
      |> String.replace("<", "&lt;")
      |> String.replace("\"", "&quot;")

    html =
      "<style>html,body{margin:0}</style><p id=target style='margin:0'>Parent target</p><iframe style='border:0;width:48pt;height:42pt' srcdoc=\"#{encoded}\"></iframe>"

    assert {:ok, pdf} =
             HtmlToPdf.render(html,
               page_size: {180, 120},
               margin: 10,
               system_font_discovery: false
             )

    assert {:ok, context} = Reader.read_validated(pdf)
    links = annotations(context)
    assert links != []

    for link <- links do
      assert [{:ref, page}, {:name, "XYZ"}, x, y, nil] = link["Dest"]
      assert page == hd(context.pages).ref
      assert x >= 9.75 and x <= 58.25
      assert y < 100
      [left, bottom, right, top] = link["Rect"]
      assert left >= 9.75 and right <= 58.25, inspect(link["Rect"])
      assert top > bottom
    end
  end

  test "Math link validator rejects unauthenticated fragment destinations and conflicting links" do
    alias NativeElixirPdfUtilities.Validators.MathLayoutValidator

    for style <- [
          %{link_url: "javascript:alert(1)"},
          %{link_destination: %{target: "invented"}},
          %{link_url: "https://example.test", link_destination: "#invented"},
          nil
        ] do
      assert {:error, {:invalid_layout, diagnostic}} =
               MathLayoutValidator.validate_link_style(style, "math")

      assert diagnostic.stage == :layout
      assert diagnostic.operation == :measure_math
      assert diagnostic.source == "math"
      assert diagnostic.message =~ "validated URI"
    end

    assert :ok =
             MathLayoutValidator.validate_link_style(%{link_url: "https://example.test"}, "math")
  end

  defp annotations(context) do
    for {_ref, %{value: %{"Subtype" => {:name, "Link"}} = value}} <- context.document.objects,
        do: value
  end
end
