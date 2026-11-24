defmodule NativeElixirPdfUtilities.HtmlToPdf.DisplayContentsReplacedTest do
  use ExUnit.Case, async: true
  alias NativeElixirPdfUtilities.{Forms, HtmlToPdf, Text}
  alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, Style}
  alias NativeElixirPdfUtilities.Pdf.Reader
  alias NativeElixirPdfUtilities.TestSupport.Paths

  test "suppressed controls and child documents produce no text or annotations in either form mode" do
    fixture = Paths.fixture("html_to_pdf/browser_parity/html_display_contents_controls.html")
    html = File.read!(fixture)

    for mode <- [:static, :interactive] do
      assert {:ok, pdf} = HtmlToPdf.render(html, forms: mode, base_url: Path.dirname(fixture))
      assert {:ok, "Before\nContainer text\nAfter"} = Text.extract(pdf)
      assert {:ok, []} = Forms.fields(pdf)
      assert {:ok, doc} = Reader.read(pdf)
      assert [page] = doc.pages
      assert {:ok, dictionary} = Reader.resolve(doc, {:ref, page.ref})
      assert Map.get(dictionary, "Annots", []) == []
    end
  end

  test "the winning display on the control, button, slot or wrapper determines what prints" do
    html = """
    <style>textarea, button, div, slot {display:contents} #visible {display:block}</style>
    <div><textarea id="visible" name="visible">Ordinary value</textarea></div>
    <button>Button text</button><slot>Slot text</slot>
    """

    assert {:ok, pdf} = HtmlToPdf.render(html, forms: :static)
    assert {:ok, text} = Text.extract(pdf)
    for visible <- ["Ordinary value", "Button text", "Slot text"], do: assert(text =~ visible)

    assert {:ok, pdf} = HtmlToPdf.render(html)
    assert {:ok, [%{name: "visible", value: "Ordinary value"}]} = Forms.fields(pdf)
  end

  test "the HTML-hosted SVG suppression rule stops at the SVG namespace boundary" do
    html = """
    <svg style="display:contents"><svg style="display:contents"><rect width="4" height="4"/></svg></svg>
    """

    assert {:ok, dom} = HtmlParser.parse_detailed(html)
    assert {:ok, %{children: [outer]}} = Style.prepare_svg_detailed(dom)
    assert outer.style.display == :none
    assert [inner] = outer.children
    assert inner.style.display == :contents
    assert [%{tag: "rect"}] = inner.children
  end

  test "suppressed embedded documents and SVG paint do not request their resources" do
    html = """
    <style>.gone {display:contents}</style>
    <object class=gone data='absent.html'>Hidden fallback</object><embed class=gone src='absent.pdf'>
    <iframe class=gone src='child.html'></iframe>
    <svg class=gone width='40' height='20'><image href='nested.png' width='40' height='20'/></svg>
    <p>After</p>
    """

    assert {:ok, pdf} =
             HtmlToPdf.render(html,
               asset_resolver: fn req -> flunk("unexpected resource #{inspect(req)}") end
             )

    assert {:ok, "After"} = Text.extract(pdf)
  end

  test "an eager image validates its source but paints only when its computed display allows it" do
    bytes = File.read!(Paths.fixture("html_to_pdf/browser_parity/assets/iframe-bars.png"))

    for tag <- ["img", "input type=image"] do
      html = "<#{tag} src='bars.png' style='display:contents'><p>After</p>"

      assert {:ok, pdf} = HtmlToPdf.render(html, assets: %{"bars.png" => {:bytes, bytes}})
      assert {:ok, "After"} = Text.extract(pdf)
      refute pdf =~ "/Subtype /Image"
      assert {:ok, []} = Forms.fields(pdf)

      assert {:error, {_, %{source: "bars.png"}}} = HtmlToPdf.render(html)

      visible = String.replace(html, "display:contents", "display:block")

      assert {:ok, pdf} =
               HtmlToPdf.render(visible, forms: :static, assets: %{"bars.png" => {:bytes, bytes}})

      assert pdf =~ "/Subtype /Image"
    end
  end
end
