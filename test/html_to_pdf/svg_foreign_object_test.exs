defmodule NativeElixirPdfUtilities.HtmlToPdf.SvgForeignObjectTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.{HtmlToPdf, Text}
  alias NativeElixirPdfUtilities.Validators.SvgFormSlotValidator

  test "native HTML drawing forms paint at their scene positions before ancestor vector effects" do
    html =
      File.read!(
        Path.expand(
          "../fixtures/html_to_pdf/browser_parity/inline_svg_foreign_object.html",
          __DIR__
        )
      )

    assert {:ok, pdf} = HtmlToPdf.render(html)
    refute pdf =~ "/Subtype /Image"
    assert pdf =~ "/NativeHtmlSurface"
    assert pdf =~ "/SMask"
    assert pdf =~ "/ca 0.6"
    assert {:ok, text} = Text.extract(pdf, layout: false)

    for original <- [
          "selected café Ω",
          "Two native lines",
          "Rotated HTML",
          "Genuine glyphs",
          "Masked native HTML",
          "Partial selection"
        ] do
      assert text =~ original
    end
  end

  test "HTML namespaces remain genuine and unsupported declarations diagnose through HTML input authority" do
    for namespace <- ["urn:other", "javascript:run()"] do
      html =
        "<svg><foreignObject width='100' height='40'><div xmlns='#{namespace}'>Text</div></foreignObject></svg>"

      assert {:error, {:unsupported_html, %{stage: :html, source: source, message: message}}} =
               HtmlToPdf.render(html)

      assert source =~ "xmlns"
      assert message != ""
    end
  end

  test "selected HTML byte fonts survive a foreignObject-only scene" do
    html =
      "<svg width='200' height='50'><foreignObject x='5' y='5' width='190' height='40'><div style='font-family:DejaVu Sans;font-size:14px'>Native café Ω</div></foreignObject></svg>"

    assert {:ok, pdf} = HtmlToPdf.render(html)
    assert {:ok, text} = Text.extract(pdf, layout: false)
    assert text == "Native café Ω"
    assert pdf =~ "/FontFile2"
    refute pdf =~ "/Subtype /Image"
  end

  test "HTML URI links use genuine form-painted content and retain clipped scene geometry" do
    html =
      "<svg width='200' height='80'><foreignObject x='10' y='10' width='100' height='50'><div style='font-family:DejaVu Sans;font-size:14px'><a href='https://example.test/native'>Native link café</a></div></foreignObject></svg>"

    assert {:ok, pdf} = HtmlToPdf.render(html, page_size: "200pt 100pt", margin: 0)
    assert {:ok, text} = Text.extract(pdf, layout: false)
    assert text == "Native link café"
    refute pdf =~ "/Subtype /Image"
    assert {:ok, document} = NativeElixirPdfUtilities.Pdf.Reader.read(pdf)

    links =
      document.objects
      |> Map.values()
      |> Enum.filter(&(is_map(&1.value) and &1.value["Subtype"] == {:name, "Link"}))

    assert links != []

    for link <- links do
      assert link.value["A"]["URI"] == {:string, "https://example.test/native"}
      [left, bottom, right, top] = link.value["Rect"]
      assert left >= 7.5 and right <= 82.5
      assert top <= 92.5 and bottom >= 55
    end
  end

  test "annotation-only writer regions reject malformed coordinates and nonnavigation links" do
    alias NativeElixirPdfUtilities.HtmlToPdf.PdfWriter

    base = %{
      type: :link_region,
      x: 1,
      y: 1,
      width: 20,
      height: 10,
      link_url: "https://example.test/native"
    }

    for region <- [
          Map.put(base, :width, -1),
          Map.put(base, :x, 1.0e100),
          Map.put(base, :link_url, "javascript:run()"),
          Map.put(base, :text, "Invisible surrogate")
        ] do
      assert {:error, {reason, %{stage: stage, reason: diagnostic_reason, message: message}}} =
               PdfWriter.render([%{size: {100, 50}, boxes: [region]}])

      assert reason == diagnostic_reason
      assert stage == :pdf
      assert message != ""
    end
  end

  test "native slots cannot be composed with an unissued HTML subtree" do
    assert {:module, _} = NativeElixirPdfUtilities.HtmlToPdf.SvgBackend.load_native()

    assert {:ok, source} =
             NativeElixirPdfUtilities.HtmlToPdf.SvgBackend.NativeBytes.vectorize_slots(
               "<svg width='100' height='40'><rect id='slot' width='100' height='40'/></svg>",
               [],
               ["slot"]
             )

    assert {:error, {:invalid_document, %{source: "slot", operation: :compose}}} =
             SvgFormSlotValidator.prepare(source, %{})
  end
end
