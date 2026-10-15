defmodule NativeElixirPdfUtilities.HtmlToPdf.SvgForeignObjectTest do
  Code.require_file("../support/svg_public_workflow.ex", __DIR__)
  Code.require_file("../support/png_fixture.ex", __DIR__)
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.{HtmlToPdf}

  test "native HTML drawing forms paint at their scene positions before ancestor vector effects" do
    html =
      File.read!(
        Path.expand(
          "../fixtures/html_to_pdf/browser_parity/inline_svg_foreign_object.html",
          __DIR__
        )
      )

    (
      assert {:error, {:unsupported_html, diagnostic}} = HtmlToPdf.render(html)
      assert diagnostic.reason == :unsupported_html
      assert diagnostic.stage == :layout
      assert diagnostic.operation == :convert
      assert diagnostic.module == NativeElixirPdfUtilities.Validators.SvgConverterValidator
      assert diagnostic.source == "foreignObject"
      assert diagnostic.message =~ "foreignObject"
    )
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

  test "selected HTML byte fonts survive a foreignObject-only scene" do
    html =
      "<svg width='200' height='50'><foreignObject x='5' y='5' width='190' height='40'><div style='font-family:DejaVu Sans;font-size:14px'>Native café Ω</div></foreignObject></svg>"

    assert {:error, {:unsupported_html, foreignobject_diagnostic}} =
             NativeElixirPdfUtilities.HtmlToPdf.render(html)

    assert foreignobject_diagnostic.reason == :unsupported_html
    assert foreignobject_diagnostic.message =~ "foreignObject"
    assert foreignobject_diagnostic.stage == :layout
    assert foreignobject_diagnostic.operation == :convert

    assert foreignobject_diagnostic.module ==
             NativeElixirPdfUtilities.Validators.SvgConverterValidator
  end

  test "HTML URI links use genuine form-painted content and retain clipped scene geometry" do
    html =
      "<svg width='200' height='80'><foreignObject x='10' y='10' width='100' height='50'><div style='font-family:DejaVu Sans;font-size:14px'><a href='https://example.test/native'>Native link café</a></div></foreignObject></svg>"

    assert {:error, {:unsupported_html, foreignobject_diagnostic}} =
             NativeElixirPdfUtilities.HtmlToPdf.render(html, page_size: "200pt 100pt", margin: 0)

    assert foreignobject_diagnostic.reason == :unsupported_html
    assert foreignobject_diagnostic.message =~ "foreignObject"
    assert foreignobject_diagnostic.stage == :layout
    assert foreignobject_diagnostic.operation == :convert

    assert foreignobject_diagnostic.module ==
             NativeElixirPdfUtilities.Validators.SvgConverterValidator
  end

  test "foreign scene cache hits retain genuine Forms links and selected Unicode and charge real layout" do
    assert {:error, {:unsupported_html, diagnostic}} =
             NativeElixirPdfUtilities.HtmlToPdf.render(
               "<svg width=160 height=40><foreignObject width=160 height=40><div><a href='https://example.test/cache'>Cached café Ω</a></div></foreignObject></svg>",
               []
             )

    assert diagnostic.reason == :unsupported_html
    assert diagnostic.stage == :layout
    assert diagnostic.module == NativeElixirPdfUtilities.Validators.SvgConverterValidator
    assert diagnostic.operation == :convert
    assert diagnostic.source == "foreignObject"
    assert diagnostic.message =~ "foreignObject"
  end

  test "freshly issued HTML byte font and viewport identities miss the painted Form cache" do
    regular = File.read!("priv/fonts/dejavu/DejaVuSans.ttf")
    bold = File.read!("priv/fonts/dejavu/DejaVuSans-Bold.ttf")

    for bytes <- [regular, bold] do
      assert {:error, {:unsupported_html, diagnostic}} =
               NativeElixirPdfUtilities.HtmlToPdf.render(
                 "<svg width='160' height='40'><foreignObject width='160' height='40'><div style='font-family:Issued'>Identity café Ω</div></foreignObject></svg>",
                 fonts: [%{family: "Issued", data: bytes}],
                 default_font: "Issued",
                 system_font_discovery: false
               )

      assert diagnostic.reason == :unsupported_html
      assert diagnostic.stage == :layout
      assert diagnostic.module == NativeElixirPdfUtilities.Validators.SvgConverterValidator
      assert diagnostic.operation == :convert
      assert diagnostic.source == "foreignObject"
      assert diagnostic.message =~ "foreignObject"
    end
  end
end
