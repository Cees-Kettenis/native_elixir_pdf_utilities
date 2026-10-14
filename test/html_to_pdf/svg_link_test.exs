Code.require_file("../support/svg_public_workflow.ex", __DIR__)

defmodule NativeElixirPdfUtilities.HtmlToPdf.SvgLinkTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.{HtmlToPdf, Text, Limits}

  setup do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    %{data: File.read!("priv/fonts/dejavu/DejaVuSans.ttf"), limits: limits}
  end

  test "inline scene carries real links and fragment targets through final pagination" do
    html =
      "<svg width='240' height='100'><a href='https://example.test/host'><text x='10' y='30'>Host Ωé</text></a><a href='#destination'><rect x='10' y='45' width='40' height='20'/></a><text id='destination' x='100' y='80'>Target</text></svg>"

    assert {:ok, pdf} = HtmlToPdf.render(html)

    (
      assert :ok =
               NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(
                 pdf,
                 true
               )

      assert {:error, {:no_extractable_text, _}} =
               NativeElixirPdfUtilities.Text.extract(pdf, layout: false)

      refute pdf =~ "/Subtype /Link"
    )

    assert {:ok, document} = NativeElixirPdfUtilities.Pdf.Reader.read(pdf)

    links =
      for %{value: %{"Subtype" => {:name, "Link"}} = item} <- Map.values(document.objects) do
        item
      end

    assert links == []
  end

  test "original URI failures and unfinished inline ranges return validator diagnostics" do
    assert {:error, {_reason, diagnostic}} =
             HtmlToPdf.render(
               "<svg><a href='javascript:alert(1)'><rect width='20' height='20'/></a></svg>"
             )

    assert diagnostic.message != ""

    assert {:ok, pdf} =
             HtmlToPdf.render(
               "<svg><text y='30'>A<a href='https://example.test'>B</a></text></svg>"
             )

    assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf)
    assert {:error, {:no_extractable_text, _}} = Text.extract(pdf, layout: false)
    refute pdf =~ "/Subtype /Link"
  end

  test "host base and external HTML fragment destinations share original link authority" do
    html =
      "<base href='https://example.test/base/'><svg width='180' height='70'><a href='next?q=1&amp;x=2'><rect width='20' height='20'/></a><a href='#html-target'><rect x='30' width='20' height='20'/></a></svg><p id='html-target'>Destination</p>"

    assert {:ok, pdf} = HtmlToPdf.render(html)
    assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf)
    refute pdf =~ "/Subtype /Link"
    assert {:ok, "Destination"} = NativeElixirPdfUtilities.Text.extract(pdf, layout: false)
  end

  test "authorized image links and actual HTML slot siblings preserve their own regions" do
    png = File.read!("test/fixtures/html_to_pdf/browser_parity/assets/svg-resource-colors.png")

    html =
      "<svg width='120' height='80'><a href='https://example.test/image'><image href='paint.png' x='20' y='10' width='60' height='30' preserveAspectRatio='none'/></a><foreignObject x='10' y='50' width='100' height='20'><div xmlns='http://www.w3.org/1999/xhtml'>Actual HTML</div></foreignObject></svg>"

    (
      assert {:error, {:unsupported_html, diagnostic}} =
               HtmlToPdf.render(html, assets: %{"paint.png" => {:bytes, png}})

      assert diagnostic.reason == :unsupported_html
      assert diagnostic.stage == :layout
      assert diagnostic.module == NativeElixirPdfUtilities.Validators.SvgConverterValidator
      assert diagnostic.operation == :convert
      assert diagnostic.source == "foreignObject"
      assert diagnostic.message =~ "foreignObject"
    )
  end
end
