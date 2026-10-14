Code.require_file("../support/svg_public_workflow.ex", __DIR__)

defmodule NativeElixirPdfUtilities.HtmlToPdf.SvgFiltersTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.{HtmlToPdf, Limits, Text}

  setup do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    %{limits: limits}
  end

  test "static graphs paint real effected shapes and selected text at the group position" do
    html =
      File.read!(
        Path.expand("../fixtures/html_to_pdf/browser_parity/inline_svg_filters.html", __DIR__)
      )

    assert {:ok, pdf} = HtmlToPdf.render(html)
    assert pdf =~ "/Subtype /Image"

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
  end

  test "empty, hidden and fully clipped effected text produces no logical surrogate" do
    for attributes <- [
          "opacity='0'",
          "visibility='hidden'",
          "display='none'",
          "transform='translate(500 0)'"
        ] do
      assert {:ok, pdf} =
               HtmlToPdf.render(
                 "<svg width='200' height='60'><defs><filter id='f'><feGaussianBlur stdDeviation='1'/></filter></defs><g filter='url(#f)' #{attributes}><text x='5' y='30' font-family='DejaVu Sans'>Hidden</text></g></svg>"
               )

      assert {:error, {:no_extractable_text, _}} = Text.extract(pdf, layout: false)
      refute pdf =~ "/ActualText"
    end
  end

  test "original HTML under an ancestor filter supplies actual painted content and text" do
    assert {:error, {:unsupported_html, diagnostic}} =
             HtmlToPdf.render(
               "<svg><defs><filter id='f'><feGaussianBlur/></filter></defs><g filter='url(#f)'><foreignObject width='100' height='30'><div>Original HTML</div></foreignObject></g></svg>"
             )

    assert diagnostic.reason == :unsupported_html
    assert diagnostic.stage == :layout
    assert diagnostic.module == NativeElixirPdfUtilities.Validators.SvgConverterValidator
    assert diagnostic.operation == :convert
    assert diagnostic.source == "foreignObject"
    assert diagnostic.message =~ "foreignObject"
  end

  test "HTML filter inputs retain original Unicode, ordinary CID forms and clipped URI geometry" do
    html =
      File.read!(
        Path.expand(
          "../fixtures/html_to_pdf/browser_parity/inline_svg_html_filters.html",
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

    linked =
      "<svg width='200' height='80'><defs><filter id='f'><feGaussianBlur stdDeviation='1'/></filter><clipPath id='clip'><rect x='10' y='10' width='90' height='45'/></clipPath></defs><g filter='url(#f)' clip-path='url(#clip)'><foreignObject x='10' y='10' width='100' height='50'><div style='font-family:DejaVu Sans;font-size:14px'><a href='https://example.test/native'>Native link café</a></div></foreignObject></g></svg>"

    (
      assert {:error, {:unsupported_html, diagnostic}} =
               HtmlToPdf.render(linked, page_size: "200pt 100pt", margin: 0)

      assert diagnostic.reason == :unsupported_html
      assert diagnostic.stage == :layout
      assert diagnostic.module == NativeElixirPdfUtilities.Validators.SvgConverterValidator
      assert diagnostic.operation == :convert
      assert diagnostic.source == "foreignObject"
      assert diagnostic.message =~ "foreignObject"
    )
  end

  test "hidden and fully clipped HTML effect inputs do not leak replacement text" do
    for attributes <- [
          "opacity='0'",
          "visibility='hidden'",
          "display='none'",
          "transform='translate(500 0)'"
        ] do
      html =
        "<svg width='200' height='60'><defs><filter id='f'><feGaussianBlur stdDeviation='1'/></filter></defs><g filter='url(#f)' #{attributes}><foreignObject width='150' height='40'><div style='background:red'>Hidden HTML</div></foreignObject></g></svg>"

      (
        assert {:error, {:unsupported_html, diagnostic}} = HtmlToPdf.render(html)
        assert diagnostic.reason == :unsupported_html
        assert diagnostic.stage == :layout
        assert diagnostic.module == NativeElixirPdfUtilities.Validators.SvgConverterValidator
        assert diagnostic.operation == :convert
        assert diagnostic.source == "foreignObject"
        assert diagnostic.message =~ "foreignObject"
      )
    end

    for style <- ["display:none", "position:relative;left:500pt"] do
      html =
        "<svg width='200' height='60'><defs><filter id='f'><feGaussianBlur stdDeviation='1'/></filter></defs><g filter='url(#f)'><foreignObject width='150' height='40'><div style='background:red'><span style='#{style}'>Hidden HTML</span></div></foreignObject></g></svg>"

      (
        assert {:error, {:unsupported_html, diagnostic}} = HtmlToPdf.render(html)
        assert diagnostic.reason == :unsupported_html
        assert diagnostic.stage == :layout
        assert diagnostic.module == NativeElixirPdfUtilities.Validators.SvgConverterValidator
        assert diagnostic.operation == :convert
        assert diagnostic.source == "foreignObject"
        assert diagnostic.message =~ "foreignObject"
      )
    end
  end

  test "filtered HTML replacement follows actual selected glyph clipping inside and outside its viewport" do
    defs =
      "<defs><filter id='f'><feGaussianBlur stdDeviation='1'/></filter><clipPath id='clip'><rect width='32' height='30'/></clipPath></defs>"

    content =
      "<foreignObject width='80' height='20'><div style='font-family:DejaVu Sans;font-size:14px;white-space:nowrap'>Visible Original Hidden Tail</div></foreignObject>"

    for clip <- ["", "clip-path='url(#clip)'"] do
      html =
        "<svg width='200' height='60'>#{defs}<g filter='url(#f)' #{clip}>#{content}<rect x='100' y='10' width='20' height='10' fill='blue'/></g></svg>"

      (
        assert {:error, {:unsupported_html, diagnostic}} = HtmlToPdf.render(html)
        assert diagnostic.reason == :unsupported_html
        assert diagnostic.stage == :layout
        assert diagnostic.module == NativeElixirPdfUtilities.Validators.SvgConverterValidator
        assert diagnostic.operation == :convert
        assert diagnostic.source == "foreignObject"
        assert diagnostic.message =~ "foreignObject"
      )
    end
  end
end
