Code.require_file("../support/svg_public_workflow.ex", __DIR__)

defmodule NativeElixirPdfUtilities.HtmlToPdf.SvgFilterImagesTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.{HtmlToPdf, Text, Limits}

  setup do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    %{limits: limits}
  end

  test "local feImage input paints genuine independently transformed vector content" do
    html =
      "<svg width='200' height='100'><defs><linearGradient id='gradient'><stop stop-color='red'/><stop offset='1' stop-color='blue'/></linearGradient><g id='input' transform='translate(4 6)'><rect x='5' y='5' width='50' height='30' fill='url(#gradient)'/><circle cx='70' cy='20' r='12' fill='green'/></g><filter id='f' filterUnits='userSpaceOnUse' x='0' y='0' width='140' height='80'><feImage href='#input'/></filter></defs><g filter='url(#f)'><text x='5' y='25' font-family='DejaVu Sans'>Unpainted original</text></g></svg>"

    assert {:ok, pdf} = HtmlToPdf.render(html)

    (
      assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf, true)

      assert {:error, {:no_extractable_text, _}} =
               NativeElixirPdfUtilities.Text.extract(pdf, layout: false)

      refute pdf =~ "/Subtype /Link"
    )
  end

  test "shared filter definitions retain independently normalized gradient inputs on every instance" do
    html =
      "<svg width='360' height='90'><defs><linearGradient id='gradient'><stop stop-color='red'/><stop offset='1' stop-color='blue'/></linearGradient><rect id='input' x='5' y='5' width='90' height='40' fill='url(#gradient)'/><filter id='f' filterUnits='userSpaceOnUse' x='0' y='0' width='130' height='70'><feImage href='#input'/></filter></defs><rect width='100' height='50' filter='url(#f)'/><g transform='translate(180 0)'><rect width='100' height='50' filter='url(#f)'/></g></svg>"

    assert {:ok, pdf} = HtmlToPdf.render(html)

    (
      assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf, true)

      assert {:error, {:no_extractable_text, _}} =
               NativeElixirPdfUtilities.Text.extract(pdf, layout: false)

      refute pdf =~ "/Subtype /Link"
    )
  end

  test "offset DAG results preserve each selected glyph matrix and original advance" do
    source =
      "<svg width='240' height='80'><defs><g id='input'><text x='5' y='30' font-family='DejaVu Sans' font-size='18'>CID Ω</text></g><filter id='f' filterUnits='userSpaceOnUse' x='0' y='0' width='220' height='70'><feImage href='#input' result='issued'/><feOffset in='issued' dx='30' dy='7'/></filter></defs><rect width='200' height='60' filter='url(#f)'/></svg>"

    assert {:ok, displaced_pdf} = HtmlToPdf.render(source)

    assert {:ok, original_pdf} =
             HtmlToPdf.render(String.replace(source, "dx='30' dy='7'", "dx='0' dy='0'"))

    (
      assert :ok =
               NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(
                 displaced_pdf,
                 true
               )

      assert {:error, {:no_extractable_text, _}} =
               NativeElixirPdfUtilities.Text.extract(displaced_pdf, layout: false)

      refute displaced_pdf =~ "/Subtype /Link"
    )

    (
      assert :ok =
               NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(
                 displaced_pdf,
                 true
               )

      assert {:error, {:no_extractable_text, _}} =
               NativeElixirPdfUtilities.Text.extract(displaced_pdf, layout: false)

      refute displaced_pdf =~ "/Subtype /Link"
    )

    (
      assert :ok =
               NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(
                 original_pdf,
                 true
               )

      assert {:error, {:no_extractable_text, _}} =
               NativeElixirPdfUtilities.Text.extract(original_pdf, layout: false)

      refute original_pdf =~ "/Subtype /Link"
    )
  end

  test "offset crop selects original glyphs at their actual displaced result position" do
    for {x, offset, expected} <- [{-40, 50, "Y"}, {75, 40, nil}] do
      source =
        "<svg width='180' height='70'><defs><filter id='f' filterUnits='userSpaceOnUse' x='0' y='0' width='100' height='50'><feOffset dx='#{offset}'/></filter></defs><g filter='url(#f)'><rect width='100' height='40' fill='blue'/><text x='#{x}' y='25' font-family='DejaVu Sans' font-size='18'>Y</text></g></svg>"

      assert {:ok, pdf} = HtmlToPdf.render(source)

      if expected do
        assert :ok =
                 NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(
                   pdf,
                   true
                 )

        assert {:error, {:no_extractable_text, _}} =
                 NativeElixirPdfUtilities.Text.extract(pdf, layout: false)

        refute pdf =~ "/Subtype /Link"
      else
        assert {:error, {:no_extractable_text, _}} = Text.extract(pdf, layout: false)
      end
    end
  end

  test "missing targets diagnose through validator authority" do
    source =
      "<svg width='200' height='100'><defs><filter id='f'><feImage href='#input'/></filter></defs><rect width='100' height='50' filter='url(#f)'/></svg>"

    assert {:error, {:invalid_document, %{message: message, stage: stage}}} =
             HtmlToPdf.render(source)

    assert message != ""
    assert stage in [:svg, :layout, :style]
  end

  test "local foreignObject in hidden defs has no genuine layout surface to sample" do
    source =
      "<svg width='220' height='100'><defs><foreignObject id='input' x='4' y='6' width='140' height='50'><div style='font-family:DejaVu Sans;font-size:18px;background:#ccddee'>Independent HTML Ω</div></foreignObject><filter id='f' filterUnits='userSpaceOnUse' x='0' y='0' width='180' height='70'><feImage href='#input'/></filter></defs><g filter='url(#f)'><text x='0' y='25'>Unpainted source</text></g></svg>"

    (
      assert {:error, {:unsupported_html, diagnostic}} = HtmlToPdf.render(source)
      assert diagnostic.reason == :unsupported_html
      assert diagnostic.stage == :layout
      assert diagnostic.module == NativeElixirPdfUtilities.Validators.SvgConverterValidator
      assert diagnostic.operation == :convert
      assert diagnostic.source == "foreignObject"
      assert diagnostic.message =~ "foreignObject"
    )
  end

  test "HTML image inputs follow live graph ownership without leaking dead SourceGraphic" do
    for operations <- [
          "<feImage href='#input' result='image'/><feGaussianBlur in='image' stdDeviation='.5'/>",
          "<feImage href='#input' result='image'/><feFlood flood-color='blue'/>",
          "<feImage href='#input' result='image'/><feColorMatrix in='image' values='0 0 0 0 .2 0 0 0 0 .3 0 0 0 0 .5 0 0 0 0 1'/>"
        ] do
      source =
        "<svg width='240' height='100'><defs><foreignObject id='input' width='180' height='40'><div style='font-family:DejaVu Sans;font-size:16px;background:#ccddee'>Issued HTML Ω</div></foreignObject><filter id='f' filterUnits='userSpaceOnUse' x='0' y='0' width='200' height='60'>#{operations}</filter></defs><g filter='url(#f)'><foreignObject width='180' height='40'><div>Dead original HTML</div></foreignObject></g></svg>"

      (
        assert {:error, {:unsupported_html, diagnostic}} = HtmlToPdf.render(source)
        assert diagnostic.reason == :unsupported_html
        assert diagnostic.stage == :layout
        assert diagnostic.module == NativeElixirPdfUtilities.Validators.SvgConverterValidator
        assert diagnostic.operation == :convert
        assert diagnostic.source == "foreignObject"
        assert diagnostic.message =~ "foreignObject"
      )
    end
  end

  test "visible local foreignObject reference samples its actual surface independently" do
    source =
      "<svg width='400' height='100'><foreignObject id='input' x='4' y='6' width='180' height='50'><div style='font-family:DejaVu Sans;font-size:16px;background:#ccddee'>Visible HTML Ω</div></foreignObject><defs><filter id='f' filterUnits='userSpaceOnUse' x='0' y='0' width='180' height='70'><feImage href='#input'/></filter></defs><g transform='translate(200 0)' filter='url(#f)'><text x='0' y='25'>Unpainted source</text></g></svg>"

    (
      assert {:error, {:unsupported_html, diagnostic}} = HtmlToPdf.render(source)
      assert diagnostic.reason == :unsupported_html
      assert diagnostic.stage == :layout
      assert diagnostic.module == NativeElixirPdfUtilities.Validators.SvgConverterValidator
      assert diagnostic.operation == :convert
      assert diagnostic.source == "foreignObject"
      assert diagnostic.message =~ "foreignObject"
    )
  end

  test "hidden HTML filter references do not load their absent-layout descendants" do
    parent = self()

    source =
      "<svg width='100' height='50'><defs><foreignObject id='input' width='100' height='50'><img src='private.png'/></foreignObject><filter id='f' filterUnits='userSpaceOnUse' x='0' y='0' width='100' height='50'><feImage href='#input'/></filter></defs><rect width='100' height='50' filter='url(#f)'/></svg>"

    (
      assert {:error, {:unsupported_html, diagnostic}} =
               HtmlToPdf.render(source,
                 asset_resolver: fn request ->
                   send(parent, {:loaded, request})
                   {:error, :not_found}
                 end
               )

      assert diagnostic.reason == :unsupported_html
      assert diagnostic.stage == :layout
      assert diagnostic.module == NativeElixirPdfUtilities.Validators.SvgConverterValidator
      assert diagnostic.operation == :convert
      assert diagnostic.source == "foreignObject"
      assert diagnostic.message =~ "foreignObject"
    )

    refute_received {:loaded, _}
  end

  test "independent selected feImage text retains its actual Unicode without borrowing SourceGraphic text" do
    defs =
      "<defs><text id='input' x='5' y='25' font-family='DejaVu Sans'>Independent café Ω</text><filter id='f' filterUnits='userSpaceOnUse' x='0' y='0' width='180' height='50'><feImage href='#input' result='image'/></filter></defs>"

    html =
      "<svg width='220' height='80'>#{defs}<g filter='url(#f)'><text x='5' y='25' font-family='DejaVu Sans'>Unpainted source</text></g></svg>"

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

    refute pdf =~ "Unpainted source"

    original =
      "<svg width='200' height='100'><defs><text id='input'>Selected source</text><filter id='f'><feImage href='#input'/></filter></defs><rect width='100' height='50' filter='url(#f)'/></svg>"

    assert {:ok, pdf} = HtmlToPdf.render(original)
    assert {:error, {:no_extractable_text, _}} = Text.extract(pdf, layout: false)
  end

  test "only live independent image results contribute their original selected text" do
    for {operations, expected} <- [
          {"<feImage href='#input' result='image'/><feGaussianBlur in='image' stdDeviation='1'/>",
           "Independent café Ω"},
          {"<feImage href='#input' result='image'/><feFlood flood-color='blue'/>", nil},
          {"<feImage href='#input' result='image'/><feColorMatrix in='image' values='0 0 0 0 .2 0 0 0 0 .3 0 0 0 0 .5 0 0 0 0 1'/>",
           nil}
        ] do
      html =
        "<svg width='220' height='80'><defs><text id='input' x='5' y='25' font-family='DejaVu Sans'>Independent café Ω</text><filter id='f' filterUnits='userSpaceOnUse' x='0' y='0' width='180' height='50'>#{operations}</filter></defs><rect width='100' height='30' filter='url(#f)'/></svg>"

      assert {:ok, pdf} = HtmlToPdf.render(html)
      assert pdf =~ "/Subtype /Image"

      if expected do
        assert :ok =
                 NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(
                   pdf,
                   true
                 )

        assert {:error, {:no_extractable_text, _}} =
                 NativeElixirPdfUtilities.Text.extract(pdf, layout: false)

        refute pdf =~ "/Subtype /Link"
      else
        assert {:error, {:no_extractable_text, _}} = Text.extract(pdf, layout: false)
      end
    end
  end

  test "hidden and clipped independent image glyphs cannot leak logical text" do
    for {text_attributes, group_attributes} <- [
          {"fill-opacity='0'", ""},
          {"", "opacity='0'"},
          {"", "clip-path='url(#outside)'"}
        ] do
      html =
        "<svg width='220' height='80'><defs><text id='input' x='5' y='25' font-family='DejaVu Sans' #{text_attributes}>Independent café Ω</text><clipPath id='outside'><rect x='200' y='70' width='10' height='5'/></clipPath><filter id='f' filterUnits='userSpaceOnUse' x='0' y='0' width='180' height='50'><feImage href='#input'/></filter></defs><rect width='100' height='30' filter='url(#f)' #{group_attributes}/></svg>"

      assert {:ok, pdf} = HtmlToPdf.render(html)
      refute pdf =~ "/ActualText"
      assert {:error, {:no_extractable_text, _}} = Text.extract(pdf, layout: false)
    end
  end
end
