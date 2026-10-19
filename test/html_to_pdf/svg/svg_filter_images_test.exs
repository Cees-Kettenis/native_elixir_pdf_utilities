alias NativeElixirPdfUtilities.TestSupport.Paths

Code.require_file(Paths.support("svg_public_workflow.ex"))
Code.require_file(Paths.support("png_fixture.ex"))

defmodule NativeElixirPdfUtilities.HtmlToPdf.SvgFilterImagesTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.{HtmlToPdf, Text, Limits}

  setup do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    %{limits: limits}
  end

  test "missing targets diagnose through validator authority" do
    source =
      "<svg width='200' height='100'><defs><filter id='f'><feImage href='#input'/></filter></defs><rect width='100' height='50' filter='url(#f)'/></svg>"

    assert {:error, {:invalid_document, %{message: message, stage: stage}}} =
             HtmlToPdf.render(source)

    assert message != ""
    assert stage in [:svg, :layout, :style]
  end

  test "independent selected feImage text retains its Unicode without borrowing SourceGraphic text" do
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

  alias NativeElixirPdfUtilities.{Limits}

  test "local foreignObject in hidden defs has no layout surface to sample" do
    source =
      "<svg width='220' height='100'><defs><foreignObject id='input' x='4' y='6' width='140' height='50'><div style='font-family:DejaVu Sans;font-size:18px;background:#ccddee'>Independent HTML Ω</div></foreignObject><filter id='f' filterUnits='userSpaceOnUse' x='0' y='0' width='180' height='70'><feImage href='#input'/></filter></defs><g filter='url(#f)'><text x='0' y='25'>Unpainted source</text></g></svg>"

    assert {:error, {:unsupported_html, foreignobject_diagnostic}} =
             NativeElixirPdfUtilities.HtmlToPdf.render(source)

    assert foreignobject_diagnostic.reason == :unsupported_html
    assert foreignobject_diagnostic.message =~ "foreignObject"
    assert foreignobject_diagnostic.stage == :layout
    assert foreignobject_diagnostic.operation == :convert

    assert foreignobject_diagnostic.module ==
             NativeElixirPdfUtilities.Validators.SvgConverterValidator
  end

  test "HTML image inputs follow live graph ownership without leaking dead SourceGraphic" do
    for {operations, _expected} <- [
          {"<feImage href='#input' result='image'/><feGaussianBlur in='image' stdDeviation='.5'/>",
           nil},
          {"<feImage href='#input' result='image'/><feFlood flood-color='blue'/>", nil},
          {"<feImage href='#input' result='image'/><feColorMatrix in='image' values='0 0 0 0 .2 0 0 0 0 .3 0 0 0 0 .5 0 0 0 0 1'/>",
           nil}
        ] do
      source =
        "<svg width='240' height='100'><defs><foreignObject id='input' width='180' height='40'><div style='font-family:DejaVu Sans;font-size:16px;background:#ccddee'>Issued HTML Ω</div></foreignObject><filter id='f' filterUnits='userSpaceOnUse' x='0' y='0' width='200' height='60'>#{operations}</filter></defs><g filter='url(#f)'><foreignObject width='180' height='40'><div>Dead original HTML</div></foreignObject></g></svg>"

      assert {:error, {:unsupported_html, diagnostic}} =
               NativeElixirPdfUtilities.HtmlToPdf.render(source)

      assert diagnostic.reason == :unsupported_html
      assert diagnostic.stage == :layout
      assert diagnostic.module == NativeElixirPdfUtilities.Validators.SvgConverterValidator
      assert diagnostic.operation == :convert
      assert diagnostic.source == "foreignObject"
      assert diagnostic.message =~ "foreignObject"
    end
  end

  test "visible local foreignObject reference samples its surface independently" do
    source =
      "<svg width='400' height='100'><foreignObject id='input' x='4' y='6' width='180' height='50'><div style='font-family:DejaVu Sans;font-size:16px;background:#ccddee'>Visible HTML Ω</div></foreignObject><defs><filter id='f' filterUnits='userSpaceOnUse' x='0' y='0' width='180' height='70'><feImage href='#input'/></filter></defs><g transform='translate(200 0)' filter='url(#f)'><text x='0' y='25'>Unpainted source</text></g></svg>"

    assert {:error, {:unsupported_html, foreignobject_diagnostic}} =
             NativeElixirPdfUtilities.HtmlToPdf.render(source)

    assert foreignobject_diagnostic.reason == :unsupported_html
    assert foreignobject_diagnostic.message =~ "foreignObject"
    assert foreignobject_diagnostic.stage == :layout
    assert foreignobject_diagnostic.operation == :convert

    assert foreignobject_diagnostic.module ==
             NativeElixirPdfUtilities.Validators.SvgConverterValidator
  end

  test "hidden HTML filter references do not load their absent-layout descendants" do
    parent = self()

    source =
      "<svg width='100' height='50'><defs><foreignObject id='input' width='100' height='50'><img src='private.png'/></foreignObject><filter id='f' filterUnits='userSpaceOnUse' x='0' y='0' width='100' height='50'><feImage href='#input'/></filter></defs><rect width='100' height='50' filter='url(#f)'/></svg>"

    assert {:error, {:unsupported_html, foreignobject_diagnostic}} =
             NativeElixirPdfUtilities.HtmlToPdf.render(source,
               asset_resolver: fn request ->
                 send(parent, {:loaded, request})
                 {:error, :not_found}
               end
             )

    assert foreignobject_diagnostic.reason == :unsupported_html
    assert foreignobject_diagnostic.message =~ "foreignObject"
    assert foreignobject_diagnostic.stage == :layout
    assert foreignobject_diagnostic.operation == :convert

    assert foreignobject_diagnostic.module ==
             NativeElixirPdfUtilities.Validators.SvgConverterValidator

    refute_received {:loaded, _}
  end
end
