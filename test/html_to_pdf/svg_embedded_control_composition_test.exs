defmodule NativeElixirPdfUtilities.HtmlToPdf.SvgEmbeddedControlCompositionTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.{HtmlToPdf}

  test "foreignObject controls keep root document names, owner, and native initial values" do
    html =
      "<body style='margin:0'><form id='owner'>\n<input name='root' value='Root' style='display:block;width:80px;height:24px'>\n<svg width='220' height='90'><foreignObject x='10' y='10' width='200' height='70'>\n<div style='margin:0'><input name='native' form='owner' value='Native' style='display:block;width:120px;height:24px'>\n<input name='tick' type='checkbox' checked></div>\n</foreignObject></svg></form></body>\n"

    (
      assert {:error, {:unsupported_html, diagnostic}} =
               HtmlToPdf.render(html, page_size: {240, 180}, margin: 0)

      assert diagnostic.reason == :unsupported_html
      assert diagnostic.stage == :layout
      assert diagnostic.module == NativeElixirPdfUtilities.Validators.SvgConverterValidator
      assert diagnostic.operation == :convert
      assert diagnostic.source == "foreignObject"
      assert diagnostic.message =~ "foreignObject"
    )
  end

  test "same-document radio groups and duplicate names obey the shared form authority" do
    host =
      "<form id='owner'><input type='radio' name='group' value='root' checked>\n<svg width='180' height='50'><foreignObject width='180' height='50'>\n<div><input type='radio' name='group' form='owner' value='svg' required checked></div>\n</foreignObject></svg></form>\n"

    (
      assert {:error, {:unsupported_html, diagnostic}} = HtmlToPdf.render(host)
      assert diagnostic.reason == :unsupported_html
      assert diagnostic.stage == :layout
      assert diagnostic.module == NativeElixirPdfUtilities.Validators.SvgConverterValidator
      assert diagnostic.operation == :convert
      assert diagnostic.source == "foreignObject"
      assert diagnostic.message =~ "foreignObject"
    )

    duplicate =
      "<input name='same'><svg width='180' height='50'><foreignObject width='180' height='50'><input name='same'></foreignObject></svg>"

    (
      assert {:error, {:unsupported_html, diagnostic}} = HtmlToPdf.render(duplicate)
      assert diagnostic.reason == :unsupported_html
      assert diagnostic.stage == :layout
      assert diagnostic.operation == :convert
      assert diagnostic.module == NativeElixirPdfUtilities.Validators.SvgConverterValidator
      assert diagnostic.source == "foreignObject"
      assert diagnostic.message =~ "foreignObject"
    )
  end

  test "rotated, skewed and reflected HTML controls carry genuine clipped affine appearance Forms" do
    for transform <- ["rotate(20 70 30)", "skewX(20)", "translate(160 0) scale(-1 1)"] do
      html = "<body style='margin:0'><svg width='220' height='100'>
<foreignObject x='20' y='20' width='140' height='60' transform='#{transform}'>
<div><input name='affine' value='Affine' style='display:block;width:100px;height:24px'></div>
</foreignObject></svg></body>
"

      (
        assert {:error, {:unsupported_html, diagnostic}} =
                 HtmlToPdf.render(html, page_size: {240, 180}, margin: 0)

        assert diagnostic.reason == :unsupported_html
        assert diagnostic.stage == :layout
        assert diagnostic.module == NativeElixirPdfUtilities.Validators.SvgConverterValidator
        assert diagnostic.operation == :convert
        assert diagnostic.source == "foreignObject"
        assert diagnostic.message =~ "foreignObject"
      )
    end
  end

  test "nested scrolling iframe controls in a foreignObject keep child scope and client clipping" do
    child =
      "<body style='margin:0'><input name='shared' value='Nested' style='display:block;width:180px;height:24px'><div style='height:180px'>Overflow</div><input name='offscreen' value='Hidden'></body>"

    escaped =
      child
      |> String.replace("&", "&amp;")
      |> String.replace("'", "&#39;")
      |> String.replace("<", "&lt;")

    html = "<body style='margin:0'><input name='shared' value='Root'>
<svg width='220' height='100'><foreignObject x='10' y='10' width='180' height='80'>
<iframe srcdoc='#{escaped}' style='display:block;border:0;width:140px;height:60px'></iframe>
</foreignObject></svg></body>
"

    (
      assert {:error, {:unsupported_html, diagnostic}} =
               HtmlToPdf.render(html, page_size: {240, 180}, margin: 0)

      assert diagnostic.reason == :unsupported_html
      assert diagnostic.stage == :layout
      assert diagnostic.module == NativeElixirPdfUtilities.Validators.SvgConverterValidator
      assert diagnostic.operation == :convert
      assert diagnostic.source == "foreignObject"
      assert diagnostic.message =~ "foreignObject"
    )
  end

  test "same-document unnamed controls reserve distinct genuine field names" do
    html =
      "<input value='Root'><svg width='180' height='50'><foreignObject width='180' height='50'><input value='Svg'></foreignObject></svg>"

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

  test "skipped special controls preserve source values without unwanted track, thumb, file or image paint" do
    alias NativeElixirPdfUtilities.Validators.HtmlValidator

    html =
      "<svg width='220' height='130'><foreignObject width='220' height='130'><div>\n<input name='visible' value='Visible'>\n<input hidden='until-found' type='range' value='30'>\n<input hidden='until-found' type='file' value='/private/secret.txt'>\n<input hidden='until-found' type='image' src='unused' width='8' height='8' style='display:block'>\n<progress hidden='until-found' value='.5' style='width:80pt;height:18pt;border:1pt solid red'></progress>\n</div></foreignObject></svg>\n"

    bytes = File.read!("test/fixtures/html_to_pdf/browser_parity/assets/iframe-bars.png")
    options = [assets: %{"unused" => {:bytes, bytes}}]

    (
      assert {:error, {:unsupported_html, diagnostic}} = HtmlToPdf.render(html, options)
      assert diagnostic.reason == :unsupported_html
      assert diagnostic.stage == :layout
      assert diagnostic.module == NativeElixirPdfUtilities.Validators.SvgConverterValidator
      assert diagnostic.operation == :convert
      assert diagnostic.source == "foreignObject"
      assert diagnostic.message =~ "foreignObject"
    )
  end

  test "affine field placement failures retain shared actionable diagnostics before native paint" do
    alias NativeElixirPdfUtilities.HtmlToPdf.ResourceContext
    alias NativeElixirPdfUtilities.Validators.{EmbeddedFormValidator, HtmlValidator}

    ResourceContext.run([], HtmlValidator.new_image_budget(), fn context ->
      assert {:ok, descriptor} = EmbeddedFormValidator.issue(context, [])

      for matrix <- [{1, 0, 0, 0, 0, 0}, {1, 0, 0, 1, :bad, 0}, {1, 2}, :bad] do
        assert {:error, {:invalid_form, diagnostic}} =
                 EmbeddedFormValidator.compose_affine(descriptor, matrix, {0, 0, 100, 100}, nil)

        assert diagnostic.stage == :forms and diagnostic.reason == :invalid_form
        assert diagnostic.operation == :compose_child_widgets and diagnostic.message =~ "affine"
      end
    end)
  end

  test "original inline image-input applicability and unauthorized sources retain canonical failures" do
    source =
      "<svg width='220' height='130'><foreignObject width='220' height='130'><div><input hidden='until-found' type='image' src='unused' width='8' height='8'></div></foreignObject></svg>"

    assert {:error, {:invalid_document, diagnostic}} = HtmlToPdf.render(source)
    assert diagnostic.stage == :asset
    assert diagnostic.source == "unused"
    assert diagnostic.message != ""
    bytes = File.read!("test/fixtures/html_to_pdf/browser_parity/assets/iframe-bars.png")

    assert {:error, {:unsupported_form, diagnostic}} =
             HtmlToPdf.render(source, assets: %{"unused" => {:bytes, bytes}})

    assert diagnostic.stage == :forms and diagnostic.source == "input/image"
    assert diagnostic.message =~ "submit coordinates"
  end
end
