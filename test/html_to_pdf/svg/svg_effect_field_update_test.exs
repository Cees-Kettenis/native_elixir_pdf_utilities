alias NativeElixirPdfUtilities.TestSupport.Paths

defmodule NativeElixirPdfUtilities.HtmlToPdf.SvgEffectFieldUpdateTest do
  Code.require_file(Paths.support("svg_public_workflow.ex"))
  Code.require_file(Paths.support("png_fixture.ex"))
  use ExUnit.Case, async: true
  alias NativeElixirPdfUtilities.{HtmlToPdf}

  test "portable program rejects executable HTML and external resources before field paint" do
    html =
      "<body style='margin:0'><svg width='120' height='50'><defs><filter id='f'>\n<feColorMatrix type='saturate' values='0'/></filter></defs><g filter='url(#f)'>\n<foreignObject width='120' height='50'><div><input name='a' value='A'></div></foreignObject>\n</g></svg></body>\n"

    (
      assert {:error, {:unsupported_html, diagnostic}} =
               HtmlToPdf.render(html, page_size: {100, 50}, margin: 0)

      assert diagnostic.reason == :unsupported_html
      assert diagnostic.stage == :layout
      assert diagnostic.module == NativeElixirPdfUtilities.Validators.SvgConverterValidator
      assert diagnostic.operation == :convert
      assert diagnostic.source == "foreignObject"
      assert diagnostic.message =~ "foreignObject"
    )

    for source <- [
          "<svg xmlns='http://www.w3.org/2000/svg' width='120' height='50'><foreignObject width='100' height='40'><div xmlns='http://www.w3.org/1999/xhtml'><input/></div></foreignObject></svg>",
          "<svg xmlns='http://www.w3.org/2000/svg' width='120' height='50'><image href='file:///etc/passwd' width='100' height='40'/></svg>"
        ] do
      if String.contains?(source, "<foreignObject") do
        assert {:error, {:unsupported_html, diagnostic}} =
                 NativeElixirPdfUtilities.HtmlToPdf.render(source)

        assert diagnostic.reason == :unsupported_html
        assert diagnostic.stage == :layout
        assert diagnostic.operation == :convert
        assert diagnostic.module == NativeElixirPdfUtilities.Validators.SvgConverterValidator
        assert diagnostic.source == "foreignObject"
        assert diagnostic.message =~ "foreignObject"
      else
        assert {:error, {reason, diagnostic}} = NativeElixirPdfUtilities.HtmlToPdf.render(source)
        assert reason == diagnostic.reason
        assert diagnostic.stage in [:asset, :resource, :style]
        assert diagnostic.message != ""
      end
    end
  end
end
