alias NativeElixirPdfUtilities.TestSupport.Paths

defmodule NativeElixirPdfUtilities.HtmlToPdf.SvgEffectFieldUpdateTest do
  Code.require_file(Paths.support("svg_public_workflow.ex"))
  Code.require_file(Paths.support("png_fixture.ex"))
  use ExUnit.Case, async: false
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

  test "opaque filtered field updates regenerate native paint and flatten identically" do
    html =
      "<body style='margin:0'><svg width='180' height='70'>\n<defs><filter id='f' filterUnits='userSpaceOnUse' x='0' y='0' width='180' height='70'>\n<feColorMatrix type='matrix' values='-1 0 0 0 1 0 -1 0 0 1 0 0 -1 0 1 0 0 0 1 0'/>\n</filter></defs><rect width='180' height='70' fill='#c0c0c0'/><g filter='url(#f)'>\n<foreignObject x='20' y='20' width='130' height='40'><div style='margin:0'>\n<input name='text' value='Old' style='display:block;width:90px;height:24px;margin:0'>\n</div></foreignObject></g></svg></body>\n"

    assert {:error, {:unsupported_html, foreignobject_diagnostic}} =
             NativeElixirPdfUtilities.HtmlToPdf.render(html, page_size: {150, 75}, margin: 0)

    assert foreignobject_diagnostic.reason == :unsupported_html
    assert foreignobject_diagnostic.message =~ "foreignObject"
    assert foreignobject_diagnostic.stage == :layout
    assert foreignobject_diagnostic.operation == :convert

    assert foreignobject_diagnostic.module ==
             NativeElixirPdfUtilities.Validators.SvgConverterValidator
  end

  test "a scalar edit regenerates filter ink outside its host and every peer appearance" do
    html =
      "<body style='margin:0'><svg width='180' height='70'>\n<defs><filter id='f' filterUnits='userSpaceOnUse' x='0' y='0' width='180' height='70'>\n<feGaussianBlur stdDeviation='3'/></filter></defs><g filter='url(#f)'>\n<foreignObject x='20' y='20' width='100' height='45'><div style='margin:0'>\n<input name='one' value='' style='display:block;width:60px;height:24px;margin:0'>\n</div></foreignObject>\n<foreignObject x='45' y='20' width='100' height='45'><div style='margin:0'>\n<input name='peer' value='Peer' style='display:block;width:60px;height:24px;margin:0'>\n</div></foreignObject></g></svg></body>\n"

    assert {:error, {:unsupported_html, foreignobject_diagnostic}} =
             NativeElixirPdfUtilities.HtmlToPdf.render(html, page_size: {150, 75}, margin: 0)

    assert foreignobject_diagnostic.reason == :unsupported_html
    assert foreignobject_diagnostic.message =~ "foreignObject"
    assert foreignobject_diagnostic.stage == :layout
    assert foreignobject_diagnostic.operation == :convert

    assert foreignobject_diagnostic.module ==
             NativeElixirPdfUtilities.Validators.SvgConverterValidator
  end

  test "same-document filtered radio peers retain canonical ownership across source slots" do
    html =
      "<body style='margin:0'><form><svg width='180' height='70'>\n<defs><filter id='f'><feColorMatrix type='saturate' values='0'/></filter></defs><g filter='url(#f)'>\n<foreignObject x='10' y='10' width='70' height='45'><div><input type='radio' name='r' value='a' checked></div></foreignObject>\n<foreignObject x='90' y='10' width='70' height='45'><div><input type='radio' name='r' value='b'></div></foreignObject>\n</g></svg></form></body>\n"

    assert {:error, {:unsupported_html, foreignobject_diagnostic}} =
             NativeElixirPdfUtilities.HtmlToPdf.render(html, page_size: {150, 75}, margin: 0)

    assert foreignobject_diagnostic.reason == :unsupported_html
    assert foreignobject_diagnostic.message =~ "foreignObject"
    assert foreignobject_diagnostic.stage == :layout
    assert foreignobject_diagnostic.operation == :convert

    assert foreignobject_diagnostic.module ==
             NativeElixirPdfUtilities.Validators.SvgConverterValidator

    assert {:error, {:unsupported_html, foreignobject_diagnostic}} =
             NativeElixirPdfUtilities.HtmlToPdf.render(
               String.replace(html, "value='a' checked", "value='a'")
               |> String.replace("value='b'", "value='b' checked"),
               page_size: {150, 75},
               margin: 0
             )

    assert foreignobject_diagnostic.reason == :unsupported_html
    assert foreignobject_diagnostic.message =~ "foreignObject"
    assert foreignobject_diagnostic.stage == :layout
    assert foreignobject_diagnostic.operation == :convert

    assert foreignobject_diagnostic.module ==
             NativeElixirPdfUtilities.Validators.SvgConverterValidator
  end

  test "filled source retains chosen full font, color, size, spacing, padding and clip" do
    {path, 0} = System.cmd("fc-match", ["--format=%{file}", "DejaVu Serif"])
    bytes = File.read!(path)

    opts = [
      page_size: {180, 90},
      margin: 0,
      fonts: [%{family: "Chosen Editor", data: bytes, weight: 400}]
    ]

    html =
      "<body style='margin:0'><svg width='220' height='90'><defs><filter id='f'\nfilterUnits='userSpaceOnUse' x='0' y='0' width='220' height='90'>\n<feColorMatrix type='saturate' values='0'/></filter></defs><g filter='url(#f)' opacity='.8'>\n<foreignObject x='16' y='12' width='190' height='65'><div style='margin:0'>\n<input name='text' value='Old' style='display:block;width:115px;height:30px;margin:0;\nfont-family:\"Chosen Editor\";font-size:20px;line-height:24px;color:#0044cc;\nletter-spacing:1px;padding:4px 7px;border:1px solid #333'>\n</div></foreignObject></g></svg></body>\n"

    assert {:error, {:unsupported_html, foreignobject_diagnostic}} =
             NativeElixirPdfUtilities.HtmlToPdf.render(html, opts)

    assert foreignobject_diagnostic.reason == :unsupported_html
    assert foreignobject_diagnostic.message =~ "foreignObject"
    assert foreignobject_diagnostic.stage == :layout
    assert foreignobject_diagnostic.operation == :convert

    assert foreignobject_diagnostic.module ==
             NativeElixirPdfUtilities.Validators.SvgConverterValidator

    assert {:error, {:unsupported_html, foreignobject_diagnostic}} =
             NativeElixirPdfUtilities.HtmlToPdf.render(
               String.replace(html, "value='Old'", "value='AV ffi'"),
               opts
             )

    assert foreignobject_diagnostic.reason == :unsupported_html
    assert foreignobject_diagnostic.message =~ "foreignObject"
    assert foreignobject_diagnostic.stage == :layout
    assert foreignobject_diagnostic.operation == :convert

    assert foreignobject_diagnostic.module ==
             NativeElixirPdfUtilities.Validators.SvgConverterValidator

    assert {:error, {:unsupported_html, foreignobject_diagnostic}} =
             NativeElixirPdfUtilities.HtmlToPdf.render(
               String.replace(html, "value='Old'", "value=''"),
               opts
             )

    assert foreignobject_diagnostic.reason == :unsupported_html
    assert foreignobject_diagnostic.message =~ "foreignObject"
    assert foreignobject_diagnostic.stage == :layout
    assert foreignobject_diagnostic.operation == :convert

    assert foreignobject_diagnostic.module ==
             NativeElixirPdfUtilities.Validators.SvgConverterValidator

    masked_html =
      String.replace(
        html,
        "filter='url(#f)' opacity='.8'",
        "mask='url(#m)' opacity='.8' transform='matrix(1 .1 -.1 1 2 0)'"
      )
      |> String.replace(
        "</defs>",
        "<mask id='m' maskUnits='userSpaceOnUse' x='0' y='0' width='220' height='90'><rect width='220' height='90' fill='white'/></mask></defs>"
      )

    assert {:error, {:unsupported_html, foreignobject_diagnostic}} =
             NativeElixirPdfUtilities.HtmlToPdf.render(masked_html, opts)

    assert foreignobject_diagnostic.reason == :unsupported_html
    assert foreignobject_diagnostic.message =~ "foreignObject"
    assert foreignobject_diagnostic.stage == :layout
    assert foreignobject_diagnostic.operation == :convert

    assert foreignobject_diagnostic.module ==
             NativeElixirPdfUtilities.Validators.SvgConverterValidator

    assert {:error, {:unsupported_html, foreignobject_diagnostic}} =
             NativeElixirPdfUtilities.HtmlToPdf.render(
               String.replace(masked_html, "value='Old'", "value='AV ffi'"),
               opts
             )

    assert foreignobject_diagnostic.reason == :unsupported_html
    assert foreignobject_diagnostic.message =~ "foreignObject"
    assert foreignobject_diagnostic.stage == :layout
    assert foreignobject_diagnostic.operation == :convert

    assert foreignobject_diagnostic.module ==
             NativeElixirPdfUtilities.Validators.SvgConverterValidator
  end

  test "partial radio flatten retains state during remaining scalar and peer graph updates" do
    html =
      "<body style='margin:0'><svg width='180' height='80'><defs><filter id='f'\nfilterUnits='userSpaceOnUse' x='0' y='0' width='180' height='80'><feGaussianBlur stdDeviation='1'/>\n</filter></defs><g filter='url(#f)'><foreignObject x='10' y='10' width='165' height='65'>\n<div><input name='text' value='Old' style='display:block;width:70px;height:22px'>\n<input type='radio' name='r' value='a' checked><input type='radio' name='r' value='b'></div>\n</foreignObject></g></svg></body>\n"

    opts = [page_size: {150, 80}, margin: 0]

    assert {:error, {:unsupported_html, foreignobject_diagnostic}} =
             NativeElixirPdfUtilities.HtmlToPdf.render(html, opts)

    assert foreignobject_diagnostic.reason == :unsupported_html
    assert foreignobject_diagnostic.message =~ "foreignObject"
    assert foreignobject_diagnostic.stage == :layout
    assert foreignobject_diagnostic.operation == :convert

    assert foreignobject_diagnostic.module ==
             NativeElixirPdfUtilities.Validators.SvgConverterValidator

    assert {:error, {:unsupported_html, foreignobject_diagnostic}} =
             NativeElixirPdfUtilities.HtmlToPdf.render(
               String.replace(html, "value='Old'", "value='New'")
               |> String.replace("value='a' checked", "value='a'")
               |> String.replace("value='b'", "value='b' checked"),
               opts
             )

    assert foreignobject_diagnostic.reason == :unsupported_html
    assert foreignobject_diagnostic.message =~ "foreignObject"
    assert foreignobject_diagnostic.stage == :layout
    assert foreignobject_diagnostic.operation == :convert

    assert foreignobject_diagnostic.module ==
             NativeElixirPdfUtilities.Validators.SvgConverterValidator
  end
end
