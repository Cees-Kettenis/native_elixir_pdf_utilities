defmodule NativeElixirPdfUtilities.HtmlToPdf.SvgWidgetEffectAppearanceTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.{HtmlToPdf}

  test "ancestor opacity, masks and filters preserve real initial widget AP and shared sibling paint" do
    scenes = [
      {"opacity='.35'", ""},
      {"mask='url(#m)'",
       "<mask id='m' maskUnits='userSpaceOnUse' x='0' y='0' width='180' height='70'><rect width='110' height='70' fill='white'/></mask>"},
      {"filter='url(#f)'",
       "<filter id='f' filterUnits='userSpaceOnUse' x='0' y='0' width='180' height='70'><feColorMatrix type='saturate' values='0'/></filter>"}
    ]

    directory =
      Path.join(System.tmp_dir!(), "native-widget-effects-#{System.unique_integer([:positive])}")

    File.mkdir_p!(directory)
    on_exit(fn -> File.rm_rf!(directory) end)

    for {effect, definition} <- scenes do
      html = "<body style='margin:0'><svg width='180' height='70'><defs>#{definition}</defs>
<rect width='180' height='70' fill='#e8e8f8'/>
<g #{effect}><rect x='5' y='4' width='145' height='50' fill='#ff7777'/>
<foreignObject x='10' y='10' width='160' height='50'><div style='margin:0'>
<input name='real' value='Actual' style='display:block;width:110px;height:24px;margin:0;color:blue'>
</div></foreignObject><rect x='80' y='12' width='25' height='20' fill='#005500'/></g>
</svg></body>
"

      (
        assert {:error, {:unsupported_html, diagnostic}} =
                 HtmlToPdf.render(html, page_size: {150, 75}, margin: 0)

        assert diagnostic.reason == :unsupported_html
        assert diagnostic.stage == :layout
        assert diagnostic.module == NativeElixirPdfUtilities.Validators.SvgConverterValidator
        assert diagnostic.operation == :convert
        assert diagnostic.source == "foreignObject"
        assert diagnostic.message =~ "foreignObject"
      )

      (
        assert {:error, {:unsupported_html, diagnostic}} =
                 HtmlToPdf.render(html, page_size: {150, 75}, margin: 0, forms: :static)

        assert diagnostic.reason == :unsupported_html
        assert diagnostic.stage == :layout
        assert diagnostic.module == NativeElixirPdfUtilities.Validators.SvgConverterValidator
        assert diagnostic.operation == :convert
        assert diagnostic.source == "foreignObject"
        assert diagnostic.message =~ "foreignObject"
      )
    end
  end

  test "effect-owned checkbox states remain real Forms when toggled and flattened" do
    html =
      "<body style='margin:0'><svg width='150' height='60'><g opacity='.4'>\n<rect width='150' height='60' fill='red'/><foreignObject x='10' y='10' width='120' height='40'>\n<div><input type='checkbox' name='tick' checked></div></foreignObject></g></svg></body>\n"

    (
      assert {:error, {:unsupported_html, diagnostic}} =
               HtmlToPdf.render(html, page_size: {120, 60}, margin: 0)

      assert diagnostic.reason == :unsupported_html
      assert diagnostic.stage == :layout
      assert diagnostic.module == NativeElixirPdfUtilities.Validators.SvgConverterValidator
      assert diagnostic.operation == :convert
      assert diagnostic.source == "foreignObject"
      assert diagnostic.message =~ "foreignObject"
    )

    (
      assert {:error, {:unsupported_html, diagnostic}} =
               HtmlToPdf.render(String.replace(html, " checked", ""),
                 page_size: {120, 60},
                 margin: 0
               )

      assert diagnostic.reason == :unsupported_html
      assert diagnostic.stage == :layout
      assert diagnostic.module == NativeElixirPdfUtilities.Validators.SvgConverterValidator
      assert diagnostic.operation == :convert
      assert diagnostic.source == "foreignObject"
      assert diagnostic.message =~ "foreignObject"
    )

    directory =
      Path.join(System.tmp_dir!(), "native-effect-button-#{System.unique_integer([:positive])}")

    File.mkdir_p!(directory)
    on_exit(fn -> File.rm_rf!(directory) end)
  end

  test "filled text paints inside the genuine ancestor opacity source graph" do
    html =
      "<body style='margin:0'><svg width='150' height='60'><g opacity='.25'>\n<rect width='150' height='60' fill='red'/><foreignObject x='10' y='10' width='120' height='40'>\n<div><input name='text' value='Old' style='width:80px;height:24px'></div></foreignObject></g></svg></body>\n"

    (
      assert {:error, {:unsupported_html, diagnostic}} =
               HtmlToPdf.render(html, page_size: {120, 60}, margin: 0)

      assert diagnostic.reason == :unsupported_html
      assert diagnostic.stage == :layout
      assert diagnostic.operation == :convert
      assert diagnostic.module == NativeElixirPdfUtilities.Validators.SvgConverterValidator
      assert diagnostic.source == "foreignObject"
      assert diagnostic.message =~ "foreignObject"
    )
  end

  test "filled text remains clipped by the genuine ancestor SVG mask" do
    html =
      "<body style='margin:0'><svg width='150' height='60'><defs><mask id='m' maskUnits='userSpaceOnUse' x='0' y='0' width='150' height='60'><rect width='75' height='60' fill='white'/></mask></defs><g mask='url(#m)'>\n<rect width='150' height='60' fill='red'/><foreignObject x='10' y='10' width='130' height='40'>\n<div><input name='text' value='Old' style='width:110px;height:24px'></div></foreignObject></g></svg></body>\n"

    (
      assert {:error, {:unsupported_html, diagnostic}} =
               HtmlToPdf.render(html, page_size: {120, 60}, margin: 0)

      assert diagnostic.reason == :unsupported_html
      assert diagnostic.stage == :layout
      assert diagnostic.operation == :convert
      assert diagnostic.module == NativeElixirPdfUtilities.Validators.SvgConverterValidator
      assert diagnostic.source == "foreignObject"
      assert diagnostic.message =~ "foreignObject"
    )
  end
end
