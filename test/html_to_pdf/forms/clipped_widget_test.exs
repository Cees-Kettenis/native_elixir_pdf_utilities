defmodule NativeElixirPdfUtilities.HtmlToPdf.ClippedWidgetTest do
  use ExUnit.Case, async: true
  alias NativeElixirPdfUtilities.Forms
  alias NativeElixirPdfUtilities.HtmlToPdf

  test "fully clipped controls create no widget while a partial control uses its visible rectangle" do
    for popover <- ["", "popover"] do
      html =
        "<div #{popover} style='display:block;position:static;width:120pt;height:12pt;overflow:hidden'><p>Label</p><input name=inside value=Native></div>"

      assert {:ok, pdf} = HtmlToPdf.render(html, system_font_discovery: false)
      assert {:ok, []} = Forms.fields(pdf)
    end

    html = File.read!("test/fixtures/html_to_pdf/browser_parity/clipped_form_controls.html")
    assert {:ok, pdf} = HtmlToPdf.render(html, system_font_discovery: false)
    assert {:ok, [field]} = Forms.fields(pdf)
    assert field.name == "partial" and field.value == "Visible"
    [left, bottom, right, top] = hd(field.widgets).rect
    assert_in_delta right - left, 60.0, 0.001
    assert top - bottom > 0 and top - bottom <= 12.0
    assert {:ok, filled} = Forms.fill(pdf, %{"partial" => "Updated"})
    assert {:ok, flat} = Forms.flatten(filled)
    assert {:ok, []} = Forms.fields(flat)
  end
end
