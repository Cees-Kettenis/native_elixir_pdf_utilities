alias NativeElixirPdfUtilities.TestSupport.Paths

Code.require_file(Paths.support("svg_public_workflow.ex"))
Code.require_file(Paths.support("png_fixture.ex"))

defmodule NativeElixirPdfUtilities.HtmlToPdf.SvgFormGlyphSourceTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.{Forms, Text}
  alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, PdfWriter, ResourceContext}
  alias NativeElixirPdfUtilities.Validators.{HtmlValidator}

  test "own-PDF text operands bind selected font programs, CIDs and source scalar owners" do
    html =
      "<body style='margin:0;font-family:DejaVu Sans;font-size:16px'><span>Alpha ffi</span><b>Beta</b></body>"

    assert {:ok, _dom} = HtmlParser.parse_document_detailed(html)
    _budget = HtmlValidator.new_image_budget()

    assert {:ok, pdf} =
             NativeElixirPdfUtilities.HtmlToPdf.render(html,
               page_size: {200, 80},
               margin: 0,
               system_font_discovery: false
             )

    assert {:ok, text} = Text.extract(pdf, layout: false)
    assert String.replace(text, " ", "") == "AlphaffiBeta"
  end

  test "updated appearance glyphs retain their own current physical operands without borrowing authored DOM text" do
    html =
      "<body style='margin:0;font-family:DejaVu Sans;font-size:16px'><p>Authored ffi</p><input name='field' value='Initial' style='width:100px;height:24px'></body>"

    assert {:ok, _dom} = HtmlParser.parse_document_detailed(html)
    _budget = HtmlValidator.new_image_budget()

    assert {:ok, initial} =
             NativeElixirPdfUtilities.HtmlToPdf.render(html,
               page_size: {220, 100},
               margin: 0,
               system_font_discovery: false
             )

    assert {:ok, changed} = Forms.fill(initial, %{"field" => "Changed"})

    for {source, _value} <- [{initial, "Initial"}, {changed, "Changed"}] do
      assert {:ok, _flattened} = Forms.flatten(source)
    end
  end

  test "original foreignObject glyphs complete through runtime paint before parser replacement" do
    for transform <- ["", "skewX(15)"] do
      original =
        "<body><?start name='A'><svg width='220' height='80'><foreignObject x='10' y='8' width='190' height='50' transform='#{transform}'><div style='margin:0;font-family:DejaVu Sans;font-size:16px'>Alpha ffi<b>Beta</b></div></foreignObject></svg><?end?><template for='A'><p>Good</p></template></body>"

      assert {:error, {:unsupported_html, diagnostic}} =
               NativeElixirPdfUtilities.HtmlToPdf.render(original, system_font_discovery: false)

      assert diagnostic.reason == :unsupported_html
      assert diagnostic.stage == :layout
      assert diagnostic.module == NativeElixirPdfUtilities.Validators.SvgConverterValidator
      assert diagnostic.operation == :convert
      assert diagnostic.source == "foreignObject"
      assert diagnostic.message =~ "foreignObject"
    end
  end

  test "clipped original Form leaves remain pending instead of borrowing painted sibling coverage" do
    original =
      "<body><?start name='A'><svg width='220' height='80'><defs><clipPath id='clip'><rect width='20' height='80'/></clipPath></defs><g clip-path='url(#clip)'><foreignObject x='10' y='8' width='190' height='50'><div style='margin:0;font-family:DejaVu Sans;font-size:16px'>Alpha ffi<b>Beta</b></div></foreignObject></g></svg><?end?><template for='A'><p>Good</p></template></body>"

    (
      assert {:error, {:unsupported_html, diagnostic}} =
               NativeElixirPdfUtilities.HtmlToPdf.render(original, system_font_discovery: false)

      assert diagnostic.reason == :unsupported_html
      assert diagnostic.stage == :layout
      assert diagnostic.operation == :convert
      assert diagnostic.module == NativeElixirPdfUtilities.Validators.SvgConverterValidator
      assert diagnostic.source == "foreignObject"
      assert diagnostic.message =~ "foreignObject"
    )
  end

  test "a shared physical glyph can be omitted in one Form invocation without changing its sibling" do
    assert {:ok, source} =
             NativeElixirPdfUtilities.HtmlToPdf.render(
               "<body style='margin:0;font-family:DejaVu Sans;font-size:16px'>ffiZ</body>",
               page_size: {100, 40},
               margin: 0,
               system_font_discovery: false
             )

    image = %{format: :pdf_form, data: source, width_px: 134, height_px: 54}

    boxes =
      for {x, y} <- [{0, 0}, {100, 10}] do
        %{type: :image, x: x, y: y, width: 100, height: 40, image: image}
      end

    assert {:ok, __pdf} = PdfWriter.render([%{size: {220, 70}, boxes: boxes}])
    evidence = "tmp/test_artifacts/svg_form_glyph_source/physical"
    File.mkdir_p!(evidence)

    covered = %{
      type: :rect,
      x: 0,
      y: 0,
      width: 100,
      height: 40,
      fill_color: {1, 1, 1},
      stroke_color: nil,
      stroke_width: 0,
      border_radius: 0
    }

    first = Map.put(hd(boxes), :clip, %{x: 0, y: 0, width: 5, height: 40})
    top = %{hd(boxes) | x: 0, y: 0}
    assert {:ok, overlap} = PdfWriter.render([%{size: {220, 70}, boxes: [first, covered, top]}])
    assert {:ok, _overlap_text} = Text.extract(overlap, layout: false)
  end

  test "scalar shaping binds only to its current approved field and AP Form" do
    assert {:ok, origin} =
             NativeElixirPdfUtilities.HtmlToPdf.render(
               "<input name='field' value='Initial' style='width:100px;height:24px'>",
               system_font_discovery: false
             )

    assert {:ok, current} = Forms.fill(origin, %{"field" => "ffiZ"})
    assert {:ok, context} = NativeElixirPdfUtilities.Pdf.Reader.read_validated(current)

    assert {:ok, %{fields: [field]}} =
             NativeElixirPdfUtilities.Validators.FormValidator.inspect_document(context)

    [widget] = field.widgets

    _authority = %{
      source_pdf_sha256: :crypto.hash(:sha256, origin),
      origin_pdf_length: byte_size(origin),
      field_ref: field.ref,
      widget_ref: widget.ref,
      field_key: field.name,
      scalar: field.value
    }

    assert {:ok, _dom} =
             HtmlParser.parse_document_detailed(
               "<body style='margin:0;font-family:DejaVu Sans;font-size:16px'>ffiZ</body>"
             )

    _budget = HtmlValidator.new_image_budget()
    assert ResourceContext.current() == nil
    Process.delete(:actual_scalar_editor_pages)
    evidence = "tmp/test_artifacts/svg_form_glyph_source/physical"
    File.mkdir_p!(evidence)
    assert {:ok, origin_context} = NativeElixirPdfUtilities.Pdf.Reader.read_validated(origin)
    {:ref, {field_id, field_generation}} = field.ref

    assert {:ok, origin_dictionary} =
             NativeElixirPdfUtilities.Pdf.Reader.dictionary(origin_context.document, field.ref)

    assert {:ok, readonly_origin} =
             NativeElixirPdfUtilities.Pdf.IncrementalWriter.write(origin_context, [
               {field_id, field_generation, {:value, Map.put(origin_dictionary, "Ff", 1)}}
             ])

    assert {:ok, _readonly_context} =
             NativeElixirPdfUtilities.Pdf.Reader.read_validated(readonly_origin)
  end
end
