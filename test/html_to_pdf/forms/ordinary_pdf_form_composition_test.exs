defmodule NativeElixirPdfUtilities.HtmlToPdf.OrdinaryPdfFormCompositionTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.HtmlToPdf.{Font, FormAppearanceComposition, PdfWriter}
  alias NativeElixirPdfUtilities.Pdf.{IncrementalWriter, Reader}
  alias NativeElixirPdfUtilities.Validators.{PdfFormValidator, WriterValidator}
  alias NativeElixirPdfUtilities.{Limits, Text}

  setup do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    data = File.read!("priv/fonts/dejavu/DejaVuSans.ttf")
    {:ok, [face]} = Font.load_configured_faces([%{family: "SourceFace", data: data}])

    box = %{
      type: :text,
      text: "Source ffi",
      x: 30,
      y: 45,
      font: Font.pdf_name(face),
      font_face: face,
      font_size: 12,
      color: {0, 0, 0},
      css_source_positioned: true,
      snap_to_css_pixel_grid: false
    }

    {:ok, frame: %{size: {100, 60}, boxes: [box]}, box: box, face: face}
  end

  test "ordinary appearance Form preserves its page source canvas, selected font and affine clip",
       %{frame: frame} do
    frame = Map.put(frame, :paint_frame, %{x: 20, y: 30, width: 200, height: 155})
    clip = [{10, 20}, {130, 20}, {130, 90}, {10, 90}]

    assert {:ok, projected} =
             FormAppearanceComposition.project_appearance(
               frame,
               {1, 0, 0.1, 1, 10, 20},
               [clip],
               {10, 20, 130, 90}
             )

    assert projected.size == {120, 70}
    [image] = projected.boxes
    assert image.image.format == :pdf_form
    assert {:ok, source} = PdfFormValidator.prepare_source(image.image.data)
    assert source.width == 120 and source.height == 70
    assert source.stream =~ "1 0 0.1 1 0 0 cm"
    assert source.stream =~ "W n"
    assert image.image.data =~ "/FontFile2"
    assert image.image.data =~ "q 1 0 0 1 -20 -30 cm"
    refute image.image.data =~ "NEPUEffectProgram"
    refute image.image.data =~ "NEPUSourceGlyphs"
    assert {:ok, text} = Text.extract(image.image.data, layout: false)
    assert text =~ "Source ffi"
    assert {:ok, imported} = PdfWriter.render([projected], compress_streams: false)
    assert {:ok, imported_text} = Text.extract(imported, layout: false)
    assert imported_text =~ "Source ffi"
    artifacts = "tmp/test_artifacts/ordinary_pdf_form_composition/ordinary-appearance"
    File.mkdir_p!(artifacts)
    File.write!(Path.join(artifacts, "projected.pdf"), image.image.data)
    File.write!(Path.join(artifacts, "imported.pdf"), imported)
  end

  test "projection input and derived coordinates use recoverable canonical diagnostics", %{
    frame: frame
  } do
    polygon = [{0, 0}, {100, 0}, {100, 60}, {0, 60}]

    for {appearance, matrix, polygons, bounds} <- [
          {nil, {1, 0, 0, 1, 0, 0}, [polygon], {0, 0, 100, 60}},
          {frame, {1, 0, 0, 0, 0, 0}, [polygon], {0, 0, 100, 60}},
          {frame, {1, 0, 0, 1, 0, 0}, [[{0, 0}]], {0, 0, 100, 60}},
          {frame, {1, 0, 0, 1, 0, 0}, [polygon], {0, 0, 0, 60}},
          {Map.put(frame, :paint_frame, %{height: "invalid"}), {1, 0, 0, 1, 0, 0}, [polygon],
           {0, 0, 100, 60}}
        ] do
      assert {:error, {reason, diagnostic}} =
               FormAppearanceComposition.project_appearance(appearance, matrix, polygons, bounds)

      assert diagnostic.reason == reason
      assert is_atom(diagnostic.stage)
      assert is_binary(diagnostic.message)
    end

    limit = Limits.get(:max_pdf_numeric_magnitude)

    assert {:error, {_, diagnostic}} =
             WriterValidator.prepare_appearance_projection(
               frame,
               {1, 0, 0, 1, limit, 0},
               [polygon],
               {-limit, 0, -limit + 100, 60}
             )

    assert diagnostic.message =~ "max_pdf_numeric_magnitude"
  end

  test "ordinary affine projection rejects overflow in derived clip coordinates", %{
    frame: frame
  } do
    limit = Limits.get(:max_pdf_numeric_magnitude)

    assert {:error, {_, diagnostic}} =
             WriterValidator.prepare_appearance_projection(
               frame,
               {1, 0, 0, 1, -limit, 0},
               [[{limit - 1, 0}, {limit, 0}, {limit, 60}, {limit - 1, 60}]],
               {-limit, 0, -limit + 100, 60}
             )

    assert diagnostic.message =~ "max_pdf_numeric_magnitude"
  end

  test "ordered PDF content streams and resources survive ordinary Form import", %{
    frame: frame
  } do
    assert {:ok, pdf} = PdfWriter.render([frame], compress_streams: false)
    assert {:ok, context} = Reader.read_validated(pdf)
    [page] = context.document.pages
    page_value = context.document.objects[page.ref].value
    content = page_value["Contents"]
    assert {:ok, original} = Reader.decoded_stream(context.document, content)
    id = context.document.trailer["Size"]
    {page_id, generation} = page.ref
    extra = "q 1 0 0 rg 0 0 4 4 re f Q"

    assert {:ok, updated} =
             IncrementalWriter.write(context, [
               {id, 0, {:stream, %{}, extra}},
               {page_id, generation,
                {:value, Map.put(page_value, "Contents", [content, {:ref, {id, 0}}])}}
             ])

    assert {:ok, prepared} = PdfFormValidator.prepare_source(updated)
    assert prepared.stream == original <> "\n" <> extra
    assert prepared.content_dictionary == %{}
    assert map_size(prepared.resources["Font"]) == 1
    assert {:ok, multi} = PdfWriter.render([frame, frame])
    assert {:error, {:invalid_pdf_input, diagnostic}} = PdfFormValidator.prepare_source(multi)
    assert diagnostic.operation == :prepare_pdf_form
    assert diagnostic.message =~ "one origin-zero page"

    assert {:ok, invalid_origin} =
             IncrementalWriter.write(context, [
               {page_id, generation, {:value, Map.put(page_value, "MediaBox", [1, 0, 101, 60])}}
             ])

    assert {:error, {:invalid_pdf_input, _}} = PdfFormValidator.prepare_source(invalid_origin)
  end

  test "source-positioned text retains baseline and rejects nonboolean metadata", %{
    frame: frame,
    box: box
  } do
    assert %{y: y} = WriterValidator.prepare_text_paint(box, 155)
    canvas = NativeElixirPdfUtilities.HtmlToPdf.DrawingGeometry.css_print_canvas(155)
    assert_in_delta y, (155 - box.y) * canvas.css_pixels_per_point, 0.000001
    assert {:ok, _} = PdfWriter.render([frame])

    assert {:error, {:invalid_pdf_input, diagnostic}} =
             PdfWriter.render([%{frame | boxes: [Map.put(box, :css_source_positioned, :yes)]}])

    assert diagnostic.stage == :pdf and diagnostic.operation == :write_pdf
  end

  test "plain empty paint clips hide source ink while appearance frames stay positive", %{
    frame: frame,
    box: box
  } do
    for clip <- [%{x: 0, y: 0, width: 100, height: 0}, %{x: 0, y: 0, width: 0, height: 60}] do
      assert :ok = WriterValidator.validate_clip_rectangle(clip)

      assert {:ok, pdf} =
               PdfWriter.render([%{frame | boxes: [Map.put(box, :clip, clip)]}],
                 compress_streams: false
               )

      assert pdf =~ "re W n"

      if clip.height == 0 do
        path =
          "tmp/test_artifacts/ordinary_pdf_form_composition/empty-clip.pdf"

        File.mkdir_p!(Path.dirname(path))
        File.write!(path, pdf)
      end

      refute pdf =~ "/Annots"

      assert {:error, {:invalid_pdf_input, _}} =
               PdfWriter.appearance_objects([Map.put(frame, :paint_frame, clip)], 1)
    end

    assert {:error, {:invalid_pdf_input, _}} =
             WriterValidator.validate_clip_rectangle(%{x: 0, y: 0, width: -1, height: 60})
  end

  test "malformed page Contents cannot become an imported Form", %{frame: frame} do
    [box] = frame.boxes
    frame = %{frame | boxes: [%{box | text: String.duplicate("Source ffi ", 40)}]}
    assert {:ok, pdf} = PdfWriter.render([frame], compress_streams: true)
    assert {:ok, prepared} = PdfFormValidator.prepare_source(pdf)
    assert prepared.content_dictionary["Filter"] == {:name, "FlateDecode"}
    assert {:ok, context} = Reader.read_validated(pdf)
    [page] = context.document.pages
    page_value = context.document.objects[page.ref].value
    {id, generation} = page.ref

    for contents <- [0, [], {:ref, page.ref}, [{:ref, page.ref}]] do
      assert {:ok, malformed} =
               IncrementalWriter.write(context, [
                 {id, generation, {:value, Map.put(page_value, "Contents", contents)}}
               ])

      assert {:error, {reason, diagnostic}} = PdfFormValidator.prepare_source(malformed)
      assert diagnostic.reason == reason
      assert is_binary(diagnostic.message) and diagnostic.message != ""
    end
  end
end
