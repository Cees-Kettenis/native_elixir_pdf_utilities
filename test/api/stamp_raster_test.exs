defmodule NativeElixirPdfUtilities.StampRasterTest do
  use ExUnit.Case, async: true

  alias NativeElixirPdfUtilities.HtmlToPdf.PdfWriter
  alias NativeElixirPdfUtilities.Stamp
  alias NativeElixirPdfUtilities.TestSupport.PdfVisualCompare

  alias NativeElixirPdfUtilities.TestSupport.Paths

  setup do
    directory =
      Paths.project("tmp/stamp_raster/#{System.pid()}-#{System.unique_integer([:positive])}")

    File.mkdir_p!(directory)
    on_exit(fn -> File.rm_rf!(directory) end)
    {:ok, artifact_dir: directory}
  end

  test "importing artwork diagnoses source layer settings and stamping preserves target visibility",
       %{
         artifact_dir: directory
       } do
    alias NativeElixirPdfUtilities.{Merge, Split, Transform}
    alias NativeElixirPdfUtilities.Pdf.{IncrementalWriter, Reader}

    page = %{size: {100.0, 100.0}, boxes: []}
    assert {:ok, blank} = PdfWriter.render([page, page])
    assert {:ok, context} = Reader.read_validated(blank)
    {catalog_id, generation} = context.catalog_ref
    [first, _second] = context.pages
    {page_id, page_generation} = first.ref
    layer_id = context.document.trailer["Size"]
    layer = {:ref, {layer_id, 0}}
    stream = layer_id + 1
    config = layer_id + 2

    dictionary =
      Map.merge(first.dictionary, %{
        "Contents" => {:ref, {stream, 0}},
        "Resources" => %{"Properties" => %{"Swatch" => layer}}
      })

    paint = "1 0 0 rg 10 10 20 20 re f /OC /Swatch BDC 0 0 1 rg 40 40 20 20 re f EMC"

    layers =
      for {state, color} <- [{"OFF", <<255, 255, 255>>}, {"ON", <<0, 0, 255>>}] do
        properties = %{"OCGs" => [layer], "D" => %{"BaseState" => {:name, state}}}
        entry = if state == "OFF", do: properties, else: {:ref, {config, 0}}

        assert {:ok, layered} =
                 IncrementalWriter.write(context, [
                   {catalog_id, generation,
                    {:value, Map.put(context.catalog, "OCProperties", entry)}},
                   {page_id, page_generation, {:value, dictionary}},
                   {layer_id, 0,
                    {:value, %{"Type" => {:name, "OCG"}, "Name" => {:string, "Swatch"}}}},
                   {stream, 0, {:stream, %{}, paint}},
                   {config, 0, {:value, properties}}
                 ])

        assert {:ok, stamped} = Stamp.overlay(layered, blank)
        assert {:ok, reopened} = Reader.read_validated(stamped)
        assert reopened.catalog["OCProperties"] == entry
        assert {:ok, ^properties} = Reader.resolve(reopened.document, entry)

        assert [%{width: 100, pixels: pixels}, _] =
                 PdfVisualCompare.rasterize_pdf!(stamped, artifact_dir: directory)

        assert binary_part(pixels, (80 * 100 + 20) * 3, 3) == <<255, 0, 0>>
        assert binary_part(pixels, (50 * 100 + 50) * 3, 3) == color

        for {module, operation, result} <- [
              {Merge, :merge, Merge.merge([blank, layered])},
              {Transform, :pick_pages, Transform.pick_pages(layered, [2, 1])},
              {Transform, :rotate_pages, Transform.rotate_pages(layered, 90)},
              {Transform, :delete_pages, Transform.delete_pages(layered, [1])},
              {Split, :split_by_page, Split.by_page(layered)},
              {Stamp, :overlay_pdf, Stamp.overlay(blank, layered)}
            ] do
          assert {:error, {:unsupported_pdf_feature, diagnostic}} = result
          assert diagnostic.stage == :optional_content
          assert diagnostic.reason == :unsupported_pdf_feature
          assert diagnostic.module == module
          assert diagnostic.operation == operation
          assert diagnostic.message =~ "layer visibility"
          if module == Merge, do: assert(diagnostic.source == "merge input 2")
        end

        layered
      end

    assert {:error, {:unsupported_pdf_feature, %{source: "merge input 1"}}} = Merge.merge(layers)

    for entry <- [nil, {:ref, {config, 0}}] do
      assert {:ok, ordinary} =
               IncrementalWriter.write(context, [
                 {catalog_id, generation,
                  {:value, Map.put(context.catalog, "OCProperties", entry)}},
                 {config, 0, {:value, nil}}
               ])

      assert {:ok, _} = Merge.merge([ordinary])
      assert {:ok, _} = Stamp.overlay(blank, ordinary)
    end
  end

  test "overlay opacity applies to composed artwork even when source alpha is explicit", %{
    artifact_dir: directory
  } do
    alias NativeElixirPdfUtilities.Pdf.{Reader, IncrementalWriter}
    assert {:ok, blank} = PdfWriter.render([%{size: {300.0, 200.0}, boxes: []}])
    assert {:ok, context} = Reader.read_validated(blank)
    page = hd(context.pages)
    {page_id, generation} = page.ref
    stream_id = context.document.trailer["Size"]

    for source_alpha <- [1.0, 0.5],
        group <- [nil, %{"S" => {:name, "Transparency"}, "I" => true}],
        opacity <- [0.0, 0.25, 1.0] do
      dictionary =
        Map.merge(page.dictionary, %{
          "Contents" => {:ref, {stream_id, 0}},
          "Resources" => %{
            "ExtGState" => %{
              "OwnAlpha" => %{
                "Type" => {:name, "ExtGState"},
                "ca" => source_alpha,
                "CA" => source_alpha
              }
            }
          }
        })

      dictionary = if group, do: Map.put(dictionary, "Group", group), else: dictionary

      assert {:ok, source} =
               IncrementalWriter.write(context, [
                 {stream_id, 0, {:stream, %{}, "/OwnAlpha gs 1 0 0 rg 10 10 50 50 re f"}},
                 {page_id, generation, {:value, dictionary}}
               ])

      assert {:ok, stamped} = Stamp.overlay(blank, source, opacity: opacity)

      assert [%{width: 300, height: 200, pixels: pixels}] =
               PdfVisualCompare.rasterize_pdf!(stamped, artifact_dir: directory)

      <<red, green, blue>> = binary_part(pixels, (150 * 300 + 20) * 3, 3)
      expected = round(255 * (1 - opacity * source_alpha))
      assert red == 255
      assert_in_delta green, expected, 1
      assert_in_delta blue, expected, 1
    end
  end

  test "isolates existing transforms and clips while preserving artwork and repeated stamps", %{
    artifact_dir: directory
  } do
    assert {:ok, blank} = PdfWriter.render([%{size: {300.0, 200.0}, boxes: []}])
    assert {:ok, context} = NativeElixirPdfUtilities.Pdf.Reader.read_validated(blank)
    page = hd(context.pages)
    {page_id, generation} = page.ref
    stream_id = context.document.trailer["Size"]

    for {state, index} <-
          Enum.with_index([
            "",
            "1 0 0 1 100 0 cm",
            "0 0 1 1 re W n",
            "2 0 0 2 20 30 cm 0 0 1 1 re W n"
          ]) do
      content = "0 0 0 rg 220 150 10 10 re f\n" <> state

      assert {:ok, target} =
               NativeElixirPdfUtilities.Pdf.IncrementalWriter.write(context, [
                 {stream_id, 0, {:stream, %{}, content}},
                 {page_id, generation,
                  {:value, Map.put(page.dictionary, "Contents", [{:ref, {stream_id, 0}}])}}
               ])

      assert {:ok, stamped} = Stamp.text(target, "MARK", position: {10, 20}, size: 12)

      assert [stats] =
               PdfVisualCompare.assert_pdf_visual_change!(target, stamped,
                 artifact_dir: Path.join(directory, "state-#{index}")
               )

      assert stats.changed_pixels > 50
      {min_x, min_y, max_x, max_y} = stats.bounds
      assert min_x in 10..15
      assert max_x < 50
      assert min_y in 20..30
      assert max_y < 35

      assert {:ok, repeated} = Stamp.text(stamped, "NEXT", position: {80, 20}, size: 12)

      assert [repeated_stats] =
               PdfVisualCompare.assert_pdf_visual_change!(stamped, repeated,
                 artifact_dir: Path.join(directory, "state-repeat-#{index}")
               )

      assert repeated_stats.changed_pixels > 50
      {repeat_x, repeat_y, repeat_right, repeat_bottom} = repeated_stats.bounds
      assert repeat_x in 80..85
      assert repeat_right < 120
      assert repeat_y in 20..30
      assert repeat_bottom < 35
    end
  end

  test "places a diagonal watermark across the page center", %{artifact_dir: directory} do
    assert {:ok, pdf} = PdfWriter.render([%{size: {300.0, 200.0}, boxes: []}])

    assert {:ok, watermarked} =
             Stamp.watermark(pdf, "DRAFT", color: {0.8, 0.0, 0.0}, opacity: 0.4)

    assert [stats] =
             PdfVisualCompare.assert_pdf_visual_change!(pdf, watermarked,
               artifact_dir: Path.join(directory, "watermark")
             )

    assert stats.width == 300
    assert stats.height == 200
    assert stats.changed_ratio > 0.005
    assert stats.changed_ratio < 0.15

    {min_x, min_y, max_x, max_y} = stats.bounds
    assert min_x in 20..130
    assert min_y in 15..100
    assert max_x in 170..280
    assert max_y in 100..185
    slope_extent = (max_y - min_y) / (max_x - min_x)
    assert slope_extent > 0.5 and slope_extent < 1.5
  end

  test "oversized crop boxes keep centered stamps visible through rotation and page scaling", %{
    artifact_dir: directory
  } do
    alias NativeElixirPdfUtilities.Pdf.{Reader, IncrementalWriter}
    assert {:ok, blank} = PdfWriter.render([%{size: {200.0, 100.0}, boxes: []}])
    assert {:ok, context} = Reader.read_validated(blank)
    page = hd(context.pages)
    {id, generation} = page.ref

    for rotation <- [0, 90, 180, 270], unit <- [1, 2] do
      dictionary =
        Map.merge(page.dictionary, %{
          "CropBox" => [-100, -100, 1000, 1000],
          "Rotate" => rotation,
          "UserUnit" => unit
        })

      assert {:ok, source} =
               IncrementalWriter.write(context, [{id, generation, {:value, dictionary}}])

      assert {:ok, stamped} = Stamp.text(source, "MARK", position: :center, color: {1, 0, 0})

      assert [stats] =
               PdfVisualCompare.assert_pdf_visual_change!(source, stamped,
                 artifact_dir: Path.join(directory, "crop-#{rotation}-#{unit}")
               )

      {left, top, right, bottom} = stats.bounds
      assert stats.changed_pixels > 10
      assert abs((left + right) / 2 - stats.width / 2) < 12
      assert abs((top + bottom) / 2 - stats.height / 2) < 12
    end
  end

  test "places an anchored text stamp at the top right", %{artifact_dir: directory} do
    assert {:ok, pdf} = PdfWriter.render([%{size: {300.0, 200.0}, boxes: []}])

    assert {:ok, stamped} =
             Stamp.text(pdf, "APPROVED",
               position: :top_right,
               margin: 12,
               size: 18,
               color: {0.0, 0.2, 0.8}
             )

    assert [stats] =
             PdfVisualCompare.assert_pdf_visual_change!(pdf, stamped,
               artifact_dir: Path.join(directory, "anchor")
             )

    {min_x, min_y, max_x, max_y} = stats.bounds
    assert min_x > 180
    assert min_y in 12..20
    assert max_x in 280..289
    assert max_y in 24..40
  end
end
