defmodule NativeElixirPdfUtilities.StampTest do
  use ExUnit.Case, async: false

  alias NativeElixirPdfUtilities.HtmlToPdf
  alias NativeElixirPdfUtilities.Info
  alias NativeElixirPdfUtilities.Limits
  alias NativeElixirPdfUtilities.Outlines
  alias NativeElixirPdfUtilities.Pdf.IncrementalWriter
  alias NativeElixirPdfUtilities.Pdf.Reader
  alias NativeElixirPdfUtilities.Pdf.StampWriter
  alias NativeElixirPdfUtilities.Stamp
  alias NativeElixirPdfUtilities.Text
  alias NativeElixirPdfUtilities.Transform
  alias NativeElixirPdfUtilities.Validators.StampValidator

  setup do
    original_limits = Limits.effective()
    on_exit(fn -> Limits.install(original_limits) end)
    :ok
  end

  test "adds text while preserving the original revision, metadata, and outlines" do
    pdf = document_pdf()

    assert {:ok, stamped} =
             Stamp.text(pdf, "APPROVED",
               pages: [2],
               position: :top_right,
               margin: 12,
               size: 14,
               color: {0.0, 0.5, 0.1},
               opacity: 0.8,
               rotation: 10
             )

    assert String.starts_with?(stamped, pdf)
    assert {:ok, 3} = Info.page_count(stamped)
    assert {:ok, %{title: "Stamp source"}} = Info.get(stamped)

    assert {:ok, [%{title: "First"}, %{title: "Second"}, %{title: "Third"}]} =
             Outlines.get(stamped)

    assert {:ok, "First\nSecond APPROVED\nThird"} = Text.extract(stamped, layout: false)

    assert {:ok, explicit} = Stamp.text(pdf, "XY", position: {10, 20}, font_weight: 700)
    assert {:ok, text} = Text.extract(explicit, layout: false)
    assert text =~ "XY"
  end

  test "uses watermark defaults and accepts every named position" do
    pdf = one_page_pdf("Original", {300, 200})

    assert {:ok, watermarked} = Stamp.watermark(pdf, "DRAFT")
    assert watermarked =~ "/Subtype /Form"
    assert watermarked =~ "/ExtGState"
    assert {:ok, "Original DRAFT"} = Text.extract(watermarked, layout: false)

    positions = [
      :top_left,
      :top_center,
      :top_right,
      :center_left,
      :center,
      :center_right,
      :bottom_left,
      :bottom_center,
      :bottom_right
    ]

    Enum.each(positions, fn position ->
      assert {:ok, stamped} = Stamp.text(pdf, Atom.to_string(position), position: position)
      assert {:ok, extracted} = Text.extract(stamped, layout: false)
      assert extracted =~ Atom.to_string(position)
    end)
  end

  test "adds document and selection-relative page numbers" do
    pdf = document_pdf()

    assert {:ok, document_numbers} =
             Stamp.page_numbers(pdf,
               pages: [2..3],
               format: "{{page}}/{{pages}}",
               numbering: :document,
               position: :bottom_left
             )

    assert {:ok, "First\nSecond 2/3\nThird 3/3"} = Text.extract(document_numbers, layout: false)

    assert {:ok, selection_numbers} =
             Stamp.page_numbers(pdf,
               pages: [2..3],
               format: "Sheet {{page}} of {{pages}}",
               numbering: :selection,
               position: :bottom_right,
               font_style: :italic
             )

    assert {:ok, "First\nSecond Sheet 1 of 2\nThird Sheet 2 of 2"} =
             Text.extract(selection_numbers, layout: false)
  end

  test "preserves ordered contents through indirect arrays for extraction and stamping" do
    for contents <- ["[4 0 R 6 0 R]", "7 0 R"] do
      source =
        pdf([
          {1, "<< /Type /Catalog /Pages 2 0 R >>"},
          {2, "<< /Type /Pages /Kids [3 0 R] /Count 1 /MediaBox [0 0 300 200] >>"},
          {3,
           "<< /Type /Page /Parent 2 0 R /Contents 5 0 R /Resources << /Font << /F1 8 0 R >> >> >>"},
          {4, stream_object("", "q BT /F1 12 Tf (One) Tj")},
          {5, contents},
          {6, stream_object("", "(Two) Tj ET Q")},
          {7, "[4 0 R 6 0 R]"},
          {8, "<< /Type /Font /Subtype /Type1 /BaseFont /Helvetica >>"}
        ])

      assert {:ok, "OneTwo"} = Text.extract(source, layout: false)
      assert {:ok, stamped} = Stamp.text(source, "Mark")
      assert {:ok, "OneTwo Mark"} = Text.extract(stamped, layout: false)
      assert {:ok, overlaid} = Stamp.overlay(one_page_pdf("Target", {300, 200}), source)
      assert {:ok, "Target OneTwo"} = Text.extract(overlaid, layout: false)
    end
  end

  test "returns diagnostics when a validated overlay stream cannot be decoded" do
    overlay =
      pdf([
        {1, "<< /Type /Catalog /Pages 2 0 R >>"},
        {2, "<< /Type /Pages /Kids [3 0 R] /Count 1 /MediaBox [0 0 300 200] >>"},
        {3, "<< /Type /Page /Parent 2 0 R /Contents 4 0 R >>"},
        {4, stream_object("/Filter /FlateDecode", "invalid compressed bytes")}
      ])

    assert {:error, {reason, diagnostic}} =
             Stamp.overlay(one_page_pdf("Target", {300, 200}), overlay)

    assert reason == :invalid_pdf_input
    assert diagnostic.reason == reason
    assert diagnostic.message =~ "FlateDecode"
    assert diagnostic.module == Stamp
  end

  test "counts the graphics-state isolation stream in object capacity" do
    target = one_page_pdf("Original", {300, 200})
    assert {:ok, stamped} = Stamp.text(target, "Mark")
    assert {:ok, context} = Reader.read_validated(stamped)
    maximum_objects = context.document.trailer["Size"] - 1
    original = Limits.effective()
    Limits.install(%{original | max_pdf_objects: maximum_objects})
    assert {:ok, _stamped} = Stamp.text(target, "Mark")
    Limits.install(%{original | max_pdf_objects: maximum_objects - 1})
    assert {:error, {:resource_limit_exceeded, %{stage: :limits}}} = Stamp.text(target, "Mark")
  end

  test "imports resource streams with indirect lengths" do
    content = "BT /F1 12 Tf (Imported) Tj ET"

    overlay =
      pdf([
        {1, "<< /Type /Catalog /Pages 2 0 R >>"},
        {2, "<< /Type /Pages /Kids [3 0 R] /Count 1 /MediaBox [0 0 300 200] >>"},
        {3,
         "<< /Type /Page /Parent 2 0 R /Contents 4 0 R /Resources << /XObject << /Form 5 0 R >> >> >>"},
        {4, stream_object("", "/Form Do")},
        {5,
         "<< /Type /XObject /Subtype /Form /BBox [0 0 300 200] /Resources << /Font << /F1 7 0 R >> >> /Length 6 0 R >>\nstream\n#{content}\nendstream"},
        {6, Integer.to_string(byte_size(content))},
        {7, "<< /Type /Font /Subtype /Type1 /BaseFont /Helvetica >>"}
      ])

    assert {:ok, stamped} = Stamp.overlay(one_page_pdf("Original", {300, 200}), overlay)
    assert {:ok, "Original Imported"} = Text.extract(stamped, layout: false)
  end

  test "repeats and matches PDF overlay pages" do
    target = document_pdf()
    overlay = one_page_pdf("LETTERHEAD", {300, 200})

    assert {:ok, repeated} =
             Stamp.overlay(target, overlay,
               pages: [1, 3],
               overlay_pages: {:repeat, 1},
               opacity: 0.5
             )

    assert {:ok, "First LETTERHEAD\nSecond\nThird LETTERHEAD"} =
             Text.extract(repeated, layout: false)

    matched_overlay = three_page_pdf(["ONE", "TWO", "THREE"], {300, 200})
    assert {:ok, matched} = Stamp.overlay(target, matched_overlay, overlay_pages: :match)

    assert {:ok, "First ONE\nSecond TWO\nThird THREE"} = Text.extract(matched, layout: false)
  end

  test "supports explicit overlay fit modes" do
    target = one_page_pdf("Target", {300, 200})
    overlay = one_page_pdf("Overlay", {100, 100})

    for fit <- [:contain, :cover, :stretch] do
      assert {:ok, stamped} = Stamp.overlay(target, overlay, fit: fit)
      assert {:ok, "Target Overlay"} = Text.extract(stamped, layout: false)
    end

    assert {:error, {:invalid_stamp, exact}} = Stamp.overlay(target, overlay)
    assert exact.stage == :geometry
  end

  test "uses displayed geometry for rotated pages and UserUnit" do
    source = one_page_pdf("Rotated", {300, 200})
    assert {:ok, rotated} = Transform.rotate_pages(source, 90)

    assert {:ok, stamped} =
             Stamp.page_numbers(rotated,
               position: :bottom_right,
               margin: 8,
               color: {0, 0, 1}
             )

    assert {:ok, [%{rotation: 90, width: 200.0, height: 300.0}]} = Info.page_sizes(stamped)
    assert {:ok, "Rotated Page 1 of 1"} = Text.extract(stamped, layout: false)

    user_unit =
      pdf([
        {1, "<< /Type /Catalog /Pages 2 0 R >>"},
        {2, "<< /Type /Pages /Kids [3 0 R] /Count 1 >>"},
        {3,
         "<< /Type /Page /Parent 2 0 R /MediaBox [10 20 110 70] /CropBox [20 25 100 65] /UserUnit 2 >>"}
      ])

    assert {:ok, unit_stamped} = Stamp.text(user_unit, "Units", position: :bottom_center)
    assert {:ok, [%{width: 200.0, height: 100.0}]} = Info.page_sizes(unit_stamped)
    assert {:ok, "Units"} = Text.extract(unit_stamped, layout: false)
  end

  test "preserves target resource names by selecting collision-free stamp names" do
    target =
      pdf([
        {1, "<< /Type /Catalog /Pages 2 0 R >>"},
        {2, "<< /Type /Pages /Kids [3 0 R] /Count 1 /MediaBox [0 0 300 200] >>"},
        {3,
         "<< /Type /Page /Parent 2 0 R /Resources << /XObject << /NEPUStamp 4 0 R >> /ExtGState << /NEPUStampGS 5 0 R >> >> >>"},
        {4, stream_object("/Type /XObject /Subtype /Form /BBox [0 0 1 1]", "")},
        {5, "<< /Type /ExtGState /ca 1 >>"}
      ])

    overlay = one_page_pdf("Overlay", {300, 200})
    assert {:ok, stamped} = Stamp.overlay(target, overlay, opacity: 0.5)
    assert stamped =~ "/NEPUStamp2"
    assert stamped =~ "/NEPUStampGS2"
  end

  test "validates text, options, fonts, selections, and overlay mappings" do
    pdf = one_page_pdf("Source", {300, 200})
    overlay = one_page_pdf("Overlay", {300, 200})

    invalid_calls = [
      Stamp.text(pdf, "", []),
      Stamp.text(pdf, <<255>>, []),
      Stamp.text(pdf, "x", :bad),
      Stamp.text(pdf, "x", [:bad]),
      Stamp.text(pdf, "x", size: 10, size: 12),
      Stamp.text(pdf, "x", unknown: true),
      Stamp.text(pdf, "x", size: 0),
      Stamp.text(pdf, "x", margin: -1),
      Stamp.text(pdf, "x", color: {2, 0, 0}),
      Stamp.text(pdf, "x", color: :red),
      Stamp.text(pdf, "x", opacity: 2),
      Stamp.text(pdf, "x", rotation: :bad),
      Stamp.text(pdf, "x", position: :somewhere),
      Stamp.text(pdf, "x", font: "Missing Font"),
      Stamp.text(pdf, "x", fonts: [:bad]),
      Stamp.text(pdf, "x", font_weight: :bold),
      Stamp.text(pdf, "x", font_style: :oblique),
      Stamp.text(pdf, "x", system_font_discovery: :yes),
      Stamp.text(pdf, <<0xF4, 0x8F, 0xBF, 0xBF>>),
      Stamp.text(pdf, "x", pages: []),
      Stamp.text(pdf, "x", pages: [2]),
      Stamp.page_numbers(pdf, format: "constant"),
      Stamp.page_numbers(pdf, numbering: :bad),
      Stamp.overlay(pdf, overlay, overlay_pages: :bad),
      Stamp.overlay(pdf, overlay, overlay_pages: {:repeat, 2}),
      Stamp.overlay(pdf, overlay, fit: :bad),
      Stamp.overlay(pdf, overlay, pages: [])
    ]

    Enum.each(invalid_calls, fn result ->
      assert {:error, {reason, diagnostic}} = result

      assert reason in [
               :invalid_options,
               :invalid_page_selection,
               :invalid_stamp,
               :page_out_of_bounds,
               :unsupported_glyph
             ]

      assert diagnostic.module == Stamp
      assert is_atom(diagnostic.operation)
      assert is_binary(diagnostic.message)
    end)

    two_page_overlay = three_page_pdf(["One", "Two"], {300, 200})

    assert {:error, {:invalid_stamp, %{stage: :overlay}}} =
             Stamp.overlay(pdf, two_page_overlay, overlay_pages: :match)
  end

  test "returns shared diagnostics for malformed PDFs and unsupported overlay resources" do
    pdf = one_page_pdf("Source", {300, 200})

    for result <- [
          Stamp.text(:bad, "x"),
          Stamp.watermark("not pdf", "x"),
          Stamp.page_numbers("not pdf"),
          Stamp.overlay(pdf, "not pdf"),
          Stamp.overlay("not pdf", pdf)
        ] do
      assert {:error, {:invalid_pdf_input, diagnostic}} = result
      assert diagnostic.module == Stamp
    end

    page_resource_overlay =
      pdf([
        {1, "<< /Type /Catalog /Pages 2 0 R >>"},
        {2, "<< /Type /Pages /Kids [3 0 R] /Count 1 /MediaBox [0 0 300 200] >>"},
        {3, "<< /Type /Page /Parent 2 0 R /Resources << /Properties << /Bad 3 0 R >> >> >>"}
      ])

    assert {:error, {:unsupported_pdf_feature, diagnostic}} =
             Stamp.overlay(pdf, page_resource_overlay)

    assert diagnostic.stage == :overlay
  end

  test "enforces text, decoded-content, and object capacity limits" do
    pdf = one_page_pdf("Source", {300, 200})
    original = Limits.effective()

    Limits.install(%{original | max_stamp_text_bytes: 8})

    assert {:error, {:resource_limit_exceeded, %{stage: :limits}}} =
             Stamp.text(pdf, "too long!")

    ten_page_pdf = three_page_pdf(List.duplicate("x", 10), {300, 200})

    assert {:error, {:resource_limit_exceeded, %{stage: :limits}}} =
             Stamp.page_numbers(ten_page_pdf, format: "{{page}}")

    Limits.install(%{original | max_stamp_decoded_content_bytes: 3})
    overlay = one_page_pdf("Overlay", {300, 200})

    assert {:error, {:resource_limit_exceeded, %{stage: :limits}}} =
             Stamp.overlay(pdf, overlay)

    Limits.install(%{original | max_pdf_objects: 12})

    assert {:error, {:resource_limit_exceeded, %{stage: :limits}}} =
             Stamp.overlay(pdf, one_page_pdf("O", {300, 200}))
  end

  test "stops overlay decoding at the aggregate budget before reading later streams" do
    target = one_page_pdf("Target", {300, 200})
    Limits.install(%{Limits.effective() | max_stamp_decoded_content_bytes: 6})

    for {entries, data} <- [{"", "q Q"}, {"/Filter /FlateDecode", :zlib.compress("q Q")}] do
      overlay =
        pdf([
          {1, "<< /Type /Catalog /Pages 2 0 R >>"},
          {2, "<< /Type /Pages /Kids [3 0 R] /Count 1 /MediaBox [0 0 300 200] >>"},
          {3, "<< /Type /Page /Parent 2 0 R /Contents [4 0 R 4 0 R 5 0 R] >>"},
          {4, stream_object(entries, data)},
          {5, stream_object("/Filter /FlateDecode", "invalid compressed data")}
        ])

      assert {:error, {:resource_limit_exceeded, diagnostic}} = Stamp.overlay(target, overlay)
      assert diagnostic.stage == :limits
      assert diagnostic.reason == :resource_limit_exceeded
      assert diagnostic.operation == :overlay_pdf
      assert diagnostic.module == Stamp
      assert diagnostic.message == "decoded overlay content exceeds the limit"
    end
  end

  test "charges overlay separators and distinct source pages without charging repeated placements" do
    target = three_page_pdf(["One", "Two"], {300, 200})

    for content <- ["", "q Q"] do
      overlay =
        pdf([
          {1, "<< /Type /Catalog /Pages 2 0 R >>"},
          {2, "<< /Type /Pages /Kids [3 0 R 4 0 R] /Count 2 /MediaBox [0 0 300 200] >>"},
          {3, "<< /Type /Page /Parent 2 0 R /Contents [5 0 R 5 0 R] >>"},
          {4, "<< /Type /Page /Parent 2 0 R /Contents [5 0 R 5 0 R] >>"},
          {5, stream_object("", content)}
        ])

      page_bytes = 2 * byte_size(content) + 1
      Limits.install(%{Limits.effective() | max_stamp_decoded_content_bytes: page_bytes})
      assert {:ok, _stamped} = Stamp.overlay(target, overlay)

      assert {:error, {:resource_limit_exceeded, %{stage: :limits}}} =
               Stamp.overlay(target, overlay, overlay_pages: :match)

      Limits.install(%{Limits.effective() | max_stamp_decoded_content_bytes: 2 * page_bytes})
      assert {:ok, _stamped} = Stamp.overlay(target, overlay, overlay_pages: :match)
    end
  end

  test "validates malformed prepared writer and incremental contexts" do
    assert {:error, {:invalid_pdf_input, %{stage: :validation}}} =
             StampValidator.prepare_text(%{}, "x", [], :text)

    assert {:error, {:invalid_pdf_input, %{stage: :validation}}} =
             StampValidator.prepare_generated_overlay(%{}, %{}, %{})

    assert {:error, {:invalid_pdf_input, %{stage: :incremental_write}}} = StampWriter.write(%{})

    assert {:error, {:invalid_pdf_input, %{stage: :incremental_write}}} =
             IncrementalWriter.write(%{}, [])

    {:ok, context} = Reader.read_validated(one_page_pdf("Source", {300, 200}))

    assert {:error, {:invalid_pdf_input, _diagnostic}} =
             IncrementalWriter.write(context, [{0, 0, {:value, %{}}}])

    assert {:error, {:invalid_pdf_input, _diagnostic}} =
             IncrementalWriter.write(context, [{99, 0, :bad}])

    assert {:error, {:invalid_pdf_input, _diagnostic}} =
             IncrementalWriter.write(context, [
               {99, 0, {:value, %{}}},
               {99, 0, {:value, %{}}}
             ])

    assert {:error, {:invalid_pdf_input, _diagnostic}} =
             IncrementalWriter.write(context, [{99, 0, {:stream, %{"Bad" => self()}, ""}}])

    assert {:ok, empty_increment} = IncrementalWriter.write(context, [])
    assert String.starts_with?(empty_increment, context.document.binary)

    identifier_context =
      put_in(context.document.trailer["ID"], [{:hex, <<1, 2>>}, {:hex, <<3, 4>>}])

    assert {:ok, identified} =
             IncrementalWriter.write(identifier_context, [{99, 0, {:value, %{}}}])

    assert identified =~ "/ID"

    invalid_trailer_context = put_in(context.document.trailer["Root"], self())

    assert {:error, {:invalid_pdf_input, _diagnostic}} =
             IncrementalWriter.write(invalid_trailer_context, [{99, 0, {:value, %{}}}])

    pdf_without_newline =
      binary_part(context.document.binary, 0, byte_size(context.document.binary) - 1)

    no_newline_context = put_in(context.document.binary, pdf_without_newline)

    assert {:ok, separated} =
             IncrementalWriter.write(no_newline_context, [{99, 0, {:value, %{}}}])

    assert binary_part(separated, byte_size(pdf_without_newline), 1) == "\n"
  end

  test "validates prepared page geometry and overlay resource graphs" do
    {:ok, target_context} = Reader.read_validated(one_page_pdf("Target", {300, 200}))
    {:ok, overlay_context} = Reader.read_validated(one_page_pdf("Overlay", {300, 200}))
    target_page = hd(target_context.pages)
    overlay_page = hd(overlay_context.pages)

    assert {:error, {:invalid_page_selection, _diagnostic}} =
             StampValidator.prepare_text(%{target_context | pages: []}, "x", [], :text)

    invalid_pages = [
      %{target_page | crop_box: [0, 0, 0, 200]},
      %{target_page | rotate: 45},
      %{target_page | dictionary: Map.put(target_page.dictionary, "UserUnit", 0)},
      %{target_page | resources: 3},
      %{target_page | resources: %{"XObject" => 3}},
      %{target_page | dictionary: Map.put(target_page.dictionary, "Contents", [3])},
      %{target_page | dictionary: Map.put(target_page.dictionary, "Contents", 3)}
    ]

    Enum.each(invalid_pages, fn page ->
      assert {:error, {:invalid_pdf_input, diagnostic}} =
               StampValidator.prepare_text(%{target_context | pages: [page]}, "x", [], :text)

      assert diagnostic.stage == :geometry
    end)

    contents = Map.fetch!(target_page.dictionary, "Contents")

    content_array_page = %{
      target_page
      | dictionary: Map.put(target_page.dictionary, "Contents", [contents])
    }

    assert {:ok, _plan} =
             StampValidator.prepare_text(
               %{target_context | pages: [content_array_page]},
               "x",
               [],
               :text
             )

    for rotation <- [180, 270] do
      assert {:ok, rotated} = Transform.rotate_pages(target_context.document.binary, rotation)
      assert {:ok, _stamped} = Stamp.text(rotated, "x")
    end

    valid_group_page =
      %{
        overlay_page
        | dictionary:
            Map.put(overlay_page.dictionary, "Group", %{
              "S" => {:name, "Transparency"},
              "CS" => {:name, "DeviceRGB"}
            })
      }

    assert {:ok, grouped_plan} =
             StampValidator.prepare_overlay(
               target_context,
               %{overlay_context | pages: [valid_group_page]},
               []
             )

    assert {:ok, _grouped_pdf} = StampWriter.write(grouped_plan)

    invalid_group_page = %{
      overlay_page
      | dictionary: Map.put(overlay_page.dictionary, "Group", 3)
    }

    assert {:error, {:invalid_pdf_input, %{stage: :overlay}}} =
             StampValidator.prepare_overlay(
               target_context,
               %{overlay_context | pages: [invalid_group_page]},
               []
             )

    missing_content_page = %{
      overlay_page
      | dictionary: Map.put(overlay_page.dictionary, "Contents", {:ref, {999, 0}})
    }

    assert {:error, {:invalid_pdf_input, _diagnostic}} =
             StampValidator.prepare_overlay(
               target_context,
               %{overlay_context | pages: [missing_content_page]},
               []
             )

    missing_resource_page = %{
      overlay_page
      | resources: %{"Properties" => %{"Missing" => {:ref, {999, 0}}}}
    }

    assert {:error, {:invalid_pdf_input, %{stage: :overlay}}} =
             StampValidator.prepare_overlay(
               target_context,
               %{overlay_context | pages: [missing_resource_page]},
               []
             )

    nested_object = %{
      offset: 0,
      stream: nil,
      tokens: [],
      value: %{"Missing" => {:ref, {999, 0}}}
    }

    nested_document = %{
      overlay_context.document
      | objects: Map.put(overlay_context.document.objects, {900, 0}, nested_object)
    }

    nested_resource_page = %{
      overlay_page
      | resources: %{"Properties" => %{"Nested" => {:ref, {900, 0}}}}
    }

    assert {:error, {:invalid_pdf_input, %{stage: :overlay}}} =
             StampValidator.prepare_overlay(
               target_context,
               %{overlay_context | document: nested_document, pages: [nested_resource_page]},
               []
             )

    assert {:ok, empty_plan} =
             StampValidator.prepare_generated_overlay(
               target_context,
               overlay_context,
               %{target_pages: [], appearances: []}
             )

    assert empty_plan.placements == []
  end

  defp document_pdf do
    {:ok, pdf} =
      HtmlToPdf.render(
        """
        <h1>First</h1><div style="break-before: page"><h1>Second</h1></div>
        <div style="break-before: page"><h1>Third</h1></div>
        """,
        page_size: {300, 200},
        margin: 10,
        metadata: [title: "Stamp source"],
        outlines: :headings
      )

    pdf
  end

  defp one_page_pdf(text, page_size) do
    three_page_pdf([text], page_size)
  end

  defp three_page_pdf(texts, page_size) do
    html =
      texts
      |> Enum.with_index()
      |> Enum.map_join(fn {text, index} ->
        break = if index == 0, do: "", else: "break-before: page;"
        ~s(<div style="#{break} font-size: 18pt">#{text}</div>)
      end)

    {:ok, pdf} = HtmlToPdf.render(html, page_size: page_size, margin: 10)
    pdf
  end

  defp stream_object(entries, data) do
    "<< #{entries} /Length #{byte_size(data)} >>\nstream\n#{data}\nendstream"
  end

  defp pdf(objects) do
    header = "%PDF-1.7\n"

    {body, offsets, _position} =
      Enum.reduce(objects, {[], %{}, byte_size(header)}, fn {id, object},
                                                            {parts, offsets, position} ->
        rendered = "#{id} 0 obj\n#{object}\nendobj\n"
        {[rendered | parts], Map.put(offsets, id, position), position + byte_size(rendered)}
      end)

    body = body |> Enum.reverse() |> IO.iodata_to_binary()
    xref_offset = byte_size(header) + byte_size(body)
    maximum_id = objects |> Enum.map(&elem(&1, 0)) |> Enum.max()

    entries =
      Enum.map_join(1..maximum_id, fn id ->
        offset = Map.get(offsets, id, 0)
        status = if Map.has_key?(offsets, id), do: "n", else: "f"
        generation = if status == "n", do: "00000", else: "00000"
        String.pad_leading(Integer.to_string(offset), 10, "0") <> " #{generation} #{status} \n"
      end)

    header <>
      body <>
      "xref\n0 #{maximum_id + 1}\n0000000000 65535 f \n" <>
      entries <>
      "trailer\n<< /Size #{maximum_id + 1} /Root 1 0 R >>\nstartxref\n#{xref_offset}\n%%EOF\n"
  end
end
