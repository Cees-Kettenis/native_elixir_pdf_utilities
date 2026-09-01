defmodule NativeElixirPdfUtilities.PageTransformsTest do
  use ExUnit.Case, async: false

  alias NativeElixirPdfUtilities.Info
  alias NativeElixirPdfUtilities.Limits
  alias NativeElixirPdfUtilities.Merge
  alias NativeElixirPdfUtilities.Pdf.Reader
  alias NativeElixirPdfUtilities.Split
  alias NativeElixirPdfUtilities.Text
  alias NativeElixirPdfUtilities.Transform
  alias NativeElixirPdfUtilities.Validators.SplitValidator
  alias NativeElixirPdfUtilities.Validators.TransformValidator

  @fixture_directory Path.expand("fixtures/pdf_transforms", __DIR__)

  setup do
    original_limits = Limits.effective()
    on_exit(fn -> Limits.install(original_limits) end)
    :ok
  end

  test "picks and reorders pages while rebuilding a readable PDF" do
    source = three_page_pdf()

    assert {:ok, transformed} = Transform.pick_pages(source, [3, 1])
    assert {:ok, %{pages: pages}} = Reader.read(transformed)
    assert length(pages) == 2
    refute transformed =~ "not retained"
    assert {:ok, "Three\nOne"} = Text.extract(transformed, layout: false)
    assert transformed =~ "/Dest"
  end

  test "deletes pages and unreachable page data from the rebuilt PDF" do
    source = three_page_pdf()

    assert {:ok, transformed} = Transform.delete_pages(source, [2..3])
    assert {:ok, %{pages: [_page]}} = Reader.read(transformed)
    assert {:ok, "One"} = Text.extract(transformed, layout: false)
    refute transformed =~ "Two"
    refute transformed =~ "Three"
    refute transformed =~ "/Dest"
    assert transformed =~ "/URI"
  end

  test "rotates selected pages relative to their effective rotations" do
    source = three_page_pdf()

    assert {:ok, transformed} = Transform.rotate_pages(source, -90, pages: [1, 2])
    assert {:ok, sizes} = Info.page_sizes(transformed)
    assert Enum.map(sizes, & &1.rotation) == [270, 0, 0]

    assert Enum.map(sizes, &{&1.width, &1.height}) == [
             {200.0, 300.0},
             {300.0, 200.0},
             {300.0, 200.0}
           ]

    assert {:ok, all_rotated} = Transform.rotate_pages(source, 90)
    assert {:ok, all_sizes} = Info.page_sizes(all_rotated)
    assert Enum.map(all_sizes, & &1.rotation) == [90, 180, 90]
  end

  test "validates picks, deletions, rotations, options, and bounds" do
    source = three_page_pdf()

    for selection <- [[], [0], [4], [2, 2], [3..1//-1], [:bad]] do
      assert {:error, {reason, diagnostic}} = Transform.pick_pages(source, selection)
      assert reason in [:invalid_page_selection, :page_out_of_bounds]
      assert diagnostic.operation == :pick_pages
      assert diagnostic.module == Transform
    end

    assert {:error, {:invalid_page_selection, deletion}} =
             Transform.delete_pages(source, [1..3])

    assert deletion.message == "page deletion must leave at least one page"

    assert {:error, {:invalid_rotation, rotation}} = Transform.rotate_pages(source, 45)
    assert rotation.operation == :rotate_pages

    assert {:error, {:invalid_options, options}} =
             Transform.rotate_pages(source, 90, unknown: true)

    assert options.stage == :options
  end

  test "splits every page into independently readable PDFs" do
    assert {:ok, outputs} = three_page_pdf() |> Split.by_page()
    assert length(outputs) == 3

    assert Enum.map(outputs, fn output ->
             assert {:ok, %{pages: [_page]}} = Reader.read(output)
             assert {:ok, text} = Text.extract(output, layout: false)
             text
           end) == ["One", "Two", "Three"]
  end

  test "splits inclusive and overlapping ranges" do
    assert {:ok, [first, second]} = Split.by_ranges(three_page_pdf(), [1..2, 2..3])
    assert {:ok, "One\nTwo"} = Text.extract(first, layout: false)
    assert {:ok, "Two\nThree"} = Text.extract(second, layout: false)
  end

  test "splits after a page into exactly two non-empty PDFs" do
    assert {:ok, {before_pdf, after_pdf}} = Split.after_page(three_page_pdf(), 2)
    assert {:ok, "One\nTwo"} = Text.extract(before_pdf, layout: false)
    assert {:ok, "Three"} = Text.extract(after_pdf, layout: false)

    for split_point <- [0, 3, 4, :bad] do
      assert {:error, {:invalid_page_selection, diagnostic}} =
               Split.after_page(three_page_pdf(), split_point)

      assert diagnostic.operation == :split_after_page
    end
  end

  test "validates split ranges and split resource limits" do
    source = three_page_pdf()

    for ranges <- [[], [1], [0..1], [3..1//-1], [1..4]] do
      assert {:error, {reason, diagnostic}} = Split.by_ranges(source, ranges)
      assert reason in [:invalid_page_range, :invalid_page_selection, :page_out_of_bounds]
      assert diagnostic.operation == :split_by_ranges
    end

    Limits.install(%{Limits.effective() | max_split_outputs: 2})

    assert {:error, {:resource_limit_exceeded, count_limit}} = Split.by_page(source)
    assert count_limit.message == "split output count exceeds the limit"
  end

  test "rejects malformed PDF inputs through the shared diagnostic contract" do
    for result <- [
          Transform.pick_pages(:invalid, [1]),
          Transform.delete_pages("not a PDF", [1]),
          Transform.rotate_pages("not a PDF", 90),
          Split.by_page(:invalid),
          Split.by_ranges("not a PDF", [1..1]),
          Split.after_page("not a PDF", 1)
        ] do
      assert {:error, {:invalid_pdf_input, diagnostic}} = result
      assert diagnostic.module in [Transform, Split]
      assert is_atom(diagnostic.operation)
    end
  end

  test "handles empty documents and validates internal preparation contexts" do
    empty =
      pdf([
        {1, "<< /Type /Catalog /Pages 2 0 R >>"},
        {2, "<< /Type /Pages /Kids [] /Count 0 >>"}
      ])

    assert {:ok, []} = Split.by_page(empty)

    assert {:error, {:invalid_page_selection, %{message: "rotation requires at least one page"}}} =
             Transform.rotate_pages(empty, 90)

    assert {:error, {:invalid_page_selection, _diagnostic}} = Transform.delete_pages(empty, [])

    assert {:error, {:invalid_pdf_input, %{stage: :validation}}} =
             TransformValidator.prepare_pick(%{}, [1])

    assert {:error, {:invalid_pdf_input, %{stage: :validation}}} =
             SplitValidator.prepare_each_page(%{})

    assert {:error, {:invalid_options, _diagnostic}} =
             Transform.rotate_pages(three_page_pdf(), 90, :invalid)

    assert {:error, {:invalid_page_selection, _diagnostic}} =
             TransformValidator.expand_page_selection(:invalid, 3, false)
  end

  test "omits absent rotation and annotations and handles indirect GoTo destinations" do
    no_optional_entries =
      pdf([
        {1, "<< /Type /Catalog /Pages 2 0 R >>"},
        {2, "<< /Type /Pages /Kids [3 0 R] /Count 1 /MediaBox [0 0 100 100] >>"},
        {3, "<< /Type /Page /Parent 2 0 R >>"}
      ])

    assert {:ok, rebuilt} = Transform.pick_pages(no_optional_entries, [1])
    assert {:ok, %{pages: [page]}} = Reader.read(rebuilt)
    assert page.rotate == nil

    named_only =
      three_page_pdf("/Annots [16 0 R]", [
        {16, "<< /Type /Annot /Subtype /Link /Rect [0 0 10 10] /Dest (chapter) >>"}
      ])

    assert {:ok, without_named_destination} = Transform.pick_pages(named_only, [1])
    refute without_named_destination =~ "/Annots"

    indirect_goto =
      three_page_pdf("/Annots [16 0 R]", [
        {16, "<< /Type /Annot /Subtype /Link /Rect [0 0 10 10] /A << /S /GoTo /D 17 0 R >> >>"},
        {17, "[5 0 R /Fit]"}
      ])

    assert {:ok, retained_goto} = Transform.pick_pages(indirect_goto, [3, 1])
    assert retained_goto =~ "/GoTo"
    assert {:ok, %{pages: [_, _]}} = Reader.read(retained_goto)
  end

  test "rejects malformed annotations instead of emitting dangling structures" do
    malformed_sources = [
      three_page_pdf("/Annots 42"),
      three_page_pdf("/Annots [42]"),
      three_page_pdf("/Annots [16 0 R]", [
        {16, "<< /Type /Annot /Subtype /Link /Rect [0 0 10 10] /A 42 >>"}
      ]),
      three_page_pdf("/Annots [16 0 R]", [
        {16, "<< /Type /Annot /Subtype /Link /Rect [0 0 10 10] /Dest [15 0 R /Fit] >>"}
      ]),
      three_page_pdf("/Annots [16 0 R]", [
        {16, "<< /Type /Annot /Subtype /Link /Rect [0 0 10 10] /Dest 42 >>"}
      ])
    ]

    for source <- malformed_sources do
      assert {:error, {:invalid_pdf_input, diagnostic}} = Transform.pick_pages(source, [1])
      assert diagnostic.stage == :annotations
    end
  end

  test "rejects retained non-navigation dependencies on removed pages" do
    source = three_page_pdf("/Private 4 0 R")

    assert {:error, {:unsupported_pdf_feature, diagnostic}} =
             Transform.pick_pages(source, [1])

    assert diagnostic.stage == :page_dependencies
    assert diagnostic.message =~ "page 4"

    assert {:error, {:unsupported_pdf_feature, split_diagnostic}} = Split.by_page(source)
    assert split_diagnostic.operation == :split_by_page
  end

  test "returns diagnostics for dangling references reached from retained pages" do
    source = three_page_pdf("/Private 16 0 R", [{17, "<< /Unused true >>"}])

    assert {:ok, _context} = Reader.read_validated(source)

    assert {:error, {:invalid_pdf_input, transform_diagnostic}} =
             Transform.pick_pages(source, [1])

    assert transform_diagnostic.stage == :page_dependencies
    assert transform_diagnostic.operation == :pick_pages
    assert transform_diagnostic.module == Transform
    assert transform_diagnostic.message =~ "missing indirect object 16 0"

    assert {:error, {:invalid_pdf_input, split_diagnostic}} = Split.by_page(source)
    assert split_diagnostic.stage == :page_dependencies
    assert split_diagnostic.operation == :split_by_page
    assert split_diagnostic.module == Split
    assert split_diagnostic.message =~ "missing indirect object 16 0"
  end

  test "enforces aggregate byte and object-write limits for split outputs" do
    source = three_page_pdf()
    original = Limits.effective()

    Limits.install(%{original | max_aggregate_split_output_bytes: 1})

    assert {:error, {:resource_limit_exceeded, bytes}} = Split.after_page(source, 1)
    assert bytes.message == "aggregate split output bytes exceed the limit"

    Limits.install(%{original | max_split_object_writes: 1})

    assert {:error, {:resource_limit_exceeded, objects}} = Split.by_ranges(source, [1..1])
    assert objects.message == "split object writes exceed the limit"
  end

  test "regresses assembled multi-page document content, geometry, and rotation" do
    source = File.read!(Path.join(@fixture_directory, "assembled-multi-page.pdf"))

    assert {:ok, {packet, final_page}} = Split.after_page(source, 3)
    assert {:ok, "Invoice\nLabel\nReport"} = Text.extract(packet, layout: false)
    assert {:ok, "Statement"} = Text.extract(final_page, layout: false)

    assert {:ok, reordered} = Transform.pick_pages(source, [4, 2, 1])
    assert {:ok, "Statement\nLabel\nInvoice"} = Text.extract(reordered, layout: false)
    assert {:ok, sizes} = Info.page_sizes(reordered)

    assert Enum.map(sizes, &{&1.width, &1.height, &1.rotation}) == [
             {612.0, 792.0, 0},
             {300.0, 200.0, 0},
             {612.0, 792.0, 0}
           ]
  end

  test "regresses transforms applied to realistic merged and nested-page-tree fixtures" do
    assembled = File.read!(Path.join(@fixture_directory, "assembled-multi-page.pdf"))
    report = File.read!(Path.join(@fixture_directory, "nested-multi-page-report.pdf"))

    assert {:ok, merged} = Merge.merge([report, assembled])
    assert {:ok, selected} = Transform.pick_pages(merged, [7, 1..2, 5])
    assert {:ok, "Statement\nCover\nDetail\nLabel"} = Text.extract(selected, layout: false)
    refute selected =~ "Summary"
    refute selected =~ "Invoice"
    refute selected =~ "Report"
    assert {:ok, %{pages: pages}} = Reader.read(selected)
    assert length(pages) == 4
  end

  defp three_page_pdf(page_one_extra \\ "", extra_objects \\ []) do
    content = fn text -> "BT /F1 12 Tf 20 100 Td (#{text}) Tj ET" end

    objects =
      [
        {1, "<< /Type /Catalog /Pages 2 0 R >>"},
        {2,
         "<< /Type /Pages /Kids [3 0 R 4 0 R 5 0 R] /Count 3 /Resources << /Font << /F1 15 0 R >> >> /MediaBox [0.0 0 300 200] /Rotate 0 >>"},
        {3,
         "<< /Type /Page /Parent 2 0 R /Contents 6 0 R /Annots [9 0 R 10 0 R 12 0 R 13 0 R 14 0 R] /CustomNull null /CustomTrue true /CustomFalse false /CustomReal 1.5 /CustomString (x) /CustomHex <AB> #{page_one_extra} >>"},
        {4, "<< /Type /Page /Parent 2 0 R /Contents 7 0 R /Rotate 90 >>"},
        {5, "<< /Type /Page /Parent 2 0 R /Contents 8 0 R >>"},
        {6, stream(content.("One"))},
        {7, stream(content.("Two"))},
        {8, stream(content.("Three"))},
        {9, "<< /Type /Annot /Subtype /Link /Rect [0 0 10 10] /Dest [5 0 R /Fit] >>"},
        {10,
         "<< /Type /Annot /Subtype /Link /Rect [0 0 10 10] /A << /S /URI /URI (https://example.com) >> >>"},
        {11, "<< /Unused (not retained) >>"},
        {12, "<< /Type /Annot /Subtype /Text /Rect [0 0 10 10] /Contents (note) >>"},
        {13, "<< /Type /Annot /Subtype /Link /Rect [0 0 10 10] >>"},
        {14, "<< /Type /Annot /Subtype /Link /Rect [0 0 10 10] /Dest /Chapter >>"},
        {15, "<< /Type /Font /Subtype /Type1 /BaseFont /Helvetica /Encoding /WinAnsiEncoding >>"}
      ] ++ extra_objects

    pdf(Enum.sort_by(objects, &elem(&1, 0)))
  end

  defp stream(content) do
    "<< /Length #{byte_size(content)} >>\nstream\n#{content}\nendstream"
  end

  defp pdf(objects) do
    header = "%PDF-1.7\n"

    {body, offsets} =
      Enum.reduce(objects, {header, %{}}, fn {id, source}, {body, offsets} ->
        rendered = "#{id} 0 obj\n#{source}\nendobj\n"
        {body <> rendered, Map.put(offsets, id, byte_size(body))}
      end)

    maximum = objects |> List.last() |> elem(0)
    xref_offset = byte_size(body)

    entries =
      Enum.map_join(0..maximum, fn object ->
        case Map.get(offsets, object) do
          nil -> "0000000000 " <> if(object == 0, do: "65535 f \n", else: "00000 f \n")
          offset -> String.pad_leading(Integer.to_string(offset), 10, "0") <> " 00000 n \n"
        end
      end)

    body <>
      "xref\n0 #{maximum + 1}\n" <>
      entries <>
      "trailer\n<< /Size #{maximum + 1} /Root 1 0 R >>\n" <>
      "startxref\n#{xref_offset}\n%%EOF\n"
  end
end
