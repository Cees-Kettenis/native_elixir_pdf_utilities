defmodule NativeElixirPdfUtilities.HtmlToPdf.ScrollbarValidatorTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.Limits
  alias NativeElixirPdfUtilities.HtmlToPdf.{PdfWriter, ScrollbarPainter}
  alias NativeElixirPdfUtilities.Validators.{ScrollbarValidator, WriterValidator}

  setup do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    :ok
  end

  test "zero offset matches seven immutable e7 theme geometry cases" do
    # Captured independently by evaluating the accepted e7d261 geometry, not
    # generated from this shared validator. Both tiny hosts and axes are retained.
    {cases, _} = Code.eval_file("test/fixtures/html_to_pdf/e7_scrollbar_parts.terms")

    for {geometry, expected} <- cases do
      assert {:ok, ^expected} = ScrollbarValidator.prepare(geometry)

      assert {:ok, ^expected} =
               ScrollbarValidator.prepare(Map.put(geometry, :scroll_offset, %{x: 0.0, y: 0.0}))

      boxes = ScrollbarPainter.paint(expected)
      refute Enum.any?(boxes, &(Map.has_key?(&1, :link_url) or Map.has_key?(&1, :html_targets)))
    end
  end

  test "popover overflow measures clipped image artwork and ignores page-break markers" do
    content = %{x: 6.0, top: 90.0, width: 60.0, height: 24.0, content_height: 6.0}
    padding = %{left: 3.0, right: 3.0, top: 3.0, bottom: 3.0}

    boxes = [
      %{type: :page_break},
      %{
        type: :image,
        x: 6.0,
        y: 0.0,
        width: 100.0,
        height: 90.0,
        clip: %{x: 6.0, y: 84.0, width: 12.0, height: 6.0}
      }
    ]

    assert {:paint, geometry} = ScrollbarValidator.popover_geometry(boxes, content, padding, nil)
    refute geometry.vertical or geometry.horizontal
    assert {:ok, []} = ScrollbarValidator.prepare(geometry)
  end

  test "scroll offsets move the shared thumb through its available track" do
    geometry = %{
      vertical: true,
      horizontal: true,
      viewport_width: 150.0,
      viewport_height: 75.0,
      client_width: 138.75,
      client_height: 63.75,
      scroll_width: 277.5,
      scroll_height: 127.5
    }

    assert {:ok, first} = ScrollbarValidator.prepare(geometry)

    assert {:ok, last} =
             ScrollbarValidator.prepare(Map.put(geometry, :scroll_offset, %{x: 138.75, y: 63.75}))

    assert {:ok, middle} =
             ScrollbarValidator.prepare(
               Map.put(geometry, :scroll_offset, %{x: 69.375, y: 31.875})
             )

    [first_v, first_h] = Enum.filter(first, &(&1.kind == :thumb))
    [last_v, last_h] = Enum.filter(last, &(&1.kind == :thumb))
    [middle_v, middle_h] = Enum.filter(middle, &(&1.kind == :thumb))
    assert first_v.y > middle_v.y and middle_v.y > last_v.y
    assert first_h.x < middle_h.x and middle_h.x < last_h.x
    assert last_v.y == 24.75 and last_h.x + last_h.width == 125.25
    assert Enum.filter(first, &(&1.kind != :thumb)) == Enum.filter(last, &(&1.kind != :thumb))

    assert {:ok, parts} =
             ScrollbarValidator.prepare(%{
               geometry
               | vertical: false,
                 horizontal: false,
                 client_width: 150.0,
                 client_height: 75.0
             })

    assert parts == []

    assert {:ok, parts} =
             ScrollbarValidator.prepare(%{
               geometry
               | horizontal: false,
                 client_height: 75.0,
                 scroll_height: 75.0
             })

    refute Enum.any?(parts, &(&1.kind == :thumb))
  end

  test "scrollbar transport rejects malformed states dimensions and offsets with diagnostics" do
    geometry = %{
      vertical: true,
      horizontal: false,
      viewport_width: 150.0,
      viewport_height: 75.0,
      client_width: 138.75,
      client_height: 75.0,
      scroll_width: 138.75,
      scroll_height: 150.0
    }

    for invalid <- [
          nil,
          %{},
          Map.put(geometry, :vertical, nil),
          Map.put(geometry, :extra, 1),
          Map.put(geometry, :scroll_width, 10),
          Map.put(geometry, :scroll_offset, %{x: 0, y: 0, extra: 1}),
          Map.put(geometry, :client_width, 151),
          Map.put(geometry, :client_width, 140),
          Map.put(geometry, :client_height, 76),
          Map.put(geometry, :scroll_width, "wide"),
          Map.put(geometry, :scroll_height, -1),
          Map.put(geometry, :viewport_width, 1_000_000_001),
          Map.put(geometry, :scroll_offset, %{}),
          Map.put(geometry, :scroll_offset, %{x: -1, y: 0}),
          Map.put(geometry, :scroll_offset, %{x: 0, y: 76})
        ] do
      assert {:error, {:invalid_layout, diagnostic}} = ScrollbarValidator.prepare(invalid)
      assert diagnostic.stage == :layout and diagnostic.operation == :prepare_scrollbar

      assert diagnostic.source == "scrollbar geometry" and
               diagnostic.message =~ "finite nonnegative"
    end

    Limits.install(%{Limits.effective() | max_css_numeric_magnitude: 10})

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             ScrollbarValidator.prepare(%{
               vertical: true,
               horizontal: false,
               viewport_width: 1.0,
               viewport_height: 1.0,
               client_width: 0.0,
               client_height: 1.0,
               scroll_width: 0.0,
               scroll_height: 1.0
             })

    assert diagnostic.message =~ "max_css_numeric_magnitude"
  end

  test "direct clipped arrow paths preserve accepted shapes and diagnose invalid transport" do
    for {direction, path, scrollbar_path} <- [
          {:up, "0 0 m 6 0 l 3 3 l h", "-0.0001 0.0001 m 5.9999 0.0001 l 2.9999 3.0001 l h"},
          {:down, "0 3 m 6 3 l 3 0 l h", "-0.0001 3.0001 m 5.9999 3.0001 l 2.9999 0.0001 l h"},
          {:left, "6 0 m 6 3 l 0 1.5 l h", "5.9999 0.0001 m 5.9999 3.0001 l -0.0001 1.5001 l h"},
          {:right, "0 0 m 0 3 l 6 1.5 l h", "-0.0001 0.0001 m -0.0001 3.0001 l 5.9999 1.5001 l h"}
        ] do
      [box] =
        ScrollbarPainter.paint([
          %{kind: :arrow, direction: direction, x: 0, y: 0, width: 6, height: 3, border_radius: 0}
        ])

      assert box.clip == %{x: 0, y: 0, width: 6, height: 3}
      # Default output compresses page streams. Check the actual decoded path
      # and clip rather than mistaking compressed bytes for missing arrow paint.
      for compressed <- [true, false] do
        assert {:ok, pdf} =
                 PdfWriter.render([%{size: {30, 30}, boxes: [box]}], compress_streams: compressed)

        assert {:ok, document} = NativeElixirPdfUtilities.Pdf.Reader.read(pdf)
        [page] = document.pages

        assert {:ok, content} =
                 NativeElixirPdfUtilities.Pdf.Reader.decoded_stream(
                   document,
                   document.objects[page.ref].value["Contents"]
                 )

        # Fluent scrollbar artwork has the writer's fixed four-decimal raster
        # boundary bias. General triangles keep their original source geometry.
        assert content =~ scrollbar_path
        assert content =~ "0 0 6 3 re W n"

        assert {:ok, plain} =
                 PdfWriter.render([%{size: {30, 30}, boxes: [Map.delete(box, :role)]}],
                   compress_streams: compressed
                 )

        assert {:ok, plain_document} = NativeElixirPdfUtilities.Pdf.Reader.read(plain)
        [plain_page] = plain_document.pages

        assert {:ok, plain_content} =
                 NativeElixirPdfUtilities.Pdf.Reader.decoded_stream(
                   plain_document,
                   plain_document.objects[plain_page.ref].value["Contents"]
                 )

        assert plain_content =~ path
        assert plain_content =~ "0 0 6 3 re W n"
      end

      assert {:error, {:invalid_pdf_input, %{stage: :pdf, message: message}}} =
               WriterValidator.prepare(
                 [%{size: {30, 30}, boxes: [Map.put(box, :shape, {:triangle, :invalid})]}],
                 []
               )

      assert message != ""
    end
  end
end
