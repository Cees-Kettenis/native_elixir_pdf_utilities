defmodule NativeElixirPdfUtilities.Validators.WriterValidatorTest do
  use ExUnit.Case, async: true

  alias NativeElixirPdfUtilities.Validators.WriterValidator
  alias NativeElixirPdfUtilities.HtmlToPdf.{Font, PdfWriter}

  test "gauge fractions and fixed four-stop RGB shading reject malformed writer input" do
    rect = %{
      type: :rect,
      x: 0,
      y: 0,
      width: 20,
      height: 10,
      fill_color: {0, 1, 0},
      stroke_color: nil,
      stroke_width: 0,
      border_radius: 0
    }

    colors = [{0.5, 0.5, 0.5}, {1, 1, 1}, {0.8, 0.8, 0.8}, {0.5, 0.5, 0.5}]

    for fraction <- [-0.1, 1.1, "bad"],
        do:
          assert(
            {:error, {:invalid_pdf_input, _}} =
              PdfWriter.render([
                %{size: {100, 100}, boxes: [Map.put(rect, :gauge_fraction, fraction)]}
              ])
          )

    for gradient <- [
          false,
          [],
          tl(colors),
          [{-1, 0, 0} | tl(colors)],
          [{0, 0, 0, 1} | tl(colors)]
        ] do
      assert {:error, {:invalid_pdf_input, diagnostic}} =
               PdfWriter.render([
                 %{size: {100, 100}, boxes: [Map.put(rect, :gauge_gradient, gradient)]}
               ])

      assert diagnostic.stage == :pdf and diagnostic.reason == :invalid_pdf_input
      assert is_binary(diagnostic.message)
    end

    for flag <- [:gauge_quantize_value, :gauge_clip_track] do
      assert {:error, {:invalid_pdf_input, _}} =
               PdfWriter.render([%{size: {100, 100}, boxes: [Map.put(rect, flag, "bad")]}])
    end

    state =
      NativeElixirPdfUtilities.Validators.HtmlGaugeValidator.prepare("progress", %{
        "value" => ".5"
      })

    framed = rect |> Map.put(:role, :gauge_track) |> Map.put(:gauge_state, state)

    for frame <- [
          false,
          {0, 0, 0, 10},
          {-1, 0, 20, 10},
          {21, 0, 20, 10},
          {0, 11, 20, 10},
          {0, 0, "bad", 10},
          {0, 0, 1.0e308, 10}
        ] do
      assert {:error, {:invalid_pdf_input, _}} =
               PdfWriter.render([
                 %{size: {100, 100}, boxes: [Map.put(framed, :gauge_frame, frame)]}
               ])
    end

    for invalid <- [Map.put(framed, :role, :other), Map.put(framed, :gauge_state, %{})] do
      assert {:error, {:invalid_pdf_input, _}} =
               PdfWriter.render([
                 %{size: {100, 100}, boxes: [Map.put(invalid, :gauge_frame, {0, 0, 20, 10})]}
               ])
    end

    for fraction <- [0, 0.5, 1] do
      assert {:ok, pdf} =
               PdfWriter.render(
                 [
                   %{
                     size: {100, 100},
                     boxes: [
                       rect
                       |> Map.put(:gauge_fraction, fraction)
                       |> Map.put(:gauge_gradient, colors)
                     ]
                   }
                 ],
                 compress_streams: false
               )

      assert pdf =~ "/Shading << /Gauge"
      assert pdf =~ "/FunctionType 3"
      assert pdf =~ " re W n"
    end
  end

  test "drawing groups prepare nested clips and reject unsupported or malformed composition" do
    text = %{
      type: :text,
      text: "Link",
      x: 0,
      y: 0,
      width: 20,
      line_height: 10,
      font: "Helvetica",
      font_size: 10,
      color: {0, 0, 0},
      link_url: "https://example.com"
    }

    group = %{
      type: :drawing_group,
      width: 40,
      height: 30,
      baseline: 10,
      transform: {2, 0.0, 0.0, 3, 10, 20},
      clip: %{x: 5, y: 2, width: 20, height: 10},
      boxes: [text]
    }

    page = %{size: {200, 200}, boxes: [group]}

    assert {:ok, %{pages: [%{boxes: [start, prepared, finish]}]}} =
             WriterValidator.prepare([page], [])

    assert start.type == :drawing_group_start and finish.type == :drawing_group_end
    assert prepared.drawing_annotation_rect == {20.0, 26.0, 50.0, 50.0}
    assert prepared.drawing_page_height == 30

    for invalid <- [
          Map.delete(group, :width),
          %{group | width: -1},
          %{group | height: "bad"},
          %{group | boxes: :bad},
          %{group | boxes: [1]},
          %{group | boxes: [%{type: :flow_marker, x: 0, y: 0, width: 0, height: 0}]},
          %{group | boxes: [%{text | text: <<255>>}]},
          %{group | boxes: [Map.put(text, :snap_to_css_pixel_grid, :bad)]},
          %{group | boxes: [%{type: :glyph_group, actual_text: "x", boxes: :bad}]},
          %{group | boxes: [%{text | line_height: -1}]},
          %{group | boxes: [%{text | line_height: :bad}]},
          %{group | boxes: [%{text | x: 1_000_000_001}]},
          %{group | boxes: [Map.put(text, :baseline_snap_depth, Integer.pow(10, 1000))]},
          %{group | boxes: [Map.put(text, :letter_spacing, :bad)]},
          %{group | boxes: [Map.put(text, :stroke_width, 1_000_000_001)]},
          %{group | boxes: [Map.put(text, :drawing_annotation_rect, {0, 0, 1, 1})]},
          %{group | boxes: [Map.put(text, :drawing_page_height, 10)]},
          %{
            group
            | boxes: [
                %{type: :glyph_group, actual_text: "x", boxes: [Map.put(text, :width, :bad)]}
              ]
          },
          %{group | transform: {1, 0, 0, 1, :bad, 0}},
          %{group | transform: {0, 0, 0, 1, 0, 0}},
          %{group | transform: {0.00001, 0, 0, 1, 0, 0}},
          %{group | transform: {1, 1, 0, 1, 0, 0}},
          %{group | transform: {1, 0, 1, 1, 0, 0}},
          %{group | transform: {-1, 0, 0, 1, 0, 0}},
          %{group | transform: {1, 0, 0, -1, 0, 0}},
          %{group | transform: {1, 0, 0, 1, 0, 0, 0}},
          %{group | width: 1_000_000_001},
          %{group | baseline: -1},
          %{group | baseline: 31},
          %{group | clip: %{x: 0, y: 0, width: -1, height: 1}},
          %{group | clip: %{x: 0, y: 0, width: 1, height: :bad}},
          %{group | clip: %{x: 1_000_000_000, y: 0, width: 1, height: 1}},
          %{group | clip: %{x: 0, y: 0, width: 1, height: 1, extra: true}},
          %{group | clip: :bad},
          Map.put(group, :link_url, "https://example.com"),
          %{group | boxes: [Map.put(group, :snap_to_css_pixel_grid, :bad)]},
          %{group | boxes: [Map.put(group, :drawing_annotation_rect, {0, 0, 1, 1})]},
          %{group | boxes: [Map.put(group, :drawing_page_height, 10)]},
          %{group | transform: {1_000_000_000, 0, 0, 1, 0, 0}},
          %{group | boxes: [%{group | transform: {1_000_000_000, 0, 0, 1, 0, 0}}]},
          %{group | clip: %{x: 500_000_000, y: 0, width: 1, height: 1}},
          %{group | boxes: [%{text | x: 500_000_000}]}
        ] do
      assert {:error, {:invalid_pdf_input, diagnostic}} =
               PdfWriter.render([%{page | boxes: [invalid]}])

      assert diagnostic.stage == :pdf and diagnostic.reason == :invalid_pdf_input
      assert diagnostic.operation == :write_pdf and diagnostic.module == PdfWriter
      assert diagnostic.message =~ "drawing_group"
    end

    for controlled <- [
          Map.put(group, :form_control, %{}),
          %{group | boxes: [Map.put(text, :form_control, %{})]},
          %{
            group
            | boxes: [
                %{
                  type: :glyph_group,
                  actual_text: "x",
                  boxes: [Map.put(text, :form_control, %{})]
                }
              ]
          },
          %{group | boxes: [%{group | boxes: [Map.put(text, :form_control, %{})]}]}
        ] do
      assert {:error, {:unsupported_form, diagnostic}} =
               PdfWriter.render([%{page | boxes: [controlled]}])

      assert diagnostic.stage == :pdf and diagnostic.operation == :write_pdf
      assert diagnostic.reason == :unsupported_form and diagnostic.message =~ "transformed widget"
    end

    for dimension <- [:width, :height] do
      zero = Map.put(group, dimension, 0) |> Map.delete(:baseline)

      assert {:ok, %{pages: [%{boxes: [%{clip: clip}, prepared, _]}]}} =
               WriterValidator.prepare([%{page | boxes: [zero]}], [])

      assert clip == %{x: 0, y: 0, width: 0, height: 0}
      refute Map.has_key?(prepared, :link_url)
    end

    assert {:ok, _} = WriterValidator.prepare([%{page | boxes: [%{group | boxes: []}]}], [])
  end

  test "nonpainting flow markers normalize away and reject drawing or interaction data" do
    marker = %{type: :flow_marker, x: 10, y: 20, width: 0.0, height: 12}
    page = %{size: {100, 100}, boxes: [marker]}
    assert {:ok, %{pages: [%{boxes: []}]}} = WriterValidator.prepare([page], [])

    for invalid <- [
          Map.put(marker, :width, 1),
          Map.put(marker, :height, -1),
          Map.put(marker, :x, "invalid"),
          Map.delete(marker, :y),
          Map.put(marker, :text, "Hidden"),
          Map.put(marker, :image, %{}),
          Map.put(marker, :fill_color, {0, 0, 0}),
          Map.put(marker, :stroke_color, {0, 0, 0}),
          Map.put(marker, :font_face, %{}),
          Map.put(marker, :glyph_ids, [1]),
          Map.put(marker, :link_url, "https://example.com"),
          Map.put(marker, :form_control, %{})
        ] do
      assert {:error, {:invalid_pdf_input, diagnostic}} =
               PdfWriter.render([%{page | boxes: [invalid]}], [])

      assert diagnostic.stage == :pdf and diagnostic.reason == :invalid_pdf_input
      assert diagnostic.operation == :write_pdf and diagnostic.module == PdfWriter
      assert diagnostic.message =~ "flow_marker"
    end
  end

  test "CMYK inversion is restricted to JPEG CMYK images and boolean flags" do
    for {flag, color_space, expected} <- [
          {true, :device_cmyk, :ok},
          {false, :device_cmyk, :ok},
          {:invalid, :device_cmyk, :error},
          {true, :device_rgb, :error}
        ] do
      image = %{
        format: :jpeg,
        data: "jpeg",
        width_px: 1,
        height_px: 1,
        color_space: color_space,
        bits_per_component: 8,
        inverted_cmyk: flag
      }

      pages = [
        %{
          size: {100, 100},
          boxes: [%{type: :image, x: 0, y: 0, width: 10, height: 10, image: image}]
        }
      ]

      case expected do
        :ok ->
          assert {:ok, _plan} = WriterValidator.prepare(pages, [])

        :error ->
          assert {:error, {:invalid_pdf_input, %{stage: :pdf}}} =
                   WriterValidator.prepare(pages, [])
      end
    end
  end

  test "rejects conflicting JPEG color conventions before writing" do
    image = %{
      format: :jpeg,
      data: "jpeg",
      width_px: 1,
      height_px: 1,
      color_space: :device_rgb,
      bits_per_component: 8,
      color_transform: :ycck,
      inverted_cmyk: false
    }

    pages = [
      %{
        size: {100, 100},
        boxes: [%{type: :image, x: 0, y: 0, width: 10, height: 10, image: image}]
      }
    ]

    assert {:error, {:invalid_pdf_input, %{stage: :pdf}}} = WriterValidator.prepare(pages, [])
  end

  test "prepares valid pages and normalized metadata for serialization" do
    pages = [
      %{
        size: {100.0, 100.0},
        boxes: [
          %{
            type: :text,
            text: "Valid",
            x: 10.0,
            y: 20.0,
            font: "Helvetica",
            font_face: %{type: :built_in, family: "Helvetica", pdf_name: "Helvetica"},
            font_size: 12.0,
            color: {0, 0, 0}
          }
        ]
      }
    ]

    assert {:ok, %{pages: ^pages, metadata: %{keywords: "one, two", creation_date: "D:20260812"}}} =
             WriterValidator.prepare(pages,
               metadata: [keywords: ["one", "two"], creation_date: ~D[2026-08-12]]
             )
  end

  test "rejects non-boolean stream-compression options" do
    pages = [%{size: {100, 100}, boxes: []}]

    assert {:ok, %{compress_streams: true, subset_fonts: true}} =
             WriterValidator.prepare(pages, [])

    assert {:ok, %{compress_streams: false, subset_fonts: false}} =
             WriterValidator.prepare(pages, compress_streams: false, subset_fonts: false)

    assert {:ok, %{compress_streams: true}} =
             WriterValidator.prepare(pages, compress_streams: true)

    assert {:error,
            {:invalid_options, %{stage: :options, reason: :invalid_options, message: message}}} =
             WriterValidator.prepare(pages, compress_streams: :auto)

    assert message =~ "compress_streams"

    assert {:error,
            {:invalid_options,
             %{stage: :options, reason: :invalid_options, message: subset_message}}} =
             WriterValidator.prepare(pages, subset_fonts: :auto)

    assert subset_message =~ "subset_fonts"
  end

  test "rejects invalid writer colors before serialization" do
    pages = [
      %{
        size: {100.0, 100.0},
        boxes: [
          %{
            type: :text,
            text: "Invalid",
            x: 10.0,
            y: 20.0,
            font: "Helvetica",
            font_size: 12.0,
            color: {1.1, 0, 0}
          }
        ]
      }
    ]

    assert {:error, {:invalid_pdf_input, %{stage: :pdf}}} =
             WriterValidator.prepare(pages, [])
  end

  test "rejects incomplete and malformed embedded font models before writing" do
    {:ok, registry} = Font.load_registry(system_font_discovery: false)
    {:ok, _, font} = Font.resolve("DejaVu Sans", 400, :normal, registry)

    invalid_fonts =
      Enum.map(
        [:pdf_name, :bbox, :ascent, :descent, :default_width, :widths, :cmap],
        &Map.delete(font, &1)
      ) ++
        [
          %{font | pdf_name: "Bad /Type /Action"},
          %{font | pdf_name: ""},
          %{font | bbox: {0, 0, 1, "bad"}},
          %{font | ascent: 32_768},
          %{font | default_width: 65_536},
          %{font | units_per_em: 65_536},
          %{font | widths: ["bad"]},
          %{font | widths: [-1]},
          %{font | widths: List.duplicate(0, 65_536)},
          %{font | cmap: %{?A => "bad"}},
          %{font | cmap: %{?A => 65_536}},
          %{font | cmap: %{?A => 0}},
          %{font | cmap: Map.new(0..65_535, &{&1, 1})}
        ]

    for face <- invalid_fonts do
      box = %{
        type: :text,
        text: "A",
        x: 0,
        y: 12,
        font_size: 12,
        font: Font.pdf_name(font),
        color: {0, 0, 0},
        font_face: face
      }

      assert {:error,
              {:invalid_pdf_input,
               %{
                 stage: :pdf,
                 reason: :invalid_pdf_input,
                 module: PdfWriter,
                 operation: :write_pdf,
                 message: message
               }}} =
               PdfWriter.render([%{size: {100, 100}, boxes: [box]}])

      assert message =~ "valid pages" or message =~ "valid embedded font data"
    end
  end
end
