defmodule NativeElixirPdfUtilities.Validators.WriterValidatorTest do
  use ExUnit.Case, async: true

  alias NativeElixirPdfUtilities.Validators.WriterValidator
  alias NativeElixirPdfUtilities.HtmlToPdf.{Font, PdfWriter}

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

      assert message =~ "valid pages"
    end
  end
end
