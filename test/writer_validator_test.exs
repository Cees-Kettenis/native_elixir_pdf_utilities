defmodule NativeElixirPdfUtilities.Validators.WriterValidatorTest do
  use ExUnit.Case, async: true

  alias NativeElixirPdfUtilities.Validators.WriterValidator

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
end
