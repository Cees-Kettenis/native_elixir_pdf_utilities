defmodule NativeElixirPdfUtilities.HtmlToPdf.PdfWriterTest do
  use ExUnit.Case

  alias NativeElixirPdfUtilities.HtmlToPdf.PdfWriter
  alias NativeElixirPdfUtilities.HtmlToPdf.Font

  test "render writes a valid PDF for a text page" do
    pages = [
      %{
        size: {595.28, 841.89},
        boxes: [
          %{
            type: :text,
            text: "Hello",
            x: 56.69,
            y: 773.2,
            font: "Helvetica",
            font_size: 12.0,
            color: {0, 0, 0}
          }
        ]
      }
    ]

    assert {:ok, pdf} = PdfWriter.render(pages)
    assert String.starts_with?(pdf, "%PDF-1.4")
    assert pdf =~ "/MediaBox [0 0 595.28 841.89]"
    assert pdf =~ "BT /F1 12 Tf 0 0 0 rg 56.69 773.2 Td (Hello) Tj ET"
    assert pdf =~ "startxref"
  end

  test "render escapes PDF text literals" do
    pages = [
      %{
        size: {100.0, 100.0},
        boxes: [
          %{
            type: :text,
            text: "A (B) \\ C",
            x: 10.0,
            y: 20.0,
            font: "Helvetica",
            font_size: 12.0,
            color: {0, 0, 0}
          }
        ]
      }
    ]

    assert {:ok, pdf} = PdfWriter.render(pages, [])
    assert pdf =~ "(A \\(B\\) \\\\ C) Tj"
  end

  test "render writes character spacing for spaced text" do
    pages = [
      %{
        size: {100.0, 100.0},
        boxes: [
          %{
            type: :text,
            text: "DATE",
            x: 10.0,
            y: 20.0,
            font: "Helvetica",
            font_size: 8.0,
            letter_spacing: 0.64,
            color: {0, 0, 0}
          },
          %{
            type: :text,
            text: "Value",
            x: 10.0,
            y: 10.0,
            font: "Helvetica",
            font_size: 8.0,
            color: {0, 0, 0}
          }
        ]
      }
    ]

    assert {:ok, pdf} = PdfWriter.render(pages, [])
    assert pdf =~ "0.64 Tc (DATE) Tj 0 Tc"
    assert pdf =~ "(Value) Tj"
  end

  test "render writes font resources for bold italic and colored runs" do
    pages = [
      %{
        size: {100.0, 100.0},
        boxes: [
          %{
            type: :text,
            text: "Bold",
            x: 10.0,
            y: 80.0,
            font: "Helvetica-Bold",
            font_size: 12.0,
            color: {1, 0, 0}
          },
          %{
            type: :text,
            text: "Italic",
            x: 40.0,
            y: 80.0,
            font: "Helvetica-Oblique",
            font_size: 12.0,
            color: {0, 0, 1}
          }
        ]
      }
    ]

    assert {:ok, pdf} = PdfWriter.render(pages, [])
    assert pdf =~ "/BaseFont /Helvetica-Bold"
    assert pdf =~ "/BaseFont /Helvetica-Oblique"
    assert pdf =~ "BT /F1 12 Tf 1 0 0 rg 10 80 Td (Bold) Tj ET"
    assert pdf =~ "BT /F2 12 Tf 0 0 1 rg 40 80 Td (Italic) Tj ET"
  end

  test "render writes filled and stroked rectangle boxes" do
    pages = [
      %{
        size: {100.0, 100.0},
        boxes: [
          %{
            type: :rect,
            x: 10.0,
            y: 20.0,
            width: 40.0,
            height: 30.0,
            fill_color: {0.9, 0.9, 0.9},
            stroke_color: {1, 0, 0},
            stroke_width: 2.0,
            border_radius: 0.0
          },
          %{
            type: :text,
            text: "Boxed",
            x: 15.0,
            y: 35.0,
            font: "Helvetica",
            font_size: 12.0,
            color: {0, 0, 0}
          }
        ]
      }
    ]

    assert {:ok, pdf} = PdfWriter.render(pages, [])
    assert pdf =~ "q 0.9 0.9 0.9 rg 1 0 0 RG 2 w 10 20 40 30 re B Q"
    assert pdf =~ "BT /F1 12 Tf 0 0 0 rg 15 35 Td (Boxed) Tj ET"
  end

  test "render writes fill-only and stroke-only rectangle boxes" do
    pages = [
      %{
        size: {100.0, 100.0},
        boxes: [
          %{
            type: :rect,
            x: 10.0,
            y: 20.0,
            width: 40.0,
            height: 30.0,
            fill_color: {0.9, 0.9, 0.9},
            stroke_color: nil,
            stroke_width: 0.0,
            border_radius: 0.0
          },
          %{
            type: :rect,
            x: 10.0,
            y: 60.0,
            width: 40.0,
            height: 20.0,
            fill_color: nil,
            stroke_color: {0, 0, 1},
            stroke_width: 1.0,
            border_radius: 0.0
          }
        ]
      }
    ]

    assert {:ok, pdf} = PdfWriter.render(pages, [])
    assert pdf =~ "q 0.9 0.9 0.9 rg 10 20 40 30 re f Q"
    assert pdf =~ "q 0 0 1 RG 1 w 10 60 40 20 re S Q"
  end

  test "render writes side-specific rectangle borders" do
    pages = [
      %{
        size: {100.0, 100.0},
        boxes: [
          %{
            type: :rect,
            x: 10.0,
            y: 20.0,
            width: 40.0,
            height: 30.0,
            fill_color: {0.9, 0.9, 0.9},
            stroke_color: {0, 0, 0},
            stroke_width: 1.0,
            border_widths: %{top: 1.0, right: 0.0, bottom: 1.0, left: 1.0},
            border_radius: 0.0
          }
        ]
      }
    ]

    assert {:ok, pdf} = PdfWriter.render(pages, [])
    assert pdf =~ "10 50 m 50 50 l S"
    refute pdf =~ "50 20 m 50 50 l S"
    assert pdf =~ "10 20 m 50 20 l S"
    assert pdf =~ "10 20 m 10 50 l S"

    stroke_only_pages = [
      %{
        size: {100.0, 100.0},
        boxes: [
          %{
            type: :rect,
            x: 10.0,
            y: 20.0,
            width: 40.0,
            height: 30.0,
            fill_color: nil,
            stroke_color: {0, 0, 0},
            stroke_width: 1.0,
            border_widths: %{top: 0.0, right: 1.0, bottom: 0.0, left: 0.0},
            border_radius: 0.0
          }
        ]
      }
    ]

    assert {:ok, stroke_only_pdf} = PdfWriter.render(stroke_only_pages, [])
    assert stroke_only_pdf =~ "50 20 m 50 50 l S"
    refute stroke_only_pdf =~ "10 20 40 30 re f"
  end

  test "render writes rounded rectangle paths when radius is set" do
    pages = [
      %{
        size: {100.0, 100.0},
        boxes: [
          %{
            type: :rect,
            x: 10.0,
            y: 20.0,
            width: 40.0,
            height: 30.0,
            fill_color: nil,
            stroke_color: {0, 0, 1},
            stroke_width: 1.0,
            border_radius: 4.0
          }
        ]
      }
    ]

    assert {:ok, pdf} = PdfWriter.render(pages, [])
    assert pdf =~ "0 0 1 RG 1 w"
    assert pdf =~ "14 20 m"
    assert pdf =~ "c"
    assert pdf =~ " h S Q"
  end

  test "render writes URI link annotations for linked text boxes" do
    pages = [
      %{
        size: {100.0, 100.0},
        boxes: [
          %{
            type: :text,
            text: "Docs",
            x: 10.0,
            y: 20.0,
            width: 60.0,
            annotation_width: 28.8,
            line_height: 14.4,
            font: "Helvetica",
            font_size: 12.0,
            color: {0, 0, 1},
            link_url: "https://example.com"
          }
        ]
      }
    ]

    assert {:ok, pdf} = PdfWriter.render(pages, [])
    assert pdf =~ "/Annots [6 0 R]"
    assert pdf =~ "/Subtype /Link"
    assert pdf =~ "/Rect [10 20 38.8 34.4]"
    assert pdf =~ "/A << /S /URI /URI (https://example.com) >>"
  end

  test "render writes PNG and JPEG image XObjects" do
    pages = [
      %{
        size: {100.0, 100.0},
        boxes: [
          %{
            type: :image,
            x: 5.0,
            y: 6.0,
            width: 10.0,
            height: 20.0,
            image: image_fixture(:png, <<255, 0, 0>>, 1, 1, :device_rgb)
          },
          %{
            type: :image,
            x: 20.0,
            y: 30.0,
            width: 15.0,
            height: 10.0,
            image: image_fixture(:jpeg, jpeg_fixture(2, 1), 2, 1, :device_rgb)
          }
        ]
      }
    ]

    assert {:ok, pdf} = PdfWriter.render(pages, [])
    assert pdf =~ "/XObject << /Im1 3 0 R /Im2 4 0 R >>"
    assert pdf =~ "/Subtype /Image"
    assert pdf =~ "/Filter /FlateDecode"
    assert pdf =~ "/Filter /DCTDecode"
    assert pdf =~ "q 10 0 0 20 5 6 cm /Im1 Do Q"
    assert pdf =~ "q 15 0 0 10 20 30 cm /Im2 Do Q"
  end

  test "render writes PNG alpha as a soft mask XObject" do
    pages = [
      %{
        size: {100.0, 100.0},
        boxes: [
          %{
            type: :image,
            x: 5.0,
            y: 6.0,
            width: 10.0,
            height: 20.0,
            image:
              image_fixture(:png, <<0, 0, 0, 255, 0, 0>>, 2, 1, :device_rgb)
              |> Map.put(:alpha_data, <<0, 255>>)
          }
        ]
      }
    ]

    assert {:ok, pdf} = PdfWriter.render(pages, [])
    assert pdf =~ "/XObject << /Im1 3 0 R >>"
    assert pdf =~ "/SMask 4 0 R"
    assert pdf =~ "/ColorSpace /DeviceGray"
    assert pdf =~ "q 10 0 0 20 5 6 cm /Im1 Do Q"
  end

  test "render writes gray and CMYK image color spaces" do
    pages = [
      %{
        size: {100.0, 100.0},
        boxes: [
          %{
            type: :image,
            x: 5.0,
            y: 6.0,
            width: 10.0,
            height: 20.0,
            image: image_fixture(:jpeg, jpeg_fixture(1, 1), 1, 1, :device_gray)
          },
          %{
            type: :image,
            x: 20.0,
            y: 30.0,
            width: 15.0,
            height: 10.0,
            image: image_fixture(:jpeg, jpeg_fixture(2, 1), 2, 1, :device_cmyk)
          }
        ]
      }
    ]

    assert {:ok, pdf} = PdfWriter.render(pages, [])
    assert pdf =~ "/ColorSpace /DeviceGray"
    assert pdf =~ "/ColorSpace /DeviceCMYK"
  end

  test "render embeds TTF fonts with Type0 Unicode text output" do
    assert {:ok, registry} = Font.load_registry(fonts: [{"Fixture Sans", ttf_font_path!()}])
    assert {:ok, _families, font} = Font.resolve("Fixture Sans", 400, :normal, registry)

    pages = [
      %{
        size: {100.0, 100.0},
        boxes: [
          %{
            type: :text,
            text: "Café",
            x: 10.0,
            y: 80.0,
            font: Font.pdf_name(font),
            font_face: font,
            font_size: 12.0,
            color: {0, 0, 0}
          }
        ]
      }
    ]

    assert {:ok, pdf} = PdfWriter.render(pages, [])
    assert pdf =~ "/Subtype /Type0"
    assert pdf =~ "/Subtype /CIDFontType2"
    assert pdf =~ "/FontFile2"
    assert pdf =~ "/ToUnicode"
    assert pdf =~ "/Encoding /Identity-H"
    assert pdf =~ "BT /F1 12 Tf 0 0 0 rg 10 80 Td <"
    refute pdf =~ "(Café) Tj"
  end

  test "render rejects unsupported link annotations" do
    pages = [
      %{
        size: {100.0, 100.0},
        boxes: [
          %{
            type: :text,
            text: "Bad",
            x: 10.0,
            y: 20.0,
            width: 60.0,
            annotation_width: 21.6,
            font: "Helvetica",
            font_size: 12.0,
            color: {0, 0, 1},
            link_url: "javascript:alert(1)"
          }
        ]
      }
    ]

    assert PdfWriter.render(pages, []) == {:error, :invalid_pdf_input}
  end

  test "render rejects invalid page data" do
    assert PdfWriter.render([], []) == {:error, :invalid_pdf_input}
    assert PdfWriter.render(:not_pages, []) == {:error, :invalid_pdf_input}
    assert PdfWriter.render([%{size: {0, 100}, boxes: []}], []) == {:error, :invalid_pdf_input}

    invalid_boxes = [
      %{type: :text, text: "Bad", x: 1, y: 1, font: "Helvetica", font_size: -1, color: {0, 0, 0}},
      %{type: :text, text: "Bad", x: 1, y: 1, font: "Helvetica", font_size: 12, color: :red},
      %{
        type: :text,
        text: "Bad",
        x: 1,
        y: 1,
        font: "Helvetica",
        font_face: %{type: :built_in, pdf_name: "BadFont"},
        font_size: 12,
        color: {0, 0, 0}
      },
      %{
        type: :text,
        text: "Bad",
        x: 1,
        y: 1,
        font: "Helvetica",
        font_face: %{type: :unknown},
        font_size: 12,
        color: {0, 0, 0}
      },
      %{
        type: :text,
        text: "Bad",
        x: 1,
        y: 1,
        width: 10,
        annotation_width: 10,
        font: "Helvetica",
        font_size: 12,
        color: {0, 0, 0},
        link_url: 123
      },
      %{
        type: :rect,
        x: 1,
        y: 1,
        width: 10,
        height: 10,
        fill_color: nil,
        stroke_color: nil,
        stroke_width: 0,
        border_radius: 0
      },
      %{
        type: :rect,
        x: 1,
        y: 1,
        width: 10,
        height: 10,
        fill_color: nil,
        stroke_color: {0, 0, 0},
        stroke_width: 1,
        border_widths: :bad,
        border_radius: 0
      },
      %{
        type: :rect,
        x: 1,
        y: 1,
        width: 10,
        height: 10,
        fill_color: :red,
        stroke_color: nil,
        stroke_width: 0,
        border_radius: 0
      },
      %{
        type: :image,
        x: 1,
        y: 1,
        width: 10,
        height: 10,
        image: image_fixture(:gif, "bad", 1, 1, :device_rgb)
      },
      %{
        type: :image,
        x: 1,
        y: 1,
        width: 10,
        height: 10,
        image:
          image_fixture(:png, <<0, 0, 0>>, 1, 1, :device_rgb)
          |> Map.put(:alpha_data, <<0, 255>>)
      },
      %{
        type: :image,
        x: 1,
        y: 1,
        width: 10,
        height: 10,
        image:
          image_fixture(:png, <<0, 0, 0>>, 1, 1, :device_rgb)
          |> Map.put(:alpha_data, :bad)
      },
      %{type: :unknown}
    ]

    Enum.each(invalid_boxes, fn box ->
      assert PdfWriter.render([%{size: {100.0, 100.0}, boxes: [box]}], []) ==
               {:error, :invalid_pdf_input}
    end)
  end

  defp image_fixture(format, data, width, height, color_space) do
    %{
      format: format,
      data: data,
      width_px: width,
      height_px: height,
      width: width * 0.75,
      height: height * 0.75,
      color_space: color_space,
      bits_per_component: 8
    }
  end

  defp ttf_font_path! do
    [
      "/usr/share/fonts/truetype/dejavu/DejaVuSans.ttf",
      "/usr/share/fonts/truetype/liberation/LiberationSans-Regular.ttf",
      "/usr/share/fonts/truetype/noto/NotoSans-Regular.ttf"
    ]
    |> Enum.find(&File.exists?/1)
    |> case do
      nil -> flunk("No local TTF font fixture found")
      path -> path
    end
  end

  defp jpeg_fixture(width, height) do
    <<255, 216, 255, 224, 0, 16, "JFIF", 0, 1, 1, 0, 0, 1, 0, 1, 0, 0, 255, 192, 0, 17, 8,
      height::16, width::16, 3, 1, 17, 0, 2, 17, 0, 3, 17, 0, 255, 217>>
  end
end
