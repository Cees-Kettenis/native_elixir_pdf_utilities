defmodule NativeElixirPdfUtilities.HtmlToPdf.PdfWriterTest do
  use ExUnit.Case

  alias NativeElixirPdfUtilities.HtmlToPdf.PdfWriter
  alias NativeElixirPdfUtilities.HtmlToPdf.Font
  alias NativeElixirPdfUtilities.HtmlToPdf.PageFurniture
  alias NativeElixirPdfUtilities.Text

  test "CSS rectangle edges snap relative to the page top" do
    box = %{
      type: :rect,
      x: 10,
      y: 20,
      width: 30,
      height: 40,
      border_radius: 0,
      fill_color: {0, 0, 0},
      stroke_color: nil,
      stroke_width: 0,
      snap_to_css_pixel_grid: true
    }

    assert {:ok, pdf} = PdfWriter.render([%{size: {100, 100}, boxes: [box]}], [])
    assert pdf =~ "9.75 19.75 30 40.5 re f"
  end

  test "advanced embedded fonts retain default widths beyond supplied metrics" do
    assert {:ok, registry} = Font.load_registry(fonts: [{"Fixture Sans", ttf_font_path!()}])
    assert {:ok, _, font} = Font.resolve("Fixture Sans", 400, :normal, registry)
    font = %{font | widths: [600], default_width: 700, kerning: %{}}

    box = %{
      type: :text,
      text: "A",
      x: 0,
      y: 20,
      font: Font.pdf_name(font),
      font_face: font,
      font_size: 10,
      color: {0, 0, 0},
      snap_to_css_pixel_grid: true
    }

    assert {:ok, pdf} = PdfWriter.render([%{size: {100, 100}, boxes: [box]}], [])
    assert pdf =~ "] TJ"
    assert pdf =~ "1 [#{Float.round(700 * 1000 / font.units_per_em, 4)}]"
    assert {:ok, "A"} = Text.extract(pdf, layout: false)
  end

  test "collapsed table borders paint around grid edges without changing layout bounds" do
    box = %{
      type: :rect,
      role: :table_border,
      x: 10,
      y: 20,
      width: 30,
      height: 40,
      border_radius: 0,
      fill_color: nil,
      stroke_color: {0, 0, 0},
      stroke_width: 2
    }

    assert {:ok, pdf} = PdfWriter.render([%{size: {100, 100}, boxes: [box]}], [])
    assert pdf =~ "0.75 0 0 0.75 0 0 cm"
    assert pdf =~ "13.3333 26.6667 40 53.3333 re S"
    assert NativeElixirPdfUtilities.HtmlToPdf.PageGeometry.box_vertical_bounds(box) == {60, 20}
  end

  test "writer validates optional advanced-font kerning maps before serialization" do
    alias NativeElixirPdfUtilities.Limits
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    assert {:ok, registry} = Font.load_registry(fonts: [{"Fixture Sans", ttf_font_path!()}])
    assert {:ok, _, font} = Font.resolve("Fixture Sans", 400, :normal, registry)

    box = %{
      type: :text,
      text: "A",
      x: 0,
      y: 20,
      font: Font.pdf_name(font),
      font_face: font,
      font_size: 10,
      color: {0, 0, 0}
    }

    for kerning <- [
          nil,
          [],
          "pairs",
          %{1 => 2},
          %{{1, 2} => "bad"},
          %{{1, 2} => Integer.pow(10, 400)},
          %{{1, 2} => -Integer.pow(10, 400)},
          %{{1, 2} => 32_767 * 65_535 + 1},
          %{{1, 2} => -32_768 * 65_535 - 1},
          %{{-1, 2} => 0},
          %{{1, -2} => 0},
          %{{1.0, 2} => 0},
          %{{1, 2.0} => 0},
          %{{length(font.widths), 1} => 0},
          %{{1, length(font.widths)} => 0}
        ] do
      invalid = %{box | font_face: Map.put(font, :kerning, kerning)}
      assert_invalid_pdf_input(PdfWriter.render([%{size: {100, 100}, boxes: [invalid]}], []))
    end

    for prepared <- [Map.delete(font, :kerning), Map.put(font, :kerning, %{})] do
      assert {:ok, _} =
               PdfWriter.render([%{size: {100, 100}, boxes: [%{box | font_face: prepared}]}], [])
    end

    for adjustment <- [-32_768 * 65_535, 32_767 * 65_535] do
      prepared = Map.put(font, :kerning, %{{font.cmap[?A], font.cmap[?V]} => adjustment})
      boundary = %{box | text: "AV", font_face: prepared}
      assert {:ok, pdf} = PdfWriter.render([%{size: {100, 100}, boxes: [boundary]}], [])
      assert pdf =~ "] TJ"
    end

    Limits.install(%{limits | max_font_kerning_pairs: 1})
    invalid = %{box | font_face: Map.put(font, :kerning, %{{1, 1} => 0, {1, 2} => 0})}
    assert_invalid_pdf_input(PdfWriter.render([%{size: {100, 100}, boxes: [invalid]}], []))
  end

  test "writer rejects malformed pixel snapping flags for every drawing kind" do
    boxes = [
      %{type: :text, text: "A", x: 0, y: 20, font: "Courier", font_size: 10, color: {0, 0, 0}},
      %{
        type: :rect,
        x: 0,
        y: 0,
        width: 20,
        height: 10,
        border_radius: 0,
        fill_color: {0, 0, 0},
        stroke_color: nil,
        stroke_width: 0
      },
      %{
        type: :image,
        x: 0,
        y: 0,
        width: 10,
        height: 10,
        image: image_fixture(:png, <<0, 0, 0>>, 1, 1, :device_rgb)
      }
    ]

    for box <- boxes, flag <- [nil, :yes, 0, "true", %{}] do
      invalid = Map.put(box, :snap_to_css_pixel_grid, flag)
      assert_invalid_pdf_input(PdfWriter.render([%{size: {100, 100}, boxes: [invalid]}], []))
    end

    for flag <- [false, true] do
      prepared = Enum.map(boxes, &Map.put(&1, :snap_to_css_pixel_grid, flag))
      assert {:ok, _} = PdfWriter.render([%{size: {100, 100}, boxes: prepared}], [])
    end
  end

  test "black relief borders retain a visible light side" do
    box = %{
      type: :rect,
      x: 0.0,
      y: 0.0,
      width: 30.0,
      height: 20.0,
      border_radius: 0.0,
      fill_color: nil,
      stroke_color: {0, 0, 0},
      stroke_width: 3.0,
      border_widths: %{top: 3.0, right: 3.0, bottom: 3.0, left: 3.0},
      border_styles: %{top: :inset, right: :inset, bottom: :inset, left: :inset}
    }

    assert {:ok, pdf} = PdfWriter.render([%{size: {100, 100}, boxes: [box]}], [])
    assert pdf =~ "0.3294 0.3294 0.3294 RG"
    assert pdf =~ "0 0 0 RG"
  end

  test "CSS font painting preserves shaped advances and text extraction" do
    assert {:ok, registry} = Font.load_registry(fonts: [{"Fixture Sans", ttf_font_path!()}])
    assert {:ok, _families, font} = Font.resolve("Fixture Sans", 400, :normal, registry)

    box = %{
      type: :text,
      text: "AV",
      x: 0.0,
      y: 80.25,
      font: Font.pdf_name(font),
      font_face: font,
      font_size: 10.0,
      color: {0, 0, 0},
      snap_to_css_pixel_grid: true
    }

    assert {:ok, pdf} = PdfWriter.render([%{size: {100, 100}, boxes: [box]}], [])
    assert pdf =~ "/F1 9.9975 Tf"
    assert pdf =~ "0 80.5001 Td ["
    assert pdf =~ "] TJ"
    assert {:ok, "AV"} = Text.extract(pdf, layout: false)
  end

  test "CSS images keep far raster edges inside their allocated rectangle" do
    image = image_fixture(:png, <<0, 0, 0>>, 1, 1, :device_rgb)

    box = %{
      type: :image,
      x: 0.0,
      y: 0.0,
      width: 21.0,
      height: 12.0,
      image: image,
      snap_to_css_pixel_grid: true
    }

    assert {:ok, pdf} = PdfWriter.render([%{size: {100, 100}, boxes: [box]}], [])
    assert pdf =~ "q 20.9999 0 0 11.9999 0 0.0001 cm /Im1 Do Q"
    assert pdf =~ "/Width 1 /Height 1"
  end

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
            font_face: %{type: :built_in, family: "Helvetica", pdf_name: "Helvetica"},
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

  test "render clips fitted and background image paint operations" do
    image = %{
      format: :png,
      data: <<255, 0, 0>>,
      width_px: 1,
      height_px: 1,
      width: 0.75,
      height: 0.75,
      color_space: :device_rgb,
      bits_per_component: 8
    }

    pages = [
      %{
        size: {100.0, 100.0},
        boxes: [
          %{
            type: :image,
            x: -10.0,
            y: 10.0,
            width: 80.0,
            height: 40.0,
            image: image,
            clip: %{x: 10.0, y: 20.0, width: 40.0, height: 20.0}
          }
        ]
      }
    ]

    assert {:ok, pdf} = PdfWriter.render(pages)
    assert pdf =~ "q 10 20 40 20 re W n 80 0 0 40 -10 10 cm /Im1 Do Q"

    invalid = put_in(pages, [Access.at(0), :boxes, Access.at(0), :clip, :width], 0)
    assert {:error, {:invalid_pdf_input, %{stage: :pdf}}} = PdfWriter.render(invalid)
  end

  test "render serializes static control drawing instructions without PDF widgets" do
    pages = [
      %{
        size: {200.0, 100.0},
        boxes: [
          %{
            type: :rect,
            x: 10.0,
            y: 70.0,
            width: 100.0,
            height: 18.0,
            fill_color: {1.0, 1.0, 1.0},
            stroke_color: {0.47, 0.47, 0.47},
            stroke_width: 0.75,
            border_radius: 0.0
          },
          %{
            type: :text,
            text: "Selected value",
            x: 13.0,
            y: 74.0,
            font: "Helvetica",
            font_size: 12.0,
            color: {0, 0, 0}
          }
        ]
      }
    ]

    assert {:ok, pdf} = PdfWriter.render(pages)
    assert pdf =~ "(Selected value) Tj"
    assert pdf =~ "10 70 100 18 re"
    refute pdf =~ "/AcroForm"
    refute pdf =~ "/Widget"
  end

  test "render writes page-furniture boxes produced after pagination" do
    layout_tree = %{
      type: :layout,
      page_size: {200.0, 100.0},
      margin: 20.0,
      boxes: [],
      content_width: 160.0,
      content_height: 60.0
    }

    pages = [
      %{
        size: {200.0, 100.0},
        boxes: [
          %{
            type: :text,
            text: "Body",
            x: 20.0,
            y: 44.0,
            font: "Helvetica",
            font_size: 10.0,
            color: {0, 0, 0}
          }
        ]
      }
    ]

    assert {:ok, decorated} =
             PageFurniture.decorate(pages, layout_tree,
               page_furniture: [
                 header: "<div style=\"font-size: 8pt\">Header</div>",
                 footer: "<div style=\"font-size: 8pt\">Page {{page}}/{{pages}}</div>"
               ]
             )

    assert {:ok, pdf} = PdfWriter.render(decorated)
    assert pdf =~ "(Body) Tj"
    assert {:ok, extracted} = Text.extract(pdf, layout: false)
    assert extracted =~ "Header"
    assert extracted =~ "Page 1/1"
  end

  test "render writes document information metadata" do
    pages = [%{size: {100.0, 100.0}, boxes: []}]
    {:ok, modification_date, _offset} = DateTime.from_iso8601("2026-07-21T12:30:45+08:00")

    assert {:ok, pdf} =
             PdfWriter.render(pages,
               metadata: %{
                 title: "Quarterly (Draft)",
                 author: "Ada \\ Bob",
                 subject: "Résumé",
                 keywords: ["finance", "quarterly"],
                 producer: "Fixture producer",
                 creation_date: ~D[2026-07-20],
                 modification_date: modification_date
               }
             )

    assert pdf =~ "/Title (Quarterly \\(Draft\\))"
    assert pdf =~ "/Author (Ada \\\\ Bob)"
    assert pdf =~ "/Subject <FEFF005200E900730075006D00E9>"
    assert pdf =~ "/Keywords (finance, quarterly)"
    assert pdf =~ "/Producer (Fixture producer)"
    assert pdf =~ "/CreationDate (D:20260720)"
    assert pdf =~ "/ModDate (D:20260721043045+00'00')"
    assert pdf =~ ~r/trailer\n<< \/Size \d+ \/Root 1 0 R \/Info \d+ 0 R >>/
  end

  test "render accepts keyword metadata and ISO date strings" do
    pages = [%{size: {100.0, 100.0}, boxes: []}]

    assert {:ok, pdf} =
             PdfWriter.render(pages,
               metadata: [
                 title: "ISO dates",
                 creation_date: "2026-07-21T10:20:30Z",
                 modification_date: "2026-07-22"
               ]
             )

    assert pdf =~ "/CreationDate (D:20260721102030+00'00')"
    assert pdf =~ "/ModDate (D:20260722)"

    assert {:ok, naive_pdf} =
             PdfWriter.render(pages, metadata: [creation_date: "2026-07-21T10:20:30"])

    assert naive_pdf =~ "/CreationDate (D:20260721102030)"

    assert {:ok, early_pdf} = PdfWriter.render(pages, metadata: [creation_date: ~D[0001-01-02]])
    assert early_pdf =~ "/CreationDate (D:00010102)"
  end

  test "render rejects malformed metadata with diagnostics" do
    pages = [%{size: {100.0, 100.0}, boxes: []}]

    for metadata <- [
          [unknown: "value"],
          [title: 123],
          [title: <<255>>],
          [keywords: <<255>>],
          [keywords: ["valid", 123]],
          [creation_date: "not-a-date"],
          [creation_date: :today],
          [:not_a_keyword]
        ] do
      assert {:error,
              {:invalid_pdf_input,
               %{
                 stage: :pdf,
                 reason: :invalid_pdf_input,
                 message: "PDF metadata must use supported fields and value types"
               }}} = PdfWriter.render(pages, metadata: metadata)
    end

    assert_invalid_pdf_input(PdfWriter.render(pages, [:not_options]))
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
    assert pdf =~ "q 0.9 0.9 0.9 rg 10 20 40 30 re f Q"
    assert pdf =~ "q 0.75 0 0 0.75 0 0 cm 1 0 0 RG 2.6667 w"
    assert pdf =~ "14.6667 28 50.6667 37.3333 re S Q"
    assert pdf =~ "BT /F1 12 Tf 0 0 0 rg 15 35 Td (Boxed) Tj ET"
  end

  test "render writes deduplicated fill and stroke opacity graphics states" do
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
            fill_color: {1, 0, 0, 0.5},
            stroke_color: {0, 0, 1, 0.25},
            stroke_width: 2.0,
            border_radius: 0.0
          },
          %{
            type: :text,
            text: "Hidden",
            x: 15.0,
            y: 35.0,
            font: "Helvetica",
            font_size: 12.0,
            color: {0, 0, 0, 0.0}
          },
          %{
            type: :text,
            text: "Faded",
            x: 15.0,
            y: 50.0,
            font: "Helvetica",
            font_size: 12.0,
            color: {0, 0, 0, 0.5}
          }
        ]
      }
    ]

    assert {:ok, pdf} = PdfWriter.render(pages, [])
    assert pdf =~ "/ExtGState <<"
    assert pdf =~ "<< /Type /ExtGState /ca 0 >>"
    assert pdf =~ "<< /Type /ExtGState /ca 0.5 >>"
    assert pdf =~ "<< /Type /ExtGState /CA 0.25 >>"
    assert length(Regex.scan(~r/<< \/Type \/ExtGState \/ca 0\.5 >>/, pdf)) == 1
    assert pdf =~ ~r/q \/GS\d+ gs 1 0 0 rg 10 20 40 30 re f Q/
    assert pdf =~ ~r/q 0\.75 0 0 0\.75 0 0 cm \/GS\d+ gs 0 0 1 RG 2\.6667 w .* re S Q/
    assert pdf =~ ~r/q \/GS\d+ gs BT \/F1 12 Tf 0 0 0 rg 15 35 Td \(Hidden\) Tj ET Q/
    assert pdf =~ ~r/q \/GS\d+ gs BT \/F1 12 Tf 0 0 0 rg 15 50 Td \(Faded\) Tj ET Q/
  end

  test "render preserves opacity while shading side-specific border colors" do
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
            stroke_color: {0.4, 0.6, 0.8, 0.5},
            stroke_width: 4.0,
            border_widths: %{top: 4.0, right: 4.0, bottom: 4.0, left: 4.0},
            border_colors: %{
              top: {0.4, 0.6, 0.8, 0.5},
              right: {0.4, 0.6, 0.8, 0.5},
              bottom: {0.4, 0.6, 0.8, 0.5},
              left: {0.4, 0.6, 0.8, 0.5}
            },
            border_styles: %{top: :groove, right: :ridge, bottom: :inset, left: :outset},
            border_radius: 0.0
          }
        ]
      }
    ]

    assert {:ok, pdf} = PdfWriter.render(pages, [])
    assert pdf =~ "<< /Type /ExtGState /CA 0.5 >>"
    assert pdf =~ ~r/q 0\.75 0 0 0\.75 0 0 cm \/GS\d+ gs 0\.2353 0\.3529 0\.4706 RG/
    assert pdf =~ ~r/q 0\.75 0 0 0\.75 0 0 cm \/GS\d+ gs 0\.502 0\.749 1 RG/
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
    assert pdf =~ "q 0.75 0 0 0.75 0 0 cm 0 0 1 RG 1.3333 w"
    assert pdf =~ "14 80.6667 52 25.3333 re S Q"
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
            border_colors: %{
              top: {0.1, 0.2, 0.3},
              right: {0, 0, 0},
              bottom: {0.8, 0.9, 1.0},
              left: {0, 0, 0}
            },
            border_radius: 0.0
          }
        ]
      }
    ]

    assert {:ok, pdf} = PdfWriter.render(pages, [])
    assert pdf =~ "q 0.75 0 0 0.75 0 0 cm 0.1 0.2 0.3 RG 1.3333 w"
    assert pdf =~ "13.3333 66 m 66.6667 66 l S"
    refute pdf =~ "66 26.6667 m 66 66.6667 l S"
    assert pdf =~ "q 0.75 0 0 0.75 0 0 cm 0.8 0.9 1 RG 1.3333 w"
    assert pdf =~ "13.3333 27.7083 m 66.6667 27.7083 l S"
    assert pdf =~ "14 26.6667 m 14 66.6667 l S"

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
    assert stroke_only_pdf =~ "66 26.6667 m 66 66.6667 l S"
    refute stroke_only_pdf =~ "10 20 40 30 re f"
  end

  test "render writes every CSS border style and skips transparent sides" do
    edges = fn value -> %{top: value, right: value, bottom: value, left: value} end

    border_box = fn x, y, border_style, stroke_width ->
      %{
        type: :rect,
        x: x,
        y: y,
        width: 10.0,
        height: 10.0,
        fill_color: nil,
        stroke_color: {0.2, 0.4, 0.6},
        stroke_width: stroke_width,
        border_widths: edges.(stroke_width),
        border_colors: edges.({0.2, 0.4, 0.6}),
        border_styles: edges.(border_style),
        border_radius: 0.0
      }
    end

    transparent_box =
      border_box.(70.0, 30.0, :solid, 1.0)
      |> Map.put(:fill_color, {0.9, 0.9, 0.9})
      |> Map.put(:border_colors, %{top: nil, right: {1, 0, 0}, bottom: nil, left: nil})

    invisible_boxes =
      [:none, :hidden]
      |> Enum.with_index()
      |> Enum.map(fn {border_style, index} ->
        border_box.(10.0 + index * 20, 50.0, border_style, 2.0)
        |> Map.put(:fill_color, {0.8, 0.8, 0.8})
      end)

    mixed_hidden_box =
      border_box.(10.0, 70.0, :solid, 1.0)
      |> Map.put(:border_styles, %{top: :hidden, right: :solid, bottom: :solid, left: :solid})

    fallback_color_box =
      border_box.(30.0, 70.0, :double, 3.0)
      |> Map.delete(:border_colors)

    fallback_style_box =
      border_box.(50.0, 70.0, :solid, 1.0)
      |> Map.delete(:border_styles)

    rounded_pattern_boxes =
      [:dotted, :dashed]
      |> Enum.with_index()
      |> Enum.map(fn {border_style, index} ->
        border_box.(70.0 + index * 15, 70.0, border_style, 2.0)
        |> Map.put(:border_radius, 2.0)
      end)

    narrow_dash_box =
      border_box.(70.0, 50.0, :dashed, 2.0)
      |> Map.put(:width, 4.1)

    pages = [
      %{
        size: {100.0, 100.0},
        boxes:
          [
            border_box.(10.0, 10.0, :dotted, 2.0),
            border_box.(30.0, 10.0, :dashed, 2.0),
            border_box.(50.0, 10.0, :double, 3.0),
            border_box.(10.0, 30.0, :groove, 3.0),
            border_box.(30.0, 30.0, :ridge, 3.0),
            border_box.(50.0, 30.0, :inset, 3.0),
            border_box.(70.0, 10.0, :outset, 3.0),
            transparent_box,
            narrow_dash_box
          ] ++
            invisible_boxes ++
            [mixed_hidden_box, fallback_color_box, fallback_style_box] ++ rounded_pattern_boxes
      }
    ]

    assert {:ok, pdf} = PdfWriter.render(pages, [])

    assert pdf =~ ~r/\[[^\]]+\] 0 d 1 J/
    assert pdf =~ ~r/\[[^\]]+\] 0 d 0 J/
    assert pdf =~ "0.2 0.4 0.6 RG"
    assert pdf =~ "0.0902 0.1804 0.2706 RG"
    assert pdf =~ "0.3098 0.6196 0.9294 RG"
    assert pdf =~ "1 0 0 RG 1.3333 w"
    assert length(Regex.scan(~r/1 0 0 RG 1\.3333 w/, pdf)) == 1

    assert pdf =~ "10 50 10 10 re f"
    assert pdf =~ "30 50 10 10 re f"

    assert pdf =~ "0.2 0.4 0.6 RG 1.3333 w"
    assert pdf =~ " re S"
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
    assert pdf =~ "0 0 1 RG 1.3333 w"
    assert pdf =~ "0.75 0 0 0.75 0 0 cm"
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
            image: image_fixture(:jpeg, jpeg_fixture(1, 1, 1), 1, 1, :device_gray)
          },
          %{
            type: :image,
            x: 20.0,
            y: 30.0,
            width: 15.0,
            height: 10.0,
            image: image_fixture(:jpeg, jpeg_fixture(2, 1, 4), 2, 1, :device_cmyk)
          }
        ]
      }
    ]

    assert {:ok, pdf} = PdfWriter.render(pages, [])
    assert pdf =~ "/ColorSpace /DeviceGray"
    assert pdf =~ "/ColorSpace [/DeviceN [/JpegC /JpegM /JpegY /JpegK] /DeviceRGB"
    assert pdf =~ "/FunctionType 0 /Domain [0 1 0 1 0 1 0 1] /Range [0 1 0 1 0 1]"
    assert pdf =~ "/Size [2 2 2 2] /BitsPerSample 8 /Length 48"
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
    assert {:ok, "Café"} = Text.extract(pdf, layout: false)
    refute pdf =~ "(Café) Tj"
  end

  test "embedded font CIDs preserve Unicode characters that share a glyph" do
    assert {:ok, registry} = Font.load_registry(fonts: [{"Fixture Sans", ttf_font_path!()}])
    assert {:ok, _families, font} = Font.resolve("Fixture Sans", 400, :normal, registry)

    shared_glyph = Map.fetch!(font.cmap, ?A)

    font = %{
      font
      | cmap: font.cmap |> Map.put(?B, shared_glyph) |> Map.put(0x1F600, shared_glyph)
    }

    pages = [
      %{
        size: {100.0, 100.0},
        boxes: [
          %{
            type: :text,
            text: "AB😀",
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
    assert pdf =~ "/CIDToGIDMap "
    refute pdf =~ "/CIDToGIDMap /Identity"
    assert pdf =~ "<0001> <0041>"
    assert pdf =~ "<0002> <0042>"
    assert pdf =~ "<0003> <D83DDE00>"
    assert {:ok, "AB😀"} = Text.extract(pdf, layout: false)
  end

  test "ligature glyphs retain the original spelling in extracted PDF text" do
    assert {:ok, registry} = Font.load_registry([])
    assert {:ok, _, font} = Font.resolve("DejaVu Sans", 700, :normal, registry)

    box = %{
      type: :text,
      text: "Office certification flower waffle",
      x: 10.0,
      y: 80.0,
      font: Font.pdf_name(font),
      font_face: font,
      font_size: 9.0,
      color: {0, 0, 0}
    }

    assert {:ok, pdf} = PdfWriter.render([%{size: {200, 100}, boxes: [box]}], [])
    assert pdf =~ ~r/<[0-9A-F]{4}> <006600660069>/
    assert {:ok, "Office certification flower waffle"} = Text.extract(pdf, layout: false)
  end

  test "embedded font ToUnicode mappings use sections of at most 100 entries" do
    assert {:ok, registry} = Font.load_registry(fonts: [{"Fixture Sans", ttf_font_path!()}])
    assert {:ok, _families, font} = Font.resolve("Fixture Sans", 400, :normal, registry)

    codepoints =
      font.cmap
      |> Map.keys()
      |> Enum.filter(&(&1 in 0x20..0xD7FF or &1 in 0xE000..0xFFFF))
      |> Enum.sort()
      |> Enum.take(120)

    assert length(codepoints) == 120
    text = List.to_string(codepoints)

    pages = [
      %{
        size: {100.0, 100.0},
        boxes: [
          %{
            type: :text,
            text: text,
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

    assert Regex.scan(~r/(\d+) beginbfchar/, pdf, capture: :all_but_first) == [
             ["100"],
             ["20"]
           ]

    assert {:ok, ^text} = Text.extract(pdf, layout: false)
  end

  test "render rejects text that its selected font cannot encode" do
    built_in_page = %{
      size: {100.0, 100.0},
      boxes: [
        %{
          type: :text,
          text: "café",
          x: 10.0,
          y: 80.0,
          font: "Helvetica",
          font_size: 12.0,
          color: {0, 0, 0}
        }
      ]
    }

    assert_invalid_pdf_input(PdfWriter.render([built_in_page], []))

    assert {:ok, registry} = Font.load_registry([])
    assert {:ok, _families, font} = Font.resolve("DejaVu Sans", 400, :normal, registry)

    embedded_page =
      put_in(
        built_in_page.boxes,
        [
          %{
            type: :text,
            text: "漢",
            x: 10.0,
            y: 80.0,
            font: Font.pdf_name(font),
            font_face: font,
            font_size: 12.0,
            color: {0, 0, 0}
          }
        ]
      )

    assert_invalid_pdf_input(PdfWriter.render([embedded_page], []))
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

    assert_invalid_pdf_input(PdfWriter.render(pages, []))
  end

  test "render rejects invalid page data" do
    assert_invalid_pdf_input(PdfWriter.render([], []))
    assert_invalid_pdf_input(PdfWriter.render(:not_pages, []))
    assert_invalid_pdf_input(PdfWriter.render([%{size: {0, 100}, boxes: []}], []))

    invalid_boxes = [
      %{type: :text, text: "Bad", x: 1, y: 1, font: "Helvetica", font_size: -1, color: {0, 0, 0}},
      %{type: :text, text: "Bad", x: 1, y: 1, font: "Helvetica", font_size: 12, color: :red},
      %{
        type: :text,
        text: "Bad",
        x: 1,
        y: 1,
        font: "Helvetica",
        font_size: 12,
        color: {0, 0, 0, 2}
      },
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
        fill_color: nil,
        stroke_color: {0, 0, 0},
        stroke_width: 1,
        border_widths: %{top: 1, right: 1, bottom: 1, left: 1},
        border_colors: :bad,
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
        border_widths: %{top: 1, right: 1, bottom: 1, left: 1},
        border_styles: %{top: :solid, right: :sparkly, bottom: :solid, left: :solid},
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
        border_widths: %{top: 1, right: 1, bottom: 1, left: 1},
        border_styles: :bad,
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
      assert_invalid_pdf_input(PdfWriter.render([%{size: {100.0, 100.0}, boxes: [box]}], []))
    end)
  end

  test "render rejects RGB channels outside the normalized color range" do
    text_box = %{
      type: :text,
      text: "Bad color",
      x: 1,
      y: 1,
      font: "Helvetica",
      font_size: 12,
      color: {0, 0, 0}
    }

    invalid_colors = [
      {-0.01, 0, 0},
      {0, 1.01, 0},
      {0, 0, -1},
      {-0.01, 0, 0, 0.5},
      {0, 1.01, 0, 0.5},
      {0, 0, -1, 0.5}
    ]

    Enum.each(invalid_colors, fn color ->
      pages = [%{size: {100.0, 100.0}, boxes: [%{text_box | color: color}]}]
      assert_invalid_pdf_input(PdfWriter.render(pages, []))
    end)
  end

  defp assert_invalid_pdf_input(result) do
    assert {:error,
            {:invalid_pdf_input,
             %{
               stage: :pdf,
               reason: :invalid_pdf_input,
               operation: :write_pdf,
               module: NativeElixirPdfUtilities.HtmlToPdf.PdfWriter
             }}} = result
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
      Path.expand("../../priv/fonts/dejavu/DejaVuSans.ttf", __DIR__),
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

  defp jpeg_fixture(width, height, components \\ 3),
    do: NativeElixirPdfUtilities.TestSupport.JpegFixture.baseline(width, height, components)
end
