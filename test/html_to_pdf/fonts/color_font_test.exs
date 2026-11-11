defmodule NativeElixirPdfUtilities.HtmlToPdf.ColorFontTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.{HtmlToPdf, Text, Limits}
  alias NativeElixirPdfUtilities.HtmlToPdf.{Font, PdfWriter}

  alias NativeElixirPdfUtilities.Validators.{
    BitmapFontValidator,
    ColorFontValidator,
    WriterValidator
  }

  alias NativeElixirPdfUtilities.TestSupport.Paths

  setup do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    :ok
  end

  test "independent sbix and CBDT strikes select density, keep alpha and extract real selected text" do
    for file <- ["selected-sbix.ttf", "selected-cbdt.ttf", "selected-bitmap-only.ttf"] do
      data = Paths.fixture("fonts/" <> file) |> File.read!()
      assert {:ok, [font]} = Font.load_configured_faces([%{family: "Color", data: data}])
      assert {:ok, strikes} = BitmapFontValidator.prepare(font)
      assert Enum.map(strikes, & &1.ppem_y) == [20, 40]
      assert BitmapFontValidator.select(strikes, font.cmap[?H], 21).ppem_y == 40
      assert BitmapFontValidator.select(strikes, font.cmap[?H], 60).ppem_y == 40
      assert BitmapFontValidator.select(strikes, font.cmap[32], 20) == nil

      box = %{
        type: :text,
        text: "Hx0",
        x: 10,
        y: 10,
        font: Font.pdf_name(font),
        font_face: font,
        font_size: 15,
        color: {0, 0, 0}
      }

      page = %{size: {150, 100}, boxes: [box]}
      assert {:ok, context} = WriterValidator.prepare([page], [])
      assert {:ok, expanded} = ColorFontValidator.prepare(context, [])
      images = Enum.filter(hd(expanded.pages).boxes, &(&1.type == :image))
      assert length(images) == 3
      assert Enum.all?(images, &is_binary(&1.image.alpha_data))
      assert hd(images).width == 7.5
      assert {:ok, pdf} = PdfWriter.render([page], compress_streams: false)
      assert pdf =~ "/SMask"
      assert pdf =~ "/ActualText"
      assert {:ok, "Hx0"} = Text.extract(pdf, layout: false)

      assert {:ok, html} =
               HtmlToPdf.render("<p style='font-family:Color'>Hx0</p>",
                 fonts: [%{family: "Color", data: data}],
                 system_font_discovery: false
               )

      assert {:ok, "Hx0"} = Text.extract(html, layout: false)
    end
  end

  test "CBDT device advances drive CSS measurement and reject substituted public metadata" do
    data = Paths.fixture("fonts/selected-cbdt.ttf") |> File.read!()
    assert {:ok, [font]} = Font.load_configured_faces([%{family: "Bitmap", data: data}])

    assert {:ok, selected} =
             NativeElixirPdfUtilities.Validators.FontRenderingValidator.prepare(font, %{
               font_size: 15
             })

    assert elem(selected.bitmap_widths, font.cmap[?H]) == 500
    assert Font.text_width("Hx0", selected, 20) == 30
    assert Font.text_width("Hx0", font, 20) == 30
    assert {:ok, ^selected} = BitmapFontValidator.prepare_metrics(selected, 20)

    assert {:ok, context} =
             NativeElixirPdfUtilities.Validators.WriterValidator.prepare(
               [
                 %{
                   size: {120, 80},
                   boxes: [
                     %{
                       type: :text,
                       text: "Hx0",
                       font_face: font,
                       font: Font.pdf_name(font),
                       font_size: 20,
                       x: 10,
                       y: 20,
                       width: 36,
                       height: 24,
                       color: {0, 0, 0}
                     }
                   ]
                 }
               ],
               []
             )

    assert {:ok, prepared} =
             NativeElixirPdfUtilities.Validators.ColorFontValidator.prepare(context, [])

    geometry = Enum.find(hd(prepared.pages).boxes, &(&1.type == :text))
    assert elem(geometry.font_face.bitmap_widths, font.cmap[?H]) == 500

    for widths <- [
          {1},
          List.to_tuple(List.duplicate(-1, length(font.widths))),
          List.to_tuple(List.duplicate("1", length(font.widths)))
        ] do
      assert {:error, {:invalid_font, diagnostic}} =
               BitmapFontValidator.validate_metrics(Map.put(font, :bitmap_widths, widths))

      assert diagnostic.operation == :write_pdf
    end
  end

  test "COLR v0 palettes and nested v1 outlines/gradients produce alpha artwork with selected Unicode" do
    for file <- ["selected-colr.ttf", "selected-colr-v1.ttf"] do
      data = Paths.fixture("fonts/" <> file) |> File.read!()
      assert {:ok, [font]} = Font.load_configured_faces([%{family: "Vector", data: data}])

      assert {:ok, paint} =
               NativeElixirPdfUtilities.Validators.ColrFontValidator.prepare(
                 font,
                 font.cmap[?H],
                 {0, 0, 0, 1}
               )

      assert paint.graph != nil

      assert {:ok, svg, {left, bottom, right, top}, _context} =
               NativeElixirPdfUtilities.Validators.ColrPaintValidator.prepare(font, paint)

      assert svg =~ "clipPath"
      assert left <= 80 and bottom <= 0 and right >= 520 and top >= 700

      assert {:ok, light} =
               NativeElixirPdfUtilities.Validators.ColrFontValidator.prepare(
                 Map.put(font, :palette, :light),
                 font.cmap[?H],
                 {0, 0, 0, 1}
               )

      assert light.colors == paint.colors

      assert {:ok, dark} =
               NativeElixirPdfUtilities.Validators.ColrFontValidator.prepare(
                 Map.put(font, :palette, :dark),
                 font.cmap[?H],
                 {0, 0, 0, 1}
               )

      refute dark.colors == paint.colors

      assert {:ok, pdf} =
               HtmlToPdf.render(
                 "<p style='font-family:Vector;font-size:40px;font-palette:dark'>Hx0É</p>",
                 fonts: [%{family: "Vector", data: data}],
                 system_font_discovery: false,
                 compress_streams: false
               )

      assert pdf =~ "/Subtype /Image"
      assert {:ok, "Hx0É"} = Text.extract(pdf, layout: false)
    end
  end

  test "plain and gzip SVG glyph documents retain selected IDs, palettes, full ink and monochrome fallback" do
    for file <- ["selected-svg.ttf", "selected-svg-gzip.ttf"] do
      data = Paths.fixture("fonts/" <> file) |> File.read!()
      assert {:ok, [font]} = Font.load_configured_faces([%{family: "SVG Font", data: data}])

      assert {:ok, paint} =
               NativeElixirPdfUtilities.Validators.SvgFontValidator.prepare(
                 font,
                 font.cmap[?H],
                 {0, 0, 0, 1}
               )

      assert paint.bounds == {80.0, 0.0, 520.0, 700.0}
      assert paint.source =~ "native-selected-glyph"
      refute paint.source =~ "var("

      assert {:ok, nil} =
               NativeElixirPdfUtilities.Validators.SvgFontValidator.prepare(
                 font,
                 font.cmap[?0],
                 {0, 0, 0, 1}
               )

      assert {:ok, pdf} =
               HtmlToPdf.render(
                 "<p style='font-family:SVG Font;font-size:40px;font-palette:dark'>Hx0</p>",
                 fonts: [%{family: "SVG Font", data: data}],
                 system_font_discovery: false
               )

      assert {:ok, "Hx0"} = Text.extract(pdf, layout: false)
    end
  end

  test "SVG glyphs admit only embedded PNG/JPEG resources and distinguish transparent glyphs from missing definitions" do
    input = Paths.fixture("fonts/selected-svg.ttf") |> File.read!()

    assert {:ok, scaler, directory} =
             NativeElixirPdfUtilities.Validators.FontValidator.prepare_table_directory(input)

    tables =
      Map.new(directory, fn {tag, {offset, size}} -> {tag, binary_part(input, offset, size)} end)

    png =
      "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAAC0lEQVR4nGNgAAIAAAUAAXpeqz8AAAAASUVORK5CYII="

    sources = [
      {"<svg id='glyph2' xmlns='http://www.w3.org/2000/svg'><image x='80' y='-700' width='440' height='700' href='data:image/png;base64,#{png}'/></svg>",
       :image},
      {"<svg id='glyph2' xmlns='http://www.w3.org/2000/svg'></svg>", :empty},
      {"<svg xmlns='http://www.w3.org/2000/svg'><rect width='20' height='20'/></svg>", :missing},
      {"<svg id='glyph2' xmlns='http://www.w3.org/2000/svg'><image width='20' height='20' href='file:///tmp/unapproved.png'/></svg>",
       :invalid},
      {"<svg id='glyph2' xmlns='http://www.w3.org/2000/svg'><image width='20' height='20' href='data:image/svg+xml;base64,PHN2Zy8+' /></svg>",
       :invalid},
      {"<svg id='glyph2' xmlns='http://www.w3.org/2000/svg'><image width='20' height='20' href='data:image/png;base64,bm90IHBuZw==' /></svg>",
       :invalid}
    ]

    for {source, expected} <- sources do
      table =
        <<0::16, 10::32, 0::32, 1::16, 2::16, 2::16, 14::32, byte_size(source)::32,
          source::binary>>

      data =
        NativeElixirPdfUtilities.HtmlToPdf.Font.Sfnt.rebuild(
          scaler,
          Map.put(tables, "SVG ", table) |> Enum.sort()
        )

      assert {:ok, [font]} = Font.load_configured_faces([%{family: "SVG Font", data: data}])
      result = NativeElixirPdfUtilities.Validators.SvgFontValidator.prepare(font, 2, {0, 0, 0, 1})

      case expected do
        :invalid ->
          assert {:error, {_, %{message: message}}} = result
          assert message != ""

        :missing ->
          assert result == {:ok, nil}

        :empty ->
          assert result == {:ok, :empty}

        :image ->
          assert {:ok, paint} = result
          assert paint.bounds == {80.0, 130.0, 520.0, 570.0}

          assert {:ok, pdf} =
                   HtmlToPdf.render("<p style='font-family:SVG Font'>H</p>",
                     fonts: [%{family: "SVG Font", data: data}],
                     system_font_discovery: false
                   )

          assert {:ok, "H"} = Text.extract(pdf, layout: false)
      end
    end
  end
end
