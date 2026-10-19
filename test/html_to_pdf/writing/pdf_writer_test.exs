alias NativeElixirPdfUtilities.TestSupport.Paths

defmodule NativeElixirPdfUtilities.HtmlToPdf.PdfWriterTest do
  use ExUnit.Case

  alias NativeElixirPdfUtilities.HtmlToPdf.PdfWriter
  alias NativeElixirPdfUtilities.HtmlToPdf.Font
  alias NativeElixirPdfUtilities.HtmlToPdf.PageFurniture
  alias NativeElixirPdfUtilities.Pdf.Reader
  alias NativeElixirPdfUtilities.Text

  test "integer paint coordinates retain precision beyond binary64's exact integer range" do
    alias NativeElixirPdfUtilities.Limits
    alias NativeElixirPdfUtilities.Validators.LimitsValidator

    original = Limits.effective()
    on_exit(fn -> Limits.install(original) end)

    assert {:ok, limits} =
             LimitsValidator.validate(max_pdf_numeric_magnitude: 18_014_398_509_481_984)

    Limits.install(limits)

    box = %{
      type: :rect,
      x: 9_007_199_254_740_993,
      y: 0,
      width: 1,
      height: 1,
      fill_color: {0, 0, 0},
      stroke_color: nil,
      stroke_width: 0,
      border_radius: 0
    }

    assert {:ok, pdf} =
             PdfWriter.render([%{size: {100, 100}, boxes: [box]}], compress_streams: false)

    assert pdf =~ "9007199254740993 0 1 1 re"
  end

  test "rejects malformed vector form image bytes through the writer diagnostic contract" do
    image = %{format: :pdf_form, data: "not a PDF", width_px: 2, height_px: 2}
    box = %{type: :image, x: 1, y: 1, width: 2, height: 2, image: image}

    assert {:error, {:invalid_pdf_input, diagnostic}} =
             PdfWriter.render([%{size: {10, 10}, boxes: [box]}])

    assert diagnostic.stage == :pdf
    assert diagnostic.reason == :invalid_pdf_input
    assert diagnostic.operation == :write_pdf
    assert diagnostic.module == PdfWriter
    assert diagnostic.message =~ "valid pages"
  end

  defp glyph_text_box(font, text) do
    %{
      type: :text,
      text: text,
      x: 10,
      y: 80,
      font: Font.pdf_name(font),
      font_face: font,
      font_size: 20,
      color: {0, 0, 0}
    }
  end

  test "image border-box links intersect page bounds independently of fitted ink clipping" do
    box = %{
      type: :image,
      x: 5.0,
      y: 15.0,
      width: 60.0,
      height: 30.0,
      image: image_fixture(:png, <<255, 0, 0>>, 1, 1, :device_rgb),
      link_url: "https://example.com/photo",
      clip: %{x: 10.0, y: 20.0, width: 40.0, height: 20.0},
      link_rect: %{x: -8.0, y: -4.0, width: 115.0, height: 110.0}
    }

    page = %{size: {100, 100}, boxes: [box]}
    assert {:ok, pdf} = PdfWriter.render([page], compress_streams: false)
    assert pdf =~ "/Subtype /Link /Rect [0 0 100 100]"
    assert pdf =~ "/URI (https://example.com/photo)"
    refute pdf =~ "/Rect [10 20 50 40]"

    invisible = %{box | link_rect: %{x: 100.0, y: 10.0, width: 10.0, height: 10.0}}
    assert {:ok, pdf} = PdfWriter.render([%{page | boxes: [invisible]}])
    refute pdf =~ "/Subtype /Link"
  end

  test "URI annotations intersect final page bounds and omit invisible regions" do
    box = %{
      type: :text,
      text: "Clipped",
      x: -5,
      y: -3,
      font: "Helvetica",
      font_size: 10,
      annotation_width: 30,
      line_height: 12,
      color: {0, 0, 0},
      link_url: "https://example.test/chapter"
    }

    pages = [
      %{size: {100, 100}, boxes: [box, %{box | x: 90, y: 95}, %{box | x: 110}, %{box | y: -20}]}
    ]

    assert {:ok, pdf} = PdfWriter.render(pages, compress_streams: false)
    assert length(Regex.scan(~r{/Subtype /Link}, pdf)) == 2
    assert pdf =~ "/Rect [0 0 25 9]"
    assert pdf =~ "/Rect [90 95 100 100]"
    assert {:ok, text} = Text.extract(pdf, layout: false)
    assert text =~ "Clipped"
  end

  test "image links intersect fitted clipping rectangles and omit invisible annotations" do
    image = image_fixture(:png, <<255, 0, 0>>, 1, 1, :device_rgb)

    box = %{
      type: :image,
      x: 5.0,
      y: 15.0,
      width: 60.0,
      height: 30.0,
      image: image,
      link_url: "https://example.com/photo",
      clip: %{x: 10.0, y: 20.0, width: 40.0, height: 20.0}
    }

    page = %{size: {100, 100}, boxes: [box]}
    assert {:ok, pdf} = PdfWriter.render([page], compress_streams: false)
    assert pdf =~ "/Subtype /Link /Rect [10 20 50 40]"
    assert pdf =~ "/URI (https://example.com/photo)"
    assert {:ok, invisible} = PdfWriter.render([%{page | boxes: [%{box | x: 80.0}]}])
    refute invisible =~ "/Subtype /Link"
    border_box = Map.put(box, :link_rect, %{x: 3.0, y: 12.0, width: 66.0, height: 36.0})
    assert {:ok, linked} = PdfWriter.render([%{page | boxes: [border_box]}])
    assert linked =~ "/Subtype /Link /Rect [3 12 69 48]"

    for invalid <- [
          %{box | link_url: "javascript:bad"},
          Map.put(box, :link_rect, %{x: 1, y: 1, width: 0, height: 1})
        ] do
      assert {:error, {:invalid_pdf_input, diagnostic}} =
               PdfWriter.render([%{page | boxes: [invalid]}])

      assert diagnostic.stage == :pdf and diagnostic.operation == :write_pdf
      assert diagnostic.message =~ "valid pages"
    end
  end

  test "gauge gradients remain scoped page and Form resources without fields or images" do
    alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, Style, Layout}

    assert {:ok, dom} =
             HtmlParser.parse_detailed(
               "<p style='margin:0'><meter value=.5 style='appearance:none;width:40pt;height:20pt'></meter></p>"
             )

    assert {:ok, styled} = Style.compute_detailed(dom)
    assert {:ok, layout} = Layout.layout(styled, page_size: {100, 100}, margin: 0)
    frame = %{size: {100, 100}, boxes: layout.boxes}
    assert {:ok, entries, references, next} = PdfWriter.appearance_objects([frame], 10)
    assert length(references) == 1 and next > 10

    {dictionary, content} =
      Enum.find_value(entries, fn entry ->
        case entry do
          {_id, 0, {:stream, %{"Subtype" => {:name, "Form"}} = dictionary, content}} ->
            {dictionary, content}

          _ ->
            nil
        end
      end)

    assert map_size(dictionary["Resources"]["Shading"]) == 2

    assert Enum.all?(dictionary["Resources"]["Shading"], fn {_name, shading} ->
             shading["ShadingType"] == 2 and shading["Function"]["FunctionType"] == 3
           end)

    assert content =~ " sh"
    refute Map.has_key?(dictionary["Resources"], "XObject")
    assert {:ok, pdf} = PdfWriter.render([frame, frame], compress_streams: false)
    assert length(Regex.scan(~r/\/Shading << \/Gauge/, pdf)) == 2
    assert {:ok, []} = NativeElixirPdfUtilities.Forms.fields(pdf)
    assert {:ok, 2} = NativeElixirPdfUtilities.Info.page_count(pdf)
  end

  test "frame viewport print bias changes only paint and preserves clipped link geometry" do
    box = %{
      type: :image,
      x: 1,
      y: 2,
      width: 3,
      height: 4,
      image: image_fixture(:png, <<255, 0, 0>>, 1, 1, :device_rgb),
      link_url: "https://example.com/frame"
    }

    group = %{
      type: :drawing_group,
      width: 20,
      height: 20,
      transform: {1, 0, 0, 1, 10, 20},
      clip: %{x: 0, y: 0, width: 20, height: 20},
      boxes: [box]
    }

    assert {:ok, ordinary} =
             PdfWriter.render([%{size: {50, 50}, boxes: [group]}], compress_streams: false)

    assert {:ok, frame} =
             PdfWriter.render(
               [%{size: {50, 50}, boxes: [Map.put(group, :role, :frameset_viewport)]}],
               compress_streams: false
             )

    assert ordinary =~ "q 1 0 0 1 10 20 cm"
    assert frame =~ "q 1 0 0 1 9.9999 20 cm"
    for pdf <- [ordinary, frame], do: assert(pdf =~ "/Rect [11 22 14 26]")

    canvas = %{
      type: :rect,
      x: 0,
      y: 0,
      width: 20,
      height: 20,
      fill_color: {1, 0, 0},
      stroke_color: nil,
      stroke_width: 0,
      border_radius: 0,
      role: :frameset_canvas
    }

    assert {:ok, canvas_pdf} =
             PdfWriter.render([%{size: {50, 50}, boxes: [canvas]}], compress_streams: false)

    assert canvas_pdf =~ "0 0.0001 20 20 re f"
  end

  test "nested nonuniform groups clip image border-box links independently of letterbox ink" do
    image = image_fixture(:png, <<255, 0, 0>>, 1, 1, :device_rgb)

    box = %{
      type: :image,
      x: 4,
      y: 4,
      width: 8,
      height: 2,
      image: image,
      clip: %{x: 4, y: 4, width: 8, height: 2},
      link_url: "https://example.com/border",
      link_rect: %{x: 1, y: 2, width: 18, height: 8}
    }

    ink = box |> Map.delete(:link_rect) |> Map.put(:link_url, "https://example.com/ink")

    inner = %{
      type: :drawing_group,
      width: 25,
      height: 15,
      transform: {0.5, 0, 0, 2, 5, 7},
      clip: %{x: 0, y: 0, width: 25, height: 15},
      boxes: [box, ink]
    }

    outer = %{
      type: :drawing_group,
      width: 20,
      height: 20,
      transform: {2, 0, 0, 3, 10, 20},
      clip: %{x: 0, y: 0, width: 20, height: 20},
      boxes: [inner]
    }

    sibling =
      box
      |> Map.delete(:clip)
      |> Map.delete(:link_rect)
      |> Map.merge(%{x: 60, y: 50, width: 10, height: 5, link_url: "https://example.com/sibling"})

    assert {:ok, pdf} =
             PdfWriter.render([%{size: {100, 150}, boxes: [outer, sibling]}],
               compress_streams: false
             )

    assert pdf =~ "/Rect [21 53 39 80]"
    assert pdf =~ "/Rect [24 65 32 77]"
    assert pdf =~ "/Rect [60 50 70 55]"
    assert length(Regex.scan(~r/\/Subtype \/Link/, pdf)) == 3

    for empty <- [%{outer | width: 0}, %{outer | clip: %{x: 0, y: 0, width: 0, height: 20}}] do
      assert {:ok, pdf} = PdfWriter.render([%{size: {100, 150}, boxes: [empty, sibling]}])
      assert length(Regex.scan(~r/\/Subtype \/Link/, pdf)) == 1
      assert pdf =~ "https://example.com/sibling"
    end

    huge = %{
      box
      | link_rect: %{
          x: 0,
          y: 0,
          width: NativeElixirPdfUtilities.Limits.get(:max_pdf_numeric_magnitude),
          height: 1
        }
    }

    assert {:error, {:invalid_pdf_input, diagnostic}} =
             PdfWriter.render([%{size: {100, 150}, boxes: [%{outer | boxes: [huge]}]}])

    assert diagnostic.stage == :pdf and diagnostic.message =~ "transformed bounds"
  end

  test "nested drawing groups preserve Unicode, images, painting order and clipped logical links" do
    assert {:ok, registry} = Font.load_registry(system_font_discovery: false)
    assert {:ok, _, font} = Font.resolve("DejaVu Sans", 400, :normal, registry)

    text =
      glyph_text_box(font, "Ω")
      |> Map.merge(%{
        x: 0,
        y: 0,
        width: 20,
        line_height: 10,
        font_size: 10,
        glyph_ids: [font.cmap[0x03A9]],
        link_url: "https://example.com/visible"
      })

    glyphs = %{type: :glyph_group, actual_text: "∫𝑥", boxes: [text]}

    rectangle = %{
      type: :rect,
      x: 0,
      y: 0,
      width: 30,
      height: 20,
      fill_color: {0, 0, 1, 0.5},
      stroke_color: nil,
      stroke_width: 0,
      border_radius: 0
    }

    image = %{
      type: :image,
      x: 1,
      y: 2,
      width: 6,
      height: 8,
      clip: %{x: 2, y: 3, width: 3, height: 4},
      image: image_fixture(:png, <<255, 0, 0>>, 1, 1, :device_rgb)
    }

    hidden = %{text | x: 100, link_url: "https://example.com/clipped"}

    inner = %{
      type: :drawing_group,
      width: 50,
      height: 30,
      transform: {0.5, 0, 0, 2, 5, 4},
      clip: %{x: 0, y: 0, width: 15, height: 8},
      boxes: [rectangle, image, glyphs, hidden]
    }

    outer = %{
      type: :drawing_group,
      width: 80,
      height: 80,
      transform: {2, 0, 0, 3, 10, 20},
      clip: %{x: 7, y: 6, width: 50, height: 8},
      boxes: [inner]
    }

    sibling =
      Map.delete(
        %{text | text: "Sibling", x: 100, y: 100, link_url: "https://example.com/sibling"},
        :glyph_ids
      )

    pages = [%{size: {300, 300}, boxes: [outer, sibling]}, %{size: {300, 300}, boxes: [outer]}]

    for options <- [[compress_streams: false], [], [subset_fonts: false]] do
      assert {:ok, pdf} = PdfWriter.render(pages, options)
      assert {:ok, extracted} = Text.extract(pdf, layout: false)
      assert extracted =~ "∫𝑥" and extracted =~ "Sibling"
      assert {:ok, context} = Reader.read_validated(pdf)
      assert length(context.pages) == 2
      assert {:ok, %{pages: [%{spans: spans}, _]}} = Text.extract_spans(pdf)
      span = Enum.find(spans, &(&1.text == "∫𝑥"))
      assert span.x == 20.0, inspect(span)
      assert span.y == 268.0
      assert span.ctm == [1.0, 0.0, 0.0, 6.0, 20.0, 32.0]
      assert pdf =~ "/Rect [24 38 35 62]"
      assert pdf =~ "/Rect [100 100 120 110]"
      refute pdf =~ "https://example.com/clipped"
      assert pdf =~ "/Subtype /Image" and pdf =~ "/ExtGState"
      assert {:ok, []} = NativeElixirPdfUtilities.Forms.fields(pdf)

      if options == [compress_streams: false] do
        assert pdf =~ "q 2 0 0 3 10 20 cm 7 6 50 8 re W n"
        assert pdf =~ "q 0.5 0 0 2 5 4 cm 0 0 15 8 re W n"
        assert pdf =~ "2 3 3 4 re W n"
        assert pdf =~ "/ActualText"
      end
    end

    assert {:ok, objects, [_ref], _next} = PdfWriter.appearance_objects([hd(pages)], 1)

    assert Enum.any?(objects, fn {_id, _generation, value} ->
             case value do
               {:stream, _dictionary, body} -> body =~ "q 2 0 0 3 10 20 cm"
               _ -> false
             end
           end)
  end

  test "drawing groups count every expanded box across nested groups and pages, and bound depth" do
    alias NativeElixirPdfUtilities.Limits
    original = Limits.effective()
    on_exit(fn -> Limits.install(original) end)

    text = %{
      type: :text,
      text: "x",
      x: 0,
      y: 0,
      font: "Helvetica",
      font_size: 10,
      color: {0, 0, 0}
    }

    group = %{
      type: :drawing_group,
      width: 10,
      height: 10,
      transform: {1, 0, 0, 1, 0, 0},
      boxes: [text]
    }

    page = %{size: {100, 100}, boxes: [group]}
    Limits.install(%{original | max_layout_boxes: 6, max_drawing_group_depth: 2})
    assert {:ok, _} = PdfWriter.render([page, page])
    assert {:ok, _} = PdfWriter.render([%{page | boxes: [%{group | boxes: [group]}]}])

    for pages <- [
          [page, page, page],
          [%{page | boxes: [group, group, group]}],
          [%{page | boxes: [%{group | boxes: [group, group]}]}]
        ] do
      assert {:error, {:resource_limit_exceeded, diagnostic}} = PdfWriter.render(pages)
      assert diagnostic.stage == :writer and diagnostic.message =~ "max_layout_boxes"
    end

    Limits.install(%{original | max_drawing_group_depth: 2})

    assert {:error, {:resource_limit_exceeded, %{message: message}}} =
             PdfWriter.render([%{page | boxes: [%{group | boxes: [%{group | boxes: [group]}]}]}])

    assert message =~ "max_drawing_group_depth"
    assert {:ok, _} = PdfWriter.render([page])

    Limits.install(%{original | max_layout_boxes: 5})
    glyphs = %{type: :glyph_group, actual_text: "x", boxes: [text]}
    assert {:ok, _} = PdfWriter.render([%{page | boxes: [%{group | boxes: [glyphs]}]}])

    assert {:error, {:resource_limit_exceeded, _}} =
             PdfWriter.render([%{page | boxes: [%{group | boxes: [glyphs, text]}]}])
  end

  test "unclipped drawing groups isolate siblings and use local viewport pixel snapping" do
    text = %{
      type: :text,
      text: "Local",
      x: 0,
      y: 7,
      font: "Helvetica",
      font_size: 10,
      color: {0, 0, 0},
      snap_to_css_pixel_grid: true,
      width: 10,
      link_url: "https://example.com/local"
    }

    group = %{
      type: :drawing_group,
      width: 20,
      height: 20,
      transform: {1.00004, 0, 0, 1, 5.00004, 6.00004},
      boxes: [text]
    }

    assert {:ok, pdf} =
             PdfWriter.render([%{size: {100, 100}, boxes: [group]}], compress_streams: false)

    assert pdf =~ "q 1 0 0 1 5 6 cm"
    assert pdf =~ "0 7.2501 Td"
    assert pdf =~ "/Rect [5 13.2501 15 25.2501]"

    rounded_up = %{group | transform: {1.00006, 0, 0, 1, 5.00006, 6.00006}}

    assert {:ok, up} =
             PdfWriter.render([%{size: {100, 100}, boxes: [rounded_up]}], compress_streams: false)

    assert up =~ "q 1.0001 0 0 1 5.0001 6.0001 cm"
    assert up =~ "/Rect [5.0001 13.2502 15.0011 25.2502]"

    for clip <- [nil, %{x: 0, y: 0, width: 0, height: 0}] do
      assert {:ok, clipped} =
               PdfWriter.render([%{size: {100, 100}, boxes: [Map.put(group, :clip, clip)]}])

      if clip == nil,
        do: assert(clipped =~ "https://example.com/local"),
        else: refute(clipped =~ "https://example.com/local")
    end
  end

  test "direct appearance objects preserve complete page artwork and resource graphs" do
    alias NativeElixirPdfUtilities.Pdf.{FormWriter, IncrementalWriter}
    assert {:ok, registry} = Font.load_registry(system_font_discovery: false)
    assert {:ok, _, font} = Font.resolve("DejaVu Sans", 400, :normal, registry)
    text = glyph_text_box(font, "Appearance Ω") |> Map.put(:color, {0.2, 0.3, 0.4, 0.5})

    rectangle = %{
      type: :rect,
      x: 1,
      y: 15,
      width: 20,
      height: 8,
      fill_color: {0.4, 0.5, 0.6, 0.7},
      stroke_color: {0, 0, 0, 0.8},
      stroke_width: 1,
      border_radius: 0
    }

    image = image_fixture(:png, <<255, 0, 0>>, 1, 1, :device_rgb) |> Map.put(:alpha_data, <<128>>)
    png = %{type: :image, x: 5, y: 10, width: 4, height: 4, image: image}
    jpeg = %{png | image: image_fixture(:jpeg, jpeg_fixture(1, 1, 4), 1, 1, :device_cmyk)}

    builtin =
      Map.drop(text, [:font_face]) |> Map.put(:font, "Helvetica") |> Map.put(:text, "Built in")

    group = %{type: :glyph_group, actual_text: "Original Ω", boxes: [text]}
    marker = %{type: :flow_marker, x: 0, y: 0, width: 0, height: 12}

    frames = [
      %{size: {100, 100}, boxes: [rectangle, group, png, jpeg, marker]},
      %{size: {30, 20}, boxes: []},
      %{size: {100, 100}, boxes: [builtin, text, png]}
    ]

    assert {:ok, base} = PdfWriter.render([%{size: {200, 200}, boxes: []}])
    assert {:ok, context} = Reader.read_validated(base)
    first = context.document.trailer["Size"]
    assert {:ok, direct, refs, next} = PdfWriter.appearance_objects(frames, first)
    assert length(refs) == 3 and next == first + length(direct)
    assert {:ok, original} = PdfWriter.render(frames, forms: :static)
    assert {:ok, source} = Reader.read_validated(original)
    {legacy, legacy_refs, _} = FormWriter.import_appearances(source, first)
    assert length(direct) < length(legacy)
    assert {:ok, direct_pdf} = IncrementalWriter.write(context, direct)
    assert {:ok, legacy_pdf} = IncrementalWriter.write(context, legacy)
    assert {:ok, direct_context} = Reader.read_validated(direct_pdf)
    assert {:ok, legacy_context} = Reader.read_validated(legacy_pdf)

    canonical = fn canonical, document, value ->
      case value do
        {:ref, ref} ->
          object = Map.fetch!(document.objects, ref)
          {canonical.(canonical, document, Map.delete(object.value, "Length")), object.stream}

        dictionary when is_map(dictionary) ->
          Map.new(dictionary, fn {key, value} -> {key, canonical.(canonical, document, value)} end)

        values when is_list(values) ->
          Enum.map(values, &canonical.(canonical, document, &1))

        value ->
          value
      end
    end

    for {direct_ref, legacy_ref} <- Enum.zip(refs, legacy_refs) do
      assert canonical.(canonical, direct_context.document, direct_ref) ==
               canonical.(canonical, legacy_context.document, legacy_ref)
    end

    assert length(direct_context.pages) == 1

    refute Enum.any?(direct, fn {_, _, body} ->
             match?({:value, %{"Type" => {:name, "Page"}}}, body) or
               match?({:value, %{"Type" => {:name, "Catalog"}}}, body)
           end)
  end

  test "appearance frame budgets are separate from page limits" do
    alias NativeElixirPdfUtilities.Limits
    original = Limits.effective()
    on_exit(fn -> Limits.install(original) end)
    page = %{size: {10, 10}, boxes: []}

    Limits.install(%{
      original
      | max_rendered_pages: 1,
        max_pdf_pages: 1,
        max_appearance_widgets: 2
    })

    assert {:ok, _, [_, _], _} = PdfWriter.appearance_objects([page, page], 1)

    assert {:error, {:resource_limit_exceeded, %{message: message}}} =
             PdfWriter.render([page, page])

    assert message =~ "max_rendered_pages"
    Limits.install(%{original | max_pdf_pages: 1})

    assert {:error, {:resource_limit_exceeded, %{message: message}}} =
             PdfWriter.render([page, page])

    assert message =~ "max_pdf_pages"
    Limits.install(%{original | max_appearance_widgets: 1})

    assert {:error, {:resource_limit_exceeded, %{message: message}}} =
             PdfWriter.appearance_objects([page, page], 1)

    assert message =~ "max_appearance_widgets"
    Limits.install(%{original | max_pdf_objects: 1})

    for id <- [2, 1] do
      assert {:error, {:resource_limit_exceeded, %{message: message}}} =
               PdfWriter.appearance_objects([page], id)

      assert message =~ "max_pdf_objects"
    end

    Limits.install(%{original | max_rendered_pdf_bytes: 1})

    assert {:error, {:resource_limit_exceeded, %{message: message}}} =
             PdfWriter.appearance_objects([page], 1)

    assert message =~ "max_rendered_pdf_bytes"
    Limits.install(original)

    for {pages, id} <- [{[], 1}, {[page], 0}, {[page], nil}, {[%{size: {0, 0}, boxes: []}], 1}] do
      assert {:error,
              {:invalid_pdf_input,
               %{reason: :invalid_pdf_input, operation: :write_pdf, message: message}}} =
               PdfWriter.appearance_objects(pages, id)

      assert message != ""
    end
  end

  test "drawing groups count and remove valid flow markers before painting" do
    marker = %{type: :flow_marker, x: 10, y: 20, width: 0, height: 12}

    group = %{
      type: :drawing_group,
      width: 100,
      height: 100,
      transform: {1, 0, 0, 1, 0, 0},
      boxes: [marker]
    }

    assert {:ok, with_marker} = PdfWriter.render([%{size: {100, 100}, boxes: [group]}])

    assert {:ok, without_marker} =
             PdfWriter.render([%{size: {100, 100}, boxes: [%{group | boxes: []}]}])

    assert with_marker == without_marker

    assert {:error, {:invalid_pdf_input, diagnostic}} =
             PdfWriter.render([
               %{size: {100, 100}, boxes: [%{group | boxes: [%{marker | width: 1}]}]}
             ])

    assert diagnostic.stage == :pdf
    assert diagnostic.message =~ "drawing_group children"
  end

  test "flow markers count toward the cumulative layout-box budget before removal" do
    alias NativeElixirPdfUtilities.Limits
    original = Limits.effective()
    on_exit(fn -> Limits.install(original) end)
    Limits.install(%{original | max_layout_boxes: 1})
    marker = %{type: :flow_marker, x: 10, y: 20, width: 0, height: 12}
    page = %{size: {100, 100}, boxes: [marker]}
    assert {:ok, _} = PdfWriter.render([page])

    for pages <- [[%{page | boxes: [marker, marker]}], [page, page]] do
      assert {:error, {:resource_limit_exceeded, diagnostic}} = PdfWriter.render(pages)
      assert diagnostic.stage == :writer and diagnostic.message =~ "max_layout_boxes"
    end
  end

  test "flow markers preserve pages and produce exactly the PDF of the visible boxes" do
    marker = %{type: :flow_marker, x: 10, y: 20, width: 0.0, height: 12}

    text = %{
      type: :text,
      text: "Visible",
      x: 10,
      y: 80,
      font: "Helvetica",
      font_size: 12,
      color: {0, 0, 0},
      annotation_width: 40,
      link_url: "https://example.com"
    }

    page = %{size: {100, 100}, boxes: [text]}
    empty = %{page | boxes: []}

    for opts <- [[], [compress_streams: false], [subset_fonts: false]] do
      assert {:ok, reference} = PdfWriter.render([page, empty, page], opts)

      assert {:ok, ^reference} =
               PdfWriter.render(
                 [
                   %{page | boxes: [marker, text, marker]},
                   %{empty | boxes: [marker]},
                   %{page | boxes: [text, marker]}
                 ],
                 opts
               )

      assert {:ok, "Visible\nVisible"} = Text.extract(reference, layout: false)
    end
  end

  test "atomic bottom baselines preserve fractional depth and reject invalid supplied depths" do
    box = %{
      type: :text,
      text: "Visible",
      x: 10,
      y: 65.5,
      font: "Helvetica",
      font_size: 8,
      color: {0, 0, 0},
      snap_to_css_pixel_grid: true,
      baseline_snap_depth: 9.5
    }

    assert {:ok, pdf} = PdfWriter.render([%{size: {100, 100}, boxes: [box]}], [])
    assert pdf =~ "10 65.7579 Td"

    assert {:ok, ordinary_pdf} =
             PdfWriter.render(
               [%{size: {100, 100}, boxes: [Map.delete(box, :baseline_snap_depth)]}],
               []
             )

    for depth <- [0, 0.0] do
      assert {:ok, ^ordinary_pdf} =
               PdfWriter.render(
                 [%{size: {100, 100}, boxes: [Map.put(box, :baseline_snap_depth, depth)]}],
                 []
               )
    end

    for invalid <- [nil, "invalid", %{}, -1] do
      assert {:error, {:invalid_pdf_input, diagnostic}} =
               PdfWriter.render(
                 [%{size: {100, 100}, boxes: [Map.put(box, :baseline_snap_depth, invalid)]}],
                 []
               )

      assert diagnostic.stage == :pdf
      assert diagnostic.operation == :write_pdf
      assert diagnostic.message =~ "baseline_snap_depth"
    end
  end

  test "decoration raster placement follows the painted baseline and validates supplied offsets" do
    box = %{
      type: :rect,
      x: 10,
      y: 20.1,
      width: 30,
      height: 0.75,
      fill_color: {0, 0, 0},
      stroke_color: nil,
      stroke_width: 0,
      border_radius: 0,
      snap_to_css_pixel_grid: true,
      text_baseline_offset: -1.5
    }

    assert {:ok, pdf} = PdfWriter.render([%{size: {100, 100}, boxes: [box]}], [])
    assert pdf =~ "9.75 19.75 30 0.75 re f"

    atomic = Map.merge(box, %{y: 64.375, text_baseline_offset: -1.125, baseline_snap_depth: 9.5})
    assert {:ok, atomic_pdf} = PdfWriter.render([%{size: {100, 100}, boxes: [atomic]}], [])
    assert atomic_pdf =~ "9.75 64.75 30 0.75 re f"

    for field <- [:text_baseline_offset, :baseline_snap_depth],
        invalid <- [nil, "invalid", %{}] do
      assert {:error,
              {:invalid_pdf_input, %{stage: :pdf, operation: :write_pdf, message: message}}} =
               PdfWriter.render(
                 [%{size: {100, 100}, boxes: [Map.put(box, field, invalid)]}],
                 []
               )

      assert is_binary(message) and message != ""
    end
  end

  test "glyph groups paint selected pieces and extract one original Unicode string" do
    assert {:ok, registry} = Font.load_registry(system_font_discovery: false)
    assert {:ok, _, font} = Font.resolve("math", 400, :normal, registry)

    pieces =
      for {text, index} <- Enum.with_index(["x", "y", "z"]),
          do:
            glyph_text_box(font, text)
            |> Map.put(:glyph_ids, [font.cmap[String.to_charlist(text) |> hd()]])
            |> Map.put(:x, 10 + index * 15)

    group = %{type: :glyph_group, actual_text: "∫𝑥", boxes: pieces}
    page = %{size: {100, 100}, boxes: [group]}

    for opts <- [[compress_streams: false], [subset_fonts: false], []] do
      assert {:ok, pdf} = PdfWriter.render([page, page], opts)
      assert {:ok, "∫𝑥\n∫𝑥"} = Text.extract(pdf, layout: false)
      assert {:ok, %{pages: [%{spans: [first]}, %{spans: [second]}]}} = Text.extract_spans(pdf)
      assert first.text == "∫𝑥"
      assert first.x == 10.0
      assert first.end_x > 40.0
      assert first.source_index == 0
      assert first == second
    end
  end

  test "glyph group strings use the shared PDF codec and permit empty replacements" do
    box = %{
      type: :text,
      text: "pieces",
      x: 10,
      y: 80,
      font: "Helvetica",
      font_size: 12,
      color: {0, 0, 0}
    }

    text = "a(b)\\c\n"

    groups = [
      %{type: :glyph_group, actual_text: text, boxes: [box]},
      %{type: :glyph_group, actual_text: "", boxes: [box]},
      %{type: :glyph_group, actual_text: "", boxes: []}
    ]

    assert {:ok, pdf} =
             PdfWriter.render([%{size: {100, 100}, boxes: groups}], compress_streams: false)

    assert {:ok, ^text} = Text.extract(pdf, layout: false)
    assert {:ok, %{pages: [%{spans: [%{text: ^text}]}]}} = Text.extract_spans(pdf)
    assert pdf =~ "/ActualText"
    assert pdf =~ "BDC"
    assert pdf =~ "EMC"
  end

  test "glyph group validation rejects malformed strings, nested groups and raw markers" do
    box = %{
      type: :text,
      text: "x",
      x: 10,
      y: 80,
      font: "Helvetica",
      font_size: 12,
      color: {0, 0, 0}
    }

    group = %{type: :glyph_group, actual_text: "x", boxes: [box]}

    for invalid <- [
          %{group | actual_text: <<255>>},
          %{group | actual_text: 1},
          %{group | boxes: [group]},
          %{group | boxes: [1]},
          %{group | boxes: :invalid},
          %{group | boxes: [%{box | type: :image}]},
          %{group | boxes: [Map.put(box, :snap_to_css_pixel_grid, 1)]},
          %{type: :actual_text_start, text: "x"},
          %{type: :actual_text_end}
        ] do
      assert {:error, {:invalid_pdf_input, diagnostic}} =
               PdfWriter.render([%{size: {100, 100}, boxes: [invalid]}])

      assert diagnostic.stage == :pdf
      assert diagnostic.reason == :invalid_pdf_input
      assert diagnostic.operation == :write_pdf
      assert diagnostic.module == PdfWriter
      assert diagnostic.message =~ "glyph_group"
    end
  end

  test "glyph groups check cumulative replacement bytes and piece expansion before allocating" do
    alias NativeElixirPdfUtilities.Limits
    original = Limits.effective()
    on_exit(fn -> Limits.install(original) end)
    Limits.install(%{original | max_rendered_text_bytes: 3, max_layout_boxes: 6})

    box = %{
      type: :text,
      text: "x",
      x: 10,
      y: 80,
      font: "Helvetica",
      font_size: 12,
      color: {0, 0, 0}
    }

    group = %{type: :glyph_group, actual_text: "ab", boxes: [box]}
    assert {:ok, _pdf} = PdfWriter.render([%{size: {100, 100}, boxes: [group]}])

    assert {:error, {:resource_limit_exceeded, %{stage: :writer, message: message}}} =
             PdfWriter.render([%{size: {100, 100}, boxes: [group, group]}])

    assert message =~ "max_rendered_text_bytes"
    Limits.install(%{original | max_layout_boxes: 3})

    assert {:error, {:resource_limit_exceeded, %{message: message}}} =
             PdfWriter.render([%{size: {100, 100}, boxes: [box, %{group | boxes: [box, box]}]}])

    assert message =~ "max_layout_boxes"

    assert {:error, {:resource_limit_exceeded, _}} =
             PdfWriter.render([
               %{size: {100, 100}, boxes: [group]},
               %{size: {100, 100}, boxes: [group]}
             ])
  end

  test "zero-size text paints no operators or extractable text" do
    box = %{
      type: :text,
      text: "Invisible",
      x: 10,
      y: 30,
      font: "Helvetica",
      font_size: 0,
      color: {0, 0, 0}
    }

    assert {:ok, pdf} = PdfWriter.render([%{size: {100, 100}, boxes: [box]}])
    refute pdf =~ "(Invisible) Tj"
    assert {:error, {:no_extractable_text, _}} = Text.extract(pdf)

    assert {:ok, pdf} =
             NativeElixirPdfUtilities.HtmlToPdf.render(
               "<pre style='font-size:0'>A\tB</pre><p>Visible</p>"
             )

    assert {:ok, "Visible"} = Text.extract(pdf)
  end

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

    bordered = %{box | fill_color: nil, stroke_color: {0, 0, 0}, stroke_width: 0.75}
    assert {:ok, pdf} = PdfWriter.render([%{size: {100, 100}, boxes: [bordered]}], forms: :static)
    assert pdf =~ "13.4999 26.8335 39 53 re S"

    control =
      Map.put(bordered, :form_control, %{
        id: :local_appearance,
        order: [0],
        tag: "input",
        attributes: %{},
        children: [],
        initial_state: %{owner: nil, checked: false, disabled: false, value: ""}
      })

    assert {:ok, pdf} = PdfWriter.render([%{size: {100, 100}, boxes: [control]}], forms: :static)
    assert pdf =~ "13.5 26.8333 39 53 re S"
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

  test "stream compression is enabled by default and keeps content readable" do
    text = String.duplicate("ABCD", 325)

    box = %{
      type: :text,
      text: text,
      x: 0,
      y: 20,
      font: "Helvetica",
      font_size: 10,
      color: {0, 0, 0}
    }

    pages = [%{size: {100, 100}, boxes: [box]}]
    assert {:ok, plain} = PdfWriter.render(pages, compress_streams: false)
    assert {:ok, compressed} = PdfWriter.render(pages)
    refute plain =~ "/Filter /FlateDecode"
    assert compressed =~ "/Filter /FlateDecode"
    assert byte_size(compressed) < byte_size(plain)
    assert {:ok, ^text} = Text.extract(compressed, layout: false)

    assert {:ok, short} =
             PdfWriter.render([%{size: {100, 100}, boxes: [%{box | text: "Short"}]}],
               compress_streams: true
             )

    refute short =~ "/Filter /FlateDecode"

    assert {:ok, built_in_subset} = PdfWriter.render(pages, subset_fonts: true)
    assert {:ok, ^text} = Text.extract(built_in_subset, layout: false)

    # The reader's decompression-ratio limit is also respected by the writer.
    assert {:ok, extreme} =
             PdfWriter.render(
               [%{size: {100, 100}, boxes: [%{box | text: String.duplicate("A", 20_000)}]}],
               compress_streams: true
             )

    refute extreme =~ "/Filter /FlateDecode"
    assert {:ok, _text} = Text.extract(extreme, layout: false)
  end

  test "solid PNG color and alpha streams remain readable within the default decompression limits" do
    pixels = :binary.copy(<<255, 0, 0>>, 40_000)
    alpha = :binary.copy(<<128>>, 40_000)

    image =
      image_fixture(:png, pixels, 200, 200, :device_rgb)
      |> Map.put(:alpha_data, alpha)

    box = %{type: :image, x: 0, y: 0, width: 200, height: 200, image: image}
    assert {:ok, pdf} = PdfWriter.render([%{size: {200, 200}, boxes: [box]}])
    assert {:ok, document} = Reader.read(pdf)

    images =
      for {ref, %{value: %{"Subtype" => {:name, "Image"}}} = object} <- document.objects do
        assert {:ok, decoded} = Reader.decoded_stream(document, {:ref, ref})
        assert object.value["Filter"] == {:name, "FlateDecode"}

        assert byte_size(decoded) <=
                 byte_size(object.stream) *
                   NativeElixirPdfUtilities.Limits.get(:max_pdf_decompression_ratio)

        {object.value["ColorSpace"], decoded}
      end

    assert Enum.sort(images) ==
             Enum.sort([{{:name, "DeviceRGB"}, pixels}, {{:name, "DeviceGray"}, alpha}])
  end

  test "writer rejects conflicting embedded faces with the same ID" do
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

    conflicting = %{box | x: 20, font_face: %{font | ascent: font.ascent - 1}}

    assert {:error,
            {:invalid_pdf_input, %{stage: :pdf, reason: :invalid_pdf_input, message: message}}} =
             PdfWriter.render([%{size: {100, 100}, boxes: [box, conflicting]}])

    assert message =~ "conflicting face data"

    metadata_only = %{box | x: 20, font_face: %{font | source: :configured}}
    assert {:ok, _pdf} = PdfWriter.render([%{size: {100, 100}, boxes: [box, metadata_only]}])
  end

  test "subset failures retain an actionable font diagnostic" do
    assert {:ok, registry} = Font.load_registry(fonts: [{"Fixture Sans", ttf_font_path!()}])
    assert {:ok, _, font} = Font.resolve("Fixture Sans", 400, :normal, registry)

    box = %{
      type: :text,
      text: "A",
      x: 0,
      y: 20,
      font: Font.pdf_name(font),
      font_face: %{font | data: "invalid sfnt"},
      font_size: 10,
      color: {0, 0, 0}
    }

    assert {:error,
            {:invalid_document, %{stage: :font, reason: :invalid_document, message: message}}} =
             PdfWriter.render([%{size: {100, 100}, boxes: [box]}], subset_fonts: true)

    assert message =~ "TrueType"
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

    thin_rule =
      %{
        box
        | height: 1.5,
          stroke_width: 0.75,
          border_widths: %{top: 0.75, right: 0.75, bottom: 0.75, left: 0.75}
      }
      |> Map.put(:role, :horizontal_rule)

    assert {:ok, pdf} = PdfWriter.render([%{size: {100, 100}, boxes: [thin_rule]}], [])
    # The two 1px strokes meet at their edges and do not overlap.
    assert pdf =~ "0 1.5 m 40 1.5 l"
    assert pdf =~ "0 0.5 m 40 0.5 l"
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
    assert pdf =~ "/F1 13.33 Tf"
    assert pdf =~ "0.23999999 0 0 -0.23999999 0 100 cm"
    assert pdf =~ "1 0 0 -1 0 26 Tm ["
    assert pdf =~ "] TJ"
    assert {:ok, "AV"} = Text.extract(pdf, layout: false)
  end

  test "CSS images preserve their snapped source canvas without shrinking raster edges" do
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

    assert pdf =~
             "q 0.23999999 0 0 -0.23999999 0 100 cm 0 365.625 87.5 50 re W n 87.5 0 0 -50 0 415.625 cm /Im1 Do Q"

    assert pdf =~ "/Width 1 /Height 1"
  end

  test "snapped block fills preserve Chromium's half-point top edge without moving their clip" do
    box = %{
      type: :rect,
      role: :block_fragment_paint,
      snap_to_css_pixel_grid: true,
      x: 12.0,
      y: 122.25,
      width: 216.0,
      height: 8.25,
      fill_color: {0, 0, 0},
      stroke_color: nil,
      stroke_width: 0,
      border_radius: 0,
      clip: %{x: 12, y: 122.25, width: 216, height: 8.25}
    }

    assert {:ok, pdf} =
             PdfWriter.render([%{size: {240, 180}, boxes: [box]}], compress_streams: false)

    assert pdf =~ "12 122.25 216 8.25 re W n"
    assert pdf =~ "11.9999 122.2501 216 8.25 re f"
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

    empty = put_in(pages, [Access.at(0), :boxes, Access.at(0), :clip, :width], 0)
    assert {:ok, empty_pdf} = PdfWriter.render(empty)
    assert empty_pdf =~ "q 10 20 0 20 re W n"
    invalid = put_in(pages, [Access.at(0), :boxes, Access.at(0), :clip, :width], -1)
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
    streams = decoded_pdf_streams(pdf)

    assert streams =~ ~r/\[[^\]]+\] 0 d 1 J/
    assert streams =~ ~r/\[[^\]]+\] 0 d 0 J/
    assert streams =~ "0.2 0.4 0.6 RG"
    assert streams =~ "0.0902 0.1804 0.2706 RG"
    assert streams =~ "0.3098 0.6196 0.9294 RG"
    assert streams =~ "1 0 0 RG 1.3333 w"
    assert length(Regex.scan(~r/1 0 0 RG 1\.3333 w/, streams)) == 1

    assert streams =~ "10 50 10 10 re f"
    assert streams =~ "30 50 10 10 re f"

    assert streams =~ "0.2 0.4 0.6 RG 1.3333 w"
    assert streams =~ " re S"
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

  test "pages reference only their own fonts, images and opacity states" do
    pages =
      for {font, rgb, alpha} <- [
            {"Helvetica", <<255, 0, 0>>, 0.25},
            {"Courier", <<0, 255, 0>>, 0.5}
          ] do
        %{
          size: {100.0, 100.0},
          boxes: [
            %{
              type: :text,
              text: "Page",
              x: 10.0,
              y: 70.0,
              font: font,
              font_size: 10.0,
              color: {0, 0, 0, alpha}
            },
            %{
              type: :image,
              x: 10.0,
              y: 10.0,
              width: 10.0,
              height: 10.0,
              image: image_fixture(:png, rgb, 1, 1, :device_rgb)
            }
          ]
        }
      end

    assert {:ok, pdf} = PdfWriter.render(pages ++ [%{size: {100, 100}, boxes: []}])
    assert {:ok, context} = Reader.read_validated(pdf)

    for page <- Enum.take(context.pages, 2) do
      assert {:ok, resources} = Reader.dictionary(context.document, page.resources)
      assert map_size(resources["Font"]) == 1
      assert map_size(resources["XObject"]) == 1
      assert map_size(resources["ExtGState"]) == 1
    end

    assert {:ok, %{"Font" => fonts} = empty} =
             Reader.dictionary(context.document, List.last(context.pages).resources)

    assert fonts == %{}
    refute Map.has_key?(empty, "XObject")
    refute Map.has_key?(empty, "ExtGState")
    assert {:ok, outputs} = NativeElixirPdfUtilities.Split.by_page(pdf)

    for {output, expected} <- Enum.zip(outputs, [1, 1, 0]) do
      assert {:ok, document} = Reader.read(output)

      assert Enum.count(document.objects, fn {_ref, object} ->
               is_map(object.value) and object.value["Subtype"] == {:name, "Image"}
             end) == expected
    end
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

    [page] = pages
    [box] = page.boxes
    opaque = %{box | image: Map.put(box.image, :alpha_data, nil)}
    assert {:ok, pdf} = PdfWriter.render([%{page | boxes: [opaque]}])
    refute pdf =~ "/SMask"
    assert {:ok, document} = Reader.read(pdf)
    assert {:ok, <<0, 0, 0, 255, 0, 0>>} = Reader.decoded_stream(document, {:ref, {3, 0}})
  end

  test "PNG soft masks preserve visible samples and extend colors only one pixel into transparency" do
    for color_space <- [:device_rgb, :device_gray],
        depth <- [8, 16],
        right <- [3, 5],
        empty <- [false, true] do
      channels = if color_space == :device_rgb, do: 3, else: 1
      hidden = Enum.take([19, 7, 3], channels)
      red = Enum.take([100, 50, 25], channels)
      blue = Enum.take([200, 120, 90], channels)

      pixels =
        for y <- 0..4, x <- 0..6, into: <<>> do
          values =
            cond do
              not empty and {x, y} == {1, 2} -> red
              not empty and {x, y} == {right, 2} -> blue
              true -> hidden
            end

          for value <- values, into: <<>>, do: <<value::size(depth)>>
        end

      alpha =
        for y <- 0..4, x <- 0..6, into: <<>> do
          value =
            cond do
              not empty and {x, y} == {1, 2} -> 1
              not empty and {x, y} == {right, 2} -> 2 ** depth - 1
              true -> 0
            end

          <<value::size(depth)>>
        end

      image =
        image_fixture(:png, pixels, 7, 5, color_space)
        |> Map.merge(%{bits_per_component: depth, alpha_data: alpha})

      box = %{type: :image, x: 0.25, y: 0.5, width: 35, height: 25, image: image}
      assert {:ok, pdf} = PdfWriter.render([%{size: {50, 50}, boxes: [box]}])
      assert {:ok, document} = Reader.read(pdf)
      assert {:ok, ^alpha} = Reader.decoded_stream(document, {:ref, {4, 0}})
      assert {:ok, colors} = Reader.decoded_stream(document, {:ref, {3, 0}})

      expected =
        for y <- 0..4, x <- 0..6, into: <<>> do
          values =
            cond do
              not empty and y in 1..3 and x in (right - 1)..(right + 1) -> blue
              not empty and y in 1..3 and x in 0..2 -> red
              true -> hidden
            end

          for value <- values, into: <<>>, do: <<value::size(depth)>>
        end

      assert colors == expected
    end
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

  describe "selected glyph text" do
    setup do
      limits = NativeElixirPdfUtilities.Limits.effective()
      on_exit(fn -> NativeElixirPdfUtilities.Limits.install(limits) end)
      assert {:ok, registry} = Font.load_registry(system_font_discovery: false)
      assert {:ok, _, font} = Font.resolve("math", 400, :normal, registry)
      {:ok, font: font, limits: limits}
    end

    test "normal, script, and scriptscript forms retain original supplementary Unicode", %{
      font: font
    } do
      assert {:ok, metrics} = Font.math_metrics(font)
      [scripts] = metrics.substitutions["ssty"]
      unicode = 0x1D465
      [script, scriptscript] = scripts[font.cmap[unicode]]
      box = glyph_text_box(font, <<unicode::utf8>>)

      boxes = [
        box,
        Map.put(%{box | y: 50}, :glyph_ids, [script]),
        Map.put(%{box | y: 20}, :glyph_ids, [scriptscript])
      ]

      expected = String.duplicate(<<unicode::utf8>>, 3)

      for subset <- [true, false], compress <- [true, false] do
        assert {:ok, pdf} =
                 PdfWriter.render([%{size: {100, 100}, boxes: boxes}],
                   subset_fonts: subset,
                   compress_streams: compress
                 )

        assert {:ok, text} = Text.extract(pdf, layout: false)
        assert String.replace(text, " ", "") == expected
        assert {:ok, document} = Reader.read_validated(pdf)

        cid_streams =
          document.document.objects
          |> Map.values()
          |> Enum.filter(fn object ->
            case object do
              %{value: %{"Length" => _} = dict, stream: data} when is_binary(data) ->
                not Map.has_key?(dict, "Length1") and byte_size(data) == 8

              _ ->
                false
            end
          end)

        # The uncompressed mapping has reserved CID 0 and three distinct selected glyph IDs.
        if not compress do
          assert Enum.any?(cid_streams, fn object ->
                   data = object.stream
                   data == <<0::16, font.cmap[unicode]::16, script::16, scriptscript::16>>
                 end)
        end
      end
    end

    test "explicit glyphs bypass Latin shaping and preserve presentation ligatures literally", %{
      font: font
    } do
      text = "ffiﬃ"
      glyphs = Enum.map(String.to_charlist(text), &Map.fetch!(font.cmap, &1))
      literal = glyph_text_box(font, text) |> Map.put(:glyph_ids, glyphs)
      regular = %{glyph_text_box(font, "ffi") | y: 50}
      repeated = %{literal | y: 20} |> Map.put(:letter_spacing, 2)

      assert {:ok, pdf} =
               PdfWriter.render([%{size: {200, 100}, boxes: [regular, literal, repeated]}],
                 compress_streams: false
               )

      assert {:ok, "ffi ffiﬃ ffiﬃ"} = Text.extract(pdf, layout: false)
      assert pdf =~ "<FB03>"
      assert pdf =~ "2 Tc"

      # Explicit default glyphs reuse normal CIDs where Unicode extraction agrees.
      normal = glyph_text_box(font, "x")
      explicit = normal |> Map.put(:glyph_ids, [font.cmap[?x]]) |> Map.put(:y, 40)

      assert {:ok, pdf} =
               PdfWriter.render([%{size: {100, 100}, boxes: [normal, explicit]}],
                 compress_streams: false
               )

      assert pdf =~ "1 beginbfchar"
      assert {:ok, "x x"} = Text.extract(pdf, layout: false)

      assert {:ok, pdf} =
               PdfWriter.render(
                 [
                   %{
                     size: {100, 100},
                     boxes: [
                       Map.put(explicit, :snap_to_css_pixel_grid, true)
                       |> Map.put(:font_size, 10.3)
                     ]
                   }
                 ],
                 compress_streams: false
               )

      assert pdf =~ "] TJ"
      assert {:ok, "x"} = Text.extract(pdf, layout: false)

      assert {:ok, pdf} =
               PdfWriter.render([%{size: {100, 100}, boxes: [Map.put(explicit, :font_size, 0)]}])

      assert {:error, {:no_extractable_text, _}} = Text.extract(pdf)
      tiny = explicit |> Map.put(:font_size, 0.001) |> Map.put(:snap_to_css_pixel_grid, true)
      assert {:ok, pdf} = PdfWriter.render([%{size: {100, 100}, boxes: [tiny]}])
      assert {:error, {:no_extractable_text, _}} = Text.extract(pdf)

      snapped = explicit |> Map.put(:font_size, 30) |> Map.put(:snap_to_css_pixel_grid, true)

      assert {:ok, pdf} =
               PdfWriter.render([%{size: {100, 100}, boxes: [snapped]}], compress_streams: false)

      assert pdf =~ "/F1 40 Tf"
      assert pdf =~ "<0001> Tj"
      refute pdf =~ "] TJ"
      advance = Enum.at(font.widths, font.cmap[?x]) / font.units_per_em
      assert_in_delta 40 * 0.75 * advance, 30 * advance, 0.00001

      fractional = %{snapped | font_size: 10.3}

      assert {:ok, pdf} =
               PdfWriter.render([%{size: {100, 100}, boxes: [fractional]}],
                 compress_streams: false
               )

      [_, painted, adjustment] = Regex.run(~r{/F1 ([0-9.]+) Tf.*\[<0001> (-?[0-9.]+)\] TJ}, pdf)
      painted = String.to_float(painted)
      adjustment = String.to_float(adjustment)
      assert painted == 13.73
      shaped_size = Float.floor(10.3 / 0.75 * 64) / 64 * 0.75

      assert_in_delta painted * 0.75 * (advance - adjustment / 1000),
                      shaped_size * advance,
                      0.00001

      empty = %{explicit | text: "", glyph_ids: []}

      for empty <- [empty, Map.delete(empty, :glyph_ids)] do
        assert {:ok, pdf} = PdfWriter.render([%{size: {100, 100}, boxes: [empty]}])
        assert {:error, {:no_extractable_text, _}} = Text.extract(pdf)
      end
    end

    test "invalid glyph lists, unavailable outlines, and cumulative budgets return diagnostics",
         %{font: font, limits: limits} do
      box = glyph_text_box(font, "x")

      for glyphs <- [nil, [], [0], [-1], [65_536], ["1"], [1, 2], [length(font.widths)], [1.5]] do
        assert {:error, {:invalid_pdf_input, diagnostic}} =
                 PdfWriter.render([
                   %{size: {100, 100}, boxes: [Map.put(box, :glyph_ids, glyphs)]}
                 ])

        assert diagnostic.stage == :pdf
        assert diagnostic.reason == :invalid_pdf_input
        assert diagnostic.operation == :write_pdf
        assert is_binary(diagnostic.message)
      end

      for face <- [nil, %{type: :built_in, pdf_name: "Helvetica", family: "Helvetica"}] do
        builtin = %{box | font: "Helvetica", font_face: face} |> Map.put(:glyph_ids, [1])

        assert {:error, {:invalid_pdf_input, _}} =
                 PdfWriter.render([%{size: {100, 100}, boxes: [builtin]}])
      end

      explicit = Map.put(box, :glyph_ids, [font.cmap[?x]])
      invalid = %{explicit | font_face: %{font | data: <<>>}}

      assert {:error, {:invalid_pdf_input, %{message: message}}} =
               PdfWriter.render([%{size: {100, 100}, boxes: [invalid]}], subset_fonts: false)

      assert message =~ "TrueType"
      truncated = %{explicit | font_face: %{font | widths: [600]}}

      assert {:error, {:invalid_pdf_input, %{message: message}}} =
               PdfWriter.render([%{size: {100, 100}, boxes: [truncated]}])

      assert message =~ "width metrics"

      NativeElixirPdfUtilities.Limits.install(%{
        NativeElixirPdfUtilities.Limits.effective()
        | max_font_math_work: 1
      })

      assert {:error, {:resource_limit_exceeded, %{stage: :font, message: message}}} =
               PdfWriter.render([%{size: {100, 100}, boxes: [explicit]}])

      assert message =~ "max_font_math_work"
      NativeElixirPdfUtilities.Limits.install(%{limits | max_font_source_bytes: 1})

      assert {:error,
              {:resource_limit_exceeded, %{stage: :font, operation: :write_pdf, message: message}}} =
               PdfWriter.render([%{size: {100, 100}, boxes: [explicit]}])

      assert message =~ "max_font_source_bytes"
    end

    test "the fixed CID ceiling includes multiple glyph forms of the same Unicode character", %{
      font: font
    } do
      unicode = Enum.to_list(0x10000..0x1FFFE)
      face = %{font | cmap: Map.new(unicode, &{&1, font.cmap[?x]})}
      normal = glyph_text_box(face, List.to_string(unicode))
      alternate = glyph_text_box(face, <<0x10000::utf8>>) |> Map.put(:glyph_ids, [font.cmap[?y]])

      assert {:error, {:invalid_pdf_input, diagnostic}} =
               PdfWriter.render([%{size: {100, 100}, boxes: [normal, alternate]}])

      assert diagnostic.message =~ "65,535 CID"
      assert diagnostic.operation == :write_pdf
    end
  end

  test "spaced and unspaced text share a font without changing their glyph sequences" do
    assert {:ok, registry} = Font.load_registry([])
    assert {:ok, _, font} = Font.resolve("DejaVu Sans", 400, :normal, registry)

    for kerning <- [font.kerning, %{}] do
      font = %{font | kerning: kerning}

      box = %{
        type: :text,
        text: "ffi",
        x: 10.0,
        y: 80.0,
        font: Font.pdf_name(font),
        font_face: font,
        font_size: 20.0,
        color: {0, 0, 0},
        letter_spacing: 4.0
      }

      plain = %{box | y: 40.0, letter_spacing: 0.0}
      assert {:ok, pdf} = PdfWriter.render([%{size: {200, 100}, boxes: [box, plain]}], [])
      assert pdf =~ "4 Tc"
      assert pdf =~ ~r/<[0-9A-F]{4}> <0066>/
      assert pdf =~ ~r/<[0-9A-F]{4}> <0069>/
      assert pdf =~ ~r/<[0-9A-F]{4}> <006600660069>/
      assert {:ok, "ffi ffi"} = Text.extract(pdf, layout: false)
    end
  end

  test "interleaved builtin and embedded runs preserve spacing and explicit glyph resources" do
    assert {:ok, registry} = Font.load_registry(system_font_discovery: false)
    assert {:ok, _, font} = Font.resolve("DejaVu Sans", 400, :normal, registry)
    normal = glyph_text_box(font, "ffi")

    builtin = %{
      type: :text,
      text: "First",
      x: 10,
      y: 100,
      font: "Helvetica",
      font_size: 20,
      color: {0, 0, 0}
    }

    repeated =
      %{builtin | text: "Last", y: 20}
      |> Map.put(:font_face, %{type: :built_in, family: "Helvetica", pdf_name: "Helvetica"})
      |> Map.put(:letter_spacing, 2)

    literal =
      %{normal | y: 40}
      |> Map.put(:glyph_ids, Enum.map(~c"ffi", &Map.fetch!(font.cmap, &1)))
      |> Map.put(:letter_spacing, 1)

    for subset <- [true, false], compress <- [true, false] do
      assert {:ok, pdf} =
               PdfWriter.render(
                 [%{size: {200, 120}, boxes: [builtin, normal, repeated, literal]}],
                 subset_fonts: subset,
                 compress_streams: compress
               )

      assert {:ok, "First ffi Last ffi"} = Text.extract(pdf, layout: false)
      assert {:ok, context} = Reader.read_validated(pdf)

      assert {:ok, resources} =
               Reader.dictionary(
                 context.document,
                 context.pages |> hd() |> Map.fetch!(:resources)
               )

      assert {:ok, fonts} = Reader.dictionary(context.document, resources["Font"])
      assert map_size(fonts) == 2
      assert pdf =~ "/BaseFont /Helvetica"
      assert pdf =~ "/Subtype /Type0"

      unless compress do
        assert pdf =~ "2 Tc (Last) Tj 0 Tc"
        assert pdf =~ "1 Tc"
      end
    end
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

    assert Regex.scan(~r/(\d+) beginbfchar/, decoded_pdf_streams(pdf), capture: :all_but_first) ==
             [
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

  defp decoded_pdf_streams(pdf) do
    assert {:ok, document} = Reader.read(pdf)

    document.objects
    |> Enum.flat_map(fn {ref, object} ->
      case object.stream do
        stream when is_binary(stream) ->
          assert {:ok, decoded} = Reader.decoded_stream(document, {:ref, ref})
          [decoded]

        _ ->
          []
      end
    end)
    |> Enum.join("\n")
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
      Paths.project("priv/fonts/dejavu/DejaVuSans.ttf"),
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

  test "fieldset relief strips preserve author colors, transparency and contrasting edges" do
    for color <- [{0, 0, 0}, {0, 0, 0.1}, {1, 1, 1}, {0.9, 0.9, 0.9, 0.4}],
        style <- [:groove, :ridge, :inset, :outset, :double] do
      box = %{
        type: :rect,
        role: :fieldset,
        x: 10,
        y: 20,
        width: 80,
        height: 30,
        fill_color: {1, 1, 0},
        stroke_color: color,
        stroke_width: 3,
        border_radius: 0,
        border_widths: %{top: 3, right: 3, bottom: 3, left: 3},
        border_styles: %{top: style, right: style, bottom: style, left: style},
        border_cutout: %{x: 5, y: -3, width: 20, height: 3}
      }

      assert {:ok, pdf} =
               PdfWriter.render([%{size: {120, 100}, boxes: [box]}], compress_streams: false)

      assert pdf =~ "W* n" and pdf =~ "1 1 0 rg"
      if tuple_size(color) == 4, do: assert(pdf =~ "/ca 0.4")
    end
  end

  test "border cutouts preserve rectangle backgrounds and validate box-relative clipping geometry" do
    box = %{
      type: :rect,
      x: 10,
      y: 20,
      width: 80,
      height: 30,
      fill_color: {1, 1, 0},
      stroke_color: {1, 0, 0},
      stroke_width: 2,
      border_radius: 0,
      border_cutout: %{x: 5, y: -2, width: 20, height: 2}
    }

    for style <- [:solid, :groove, :dashed] do
      box = Map.put(box, :border_styles, %{top: style, right: style, bottom: style, left: style})
      box = Map.put(box, :border_widths, %{top: 2, right: 2, bottom: 2, left: 2})

      assert {:ok, pdf} =
               PdfWriter.render([%{size: {120, 100}, boxes: [box]}], compress_streams: false)

      assert pdf =~ "15 48 20 2 re W* n"
      assert pdf =~ "1 1 0 rg"
      assert {:ok, 1} = NativeElixirPdfUtilities.Info.page_count(pdf)
    end

    for cutout <- [
          %{x: 0, y: 0, width: 0, height: 1},
          %{x: 0, y: 0, width: 1, height: -1},
          %{x: "bad", y: 0, width: 1, height: 1},
          %{x: 0, y: 0, width: 1, height: 1, extra: true},
          :bad
        ] do
      assert {:error, {:invalid_pdf_input, diagnostic}} =
               PdfWriter.render([
                 %{size: {120, 100}, boxes: [Map.put(box, :border_cutout, cutout)]}
               ])

      assert diagnostic.stage == :pdf and diagnostic.operation == :write_pdf
      assert diagnostic.message =~ "border_cutout"
    end
  end
end
