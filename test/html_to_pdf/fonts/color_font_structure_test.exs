Code.require_file(NativeElixirPdfUtilities.TestSupport.Paths.support("font_fixture.ex"))

defmodule NativeElixirPdfUtilities.HtmlToPdf.ColorFontStructureTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.TestSupport.FontCorpus
  alias NativeElixirPdfUtilities.{Limits, Text}
  alias NativeElixirPdfUtilities.HtmlToPdf.{Font, PdfWriter}
  alias NativeElixirPdfUtilities.TestSupport.{Paths, FontFixture}

  alias NativeElixirPdfUtilities.Validators.{
    ColrFontValidator,
    ColrPaintValidator,
    SvgFontValidator
  }

  setup do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    data = File.read!(FontCorpus.path("selected-colr.ttf"))
    {:ok, [font]} = Font.load_configured_faces([%{family: "Color", data: data}])
    {:ok, font: font}
  end

  test "CPAL versions, palette fallback, optional suitability arrays and malformed owners", %{
    font: font
  } do
    {_, tables} = FontFixture.tables(font.data)
    cpal = <<0::16, 1::16, 1::16, 1::16, 14::32, 0::16, 0, 0, 255, 255>>
    assert ColrFontValidator.prepare_palette(cpal, 99) == {:ok, {{1.0, 0.0, 0.0, 1.0}}}
    v1 = <<1::16, 1::16, 1::16, 1::16, 26::32, 0::16, 0::32, 0::32, 0::32, 0, 0, 255, 255>>
    assert ColrFontValidator.prepare_palette(v1, :light) == {:ok, {{1.0, 0.0, 0.0, 1.0}}}

    for cpal <- [
          <<>>,
          FontFixture.replace(cpal, 0, <<2::16>>),
          FontFixture.replace(cpal, 12, <<1::16>>),
          FontFixture.replace(v1, 14, <<26::32>>) <> <<4::32>>
        ] do
      assert {:error, {:invalid_font, _}} = ColrFontValidator.prepare_palette(cpal, :normal)
    end

    colr = tables["COLR"]
    <<_::binary-size(4), base::32, _::binary>> = colr

    for changes <- [
          %{"COLR" => <<>>},
          %{"CPAL" => nil},
          %{"COLR" => FontFixture.replace(colr, base, <<65535::16>>)},
          %{"COLR" => FontFixture.replace(colr, base + 2, <<65535::16>>)}
        ] do
      changed = %{font | data: FontFixture.rebuild(font.data, changes)}

      assert {:error, {:invalid_font, %{stage: :font, operation: :prepare_color_font}}} =
               ColrFontValidator.prepare(changed, font.cmap[?H], {0, 0, 0, 1})
    end
  end

  test "COLR forward offsets, layers, color lines and singular transforms are validated before painting",
       %{font: font} do
    id = font.cmap[?H]

    for paint <- [
          <<1, 1, 0::32>>,
          <<2, 100::16, 16384::16>>,
          <<10, 0::24, id::16>>,
          <<28, 8::24, 8192::signed-16, 0::16, 2, 0::16, 16384::16>>,
          <<4, 16::24, 0::96, 3, 0::16>>
        ] do
      changed = %{
        font
        | data: FontFixture.rebuild(font.data, %{"COLR" => version_one(id, paint)})
      }

      assert {:error, {:invalid_font, diagnostic}} =
               ColrFontValidator.prepare(changed, id, {0, 0, 0, 1})

      assert diagnostic.message != ""
    end

    changed = %{
      font
      | data: FontFixture.rebuild(font.data, %{"COLR" => version_one(id, <<255>>)})
    }

    assert {:ok, %{graph: :empty} = paint} = ColrFontValidator.prepare(changed, id, {0, 0, 0, 1})
    assert {:ok, "", _, _} = ColrPaintValidator.prepare(changed, paint)
    # The absent LayerList is valid when the selected paint does not use layers.
    changed = %{
      font
      | data:
          FontFixture.rebuild(font.data, %{"COLR" => version_one(id, <<2, 0::16, 16384::16>>)})
    }

    assert {:ok, %{graph: {:solid, _}}} = ColrFontValidator.prepare(changed, id, {0, 0, 0, 1})
    data = File.read!(FontCorpus.path("selected-colr-paints.ttf"))
    assert {:ok, [variable]} = Font.load_configured_faces([%{family: "Paints", data: data}])
    {_, tables} = FontFixture.tables(variable.data)
    colr = tables["COLR"]
    # Omit the optional index map: implicit outer/inner variation indices remain valid.
    changed = %{
      variable
      | data:
          FontFixture.rebuild(variable.data, %{"COLR" => FontFixture.replace(colr, 26, <<0::32>>)})
    }

    for {_unicode, glyph} <- changed.cmap do
      assert {:ok, _} = ColrFontValidator.prepare(changed, glyph, {0, 0, 0, 1})
    end

    Limits.install(%{max_font_color_work: 1})

    assert {:error, {:resource_limit_exceeded, _}} =
             ColrFontValidator.prepare(changed, id, {0, 0, 0, 1})
  end

  test "SVG directory ownership, compressed input and palette substitution failures stay recoverable",
       %{font: font} do
    id = font.cmap[?H]

    svg =
      "<svg xmlns='http://www.w3.org/2000/svg'><g id='glyph#{id}'><rect width='100' height='100' fill='red'/></g></svg>"

    for payload <- [
          svg,
          :zlib.gzip(svg <> "<!--" <> String.duplicate("padding", 10000) <> "-->"),
          "<svg xmlns='http://www.w3.org/2000/svg'><g id='glyph#{id}'>text<rect width='100' height='100'/></g></svg>"
        ] do
      changed = %{
        font
        | data: FontFixture.rebuild(font.data, %{"SVG " => svg_table(id, payload), "CPAL" => nil})
      }

      assert {:ok, %{bounds: _}} = SvgFontValidator.prepare(changed, id, {0, 0, 0, 1})
    end

    table = svg_table(id, svg)

    for table <- [
          <<>>,
          FontFixture.replace(table, 0, <<1::16>>),
          FontFixture.replace(table, 12, <<65535::16>>),
          svg_table(
            id,
            "<svg xmlns='http://www.w3.org/2000/svg'><g id='glyph#{id}'><rect fill='var(--missing)' width='10' height='10'/></g></svg>"
          ),
          svg_table(id, <<31, 139, 8, 0, 0, 0, 0, 0, 0, 0, 1, 2, 3>>)
        ] do
      changed = %{font | data: FontFixture.rebuild(font.data, %{"SVG " => table})}

      assert {:error, {_, %{message: message}}} =
               SvgFontValidator.prepare(changed, id, {0, 0, 0, 1})

      assert message != ""
    end

    changed = %{
      font
      | data:
          FontFixture.rebuild(font.data, %{
            "COLR" => nil,
            "SVG " =>
              svg_table(id, "<svg xmlns='http://www.w3.org/2000/svg'><g id='glyph#{id}'/></svg>")
          })
    }

    assert {:ok, :empty} = SvgFontValidator.prepare(changed, id, {0, 0, 0, 1})

    box = %{
      type: :text,
      text: "H",
      font: Font.pdf_name(changed),
      font_face: changed,
      x: 10,
      y: 10,
      font_size: 12,
      color: {0, 0, 0, 0.5}
    }

    assert {:ok, pdf} = PdfWriter.render([%{size: {80, 80}, boxes: [box]}])
    assert {:ok, "H"} = Text.extract(pdf, layout: false)
  end

  test "bounded COLR layers retain empty, singular and unbounded child behavior", %{font: font} do
    solid = {:solid, {1, 0, 0, 1}}
    glyph = {:glyph, font.cmap[?H], solid}

    for graph <- [
          {:layers, [:empty, glyph]},
          {:layers, [{:transform, [0, 0, 0, 0, 0, 0], glyph}, glyph]},
          {:layers, [{:transform, [1, 0, 0, 1, 0, 0], solid}, glyph]},
          {:layers, [{:composite, 0, glyph, glyph}, glyph]},
          {:layers, [glyph, solid, glyph]}
        ] do
      assert {:ok, source, _, _} =
               ColrPaintValidator.prepare(font, %{graph: graph, clip: {0, 0, 600, 800}})

      assert source =~ "<svg"
    end

    radial =
      {:glyph, font.cmap[?H],
       {:gradient, 6, [0, 0, 0, 100, 100, 100], {0, [{0, {1, 0, 0, 1}}, {1, {0, 0, 1, 1}}]}}}

    Limits.install(Map.put(Limits.effective(), :max_svg_raster_dimension, 1))
    result = ColrPaintValidator.prepare(font, %{graph: radial, clip: nil}, 100)
    assert {:error, {_, _}} = result
  end

  test "SVG image and CSS-variable resource failures propagate without decoding external resources",
       %{font: font} do
    id = font.cmap[?H]

    payload =
      "<svg xmlns='http://www.w3.org/2000/svg'><g id='glyph#{id}'><image width='1' height='1' href='data:image/png;base64,AAAA'/></g></svg>"

    changed = %{font | data: FontFixture.rebuild(font.data, %{"SVG " => svg_table(id, payload)})}
    limits = Limits.effective()
    Limits.install(Map.put(limits, :max_aggregate_image_source_bytes, 1))

    assert {:error, {:resource_limit_exceeded, _}} =
             SvgFontValidator.prepare(changed, id, {0, 0, 0, 1})

    Limits.install(Map.put(limits, :max_css_variable_bytes, 1))

    payload =
      "<svg xmlns='http://www.w3.org/2000/svg'><g id='glyph#{id}'><rect width='10' height='10' fill='var(--color0)'/></g></svg>"

    changed = %{font | data: FontFixture.rebuild(font.data, %{"SVG " => svg_table(id, payload)})}

    assert {:error, {:resource_limit_exceeded, _}} =
             SvgFontValidator.prepare(changed, id, {0, 0, 0, 1})
  end

  test "COLR base and clip ranges, missing deltas and no-variation sentinels preserve table ownership" do
    data = File.read!(FontCorpus.path("selected-colr-paints.ttf"))
    manifest = File.read!(Paths.fixture("fonts/colr-paints.json")) |> :json.decode()
    assert {:ok, [font]} = Font.load_configured_faces([%{family: "Paints", data: data}])
    {_, tables} = FontFixture.tables(font.data)
    colr = tables["COLR"]
    <<base::32>> = binary_part(colr, 14, 4)
    <<clips::32>> = binary_part(colr, 22, 4)

    for changed <- [
          FontFixture.replace(colr, base + 4, <<65535::16>>),
          FontFixture.replace(colr, clips + 5, <<65535::16>>)
        ] do
      assert {:error, {:invalid_font, _}} =
               ColrFontValidator.prepare(
                 %{font | data: FontFixture.rebuild(font.data, %{"COLR" => changed})},
                 2,
                 {0, 0, 0, 1}
               )
    end

    id = manifest["glyphs"]["paint3"]
    <<count::32>> = binary_part(colr, base, 4)

    records =
      for <<glyph::16, relative::32 <- binary_part(colr, base + 4, count * 6)>>,
        do: {glyph, base + relative}

    paint = elem(Enum.find(records, &(elem(&1, 0) == id)), 1)

    <<mapping::32>> = binary_part(colr, 26, 4)
    invalid_map = FontFixture.replace(colr, mapping + 4, <<255>>)

    assert {:error, {:invalid_font, _}} =
             ColrFontValidator.prepare(
               %{font | data: FontFixture.rebuild(font.data, %{"COLR" => invalid_map})},
               id,
               {0, 0, 0, 1}
             )

    for {index, expected} <- [{0xFFFFFFFF, :ok}, {0xFFFF0000, :error}] do
      changed = FontFixture.replace(colr, paint + 5, <<index::32>>)

      result =
        ColrFontValidator.prepare(
          %{font | data: FontFixture.rebuild(font.data, %{"COLR" => changed})},
          id,
          {0, 0, 0, 1}
        )

      assert elem(result, 0) == expected
    end
  end

  test "color outline and selected metrics failures retain the admitted writer diagnostics", %{
    font: font
  } do
    alias NativeElixirPdfUtilities.Validators.{ColorFontValidator, TrueTypeSubsetValidator}
    assert {:error, {:invalid_document, _}} = TrueTypeSubsetValidator.prepare_outlines(nil)
    {_, tables} = FontFixture.tables(font.data)
    bytes = <<1::16, 0::32, 100::16, 100::16, 0::16, 0::16, 128, 0>>

    loca =
      <<0::32, 0::32, 0::32, byte_size(bytes)::32, byte_size(bytes)::32, byte_size(bytes)::32,
        byte_size(bytes)::32>>

    changed = %{
      font
      | data:
          FontFixture.rebuild(font.data, %{
            "glyf" => bytes,
            "loca" => loca,
            "head" => FontFixture.replace(tables["head"], 50, <<1::16>>)
          })
    }

    assert {:error, {:invalid_font, _}} = TrueTypeSubsetValidator.prepare_outlines(changed.data)

    assert {:error, {:invalid_font, _}} =
             ColrPaintValidator.prepare(changed, %{
               graph: {:glyph, font.cmap[?H], {:solid, {1, 0, 0, 1}}},
               clip: nil
             })

    assert {:ok, %{pages: [%{boxes: [%{type: :rect}]}]}} =
             ColorFontValidator.prepare(
               %{color_fonts?: true, pages: [%{size: {80, 80}, boxes: [%{type: :rect}]}]},
               []
             )

    for {source, changes} <- [
          {"selected-colr.ttf", %{"COLR" => <<>>}},
          {"selected-svg.ttf", %{"SVG " => <<>>}},
          {"selected-cbdt.ttf", %{"CBLC" => <<>>}}
        ] do
      data = File.read!(FontCorpus.path(source))
      assert {:ok, [font]} = Font.load_configured_faces([%{family: "Broken", data: data}])
      font = %{font | data: FontFixture.rebuild(data, changes)}

      box = %{
        type: :text,
        text: "H",
        x: 10,
        y: 20,
        font: Font.pdf_name(font),
        font_face: font,
        font_size: 12,
        color: {0, 0, 0}
      }

      assert {:error, {:invalid_font, _}} = PdfWriter.render([%{size: {80, 80}, boxes: [box]}])
    end
  end

  test "named palette overrides and sweep-stop sampling consume their full color-work allowance",
       %{font: font} do
    limits = Limits.effective()
    cpal = <<0::16, 1::16, 1::16, 1::16, 14::32, 0::16, 0, 0, 255, 255>>
    Limits.install(Map.put(limits, :max_font_color_work, 3))

    assert {:error, {:resource_limit_exceeded, %{message: message}}} =
             ColrFontValidator.prepare_palette(cpal, %{
               base: :normal,
               overrides: %{0 => {1, 0, 0, 1}, 1 => {0, 1, 0, 1}}
             })

    assert message =~ "max_font_color_work"
    segments = Limits.get(:font_color_sweep_segments)
    Limits.install(Map.put(limits, :max_font_color_work, segments + 10))

    gradient =
      {:glyph, font.cmap[?H],
       {:gradient, 8, [300, 350, 0, 1], {0, [{0, {1, 0, 0, 1}}, {1, {0, 0, 1, 1}}]}}}

    assert {:error, {:resource_limit_exceeded, %{message: message}}} =
             ColrPaintValidator.prepare(font, %{graph: gradient, clip: nil})

    assert message =~ "max_font_color_work"
  end

  defp version_one(id, paint) do
    <<1::16, 0::16, 0::32, 0::32, 0::16, 34::32, 0::32, 0::32, 0::32, 0::32, 1::32, id::16,
      10::32, paint::binary>>
  end

  defp svg_table(id, source) do
    <<0::16, 10::32, 0::32, 1::16, id::16, id::16, 14::32, byte_size(source)::32, source::binary>>
  end
end
