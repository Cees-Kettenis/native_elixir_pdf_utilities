Code.require_file(NativeElixirPdfUtilities.TestSupport.Paths.support("open_type_fixture.ex"))

defmodule NativeElixirPdfUtilities.HtmlToPdf.OpenTypeVerticalTest do
  use ExUnit.Case, async: true
  alias NativeElixirPdfUtilities.HtmlToPdf.{Font, TextShaper}
  alias NativeElixirPdfUtilities.TestSupport.{FontFixture, OpenTypeFixture}
  alias NativeElixirPdfUtilities.Validators.OpenTypeVerticalValidator, as: Validator

  test "absent vertical tables synthesize advances and glyph-specific origins matching HarfBuzz" do
    path = NativeElixirPdfUtilities.TestSupport.Paths.project("priv/fonts/dejavu/DejaVuSans.ttf")

    assert {:ok, [font]} =
             Font.load_configured_faces([%{family: "Vertical fallback", path: path}])

    assert {:ok, shaped} = TextShaper.shape("AB", font, direction: :ttb)
    # Independently measured with system libharfbuzz on the unchanged bundled
    # face: horizontal ascender minus descender is 2384, rather than one em.
    assert Enum.map(
             shaped.glyphs,
             &{&1.glyph, &1.x_advance, &1.y_advance, &1.x_offset, &1.y_offset}
           ) == [
             {36, 0, -2384, -700, -1938},
             {37, 0, -2384, -702, -1938}
           ]

    assert shaped.vertical_advance == -4768
    assert {:ok, points} = Font.css_glyph_positions("AB", font, 12, 0, direction: :ttb)
    assert_in_delta points.vertical_advance, -27.9375, 0.0001
  end

  test "vertical alternates, origins and positive-height kerning match an independent shaping oracle" do
    data = OpenTypeFixture.vertical_font()
    assert byte_size(data) < 7000

    assert {:ok, [font]} =
             Font.load_configured_faces([%{family: "Authored vertical", data: data}])

    # System libharfbuzz shaped the same test-owned bytes with direction TTB.
    # Font SHA256: 00bc3842b6af635b6bbc9c0d6344a9fdd3b27c5048a7dc311abe4a9513912cb5
    # vkrn is explicitly enabled in the oracle, as required when CSS font
    # kerning is enabled. CSS Writing Modes 4 does not enable vrt2 by default.
    for {text, expected} <- [
          {"AB", [{4, 0, -1280, -500, -750}, {2, 0, -1200, -400, -770}]},
          {"A", [{4, 0, -1200, -500, -750}]},
          {"BC", [{2, 0, -1200, -400, -770}, {3, 0, -1200, -250, -750}]}
        ] do
      assert {:ok, shaped} = TextShaper.shape(text, font, direction: :ttb)

      assert Enum.map(
               shaped.glyphs,
               &{&1.glyph, &1.x_advance, &1.y_advance, &1.x_offset, &1.y_offset}
             ) == expected

      assert shaped.advance == 0
      assert shaped.vertical_advance == Enum.sum(Enum.map(expected, &elem(&1, 2)))
      assert shaped.text == text
    end

    assert {:ok, unkerned} =
             TextShaper.shape("AB", font, direction: :ttb, features: %{"kern" => 0})

    assert Enum.map(unkerned.glyphs, & &1.y_advance) == [-1200, -1200]
    assert {:ok, rotated} = TextShaper.shape("A", font, direction: :ttb, features: %{"vrt2" => 1})
    assert hd(rotated.glyphs).glyph == 5
    assert {:ok, points} = Font.css_glyph_positions("AB", font, 12, 0, direction: :ttb)
    assert_in_delta points.vertical_advance, -29.76, 0.0001
    assert_in_delta hd(points.glyphs).x_offset, -6.0, 0.0001
    assert_in_delta hd(points.glyphs).y_offset, -9.0, 0.0001
  end

  test "VORG uses sorted CFF overrides and is ignored for TrueType outlines" do
    source = OpenTypeFixture.vertical_font()
    {_, tables} = FontFixture.tables(source)

    origins =
      <<1::16, 0::16, 800::signed-16, 2::16, 1::16, 760::signed-16, 2::16, 830::signed-16>>

    tables = Map.put(tables, "VORG", origins)
    assert {:ok, vertical} = Validator.prepare(tables, 64)
    assert vertical.header == %{ascent: 500, descent: -500, line_gap: 0}
    assert elem(vertical.metrics, 1) == {900, 30}
    assert elem(vertical.metrics, 63) == {1200, 50}
    assert {:ok, resolved} = Validator.origins(%{}, vertical)
    assert Enum.map([0, 1, 2, 63], &elem(resolved, &1)) == [800, 760, 830, 800]
    assert {:ok, ignored} = Validator.prepare(Map.put(tables, :true_type?, true), 64)
    assert ignored.origins == nil

    assert {:ok, %{metrics: nil, origins: nil}} =
             Validator.prepare(%{"VORG" => <<0>>, true_type?: true}, 64)

    assert {:ok, empty} = Validator.prepare(%{}, 64)
    assert {:ok, nil} = Validator.origins(%{}, empty)
  end

  test "malformed vertical tables return font-owned diagnostics before shaping" do
    source = OpenTypeFixture.vertical_font()
    {_, tables} = FontFixture.tables(source)
    header = tables["vhea"]
    origins = <<1::16, 0::16, 800::signed-16, 1::16, 1::16, 760::signed-16>>

    for change <- [
          %{"vhea" => nil},
          %{"vmtx" => nil},
          %{"vhea" => <<0>>},
          %{"vhea" => FontFixture.replace(header, 0, <<0::32>>)},
          %{"vhea" => FontFixture.replace(header, 24, <<1::16>>)},
          %{"vhea" => FontFixture.replace(header, 32, <<1::16>>)},
          %{"vhea" => FontFixture.replace(header, 34, <<0::16>>)},
          %{"vhea" => FontFixture.replace(header, 34, <<65::16>>)},
          %{"vmtx" => <<1::16>>},
          %{"VORG" => <<0>>},
          %{"VORG" => FontFixture.replace(origins, 0, <<2::16>>)},
          %{"VORG" => FontFixture.replace(origins, 6, <<65::16>>)},
          %{"VORG" => FontFixture.replace(origins, 8, <<64::16>>)},
          %{
            "VORG" =>
              <<1::16, 0::16, 800::signed-16, 2::16, 2::16, 800::signed-16, 1::16,
                810::signed-16>>
          }
        ] do
      assert {:error, {:invalid_font, diagnostic}} =
               Validator.prepare(Map.merge(tables, change), 64)

      assert diagnostic.stage == :font
      assert diagnostic.operation == :prepare_vertical_metrics
      assert diagnostic.module == Validator
      assert diagnostic.source =~ ~r/vhea|vmtx|VORG/
    end

    data = FontFixture.rebuild(source, %{"vmtx" => <<0>>})
    assert {:ok, [font]} = Font.load_configured_faces([%{family: "Broken vertical", data: data}])
    assert {:error, {:invalid_font, diagnostic}} = TextShaper.shape("A", font, direction: :ttb)
    assert diagnostic.source =~ "vmtx"
  end

  test "a GPOS height advance moves following paint even when nominal glyph selection is unchanged" do
    alias NativeElixirPdfUtilities.HtmlToPdf.{PdfWriter, ShapedTextPaint}

    single =
      OpenTypeFixture.join(
        fn [covered] -> OpenTypeFixture.words([1, covered, 8, 80]) end,
        [OpenTypeFixture.coverage([1])]
      )

    data =
      OpenTypeFixture.outline_font([], [%{type: 1, subtables: [single]}],
        gpos_features: [{"test", [0]}]
      )

    assert {:ok, [font]} = Font.load_configured_faces([%{family: "Height advance", data: data}])
    font = Map.put(font, :feature_plan, %{features: %{"test" => 1}})

    box = %{
      type: :text,
      text: "AB",
      x: 10.0,
      y: 50.0,
      font_size: 12.0,
      font: Font.pdf_name(font),
      font_face: font,
      color: {0, 0, 0}
    }

    page = %{size: {100, 100}, boxes: [box]}
    assert {:ok, [paint], _faces} = ShapedTextPaint.prepare([page])
    [first, second] = Enum.filter(paint.boxes, &(&1.type == :text))
    assert_in_delta second.y - first.y, -0.96, 0.0001
    assert {:ok, pdf} = PdfWriter.render([page])
    assert {:ok, "AB"} = NativeElixirPdfUtilities.Text.extract(pdf, layout: false)
  end
end
