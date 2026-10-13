defmodule NativeElixirPdfUtilities.HtmlToPdf.SvgTextRotationTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.{HtmlToPdf, Text, Limits}
  alias NativeElixirPdfUtilities.HtmlToPdf.{SvgBackend, HtmlParser, Style, SvgScene}
  alias NativeElixirPdfUtilities.Validators.{HtmlValidator, SvgValidator}

  setup do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    %{data: File.read!("priv/fonts/dejavu/DejaVuSans.ttf"), limits: limits}
  end

  test "each original Unicode character keeps actual rotated selected-glyph geometry", %{
    data: data
  } do
    source =
      "<svg width='240' height='120'><text x='20 60 100 140' y='50' font-size='20' font-family='NativeSvgFont0' rotate='0,30 -30 180'>AΩéB</text></svg>"

    assert {:ok, pdf} = SvgBackend.vectorize(source, [], %{mode: :inline, fonts: [%{data: data}]})
    assert {:ok, "AΩéB"} = Text.extract(pdf, layout: false)
    assert {:ok, %{pages: [%{spans: spans}]}} = Text.extract_spans(pdf)
    assert Enum.map(spans, & &1.text) == ["A", "Ω", "é", "B"]
    assert Enum.all?(spans, &(&1.paints_text? and &1.font_size == 20))
    [a, omega, e, b] = spans
    assert_in_delta a.y, a.end_y, 0.001
    assert omega.end_y > omega.y
    assert e.end_y < e.y
    assert b.end_x < b.x
    assert {:ok, doc} = NativeElixirPdfUtilities.Pdf.Reader.read(pdf)
    assert Enum.any?(Map.values(doc.objects), &(Map.get(&1, :stream) == data))
    refute pdf =~ "3 Tr"
  end

  test "host plans normalize large authored turns before native float conversion" do
    html =
      "<svg width=220 height=100><text x=20 y=50 font-size=20 rotate='360000030 -360000030'>AB</text></svg>"

    assert {:ok, dom} = HtmlParser.parse_detailed(html)
    assert {:ok, %{children: [svg]}} = Style.prepare_svg_detailed(dom)
    assert {:ok, scene} = SvgScene.prepare(svg, HtmlValidator.new_image_budget())
    assert scene.source =~ "rotate=\"30.0 330.0\""
    assert {:ok, pdf} = HtmlToPdf.render(html)
    assert {:ok, "AB"} = Text.extract(pdf, layout: false)

    assert {:ok, canonical} =
             HtmlToPdf.render(String.replace(html, "360000030 -360000030", "30 -30"))

    assert {:ok, original_spans} = Text.extract_spans(pdf)
    assert {:ok, canonical_spans} = Text.extract_spans(canonical)
    assert original_spans == canonical_spans
  end

  test "raw angles require canonical semantics and reject units before both paint modes", %{
    data: data
  } do
    for {value, reason} <- [
          {"361", :unsupported_html},
          {"1deg", :invalid_document},
          {"NaN", :invalid_document},
          {"0,,1", :invalid_document}
        ] do
      source =
        "<svg width='200' height='80'><text y='40' rotate='#{value}' font-family='NativeSvgFont0'>AB</text></svg>"

      for operation <- [:vectorize, :rasterize] do
        assert {:error, {^reason, d}} =
                 apply(SvgBackend, operation, [
                   source,
                   [],
                   %{mode: :inline, fonts: [%{data: data}]}
                 ])

        assert d.stage == :layout
        assert d.source == "text rotate"
        assert d.module == NativeElixirPdfUtilities.Validators.SvgTextPositionValidator
      end
    end
  end

  test "angle boundaries inside a shaping cluster diagnose instead of dropping scalar positions",
       %{data: data} do
    for content <- [
          "<text y='40' rotate='0 15 30' font-family='NativeSvgFont0'>ffi</text>",
          "<text y='40' rotate='0 15 30' font-family='NativeSvgFont0'>á̈</text>"
        ] do
      source = "<svg width='200' height='80'>#{content}</svg>"

      for operation <- [:vectorize, :rasterize] do
        assert {:error, {:invalid_document, d}} =
                 apply(SvgBackend, operation, [
                   source,
                   [],
                   %{mode: :inline, fonts: [%{data: data}]}
                 ])

        assert d.message =~ "rotations inside a shaped Unicode cluster"
        assert d.stage == :layout
      end
    end
  end

  test "unused authored rotation entries and expanded instances charge the shared complexity ceiling",
       %{limits: limits} do
    source =
      "<svg width='200' height='80'><defs><text id='label' font-family='NativeSvgFont0' rotate='0 1 2 3'>A</text></defs><use href='#label'/><use href='#label'/></svg>"

    assert {:ok, summary} = SvgValidator.validate_processing(source, %{mode: :inline})
    assert summary.expanded_position_work == 12
    Limits.install(Map.put(limits, :max_svg_expansion_work, 3))

    assert {:error, {:resource_limit_exceeded, d}} =
             SvgValidator.validate_processing(source, %{mode: :inline})

    assert d.message =~ "max_svg_expansion_work"
  end
end
