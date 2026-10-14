defmodule NativeElixirPdfUtilities.HtmlToPdf.CssGlyphAdvanceTest do
  use ExUnit.Case
  alias NativeElixirPdfUtilities.HtmlToPdf.{Font, PdfWriter}
  alias NativeElixirPdfUtilities.{Limits, Text}
  alias NativeElixirPdfUtilities.Validators.HtmlValidator
  @font Path.expand("../fixtures/html_to_pdf/browser_parity/assets/cjk-ruby-fixture.ttf", __DIR__)

  setup do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)

    {:ok, registry} =
      Font.load_registry(
        fonts: [%{family: "CJK Ruby Fixture", path: @font}],
        system_font_discovery: false
      )

    {:ok, _, cjk} = Font.resolve("CJK Ruby Fixture", 400, :normal, registry)
    {:ok, _, latin} = Font.resolve("DejaVu Sans", 400, :normal, registry)
    {:ok, cjk: cjk, latin: latin}
  end

  test "real selected-font CSS pen advances retain original glyph IDs and design widths", %{
    cjk: font
  } do
    expected = 1_048_578 / 65_536 * 0.75
    plan = Font.css_glyph_advances("東京京都", font, 12)
    assert Enum.map(plan, &elem(&1, 0)) == String.to_charlist("東京京都")

    for {codepoint, glyph, advance} <- plan do
      assert glyph == font.cmap[codepoint]
      assert Enum.at(font.widths, glyph) == 1000
      assert advance == expected
    end

    assert Font.text_width("東京京都", font, 12) == 48.0
    assert Enum.sum(Enum.map(plan, &elem(&1, 2))) == Font.css_text_width("東京京都", font, 12)
  end

  test "the shared plan preserves ligatures, kerning, letter-spacing shaping and size quantization",
       %{latin: font} do
    for text <- ["AV", "office", "ffi", "word"],
        spacing <- [0, 2],
        size <- [0.375, 10.5, 12.0, 13.0] do
      plan = Font.css_glyph_advances(text, font, size, spacing)

      assert Enum.map(plan, &elem(&1, 0)) ==
               String.to_charlist(Font.shape_ligatures(text, font, spacing))

      assert Enum.all?(plan, fn {cp, gid, _} -> font.cmap[cp] == gid end)

      assert_in_delta Enum.sum(Enum.map(plan, &elem(&1, 2))),
                      Font.css_text_width(text, font, size, spacing),
                      1.0e-9
    end
  end

  test "cached glyph plans still charge work and respect existing cache byte limits", %{cjk: font} do
    Font.with_measurement_cache(fn ->
      plan = Font.css_glyph_advances("東京", font, 12)
      limits = Limits.effective()
      Limits.install(%{limits | max_layout_text_work: 1})

      assert {:error, {:resource_limit_exceeded, diagnostic}} =
               HtmlValidator.with_render_budget(fn -> Font.css_glyph_advances("東京", font, 12) end)

      assert diagnostic.stage == :layout
      assert diagnostic.message =~ "max_layout_text_work"
      Limits.install(limits)
      assert Font.css_glyph_advances("東京", font, 12) == plan
    end)

    Limits.install(%{
      Limits.effective()
      | max_layout_measurement_cache_bytes: 1,
        max_layout_measurement_cache_entries: 1
    })

    Font.with_measurement_cache(fn ->
      assert Font.css_glyph_advances("東京", font, 12) == Font.css_glyph_advances("東京", font, 12)
    end)
  end

  test "ordinary CSS paint supplies the measured advance through TJ without changing CID design widths",
       %{cjk: font} do
    box = %{
      type: :text,
      text: "東京",
      x: 10.0,
      y: 80.0,
      font: Font.pdf_name(font),
      font_face: font,
      font_size: 12.0,
      color: {0, 0, 0},
      snap_to_css_pixel_grid: true
    }

    assert {:ok, pdf} = PdfWriter.render([%{size: {200, 100}, boxes: [box]}])
    assert pdf =~ "16 Tf"
    assert pdf =~ "-0.0019"
    assert pdf =~ " TJ"
    assert pdf =~ "1000"
    assert {:ok, "東京"} = Text.extract(pdf, layout: false)
    explicit = Map.put(box, :glyph_ids, Enum.map(String.to_charlist("東京"), &font.cmap[&1]))
    assert {:ok, math_pdf} = PdfWriter.render([%{size: {200, 100}, boxes: [explicit]}])
    refute math_pdf =~ "-0.0019"
    assert {:ok, "東京"} = Text.extract(math_pdf, layout: false)
  end
end
