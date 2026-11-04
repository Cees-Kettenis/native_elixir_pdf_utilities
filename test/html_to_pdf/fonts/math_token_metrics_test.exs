defmodule NativeElixirPdfUtilities.HtmlToPdf.MathTokenMetricsTest do
  use ExUnit.Case, async: true

  alias NativeElixirPdfUtilities.HtmlToPdf.{Font, HtmlParser, ResourceContext, Style}
  alias NativeElixirPdfUtilities.HtmlToPdf.Font.MathTokenMetrics
  alias NativeElixirPdfUtilities.TestSupport.Paths
  alias NativeElixirPdfUtilities.Validators.HtmlValidator
  alias NativeElixirPdfUtilities.Validators.FontValidator

  test "device metrics follow Chromium across fractional sizes, overshoots and descenders" do
    {:ok, registry} = Font.load_registry(system_font_discovery: false)
    {:ok, _, font} = Font.resolve("math", 400, :normal, registry)
    profile = MathTokenMetrics.profile(:crypto.hash(:sha256, font.data))

    # Values observed with the original font through Chromium's text painter.
    for {size, text, expected} <- [
          {12, "3", {8.25, 0.0}},
          {14, "2", {9.0, 0.0}},
          {18, "1", {12.0, 0.0}},
          {18, "3", {12.0, 0.0}},
          {18, "7", {12.0, 0.0}},
          {18, "g", {9.0, 3.75}},
          {20, "x", {9.75, 0.0}},
          {30, "3", {20.25, 0.75}}
        ] do
      <<codepoint::utf8>> = text
      assert MathTokenMetrics.bounds(profile, font.cmap[codepoint], size) == expected
    end

    assert MathTokenMetrics.bounds(profile, font.cmap[32], 18) == {0.0, 0.0}
    assert MathTokenMetrics.bounds(profile, font.cmap[?1], 0.75) != nil
    assert MathTokenMetrics.bounds(profile, font.cmap[?1], 96) != nil
    assert MathTokenMetrics.bounds(profile, font.cmap[?1], 0.5) == nil
    assert MathTokenMetrics.bounds(profile, font.cmap[?1], 96.75) == nil
    assert MathTokenMetrics.profile(<<0::256>>) == nil
    assert MathTokenMetrics.bounds(nil, font.cmap[?1], 18) == nil
  end

  test "CSS token layout checks cached font contents and preserves selected script metrics" do
    ResourceContext.run([], HtmlValidator.new_image_budget(), fn _ ->
      {:ok, registry} = Font.load_registry(system_font_discovery: false)
      {:ok, _, font} = Font.resolve("math", 400, :normal, registry)
      FontValidator.memo({:math_token_metrics, font.id}, fn -> {<<>>, nil} end)

      {:ok, dom} = HtmlParser.parse_detailed("<math style='font-size:18pt'><mn>3</mn></math>")
      {:ok, %{children: [node]}} = Style.compute_detailed(dom, system_font_discovery: false)
      assert {node.style._math_box.ascent, node.style._math_box.descent} == {12.0, 0.0}

      html = "<math style='font-size:20pt'><mfrac><mn>1</mn><mn>3</mn></mfrac></math>"
      {:ok, dom} = HtmlParser.parse_detailed(html)
      {:ok, %{children: [node]}} = Style.compute_detailed(dom, system_font_discovery: false)
      measured = node.style._math_box
      [numerator, denominator] = measured.glyphs
      assert numerator.font_size == 14
      {:ok, outlines} = Font.glyph_bounds(font)
      {_left, _bottom, _right, top} = elem(outlines, numerator.glyph_id)
      {_left, bottom, _right, _top} = elem(outlines, denominator.glyph_id)
      scale = 14 / font.units_per_em
      assert_in_delta measured.ascent, numerator.y + top * scale, 1.0e-10
      assert_in_delta measured.descent, -denominator.y - bottom * scale, 1.0e-10
      assert measured.actual_text == "13"
      assert numerator.glyph_id != numerator.font_face.cmap[?1]
      assert denominator.glyph_id != denominator.font_face.cmap[?3]
    end)
  end

  test "sampled data remains tied to the unchanged bundled font bytes" do
    font = File.read!(Paths.project("priv/fonts/stix/STIXTwoMath-Regular.ttf"))

    metadata =
      Paths.project("priv/fonts/stix/token-ink-metrics.json") |> File.read!() |> :json.decode()

    assert Base.encode16(:crypto.hash(:sha256, font), case: :lower) == metadata["font_sha256"]
    data = File.read!(Paths.project("priv/fonts/stix/token-ink-metrics.bin"))
    assert Base.encode16(:crypto.hash(:sha256, data), case: :lower) == metadata["sha256"]
    assert metadata["glyph_count"] == 6760
  end
end
