alias NativeElixirPdfUtilities.TestSupport.Paths

defmodule NativeElixirPdfUtilities.HtmlToPdf.CjkQuantizationTest do
  use ExUnit.Case
  alias NativeElixirPdfUtilities.HtmlToPdf.{Font, HtmlParser, Layout, Style}
  alias NativeElixirPdfUtilities.Validators.{FontValidator, HtmlLineBreakValidator}

  @font Paths.fixture("html_to_pdf/browser_parity/assets/cjk-ruby-fixture.ttf")
  @fonts [%{family: "CJK Ruby Fixture", path: @font}]

  test "selected CJK font linear advance exactly matches FreeType's physical fixed-point metrics" do
    assert {:ok, registry} = Font.load_registry(fonts: @fonts, system_font_discovery: false)
    assert {:ok, _, font} = Font.resolve("CJK Ruby Fixture", 400, :normal, registry)
    assert FontValidator.css_advance_scale(1000, 12.0) == %{scale: 67_109, correction: 1.0}
    # Physical FT_Load_Glyph NO_HINTING on these exact source bytes returns this 16.16 advance.
    assert Font.css_text_width("東", font, 12.0) == 1_048_578 / 65_536 * 0.75
    assert Font.css_text_width("東京京都", font, 12.0) == 4 * 1_048_578 / 65_536 * 0.75
    assert Font.text_width("東京京都", font, 12.0) == 48.0
    assert Font.css_text_width("東", font, 13.0) == 1_135_609 / 65_536 * 0.75
    assert Font.css_text_width("東", font, 0.0) == 0.0
    assert_in_delta Font.css_text_width("東", font, 0.375), 0.375, 0.00001
  end

  test "design and CSS metric caches preserve their distinct measured results and selected fonts" do
    assert {:ok, registry} = Font.load_registry(fonts: @fonts, system_font_discovery: false)
    assert {:ok, _, cjk} = Font.resolve("CJK Ruby Fixture", 400, :normal, registry)
    assert {:ok, _, latin} = Font.resolve("DejaVu Sans", 400, :normal, registry)

    Font.with_measurement_cache(fn ->
      for _ <- 1..3 do
        assert Font.text_width("東京", cjk, 12.0) == 24.0
        assert Font.css_text_width("東京", cjk, 12.0) == 2 * 1_048_578 / 65_536 * 0.75
        assert Font.css_text_width("Latin", latin, 12.0) == Font.text_width("Latin", latin, 12.0)
      end
    end)
  end

  test "one rounded fragment fits, while separate styled fragments retain their own layout units" do
    assert HtmlLineBreakValidator.snap_fragment_width(24.000045776367188) == 24.01171875
    assert HtmlLineBreakValidator.fits_width?(48.01171875, 48.0)
    refute HtmlLineBreakValidator.fits_width?(48.0234375, 48.0)

    for {content, expected} <- [
          {"東京京都大阪漢字", ["東京京都", "大阪漢字"]},
          {"東京<span>京都</span>、大阪漢字", ["東京京", "都、大", "阪漢字"]}
        ] do
      html =
        "<p style='margin:0;width:48pt;font-family:CJK Ruby Fixture;line-height:30pt'>#{content}</p>"

      assert {:ok, dom} = HtmlParser.parse_detailed(html)

      assert {:ok, styled} =
               Style.compute_detailed(dom, fonts: @fonts, system_font_discovery: false)

      assert {:ok, layout} = Layout.layout(styled, page_size: {200, 200}, margin: 0)

      lines =
        layout.boxes
        |> Enum.filter(&(Map.get(&1, :type) == :text))
        |> Enum.group_by(& &1.y)
        |> Enum.sort_by(fn {y, _} -> -y end)
        |> Enum.map(fn {_y, boxes} ->
          boxes |> Enum.sort_by(& &1.x) |> Enum.map_join(& &1.text)
        end)

      assert lines == expected
    end
  end

  test "the unchanged combined CJK fixture preserves glyphs and the browser's styled line division" do
    path =
      Paths.fixture("html_to_pdf/browser_parity/ruby_cjk_default_wrap.html")

    assert {:ok, dom} = HtmlParser.parse_detailed(File.read!(path))

    assert {:ok, styled} =
             Style.compute_detailed(dom, fonts: @fonts, system_font_discovery: false)

    assert {:ok, layout} = Layout.layout(styled, page_size: {200, 200}, margin: 10)
    bases = Enum.filter(layout.boxes, &(Map.get(&1, :type) == :text and &1.font_size == 12.0))
    assert Enum.map_join(bases, & &1.text) == "東京京都大阪漢字東京京都、大阪漢字東京京都大阪漢字"
    assert Enum.any?(bases, &(&1.text == "阪漢字"))
  end
end
