defmodule NativeElixirPdfUtilities.HtmlToPdf.CssEffectsTest do
  use ExUnit.Case, async: true
  alias NativeElixirPdfUtilities.{HtmlToPdf, Text}
  alias NativeElixirPdfUtilities.Validators.CssEffectsValidator

  test "filter functions retain order, dimensions, percentages and selected colors" do
    assert {:ok, style} =
             CssEffectsValidator.prepare(
               %{},
               "filter",
               "brightness(50%) contrast(2) grayscale(.2) invert() opacity(.5) saturate(2) sepia(20%) hue-rotate(90deg) blur(2px) drop-shadow(1px 2px 3px red)"
             )

    assert length(style.css_filter) == 10
    assert {"brightness", 0.5} = hd(style.css_filter)
    assert CssEffectsValidator.valid_style?(style)

    for source <- [
          "blur(-1px)",
          "blur(20%)",
          "brightness(-1)",
          "hue-rotate(1px)",
          "foo(1)",
          "blur(",
          "drop-shadow(1px)",
          "url(https://example.com/f.svg)"
        ] do
      assert :error = CssEffectsValidator.prepare(%{}, "filter", source)
    end

    assert {:ok, %{css_filter: [{"url", "local"}]}} =
             CssEffectsValidator.prepare(%{}, "filter", "url(#local)")

    assert {:ok, %{css_backdrop_filter: []}} =
             CssEffectsValidator.prepare(%{}, "backdrop-filter", "none")
  end

  test "HTML filters paint selected source outlines and retain searchable logical text" do
    html =
      "<style>html,body{margin:0;font:12pt/18pt DejaVu Sans}div{width:80pt;height:45pt;background:red;filter:blur(1px) invert(1)}</style><div>Filtered ink</div>"

    assert {:ok, pdf} =
             HtmlToPdf.render(html,
               margin: 0,
               page_size: {180, 120},
               system_font_discovery: false
             )

    assert pdf =~ "/Subtype /Image"
    assert {:ok, text} = Text.extract(pdf)
    assert text =~ "Filtered ink"
  end

  test "filter channels follow independent Filter Effects scalar vectors" do
    # Filter Effects 1, 2018-12-18, filter functions: encoded-sRGB channel formulas.
    alias NativeElixirPdfUtilities.HtmlToPdf.CssEffectScene

    box = %{
      type: :rect,
      x: 0.0,
      y: 0.0,
      width: 9.0,
      height: 9.0,
      fill_color: {0.8, 0.4, 0.2},
      stroke_color: nil,
      stroke_width: 0.0,
      border_radius: 0.0
    }

    effect = %{definitions: "", color: {0, 0, 0}}
    region = %{x: 0.0, y: 0.0, width: 9.0, height: 9.0}

    for {operation, expected} <- [
          {{"invert", 1.0}, {51, 153, 204}},
          {{"brightness", 0.5}, {102, 51, 26}},
          {{"contrast", 0.5}, {166, 115, 89}},
          {{"saturate", 0.0}, {120, 120, 120}},
          {{"grayscale", 1.0}, {120, 120, 120}},
          {{"sepia", 1.0}, {168, 149, 116}},
          {{"hue-rotate", 0.0}, {204, 102, 51}},
          {{"opacity", 0.5}, {204, 102, 51}}
        ] do
      assert {:ok, %{image: image}} =
               CssEffectScene.render([box], region, [operation], effect, {9, 9})

      center = div(image.height_px, 2) * image.width_px + div(image.width_px, 2)
      <<r, g, b>> = binary_part(image.data, center * 3, 3)
      {er, eg, eb} = expected

      assert abs(r - er) <= 2 and abs(g - eg) <= 2 and abs(b - eb) <= 2,
             inspect({operation, {r, g, b}, expected})

      if elem(operation, 0) == "opacity",
        do: assert(abs(:binary.at(image.alpha_data, center) - 128) <= 1)
    end
  end

  test "owned referenced filter graphs resolve without acquiring another tree scope" do
    html =
      "<svg width='1' height='1'><defs><filter id='gray'><feColorMatrix type='saturate' values='0'/></filter></defs></svg><div style='filter:url(#gray);width:45pt;height:24pt;background:red'>Graph</div>"

    assert {:ok, pdf} = HtmlToPdf.render(html, system_font_discovery: false)
    assert pdf =~ "/Subtype /Image"
    assert {:ok, text} = Text.extract(pdf)
    assert text =~ "Graph"

    assert {:error, {:unsupported_html, diagnostic}} =
             HtmlToPdf.render("<div style='filter:url(#missing)'>Missing</div>")

    assert diagnostic.source == "filter url(#missing)"
  end

  test "backdrop sampling and unsupported mutable controls return bounded native results" do
    assert {:ok, pdf} =
             HtmlToPdf.render(
               "<div style='background:red;width:80pt;height:30pt;position:relative'><span style='backdrop-filter:invert(1);display:block;width:40pt;height:20pt'>Sample</span></div>"
             )

    assert pdf =~ "/Subtype /Image"

    assert {:error, {:unsupported_html, diagnostic}} =
             HtmlToPdf.render("<div style='filter:blur(1px)'><input value='Mutable'></div>")

    assert diagnostic.source == "filter controls"
    assert diagnostic.message =~ "forms: :static"

    assert {:ok, _} =
             HtmlToPdf.render("<div style='filter:blur(1px)'><input value='Static'></div>",
               forms: :static
             )
  end
end
