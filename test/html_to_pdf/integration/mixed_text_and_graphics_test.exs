defmodule NativeElixirPdfUtilities.HtmlToPdf.MixedTextAndGraphicsTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.{HtmlToPdf, Limits, Text}
  alias NativeElixirPdfUtilities.Pdf.Reader
  alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, Layout, Style}

  setup do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    %{limits: limits}
  end

  test "CSS print text, authored Math glyphs and transformed SVG strokes share one valid PDF" do
    html =
      "<style>body{margin:0}p{margin:0;font-family:DejaVu Sans;font-size:14px}</style>\n<p><a href='https://example.com/source'>Selected text</a></p>\n<math><mi>x</mi><mfrac><mn>12</mn><mn>3</mn></mfrac></math>\n<svg width='180' height='80'>\n  <defs><linearGradient id='shade'>\n    <stop stop-color='red' stop-opacity='.2'/>\n    <stop offset='1' stop-color='blue' stop-opacity='.8'/>\n  </linearGradient></defs>\n  <rect width='100' height='30' fill='url(#shade)'/>\n  <path transform='rotate(30 80 40)' d='M10 40 H150' fill='none'\n        stroke='#174a86' stroke-width='3' stroke-opacity='.4'/>\n</svg>\n"

    assert {:ok, pdf} =
             HtmlToPdf.render(html,
               system_font_discovery: false,
               compress_streams: false,
               page_size: {240, 240},
               margin: 12
             )

    assert {:ok, %{document: document}} = Reader.read_validated(pdf)
    assert length(document.pages) == 1
    assert pdf =~ "0.23999999"
    assert pdf =~ "/ActualText"
    assert pdf =~ "/CIDFontType2"
    assert pdf =~ "/URI (https://example.com/source)"

    images =
      for {_id, %{value: value} = image} <- document.objects,
          is_map(value) and value["Subtype"] == {:name, "Image"} and
            value["ColorSpace"] == {:name, "DeviceRGB"} do
        image
      end

    assert images != []

    assert Enum.any?(images, fn image ->
             case image.value["SMask"] do
               {:ref, reference} ->
                 pixels = :zlib.uncompress(image.stream)
                 alpha = :zlib.uncompress(document.objects[reference].stream)

                 for {<<r, g, b>>, index} <-
                       Enum.with_index(
                         for <<r, g, b <- pixels>> do
                           <<r, g, b>>
                         end
                       ) do
                   abs(r - 23) <= 1 and abs(g - 74) <= 1 and abs(b - 134) <= 1 and
                     abs(:binary.at(alpha, index) - 102) <= 1
                 end
                 |> Enum.any?()

               nil ->
                 false
             end
           end)

    assert {:ok, text} = Text.extract(pdf, layout: false)
    assert text =~ "Selected text"
    assert String.replace(text, ~r/\s/u, "") =~ "x123"
  end

  test "ordinary HTML loads Unicode line breaking in a fresh VM without the private binding path" do
    paths =
      :code.get_path()
      |> Enum.map(&List.to_string/1)
      |> Enum.filter(&(Path.basename(&1) == "ebin"))
      |> Enum.flat_map(&["-pa", &1])

    script =
      "{:ok, _} = Application.ensure_all_started(:native_elixir_pdf_utilities)\nfalse = Code.ensure_loaded?(NativeElixirPdfUtilities.HtmlToPdf.SvgBackend.NativeBytes)\n{:ok, pdf} = NativeElixirPdfUtilities.HtmlToPdf.render(\"<p>Cold ordinary text</p>\",\n  default_font: \"Helvetica\", system_font_discovery: false)\n{:ok, \"Cold ordinary text\"} = NativeElixirPdfUtilities.Text.extract(pdf, layout: false)\nIO.puts(\"cold ordinary render passed\")\n"

    assert {output, 0} =
             System.cmd(System.find_executable("elixir"), paths ++ ["-e", script],
               stderr_to_stdout: true
             )

    assert output =~ "cold ordinary render passed"
  end

  test "an atomic owner's preserved text does not suppress its parent's wrapping opportunities" do
    for {parent_white_space, same_line} <- [{"normal", false}, {"nowrap", true}, {"pre", true}] do
      html = "<p style='margin:0;width:90pt;white-space:#{parent_white_space}'>
<span style='display:inline-block;width:60pt;white-space:pre'>First</span> <span style='display:inline-block;width:60pt;white-space:nowrap'>Second</span>
</p>
"
      assert {:ok, dom} = HtmlParser.parse_detailed(html)
      assert {:ok, styled} = Style.compute_detailed(dom, system_font_discovery: false)
      assert {:ok, layout} = Layout.layout(styled, page_size: {240, 240}, margin: 0)
      first = Enum.find(layout.boxes, &(Map.get(&1, :text) == "First"))
      second = Enum.find(layout.boxes, &(Map.get(&1, :text) == "Second"))
      assert first.y == second.y == same_line

      if not same_line do
        assert second.y < first.y and second.x == first.x
      end
    end
  end

  test "a painted replacement does not bypass original selected Math font validation" do
    html =
      "<body><?start name='Original'>\n<math style='font-family:MissingOriginalMath'><mi>x</mi></math>\n<?end?><template for='Original'><p>Selected replacement</p></template></body>\n"

    assert {:error, {:unsupported_glyph, diagnostic}} =
             HtmlToPdf.render(html, system_font_discovery: false)

    assert diagnostic.reason == :unsupported_glyph
    assert diagnostic.stage == :layout
    assert diagnostic.operation == :measure_math
    assert diagnostic.source == "x"
    assert diagnostic.module == NativeElixirPdfUtilities.Validators.MathLayoutValidator
    assert diagnostic.message =~ "selected math font"
  end

  test "symbolic unapplied CSS and original Math glyph validation compose before replacement" do
    html =
      "<body><style>.unused-source { width:2em; color:blue }</style>\n<?start name='Original'><math><mi>x</mi><mn>12</mn></math><?end?>\n<template for='Original'><p>Good</p></template></body>\n"

    assert {:ok, pdf} =
             HtmlToPdf.render(html, system_font_discovery: false, compress_streams: false)

    assert {:ok, expected} =
             HtmlToPdf.render("<p>Good</p>",
               system_font_discovery: false,
               compress_streams: false
             )

    assert pdf == expected
    assert {:ok, "Good"} = Text.extract(pdf)
  end

  test("composed SVG alpha and PNG conversion use the shared budget without leaking state", %{
    limits: limits
  }) do
    html =
      "<p>Before SVG</p><svg width='120' height='50'>\n  <path d='M10 20 H110' fill='none' stroke='blue' stroke-width='3' stroke-opacity='.4'/>\n</svg><math><mi>x</mi></math>\n"

    Limits.install(%{limits | max_svg_raster_pixels: 0})

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             HtmlToPdf.render(html, system_font_discovery: false)

    assert diagnostic.reason == :resource_limit_exceeded
    assert diagnostic.stage in [:layout, :limits]
    assert diagnostic.message =~ "pixel"
    assert is_binary(Map.get(diagnostic, :source))
    Limits.install(limits)
    assert {:ok, pdf} = HtmlToPdf.render(html, system_font_discovery: false)
    assert {:ok, text} = Text.extract(pdf)
    assert text =~ "Before SVG"
  end
end
