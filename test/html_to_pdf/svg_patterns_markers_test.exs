defmodule NativeElixirPdfUtilities.HtmlToPdf.SvgPatternsMarkersTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.{HtmlToPdf, Limits, Text}
  alias NativeElixirPdfUtilities.HtmlToPdf.SvgBackend
  alias NativeElixirPdfUtilities.Validators.SvgEffectsValidator

  setup do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    %{limits: limits}
  end

  test "pattern units, inheritance and transformed stroke paints produce native PDF tiling resources" do
    html =
      File.read!(
        Path.expand(
          "../fixtures/html_to_pdf/browser_parity/inline_svg_patterns_markers.html",
          __DIR__
        )
      )

    assert {:ok, pdf} = HtmlToPdf.render(html)
    assert pdf =~ "/PatternType 1"
    assert pdf =~ "/PaintType 1"
    assert pdf =~ "/XStep"
    refute pdf =~ "/Subtype /Image"
    assert {:ok, text} = Text.extract(pdf)
    assert text =~ "Ω café"
  end

  test "marker references and definition geometry use the semantic validator" do
    for content <- [
          "<rect id='bad'/><path d='M0 0L10 10' marker-end='url(#bad)'/>",
          "<marker id='m' markerUnits='bad'/>",
          "<marker id='m' orient='bad'/>",
          "<marker id='m' markerWidth='-1'/>",
          "<pattern id='p' width='bad' height='1'/>",
          "<pattern id='p' width='1' height='1' patternUnits='bad'/>",
          "<pattern id='p' width='1' height='1' patternTransform='bad'/>",
          "<rect id='bad'/><pattern id='p' href='#bad'/>"
        ] do
      assert {:error, {:invalid_document, diagnostic}} =
               SvgBackend.vectorize(svg(content), [width: 100, height: 60], %{mode: :inline})

      assert diagnostic.stage == :layout
      assert diagnostic.message != ""
      assert diagnostic.source != ""
      # Public execution adds its own operation context to the semantic diagnostic.
      assert diagnostic.operation == :prepare
    end

    assert {:error, {:invalid_document, %{module: SvgEffectsValidator}}} =
             SvgEffectsValidator.validate_tree(%{
               type: :element,
               tag: "marker",
               attributes: %{"orient" => "bad"},
               children: []
             })
  end

  test "pattern tile pixels are reserved before native raster allocation", %{limits: limits} do
    source =
      svg(
        "<defs><pattern id='p' patternUnits='userSpaceOnUse' width='2000' height='2000'><rect width='100' height='100'/></pattern></defs><rect width='100' height='60' fill='url(#p)'/>"
      )

    Limits.install(Map.put(limits, :max_aggregate_svg_surface_pixels, 10_000))

    assert {:error, {:resource_limit_exceeded, %{message: message, stage: :limits}}} =
             SvgBackend.rasterize(source, [width: 100, height: 60], %{mode: :inline})

    assert message =~ "pattern"
    # PDF tiling cells contain a bounded drawing program rather than pixel surfaces.
    assert {:ok, pdf} = SvgBackend.vectorize(source, [width: 100, height: 60], %{mode: :inline})
    assert pdf =~ "/PatternType 1"
  end

  test "pattern children retain independent path and gradient work budgets", %{limits: limits} do
    source =
      svg(
        "<defs><pattern id='p' patternUnits='userSpaceOnUse' width='10' height='10'><path d='M0 0H10V10H0Z'/></pattern></defs><rect width='100' height='60' fill='url(#p)'/>"
      )

    Limits.install(Map.put(limits, :max_svg_stroke_work, 7))

    assert {:error, {:resource_limit_exceeded, %{message: message}}} =
             SvgBackend.vectorize(source, [width: 100, height: 60], %{mode: :inline})

    assert message =~ "max_svg_stroke_work"
  end

  defp svg(content), do: "<svg width='100' height='60'>#{content}</svg>"
end
