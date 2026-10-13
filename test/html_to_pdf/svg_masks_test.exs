defmodule NativeElixirPdfUtilities.HtmlToPdf.SvgMasksTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.{HtmlToPdf, Limits, Text}
  alias NativeElixirPdfUtilities.HtmlToPdf.SvgBackend
  alias NativeElixirPdfUtilities.Validators.SvgEffectsValidator

  setup do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    %{limits: limits}
  end

  test "luminance and alpha masks become PDF transparency masks over native vector artwork" do
    html =
      File.read!(
        Path.expand("../fixtures/html_to_pdf/browser_parity/inline_svg_masks.html", __DIR__)
      )

    assert {:ok, pdf} = HtmlToPdf.render(html)
    assert pdf =~ "/SMask"
    assert pdf =~ "/S /Luminosity"
    assert pdf =~ "/S /Alpha"
    assert pdf =~ "/Group"
    refute pdf =~ "/Subtype /Image"
    assert {:ok, text} = Text.extract(pdf, layout: false)
    assert text =~ "café Ω"
  end

  test "mask geometry, units and references fail through their semantic authority" do
    for attrs <- [
          "maskUnits='bad'",
          "maskContentUnits='bad'",
          "width='bad'",
          "height='-1'",
          "mask-type='bad'"
        ] do
      source =
        svg(
          "<defs><mask id='m' #{attrs}><rect width='10' height='10' fill='white'/></mask></defs><rect width='20' height='20' mask='url(#m)'/>"
        )

      assert {:error, {:invalid_document, %{stage: :layout, message: message}}} =
               SvgBackend.vectorize(source, [width: 100, height: 60], %{mode: :inline})

      assert message != ""
    end

    node = %{type: :element, tag: "rect", attributes: %{"mask" => "url(#wrong)"}, children: []}

    assert {:error,
            {:invalid_document,
             %{module: SvgEffectsValidator, source: "rect mask", operation: :prepare}}} =
             SvgEffectsValidator.validate_tree(node)
  end

  test "mask intermediate allocations share the cumulative surface limit while vector masks stay pixel-free",
       %{limits: limits} do
    source =
      svg(
        "<defs><mask id='m'><rect width='100' height='60' fill='white'/></mask></defs><rect width='100' height='60' mask='url(#m)'/>"
      )

    Limits.install(Map.put(limits, :max_aggregate_svg_surface_pixels, 10_000))

    assert {:error, {:resource_limit_exceeded, %{stage: :limits, message: message}}} =
             SvgBackend.rasterize(source, [width: 100, height: 60], %{mode: :inline})

    assert message =~ "max_aggregate_svg_surface_pixels"
    assert {:ok, pdf} = SvgBackend.vectorize(source, [width: 100, height: 60], %{mode: :inline})
    assert pdf =~ "/S /Luminosity"
  end

  test "mask content charges real path work rather than only its outer target", %{limits: limits} do
    source =
      svg(
        "<defs><mask id='m'><path d='M0 0H10V10H0Z' fill='white'/></mask></defs><rect width='20' height='20' mask='url(#m)'/>"
      )

    Limits.install(Map.put(limits, :max_svg_stroke_work, 7))

    assert {:error, {:resource_limit_exceeded, %{message: message}}} =
             SvgBackend.vectorize(source, [width: 100, height: 60], %{mode: :inline})

    assert message =~ "max_svg_stroke_work"
  end

  defp svg(content), do: "<svg width='100' height='60'>#{content}</svg>"
end
