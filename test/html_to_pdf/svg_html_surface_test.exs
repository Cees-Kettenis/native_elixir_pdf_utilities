defmodule NativeElixirPdfUtilities.HtmlToPdf.SvgHtmlSurfaceTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.{Limits, Text}

  alias NativeElixirPdfUtilities.HtmlToPdf.{
    HtmlParser,
    Style,
    ResourceContext,
    SvgBackend,
    PngDecoder
  }

  alias NativeElixirPdfUtilities.Validators.{
    HtmlValidator,
    SvgFormSlotValidator,
    SvgHtmlSurfaceValidator
  }

  setup do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    %{limits: limits}
  end

  test "internal input rasterizes the actual issued HTML painting PDF and selected Unicode" do
    with_slot(fn slot ->
      assert {:ok, prepared} = SvgHtmlSurfaceValidator.prepare(slot)
      assert prepared.source =~ "/FontFile2"
      refute prepared.source =~ "/Subtype /Image"
      assert {:ok, "Native input café Ω"} = Text.extract(prepared.source, layout: false)
      assert prepared.glyphs > 0
      assert prepared.selected != []

      assert Enum.all?(prepared.selected, fn {unicode, gid, matrix, bounds, visible} ->
               is_binary(unicode) and is_integer(gid) and tuple_size(matrix) == 6 and
                 tuple_size(bounds) == 4 and is_boolean(visible)
             end)

      assert prepared.logical == "Native input café Ω"
      assert prepared.surface_pixels > prepared.dimensions.width * prepared.dimensions.height
      assert {:ok, png} = SvgHtmlSurfaceValidator.render(prepared)
      assert {:ok, image} = PngDecoder.decode(png, HtmlValidator.new_image_budget(), false)

      assert {image.width_px, image.height_px} ==
               {prepared.dimensions.width, prepared.dimensions.height}

      assert Enum.any?(:binary.bin_to_list(image.data), &(&1 != 255))
    end)
  end

  test "actual source preparation stops before native raster allocation on shared ceilings", %{
    limits: limits
  } do
    with_slot(fn slot ->
      Limits.install(Map.put(limits, :max_aggregate_svg_surface_pixels, 100))

      assert {:error, {:resource_limit_exceeded, %{stage: :limits, message: message}}} =
               SvgHtmlSurfaceValidator.prepare(slot)

      assert message =~ "max_aggregate_svg_surface_pixels"
      Limits.install(Map.put(limits, :max_svg_filter_work, 100))

      assert {:error, {:resource_limit_exceeded, %{stage: :limits, message: message}}} =
               SvgHtmlSurfaceValidator.prepare(slot)

      assert message =~ "max_svg_filter_work"
    end)
  end

  defp with_slot(fun) do
    budget = HtmlValidator.new_image_budget()

    ResourceContext.run([], budget, fn _context ->
      html =
        "<svg width='200' height='80'><foreignObject x='10' y='10' width='180' height='50'><div style='font-family:DejaVu Sans;font-size:14px;color:#234a88;background:#e3c369'>Native input café Ω</div></foreignObject></svg>"

      assert {:ok, dom} = HtmlParser.parse_detailed(html)
      assert {:ok, %{children: [svg]}} = Style.compute_detailed(dom, [], budget)
      scene = svg.style.svg_scene

      source =
        String.replace(scene.source, "<svg ", "<svg width='200' height='80' ", global: false)

      assert {:ok, pdf} =
               SvgBackend.vectorize_slots(
                 source,
                 [width: 400, height: 160],
                 %{mode: :inline, fonts: scene.selected_fonts},
                 scene.foreign_slots,
                 budget
               )

      assert {:ok, _context, [slot]} = SvgFormSlotValidator.prepare(pdf, scene.foreign_slots)
      fun.(slot)
    end)
  end
end
