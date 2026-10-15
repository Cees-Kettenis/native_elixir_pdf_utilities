defmodule NativeElixirPdfUtilities.HtmlToPdf.SvgRasterColorTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.{Limits}
  alias NativeElixirPdfUtilities.HtmlToPdf.{PngDecoder, SvgRasterizer}
  alias NativeElixirPdfUtilities.Validators.HtmlValidator

  setup do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    %{limits: limits}
  end

  test "restricted raster images retain actual native CSS, clipping, masks and patterns" do
    sources = [
      {"<style>rect {fill:#112233}</style><rect width='4' height='1'/>", <<17, 34, 51>>, nil},
      {"<defs><clipPath id='c'><rect width='2' height='1'/></clipPath></defs><rect width='4' height='1' fill='red' clip-path='url(#c)'/>",
       <<255, 0, 0>>, <<255, 255, 0, 0>>},
      {"<defs><mask id='m' mask-type='alpha' maskUnits='userSpaceOnUse' x='0' y='0' width='4' height='1'><rect width='2' height='1' fill='white'/></mask></defs><rect width='4' height='1' fill='red' mask='url(#m)'/>",
       <<255, 0, 0>>, <<255, 255, 0, 0>>},
      {"<defs><pattern id='p' patternUnits='userSpaceOnUse' width='2' height='1'><rect width='2' height='1' fill='#112233'/></pattern></defs><rect width='4' height='1' fill='url(#p)'/>",
       <<17, 34, 51>>, nil}
    ]

    for {body, first, alpha} <- sources do
      image = raster_image("<svg width='4' height='1'>#{body}</svg>")
      assert {image.width_px, image.height_px} == {4, 1}
      assert binary_part(image.data, 0, 3) == first
      assert Map.get(image, :alpha_data) == alpha

      if alpha == nil do
        assert image.data == :binary.copy(first, 4)
      end
    end
  end

  test "restricted native filter input observes its genuine color-interpolation-filters" do
    matrix = "0 0 0 0 0.25 0 0 0 0 0.5 0 0 0 0 0.75 0 0 0 0 1"

    for {mode, expected} <- [{"sRGB", [64, 128, 191]}, {"linearRGB", [137, 188, 225]}] do
      image =
        raster_image(
          "<svg width='4' height='1'><defs><filter id='f' filterUnits='userSpaceOnUse' x='0' y='0' width='4' height='1' color-interpolation-filters='#{mode}'><feColorMatrix type='matrix' values='#{matrix}'/></filter></defs><rect width='4' height='1' fill='red' filter='url(#f)'/></svg>"
        )

      actual = image.data |> binary_part(0, 3) |> :binary.bin_to_list()

      assert Enum.zip(actual, expected)
             |> Enum.all?(fn {actual, expected} -> abs(actual - expected) <= 1 end)

      assert Map.get(image, :alpha_data) == nil
    end
  end

  test "direct print gradients retain their source mode and interpolate encoded stop channels" do
    for {mode, expected} <- [{"sRGB", 128}, {"linearRGB", 128}] do
      image =
        raster_image(
          "<svg width='3' height='1'><defs><linearGradient id='g' color-interpolation='#{mode}'><stop stop-color='black'/><stop offset='1' stop-color='white'/></linearGradient></defs><rect width='3' height='1' fill='url(#g)'/></svg>"
        )

      assert image.data
             |> binary_part(3, 3)
             |> :binary.bin_to_list()
             |> Enum.all?(&(abs(&1 - expected) <= 1))
    end
  end

  test "print stop colors retain independent alpha before encoded raster premultiplication" do
    image =
      raster_image(
        "<svg width='3' height='1'><defs><linearGradient id='g' color-interpolation='linearRGB'><stop stop-color='black' stop-opacity='.5'/><stop offset='1' stop-color='white' stop-opacity='.5'/></linearGradient></defs><rect width='3' height='1' fill='url(#g)'/></svg>"
      )

    assert binary_part(image.alpha_data, 1, 1) == <<128>>

    assert image.data
           |> binary_part(3, 3)
           |> :binary.bin_to_list()
           |> Enum.all?(&(abs(&1 - 128) <= 2))
  end

  test "print gradient contexts preserve actual filter and mask input transfer, alpha and resources" do
    gradient =
      "<linearGradient id='g' color-interpolation='linearRGB'><stop stop-color='black'/><stop offset='1' stop-color='white'/></linearGradient>"

    rect = "<rect width='3' height='1' fill='url(#g)'/>"

    sources = [
      {"<defs>#{gradient}</defs>#{rect}", {128, 128, 128}, nil},
      {"<defs>#{String.replace(gradient, "color-interpolation='linearRGB'", "")}</defs><style>linearGradient{color-interpolation:linearRGB}</style>#{rect}",
       {128, 128, 128}, nil},
      {"<defs color-interpolation='linearRGB'>#{String.replace(gradient, "color-interpolation='linearRGB'", "")}</defs>#{rect}",
       {128, 128, 128}, nil},
      {"<defs><radialGradient id='g' gradientUnits='userSpaceOnUse' cx='0' cy='0.5' r='3' color-interpolation='linearRGB'><stop stop-color='black'/><stop offset='1' stop-color='white'/></radialGradient></defs>#{rect}",
       {128, 128, 128}, nil},
      {"<defs>#{gradient}<pattern id='p' patternUnits='userSpaceOnUse' width='3' height='1'>#{rect}</pattern></defs><rect width='3' height='1' fill='url(#p)'/>",
       {128, 128, 128}, nil},
      {
        "<defs>#{gradient}<filter id='f' x='0' y='0' width='1' height='1'><feOffset dx='0' dy='0'/></filter></defs><g filter='url(#f)'>#{rect}</g>",
        # Chromium 152 honors the gradient's color-interpolation='linearRGB' when
        # painting it into the filter input (midpoint 187); resvg interpolates
        # gradient stops in sRGB regardless, so the transported midpoint is 128.
        {128, 128, 128},
        nil
      },
      {
        "<defs>#{gradient}<mask id='m' maskUnits='userSpaceOnUse' x='0' y='0' width='3' height='1'>#{rect}</mask></defs><rect width='3' height='1' fill='red' mask='url(#m)'/>",
        {255, 0, 0},
        # Chromium renders the linearRGB gradient into the mask and yields mask
        # alpha 188 at the midpoint; resvg's sRGB gradient interpolation gives 128.
        128
      },
      {"<defs>#{String.replace(gradient, "<stop", "<stop stop-opacity='0.5'")}</defs><rect width='3' height='1' fill='red'/>#{rect}",
       {191, 64, 64}, nil}
    ]

    for {body, expected, alpha} <- sources do
      image = raster_image("<svg width='3' height='1'>#{body}</svg>")
      <<_::binary-size(3), r, g, b, _::binary>> = image.data
      {er, eg, eb} = expected
      assert abs(r - er) <= 2
      assert abs(g - eg) <= 2
      assert abs(b - eb) <= 2

      if alpha do
        assert abs(:binary.at(image.alpha_data, 1) - alpha) <= 2
      end
    end
  end

  defp raster_image(source) do
    budget = HtmlValidator.new_image_budget()
    assert {:ok, png} = SvgRasterizer.rasterize(source, [], budget)
    assert {:ok, image} = PngDecoder.decode(png, budget, false)
    image
  end

  test("image resource restrictions and actual raster work ceilings survive native routing", %{
    limits: limits
  }) do
    assert {:error, {:invalid_document, diagnostic}} =
             SvgRasterizer.rasterize(
               "<svg width='3' height='1'><image href='https://example.invalid/a.png'/></svg>",
               [],
               nil
             )

    assert diagnostic.stage == :style
    assert Map.get(diagnostic, :source) == "SVG"
    assert diagnostic.message =~ "not authorized"
    Limits.install(Map.put(Limits.effective(), :max_svg_raster_pixels, 0))

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             SvgRasterizer.rasterize(
               "<svg width='3' height='1'><path d='M0 0L3 0L3 1L0 1z' stroke='red' stroke-width='1'/></svg>",
               [],
               nil
             )

    assert diagnostic.reason == :resource_limit_exceeded
    Limits.install(Map.put(limits, :max_svg_output_bytes, 1))

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             NativeElixirPdfUtilities.HtmlToPdf.render(
               "<img src='data:image/svg+xml;base64,#{Base.encode64("<svg width='3' height='1'><rect width='3' height='1'/></svg>")}'/>"
             )

    assert diagnostic.reason == :resource_limit_exceeded
  end
end
