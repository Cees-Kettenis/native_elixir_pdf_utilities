defmodule NativeElixirPdfUtilities.HtmlToPdf.IntegrationWave23Test do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, Style, SvgRasterizer}
  alias NativeElixirPdfUtilities.Validators.{FontValidator, HtmlValidator}
  alias NativeElixirPdfUtilities.Limits

  setup do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    :ok
  end

  test "SVG image paints with the explicitly authorized independent font bytes and does not request another resource" do
    font = File.read!("priv/fonts/dejavu/DejaVuSans.ttf")

    svg =
      "<svg xmlns='http://www.w3.org/2000/svg' width='80' height='80'><rect width='80' height='80' fill='#b9251b'/><text x='12' y='52' font-family='DejaVu Sans' font-size='32' fill='white'>SG</text></svg>"

    source = "data:image/svg+xml;base64," <> Base.encode64(svg)
    html = "<img style='font-family:ExactHost;width:60pt;height:60pt' src='#{source}'>"
    assert {:ok, dom} = HtmlParser.parse_detailed(html)

    assert {:ok, styled} =
             Style.compute_detailed(dom,
               fonts: [%{family: "ExactHost", data: font}],
               system_font_discovery: false,
               resource_fetcher: fn url -> flunk("unexpected resource request #{url}") end
             )

    [image] = styled.children

    assert {:ok, local_dom} =
             HtmlParser.parse_detailed(
               "<img style='font-family:ExactHost;width:60pt;height:60pt' src='art.svg'>"
             )

    assert {:ok, local_styled} =
             Style.compute_detailed(local_dom,
               fonts: [%{family: "ExactHost", data: font}],
               system_font_discovery: false,
               assets: %{"art.svg" => {:bytes, svg}},
               resource_fetcher: fn url -> flunk("unexpected resource request #{url}") end
             )

    [local_image] = local_styled.children
    assert local_image.style.image.data == image.style.image.data
    assert image.style.font_face.data == font

    assert {:ok, [%{data: ^font} = descriptor]} =
             FontValidator.prepare_svg_image_fonts([%{family: "DejaVu Sans", data: font}])

    assert {:ok, png} =
             SvgRasterizer.rasterize(
               svg,
               [width: 80, height: 80],
               HtmlValidator.new_image_budget(),
               [descriptor]
             )

    assert {:ok, decoded} =
             NativeElixirPdfUtilities.HtmlToPdf.PngDecoder.decode(
               png,
               HtmlValidator.new_image_budget(),
               false
             )

    assert image.style.image.data == decoded.data
    assert Enum.any?(for(<<r, g, b <- decoded.data>>, do: {r, g, b}), &(&1 == {255, 255, 255}))
  end

  test "selected image font transport retains canonical failures and shared source byte limits" do
    assert {:ok, []} = FontValidator.prepare_svg_image_fonts([])

    for face <- [false, [%{family: "Bad", data: nil}], [%{family: "Bad", data: <<0>>}]] do
      assert {:error, {_reason, diagnostic}} = FontValidator.prepare_svg_image_fonts(face)
      assert is_atom(diagnostic.stage) and is_atom(diagnostic.reason)
      assert is_binary(diagnostic.message)
    end

    Limits.install(Map.put(Limits.effective(), :max_font_source_bytes, 1))
    assert {:ok, []} = FontValidator.prepare_svg_image_fonts([])

    result =
      FontValidator.with_budget(fn ->
        FontValidator.prepare_svg_image_fonts([
          %{
            family: "DejaVu Sans",
            data: File.read!("priv/fonts/dejavu/DejaVuSans.ttf")
          }
        ])
      end)

    assert {:error, {:resource_limit_exceeded, diagnostic}} = result
    assert diagnostic.stage == :font and diagnostic.message =~ "max_font_source_bytes"

    assert {:error, {:invalid_options, invalid_context}} =
             SvgRasterizer.rasterize("<svg/>", [], nil, :invalid)

    assert is_atom(invalid_context.stage) and is_binary(invalid_context.message)
  end
end
