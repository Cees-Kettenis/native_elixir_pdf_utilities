defmodule NativeElixirPdfUtilities.HtmlToPdf.SvgBackgroundTransportTest do
  Code.require_file("../support/svg_public_workflow.ex", __DIR__)
  Code.require_file("../support/png_fixture.ex", __DIR__)
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, Layout, Pagination, PdfWriter, Style}
  alias NativeElixirPdfUtilities.{Limits}

  @checker "<svg xmlns='http://www.w3.org/2000/svg' width='8' height='8'><rect width='8' height='8' fill='#e7edf4'/><rect width='4' height='4' fill='#bed0e2'/></svg>"
  setup do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    %{limits: limits}
  end

  test "background SVG retains its original source and image-policy dimensions" do
    uri = "data:image/svg+xml;base64," <> Base.encode64(@checker)

    html =
      "<div style='width:36pt;height:24pt;background-image:url(#{uri});background-size:6pt 6pt'></div>"

    assert {:ok, dom} = HtmlParser.parse_detailed(html)
    assert {:ok, styled} = Style.compute_detailed(dom, system_font_discovery: false)
    [node] = styled.children
    image = node.style.background_image
    assert image.format == :png
    assert {image.width_px, image.height_px} == {8, 8}
    assert {:ok, layout} = Layout.layout(styled, page_size: {80, 80}, margin: 0)
    assert {:ok, pages} = Pagination.paginate(layout)

    tiles =
      for page <- pages, box <- page.boxes, box.type == :image do
        box
      end

    assert length(tiles) >= 24

    assert Enum.all?(
             tiles,
             &(&1.image.data == image.data and &1.width == 6.0 and &1.height == 6.0)
           )

    assert {:ok, pdf} = PdfWriter.render(pages)
    assert pdf =~ "/Subtype /Image"
  end

  test("background source rasterization respects actual SVG output and tile budgets", %{
    limits: limits
  }) do
    uri = "data:image/svg+xml;base64," <> Base.encode64(@checker)

    source =
      "<div style='width:36pt;height:24pt;background-image:url(#{uri});background-size:6pt 6pt'></div>"

    Limits.install(Map.put(limits, :max_svg_output_bytes, 1))

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             NativeElixirPdfUtilities.HtmlToPdf.render(source)

    assert diagnostic.message =~ "output"
    assert is_binary(diagnostic.source)
    assert diagnostic.reason == :resource_limit_exceeded
    Limits.install(Map.put(limits, :max_background_image_tiles, 1))

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             NativeElixirPdfUtilities.HtmlToPdf.render(source)

    assert diagnostic.stage == :limits
    assert diagnostic.reason == :resource_limit_exceeded
    assert diagnostic.message =~ "tile"
  end

  alias NativeElixirPdfUtilities.{Limits, Text}
  alias NativeElixirPdfUtilities.HtmlToPdf.SvgRasterizer

  @checkerx "<svg xmlns='http://www.w3.org/2000/svg' width='8' height='8'><rect width='8' height='8' fill='#e7edf4'/><rect width='4' height='4' fill='#bed0e2'/></svg>"

  test "original nested three-page backgrounds retain native layers and retain all text" do
    source =
      File.read!(
        "test/fixtures/html_to_pdf/browser_parity/html_decorated_background_fragments.html"
      )

    assert {:ok, dom} = HtmlParser.parse_detailed(source)
    assert {:ok, styled} = Style.compute_detailed(dom, system_font_discovery: false)
    assert {:ok, layout} = Layout.layout(styled, page_size: {210, 180}, margin: 12)
    assert {:ok, pages} = Pagination.paginate(layout)
    assert {:ok, pdf} = PdfWriter.render(pages)
    assert {:ok, document} = NativeElixirPdfUtilities.Pdf.Reader.read(pdf)
    assert length(document.pages) == 3

    images =
      Enum.filter(document.objects, fn {_id, object} ->
        match?(%{value: %{"Subtype" => {:name, "Image"}}}, object)
      end)

    assert length(images) <= 3
    assert {:ok, text} = Text.extract(pdf)

    for value <- [
          "Before nested backgrounds",
          "Outer opening",
          "Nested line 18",
          "Outer ending",
          "After nested backgrounds"
        ] do
      assert text =~ value
    end
  end

  test "fractional tile translations retain genuine half and quarter coverage" do
    assert {:ok, png} = SvgRasterizer.rasterize(@checkerx, [], nil)
    assert {:ok, image} = NativeElixirPdfUtilities.HtmlToPdf.PngDecoder.decode(png, nil, false)
    clip = %{x: 0.0, y: 0.0, width: 12.0, height: 12.0}

    tiles =
      for x <- [-4.5, 1.5, 7.5], y <- [-1.5, 4.5, 10.5] do
        %{type: :image, x: x, y: y, width: 6.0, height: 6.0, image: image, clip: clip}
      end

    assert {:ok, pdf} = PdfWriter.render([%{size: {12, 12}, boxes: tiles}])
    base = Path.join(System.tmp_dir!(), "svg-original-tile-1202")

    on_exit(fn ->
      File.rm(base <> ".pdf")
      File.rm(base <> ".ppm")
    end)

    File.write!(base <> ".pdf", pdf)
    assert {_, 0} = System.cmd("pdftoppm", ["-singlefile", "-r", "72", base <> ".pdf", base])
    [_header, pixels] = :binary.split(File.read!(base <> ".ppm"), "\n255\n")
    pixel = fn x, y -> binary_part(pixels, (y * 12 + x) * 3, 3) end

    # The fractional tile origins must survive transport verbatim in the PDF
    # content stream. Chromium itself snaps background tiles to whole device
    # pixels at paint time (Chromium 152 print output renders these same nine
    # 6px tiles with ceil-snapped origins: pixels (1,1), (2,2) and (5,5) all
    # rasterize to the flat background color e7edf4), and the Poppler raster
    # below snaps with its own phase. The former exact blend expectations
    # (<<221, 230, 240>> half coverage and <<190, 208, 226>> at (2,2)) were
    # artifacts of the deleted first-party painter's supersampled compositor,
    # which neither Chromium nor the PDF rasterizer reproduces.
    assert pdf =~ "6 0 0 6 1.5 -1.5 cm"
    assert pdf =~ "6 0 0 6 -4.5 10.5 cm"

    tile_colors = [<<231, 237, 244>>, <<190, 208, 226>>]

    for y <- 0..11, x <- 0..11 do
      assert pixel.(x, y) in tile_colors
    end

    assert pixel.(1, 1) == <<231, 237, 244>>
    assert pixel.(5, 5) == <<231, 237, 244>>
  end
end
