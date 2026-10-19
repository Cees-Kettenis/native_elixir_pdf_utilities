alias NativeElixirPdfUtilities.TestSupport.Paths

defmodule NativeElixirPdfUtilities.HtmlToPdf.SvgBackgroundTransportTest do
  Code.require_file(Paths.support("svg_public_workflow.ex"))
  Code.require_file(Paths.support("png_fixture.ex"))
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, Layout, Pagination, PdfWriter, Style}
  alias NativeElixirPdfUtilities.{Limits, Text}

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
    assert {image.width_px, image.height_px} == {16, 16}
    assert {image.width, image.height} == {6.0, 6.0}
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
end
