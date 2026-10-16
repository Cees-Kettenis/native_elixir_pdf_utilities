Code.require_file("../support/svg_public_workflow.ex", __DIR__)
Code.require_file("../support/png_fixture.ex", __DIR__)

defmodule NativeElixirPdfUtilities.HtmlToPdf.SvgFilterResourcesTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.{HtmlToPdf, Text, Limits}
  alias NativeElixirPdfUtilities.Pdf.Reader
  alias NativeElixirPdfUtilities.TestSupport.PngFixture

  setup do
    limits = Limits.effective()

    directory =
      Path.join(System.tmp_dir!(), "svg-filter-inputs-#{System.unique_integer([:positive])}")

    File.mkdir_p!(Path.join(directory, "nested"))

    png =
      PngFixture.build(2, 1, 2, 8, 0, fn x, _ ->
        if x == 0 do
          [255, 0, 0]
        else
          [0, 0, 255]
        end
      end)

    File.write!(Path.join(directory, "nested/paint.png"), png)

    on_exit(fn ->
      Limits.install(limits)
      File.rm_rf!(directory)
    end)

    %{directory: directory, png: png, limits: limits}
  end

  test("authorized PNG feImage inputs genuinely paint independent color pixels", %{png: png}) do
    html =
      "<svg width='100' height='50'><defs><filter id='f' filterUnits='userSpaceOnUse' x='0' y='0' width='100' height='50'><feImage href='paint.png' preserveAspectRatio='none'/></filter></defs><rect width='100' height='50' filter='url(#f)'/></svg>"

    assert {:ok, pdf} = HtmlToPdf.render(html, assets: %{"paint.png" => {:bytes, png}})
    assert {:ok, document} = Reader.read(pdf)

    pixels =
      for {reference, object} <- document.objects,
          is_map(object.value) and object.value["Subtype"] == {:name, "Image"} and
            object.value["ColorSpace"] == {:name, "DeviceRGB"},
          {:ok, stream} = Reader.decoded_stream(document, {:ref, reference}),
          <<r, g, b <- stream>> do
        {r, g, b}
      end

    assert Enum.any?(pixels, fn {r, g, b} -> r > 240 and g < 10 and b < 10 end)
    assert Enum.any?(pixels, fn {r, g, b} -> b > 240 and g < 10 and r < 10 end)
    refute pdf =~ "/ActualText"
    assert {:error, {:no_extractable_text, _}} = Text.extract(pdf, layout: false)
  end

  test "selected JPEG filter inputs use genuine bytes and diagnose corrupt JPEGs" do
    jpeg =
      File.read!(
        Path.expand("../fixtures/html_to_pdf/browser_parity/assets/embedded-neutral.jpg", __DIR__)
      )

    html =
      "<svg width='80' height='40'><defs><filter id='f' filterUnits='userSpaceOnUse' x='0' y='0' width='80' height='40'><feImage href='photo.jpg' preserveAspectRatio='none'/></filter></defs><rect width='80' height='40' filter='url(#f)'/></svg>"

    assert {:ok, pdf} = HtmlToPdf.render(html, assets: %{"photo.jpg" => {:bytes, jpeg}})
    assert pdf =~ "/Subtype /Image"

    assert {:error, {:invalid_document, diagnostic}} =
             HtmlToPdf.render(html, assets: %{"photo.jpg" => {:bytes, <<255, 216, 0>>}})

    assert diagnostic.stage == :style
    assert diagnostic.message =~ "JPEG resource bytes are invalid"
    assert diagnostic.source == "SVG"
    assert diagnostic.module == NativeElixirPdfUtilities.Validators.SvgConverterValidator
    assert diagnostic.operation == :convert
  end

  test("local image targets reuse their actual byte resource input", %{png: png}) do
    html =
      "<svg width='100' height='50'><defs><image id='input' href='paint.png' width='100' height='50' preserveAspectRatio='none'/><filter id='f' filterUnits='userSpaceOnUse' x='0' y='0' width='100' height='50'><feImage href='#input'/></filter></defs><rect width='100' height='50' filter='url(#f)'/></svg>"

    assert {:ok, pdf} = HtmlToPdf.render(html, assets: %{"paint.png" => {:bytes, png}})
    assert pdf =~ "/Subtype /Image"
  end

  test("nested SVG feImage inputs select actual own text and relative sibling resources", %{
    directory: directory
  }) do
    File.write!(
      Path.join(directory, "nested/words.svg"),
      "<svg xmlns='http://www.w3.org/2000/svg' width='140' height='40'><image href='paint.png' width='20' height='10'/><text x='5' y='30' font-family='DejaVu Sans' font-size='18'>Independent Ω</text></svg>"
    )

    html =
      "<svg width='160' height='50'><defs><filter id='f' filterUnits='userSpaceOnUse' x='0' y='0' width='140' height='40'><feImage href='nested/words.svg'/></filter></defs><g filter='url(#f)'><text x='0' y='25'>Unpainted source</text></g></svg>"

    assert {:ok, pdf} = HtmlToPdf.render(html, base_url: directory)
    assert pdf =~ "/Subtype /Image"

    (
      assert :ok =
               NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(
                 pdf,
                 true
               )

      assert {:error, {:no_extractable_text, _}} =
               NativeElixirPdfUtilities.Text.extract(pdf, layout: false)

      refute pdf =~ "/Subtype /Link"
    )
  end

  test "nested image viewport crop prevents logical leakage beyond its actual painted image" do
    nested =
      "<svg xmlns='http://www.w3.org/2000/svg' width='80' height='20'><rect width='80' height='20' fill='blue'/><text x='5' y='42' font-family='DejaVu Sans' font-size='18'>Y</text></svg>"

    html =
      "<svg width='160' height='80'><defs><filter id='f' filterUnits='userSpaceOnUse' x='0' y='0' width='160' height='80'><feImage href='nested.svg'/></filter></defs><rect width='160' height='80' filter='url(#f)'/></svg>"

    assert {:ok, pdf} = HtmlToPdf.render(html, assets: %{"nested.svg" => {:bytes, nested}})
    assert {:error, {:no_extractable_text, _}} = Text.extract(pdf, layout: false)
  end

  test("unused filter inputs never invoke a provider", %{png: png}) do
    parent = self()

    resolver = fn %{reference: reference} ->
      send(parent, {:loaded, reference})
      {:ok, png}
    end

    html =
      "<svg width='100' height='50'><defs><filter id='unused'><feImage href='unused.png'/></filter><filter id='selected'><feImage href='selected.png'/></filter></defs><rect width='100' height='50' filter='url(#selected)'/></svg>"

    assert {:ok, _} = HtmlToPdf.render(html, asset_resolver: resolver)
    assert_received {:loaded, "selected.png"}
    refute_received {:loaded, "unused.png"}
  end

  test(
    "nested HTML filter image input samples its genuine scoped native surface",
    %{directory: directory}
  ) do
    File.write!(
      Path.join(directory, "nested/html.svg"),
      "<svg xmlns='http://www.w3.org/2000/svg' width='80' height='40'><foreignObject width='80' height='40'><p xmlns='http://www.w3.org/1999/xhtml'>Actual HTML input</p></foreignObject></svg>"
    )

    html =
      "<svg width='80' height='40'><defs><filter id='f' filterUnits='userSpaceOnUse' width='80' height='40'><feImage href='nested/html.svg'/></filter></defs><rect width='80' height='40' filter='url(#f)'/></svg>"

    (
      assert {:error, {:unsupported_html, diagnostic}} =
               HtmlToPdf.render(html, base_url: directory)

      assert diagnostic.reason == :unsupported_html
      assert diagnostic.stage == :layout
      assert diagnostic.operation == :convert
      assert diagnostic.module == NativeElixirPdfUtilities.Validators.SvgConverterValidator
      assert diagnostic.source == "foreignObject"
      assert diagnostic.message =~ "foreignObject"
    )
  end

  test("filter input decoded image work is charged before backend allocation", %{limits: limits}) do
    png = PngFixture.build(8, 8, 2, 8, 0, fn _, _ -> [255, 0, 0] end)
    Limits.install(Map.put(limits, :max_aggregate_decoded_image_bytes, 100))

    html =
      "<svg width='80' height='40'><defs><filter id='f'><feImage href='paint.png'/></filter></defs><rect width='80' height='40' filter='url(#f)'/></svg>"

    assert {:error, {:resource_limit_exceeded, %{message: message}}} =
             HtmlToPdf.render(html, assets: %{"paint.png" => {:bytes, png}})

    assert message =~ "decoded image"
  end

  test("repeated shared filter image resources charge each actual decoded occurrence", %{
    png: png,
    limits: limits
  }) do
    Limits.install(Map.put(limits, :max_aggregate_decoded_image_bytes, 44))

    html =
      "<svg width='100' height='50'><defs><filter id='f'><feImage href='paint.png'/></filter></defs><rect width='20' height='10' filter='url(#f)'/><rect x='30' width='20' height='10' filter='url(#f)'/><rect x='60' width='20' height='10' filter='url(#f)'/></svg>"

    assert {:error, {:resource_limit_exceeded, %{message: message}}} =
             HtmlToPdf.render(html, assets: %{"paint.png" => {:bytes, png}})

    assert message =~ "decoded image"
  end

  test("nested filter image inputs retain resource root and ancestor cycle diagnostics", %{
    directory: directory,
    png: png
  }) do
    parent = self()

    for {reference, reason} <- [
          {"../../private.png", :resource_denied},
          {"https://private.invalid/art.png", :resource_denied},
          {"words.svg", :resource_cycle}
        ] do
      File.write!(
        Path.join(directory, "nested/words.svg"),
        "<svg xmlns='http://www.w3.org/2000/svg' width='30' height='20'><defs><filter id='f'><feImage href='#{reference}'/></filter></defs><rect width='30' height='20' filter='url(#f)'/></svg>"
      )

      html =
        "<svg width='30' height='20'><defs><filter id='f'><feImage href='nested/words.svg'/></filter></defs><rect width='30' height='20' filter='url(#f)'/></svg>"

      assert {:error, {^reason, diagnostic}} =
               HtmlToPdf.render(html,
                 base_url: directory,
                 assets: %{
                   reference =>
                     {:bytes,
                      if reason == :resource_cycle do
                        File.read!(Path.join(directory, "nested/words.svg"))
                      else
                        png
                      end},
                   "nested/words.svg" => {:file, Path.join(directory, "nested/words.svg")}
                 },
                 asset_resolver: fn request ->
                   send(parent, {:resolver, request})
                   {:ok, png}
                 end
               )

      assert diagnostic.source == reference
      assert diagnostic.message != ""
      refute_received {:resolver, _}
    end
  end
end
