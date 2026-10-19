alias NativeElixirPdfUtilities.TestSupport.Paths

Code.require_file(Paths.support("png_fixture.ex"))

defmodule NativeElixirPdfUtilities.HtmlToPdf.ImageResourceValidationTest do
  use ExUnit.Case, async: false

  alias NativeElixirPdfUtilities.{HtmlToPdf, Limits}
  alias NativeElixirPdfUtilities.HtmlToPdf.{Font, HtmlParser, ResourceContext, SvgDocumentImage}
  alias NativeElixirPdfUtilities.TestSupport.PngFixture

  alias NativeElixirPdfUtilities.Validators.{
    HtmlValidator,
    PngValidator,
    PrintMediaValidator,
    ResponsiveImageValidator,
    SvgDocumentImageValidator,
    SvgDocumentViewportValidator,
    SvgImageValidator,
    SvgStyleValidator,
    SourceSizeValidator,
    FontValidator
  }

  setup do
    limits = Limits.effective()

    directory =
      Path.join(System.tmp_dir!(), "image-viewport-#{System.unique_integer([:positive])}")

    File.mkdir_p!(directory)

    on_exit(fn ->
      Limits.install(limits)
      File.rm_rf!(directory)
    end)

    %{directory: directory, limits: limits}
  end

  test "malformed compressed PNG scanlines retain their specific public diagnostics" do
    header = PngFixture.chunk("IHDR", <<1::32, 1::32, 8, 2, 0, 0, 0>>)

    for {compressed, explanation} <- [
          {"not zlib", "compressed data is invalid or truncated"},
          {:zlib.compress(<<0, 255, 0, 0, 1>>), "exceeds the expected scanline size"},
          {:zlib.compress(<<0, 255>>), "shorter than the expected scanlines"},
          {:zlib.compress(<<5, 255, 0, 0>>), "filter must be between 0 and 4"}
        ] do
      png =
        <<137, 80, 78, 71, 13, 10, 26, 10>> <>
          header <> PngFixture.chunk("IDAT", compressed) <> PngFixture.chunk("IEND", "")

      assert {:error, {:invalid_document, diagnostic}} =
               PngValidator.prepare_decode(png, HtmlValidator.new_image_budget(), true)

      assert diagnostic.stage == :style and diagnostic.source == "PNG"
      assert diagnostic.message =~ explanation
    end
  end

  test "empty source-size entries keep the selected print width and propagate font failures" do
    assert {:ok, context} = PrintMediaValidator.context(print_viewport: {800, 600})
    assert ResponsiveImageValidator.source_size(", /* empty */ ,", context) == 800

    assert {:ok, failed_context} =
             PrintMediaValidator.context(
               print_viewport: {800, 600},
               fonts: [%{family: "Broken", data: "invalid font bytes"}],
               system_font_discovery: false
             )

    assert {:error, {:invalid_font, diagnostic}} =
             ResponsiveImageValidator.source_size("1ex", failed_context)

    assert diagnostic.module == SourceSizeValidator and diagnostic.stage == :font
    assert diagnostic.operation == :prepare_context
  end

  test "SVG marker shorthand and stops share canonical authored grammar" do
    assert {:ok, dom} =
             HtmlParser.parse_detailed(
               "<svg><g marker='url(#point)'><path d='M0 0 L1 1'/></g><defs><marker id='point'/></defs></svg>"
             )

    [svg] = dom.children
    [group | _] = svg.children
    assert SvgStyleValidator.presentation_property?(group, "marker")
    assert {:ok, [{"marker", "url(#point)"}]} = SvgStyleValidator.presentation_declarations(group)

    assert {:error, {:invalid_css, diagnostic}} =
             SvgStyleValidator.apply(SvgStyleValidator.defaults(%{}), "marker", "url(outside)")

    assert diagnostic.source == "marker-start"

    for {value, expected} <- [{"0.25", 0.25}, {"50%", 0.5}, {"200%", 1.0}] do
      assert {:ok, ^expected} = SvgStyleValidator.parse("offset", value)
    end

    assert :error = SvgStyleValidator.parse("offset", "invalid")
    assert :error = SvgStyleValidator.parse("stroke-miterlimit", "0.5")
  end

  test "selected SVG document failures retain host and issued request scope", %{
    directory: directory
  } do
    ResourceContext.run(
      [authorization_root: directory],
      HtmlValidator.new_image_budget(),
      fn owner ->
        source = "<svg width='40' height='20'><rect width='40' height='20' fill='red'/></svg>"

        assert {:error, {:invalid_document, host_error}} =
                 SvgDocumentImage.prepare(
                   source,
                   owner.image_budget,
                   "original.svg",
                   :invalid,
                   [],
                   []
                 )

        assert host_error.message =~ "supported host"
        assert host_error.source == "original.svg"

        assert {:ok, descriptor} =
                 SvgDocumentImage.prepare(
                   source,
                   owner.image_budget,
                   "original.svg",
                   :iframe,
                   [],
                   []
                 )

        assert {:ok, request} = SvgDocumentImageValidator.request(descriptor)

        assert {:error, {:invalid_document, diagnostic}} =
                 SvgDocumentViewportValidator.prepare_viewport(
                   request.plan,
                   30,
                   15,
                   owner.image_budget,
                   %{host: :object, layout: nil}
                 )

        assert diagnostic.message =~ "original issued host"

        for context <- [
              %{},
              %{main_axis: :invalid},
              %{main_axis: :row, alignment: :center, containing_height: -1, height: nil}
            ] do
          assert {:error, {:invalid_document, diagnostic}} =
                   SvgDocumentViewportValidator.prepare_layout_context(context)

          assert diagnostic.stage == :layout and diagnostic.operation == :rasterize_document
        end

        assert {:error, {:not_implemented, overflow}} =
                 SvgDocumentImage.compose(descriptor, 10, 10)

        assert overflow.message =~ "initial scrollbar geometry"
        assert overflow.source == "original.svg"
      end
    )
  end

  test "an issued document cannot be requested after its resource scope exits", %{
    directory: directory
  } do
    descriptor =
      ResourceContext.run(
        [authorization_root: directory],
        HtmlValidator.new_image_budget(),
        fn owner ->
          assert {:ok, descriptor} =
                   SvgDocumentImage.prepare(
                     "<svg width='2' height='1'/>",
                     owner.image_budget,
                     "original.svg",
                     :object,
                     [],
                     []
                   )

          descriptor
        end
      )

    assert {:error, {_reason, diagnostic}} = SvgDocumentImageValidator.request(descriptor)
    assert diagnostic.stage == :resource and diagnostic.message =~ "scope"
  end

  test "indefinite embed flex context resolves its initial document canvas", %{
    directory: directory
  } do
    ResourceContext.run(
      [authorization_root: directory],
      HtmlValidator.new_image_budget(),
      fn owner ->
        source =
          "<svg width='100%' height='100%'><rect width='100%' height='100%' fill='red'/></svg>"

        assert {:ok, descriptor} =
                 SvgDocumentImage.prepare(source, owner.image_budget, "embed.svg", :embed, [], [])

        assert {:ok, request} = SvgDocumentImageValidator.request(descriptor)
        layout = %{main_axis: :row, alignment: :stretch, containing_height: nil, height: nil}

        assert {:ok, viewport} =
                 SvgDocumentViewportValidator.prepare_viewport(
                   request.plan,
                   30,
                   15,
                   owner.image_budget,
                   %{host: :embed, layout: layout}
                 )

        assert viewport.root_size == {40.0, 150.0}
        assert {viewport.width, viewport.height} == {30, 15}
      end
    )
  end

  test "selected positive dimensions below native f32 range return a diagnostic", %{
    directory: directory
  } do
    ResourceContext.run(
      [authorization_root: directory],
      HtmlValidator.new_image_budget(),
      fn owner ->
        source = "<svg width='1e-46' height='1'><rect width='1' height='1' fill='red'/></svg>"

        assert {:ok, descriptor} =
                 SvgDocumentImage.prepare(source, owner.image_budget, "tiny.svg", :iframe, [], [])

        assert {:error, {:invalid_document, diagnostic}} =
                 SvgDocumentImage.compose(descriptor, 30, 15)

        assert diagnostic.message =~ "represented by the native viewport"
        assert diagnostic.source == "tiny.svg"
      end
    )
  end

  test "selected document raster accounting charges its issued canvas", %{
    directory: directory,
    limits: limits
  } do
    ResourceContext.run(
      [authorization_root: directory],
      HtmlValidator.new_image_budget(),
      fn owner ->
        assert {:ok, descriptor} =
                 SvgDocumentImage.prepare(
                   "<svg width='2' height='1'/>",
                   owner.image_budget,
                   "canvas.svg",
                   :object,
                   [],
                   []
                 )

        assert {:ok, request} = SvgDocumentImageValidator.request(descriptor)

        assert {:ok, :skipped} =
                 SvgDocumentViewportValidator.prepare_raster(
                   request.plan,
                   0,
                   0,
                   owner.image_budget
                 )

        Limits.install(%{limits | max_aggregate_svg_surface_pixels: 1})

        assert {:error, {:resource_limit_exceeded, diagnostic}} =
                 SvgDocumentViewportValidator.prepare_raster(
                   request.plan,
                   1.5,
                   0.75,
                   owner.image_budget
                 )

        assert diagnostic.source == "SVG"
        assert diagnostic.module == HtmlValidator
        assert diagnostic.operation == :rasterize_document
        assert diagnostic.message =~ "max_aggregate_svg_surface_pixels"
      end
    )
  end

  test "an explicit outside-root image mapping does not authorize its sibling files", %{
    directory: directory
  } do
    root = Path.join(directory, "authorized")
    File.mkdir_p!(root)
    source = Path.join(directory, "mapped.svg")
    File.write!(source, "<svg width='2' height='1'><rect width='2' height='1' fill='red'/></svg>")
    opts = [base_url: root, assets: %{"mapped.svg" => {:file, source}}]
    html = "<svg width='2' height='1'><image href='mapped.svg' width='2' height='1'/></svg>"
    assert {:ok, _} = HtmlToPdf.render(html, opts)
    sibling = Path.join(directory, "sibling.png")
    File.write!(sibling, PngFixture.build(1, 1, 2, 8, 0, fn _, _ -> [255, 0, 0] end))

    File.write!(
      source,
      "<svg width='2' height='1'><image href='sibling.png' width='2' height='1'/></svg>"
    )

    assert {:error, {_reason, diagnostic}} = HtmlToPdf.render(html, opts)
    assert diagnostic.stage == :resource
    assert diagnostic.message =~ "local"
  end

  test "a converter staging failure returns a recoverable public diagnostic", %{
    directory: directory
  } do
    before = System.get_env("TMPDIR")
    System.put_env("TMPDIR", "/proc/self/fd")
    assert System.tmp_dir!() == "/proc/self/fd"

    try do
      assert {:error, {:invalid_document, diagnostic}} =
               NativeElixirPdfUtilities.HtmlToPdf.SvgRasterizer.rasterize(
                 "<svg width='1' height='1'><rect width='1' height='1' fill='red'/></svg>",
                 [],
                 nil
               )

      assert diagnostic.message =~ "stage approved SVG font bytes"
      assert diagnostic.operation == :convert
      assert File.ls!(directory) == []
    after
      if before, do: System.put_env("TMPDIR", before), else: System.delete_env("TMPDIR")
    end
  end

  test "a raw SVG size below converter representation returns its backend explanation" do
    assert {:error, {:invalid_document, diagnostic}} =
             NativeElixirPdfUtilities.HtmlToPdf.SvgRasterizer.rasterize(
               "<svg width='1e-46' height='1'><rect width='1' height='1'/></svg>",
               [],
               nil
             )

    assert diagnostic.source == "SVG" and diagnostic.stage == :style
    assert diagnostic.message =~ "SVG conversion failed"
    assert diagnostic.message =~ "invalid size"
  end

  test "ideographic source sizing measures approved upstream water glyph metrics" do
    data = File.read!("test/fixtures/fonts/source-metrics-water.ttf")
    assert {:ok, family} = FontValidator.prepare_font_family_name(data)

    opts = [
      fonts: [%{family: family, data: data}],
      default_font: family,
      system_font_discovery: false
    ]

    assert {:ok, registry} = Font.load_registry(opts)
    assert {:ok, _, face} = Font.resolve(family, 400, :normal, registry)
    assert face.cmap[?水] > 0
    assert Font.supports_text?(face, "水")
    assert {:ok, context} = PrintMediaValidator.context(opts)
    assert {:ok, measured} = SourceSizeValidator.prepare_context(context, ["2ic"], opts)
    assert measured.source_metrics.ic == 16.0
    assert_in_delta measured.source_metrics.ic, Font.text_width("水", face, 16), 0.000001
    assert {:ok, 32.0} = PrintMediaValidator.source_length("2ic", measured)
  end

  test "image data URI failures are reported before child resource resolution", %{limits: limits} do
    assert :error =
             SvgImageValidator.data_uri(
               "data:image/png;base64,A===",
               HtmlValidator.new_image_budget()
             )

    for reference <- ["data:image/png;base64,A===", "data:image/png,unsupported"] do
      assert {:error, {:invalid_document, diagnostic}} =
               HtmlToPdf.render(
                 "<svg width='2' height='1'><image href='#{reference}' width='2' height='1'/></svg>"
               )

      assert diagnostic.message =~ "supported base64"
    end

    png = PngFixture.build(1, 1, 2, 8, 0, fn _, _ -> [255, 0, 0] end)
    reference = "data:image/png;base64," <> Base.encode64(png)

    assert {:ok, _} =
             HtmlToPdf.render(
               "<svg width='2' height='1'><image href='#{reference}' width='2' height='1'/></svg>"
             )

    Limits.install(%{limits | max_image_source_bytes: 1})

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             HtmlToPdf.render(
               "<svg width='2' height='1'><image href='#{reference}' width='2' height='1'/></svg>"
             )

    assert diagnostic.message =~ "source"
  end

  test "selected resource bytes retain SVG size and format diagnostics", %{limits: limits} do
    html = "<svg width='2' height='1'><image href='source.bin' width='2' height='1'/></svg>"

    assert {:error, {:invalid_document, diagnostic}} =
             HtmlToPdf.render(html, assets: %{"source.bin" => {:bytes, <<255, 1>>}})

    assert diagnostic.source == "source.bin"
    assert diagnostic.message =~ "PNG, JPEG or SVG artwork"

    png = PngFixture.build(1, 1, 2, 8, 0, fn _, _ -> [255, 0, 0] end)
    Limits.install(%{limits | max_svg_resource_bytes: 1})

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             HtmlToPdf.render(html, assets: %{"source.bin" => {:bytes, png}})

    assert diagnostic.message =~ "max_svg_resource_bytes"
    assert diagnostic.source == "source.bin"
  end
end
