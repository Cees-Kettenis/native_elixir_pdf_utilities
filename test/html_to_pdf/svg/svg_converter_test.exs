defmodule NativeElixirPdfUtilities.HtmlToPdf.SvgConverterTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.HtmlToPdf.{SvgRasterizer, SvgScene, PngDecoder}
  alias NativeElixirPdfUtilities.Validators.{SvgConverterValidator, HtmlValidator}
  alias NativeElixirPdfUtilities.Limits

  setup do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    :ok
  end

  test "the existing converter produces PNG pixels from complete SVG" do
    source =
      "<svg xmlns='http://www.w3.org/2000/svg' width='8' height='4'><rect width='8' height='4' fill='red'/></svg>"

    before = Path.wildcard(Path.join(System.tmp_dir!(), "nepu-svg-#{System.pid()}-*"))
    assert {:ok, png} = SvgRasterizer.rasterize(source, [], nil)
    assert <<137, 80, 78, 71, 13, 10, 26, 10, _::binary>> = png

    assert {:ok, %{format: :png, width_px: 8, height_px: 4, data: rgb}} =
             PngDecoder.decode(png, nil, false)

    assert rgb == :binary.copy(<<255, 0, 0>>, 32)
    assert Path.wildcard(Path.join(System.tmp_dir!(), "nepu-svg-#{System.pid()}-*")) == before
  end

  test "approved text bytes reach the converter without rereading their original path" do
    data = File.read!("priv/fonts/dejavu/DejaVuSans.ttf")

    source =
      "<svg xmlns='http://www.w3.org/2000/svg' width='50' height='20'><text x='0' y='15' font-family='DejaVu Sans'>Text</text></svg>"

    assert {:ok, png} =
             SvgRasterizer.rasterize(source, [], nil, [%{data: data, family: "DejaVu Sans"}])

    assert {:ok, image} = PngDecoder.decode(png, nil, false)
    assert Enum.any?(:binary.bin_to_list(image.alpha_data), &(&1 > 0))
    assert {:error, {:unsupported_html, diagnostic}} = SvgRasterizer.rasterize(source, [], nil)
    assert diagnostic.source == "SVG text"
  end

  test "authorized nested SVG is self-contained before the converter sees it" do
    nested =
      "<svg xmlns='http://www.w3.org/2000/svg' width='4' height='4'><rect width='4' height='4' fill='blue'/></svg>"

    source =
      "<svg xmlns='http://www.w3.org/2000/svg' width='4' height='4'><image href='svg-resource:child' width='4' height='4'/></svg>"

    context = %{
      mode: :inline,
      resources: %{"svg-resource:child" => %{data: nested, media_type: "image/svg+xml"}}
    }

    assert {:ok, request} = SvgConverterValidator.prepare(source, [], nil, [], context)
    assert request.source =~ "data:image/svg+xml;base64,"
    refute request.source =~ "svg-resource:child"
    assert {:ok, png} = SvgRasterizer.rasterize(source, [], nil, [], context)
    assert {:ok, image} = PngDecoder.decode(png, nil, false)
    assert image.data == :binary.copy(<<0, 0, 255>>, 16)
  end

  test "a smaller document root remains smaller in its child viewport" do
    source =
      "<svg xmlns='http://www.w3.org/2000/svg' width='2' height='2'><rect width='2' height='2' fill='red'/></svg>"

    assert {:ok, projected} =
             SvgConverterValidator.with_document_viewport(source, {2, 2}, {4, 4}, %{mode: :image})

    assert {:ok, png} = SvgRasterizer.rasterize(projected, [], nil)
    assert {:ok, image} = PngDecoder.decode(png, nil, false)
    assert :binary.at(image.alpha_data, 0) == 255
    assert :binary.at(image.alpha_data, 15) == 0
  end

  test "unapproved paths, executable input and malformed XML fail before backend resolution" do
    for content <- ["<image href='/etc/passwd'/>", "<script>alert(1)</script>", "<rect>"] do
      source = "<svg xmlns='http://www.w3.org/2000/svg' width='4' height='4'>#{content}</svg>"
      assert {:error, {_reason, diagnostic}} = SvgRasterizer.rasterize(source, [], nil)
      assert diagnostic.source == "SVG" and is_binary(diagnostic.message)
    end

    assert {:error, {:invalid_options, _}} = SvgRasterizer.rasterize("bad", [], nil, [], :unbound)
  end

  test "converter allocation retains the canonical SVG raster and surface limits" do
    source =
      "<svg xmlns='http://www.w3.org/2000/svg' width='8' height='4'><rect width='8' height='4'/></svg>"

    Limits.install(%{Limits.effective() | max_svg_raster_pixels: 8})

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             SvgRasterizer.rasterize(source, [], nil)

    assert diagnostic.message =~ "pixel"
  end

  test "inline host CSS is extracted then embedded as a raster image" do
    source =
      "<svg width='8' height='4'><style>rect{fill:blue}</style><rect width='8' height='4'/></svg>"

    budget = HtmlValidator.new_image_budget()
    assert {:ok, scene} = SvgScene.extract(source, budget, system_font_discovery: false)
    assert scene.source =~ "fill=\"rgb(0,0,255)\""
    assert {:ok, %{image: %{format: :png} = image}} = SvgScene.paint(scene, 6)
    assert image.data == :binary.copy(<<0, 0, 255>>, image.width_px * image.height_px)
    refute Map.has_key?(image, :svg_glyph_runs)
  end

  test "inline configured font aliases serialize their approved font family" do
    data = File.read!("priv/fonts/dejavu/DejaVuSans.ttf")

    source =
      "<svg width='50' height='20' style='font-family:Readable'><text x='0' y='15'>Text</text></svg>"

    budget = HtmlValidator.new_image_budget()

    assert {:ok, scene} =
             SvgScene.extract(source, budget,
               system_font_discovery: false,
               fonts: [%{family: "Readable", data: data}]
             )

    assert scene.source =~ "font-family=\"DejaVu Sans\""
    refute scene.source =~ "NativeSvgFont"
    assert {:ok, %{image: image} = painted} = SvgScene.paint(scene, 37.5)
    assert Enum.any?(:binary.bin_to_list(image.alpha_data), &(&1 > 0))
    refute Map.has_key?(painted, :logical_text)
    refute Map.has_key?(image, :svg_glyph_runs)
  end

  test "document root failures use the canonical extraction diagnostic" do
    for source <- ["<p>Text</p>", "<svg></svg><svg></svg>"] do
      assert {:error, {:invalid_document, diagnostic}} =
               SvgScene.extract(
                 source,
                 HtmlValidator.new_image_budget(),
                 system_font_discovery: false
               )

      assert diagnostic.source == "SVG"
      assert diagnostic.message =~ "exactly one complete SVG"
    end
  end

  test "font staging failures surface recoverable diagnostics when snapshots cannot be written" do
    # Grow a scratch directory until the converter's font snapshot file name
    # would exceed the kernel path limit, so the per-font file write fails
    # after the staging directory itself was created successfully.
    # The staging directory is TMPDIR <> "/nepu-svg-" <> pid <> "-" <> 32 hex
    # characters and the first font snapshot appends "/0.ttf".
    pid_size = byte_size(System.pid())
    target = 4051 - pid_size

    base =
      Path.join(System.tmp_dir!(), "nepu-staging-#{System.unique_integer([:positive])}")

    File.mkdir_p!(base)

    directory =
      1..40
      |> Enum.reduce_while(base, fn _, current ->
        if target - byte_size(Path.join(current, String.duplicate("d", 200))) >= 2,
          do: {:cont, Path.join(current, String.duplicate("d", 200))},
          else: {:halt, current}
      end)
      |> then(fn current ->
        filler = target - byte_size(current) - 1
        if filler > 0, do: Path.join(current, String.duplicate("d", filler)), else: current
      end)

    File.mkdir_p!(directory)
    assert byte_size(directory) == target

    previous = System.get_env("TMPDIR")
    System.put_env("TMPDIR", directory)

    try do
      font = %{data: File.read!("priv/fonts/dejavu/DejaVuSans.ttf"), family: "DejaVu Sans"}

      source =
        "<svg xmlns='http://www.w3.org/2000/svg' width='4' height='4'><rect width='4' height='4' fill='red'/></svg>"

      assert {:error, {:invalid_document, diagnostic}} =
               SvgRasterizer.rasterize(source, [], nil, [font])

      assert diagnostic.module == SvgConverterValidator
      assert diagnostic.operation == :convert
      assert diagnostic.message =~ "could not stage approved SVG font bytes"
      assert diagnostic.message =~ "enametoolong"
    after
      if previous, do: System.put_env("TMPDIR", previous), else: System.delete_env("TMPDIR")
      File.rm_rf!(base)
    end
  end
end
