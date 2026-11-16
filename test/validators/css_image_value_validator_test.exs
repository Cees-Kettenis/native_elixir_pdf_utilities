defmodule NativeElixirPdfUtilities.Validators.CssImageValueValidatorTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.{Diagnostics, Limits}
  alias NativeElixirPdfUtilities.HtmlToPdf.{CssImageValue, HtmlParser, Style}

  alias NativeElixirPdfUtilities.Validators.{
    CssImageValueValidator,
    HtmlValidator,
    WriterValidator
  }

  @style %{color: {0, 0, 0}, font_size: 12.0, _root_font_size: 12.0}
  @image %{
    format: :png,
    data: <<255, 0, 0>>,
    width_px: 1,
    height_px: 1,
    width: 0.75,
    height: 0.75,
    color_space: :device_rgb,
    bits_per_component: 8,
    decoded_stride: 4
  }

  test "image fallback loads the selected URL and retains decode, denial and resource failures" do
    {:ok, fallback} = CssImageValueValidator.prepare("image(rtl url(missing), red)", @style)

    invalid =
      Diagnostics.error(:image, :invalid_document, "cannot decode missing image",
        source: "missing"
      )

    assert {:ok, %{kind: :solid, direction: :rtl}} =
             CssImageValue.load(fallback, [], fn "missing" -> invalid end)

    assert {:ok, %{kind: :decoded, direction: :rtl, image: @image}} =
             CssImageValue.load(fallback, [], fn "missing" -> {:ok, @image} end)

    {:ok, bare} = CssImageValueValidator.prepare("image(url(missing))", @style)
    assert CssImageValue.load(bare, [], fn _ -> invalid end) == invalid

    for reason <- [:resource_limit_exceeded, :resource_denied, :unsupported_html] do
      error =
        Diagnostics.error(:image, reason, "keep the original resource failure", source: "missing")

      assert CssImageValue.load(fallback, [], fn _ -> error end) == error
    end

    assert {:ok, nil} = CssImageValue.load(nil, [], fn _ -> flunk("no selected resource") end)

    {:ok, unsupported} =
      CssImageValueValidator.prepare("image-set(url(unused) type('image/unsupported'))", @style)

    assert {:ok, nil} =
             CssImageValue.load(unsupported, [], fn _ ->
               flunk("unsupported candidate was loaded")
             end)
  end

  test "source grammar handles MIME case, nested-set rejection and source policy diagnostics" do
    {:ok, set} =
      CssImageValueValidator.prepare("image-set(url(one) 1x tYpE('image/png'))", @style)

    assert %{kind: :url, source: "one"} = CssImageValueValidator.select(set, 1)
    assert :error = CssImageValueValidator.prepare("image-set(image-set(url(a)))", @style)

    assert {:error, {:unsupported_html, %{source: "background-image"}}} =
             CssImageValueValidator.prepare("url('javascript:run()')", @style)

    for source <- [
          "image(red,blue)",
          "image(none,red)",
          "image(none)",
          "image(,red)",
          "image(url(x),invalid)",
          "image-set(url(x) type('image/png') type('image/jpeg'))"
        ] do
      refute match?({:ok, _}, CssImageValueValidator.prepare(source, @style))
    end

    {:ok, set} =
      CssImageValueValidator.prepare("image-set(linear-gradient(red 1em,blue 2em) 1x)", @style)

    assert {:ok, %{candidates: [%{image: %{stops: [%{position: 12.0}, %{position: 24.0}]}}]}} =
             CssImageValueValidator.compute(set, @style)

    assert {:ok, _} =
             CssImageValueValidator.compute(
               elem(CssImageValueValidator.prepare("image(url(x), red)", @style), 1),
               @style
             )
  end

  test "directional images and every TIFF transform preserve the intended source corners" do
    for {n, matrix} <- [
          {1, [1, 0, 0, 1, 0, 0]},
          {2, [-1, 0, 0, 1, 1, 0]},
          {3, [-1, 0, 0, -1, 1, 1]},
          {4, [1, 0, 0, -1, 0, 1]},
          {5, [0, -1, -1, 0, 1, 1]},
          {6, [0, -1, 1, 0, 0, 1]},
          {7, [0, 1, 1, 0, 0, 0]},
          {8, [0, 1, -1, 0, 1, 0]}
        ] do
      assert CssImageValue.orientation_matrix(n) == matrix
      value = %{kind: :decoded, image: Map.put(@image, :image_orientation, n), direction: :rtl}
      assert {:ok, mirrored} = CssImageValue.rasterize(value, 10, 10, @style)

      assert mirrored.image_orientation ==
               %{1 => 2, 2 => 1, 3 => 4, 4 => 3, 5 => 6, 6 => 5, 7 => 8, 8 => 7}[n]

      assert mirrored.data == @image.data
    end

    refute WriterValidator.valid_image?(Map.put(@image, :image_orientation, 9))

    assert {:ok, %{image_orientation: {:turns, 0, true}}} =
             CssImageValueValidator.prepare_policy("image-orientation", "flip")

    assert {:ok, %{image_orientation: {:turns, 0, false}}} =
             CssImageValueValidator.prepare_policy("image-orientation", "0")

    assert {:ok, %{image_resolution: %{density: 2.0, snap?: true}}} =
             CssImageValueValidator.prepare_policy("image-resolution", "2x snap")
  end

  test "hidden generated images load no assets and generated surfaces retain shared budget diagnostics" do
    {:ok, dom} =
      HtmlParser.parse(
        "<div style='display:none;background-image:linear-gradient(red,blue);border-image-source:url(no-load)'></div>"
      )

    assert {:ok, _} =
             Style.compute(dom, asset_resolver: fn _ -> flunk("hidden resource loaded") end)

    previous = Limits.effective()

    try do
      Limits.install(Map.put(previous, :max_raster_pixels, 1))

      assert {:error, {:resource_limit_exceeded, diagnostic}} =
               CssImageValueValidator.reserve_surface(2, 2, 4)

      assert diagnostic.message =~ "max_raster_pixels"
    after
      Limits.install(previous)
    end

    assert :ok =
             HtmlValidator.with_render_budget(fn ->
               CssImageValueValidator.reserve_surface(1, 1, 1)
             end)
  end

  test "invalid nested URLs retain source denial diagnostics across image functions" do
    for source <- [
          "image(url('javascript:run()'))",
          "image(url('javascript:run()'), red)",
          "image-set(url('javascript:run()') 1x)"
        ] do
      assert {:error, {:unsupported_html, %{source: "background-image"}}} =
               CssImageValueValidator.prepare(source, @style)
    end

    assert :error =
             CssImageValueValidator.prepare("image-set(url(a) type('image/png')extra)", @style)

    assert {:ok, %{image_rendering: :auto}} =
             CssImageValueValidator.prepare_policy("image-rendering", "auto")

    assert {:ok, %{image_orientation: :from_image}} =
             CssImageValueValidator.prepare_policy("image-orientation", "from-image")

    assert {:ok, rotated} =
             CssImageValueValidator.apply_image(
               @image,
               %{image_orientation: {:turns, 1, false}},
               1
             )

    assert rotated.image_orientation == 6

    for value <- [:bad, %{density: 1, from_image?: false, snap?: false, extra: true}] do
      assert {:error, {:invalid_layout, %{source: "img"}}} =
               CssImageValueValidator.validate_style(%{image_resolution: value}, "img")
    end

    assert :ok =
             CssImageValueValidator.validate_style(
               %{image_resolution: %{density: 1, from_image?: false, snap?: true}},
               "img"
             )

    refute CssImageValueValidator.valid_loaded?(%{kind: :decoded, image: %{@image | width: :bad}})
  end

  test "resolution overflow propagates from each loaded image owner" do
    fields = %{image_resolution: %{density: 1.0e-100, from_image?: false, snap?: false}}

    for image_fields <- [
          %{image: @image},
          %{background_images: [%{kind: :decoded, image: @image}]},
          %{border_image_source: %{kind: :decoded, image: @image}}
        ] do
      assert {:error, {:resource_limit_exceeded, %{source: "CSS image-resolution"}}} =
               CssImageValueValidator.apply_style(Map.merge(fields, image_fields), [])
    end
  end

  test "layer resource decode errors and malformed natural dimensions retain ownership" do
    assert {:ok, dom} =
             HtmlParser.parse("<div style='background-image:url(broken),image(red)'></div>")

    assert {:error, {_, %{message: message}}} =
             Style.compute_detailed(dom,
               assets: %{"broken" => {:bytes, "invalid pixels"}},
               system_font_discovery: false
             )

    assert is_binary(message)
    refute CssImageValueValidator.valid_loaded?(%{kind: :decoded, image: %{@image | data: <<>>}})
    refute CssImageValueValidator.valid_loaded?(%{kind: :decoded, image: %{@image | width: -1}})
    refute CssImageValueValidator.valid_loaded?(%{kind: :decoded, image: %{@image | height: -1}})
  end
end
