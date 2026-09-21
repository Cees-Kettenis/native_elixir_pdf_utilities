Code.require_file("../support/png_fixture.ex", __DIR__)

defmodule NativeElixirPdfUtilities.HtmlToPdf.PngDecoderTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.HtmlToPdf.PngDecoder
  alias NativeElixirPdfUtilities.HtmlToPdf
  alias NativeElixirPdfUtilities.Limits
  alias NativeElixirPdfUtilities.Validators.HtmlValidator
  alias NativeElixirPdfUtilities.TestSupport.PngFixture, as: PNG

  test "all static color types and sample depths preserve samples with every filter and Adam7" do
    for {type, depths} <- [
          {0, [1, 2, 4, 8, 16]},
          {2, [8, 16]},
          {3, [1, 2, 4, 8]},
          {4, [8, 16]},
          {6, [8, 16]}
        ],
        depth <- depths,
        interlace <- [0, 1],
        {width, height} <- [{1, 1}, {1, 9}, {9, 1}, {9, 9}] do
      count = %{0 => 1, 2 => 3, 3 => 1, 4 => 2, 6 => 4}[type]
      max_sample = 2 ** depth - 1

      pixel = fn x, y ->
        for c <- 0..(count - 1), do: rem(x * 137 + y * 71 + c * 23, max_sample + 1)
      end

      palette = for i <- 0..min(max_sample, 255), into: <<>>, do: <<i, 255 - i, i>>
      chunks = if type == 3, do: PNG.chunk("PLTE", palette), else: ""
      png = PNG.build(width, height, type, depth, interlace, pixel, chunks: chunks, filters: true)
      assert {:ok, image} = decode(png)
      out = max(depth, 8)

      expected =
        for y <- 0..(height - 1), x <- 0..(width - 1), into: <<>> do
          samples = pixel.(x, y)

          case type do
            3 ->
              i = hd(samples)
              <<i, 255 - i, i>>

            _ ->
              colors = if type in [4, 6], do: Enum.drop(samples, -1), else: samples

              for value <- colors,
                  into: <<>>,
                  do: <<div(value * (2 ** out - 1), max_sample)::size(out)>>
          end
        end

      assert image.data == expected
      assert image.bits_per_component == out
      assert image.color_space == if(type in [0, 4], do: :device_gray, else: :device_rgb)

      if type in [4, 6] do
        expected_alpha =
          for y <- 0..(height - 1),
              x <- 0..(width - 1),
              into: <<>>,
              do: <<List.last(pixel.(x, y))::size(out)>>

        assert image.alpha_data == expected_alpha
      end
    end
  end

  test "palette transparency fills omitted entries with opaque alpha" do
    chunks =
      PNG.chunk("PLTE", <<255, 0, 0, 0, 255, 0, 0, 0, 255>>) <> PNG.chunk("tRNS", <<0, 128>>)

    png = PNG.build(3, 1, 3, 2, 1, fn x, _ -> [x] end, chunks: chunks)

    assert {:ok, %{data: <<255, 0, 0, 0, 255, 0, 0, 0, 255>>, alpha_data: <<0, 128, 255>>}} =
             decode(png)
  end

  test "transparent color comparisons retain low bits of 16-bit samples" do
    for type <- [0, 2], depth <- [8, 16] do
      values = if type == 0, do: [128], else: [128, 64, 32]
      transparent = for value <- values, into: <<>>, do: <<value::16>>

      png =
        PNG.build(2, 1, type, depth, 0, fn x, _ -> List.update_at(values, 0, &(&1 + x)) end,
          chunks: PNG.chunk("tRNS", transparent)
        )

      assert {:ok, image} = decode(png)
      assert image.alpha_data == <<0::size(depth), 2 ** depth - 1::size(depth)>>

      assert {:ok, pdf} =
               HtmlToPdf.render("<img src=\"sample\">", assets: %{"sample" => {:bytes, png}})

      assert pdf =~ "/BitsPerComponent #{depth}"
      assert :binary.match(pdf, :zlib.compress(image.data)) != :nomatch
      assert :binary.match(pdf, :zlib.compress(image.alpha_data)) != :nomatch
    end
  end

  test "opaque alpha is omitted and low depth grayscale transparency is preserved" do
    for type <- [4, 6], depth <- [8, 16] do
      pixel = if type == 4, do: [19, 2 ** depth - 1], else: [19, 20, 21, 2 ** depth - 1]
      assert {:ok, image} = decode(PNG.build(1, 1, type, depth, 0, fn _, _ -> pixel end))
      refute Map.has_key?(image, :alpha_data)
    end

    assert {:ok, %{data: <<0, 255>>, alpha_data: <<0, 255>>}} =
             decode(
               PNG.build(2, 1, 0, 1, 0, fn x, _ -> [x] end, chunks: PNG.chunk("tRNS", <<0::16>>))
             )
  end

  test "missing PNG signatures return diagnostics" do
    assert {:error, {:invalid_document, %{source: "PNG", message: message}}} = decode("not PNG")
    assert message =~ "signature"
  end

  test "invalid palettes and transparency fail with actionable diagnostics" do
    cases = [
      {3, 1, ""},
      {3, 1, PNG.chunk("PLTE", <<0, 0, 0, 1, 1, 1, 2, 2, 2>>)},
      {0, 8, PNG.chunk("PLTE", <<0, 0, 0>>)},
      {3, 1, PNG.chunk("tRNS", <<0>>)},
      {3, 1, PNG.chunk("PLTE", <<0, 0, 0>>) <> PNG.chunk("tRNS", <<0, 1>>)},
      {0, 1, PNG.chunk("tRNS", <<2::16>>)},
      {2, 8, PNG.chunk("tRNS", <<256::16, 0::16, 0::16>>)},
      {3, 1, PNG.chunk("PLTE", <<0, 0, 0>>)}
    ]

    for {type, depth, chunks} <- cases do
      samples = if type == 2, do: [1, 1, 1], else: [1]
      png = PNG.build(1, 1, type, depth, 0, fn _, _ -> samples end, chunks: chunks)
      assert {:error, {:invalid_document, %{source: "PNG", message: message}}} = decode(png)
      assert message != ""
    end
  end

  test "working and final buffer budgets are checked before decompression" do
    old = Limits.effective()
    on_exit(fn -> Limits.install(old) end)
    png = PNG.build(2, 2, 6, 16, 1, fn _, _ -> [1, 2, 3, 4] end)
    Limits.install(%{old | max_png_working_bytes: 1})
    assert {:error, {:resource_limit_exceeded, %{message: message}}} = decode(png)
    assert message =~ "work buffers"
    Limits.install(%{old | max_decoded_image_bytes: 31})
    assert {:error, {:resource_limit_exceeded, %{message: message}}} = decode(png)
    assert message =~ "decoded image"
  end

  test "images with identical samples but different geometry have distinct PDF resources" do
    a = PNG.build(2, 1, 0, 8, 0, fn _, _ -> [128] end)
    b = PNG.build(1, 2, 0, 8, 0, fn _, _ -> [128] end)
    c = PNG.build(1, 1, 0, 16, 0, fn _, _ -> [0x8080] end)

    assert {:ok, pdf} =
             HtmlToPdf.render("<img src=\"a\"><img src=\"b\"><img src=\"c\"><img src=\"a\">",
               assets: %{"a" => {:bytes, a}, "b" => {:bytes, b}, "c" => {:bytes, c}}
             )

    assert length(Regex.scan(~r/\/Subtype \/Image/, pdf)) == 3
  end

  defp decode(png) do
    PngDecoder.decode(png, HtmlValidator.new_image_budget(), true)
  end
end
