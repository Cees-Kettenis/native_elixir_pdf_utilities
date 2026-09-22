defmodule NativeElixirPdfUtilities.JpegValidatorTest do
  use ExUnit.Case, async: true
  alias NativeElixirPdfUtilities.Validators.JpegValidator
  alias NativeElixirPdfUtilities.HtmlToPdf
  alias NativeElixirPdfUtilities.TestSupport.JpegFixture
  import NativeElixirPdfUtilities.TestSupport.JpegFixture, only: [segment: 2]

  test "rejects missing, malformed and empty scans through the rendering API" do
    [tables, _scan] = :binary.split(scan([1]), <<255, 218>>)
    header = segment(218, <<1, 1, 0, 0, 63, 0>>)
    prefix = <<255, 216>> <> frame([1]) <> tables

    malformed = [
      jpeg(frame([1])),
      jpeg(header <> <<63>>),
      prefix <> header,
      prefix <> header <> <<63>>,
      prefix <> header <> <<63, 255>>,
      prefix <> header <> <<255, 217>>,
      prefix <> header <> <<255, 255, 217>>,
      prefix <> <<255, 218, 0, 1>>,
      prefix <> <<255, 218, 0, 20, 1, 1, 0, 0, 63, 0, 63, 255, 217>>,
      jpeg(frame([1]) <> tables <> segment(218, <<0, 0, 63, 0>>) <> <<63>>),
      jpeg(frame([1]) <> tables <> segment(218, <<1, 1, 0, 0, 63>>) <> <<63>>),
      jpeg(frame([1]) <> tables <> segment(218, <<1, 9, 0, 0, 63, 0>>) <> <<63>>),
      jpeg(frame([1]) <> tables <> segment(218, <<2, 1, 0, 1, 0, 0, 63, 0>>) <> <<63>>),
      jpeg(frame([1]) <> tables <> segment(218, <<1, 1, 64, 0, 63, 0>>) <> <<63>>),
      jpeg(frame([1]) <> tables <> segment(218, <<1, 1, 1, 0, 63, 0>>) <> <<63>>),
      jpeg(frame([1]) <> tables <> segment(218, <<1, 1, 0, 1, 63, 0>>) <> <<63>>),
      jpeg(frame([1]) <> tables <> segment(218, <<1, 1, 0, 0, 63, 1>>) <> <<63>>),
      jpeg(frame([1]) <> header <> <<63>>),
      jpeg(frame([1, 2, 3]) <> tables <> header <> <<63>>),
      jpeg(frame([1]) <> tables <> header <> <<63>> <> header <> <<63>>),
      jpeg(<<255, 216>> <> frame([1]) <> scan([1])),
      jpeg(<<255, 208>> <> frame([1]) <> scan([1]))
    ]

    for data <- malformed do
      assert :error = JpegValidator.metadata(data)

      assert {:error, {:invalid_document, diagnostic}} =
               HtmlToPdf.render("<img src='broken.jpg'>",
                 assets: %{"broken.jpg" => {:bytes, data}}
               )

      assert diagnostic.reason == :invalid_document
      assert diagnostic.stage == :style
      assert diagnostic.module == HtmlToPdf
      assert diagnostic.operation == :render
      assert diagnostic.message =~ "image"
    end
  end

  test "requires a complete marker stream for baseline and progressive JPEGs" do
    for data <- [
          JpegFixture.baseline(2, 1),
          fixture("progressive_rgb.jpg"),
          fixture("restart_rgb.jpg")
        ] do
      assert {:ok, _} = JpegValidator.metadata(data)
      assert {:ok, _} = HtmlToPdf.render("<img src='x'>", assets: %{"x" => {:bytes, data}})

      for size <- 0..(byte_size(data) - 1) do
        assert :error = JpegValidator.metadata(binary_part(data, 0, size)),
               "accepted a JPEG truncated to #{size} of #{byte_size(data)} bytes"
      end
    end
  end

  test "validates table lengths, identifiers and Huffman code space" do
    [tables, _] = :binary.split(scan([1]), <<255, 218>>)
    [quantization, huffman] = :binary.split(tables, <<255, 196>>)
    huffman = <<255, 196>> <> huffman
    scan = segment(218, <<1, 1, 0, 0, 63, 0>>) <> <<63>>

    for invalid_table <- [
          segment(219, ""),
          segment(219, <<0, 1>>),
          segment(219, <<32>> <> :binary.copy(<<1>>, 64)),
          segment(219, <<4>> <> :binary.copy(<<1>>, 64)),
          segment(219, <<0>> <> :binary.copy(<<0>>, 64)),
          segment(196, ""),
          segment(196, <<0, 1>>),
          segment(196, <<32, 1, 0::120, 0>>),
          segment(196, <<4, 1, 0::120, 0>>),
          segment(196, <<0, 0::128>>),
          segment(196, <<0, 2, 0::120, 0, 1>>),
          segment(196, <<0, 1, 0::120>>),
          segment(196, <<0, 1, 0::120, 12>>),
          segment(196, <<16, 1, 0::120, 11>>),
          segment(221, <<1>>)
        ] do
      assert :error = JpegValidator.metadata(jpeg(frame([1]) <> tables <> invalid_table <> scan))
    end

    for incomplete_tables <- [quantization, huffman] do
      assert :error = JpegValidator.metadata(jpeg(frame([1]) <> incomplete_tables <> scan))
    end

    wide_quantization = segment(219, <<16>> <> :binary.copy(<<1::16>>, 64))

    assert :error =
             JpegValidator.metadata(jpeg(frame([1]) <> wide_quantization <> huffman <> scan))

    progressive = segment(194, <<8, 1::16, 1::16, 1, 1, 17, 0>>)
    dc_scan = segment(218, <<1, 1, 0, 0, 0, 0>>) <> <<127>>

    assert {:ok, _} =
             JpegValidator.metadata(jpeg(progressive <> wide_quantization <> huffman <> dc_scan))
  end

  test "progressive scan parameters and refinements must agree with earlier scans" do
    [tables, _] = :binary.split(scan([1]), <<255, 218>>)
    frame = segment(194, <<8, 1::16, 1::16, 1, 1, 17, 0>>)
    initial = segment(218, <<1, 1, 0, 0, 0, 1>>) <> <<127>>
    refinement = segment(218, <<1, 1, 0, 0, 0, 16>>) <> <<127>>
    ac = segment(218, <<1, 1, 0, 1, 63, 0>>) <> <<127>>
    assert {:ok, _} = JpegValidator.metadata(jpeg(frame <> tables <> initial <> refinement <> ac))

    for scans <- [
          refinement,
          ac,
          initial <> initial,
          initial <> segment(218, <<1, 1, 0, 1, 63, 16>>) <> <<127>>,
          segment(218, <<1, 1, 0, 0, 1, 0>>) <> <<127>>,
          segment(218, <<1, 1, 0, 0, 0, 14>>) <> <<127>>,
          segment(218, <<1, 1, 0, 0, 0, 32>>) <> <<127>>,
          initial <> segment(218, <<1, 1, 0, 64, 64, 0>>) <> <<127>>,
          initial <> segment(218, <<1, 1, 0, 2, 1, 0>>) <> <<127>>
        ] do
      assert :error = JpegValidator.metadata(jpeg(frame <> tables <> scans))
    end
  end

  test "scan data handles byte stuffing, marker fill and ordered restart markers" do
    [prefix, _] = :binary.split(JpegFixture.baseline(16, 8, 1), <<255, 218>>)
    scan = segment(218, <<1, 1, 0, 0, 63, 0>>)
    restart = segment(221, <<1::16>>)

    for entropy <- [<<255, 0>>, <<63, 255, 255>>, <<63>>] do
      assert {:ok, _} = JpegValidator.metadata(prefix <> scan <> entropy <> <<255, 217>>)
    end

    assert {:ok, _} =
             JpegValidator.metadata(prefix <> restart <> scan <> <<63, 255, 208, 63, 255, 217>>)

    for {interval, entropy} <- [
          {"", <<63, 255, 208, 63>>},
          {restart, <<63, 255, 209, 63>>},
          {restart, <<255, 208, 63>>},
          {restart, <<63, 255, 208>>},
          {restart, <<63, 255, 208, 255, 209, 63>>}
        ] do
      assert :error =
               JpegValidator.metadata(prefix <> interval <> scan <> entropy <> <<255, 217>>)
    end
  end

  test "preserves supported color conventions and rejects ambiguous frames" do
    for {ids, adobe, convention} <- [
          {[1], nil, :gray},
          {[1], 0, :gray},
          {[1, 2, 3], nil, :ycbcr},
          {[?R, ?G, ?B], nil, :rgb},
          {[1, 2, 3], 0, :rgb},
          {[1, 2, 3], 1, :ycbcr},
          {[?C, ?M, ?Y, ?K], nil, :cmyk},
          {[1, 2, 3, 4], nil, :cmyk},
          {[1, 2, 3, 4], 0, :adobe_cmyk},
          {[1, 2, 3, 4], 2, :ycck}
        ] do
      data = jpeg(frame(ids) <> adobe(adobe) <> scan(ids))
      assert {:ok, %{color_transform: ^convention}} = JpegValidator.metadata(data)
      assert {:ok, pdf} = HtmlToPdf.render("<img src='x'>", assets: %{"x" => {:bytes, data}})
      if convention in [:rgb, :cmyk, :adobe_cmyk], do: assert(pdf =~ "/ColorTransform 0")
      if convention in [:ycbcr, :ycck], do: assert(pdf =~ "/ColorTransform 1")
    end

    assert {:ok, _} =
             JpegValidator.metadata(
               jpeg(frame([1, 2, 3]) <> adobe(1) <> adobe(1) <> scan([1, 2, 3]))
             )

    for body <- [
          frame([1, 2, 3]) <> adobe(0) <> adobe(1),
          frame([1, 2, 3]) <> frame([1, 2, 3]),
          frame([1, 1, 3]),
          frame([1, 2, 3], <<1, 0, 0, 2, 17, 0, 3, 17, 0>>),
          frame([1, 2, 3], <<1, 17, 4, 2, 17, 0, 3, 17, 0>>),
          frame([1, 2, 3], <<1, 17, 0>>),
          frame([1, 2, 3]) <> adobe(2),
          frame([1, 2, 3, 4]) <> adobe(1),
          frame([9, 8, 7]),
          frame([9, 8, 7, 6]),
          frame([1]) <> adobe(1),
          frame([1, 2, 3]) <> adobe(3),
          frame([1, 2, 3]) <> segment(238, "Adobe"),
          segment(193, <<8, 1::16, 1::16, 3, 1, 17, 0, 2, 17, 0, 3, 17, 0>>)
        ] do
      data = jpeg(body <> scan([1, 2, 3]))
      assert :error = JpegValidator.metadata(data)

      assert {:error, {:invalid_document, %{operation: :render, stage: :style}}} =
               HtmlToPdf.render("<img src='x'>", assets: %{"x" => {:bytes, data}})
    end
  end

  test "real ordinary CMYK is distinct from Adobe CMYK and YCCK" do
    for {file, convention, inversion} <- [
          {"ordinary_cmyk.jpg", :cmyk, false},
          {"adobe_cmyk.jpg", :adobe_cmyk, true},
          {"adobe_ycck.jpg", :ycck, true}
        ] do
      data = File.read!(Path.join([__DIR__, "fixtures", "html_to_pdf", file]))

      assert {:ok, %{color_transform: ^convention, inverted_cmyk: ^inversion}} =
               JpegValidator.metadata(data)

      assert {:ok, pdf} = HtmlToPdf.render("<img src='x'>", assets: %{"x" => {:bytes, data}})
      assert String.contains?(pdf, "/Decode [1 0 1 0 1 0 1 0]") == inversion
    end
  end

  @tag :browser_parity
  test "independent PDF rasterization preserves red across all CMYK conventions" do
    directory = Path.join(System.tmp_dir!(), "nepu-cmyk-#{System.unique_integer([:positive])}")
    File.mkdir_p!(directory)
    on_exit(fn -> File.rm_rf!(directory) end)
    rasterizer = System.find_executable("pdftoppm") || flunk("pdftoppm is required")

    for name <- ["ordinary_cmyk", "adobe_cmyk", "adobe_ycck"] do
      data = File.read!(Path.join([__DIR__, "fixtures", "html_to_pdf", name <> ".jpg"]))

      {:ok, pdf} =
        HtmlToPdf.render("<img src='x' style='display:block;width:30pt;height:30pt'>",
          assets: %{"x" => {:bytes, data}},
          page_size: {40, 40},
          margin: 0
        )

      path = Path.join(directory, name)
      File.write!(path <> ".pdf", pdf)

      assert {_log, 0} =
               System.cmd(rasterizer, ["-singlefile", "-r", "72", path <> ".pdf", path],
                 stderr_to_stdout: true
               )

      # PPM pixels are uncompressed RGB; sample the middle of the solid-red fixture.
      assert <<"P6\n40 40\n255\n", pixels::binary>> = File.read!(path <> ".ppm")
      <<red, green, blue>> = binary_part(pixels, (10 * 40 + 10) * 3, 3)

      assert red > 200 and green < 50 and blue < 50,
             "#{name} rendered #{inspect({red, green, blue})}"
    end
  end

  defp fixture(name), do: File.read!(Path.join([__DIR__, "fixtures", "html_to_pdf", name]))

  defp jpeg(body), do: <<255, 216>> <> body <> <<255, 217>>

  defp scan(ids) do
    data = JpegFixture.baseline(1, 1, ids)
    offset = 2 + byte_size(frame(ids))
    binary_part(data, offset, byte_size(data) - offset - 2)
  end

  defp adobe(transform) do
    case transform do
      nil -> ""
      value -> segment(238, <<"Adobe", 100::16, 0::16, 0::16, value>>)
    end
  end

  defp frame(ids, descriptors \\ nil) do
    descriptors = descriptors || for id <- ids, into: <<>>, do: <<id, 17, 0>>
    segment(192, <<8, 1::16, 1::16, length(ids), descriptors::binary>>)
  end
end
