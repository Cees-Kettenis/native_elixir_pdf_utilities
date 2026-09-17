defmodule NativeElixirPdfUtilities.JpegValidatorTest do
  use ExUnit.Case, async: true
  alias NativeElixirPdfUtilities.Validators.JpegValidator
  alias NativeElixirPdfUtilities.HtmlToPdf

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
      data = jpeg(frame(ids) <> adobe(adobe))
      assert {:ok, %{color_transform: ^convention}} = JpegValidator.metadata(data)
      assert {:ok, pdf} = HtmlToPdf.render("<img src='x'>", assets: %{"x" => {:bytes, data}})
      if convention in [:rgb, :cmyk, :adobe_cmyk], do: assert(pdf =~ "/ColorTransform 0")
      if convention in [:ycbcr, :ycck], do: assert(pdf =~ "/ColorTransform 1")
    end

    assert {:ok, _} = JpegValidator.metadata(jpeg(frame([1, 2, 3]) <> adobe(1) <> adobe(1)))

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
      data = jpeg(body)
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

  defp jpeg(body), do: <<255, 216>> <> body <> <<255, 217>>
  defp segment(marker, bytes), do: <<255, marker, byte_size(bytes) + 2::16, bytes::binary>>

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
