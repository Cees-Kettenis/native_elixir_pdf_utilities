defmodule NativeElixirPdfUtilities.PngValidatorTest do
  use ExUnit.Case, async: true

  alias NativeElixirPdfUtilities.HtmlToPdf
  alias NativeElixirPdfUtilities.Validators.PngValidator

  test "checks CRCs, required chunk order, duplicate chunks and critical types" do
    header = chunk("IHDR", <<1::32, 1::32, 8, 2, 0, 0, 0>>)
    data = chunk("IDAT", :zlib.compress(<<0, 255, 0, 0>>))
    ending = chunk("IEND", "")
    palette = chunk("PLTE", <<255, 0, 0>>)
    transparency = chunk("tRNS", <<255::16, 0::16, 0::16>>)
    ancillary = chunk("tEXt", "ignored")

    valid = [
      header <> data <> ending,
      header <> palette <> transparency <> data <> ancillary <> ending,
      header <> ancillary <> data <> chunk("IDAT", "") <> ending
    ]

    for png <- valid do
      assert {:ok, %{width_px: 1, height_px: 1, color_type: 2}} = PngValidator.prepare(png)
      assert {:ok, _} = render(png)
    end

    invalid = [
      "",
      <<0>>,
      binary_part(header, 0, byte_size(header) - 1),
      <<13::32, "IHDR", 1::32, 1::32, 8, 2, 0, 0, 0, 0::32>> <> data <> ending,
      data <> header <> ending,
      header <> header <> data <> ending,
      chunk("IHDR", <<0::32, 1::32, 8, 2, 0, 0, 0>>) <> data <> ending,
      chunk("IHDR", <<1::32, 1::32, 16, 2, 0, 0, 0>>) <> data <> ending,
      header <> ending,
      header <> data,
      header <> data <> chunk("IEND", "bad"),
      header <> data <> ending <> ancillary,
      header <> chunk("ABCD", "critical") <> data <> ending,
      header <> chunk("abca", "reserved") <> data <> ending,
      header <> chunk("ab1d", "invalid") <> data <> ending,
      header <> palette <> palette <> data <> ending,
      header <> chunk("PLTE", <<0>>) <> data <> ending,
      header <> transparency <> palette <> data <> ending,
      header <> data <> palette <> ending,
      header <> transparency <> transparency <> data <> ending,
      header <> data <> transparency <> ending,
      header <> chunk("tRNS", <<0>>) <> data <> ending,
      chunk("IHDR", <<1::32, 1::32, 8, 6, 0, 0, 0>>) <> transparency <> data <> ending,
      header <> data <> ancillary <> data <> ending
    ]

    for png <- invalid do
      assert {:error, {:invalid_document, %{stage: :style, message: message}}} =
               PngValidator.prepare(png)

      assert is_binary(message) and message != ""
      assert {:error, {:invalid_document, %{operation: :render, module: HtmlToPdf}}} = render(png)
    end
  end

  defp chunk(type, data) do
    <<byte_size(data)::32, type::binary, data::binary, :erlang.crc32(type <> data)::32>>
  end

  defp render(chunks) do
    png = <<137, 80, 78, 71, 13, 10, 26, 10>> <> chunks
    HtmlToPdf.render("<img src=\"asset.png\">", assets: %{"asset.png" => {:bytes, png}})
  end
end
