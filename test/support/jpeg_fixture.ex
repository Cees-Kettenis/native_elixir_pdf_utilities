defmodule NativeElixirPdfUtilities.TestSupport.JpegFixture do
  @moduledoc false

  @doc "Builds a baseline JPEG whose component samples are all 128."
  @spec baseline(pos_integer(), pos_integer(), pos_integer() | [byte()]) :: binary()
  def baseline(width, height, components \\ 3) do
    ids = if is_list(components), do: components, else: Enum.to_list(1..components)
    descriptors = for id <- ids, into: <<>>, do: <<id, 17, 0>>
    selectors = for id <- ids, into: <<>>, do: <<id, 0>>
    frame = segment(192, <<8, height::16, width::16, length(ids), descriptors::binary>>)
    quantization = segment(219, <<0>> <> :binary.copy(<<1>>, 64))

    # One-bit Huffman codes: DC category zero and AC end-of-block. Every 8x8
    # block therefore encodes as 00. JPEG pads the final byte with one bits.
    counts = <<1, 0::120>>
    huffman = segment(196, <<0, counts::binary, 0, 16, counts::binary, 0>>)
    blocks = div(width + 7, 8) * div(height + 7, 8) * length(ids)
    padding = Integer.mod(-2 * blocks, 8)
    entropy = <<0::size(2 * blocks), Integer.pow(2, padding) - 1::size(padding)>>
    scan = segment(218, <<length(ids), selectors::binary, 0, 63, 0>>)
    <<255, 216>> <> frame <> quantization <> huffman <> scan <> entropy <> <<255, 217>>
  end

  @doc false
  @spec segment(byte(), binary()) :: binary()
  def segment(marker, bytes), do: <<255, marker, byte_size(bytes) + 2::16, bytes::binary>>
end
