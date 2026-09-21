defmodule NativeElixirPdfUtilities.TestSupport.PngFixture do
  @moduledoc false

  @doc false
  @spec chunk(binary(), binary()) :: binary()
  def chunk(type, data) do
    <<byte_size(data)::32, type::binary, data::binary, :erlang.crc32(type <> data)::32>>
  end

  @doc false
  @spec build(
          pos_integer(),
          pos_integer(),
          integer(),
          integer(),
          integer(),
          function(),
          keyword()
        ) :: binary()
  def build(width, height, type, depth, interlace, pixel, opts \\ []) do
    passes =
      if interlace == 0,
        do: [{0, 0, 1, 1}],
        else: [
          {0, 0, 8, 8},
          {4, 0, 8, 8},
          {0, 4, 4, 8},
          {2, 0, 4, 4},
          {0, 2, 2, 4},
          {1, 0, 2, 2},
          {0, 1, 1, 2}
        ]

    bpp = max(div(%{0 => 1, 2 => 3, 3 => 1, 4 => 2, 6 => 4}[type] * depth + 7, 8), 1)

    rows =
      for {sx, sy, dx, dy} <- passes, sx < width, sy < height do
        ys = Enum.filter(0..(height - 1), &(rem(&1 - sy, dy) == 0 and &1 >= sy))
        xs = Enum.filter(0..(width - 1), &(rem(&1 - sx, dx) == 0 and &1 >= sx))

        {rows, _} =
          Enum.map_reduce(ys, <<>>, fn y, previous ->
            samples = for x <- xs, sample <- pixel.(x, y), into: <<>>, do: <<sample::size(depth)>>
            row = <<samples::bitstring, 0::size(rem(8 - rem(bit_size(samples), 8), 8))>>
            filter = if Keyword.get(opts, :filters, false), do: rem(y, 5), else: 0

            encoded =
              for i <- 0..(byte_size(row) - 1), into: <<>> do
                left = if i >= bpp, do: :binary.at(row, i - bpp), else: 0
                up = if previous == "", do: 0, else: :binary.at(previous, i)

                corner =
                  if previous != "" and i >= bpp, do: :binary.at(previous, i - bpp), else: 0

                prediction =
                  case filter do
                    0 ->
                      0

                    1 ->
                      left

                    2 ->
                      up

                    3 ->
                      div(left + up, 2)

                    4 ->
                      p = left + up - corner

                      [{abs(p - left), left}, {abs(p - up), up}, {abs(p - corner), corner}]
                      |> Enum.min_by(&elem(&1, 0))
                      |> elem(1)
                  end

                <<Integer.mod(:binary.at(row, i) - prediction, 256)>>
              end

            {<<filter, encoded::binary>>, row}
          end)

        rows
      end

    <<137, 80, 78, 71, 13, 10, 26, 10>> <>
      chunk("IHDR", <<width::32, height::32, depth, type, 0, 0, interlace>>) <>
      Keyword.get(opts, :chunks, "") <>
      chunk("IDAT", :zlib.compress(IO.iodata_to_binary(rows))) <> chunk("IEND", "")
  end
end
