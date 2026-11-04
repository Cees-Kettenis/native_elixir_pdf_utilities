defmodule NativeElixirPdfUtilities.Pdf.ReaderPredictorTest do
  use ExUnit.Case, async: false
  import Bitwise

  alias NativeElixirPdfUtilities.Limits
  alias NativeElixirPdfUtilities.Pdf.Reader

  setup do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    %{limits: limits}
  end

  test "TIFF reconstruction preserves component history, bit depths and row padding" do
    for bits <- [1, 2, 4, 8, 16], colors <- [1, 2, 3], columns <- [1, 2, 3, 9] do
      modulus = 1 <<< bits
      count = colors * columns
      padding = rem(8 - rem(count * bits, 8), 8)

      rows =
        for row <- 0..1 do
          samples = for i <- 0..(count - 1), do: rem(i * 3 + row, modulus)

          encoded =
            samples
            |> Enum.with_index()
            |> Enum.reduce(<<>>, fn {sample, index}, acc ->
              left = if index < colors, do: 0, else: Enum.at(samples, index - colors)
              value = Integer.mod(sample - left, modulus)
              <<acc::bitstring, value::size(bits)>>
            end)

          expected = for sample <- samples, into: <<>>, do: <<sample::size(bits)>>
          pad = (1 <<< padding) - 1

          {<<encoded::bitstring, pad::size(padding)>>,
           <<expected::bitstring, pad::size(padding)>>}
        end

      encoded = rows |> Enum.map(&elem(&1, 0)) |> IO.iodata_to_binary()
      expected = rows |> Enum.map(&elem(&1, 1)) |> IO.iodata_to_binary()

      assert {:ok, ^expected} =
               decode(encoded, %{
                 "Predictor" => 2,
                 "Colors" => colors,
                 "Columns" => columns,
                 "BitsPerComponent" => bits
               })
    end
  end

  test "PNG reconstruction preserves every filter with packed and multibyte components" do
    for {colors, bits, columns} <- [{1, 1, 55}, {3, 4, 3}, {3, 8, 3}, {2, 16, 3}],
        filter <- 0..4 do
      row_bytes = div(colors * columns * bits + 7, 8)
      pixel_bytes = div(colors * bits + 7, 8)
      first = for i <- 1..row_bytes, into: <<>>, do: <<rem(i * 53, 256)>>
      second = for i <- 1..row_bytes, into: <<>>, do: <<rem(i * 37 + 71, 256)>>

      encoded =
        png_encode(first, :binary.copy(<<0>>, row_bytes), pixel_bytes, filter) <>
          png_encode(second, first, pixel_bytes, filter)

      expected = first <> second

      assert {:ok, ^expected} =
               decode(encoded, %{
                 "Predictor" => 15,
                 "Colors" => colors,
                 "Columns" => columns,
                 "BitsPerComponent" => bits
               })
    end
  end

  test "predictor work is admitted for unpacked samples before reconstruction", %{limits: limits} do
    Limits.install(%{limits | max_pdf_reader_work: 20})

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             decode(<<0, 0, 0, 0, 0, 0>>, %{
               "Predictor" => 2,
               "Columns" => 48,
               "BitsPerComponent" => 1
             })

    assert diagnostic.stage == :limits
    assert diagnostic.reason == :resource_limit_exceeded
    assert diagnostic.operation == :read
    assert diagnostic.module == Reader
    assert diagnostic.source == "object 1 0"
    assert diagnostic.message =~ "max_pdf_reader_work"
  end

  test "predictor buffer admission accepts the boundary and rejects one byte less", %{
    limits: limits
  } do
    params = %{"Predictor" => 2, "Columns" => 3}
    Limits.install(%{limits | max_pdf_predictor_working_bytes: 17})
    assert {:ok, <<1, 3, 6>>} = decode(<<1, 2, 3>>, params)
    Limits.install(%{limits | max_pdf_predictor_working_bytes: 16})
    assert {:error, {:resource_limit_exceeded, diagnostic}} = decode(<<1, 2, 3>>, params)
    assert diagnostic.stage == :limits and diagnostic.operation == :read
    assert diagnostic.source == "object 1 0"
    assert diagnostic.message =~ "max_pdf_predictor_working_bytes"

    Limits.install(%{limits | max_pdf_predictor_working_bytes: 1})

    for predictor <- [2, 15] do
      assert {:ok, <<>>} = decode(<<>>, %{"Predictor" => predictor, "Columns" => 1_000})
    end
  end

  test "predictor admission diagnoses incomplete rows and invalid PNG filter bytes" do
    for {data, params, message} <- [
          {<<1, 2>>, %{"Predictor" => 2, "Columns" => 3}, "TIFF predictor rows are truncated"},
          {<<0, 1, 2>>, %{"Predictor" => 15, "Columns" => 3}, "PNG predictor rows are truncated"},
          {<<5, 0>>, %{"Predictor" => 15}, "PNG predictor row has an invalid filter"}
        ] do
      assert {:error, {:invalid_pdf_input, diagnostic}} = decode(data, params)
      assert diagnostic.stage == :filter and diagnostic.operation == :read
      assert diagnostic.module == Reader and diagnostic.source == "object 1 0"
      assert diagnostic.message =~ message
    end
  end

  defp decode(data, params) do
    encoded = Base.encode16(data) <> ">"

    document = %{
      objects: %{
        {1, 0} => %{
          value: %{
            "Length" => byte_size(encoded),
            "Filter" => {:name, "ASCIIHexDecode"},
            "DecodeParms" => params
          },
          stream: encoded
        }
      }
    }

    Reader.decoded_stream(document, {:ref, {1, 0}})
  end

  defp png_encode(row, previous, pixel_bytes, filter) do
    bytes = for <<byte <- row>>, do: byte

    encoded =
      bytes
      |> Enum.with_index()
      |> Enum.map(fn {byte, index} ->
        left = if index < pixel_bytes, do: 0, else: Enum.at(bytes, index - pixel_bytes)
        up = :binary.at(previous, index)
        diagonal = if index < pixel_bytes, do: 0, else: :binary.at(previous, index - pixel_bytes)

        predicted =
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
              p = left + up - diagonal

              [{abs(p - left), left}, {abs(p - up), up}, {abs(p - diagonal), diagonal}]
              |> Enum.min_by(&elem(&1, 0))
              |> elem(1)
          end

        Integer.mod(byte - predicted, 256)
      end)

    IO.iodata_to_binary([filter | encoded])
  end
end
