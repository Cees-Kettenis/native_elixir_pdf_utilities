defmodule NativeElixirPdfUtilities.Validators.FlateValidator do
  @moduledoc false

  @type status :: :continue | :finished
  @type error_result :: {:error, {atom(), map()}}

  @doc "Inflates a self-contained stream, checking each output chunk before retaining it."
  @spec inflate(binary(), 15 | 31, (status(), non_neg_integer(), non_neg_integer() ->
                                      :ok | error_result())) ::
          {:ok, binary()} | :invalid_data | error_result()
  def inflate(data, window_bits, validate) do
    stream = :zlib.open()

    try do
      :ok = :zlib.inflateInit(stream, window_bits)
      read_chunks(stream, data, 0, [], validate)
    rescue
      ErlangError -> :invalid_data
    after
      :zlib.close(stream)
    end
  end

  defp read_chunks(stream, data, size, chunks, validate) do
    with {:ok, status, output} <- chunk(:zlib.safeInflate(stream, data)),
         bytes = IO.iodata_length(output),
         size = size + bytes,
         :ok <- validate.(status, size, bytes) do
      chunks = [output | chunks]

      if status == :finished do
        :ok = :zlib.inflateEnd(stream)
        {:ok, chunks |> Enum.reverse() |> IO.iodata_to_binary()}
      else
        read_chunks(stream, <<>>, size, chunks, validate)
      end
    end
  end

  @doc "Classifies an OTP inflater result. The supported formats do not supply preset dictionaries."
  @spec chunk({status(), iodata()} | {:need_dictionary, non_neg_integer(), iodata()}) ::
          {:ok, status(), iodata()} | :invalid_data
  def chunk(result) do
    case result do
      {:need_dictionary, _checksum, _output} -> :invalid_data
      {status, output} when status in [:continue, :finished] -> {:ok, status, output}
    end
  end
end
