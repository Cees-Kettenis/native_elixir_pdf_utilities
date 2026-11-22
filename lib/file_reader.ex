defmodule NativeElixirPdfUtilities.FileReader do
  @moduledoc false

  alias NativeElixirPdfUtilities.Validators.{FileValidator, ResourceBudgetValidator}
  alias NativeElixirPdfUtilities.Diagnostics

  @chunk_bytes 16_384

  @doc """
  Reads an authorized regular file under an explicit or remaining input-byte limit.
  A caller supplying previously admitted font bytes permits a work-budgeted
  fingerprint scan of matching lengths. Only previously admitted bytes can be
  returned by that route.
  """
  @spec read(String.t(), pos_integer() | :remaining_input) ::
          {:ok, binary()} | {:error, File.posix() | {atom(), Diagnostics.diagnostic()}}
  @spec read(String.t(), pos_integer() | :remaining_input, map()) ::
          {:ok, binary()} | {:error, File.posix() | {atom(), Diagnostics.diagnostic()}}
  def read(path, maximum_bytes, admitted_sources \\ %{}) do
    with :ok <- FileValidator.validate_read(path, maximum_bytes),
         maximum_bytes =
           if(maximum_bytes == :remaining_input,
             do: ResourceBudgetValidator.remaining(:max_input_bytes),
             else: maximum_bytes
           ),
         {:ok, info} <- File.stat(path),
         :ok <- validate_info(info, maximum_bytes, admitted_sources, path),
         {:ok, file} <- :file.open(path, [:raw, :binary, :read]) do
      try do
        with {:ok, info} <- :file.read_file_info(file),
             stat = File.Stat.from_record(info),
             :ok <- validate_info(stat, maximum_bytes, admitted_sources, path) do
          if stat.size > maximum_bytes do
            # A repeated font can fit the unique-source budget. Verify its
            # current contents in bounded blocks before reusing admitted bytes.
            with :ok <- ResourceBudgetValidator.reserve(:max_work, stat.size, :font, source: path),
                 {:ok, digest} <- fingerprint(file, stat.size, :crypto.hash_init(:sha256), path) do
              case Map.fetch(Map.get(admitted_sources, stat.size, %{}), digest) do
                {:ok, bytes} -> {:ok, bytes}
                :error -> FileValidator.validate_size(stat.size, maximum_bytes, path)
              end
            end
          else
            read_bytes(file, stat.size + 1, maximum_bytes, path, 0, [])
          end
        end
      after
        :file.close(file)
      end
    end
  end

  defp read_bytes(file, request, maximum, path, consumed, chunks) do
    result =
      case :file.read(file, request) do
        :eof -> {:ok, ""}
        result -> result
      end

    with {:ok, bytes} <- result,
         total = consumed + byte_size(bytes),
         :ok <- FileValidator.validate_size(total, maximum, path) do
      if byte_size(bytes) == request do
        # The file grew beyond its observed size. Read at most one byte past
        # the remaining allowance and keep each subsequent buffer bounded.
        next = min(@chunk_bytes, maximum - total + 1)
        read_bytes(file, next, maximum, path, total, [bytes | chunks])
      else
        {:ok, IO.iodata_to_binary(:lists.reverse(chunks, bytes))}
      end
    end
  end

  defp validate_info(info, maximum, admitted_sources, path) do
    ceiling =
      if Map.has_key?(admitted_sources, info.size), do: max(info.size, maximum), else: maximum

    FileValidator.validate_info(info, ceiling, path)
  end

  defp fingerprint(file, remaining, state, path) do
    result = :file.read(file, min(@chunk_bytes, remaining + 1))

    if result == :eof do
      {:ok, :crypto.hash_final(state)}
    else
      with {:ok, chunk} <- result,
           :ok <- FileValidator.validate_size(byte_size(chunk), remaining, path) do
        fingerprint(file, remaining - byte_size(chunk), :crypto.hash_update(state, chunk), path)
      end
    end
  end
end
