defmodule NativeElixirPdfUtilities.FileReader do
  @moduledoc false

  alias NativeElixirPdfUtilities.Validators.{FileValidator, ResourceBudgetValidator}
  alias NativeElixirPdfUtilities.Diagnostics

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
            result =
              case :file.read(file, maximum_bytes + 1) do
                :eof -> {:ok, ""}
                result -> result
              end

            with {:ok, bytes} <- result,
                 :ok <- FileValidator.validate_size(byte_size(bytes), maximum_bytes, path) do
              {:ok, bytes}
            end
          end
        end
      after
        :file.close(file)
      end
    end
  end

  defp validate_info(info, maximum, admitted_sources, path) do
    ceiling =
      if Map.has_key?(admitted_sources, info.size), do: max(info.size, maximum), else: maximum

    FileValidator.validate_info(info, ceiling, path)
  end

  defp fingerprint(file, remaining, state, path) do
    result = :file.read(file, min(16_384, remaining + 1))

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
