defmodule NativeElixirPdfUtilities.FileReader do
  @moduledoc false

  alias NativeElixirPdfUtilities.Validators.FileValidator
  alias NativeElixirPdfUtilities.Diagnostics
  alias NativeElixirPdfUtilities.Limits

  @external_resource Path.join(__DIR__, "confined_file_reader.py")
  @confined_reader File.read!(@external_resource)

  @doc "Reads at most the allowed bytes from an opened regular file. Path authorization belongs to the caller."
  @spec read(String.t(), pos_integer()) ::
          {:ok, binary()} | {:error, File.posix() | {atom(), Diagnostics.diagnostic()}}
  def read(path, maximum_bytes) do
    with :ok <- FileValidator.validate_read(path, maximum_bytes),
         {:ok, file} <- :file.open(path, [:raw, :binary, :read]) do
      try do
        with {:ok, info} <- :file.read_file_info(file),
             :ok <- FileValidator.validate_info(File.Stat.from_record(info), maximum_bytes, path) do
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
      after
        :file.close(file)
      end
    end
  end

  @doc "Reads a relative asset under a root pinned by Linux openat2, rejecting every symlink component."
  @spec read_confined(String.t(), String.t(), pos_integer()) ::
          {:ok, binary()} | {:error, {atom(), Diagnostics.diagnostic()}}
  def read_confined(root, relative, maximum_bytes) do
    with :ok <- FileValidator.validate_confined_read(root, relative, maximum_bytes),
         source = Path.join(root, relative),
         {:ok, python} <-
           FileValidator.validate_confined_runtime(:os.type(), System.find_executable("python3")) do
      try do
        timeout = Limits.get(:max_asset_file_read_timeout_ms)
        deadline = System.monotonic_time(:millisecond) + timeout

        port =
          Port.open({:spawn_executable, python}, [
            :binary,
            :exit_status,
            :use_stdio,
            :hide,
            {:packet, 4},
            {:args, ["-I", "-S", "-c", @confined_reader, root, relative, to_string(timeout)]}
          ])

        try do
          with {:ok, <<1, mode::32, size::64>>} <- file_message(port, deadline, source),
               :ok <- FileValidator.validate_opened_metadata(mode, size, maximum_bytes, source),
               true <- Port.command(port, <<maximum_bytes::64>>),
               {:ok, <<0, bytes::binary>>} <- file_message(port, deadline, source),
               :ok <- FileValidator.validate_size(byte_size(bytes), maximum_bytes, source) do
            {:ok, bytes}
          else
            {:ok, response} -> FileValidator.confined_error(response, source)
            {:error, _} = error -> error
          end
        after
          # A process can exit between receiving its final packet and this close.
          try do
            Port.close(port)
          rescue
            ArgumentError -> :ok
          end
        end
      rescue
        _error in [ArgumentError, ErlangError] ->
          FileValidator.confined_error(<<5>>, source)
      end
    end
  end

  defp file_message(port, deadline, source) do
    receive do
      {^port, {:data, data}} -> {:ok, data}
      {^port, {:exit_status, 124}} -> FileValidator.confined_error(<<3>>, source)
      {^port, {:exit_status, _status}} -> FileValidator.confined_error(<<5>>, source)
    after
      max(deadline - System.monotonic_time(:millisecond), 0) ->
        FileValidator.confined_error(<<3>>, source)
    end
  end
end
