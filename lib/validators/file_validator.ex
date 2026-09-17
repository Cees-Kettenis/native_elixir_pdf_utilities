defmodule NativeElixirPdfUtilities.Validators.FileValidator do
  @moduledoc false

  alias NativeElixirPdfUtilities.Diagnostics

  @doc false
  @spec validate_read(term(), term()) :: :ok | {:error, {atom(), Diagnostics.diagnostic()}}
  def validate_read(path, maximum_bytes) do
    case is_binary(path) and path != "" and is_integer(maximum_bytes) and maximum_bytes > 0 do
      true ->
        :ok

      false ->
        Diagnostics.error(
          :file,
          :invalid_options,
          "bounded file reads require a path and a positive byte limit"
        )
    end
  end

  @doc false
  @spec validate_info(
          File.Stat.t() | %{type: atom(), size: non_neg_integer()},
          pos_integer(),
          String.t()
        ) ::
          :ok | {:error, {atom(), Diagnostics.diagnostic()}}
  def validate_info(info, maximum_bytes, path) do
    case info.type do
      :regular ->
        validate_size(info.size, maximum_bytes, path)

      _ ->
        Diagnostics.error(:file, :invalid_document, "approved input must be a regular file",
          source: path
        )
    end
  end

  @doc false
  @spec validate_size(non_neg_integer(), pos_integer(), String.t()) ::
          :ok | {:error, {atom(), Diagnostics.diagnostic()}}
  def validate_size(size, maximum_bytes, path) do
    case size <= maximum_bytes do
      true ->
        :ok

      false ->
        Diagnostics.error(
          :file,
          :resource_limit_exceeded,
          "file exceeds the configured #{maximum_bytes}-byte read limit",
          source: path
        )
    end
  end

  @doc false
  @spec validate_confined_read(term(), term(), term()) ::
          :ok | {:error, {atom(), Diagnostics.diagnostic()}}
  def validate_confined_read(root, relative, maximum_bytes) do
    with :ok <- validate_read(root, maximum_bytes),
         :ok <- validate_read(relative, maximum_bytes) do
      # Linux pathname arguments are at most PATH_MAX - 1 bytes. This is an OS
      # invariant, not a configurable resource allowance. The helper protocol
      # carries the allowance in 64 bits and the result length in 32 bits;
      # reserve one byte each for status and the oversize sentinel.
      if Path.type(root) == :absolute and Path.type(relative) == :relative and
           not String.contains?(root <> relative, <<0>>) and
           not Enum.member?(Path.split(relative), "..") and
           byte_size(root) < 4096 and byte_size(relative) < 4096 and
           maximum_bytes <= 0xFFFFFFFD do
        :ok
      else
        Diagnostics.error(
          :file,
          :invalid_document,
          "confined reads require an absolute root and a relative path without traversal or NUL bytes, and a byte allowance within the 32-bit response framing limit"
        )
      end
    end
  end

  @doc false
  @spec validate_confined_runtime(term(), term()) ::
          {:ok, String.t()} | {:error, {atom(), Diagnostics.diagnostic()}}
  def validate_confined_runtime(platform, python) do
    case {platform, python} do
      {{:unix, :linux}, python} when is_binary(python) -> {:ok, python}
      _ -> confined_error(<<4>>, nil)
    end
  end

  @doc false
  @spec validate_opened_metadata(non_neg_integer(), non_neg_integer(), pos_integer(), String.t()) ::
          :ok | {:error, {atom(), Diagnostics.diagnostic()}}
  def validate_opened_metadata(mode, size, maximum_bytes, source) do
    type = if Bitwise.band(mode, 0o170000) == 0o100000, do: :regular, else: :other
    validate_info(%{type: type, size: size}, maximum_bytes, source)
  end

  @doc false
  @spec confined_error(binary(), String.t() | nil) :: {:error, {atom(), Diagnostics.diagnostic()}}
  def confined_error(response, source) do
    {reason, message} =
      case response do
        <<2, errno::32>> ->
          {:invalid_document,
           "confined local file could not be opened or read without symlinks (OS error #{errno})"}

        <<3>> ->
          {:resource_limit_exceeded,
           "confined asset file read exceeded max_asset_file_read_timeout_ms"}

        <<4>> ->
          {:invalid_document,
           "implicit base_url assets require Linux openat2 (kernel 5.6 or newer, x86_64/aarch64) and Python 3; install the backend or supply approved bytes through :assets or :asset_resolver"}

        _ ->
          {:invalid_document,
           "confined file reader failed; verify the Python 3 installation or supply approved asset bytes"}
      end

    Diagnostics.error(:file, reason, message, source: source)
  end
end
