defmodule NativeElixirPdfUtilities.Validators.FormInspectionValidator do
  @moduledoc false

  alias NativeElixirPdfUtilities.Limits
  alias NativeElixirPdfUtilities.Pdf.Reader
  alias NativeElixirPdfUtilities.Validators.{FontValidator, PdfValidator}

  @scope_key {__MODULE__, :inspection}

  @doc false
  @spec with_scope(map(), (-> result)) :: result | {:error, {atom(), map()}}
        when result: term()
  def with_scope(document, fun) do
    previous = Process.get(@scope_key)
    objects = document.objects

    case previous do
      %{objects: ^objects} ->
        fun.()

      _ ->
        Process.put(@scope_key, %{objects: objects, fonts: %{}, used: %{}})

        try do
          PdfValidator.with_reader_budget(fn -> FontValidator.with_budget(fun) end)
        after
          if previous, do: Process.put(@scope_key, previous), else: Process.delete(@scope_key)
        end
    end
  end

  @doc false
  @spec with_active_scope((-> result)) :: result | {:error, {atom(), map()}}
        when result: term()
  def with_active_scope(fun) do
    if Process.get(@scope_key), do: fun.(), else: with_scope(%{objects: %{}}, fun)
  end

  @doc false
  @spec loaded_fonts(term(), (-> {:ok, [map()]} | {:error, {atom(), map()}})) ::
          {:ok, [map()]} | {:error, {atom(), map()}}
  def loaded_fonts(descriptors, loader) do
    scope = Process.get(@scope_key)
    # Exact equality is cheap for descriptors sharing the cached stream bytes.
    # Keep this single-entry memo ahead of the general font memo, whose key
    # hashes complete font binaries even when a row reuses the same faces.
    case scope do
      %{last_fonts: ^descriptors, last_faces: faces} ->
        {:ok, faces}

      _ ->
        with {:ok, faces} <- loader.() do
          scope = Process.get(@scope_key)

          Process.put(
            @scope_key,
            scope |> Map.put(:last_fonts, descriptors) |> Map.put(:last_faces, faces)
          )

          {:ok, faces}
        end
    end
  end

  @doc false
  @spec reserve(Limits.key(), non_neg_integer(), binary() | nil) ::
          :ok | {:error, {atom(), map()}}
  def reserve(limit, amount, source \\ nil) do
    scope = Process.get(@scope_key)
    used = Map.get(scope.used, limit, 0) + amount

    case NativeElixirPdfUtilities.Validators.ResourceBudgetValidator.reserve(
           limit,
           amount,
           :forms,
           module: __MODULE__,
           source: source
         ) do
      :ok ->
        Process.put(@scope_key, %{scope | used: Map.put(scope.used, limit, used)})
        :ok

      error ->
        error
    end
  end

  @doc false
  @spec decoded_font(map(), term()) :: {:ok, binary()} | {:error, {atom(), map()}}
  def decoded_font(document, source) do
    with :ok <- reserve(:max_entries, 1),
         {:ok, stream} <- PdfValidator.validate_stream(document, source) do
      scope = Process.get(@scope_key)

      case Map.fetch(scope.fonts, stream.ref) do
        {:ok, bytes} ->
          with :ok <- reserve(:max_work, byte_size(bytes)), do: {:ok, bytes}

        :error ->
          # Keep unique font decodes inside the inspection's shared reader budget.
          with {:ok, prepared} <-
                 PdfValidator.prepare_decoded_stream(document, {:ref, stream.ref},
                   operation: :read,
                   module: Reader
                 ),
               {:ok, bytes} <- Reader.decode_prepared_stream(prepared),
               :ok <- reserve(:max_work, byte_size(bytes)) do
            scope = Process.get(@scope_key)
            Process.put(@scope_key, %{scope | fonts: Map.put(scope.fonts, stream.ref, bytes)})
            {:ok, bytes}
          end
      end
    end
  end
end
