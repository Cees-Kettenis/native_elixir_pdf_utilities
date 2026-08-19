defmodule NativeElixirPdfUtilities.HtmlToPdf.AssetLoader do
  @moduledoc false

  alias NativeElixirPdfUtilities.Diagnostics
  alias NativeElixirPdfUtilities.Validators.HtmlValidator

  @type kind :: :image | :background_image | :font

  @doc false
  @spec resolve(String.t(), kind(), keyword(), HtmlValidator.image_budget() | nil) ::
          {:ok, binary()} | {:error, {atom(), Diagnostics.diagnostic()}}
  def resolve(reference, kind, opts, image_budget \\ nil) do
    assets = Keyword.get(opts, :assets, %{})

    case Map.fetch(assets, reference) do
      {:ok, source} ->
        resolve_explicit_source(source, reference, image_budget)

      :error ->
        resolve_unmapped_reference(reference, kind, opts, image_budget)
    end
  end

  defp resolve_explicit_source(source, reference, image_budget) do
    case source do
      {:bytes, bytes} ->
        with :ok <- reserve_image_source(image_budget, byte_size(bytes)) do
          {:ok, bytes}
        end

      {:file, path} ->
        read_file(path, reference, image_budget)
    end
  end

  defp resolve_unmapped_reference(reference, kind, opts, image_budget) do
    case remote_reference?(reference) do
      true ->
        resolve_with_callback(reference, kind, opts, image_budget, :remote)

      false ->
        case HtmlValidator.validate_local_resource_path(reference, Keyword.get(opts, :base_url)) do
          {:ok, path} ->
            read_file(path, reference, image_budget)

          {:error, {_reason, _diagnostic}} ->
            resolve_with_callback(reference, kind, opts, image_budget, :local)
        end
    end
  end

  defp resolve_with_callback(reference, kind, opts, image_budget, reference_type) do
    case Keyword.get(opts, :asset_resolver) do
      resolver when is_function(resolver, 1) ->
        result =
          try do
            resolver.(%{reference: reference, kind: kind})
          rescue
            _error -> {:error, :resolver_exception}
          catch
            _kind, _reason -> {:error, :resolver_exit}
          end

        case HtmlValidator.validate_asset_resolver_result(result, reference) do
          {:ok, bytes} ->
            with :ok <- reserve_image_source(image_budget, byte_size(bytes)) do
              {:ok, bytes}
            end

          :not_found ->
            unresolved_asset(reference, kind, reference_type)

          {:error, {_reason, _diagnostic}} = error ->
            error
        end

      _ ->
        unresolved_asset(reference, kind, reference_type)
    end
  end

  defp read_file(path, reference, image_budget) do
    with {:ok, %{size: source_bytes, type: :regular}} <- File.stat(path),
         :ok <- reserve_image_source(image_budget, source_bytes),
         {:ok, bytes} <- File.read(path) do
      {:ok, bytes}
    else
      {:error, {_reason, _diagnostic}} = error ->
        error

      _ ->
        Diagnostics.error(
          :asset,
          :invalid_document,
          "approved local asset file could not be read",
          source: reference
        )
    end
  end

  defp reserve_image_source(image_budget, source_bytes) do
    case image_budget do
      nil -> :ok
      image_budget -> HtmlValidator.reserve_image_source(image_budget, source_bytes)
    end
  end

  defp unresolved_asset(reference, kind, reference_type) do
    message =
      case reference_type do
        :remote ->
          "remote #{asset_label(kind)} assets are not fetched; provide bytes with :assets or :asset_resolver"

        :local ->
          "#{asset_label(kind)} asset is not an authorized local file and was not supplied by the caller"
      end

    Diagnostics.error(:asset, :invalid_document, message, source: reference)
  end

  defp remote_reference?(reference) do
    case URI.parse(reference) do
      %URI{scheme: scheme} when scheme in ["http", "https"] -> true
      %URI{scheme: nil} -> false
      %URI{scheme: "file"} -> false
      %URI{} -> true
    end
  end

  defp asset_label(kind) do
    case kind do
      :background_image -> "background image"
      :font -> "font"
      :image -> "image"
    end
  end
end
