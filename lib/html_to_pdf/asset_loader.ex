defmodule NativeElixirPdfUtilities.HtmlToPdf.AssetLoader do
  @moduledoc false

  alias NativeElixirPdfUtilities.Validators.FontValidator
  alias NativeElixirPdfUtilities.FileReader
  alias NativeElixirPdfUtilities.Diagnostics
  alias NativeElixirPdfUtilities.Validators.HtmlUrlValidator
  alias NativeElixirPdfUtilities.Validators.HtmlValidator
  alias NativeElixirPdfUtilities.Validators.ResourceValidator
  alias NativeElixirPdfUtilities.HtmlToPdf.ResourceContext
  alias NativeElixirPdfUtilities.Limits
  alias NativeElixirPdfUtilities.Validators.TemplateRendererContextValidator

  @type kind :: :image | :background_image | :font

  @doc false
  @spec resolve(String.t(), kind(), keyword(), HtmlValidator.image_budget() | nil) ::
          {:ok, binary()} | {:error, {atom(), Diagnostics.diagnostic()}}
  def resolve(reference, kind, opts, image_budget \\ nil) do
    TemplateRendererContextValidator.with_resource_observation(reference, kind, opts, fn ->
      result =
        FontValidator.with_budget(fn ->
          case kind do
            :font ->
              FontValidator.reserve(:max_font_candidates, 1)

              case Map.get(Keyword.get(opts, :assets, %{}), reference) do
                {:bytes, bytes} -> FontValidator.check(:max_font_source_bytes, byte_size(bytes))
                _ -> :ok
              end

              FontValidator.memo(
                {:asset, reference, Keyword.get(opts, :base_url), Keyword.get(opts, :assets, %{}),
                 Keyword.get(opts, :asset_resolver),
                 Keyword.get(opts, :__document_url_context__)},
                fn ->
                  do_resolve(reference, kind, opts, :font)
                end
              )

            _ ->
              do_resolve(reference, kind, opts, image_budget)
          end
        end)

      TemplateRendererContextValidator.observe_resource(
        reference,
        kind,
        result,
        :ordinary_renderer_options,
        nil,
        opts
      )

      result
    end)
  end

  @doc "Resolves a child dependency after local authorization, without invoking its resolver."
  @spec resolve_local(term(), kind() | :stylesheet, ResourceContext.t(), keyword()) ::
          {:ok, binary()} | {:error, {atom(), Diagnostics.diagnostic()}}
  def resolve_local(reference, kind, context, opts \\ []) do
    TemplateRendererContextValidator.with_resource_observation(reference, kind, opts, fn ->
      with {:ok, source} <-
             ResourceValidator.authorize_dependency(reference, kind, context, opts) do
        budget =
          case kind do
            :font -> :local_font
            :stylesheet -> :stylesheet
            _ -> context.image_budget
          end

        result =
          FontValidator.with_budget(fn ->
            if kind == :font, do: FontValidator.reserve(:max_font_candidates, 1)
            resolve_explicit_source(source, reference, budget)
          end)

        TemplateRendererContextValidator.observe_resource(
          reference,
          kind,
          result,
          :issued_local_dependency,
          source,
          opts,
          context
        )

        result
      end
    end)
  end

  defp do_resolve(reference, kind, opts, image_budget) do
    assets = Keyword.get(opts, :assets, %{})

    case Map.fetch(assets, reference) do
      {:ok, source} ->
        resolve_explicit_source(source, reference, image_budget)

      :error ->
        prepared =
          case Keyword.get(opts, :__document_url_context__) do
            nil -> {:ok, reference}
            context -> HtmlUrlValidator.resource_reference(reference, context)
          end

        with {:ok, resolved} <- prepared do
          case Map.fetch(assets, resolved) do
            {:ok, source} -> resolve_explicit_source(source, resolved, image_budget)
            :error -> resolve_unmapped_reference(resolved, kind, opts, image_budget)
          end
        end
    end
  end

  defp resolve_explicit_source(source, reference, image_budget) do
    case source do
      {:bytes, bytes} ->
        with :ok <- reserve_source_bytes(image_budget, bytes) do
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
          {:ok, context} ->
            case read_file(context, reference, image_budget) do
              {:error, {:invalid_document, _}} = error ->
                if is_function(Keyword.get(opts, :asset_resolver), 1),
                  do: resolve_with_callback(reference, kind, opts, image_budget, :local),
                  else: error

              result ->
                result
            end

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
            with :ok <- reserve_source_bytes(image_budget, bytes) do
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
    maximum =
      case image_budget do
        :font -> {:ok, FontValidator.read_limit()}
        :local_font -> {:ok, Limits.get(:max_font_source_bytes)}
        :stylesheet -> {:ok, Limits.get(:max_css_source_bytes)}
        _ -> HtmlValidator.image_source_read_limit(image_budget)
      end

    with {:ok, maximum} <- maximum,
         result = FileReader.read(path, maximum),
         {:ok, bytes} <-
           if(image_budget in [:font, :local_font],
             do: FontValidator.source_result(result),
             else: result
           ),
         :ok <- reserve_source_bytes(image_budget, bytes) do
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

  defp reserve_source_bytes(budget, bytes) do
    case budget do
      budget when budget in [:font, :local_font] -> FontValidator.reserve_source(bytes)
      :stylesheet -> :ok
      _ -> reserve_image_source(budget, byte_size(bytes))
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
