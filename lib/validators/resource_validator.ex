defmodule NativeElixirPdfUtilities.Validators.ResourceValidator do
  @moduledoc """
  Authorizes embedded sources before asset mappings, caches, callbacks or reads.

  Referenced documents are local files. Inline HTML uses a distinct `:srcdoc`
  descriptor. Child dependencies may also use explicitly supplied bytes, but a
  mapping cannot authorize a remote reference or an out-of-root file. Missing
  authorized files are load failures, not authorization denials.
  """

  alias NativeElixirPdfUtilities.Diagnostics
  alias NativeElixirPdfUtilities.HtmlToPdf.ResourceContext
  alias NativeElixirPdfUtilities.Limits
  alias NativeElixirPdfUtilities.Validators.HtmlUrlValidator
  alias NativeElixirPdfUtilities.Validators.HtmlValidator

  @type dependency_descriptor :: {:file, String.t()} | {:bytes, binary()}
  @type document_descriptor :: {:file, String.t()} | {:srcdoc, String.t()} | {:blank, String.t()}

  @doc "Identifies an embedded source in diagnostics without selecting or authorizing it."
  @spec document_source_label(term()) :: String.t() | nil
  def document_source_label(source) do
    case source do
      {:srcdoc, _html} -> "about:srcdoc"
      {:blank, url} when is_binary(url) -> url
      reference when is_binary(reference) -> reference
      _ -> nil
    end
  end

  @doc false
  @spec renderer_context_options(keyword()) :: keyword()
  def renderer_context_options(opts) do
    base = Keyword.get(opts, :base_url)

    root =
      case base do
        base when is_binary(base) ->
          if URI.parse(base).scheme in [nil, "file"], do: base, else: nil

        nil ->
          nil
      end

    [authorization_root: root, base_url: base]
  end

  @doc false
  @spec iframe_source(map()) :: document_descriptor() | String.t()
  def iframe_source(attributes) do
    case Map.fetch(attributes, "srcdoc") do
      {:ok, html} ->
        {:srcdoc, html}

      :error ->
        source = attributes |> Map.get("src", "") |> HtmlValidator.trim_ascii_whitespace()

        if source == "" do
          {:blank, "about:blank"}
        else
          case HtmlUrlValidator.blank_document_url(source) do
            {:ok, _url} -> {:blank, source}
            :not_blank -> source
          end
        end
    end
  end

  @doc false
  @spec prepare_context(term(), term(), reference()) ::
          {:ok, ResourceContext.t()} | {:error, {atom(), Diagnostics.diagnostic()}}
  def prepare_context(opts, image_budget, scope) do
    with true <- is_list(opts) and Keyword.keyword?(opts),
         {:ok, root} <- local_base(Keyword.get(opts, :authorization_root)),
         :ok <- validate_root(root),
         :ok <- HtmlValidator.validate_image_budget(image_budget),
         context <- %ResourceContext{
           scope: scope,
           authorization_root: root,
           resolution_base: root,
           image_budget: image_budget,
           document_id: make_ref(),
           tree_scope_id: make_ref()
         },
         {:ok, base} <- resolution_base(Keyword.get(opts, :base_url, root), context) do
      {:ok, %{context | resolution_base: base}}
    else
      false -> error(:invalid_options, "resource context options must be a keyword list", nil)
      :error -> error(:invalid_options, "authorization_root must be a local directory", nil)
      {:error, {_reason, _diagnostic}} = error -> error
    end
  end

  @doc false
  @spec validate_context(term()) :: :ok | {:error, {atom(), Diagnostics.diagnostic()}}
  def validate_context(context) do
    case context do
      %ResourceContext{scope: scope, authorization_root: root, image_budget: image_budget} ->
        case Process.get({ResourceContext, scope}) do
          %{authorization_root: ^root, image_budget: ^image_budget, documents: documents} ->
            case Map.get(documents, context.document_id) do
              {tree_scope_id, ancestors, depth}
              when tree_scope_id == context.tree_scope_id and ancestors == context.ancestors and
                     depth == context.depth ->
                :ok

              _ ->
                error(
                  :invalid_options,
                  "resource context document identity or ancestry was changed",
                  nil
                )
            end

          _ ->
            error(:invalid_options, "resource context is outside its immutable scope", nil)
        end

      _ ->
        error(:invalid_options, "resource loading requires a scoped context", nil)
    end
  end

  @doc "Validates that document preparation shares the current issued image budget."
  @spec validate_preparation_budget(term()) ::
          :ok | {:error, {atom(), Diagnostics.diagnostic()}}
  def validate_preparation_budget(image_budget) do
    context = ResourceContext.current()

    with :ok <- validate_context(context) do
      if image_budget == context.image_budget do
        :ok
      else
        Diagnostics.error(
          :options,
          :invalid_options,
          "document preparation must use the current issued image budget",
          operation: :prepare_document,
          module: __MODULE__,
          source: "image_budget"
        )
      end
    end
  end

  @doc false
  @spec rebase(ResourceContext.t(), term()) ::
          {:ok, ResourceContext.t()} | {:error, {atom(), Diagnostics.diagnostic()}}
  def rebase(context, base) do
    with :ok <- validate_context(context) do
      case base do
        {:document_url, url} ->
          parsed =
            if url == nil, do: {:ok, nil}, else: HtmlUrlValidator.resolve_reference(url, nil)

          with {:ok, url} <- parsed,
               {:ok, base} <-
                 (case if(is_binary(url), do: URI.parse(url)) do
                    %URI{scheme: "file", host: host, path: path, userinfo: nil, port: nil}
                    when host in [nil, "", "localhost"] and is_binary(path) ->
                      with {:ok, _path} <-
                             authorize_local_path(
                               URI.decode(path),
                               context.authorization_root,
                               context.authorization_root,
                               :child
                             ),
                           do: {:ok, url}

                    _ ->
                      resolution_base(url, context)
                  end),
               do: {:ok, %{context | resolution_base: base}}

        _ ->
          with {:ok, base} <- resolution_base(base, context),
               do: {:ok, %{context | resolution_base: base}}
      end
    end
  end

  @doc false
  @spec authorize_document(term(), ResourceContext.t(), term()) ::
          {:ok, document_descriptor(), ResourceContext.t()}
          | {:error, {atom(), Diagnostics.diagnostic()}}
  def authorize_document(source, context, opts \\ []) do
    with :ok <- validate_context(context),
         {:ok, descriptor, child} <- select_document_source(source, context),
         :ok <- validate_load_options(opts) do
      {:ok, descriptor, child}
    end
  catch
    {:render_resource_limit, {:error, {reason, diagnostic}}} ->
      {:error,
       {reason,
        Diagnostics.with_context(diagnostic,
          operation: :load_embedded_resource,
          module: __MODULE__,
          source: document_source_label(source)
        )}}
  end

  defp select_document_source(source, context) do
    case source do
      {:srcdoc, html} ->
        with {:ok, html} <- HtmlValidator.validate_html_source(html),
             {:ok, child} <- child_context(context, nil, context.resolution_base, nil) do
          {:ok, {:srcdoc, html}, child}
        end

      {:blank, url} ->
        case HtmlUrlValidator.blank_document_url(url) do
          {:ok, _normalized} ->
            with {:ok, child} <- child_context(context, nil, context.resolution_base, nil),
                 do: {:ok, {:blank, url}, child}

          :not_blank ->
            error(:invalid_document, "blank embedded source requires an about:blank URL", url)
        end

      reference when is_binary(reference) ->
        with {:ok, path} <- authorize_reference(reference, context),
             {:ok, identity} <- file_identity(path),
             {:ok, child} <- child_context(context, identity, Path.dirname(path), reference) do
          {:ok, {:file, path}, child}
        end

      _ ->
        error(:invalid_document, "embedded source must be a local path or {:srcdoc, HTML}", nil)
    end
  end

  @doc false
  @spec authorize_dependency(term(), term(), ResourceContext.t(), term()) ::
          {:ok, dependency_descriptor()} | {:error, {atom(), Diagnostics.diagnostic()}}
  def authorize_dependency(reference, kind, context, opts) do
    with :ok <- validate_context(context),
         true <- kind in [:image, :background_image, :font, :stylesheet],
         {:ok, descriptor} <-
           (case reference do
              {:bytes, bytes} when is_binary(bytes) ->
                {:ok, {:bytes, bytes}}

              _ ->
                with {:ok, path} <- authorize_reference(reference, context),
                     do: {:ok, {:file, path}}
            end),
         :ok <- validate_load_options(opts) do
      case descriptor do
        {:bytes, _bytes} ->
          {:ok, descriptor}

        {:file, path} ->
          case Map.fetch(Keyword.get(opts, :assets, %{}), reference) do
            {:ok, {:bytes, bytes}} ->
              {:ok, {:bytes, bytes}}

            {:ok, {:file, mapped_path}} ->
              with {:ok, mapped_path} <- authorize_reference(mapped_path, context) do
                {:ok, {:file, mapped_path}}
              end

            :error ->
              {:ok, {:file, path}}
          end
      end
    else
      false -> error(:invalid_options, "invalid child dependency kind or options", nil)
      {:error, {_reason, _diagnostic}} = error -> error
    end
  end

  defp validate_load_options(opts) do
    case is_list(opts) and Keyword.keyword?(opts) do
      true ->
        HtmlValidator.validate_asset_options(
          Keyword.get(opts, :assets, %{}),
          Keyword.get(opts, :asset_resolver)
        )

      false ->
        error(:invalid_options, "child resource options must be a keyword list", nil)
    end
  end

  @doc false
  @spec authorize_reference(term(), ResourceContext.t()) ::
          {:ok, String.t()} | {:error, {atom(), Diagnostics.diagnostic()}}
  def authorize_reference(reference, context) do
    with :ok <- validate_context(context),
         reference when is_binary(reference) and reference != "" <- reference,
         false <- String.contains?(reference, ["\0", "\\"]),
         false <- String.starts_with?(reference, "//"),
         %URI{scheme: nil, host: nil, path: path} when is_binary(path) <-
           URI.parse(reference),
         path <- URI.decode(path),
         false <- String.contains?(path, ["\0", "\\"]),
         false <- String.starts_with?(path, "//"),
         {:ok, path} <-
           authorize_local_path(path, context.authorization_root, context.resolution_base, :child) do
      {:ok, path}
    else
      {:error, {_reason, _diagnostic}} = failure ->
        {:error, {reason, diagnostic}} = failure
        {:error, {reason, Diagnostics.with_context(diagnostic, source: reference)}}

      _ ->
        error(:resource_denied, "child resources require an authorized local path", reference)
    end
  end

  @doc false
  @spec authorize_local_path(term(), term(), term(), :legacy | :child) ::
          {:ok, String.t()} | {:error, {atom(), Diagnostics.diagnostic()}}
  def authorize_local_path(source, root_url, base_url, mode) do
    with source when is_binary(source) and source != "" <- source,
         false <- String.contains?(source, ["\0", "://"]),
         {:ok, root} when is_binary(root) <- local_base(root_url),
         {:ok, base} when is_binary(base) <-
           if(mode == :child,
             do: child_resolution_directory(base_url),
             else: local_base(base_url)
           ),
         path <- Path.expand(source, base),
         relative <- Path.relative_to(path, root),
         true <- contained?(relative),
         true <- symlink_free?(root, relative, mode == :child),
         :ok <- if(mode == :child, do: validate_root(root), else: :ok) do
      {:ok, path}
    else
      _ ->
        case mode do
          :legacy ->
            Diagnostics.error(
              :style,
              :invalid_document,
              "local document resource path is not authorized by base_url"
            )

          :child ->
            error(
              :resource_denied,
              "local path is outside the authorization root or uses a symlink",
              source
            )
        end
    end
  end

  @doc false
  @spec file_identity(String.t()) ::
          {:ok, term()} | {:error, {atom(), Diagnostics.diagnostic()}}
  def file_identity(path) do
    case File.stat(path) do
      {:ok, stat} ->
        {:ok, {stat.major_device, stat.minor_device, stat.inode}}

      {:error, reason} when reason in [:enoent, :enotdir] ->
        {:ok, {:missing, path}}

      {:error, reason} ->
        error(:invalid_document, "authorized file cannot be inspected: #{reason}", path)
    end
  end

  defp child_context(context, identity, base, source) do
    cond do
      not is_nil(identity) and identity in context.ancestors ->
        error(:resource_cycle, "embedded document repeats an active ancestor file", source)

      context.depth + 1 > Limits.get(:max_embedded_document_depth) ->
        error(
          :resource_limit_exceeded,
          "embedded documents exceed max_embedded_document_depth",
          source
        )

      true ->
        HtmlValidator.reserve_render_resource(:max_embedded_documents, 1, :resource)

        child = %{
          context
          | document_id: make_ref(),
            tree_scope_id: make_ref(),
            resolution_base: base,
            depth: context.depth + 1,
            ancestors:
              if(is_nil(identity), do: context.ancestors, else: [identity | context.ancestors])
        }

        key = {ResourceContext, context.scope}
        state = Process.get(key)

        documents =
          Map.put(
            state.documents,
            child.document_id,
            {child.tree_scope_id, child.ancestors, child.depth}
          )

        Process.put(key, %{state | documents: documents})
        {:ok, child}
    end
  end

  defp resolution_base(base, context) do
    case base do
      nil ->
        {:ok, nil}

      base when is_binary(base) and base != "" ->
        case URI.parse(base) do
          %URI{scheme: scheme, host: host} when scheme in ["http", "https"] and is_binary(host) ->
            {:ok, base}

          %URI{scheme: nil, host: host} when is_binary(host) ->
            {:ok, base}

          _ ->
            case context.authorization_root do
              nil ->
                error(
                  :resource_denied,
                  "a local resolution base requires an authorization root",
                  base
                )

              root ->
                source =
                  case URI.parse(base) do
                    %URI{scheme: "file", host: host, path: path, query: nil, fragment: nil}
                    when host in [nil, "", "localhost"] and is_binary(path) ->
                      path

                    _ ->
                      base
                  end

                resolution_base =
                  if Path.type(source) == :absolute,
                    do: root,
                    else: context.resolution_base || root

                authorize_local_path(source, root, resolution_base, :child)
            end
        end

      _ ->
        error(:invalid_options, "resolution base must be a local directory or remote URL", nil)
    end
  end

  defp child_resolution_directory(base) do
    case if(is_binary(base), do: URI.parse(base)) do
      %URI{scheme: "file", host: host, path: path, userinfo: nil, port: nil}
      when host in [nil, "", "localhost"] and is_binary(path) ->
        path = URI.decode(path)

        if String.contains?(path, ["\0", "\\"]),
          do: :error,
          else: {:ok, if(String.ends_with?(path, "/"), do: path, else: Path.dirname(path))}

      _ ->
        local_base(base)
    end
  end

  defp local_base(base) do
    case base do
      nil ->
        {:ok, nil}

      base when is_binary(base) ->
        case URI.parse(base) do
          %URI{scheme: nil, host: nil, path: path, query: nil, fragment: nil}
          when is_binary(path) and path != "" ->
            {:ok, Path.expand(path)}

          %URI{scheme: "file", host: host, path: path, query: nil, fragment: nil}
          when host in [nil, "", "localhost"] and is_binary(path) and path != "" ->
            {:ok, Path.expand(path)}

          _ ->
            :error
        end

      _ ->
        :error
    end
  end

  defp validate_root(root) do
    case root do
      nil ->
        :ok

      root ->
        with true <- symlink_free?("/", Path.relative_to(root, "/"), false),
             {:ok, %File.Stat{type: :directory}} <- File.lstat(root) do
          :ok
        else
          _ ->
            error(
              :resource_denied,
              "authorization root must be an existing directory without symlink ancestors",
              root
            )
        end
    end
  end

  defp contained?(relative) do
    relative == "." or
      (Path.type(relative) == :relative and relative != ".." and
         not String.starts_with?(relative, "../"))
  end

  defp symlink_free?(root, relative, allow_missing) do
    relative
    |> Path.split()
    |> Enum.reduce_while(root, fn component, current ->
      path = Path.join(current, component)

      case File.lstat(path) do
        {:ok, %File.Stat{type: :symlink}} -> {:halt, false}
        {:ok, _stat} -> {:cont, path}
        {:error, reason} when allow_missing and reason in [:enoent, :enotdir] -> {:cont, path}
        {:error, _reason} -> {:halt, false}
      end
    end)
    |> is_binary()
  end

  defp error(reason, message, source) do
    Diagnostics.error(:resource, reason, message,
      operation: :load_embedded_resource,
      module: __MODULE__,
      source: if(is_binary(source), do: source)
    )
  end
end
