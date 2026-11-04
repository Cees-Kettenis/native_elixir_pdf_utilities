defmodule NativeElixirPdfUtilities.Validators.SvgImageValidator do
  @moduledoc false
  alias NativeElixirPdfUtilities.{Diagnostics, Limits}

  alias NativeElixirPdfUtilities.HtmlToPdf.{
    AssetLoader,
    RenderCache,
    ResourceContext,
    SvgScene
  }

  alias NativeElixirPdfUtilities.Validators.{
    HtmlValidator,
    ResourceValidator,
    SvgSceneValidator,
    SvgConverterValidator,
    SvgReferenceValidator,
    SvgViewportValidator
  }

  @doc "Prepares an image and retains its approved bytes for its eventual CSS viewport."
  @spec prepare_viewport_source(term(), term(), term(), term(), term()) ::
          {:ok, map(), map() | nil} | {:error, {atom(), map()}}
  def prepare_viewport_source(source, options, budget, fonts, font_options) do
    with {:ok, request} <-
           SvgConverterValidator.prepare(
             source,
             options,
             budget,
             fonts,
             %{mode: :image},
             font_options
           ) do
      descriptor =
        case ResourceContext.current() do
          nil ->
            nil

          context ->
            id = make_ref()

            RenderCache.fetch(context.scope, {__MODULE__, context.document_id, id}, fn ->
              %{
                source: source,
                fonts: Enum.map(request.fonts, &%{data: &1}),
                budget: budget,
                view_box?: request.view_box?
              }
            end)

            %{id: id, context: context}
        end

      {:ok, request, descriptor}
    end
  end

  @doc "Resolves an issued image viewport without accepting substituted source or font bytes."
  @spec viewport_request(term(), term(), term()) ::
          {:ok, map(), map()} | {:error, {atom(), map()}}
  def viewport_request(descriptor, width, height) do
    with %{id: id, context: context} <- descriptor,
         true <- is_reference(id),
         :ok <- ResourceValidator.validate_context(context),
         {:ok, width} <- HtmlValidator.normalize_geometry_number(width),
         {:ok, height} <- HtmlValidator.normalize_geometry_number(height),
         true <- width > 0 and height > 0,
         request when is_map(request) <-
           RenderCache.fetch(context.scope, {__MODULE__, context.document_id, id}, fn -> nil end) do
      {:ok, Map.merge(request, %{width: width, height: height}), context}
    else
      {:error, _} = error ->
        error

      _ ->
        invalid(
          "SVG image viewport requires its issued source scope and positive dimensions",
          "SVG"
        )
    end
  end

  @doc "Collects authored image references for XML policy inspection without loading them."
  @spec pending_references(map()) :: map()
  def pending_references(node) do
    Map.new(
      for {image, _} <- SvgSceneValidator.tree_nodes(node),
          image.tag in ~w(image feImage),
          not String.starts_with?(
            Map.get(image.attributes, SvgReferenceValidator.href_attribute(image), ""),
            "#"
          ),
          do:
            {Map.get(image.attributes, SvgReferenceValidator.href_attribute(image), ""), :pending}
    )
  end

  @doc false
  @spec validate_node(map()) :: :ok | {:error, {atom(), map()}}
  def validate_node(node) do
    if node.tag in ~w(image feImage) do
      href = Map.get(node.attributes, SvgReferenceValidator.href_attribute(node), "")

      cond do
        node.tag == "feImage" and is_binary(href) and String.starts_with?(href, "#") ->
          :ok

        is_binary(href) and String.trim(href) != "" and not String.starts_with?(href, "#") ->
          with {:ok, _} <-
                 SvgViewportValidator.aspect_ratio(
                   Map.get(node.attributes, "preserveAspectRatio")
                 ),
               do: :ok

        true ->
          invalid(
            "SVG image requires an explicit raster or SVG resource reference",
            node.tag <> " href"
          )
      end
    else
      :ok
    end
  end

  @doc false
  @spec validate_descriptors(map()) :: :ok | {:error, {atom(), map()}}
  def validate_descriptors(resources) do
    Enum.reduce_while(resources, :ok, fn {name, descriptor}, :ok ->
      result =
        case {name, descriptor} do
          {name, %{data: data, media_type: mime}}
          when is_binary(name) and is_binary(data) and
                 mime in ["image/png", "image/jpeg", "image/svg+xml"] ->
            cond do
              not Regex.match?(~r/\Asvg-resource:[A-Za-z0-9_.-]+\z/, name) ->
                invalid("SVG resource keys must be opaque svg-resource: identifiers", "SVG")

              byte_size(data) > Limits.get(:max_svg_resource_bytes) ->
                Diagnostics.error(
                  :limits,
                  :resource_limit_exceeded,
                  "SVG resource exceeds max_svg_resource_bytes",
                  source: name,
                  module: __MODULE__,
                  operation: :prepare
                )

              true ->
                :ok
            end

          _ ->
            invalid("SVG resources require PNG, JPEG or nested SVG byte descriptors", "SVG")
        end

      case result do
        :ok -> {:cont, :ok}
        error -> {:halt, error}
      end
    end)
  end

  @doc "Decodes the shared supported image data URI syntax and charges its original encoded source."
  @spec data_uri(term(), HtmlValidator.image_budget()) ::
          {:ok, binary(), atom()} | :error | {:error, {atom(), map()}}
  def data_uri(source, budget) do
    case Regex.run(
           ~r/^data:(image\/(?:svg\+xml|png|jpeg));base64,([A-Za-z0-9+\/=\r\n]+)$/u,
           source
         ) do
      [_, mime, encoded] ->
        with :ok <- HtmlValidator.reserve_image_source(budget, byte_size(encoded)),
             {:ok, bytes} <- Base.decode64(String.replace(encoded, ~r/\s/u, "")) do
          {:ok, bytes,
           Map.fetch!(
             %{"image/svg+xml" => :svg, "image/png" => :png, "image/jpeg" => :jpeg},
             mime
           )}
        else
          :error -> :error
          error -> error
        end

      _ ->
        :error
    end
  end

  @doc "Prepares only selected image bytes and nested documents in their issued resource scopes."
  @spec prepare_tree(map(), HtmlValidator.image_budget(), keyword(), [map()]) ::
          {:ok, map(), map(), [map()]} | {:error, {atom(), map()}}
  def prepare_tree(node, budget, opts, fonts) do
    nodes =
      Map.new(SvgSceneValidator.tree_nodes(node), fn {item, _} ->
        {Map.get(item.attributes, "id"), item}
      end)

    references = expand_references(active_references(node), nodes, :sets.new(version: 2))
    prepare_selected(node, budget, opts, references, true, false, %{}, fonts)
  end

  # Only reachable definitions participate in selected dependency loading.
  @spec expand_references(MapSet.t(String.t()), map(), :sets.set(String.t())) ::
          MapSet.t(String.t())
  defp expand_references(references, nodes, visited) do
    pending = Enum.reject(references, &:sets.is_element(&1, visited))

    if pending == [] do
      references
    else
      expanded =
        Enum.reduce(pending, references, fn id, ids ->
          case Map.get(nodes, id) do
            nil -> ids
            node -> MapSet.union(ids, active_references(node, true))
          end
        end)

      expand_references(
        expanded,
        nodes,
        :sets.union(visited, :sets.from_list(pending, version: 2))
      )
    end
  end

  defp active_references(node, selected? \\ false) do
    if Map.get(node.style, :display) == :none or
         (not selected? and node.tag in ~w(defs symbol pattern marker mask clipPath)) do
      MapSet.new()
    else
      HtmlValidator.reserve_render_resource(:max_svg_expansion_work, 1, :style)
      values = Map.values(node.attributes) ++ Map.values(Map.get(node, :style, %{}))

      ids =
        Enum.reduce(values, MapSet.new(), fn value, ids ->
          case value do
            "#" <> id ->
              MapSet.put(ids, id)

            {:paint_server, id} ->
              MapSet.put(ids, id)

            {:fragment, id} ->
              MapSet.put(ids, id)

            value when is_binary(value) ->
              Regex.scan(~r/url\(\s*#([^\s)]+)\s*\)/, value)
              |> Enum.reduce(ids, fn [_, id], ids -> MapSet.put(ids, id) end)

            _ ->
              ids
          end
        end)

      Enum.reduce(node.children, ids, fn child, ids ->
        if Map.get(child, :type) == :element and Map.get(child, :namespace) == :svg,
          do: MapSet.union(ids, active_references(child)),
          else: ids
      end)
    end
  end

  defp prepare_selected(node, budget, opts, references, visible?, definition?, resources, fonts) do
    definition? = definition? or node.tag in ~w(defs symbol pattern marker mask clipPath)
    referenced? = MapSet.member?(references, Map.get(node.attributes, "id"))
    definition? = definition? and not referenced?
    visible? = visible? and Map.get(node.style, :display) != :none

    reference = Map.get(node.attributes, SvgReferenceValidator.href_attribute(node), "")
    external_input? = node.tag in ~w(image feImage) and not String.starts_with?(reference, "#")

    if external_input? do
      if visible? and not definition? do
        with {:ok, bytes} <- load(reference, budget, opts),
             {:ok, descriptor, nested_resources, nested_fonts} <-
               prepare_bytes(bytes, reference, budget, opts) do
          key =
            "svg-resource:" <>
              (:crypto.hash(:sha256, :erlang.term_to_binary({reference, descriptor}))
               |> Base.encode16(case: :lower))

          attrs =
            node.attributes
            |> Map.delete(SvgReferenceValidator.href_attribute(node))
            |> Map.put("href", key)

          {:ok, %{node | attributes: attrs},
           Map.merge(resources, nested_resources) |> Map.put(key, descriptor),
           fonts ++ nested_fonts}
        end
      else
        # Preserve the original ID graph for hidden/unused use references.
        # The selected scene has no image resource at this nonpainting target.
        if node.tag == "feImage",
          do: {:ok, nil, resources, fonts},
          else:
            {:ok, %{node | tag: "g", attributes: Map.take(node.attributes, ["id"]), children: []},
             resources, fonts}
      end
    else
      Enum.reduce_while(node.children, {:ok, [], resources, fonts}, fn child,
                                                                       {:ok, children, resources,
                                                                        fonts} ->
        result =
          if Map.get(child, :type) == :element and Map.get(child, :namespace) == :svg,
            do:
              prepare_selected(
                child,
                budget,
                opts,
                references,
                visible?,
                definition?,
                resources,
                fonts
              ),
            else: {:ok, child, resources, fonts}

        case result do
          {:ok, nil, resources, fonts} -> {:cont, {:ok, children, resources, fonts}}
          {:ok, child, resources, fonts} -> {:cont, {:ok, [child | children], resources, fonts}}
          error -> {:halt, error}
        end
      end)
      |> case do
        {:ok, children, resources, fonts} ->
          {:ok, %{node | children: Enum.reverse(children)}, resources, fonts}

        error ->
          error
      end
    end
  end

  defp load(reference, budget, opts) do
    if String.starts_with?(reference, "data:") do
      case data_uri(reference, budget) do
        {:ok, bytes, _} ->
          {:ok, bytes}

        :error ->
          invalid("SVG image requires a supported base64 PNG, JPEG or SVG data URI", "image href")

        error ->
          error
      end
    else
      case ResourceContext.current() do
        %{depth: depth} = context when depth > 0 ->
          AssetLoader.resolve_local(reference, :image, context, opts)

        _ ->
          AssetLoader.resolve(reference, :image, opts, budget)
      end
    end
  end

  defp prepare_bytes(bytes, reference, budget, opts) do
    cond do
      byte_size(bytes) > Limits.get(:max_svg_resource_bytes) ->
        Diagnostics.error(
          :limits,
          :resource_limit_exceeded,
          "SVG resource exceeds max_svg_resource_bytes",
          source: reference,
          module: __MODULE__,
          operation: :prepare
        )

      String.starts_with?(bytes, <<137, 80, 78, 71, 13, 10, 26, 10>>) ->
        {:ok, %{data: bytes, media_type: "image/png"}, %{}, []}

      String.starts_with?(bytes, <<255, 216>>) ->
        {:ok, %{data: bytes, media_type: "image/jpeg"}, %{}, []}

      String.valid?(bytes) ->
        context = ResourceContext.current()

        with {:ok, child} <- document_context(reference, context, opts) do
          ResourceContext.with_document(child, fn ->
            safe_opts =
              opts
              |> Enum.reject(fn {key, _} ->
                key == :asset_resolver or String.starts_with?(Atom.to_string(key), "__")
              end)
              |> Keyword.put(:base_url, child.resolution_base)

            with {:ok, scene} <- SvgScene.extract(bytes, budget, safe_opts) do
              {:ok, %{data: scene.document_source, media_type: "image/svg+xml"}, scene.resources,
               scene.selected_fonts}
            end
          end)
        end

      true ->
        invalid("SVG image bytes must contain supported PNG, JPEG or SVG artwork", reference)
    end
  end

  # A root mapping may supply bytes from a different actual file. Its directory,
  # not the authored alias, is the nested document's base, within the fixed root.
  defp document_context(reference, context, opts) do
    with :ok <- ResourceValidator.validate_context(context) do
      mapped = Map.get(Keyword.get(opts, :assets, %{}), reference)

      source =
        case mapped do
          {:file, path} -> path
          _ -> reference
        end

      if not String.starts_with?(reference, "data:") and
           (context.depth > 0 or
              match?({:ok, _}, ResourceValidator.authorize_reference(source, context))) do
        with {:ok, _, child} <- ResourceValidator.authorize_document(source, context),
             do: {:ok, child}
      else
        with {:ok, _, child} <- ResourceValidator.authorize_document({:srcdoc, ""}, context) do
          case URI.parse(source) do
            %URI{scheme: scheme} when scheme in ["http", "https"] ->
              ResourceContext.rebase(child, {:document_url, source})

            _ ->
              if match?({:file, _}, mapped),
                do: ResourceContext.rebase(child, nil),
                else: {:ok, child}
          end
        end
      end
    end
  end

  defp invalid(message, source) do
    Diagnostics.error(:style, :invalid_document, message,
      source: source,
      module: __MODULE__,
      operation: :prepare
    )
  end
end
