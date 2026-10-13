defmodule NativeElixirPdfUtilities.Validators.SvgLinkValidator do
  @moduledoc false
  alias NativeElixirPdfUtilities.{Diagnostics, Limits}
  alias NativeElixirPdfUtilities.HtmlToPdf.ResourceContext

  alias NativeElixirPdfUtilities.Validators.{
    HtmlUrlValidator,
    HtmlValidator,
    HtmlLinkValidator,
    SvgSceneValidator
  }

  @doc false
  @spec prepare_tree(map(), keyword()) :: {:ok, map()} | {:error, {atom(), map()}}
  def prepare_tree(root, options \\ []) do
    nodes = SvgSceneValidator.tree_nodes(root)
    current = ResourceContext.current()
    base = if current, do: current.resolution_base, else: nil

    urls =
      Keyword.get(options, :__document_url_context__, %{
        fallback_base: base,
        resolution_base: base,
        authored_base?: false
      })

    context = Keyword.get(options, :__link_context__)
    reuse? = Enum.any?(nodes, fn {node, _} -> node.tag == "use" end)

    result =
      Enum.reduce_while(nodes, {:ok, %{urls: [], destinations: %{}, targets: %{}}}, fn {node,
                                                                                        ancestors},
                                                                                       {:ok, plan} ->
        plan =
          if context && Map.has_key?(node, :_selector_id) do
            case HtmlLinkValidator.target(node, context) do
              nil ->
                plan

              identity ->
                %{plan | targets: Map.put(plan.targets, Map.get(node.attributes, "id"), identity)}
            end
          else
            plan
          end

        if Map.get(node, :namespace) == :svg and node.tag == "a" and
             (Map.has_key?(node.attributes, "href") or Map.has_key?(node.attributes, "xlink:href")) do
          href = Map.get(node.attributes, "href", Map.get(node.attributes, "xlink:href", ""))

          result =
            cond do
              reuse? and Enum.any?(ancestors, &(&1 in ~w(defs symbol))) ->
                failure(
                  "SVG referenced anchor scopes require original per-instance link geometry; wrap the use instance in its source anchor",
                  "use"
                )

              "a" in ancestors ->
                failure("SVG anchors cannot contain nested anchors", "a")

              Enum.any?(ancestors, &(&1 in ~w(text tspan textPath pattern marker mask))) ->
                failure(
                  "SVG inline-text/paint-server anchors require original range or instance geometry preparation",
                  "a"
                )

              Enum.any?(SvgSceneValidator.tree_nodes(node), fn {child, _} ->
                child.tag == "foreignObject"
              end) ->
                failure(
                  "SVG foreignObject anchors require actual inserted HTML surface link geometry",
                  "a"
                )

              Enum.any?(nodes, fn {child, _} ->
                Map.get(child.attributes, "mask", "none") != "none" or
                    Map.get(child.attributes, "filter", "none") != "none"
              end) ->
                failure(
                  "SVG masked/filtered hyperlink regions require their actual effect geometry plan",
                  "a"
                )

              true ->
                HtmlUrlValidator.prepare_anchor(href, urls)
            end

          case result do
            {:ok, destination} ->
              source =
                case destination do
                  {:uri, url} -> url
                  {:fragment, id} -> "#" <> id
                end

              destination = if context, do: Map.get(context.links, href), else: destination

              {:cont,
               {:ok,
                %{
                  plan
                  | urls: [source | plan.urls],
                    destinations:
                      Map.put(
                        plan.destinations,
                        map_size(plan.destinations),
                        {source, destination}
                      )
                }}}

            error ->
              {:halt, error}
          end
        else
          {:cont, {:ok, plan}}
        end
      end)

    case result do
      {:ok, plan} -> {:ok, %{plan | urls: Enum.reverse(plan.urls)}}
      error -> error
    end
  end

  @doc false
  @spec project_tree(map(), map()) :: map()
  def project_tree(root, plan) do
    {tree, []} = canonical_tree(root, plan.urls)
    tree
  end

  # Preorder matches the original SVG XML anchor order consumed by the native
  # preparation. Canonical URLs retain the shared host base and URL authority.
  defp canonical_tree(node, urls) do
    {node, urls} =
      if Map.get(node, :namespace) == :svg and node.tag == "a" and
           (Map.has_key?(node.attributes, "href") or Map.has_key?(node.attributes, "xlink:href")) do
        [url | tail] = urls
        {%{node | attributes: Map.put(node.attributes, "href", url)}, tail}
      else
        {node, urls}
      end

    {children, urls} =
      Enum.map_reduce(node.children, urls, fn child, remaining ->
        case child do
          %{type: :element, namespace: :svg} -> canonical_tree(child, remaining)
          _ -> {child, remaining}
        end
      end)

    {%{node | children: children}, urls}
  end

  @doc false
  @spec prepare_summary(map()) :: {:ok, [binary()]} | {:error, {atom(), map()}}
  def prepare_summary(summary) do
    if summary.context.mode == :inline do
      with {:ok, plan} <- prepare_tree(SvgSceneValidator.summary_tree(summary)),
           do: {:ok, plan.urls}
    else
      {:ok, []}
    end
  end

  @doc false
  @spec validate_nested_request(map(), binary()) :: :ok | {:error, {atom(), map()}}
  def validate_nested_request(request, source) do
    if request.annotations?,
      do:
        failure(
          "Nested SVG image anchors require their original child-document annotation transform and destination scope",
          source
        ),
      else: :ok
  end

  @doc false
  @spec native_ceilings() :: {non_neg_integer(), non_neg_integer(), non_neg_integer()}
  def native_ceilings() do
    budget = Process.get({HtmlValidator, :render_budget}, %{})

    {max(Limits.get(:max_svg_expansion_work) - Map.get(budget, :max_svg_expansion_work, 0), 0),
     max(Limits.get(:max_layout_boxes) - Map.get(budget, :max_layout_boxes, 0), 0),
     Limits.get(:max_pdf_objects)}
  end

  @doc false
  @spec native_result(term()) :: {:ok, binary()} | {:error, {atom(), map()}}
  def native_result(result) do
    case result do
      {:ok, {pdf, work, boxes}} ->
        HtmlValidator.reserve_render_resource(:max_svg_expansion_work, work, :layout)
        HtmlValidator.reserve_render_resource(:max_layout_boxes, boxes, :layout)
        {:ok, pdf}

      {:error, message} ->
        resource? =
          Enum.any?(
            ~w(max_svg_expansion_work max_layout_boxes max_pdf_objects),
            &String.contains?(message, &1)
          )

        Diagnostics.error(
          if(resource?, do: :limits, else: :layout),
          if(resource?, do: :resource_limit_exceeded, else: :invalid_document),
          message,
          source: "SVG links",
          module: __MODULE__,
          operation: :prepare
        )
    end
  end

  @doc false
  @spec project(map(), map()) :: {:ok, [map()]} | {:error, {atom(), map()}}
  def project(form, plan) do
    links =
      Enum.flat_map(form.links, fn link ->
        destination =
          case Map.fetch(plan.destinations, link.index) do
            {:ok, {source, destination}} when source == link.source -> destination
            _ -> nil
          end

        [left, bottom, right, top] = link.rect

        base = %{
          type: :link_region,
          x: left,
          y: bottom,
          width: right - left,
          height: top - bottom
        }

        case destination do
          {:uri, url} ->
            [Map.put(base, :link_url, url)]

          destination when is_tuple(destination) and elem(destination, 0) in [:target, :top] ->
            [Map.put(base, :link_destination, destination)]

          _ ->
            []
        end
      end)

    targets =
      Enum.flat_map(form.targets, fn {id, [left, bottom, _right, top]} ->
        case Map.get(plan.targets, id) do
          nil ->
            []

          identity ->
            [
              %{
                type: :flow_marker,
                x: left,
                y: bottom,
                width: 0.0,
                height: top - bottom,
                html_targets: [%{identity: identity, dx: 0.0, dy: top - bottom}]
              }
            ]
        end
      end)

    {:ok, links ++ targets}
  end

  defp failure(message, source),
    do:
      Diagnostics.error(:layout, :unsupported_html, message,
        source: source,
        module: __MODULE__,
        operation: :prepare
      )
end
