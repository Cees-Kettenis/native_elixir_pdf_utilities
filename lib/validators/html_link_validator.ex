defmodule NativeElixirPdfUtilities.Validators.HtmlLinkValidator do
  @moduledoc """
  Selects HTML fragment targets in one document tree and resolves their final
  paginated positions. Preparation never grants resource permissions.

  Prepared identities include separate document, tree and node references. A
  hidden first target remains selected but has no PDF destination. Missing targets
  also have no annotation. Empty fragments and unmatched ASCII `top` select the
  beginning of the document. Final destinations use PDF bottom-left points.
  """

  alias NativeElixirPdfUtilities.HtmlToPdf.DrawingGeometry
  alias NativeElixirPdfUtilities.Validators.WriterValidator
  alias NativeElixirPdfUtilities.Diagnostics
  alias NativeElixirPdfUtilities.Validators.HtmlValidator
  alias NativeElixirPdfUtilities.Validators.HtmlUrlValidator

  @type identity :: {reference(), reference(), reference()}
  @type context :: %{
          document_id: reference(),
          tree_scope_id: reference(),
          ids: map(),
          names: map(),
          links: map(),
          selected: MapSet.t(),
          document_targets: [map()]
        }
  @type destination :: {:xyz, pos_integer(), number(), number()}

  @doc false
  @spec prepare([map()], HtmlUrlValidator.context()) ::
          {:ok, context()} | {:error, {atom(), Diagnostics.diagnostic()}}
  def prepare(children, url_context) do
    prepare_document(children, url_context, make_ref(), make_ref())
  end

  @doc false
  @spec prepare(
          [map()],
          HtmlUrlValidator.context(),
          NativeElixirPdfUtilities.HtmlToPdf.ResourceContext.t()
        ) :: {:ok, context()} | {:error, {atom(), Diagnostics.diagnostic()}}
  def prepare(children, url_context, resource_context) do
    with :ok <-
           NativeElixirPdfUtilities.Validators.ResourceValidator.validate_context(
             resource_context
           ) do
      prepare_document(
        children,
        url_context,
        resource_context.document_id,
        resource_context.tree_scope_id
      )
    end
  end

  defp prepare_document(children, url_context, document_id, tree_scope_id) do
    context =
      collect(children, %{
        document_id: document_id,
        tree_scope_id: tree_scope_id,
        ids: %{},
        names: %{},
        links: %{},
        selected: MapSet.new(),
        document_targets: []
      })

    with {:ok, links} <-
           Enum.reduce_while(context.links, {:ok, %{}}, fn {href, _}, {:ok, links} ->
             case HtmlUrlValidator.prepare_anchor(href, url_context) do
               {:ok, destination} -> {:cont, {:ok, Map.put(links, href, destination)}}
               error -> {:halt, error}
             end
           end) do
      index =
        if Enum.any?(links, fn {_href, destination} -> match?({:fragment, _}, destination) end),
          do: NativeElixirPdfUtilities.Validators.HtmlMetadataValidator.first_tree_ids(children),
          else: %{}

      ids =
        Map.new(index, fn {{_logical_document, _logical_tree, id}, node} ->
          {id, {document_id, tree_scope_id, node._selector_id}}
        end)

      context = %{context | ids: ids, links: links}

      context =
        Enum.reduce(links, context, fn {href, destination}, context ->
          case destination do
            {:fragment, decoded} ->
              destination = select(href, decoded, context)

              selected =
                case destination do
                  {:target, identity} -> MapSet.put(context.selected, identity)
                  _ -> context.selected
                end

              %{context | links: Map.put(context.links, href, destination), selected: selected}

            {:uri, _url} ->
              context
          end
        end)

      document_targets =
        for {{_logical_document, _logical_tree, id}, %{tag: "html"} = node} <- index,
            Map.get(node, :namespace, :html) == :html,
            identity = Map.fetch!(ids, id),
            MapSet.member?(context.selected, identity),
            do: %{identity: identity, source: id}

      {:ok, %{context | document_targets: document_targets}}
    end
  end

  defp collect(children, context) do
    Enum.reduce(children, context, fn node, context ->
      case node do
        %{type: :element, tag: "template"} ->
          context

        %{type: :element, attributes: attributes, children: nested, _selector_id: node_id} ->
          identity = {context.document_id, context.tree_scope_id, node_id}

          name =
            if node.tag == "a" and Map.get(node, :namespace, :html) == :html,
              do: Map.get(attributes, "name", ""),
              else: ""

          names =
            if name == "", do: context.names, else: Map.put_new(context.names, name, identity)

          links =
            case {node.tag, Map.get(attributes, "href")} do
              {tag, href} when tag in ["a", "area"] and is_binary(href) ->
                Map.put_new(context.links, href, nil)

              _ ->
                context.links
            end

          collect(nested, %{context | names: names, links: links})

        _ ->
          context
      end
    end)
  end

  @doc false
  @spec target(map(), context()) :: identity() | nil
  def target(node, context) do
    identity = {context.document_id, context.tree_scope_id, node._selector_id}
    if MapSet.member?(context.selected, identity), do: identity, else: nil
  end

  @doc false
  @spec select(String.t(), String.t(), context()) :: {:target, identity()} | {:top, tuple()} | nil
  def select(href, decoded, context) do
    raw = HtmlUrlValidator.raw_fragment(href)

    target =
      Map.get(context.ids, raw) || Map.get(context.names, raw) ||
        Map.get(context.ids, decoded) || Map.get(context.names, decoded)

    cond do
      decoded == "" ->
        {:top, {context.document_id, context.tree_scope_id}}

      target != nil ->
        {:target, target}

      Regex.match?(~r/\A[tT][oO][pP]\z/, decoded) ->
        {:top, {context.document_id, context.tree_scope_id}}

      true ->
        nil
    end
  end

  @doc false
  @spec valid_identity?(term()) :: boolean()
  def valid_identity?(identity) do
    case identity do
      {document, tree, node} ->
        is_reference(document) and is_reference(tree) and is_reference(node)

      _ ->
        false
    end
  end

  @doc false
  @spec valid_prepared_destination?(term()) :: boolean()
  def valid_prepared_destination?(destination) do
    case destination do
      nil -> true
      {:target, identity} -> valid_identity?(identity)
      {:top, {document, tree}} -> is_reference(document) and is_reference(tree)
      _ -> false
    end
  end

  @doc false
  @spec validate_boxes([map()]) :: :ok | {:error, {atom(), Diagnostics.diagnostic()}}
  def validate_boxes(boxes) do
    if Enum.all?(boxes, fn box ->
         case box do
           box when is_map(box) ->
             targets = Map.get(box, :html_targets, [])

             if is_list(targets) and targets != [],
               do:
                 HtmlValidator.reserve_render_resource(
                   :max_css_work,
                   length(targets),
                   :pagination
                 )

             origin_valid? =
               targets == [] or
                 (match?({:ok, _}, HtmlValidator.normalize_geometry_number(Map.get(box, :x))) and
                    match?({:ok, _}, HtmlValidator.normalize_geometry_number(Map.get(box, :y))))

             destination = Map.get(box, :link_destination)

             is_list(targets) and origin_valid? and
               Enum.all?(targets, fn target ->
                 case target do
                   %{identity: identity, dx: dx, dy: dy} ->
                     valid_identity?(identity) and
                       match?({:ok, _}, HtmlValidator.normalize_geometry_number(dx)) and
                       match?({:ok, _}, HtmlValidator.normalize_geometry_number(dy))

                   _ ->
                     false
                 end
               end) and valid_prepared_destination?(destination) and
               (destination == nil or Map.get(box, :type) in [:text, :image, :link_region])

           _ ->
             false
         end
       end) do
      :ok
    else
      Diagnostics.error(
        :pagination,
        :invalid_layout,
        "HTML targets require document/tree/node identities and numeric offsets; prepared links require a scoped target or document beginning",
        operation: :paginate,
        module: __MODULE__,
        source: "html_targets/link_destination"
      )
    end
  end

  @doc false
  @spec validate_boxes([map()], term()) :: :ok | {:error, {atom(), Diagnostics.diagnostic()}}
  def validate_boxes(boxes, scope) do
    with :ok <- validate_boxes(boxes) do
      if Enum.all?(boxes, fn box ->
           target_scopes =
             Enum.map(Map.get(box, :html_targets, []), fn %{identity: {document, tree, _node}} ->
               {document, tree}
             end)

           scopes =
             case Map.get(box, :link_destination) do
               {:target, {document, tree, _node}} -> [{document, tree} | target_scopes]
               {:top, document_scope} -> [document_scope | target_scopes]
               nil -> target_scopes
             end

           Enum.all?(scopes, &(&1 == scope))
         end) do
        Enum.reduce_while(boxes, :ok, fn box, :ok ->
          result =
            case box do
              %{type: :drawing_group, boxes: children, width: width, height: height} ->
                child_scope = Map.get(box, :link_scope, scope)

                with :ok <- WriterValidator.validate_drawing_group(box),
                     :ok <- validate_document_origin(box, child_scope, {width, height}),
                     :ok <- validate_boxes(children, child_scope),
                     :ok <-
                       validate_expected_targets(
                         Map.get(box, :link_expected_targets, []),
                         child_scope
                       ),
                     :ok <-
                       validate_document_targets(
                         Map.get(box, :link_document_targets, []),
                         child_scope,
                         {width, height}
                       ),
                     do: :ok

              %{type: :glyph_group, boxes: children} ->
                validate_boxes(children, scope)

              _ ->
                :ok
            end

          case result do
            :ok -> {:cont, :ok}
            error -> {:halt, error}
          end
        end)
      else
        Diagnostics.error(
          :pagination,
          :unsupported_html,
          "HTML fragment targets and links must retain the current document and tree scope; child destinations require scoped root placement and final composition geometry",
          operation: :paginate,
          module: __MODULE__,
          source: "link_scope"
        )
      end
    end
  end

  defp validate_document_origin(box, scope, size) do
    case Map.fetch(box, :link_scope) do
      :error ->
        :ok

      {:ok, {document, tree}} when is_reference(document) and is_reference(tree) ->
        case Map.get(box, :link_document_origin) do
          %{x: x, y: y} ->
            validate_document_targets(
              [
                %{
                  identity: {document, tree, make_ref()},
                  source: "document beginning",
                  x: x,
                  y: y
                }
              ],
              scope,
              size
            )

          _ ->
            Diagnostics.error(
              :pagination,
              :invalid_layout,
              "A scoped child document requires its local printed root origin",
              operation: :paginate,
              module: __MODULE__,
              source: "link_document_origin"
            )
        end

      _ ->
        Diagnostics.error(
          :pagination,
          :invalid_layout,
          "A child document requires reference document and tree identities",
          operation: :paginate,
          module: __MODULE__,
          source: "link_scope"
        )
    end
  end

  @doc false
  @spec required_targets([map()]) :: [map()]
  def required_targets(children) do
    Enum.flat_map(children, fn node ->
      own =
        case node do
          %{style: %{display: :none}} ->
            []

          %{style: %{html_target: identity, html_target_source: source}} when identity != nil ->
            [%{identity: identity, source: source}]

          _ ->
            []
        end

      own ++ required_targets(Map.get(node, :children, []))
    end)
  end

  @doc false
  @spec validate_expected_targets(term(), term()) ::
          :ok | {:error, {atom(), Diagnostics.diagnostic()}}
  def validate_expected_targets(targets, scope) do
    if is_list(targets) and
         Enum.all?(targets, fn target ->
           case target do
             %{identity: {document, tree, _node} = identity, source: source} ->
               valid_identity?(identity) and {document, tree} == scope and is_binary(source) and
                 String.valid?(source)

             _ ->
               false
           end
         end) do
      HtmlValidator.reserve_render_resource(:max_css_work, length(targets), :pagination)
      :ok
    else
      Diagnostics.error(
        :pagination,
        :invalid_layout,
        "Expected HTML targets require an identity in the current document/tree and an authored source identifier",
        operation: :paginate,
        module: __MODULE__,
        source: "link_expected_targets"
      )
    end
  end

  @doc false
  @spec validate_document_targets([map()], term(), {number(), number()}) ::
          :ok | {:error, {atom(), Diagnostics.diagnostic()}}
  def validate_document_targets(targets, scope, {width, height}) do
    with :ok <- validate_expected_targets(targets, scope) do
      if Enum.all?(targets, fn target ->
           case target do
             %{x: x, y: y} ->
               is_number(x) and is_number(y) and x >= 0 and x <= width and y >= 0 and y <= height

             _ ->
               false
           end
         end) do
        :ok
      else
        Diagnostics.error(
          :pagination,
          :invalid_layout,
          "Document-root HTML targets require coordinates in the final printed viewport",
          operation: :paginate,
          module: __MODULE__,
          source: "link_document_targets"
        )
      end
    end
  end

  @doc false
  @spec resolve_pages([map()], term(), [map()], [map()]) ::
          {:ok, [map()]} | {:error, {atom(), Diagnostics.diagnostic()}}
  def resolve_pages(pages, root_scope, expected_targets \\ [], document_targets \\ []) do
    if root_scope == nil and
         Enum.all?(pages, fn page ->
           Enum.all?(page.boxes, &(Map.get(&1, :type) not in [:drawing_group, :glyph_group]))
         end) do
      {:ok, pages}
    else
      document_positions = Map.new(document_targets, &{&1.identity, {:xyz, 1, &1.x, &1.y}})

      beginnings =
        if root_scope == nil,
          do: %{},
          else: %{root_scope => {:xyz, 1, 0, elem(hd(pages).size, 1)}}

      {positions, beginnings, expected, requested} =
        pages
        |> Enum.with_index(1)
        |> Enum.reduce({document_positions, beginnings, expected_targets, MapSet.new()}, fn {page,
                                                                                             number},
                                                                                            acc ->
          {width, height} = page.size
          collect_positions(page.boxes, number, {1, 1, 0, 0, {0, 0, width, height}, height}, acc)
        end)

      missing = Enum.find(expected, &(not Map.has_key?(positions, &1.identity)))

      unavailable =
        Enum.find(
          Map.merge(positions, Map.take(beginnings, MapSet.to_list(requested))),
          fn {_identity, position} ->
            elem(position, 0) == :unavailable
          end
        )

      cond do
        missing != nil ->
          Diagnostics.error(
            :pagination,
            :unsupported_html,
            "Selected visible HTML fragment target has no final placement for PDF destination serialization; this element requires target geometry transport through layout and pagination",
            operation: :paginate,
            module: __MODULE__,
            source: missing.source
          )

        unavailable != nil ->
          {identity, {:unavailable, page, x, y}} = unavailable

          source =
            Enum.find_value(expected, "document beginning", fn target ->
              if target.identity == identity, do: target.source
            end)

          Diagnostics.error(
            :pagination,
            :unsupported_html,
            "Selected HTML fragment target has no representable destination inside final PDF page #{page} at #{x}, #{y}; keep its target geometry inside the printed page and all ancestor clips, with a positive visible viewport",
            operation: :paginate,
            module: __MODULE__,
            source: source
          )

        true ->
          {:ok,
           Enum.map(pages, fn page ->
             %{page | boxes: resolve_boxes(page.boxes, positions, beginnings)}
           end)}
      end
    end
  end

  # Recursion follows paint groups after parent pagination. Each local transform
  # uses the same PDF precision and intersected clips as writer annotations.
  defp collect_positions(boxes, page, composition, acc) do
    Enum.reduce(boxes, acc, fn box, {positions, beginnings, expected, requested} ->
      {sx, sy, tx, ty, clip, _height} = composition

      requested =
        case Map.get(box, :link_destination) do
          {:top, scope} -> MapSet.put(requested, scope)
          _ -> requested
        end

      positions =
        Enum.reduce(Map.get(box, :html_targets, []), positions, fn target, positions ->
          position =
            target_position(
              box.x + target.dx,
              box.y + target.dy,
              page,
              {sx, sy, tx, ty},
              DrawingGeometry.intersect_clip(
                clip,
                DrawingGeometry.transform_box_clip(Map.get(box, :clip), {sx, sy, tx, ty})
              )
            )

          Map.put_new(positions, target.identity, position)
        end)

      case box do
        %{type: :drawing_group, boxes: children} ->
          composition = DrawingGeometry.compose_group(box, composition)
          {sx, sy, tx, ty, clip, _height} = composition
          transform = {sx, sy, tx, ty}

          positions =
            Enum.reduce(Map.get(box, :link_document_targets, []), positions, fn target,
                                                                                positions ->
              Map.put_new(
                positions,
                target.identity,
                target_position(target.x, target.y, page, transform, clip)
              )
            end)

          beginnings =
            case Map.get(box, :link_scope) do
              nil ->
                beginnings

              scope ->
                origin = box.link_document_origin

                Map.put_new(
                  beginnings,
                  scope,
                  target_position(origin.x, origin.y, page, transform, clip)
                )
            end

          collect_positions(
            children,
            page,
            composition,
            {positions, beginnings, expected ++ Map.get(box, :link_expected_targets, []),
             requested}
          )

        %{type: :glyph_group, boxes: children} ->
          collect_positions(
            children,
            page,
            composition,
            {positions, beginnings, expected, requested}
          )

        _ ->
          {positions, beginnings, expected, requested}
      end
    end)
  end

  defp target_position(x, y, page, transform, {left, bottom, right, top}) do
    {x, y} = DrawingGeometry.transform_point(x, y, transform)
    HtmlValidator.check_render_resource(:max_pdf_numeric_magnitude, abs(x), :pagination)
    HtmlValidator.check_render_resource(:max_pdf_numeric_magnitude, abs(y), :pagination)

    if right > left and top > bottom and x >= left and x <= right and y >= bottom and y <= top,
      do: {:xyz, page, x, y},
      else: {:unavailable, page, x, y}
  end

  defp resolve_boxes(boxes, positions, beginnings) do
    Enum.map(boxes, fn box ->
      destination =
        case Map.get(box, :link_destination) do
          {:target, identity} -> Map.get(positions, identity)
          {:top, scope} -> Map.get(beginnings, scope)
          nil -> nil
        end

      box =
        Map.drop(box, [
          :html_targets,
          :link_scope,
          :link_expected_targets,
          :link_document_targets,
          :link_document_origin
        ])

      box =
        if destination == nil,
          do: Map.delete(box, :link_destination),
          else: Map.put(box, :link_destination, destination)

      case box do
        %{type: type, boxes: children} when type in [:drawing_group, :glyph_group] ->
          %{
            box
            | boxes:
                resolve_boxes(children, positions, beginnings)
                |> Enum.reject(&(Map.get(&1, :type) == :flow_marker))
          }

        _ ->
          box
      end
    end)
  end

  @doc false
  @spec validate_destinations([map()]) :: :ok | {:error, {atom(), Diagnostics.diagnostic()}}
  def validate_destinations(pages) do
    sizes = pages |> Enum.map(& &1.size) |> List.to_tuple()

    if Enum.all?(pages, &valid_destination_boxes?(&1.boxes, sizes)) do
      :ok
    else
      Diagnostics.error(
        :pdf,
        :invalid_pdf_input,
        "link_destination requires {:xyz, page_number, x, y} on a text, image or image-map region link, with an existing page and coordinates inside its bounds; prepared HTML targets must be resolved after pagination",
        operation: :write_pdf,
        module: __MODULE__,
        source: "link_destination"
      )
    end
  end

  defp valid_destination_boxes?(boxes, sizes) do
    Enum.all?(boxes, fn box ->
      case Map.get(box, :link_destination) do
        nil ->
          case box do
            %{type: type, boxes: children} when type in [:drawing_group, :glyph_group] ->
              valid_destination_boxes?(children, sizes)

            _ ->
              true
          end

        {:xyz, number, x, y}
        when is_integer(number) and number > 0 and number <= tuple_size(sizes) ->
          {width, height} = elem(sizes, number - 1)

          box.type in [:text, :image, :link_region] and Map.get(box, :link_url) == nil and
            is_number(x) and is_number(y) and x >= 0 and x <= width and y >= 0 and y <= height

        _ ->
          false
      end
    end)
  end
end
