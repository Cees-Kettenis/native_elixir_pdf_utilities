defmodule NativeElixirPdfUtilities.Validators.SvgSceneValidator do
  @moduledoc false
  alias NativeElixirPdfUtilities.{Diagnostics, Limits}
  alias NativeElixirPdfUtilities.HtmlToPdf.SvgBackend

  alias NativeElixirPdfUtilities.Validators.{
    HtmlValidator,
    SvgBackendValidator,
    SvgStyleValidator,
    SvgViewportValidator
  }

  @paint_tags ~w(svg g defs symbol use pattern marker mask foreignObject linearGradient radialGradient stop clipPath image rect circle ellipse line polyline polygon path text tspan textPath a title desc style)
  @length_attributes %{
    "rect" => ~w(x y width height rx ry),
    "circle" => ~w(cx cy r),
    "ellipse" => ~w(cx cy rx ry),
    "line" => ~w(x1 y1 x2 y2),
    "svg" => ~w(x y width height),
    "use" => ~w(x y width height),
    "image" => ~w(x y width height)
  }

  @doc false
  @spec validate_node(map()) :: :ok | {:error, {atom(), map()}}
  def validate_node(node) do
    with :ok <-
           NativeElixirPdfUtilities.Validators.ForeignContentValidator.validate_static_input(
             :svg,
             node.tag,
             node.attributes
           ),
         :ok <- validate_structure(node) do
      validate_geometry_node(node)
    end
  end

  defp validate_geometry_node(node) do
    cond do
      Enum.any?(node.attributes, fn {key, value} ->
        key in ~w(vector-effect transform-origin pathLength) and
            value != "none"
      end) ->
        unsupported(
          "SVG effect, vector-effect, transform-origin or pathLength requires its complete geometry/effect implementation",
          node.tag
        )

      node.tag not in (@paint_tags ++
                         NativeElixirPdfUtilities.Validators.SvgFilterValidator.tags()) ->
        unsupported(
          "SVG #{node.tag} requires its complete scene, logical content or effect implementation; supported geometry cannot omit it",
          node.tag
        )

      true ->
        with :ok <- validate_lengths(node),
             :ok <- validate_points(node),
             :ok <- validate_path(node),
             :ok <- NativeElixirPdfUtilities.Validators.SvgImageValidator.validate_node(node) do
          :ok
        end
    end
  end

  @doc false
  @spec validate_structure(map()) :: :ok | {:error, {atom(), map()}}
  def validate_structure(node) do
    if node.tag in ~w(rect circle ellipse line polyline polygon path) and
         Enum.any?(
           node.children,
           &(Map.get(&1, :type) == :element and Map.get(&1, :tag) not in ~w(title desc))
         ) do
      invalid(
        "SVG graphics elements may contain descriptive metadata, not nested artwork",
        node.tag
      )
    else
      :ok
    end
  end

  @doc false
  @spec validate_document_svg([map()]) :: :ok | {:error, {atom(), map()}}
  def validate_document_svg(nodes) do
    Enum.reduce_while(nodes, :ok, fn node, :ok ->
      result =
        case node do
          %{type: :element, namespace: :svg} ->
            source = NativeElixirPdfUtilities.HtmlToPdf.SvgScene.authored_source(node)

            case NativeElixirPdfUtilities.Validators.SvgValidator.validate_processing(source, %{
                   mode: :inline,
                   resources:
                     NativeElixirPdfUtilities.Validators.SvgImageValidator.pending_references(
                       node
                     )
                 }) do
              {:ok, _} ->
                NativeElixirPdfUtilities.Validators.SvgTextValidator.validate_original_fonts(node)

              error ->
                error
            end

          %{type: :element, children: children} ->
            validate_document_svg(children)

          _ ->
            :ok
        end

      case result do
        :ok -> {:cont, :ok}
        error -> {:halt, error}
      end
    end)
  end

  defp validate_lengths(node) do
    Enum.reduce_while(Map.get(@length_attributes, node.tag, []), :ok, fn property, :ok ->
      value = Map.get(node.attributes, property)
      signed? = property in ~w(x y cx cy x1 y1 x2 y2)
      result = if is_nil(value), do: {:ok, nil}, else: SvgStyleValidator.length(value, signed?)

      result =
        if value == "auto" and property in ~w(width height rx ry), do: {:ok, :auto}, else: result

      case result do
        {:ok, _} ->
          {:cont, :ok}

        _ ->
          {:halt,
           invalid(
             "SVG #{property} requires a valid #{if signed?, do: "signed", else: "nonnegative"} geometry length",
             node.tag <> " " <> property
           )}
      end
    end)
  end

  defp validate_points(node) do
    if node.tag in ~w(polyline polygon) do
      source = String.trim(Map.get(node.attributes, "points", ""))
      number = "[+-]?(?:[0-9]+(?:\\.[0-9]*)?|\\.[0-9]+)(?:[eE][+-]?[0-9]+)?"
      pattern = Regex.compile!(number)
      tokens = Regex.scan(pattern, source) |> List.flatten()
      residue = Regex.replace(pattern, source, "")

      valid? =
        Regex.match?(~r/\A[ \t\r\n,]*\z/, residue) and rem(length(tokens), 2) == 0 and
          not Regex.match?(~r/\A,|,\z|,\s*,/, source) and
          Enum.all?(tokens, fn token ->
            token = Regex.replace(~r/\.(?=[eE]|$)/, token, ".0")
            match?({_, ""}, HtmlValidator.parse_css_number(token))
          end)

      if valid?,
        do: :ok,
        else:
          invalid("SVG points require complete finite coordinate pairs", node.tag <> " points")
    else
      :ok
    end
  end

  defp validate_path(node) do
    if node.tag == "path" do
      with :ok <- SvgBackendValidator.native_load_result(SvgBackend.load_native()) do
        case NativeElixirPdfUtilities.HtmlToPdf.SvgBackend.NativeBytes.validate_path(
               Map.get(node.attributes, "d", ""),
               Limits.get(:max_svg_path_segments),
               Limits.get(:max_css_numeric_magnitude) * 1.0
             ) do
          {:ok, count} ->
            HtmlValidator.reserve_render_resource(:max_svg_path_segments, count, :style)

          {:error, "max_svg_path_segments"} ->
            Diagnostics.error(
              :limits,
              :resource_limit_exceeded,
              "SVG path exceeds max_svg_path_segments",
              source: "path d",
              module: __MODULE__,
              operation: :prepare
            )

          {:error, message} ->
            invalid("SVG path data is invalid: " <> message, "path d")
        end
      end
    else
      :ok
    end
  end

  @doc false
  @spec validate_paint_style(map()) :: :ok | {:error, {atom(), map()}}
  def validate_paint_style(style) do
    cond do
      Map.get(style, :box_sizing, :content_box) == :border_box or
          Map.get(style, :border_radius, 0.0) > 0 ->
        unsupported(
          "SVG border-box sizing and rounded outer clips require their complete replaced-box composition",
          "svg box"
        )

      Map.get(style, :position, :static) == :absolute or
          Map.get(style, :object_fit, :fill) != :fill ->
        unsupported(
          "SVG absolute positioning and object fitting require canonical replaced viewport integration",
          "svg placement"
        )

      Map.get(style, :svg_opacity, 1.0) != 1.0 and
          (Map.get(style, :background_color) != nil or
             Enum.any?(Map.get(style, :border_styles, %{}), fn {edge, border_style} ->
               border_style not in [:none, :hidden] and
                   Map.get(Map.get(style, :border_widths, %{}), edge, 0.0) > 0
             end)) ->
        unsupported(
          "SVG opacity over an outer background/border requires unified scene effects",
          "svg opacity"
        )

      Map.get(style, :overflow, :hidden) == :visible ->
        unsupported(
          "SVG visible overflow requires expanded ink composition; viewport rasterization cannot omit artwork outside the box",
          "svg overflow"
        )

      Map.get(style, :svg_transform, {1.0, 0.0, 0.0, 1.0, 0.0, 0.0}) !=
          {1.0, 0.0, 0.0, 1.0, 0.0, 0.0} ->
        unsupported(
          "outer SVG transforms require affine ink and annotation composition; inner graphics transforms are retained",
          "svg transform"
        )

      Map.get(style, :display) not in [:inline, :inline_block, :block, :none] ->
        unsupported("SVG display requires replaced inline or block formatting", "svg display")

      true ->
        :ok
    end
  end

  @doc false
  @spec validate_prepared(term()) :: :ok | {:error, {atom(), map()}}
  def validate_prepared(scene) do
    case scene do
      %{source: source, attributes: attrs, style: style, budget: budget}
      when is_binary(source) and is_map(attrs) and is_map(style) ->
        with {:ok, _} <- SvgViewportValidator.prepare(attrs, style, %{width: 0.0, height: nil}),
             :ok <- validate_paint_style(style),
             {:ok, _} <-
               NativeElixirPdfUtilities.Validators.SvgValidator.validate_processing(source, %{
                 mode: :inline,
                 resources: Map.get(scene, :resources, %{})
               }) do
          valid_budget? =
            if is_nil(budget) do
              true
            else
              try do
                case :atomics.info(budget) do
                  %{size: 3, min: 0} -> true
                  _ -> false
                end
              rescue
                ArgumentError -> false
              end
            end

          if valid_budget?,
            do: :ok,
            else: invalid("prepared SVG requires an issued image budget", "svg scene")
        end

      _ ->
        invalid("prepared SVG requires complete source, geometry and style", "svg scene")
    end
  end

  @doc false
  @spec validate_paint_scope(map(), term()) :: :ok | {:error, {atom(), map()}}
  def validate_paint_scope(scene, context) do
    issued = Map.get(scene, :resource_context)

    if is_nil(issued) and is_nil(context) do
      with {:ok, _ids} <-
             NativeElixirPdfUtilities.Validators.SvgForeignObjectValidator.validate_slots(
               Map.get(scene, :foreign_slots, %{})
             ),
           do: :ok
    else
      with :ok <- NativeElixirPdfUtilities.Validators.ResourceValidator.validate_context(issued),
           :ok <- NativeElixirPdfUtilities.Validators.ResourceValidator.validate_context(context) do
        if issued.document_id == context.document_id and
             issued.tree_scope_id == context.tree_scope_id and issued.image_budget == scene.budget do
          with {:ok, _ids} <-
                 NativeElixirPdfUtilities.Validators.SvgForeignObjectValidator.validate_slots(
                   Map.get(scene, :foreign_slots, %{})
                 ) do
            Enum.reduce_while(Map.get(scene, :foreign_slots, %{}), :ok, fn {_id, slot}, :ok ->
              if slot.context.scope == context.scope and
                   slot.context.authorization_root == context.authorization_root and
                   slot.context.image_budget == scene.budget do
                {:cont, :ok}
              else
                {:halt,
                 invalid(
                   "SVG HTML slot requires its scene permission scope and shared image budget",
                   "foreignObject"
                 )}
              end
            end)
          end
        else
          invalid(
            "SVG paint requires its issued source document scope and shared image budget",
            "SVG"
          )
        end
      end
    end
  end

  defp reserve_foreign_paint_hit(painted) do
    Enum.reduce_while(painted.html_placements, :ok, fn placement, :ok ->
      HtmlValidator.reserve_render_resource(:max_layout_boxes, 1, :layout)

      with {:ok, group} <-
             NativeElixirPdfUtilities.Validators.SvgForeignObjectValidator.layout_slot(placement),
           {:ok, _, _, _} <-
             NativeElixirPdfUtilities.HtmlToPdf.PdfWriter.appearance_objects(
               [%{size: {placement.width, placement.height}, boxes: [group]}],
               1
             ),
           {:ok, _links} <-
             NativeElixirPdfUtilities.Validators.SvgFormSlotValidator.prepare_links(
               [Map.put(placement, :group, group)],
               painted.viewport.width,
               painted.viewport.height
             ) do
        {:cont, :ok}
      else
        error -> {:halt, error}
      end
    end)
  end

  @doc false
  @spec reserve_paint_hit(map(), number(), number() | nil, term()) ::
          :ok | {:error, {atom(), map()}}
  def reserve_paint_hit(scene, width, height, result) do
    with {:ok, viewport} <-
           NativeElixirPdfUtilities.HtmlToPdf.SvgScene.measure(scene, width, height) do
      if viewport.paint? and Map.get(scene.style, :display) != :none and match?({:ok, _}, result) do
        scale = Limits.get(:svg_raster_scale)

        dimensions = [
          width: max(ceil(viewport.width / 0.75 * scale), 1),
          height: max(ceil(viewport.height / 0.75 * scale), 1)
        ]

        source =
          String.replace(
            scene.source,
            "<svg ",
            "<svg width=\"#{viewport.width / 0.75}\" height=\"#{viewport.height / 0.75}\" ",
            global: false
          )

        context = %{
          mode: :inline,
          fonts: scene.selected_fonts,
          resources: Map.get(scene, :resources, %{})
        }

        if map_size(Map.get(scene, :foreign_slots, %{})) > 0 do
          with :ok <-
                 validate_paint_scope(
                   scene,
                   NativeElixirPdfUtilities.HtmlToPdf.ResourceContext.current()
                 ),
               {:ok, _} <-
                 SvgBackendValidator.prepare_vector(source, dimensions, context, scene.budget),
               {:ok, painted} <- result,
               :ok <- reserve_foreign_paint_hit(painted),
               {:ok, _} <-
                 NativeElixirPdfUtilities.Validators.SvgFormValidator.prepare_source(
                   painted.image.data
                 ) do
            :ok
          end
        else
          with {:ok, _} <- SvgBackendValidator.prepare(source, dimensions, context, scene.budget) do
            case result do
              {:ok, %{image: nil}} ->
                :ok

              _ ->
                with {:ok, _} <-
                       SvgBackendValidator.prepare_vector(
                         source,
                         dimensions,
                         context,
                         scene.budget
                       ),
                     do: :ok
            end
          end
        end
      else
        :ok
      end
    end
  end

  @doc false
  @spec summary_tree(map()) :: map()
  def summary_tree(summary) do
    nodes = Map.new(summary.geometry, &{&1.svg_index, &1})
    rebuild(1, nodes, summary.graph)
  end

  @doc false
  @spec tree_nodes(map()) :: [{map(), [String.t()]}]
  def tree_nodes(root), do: flatten(root, [])

  defp rebuild(index, nodes, graph) do
    node = Map.fetch!(nodes, index)

    %{
      node
      | children: Enum.map(Enum.reverse(Map.fetch!(graph, index)), &rebuild(&1, nodes, graph))
    }
  end

  defp flatten(node, ancestors) do
    [
      {node, ancestors}
      | Enum.flat_map(node.children, fn child ->
          if node.tag != "foreignObject" and Map.get(child, :type) == :element,
            do: flatten(child, [node.tag | ancestors]),
            else: []
        end)
    ]
  end

  defp unsupported(message, source) do
    Diagnostics.error(:layout, :unsupported_html, message,
      source: source,
      module: __MODULE__,
      operation: :prepare
    )
  end

  defp invalid(message, source) do
    Diagnostics.error(:layout, :invalid_document, message,
      source: source,
      module: __MODULE__,
      operation: :prepare
    )
  end
end
