defmodule NativeElixirPdfUtilities.Validators.SvgSceneValidator do
  @moduledoc false
  alias NativeElixirPdfUtilities.{Diagnostics, Limits}

  alias NativeElixirPdfUtilities.Validators.{
    HtmlValidator,
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

  @doc "Selects the single actual SVG root issued by the document parser."
  @spec extraction_root(term()) :: {:ok, map()} | {:error, {atom(), map()}}
  def extraction_root(document) do
    case document do
      %{children: children} when is_list(children) ->
        case Enum.filter(children, &(Map.get(&1, :type) == :element)) do
          [%{tag: "svg", namespace: :svg} = root] -> {:ok, root}
          _ -> invalid("SVG extraction requires exactly one complete SVG document", "SVG")
        end

      _ ->
        invalid("SVG extraction requires a parsed document", "SVG")
    end
  end

  @doc "Preserves canonical XML attribute spelling before HTML-oriented style projection."
  @spec align_xml_source(map(), map()) :: {:ok, map()} | {:error, {atom(), map()}}
  def align_xml_source(document, summary) do
    sources = Enum.sort_by(summary.source_nodes, & &1.index)
    {children, remaining} = align_xml_children(document.children, sources)

    if remaining == [],
      do: {:ok, %{document | children: children}},
      else: invalid("SVG XML source and parsed extraction tree have different structures", "SVG")
  catch
    :svg_xml_structure ->
      invalid("SVG XML source and parsed extraction tree have different structures", "SVG")
  end

  defp align_xml_children(children, sources) do
    Enum.map_reduce(children, sources, fn child, sources ->
      case {child, sources} do
        {%{type: :element}, [source | sources]} ->
          qualified =
            Map.filter(child.attributes, fn {name, _} ->
              name == "xmlns" or String.contains?(name, ":")
            end)

          attributes = Map.merge(qualified, source.attributes)
          {children, sources} = align_xml_children(child.children, sources)

          spellings =
            Map.new(source.attributes, fn {name, _} -> {String.downcase(name, :ascii), name} end)

          order =
            Enum.map(Map.get(child, :attribute_order, Map.keys(child.attributes)), fn name ->
              if Map.has_key?(qualified, name),
                do: name,
                else: Map.get(spellings, String.downcase(name, :ascii), name)
            end)

          child = %{child | tag: source.tag, attributes: attributes, children: children}
          {Map.put(child, :attribute_order, order), sources}

        {%{type: :element}, []} ->
          throw(:svg_xml_structure)

        _ ->
          {child, sources}
      end
    end)
  end

  @doc false
  @spec validate_node(map()) :: :ok | {:error, {atom(), map()}}
  def validate_node(node) do
    with :ok <- validate_converter_node(node),
         :ok <- validate_path_length(node),
         :ok <- validate_lengths(node),
         :ok <- validate_points(node),
         :ok <- validate_path_data(node),
         do: :ok
  end

  @doc "Validates source structure and image references without interpreting converter geometry."
  @spec validate_converter_node(map()) :: :ok | {:error, {atom(), map()}}
  def validate_converter_node(node) do
    with :ok <-
           NativeElixirPdfUtilities.Validators.ForeignContentValidator.validate_static_input(
             :svg,
             node.tag,
             node.attributes
           ),
         :ok <- validate_structure(node) do
      if node.tag in (@paint_tags ++
                        NativeElixirPdfUtilities.Validators.SvgFilterValidator.tags()),
         do: NativeElixirPdfUtilities.Validators.SvgImageValidator.validate_node(node),
         else:
           unsupported(
             "SVG #{node.tag} is not admitted by the supported source policy; unsupported elements cannot be silently omitted",
             node.tag
           )
    end
  end

  @doc false
  @spec validate_structure(map()) :: :ok | {:error, {atom(), map()}}
  def validate_structure(node) do
    cond do
      node.tag == "textPath" and
          not Enum.any?(~w(href xlink:href path), &Map.has_key?(node.attributes, &1)) ->
        invalid("SVG textPath requires a same-scene #id href or inline path", "textPath href")

      node.tag in ~w(rect circle ellipse line polyline polygon path) and
          Enum.any?(
            node.children,
            &(Map.get(&1, :type) == :element and Map.get(&1, :tag) not in ~w(title desc))
          ) ->
        invalid(
          "SVG graphics elements may contain descriptive metadata, not nested artwork",
          node.tag
        )

      true ->
        :ok
    end
  end

  @doc "Admits an existing parsed SVG source tree with common root policy and nonroot syntax validation."
  @spec validate_source_tree(map()) :: :ok | {:error, {atom(), map()}}
  def validate_source_tree(root) do
    Enum.reduce_while(tree_nodes(root), :ok, fn {node, ancestors}, :ok ->
      result =
        cond do
          Map.get(node, :namespace) != :svg ->
            :ok

          node.tag in ~w(tspan textPath) and "text" not in ancestors ->
            invalid("SVG #{node.tag} requires an enclosing text element", node.tag)

          node == root ->
            validate_converter_node(node)

          true ->
            validate_node(node)
        end

      case result do
        :ok -> {:cont, :ok}
        error -> {:halt, error}
      end
    end)
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
                validate_source_tree(node)

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

  # This parses only the path attribute's syntax. Command arities and the two
  # one-byte arc flags are fixed SVG grammar, not configurable work limits.
  defp validate_path_data(node) do
    if node.tag == "path" do
      case Map.get(node.attributes, "d", "") do
        value when is_binary(value) ->
          HtmlValidator.with_render_budget(fn ->
            HtmlValidator.check_render_resource(:max_svg_path_bytes, byte_size(value), :layout)
            source = HtmlValidator.trim_ascii_whitespace(value)

            result =
              if String.downcase(source, :ascii) == "none",
                do: :ok,
                else: parse_path_commands(source, nil, true)

            if result == :ok,
              do: :ok,
              else:
                invalid(
                  "SVG path d requires complete commands beginning with M or m, bounded finite numbers and arc flags 0 or 1",
                  "path d"
                )
          end)

        _ ->
          invalid("SVG path d requires a path data string", "path d")
      end
    else
      :ok
    end
  end

  defp parse_path_commands(source, repeated_command, first?) do
    source = skip_path_whitespace(source)

    case source do
      "" ->
        :ok

      <<command, rest::binary>> when command in ~c"MmZzLlHhVvCcSsQqTtAa" ->
        cond do
          first? and command not in ~c"Mm" ->
            :error

          command in ~c"Zz" ->
            parse_path_commands(rest, nil, false)

          true ->
            with {:ok, rest} <- parse_path_arguments(rest, command),
                 do: parse_path_commands(rest, command, false)
        end

      _ ->
        if is_nil(repeated_command) do
          :error
        else
          with {:ok, rest} <- path_argument_separator(source, :optional),
               {:ok, rest} <- parse_path_arguments(rest, repeated_command),
               do: parse_path_commands(rest, repeated_command, false)
        end
    end
  end

  defp parse_path_arguments(source, command) do
    arguments =
      case command do
        value when value in ~c"MmLlTt" -> [:number, :number]
        value when value in ~c"HhVv" -> [:number]
        value when value in ~c"Cc" -> [:number, :number, :number, :number, :number, :number]
        value when value in ~c"SsQq" -> [:number, :number, :number, :number]
        value when value in ~c"Aa" -> [:number, :number, :number, :flag, :flag, :number, :number]
      end

    arguments
    |> Enum.with_index()
    |> Enum.reduce_while({:ok, source}, fn {kind, index}, {:ok, remaining} ->
      separator =
        cond do
          index == 0 -> :command
          index == 3 and command in ~c"Aa" -> :required
          true -> :optional
        end

      result =
        with {:ok, remaining} <- path_argument_separator(remaining, separator),
             do: consume_path_argument(remaining, kind)

      case result do
        {:ok, remaining} -> {:cont, {:ok, remaining}}
        :error -> {:halt, :error}
      end
    end)
  end

  defp path_argument_separator(source, requirement) do
    trimmed = skip_path_whitespace(source)

    case {requirement, trimmed} do
      {:command, <<",", _rest::binary>>} -> :error
      {:command, _} -> {:ok, trimmed}
      {_, <<",", rest::binary>>} -> {:ok, skip_path_whitespace(rest)}
      {:required, _} when byte_size(source) == byte_size(trimmed) -> :error
      _ -> {:ok, trimmed}
    end
  end

  defp skip_path_whitespace(source) do
    case source do
      <<character, rest::binary>> when character in [9, 10, 12, 13, 32] ->
        skip_path_whitespace(rest)

      _ ->
        source
    end
  end

  defp consume_path_argument(source, kind) do
    case {kind, source} do
      {:flag, <<flag, rest::binary>>} when flag in ~c"01" ->
        {:ok, rest}

      {:number, source} ->
        case Regex.run(~r/\A[+-]?(?:[0-9]+(?:\.[0-9]*)?|\.[0-9]+)(?:[eE][+-]?[0-9]+)?/, source,
               capture: :first
             ) do
          [token] ->
            # Retain the original SVG 1.1 trailing-dot spelling. Only its lexical
            # token is normalized for the existing canonical numeric parser.
            normalized = Regex.replace(~r/\.(?=[eE]|$)/, token, ".0")

            case HtmlValidator.parse_css_number(normalized) do
              {_number, ""} ->
                {:ok, binary_part(source, byte_size(token), byte_size(source) - byte_size(token))}

              _ ->
                :error
            end

          _ ->
            :error
        end

      _ ->
        :error
    end
  end

  defp validate_path_length(node) do
    case Map.fetch(node.attributes, "pathLength") do
      :error ->
        :ok

      {:ok, value} ->
        case HtmlValidator.parse_css_number(value) do
          {length, ""} when length >= 0 ->
            if node.tag in ~w(path rect circle ellipse line polyline polygon),
              do: :ok,
              else: invalid("SVG pathLength belongs to geometry", node.tag <> " pathLength")

          _ ->
            invalid(
              "SVG pathLength requires a finite nonnegative number",
              node.tag <> " pathLength"
            )
        end
    end
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

  @doc false
  @spec validate_paint_style(map()) :: :ok | {:error, {atom(), map()}}
  def validate_paint_style(style) do
    if Map.get(style, :display) in [:inline, :inline_block, :block, :none],
      do: :ok,
      else: unsupported("SVG display requires replaced inline or block formatting", "svg display")
  end

  @doc false
  @spec validate_prepared(term()) :: :ok | {:error, {atom(), map()}}
  def validate_prepared(scene) do
    case scene do
      %{source: source, attributes: attrs, style: style, budget: budget}
      when is_binary(source) and is_map(attrs) and is_map(style) ->
        with {:ok, _} <- SvgViewportValidator.prepare(attrs, style, %{width: 0.0, height: nil}),
             :ok <- validate_paint_style(style),
             {:ok, summary} <-
               NativeElixirPdfUtilities.Validators.SvgValidator.validate_processing(source, %{
                 mode: :inline,
                 resources: Map.get(scene, :resources, %{})
               }),
             :ok <-
               NativeElixirPdfUtilities.Validators.SvgConverterValidator.validate_capabilities(
                 summary
               ) do
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
      :ok
    else
      with :ok <- NativeElixirPdfUtilities.Validators.ResourceValidator.validate_context(issued),
           :ok <- NativeElixirPdfUtilities.Validators.ResourceValidator.validate_context(context) do
        if issued.document_id == context.document_id and
             issued.tree_scope_id == context.tree_scope_id and
             issued.image_budget == scene.budget,
           do: :ok,
           else:
             invalid(
               "SVG paint requires its issued source document scope and shared image budget",
               "SVG"
             )
      end
    end
  end

  @doc false
  @spec reserve_paint_hit(map(), number(), number() | nil, term()) ::
          :ok | {:error, {atom(), map()}}
  def reserve_paint_hit(scene, width, height, result) do
    with {:ok, viewport} <-
           NativeElixirPdfUtilities.HtmlToPdf.SvgScene.measure(scene, width, height) do
      if viewport.paint? and Map.get(scene.style, :display) != :none and match?({:ok, _}, result) do
        scale = Limits.get(:svg_raster_scale)

        NativeElixirPdfUtilities.Validators.SvgValidator.validate_raster_dimensions(
          max(ceil(viewport.width / 0.75 * scale), 1),
          max(ceil(viewport.height / 0.75 * scale), 1),
          scene.budget
        )
      else
        :ok
      end
    end
  end

  @doc false
  @spec summary_tree(map()) :: map()
  def summary_tree(summary) do
    nodes =
      if summary.geometry == [] do
        # Every processing mode already retains these authentic parsed nodes.
        # Semantic resource checks need their source tree even when image mode
        # has no inline geometry transport. Reuse the original hierarchy.
        Map.new(summary.source_nodes, fn node ->
          {node.index, node |> Map.put(:type, :element) |> Map.put(:svg_index, node.index)}
        end)
      else
        Map.new(summary.geometry, &{&1.svg_index, &1})
      end

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
