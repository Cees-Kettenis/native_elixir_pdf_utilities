defmodule NativeElixirPdfUtilities.Validators.SvgPaintValidator do
  @moduledoc false
  alias NativeElixirPdfUtilities.Diagnostics

  alias NativeElixirPdfUtilities.Validators.{
    HtmlValidator,
    SvgSceneValidator,
    SvgStyleValidator,
    SvgTransformValidator
  }

  @gradients ~w(linearGradient radialGradient)

  @doc false
  @spec validate_summary(map()) :: :ok | {:error, {atom(), map()}}
  def validate_summary(summary) do
    if summary.context.mode == :inline,
      do: validate_tree(SvgSceneValidator.summary_tree(summary)),
      else: :ok
  end

  @doc false
  @spec validate_tree(map()) :: :ok | {:error, {atom(), map()}}
  def validate_tree(root) do
    nodes = SvgSceneValidator.tree_nodes(root)
    ids = Map.new(nodes, fn {node, _} -> {Map.get(node.attributes, "id"), node.tag} end)

    Enum.reduce_while(nodes, :ok, fn {node, ancestors}, :ok ->
      result =
        cond do
          (case Map.get(node, :style) do
             nil -> Map.get(node.attributes, "color-interpolation", "sRGB")
             style -> Map.get(style, :svg_color_interpolation, "sRGB")
           end) not in ~w(sRGB auto) ->
            unsupported("SVG gradient color interpolation currently requires sRGB", node.tag)

          not Map.has_key?(node, :style) and
              ((node.tag in (@gradients ++ ["stop"]) and
                  Map.get(node.attributes, "style", "") != "") or
                 Regex.match?(
                   ~r/(?:^|;)\s*(?:(?:fill|stroke)\s*:[^;]*url|color-interpolation\s*:)/i,
                   Map.get(node.attributes, "style", "")
                 )) ->
            unsupported("SVG paint-server CSS requires host cascade preparation", node.tag)

          node.tag == "stop" and
              Enum.any?(
                node.children,
                &(Map.get(&1, :type) == :element and &1.tag not in ~w(title desc))
              ) ->
            invalid("SVG stops accept descriptive metadata, not nested artwork", "stop")

          node.tag in @gradients ->
            validate_gradient(node)

          node.tag == "stop" and List.first(ancestors) not in @gradients ->
            invalid("SVG gradient stops require a gradient parent", "stop")

          true ->
            Enum.reduce_while(~w(fill stroke), :ok, fn property, :ok ->
              paint =
                case Map.get(node, :style) do
                  nil ->
                    if Map.get(node.attributes, property) == "inherit" do
                      :inherit
                    else
                      SvgStyleValidator.parse(
                        property,
                        Map.get(
                          node.attributes,
                          property,
                          if(property == "fill", do: "black", else: "none")
                        )
                      )
                    end

                  style ->
                    {:ok,
                     Map.get(style, if(property == "fill", do: :svg_fill, else: :svg_stroke))}
                end

              case paint do
                :inherit ->
                  {:cont, :ok}

                {:ok, {:paint_server, id}} ->
                  if Map.get(ids, id) in @gradients,
                    do: {:cont, :ok},
                    else:
                      {:halt,
                       invalid(
                         "SVG #{property} target ##{id} must identify a same-scene linear or radial gradient",
                         node.tag
                       )}

                {:ok, _} ->
                  {:cont, :ok}

                _ ->
                  {:halt,
                   invalid(
                     "SVG paint requires a supported color or local gradient reference",
                     node.tag
                   )}
              end
            end)
        end

      case result do
        :ok -> {:cont, :ok}
        error -> {:halt, error}
      end
    end)
  end

  defp validate_gradient(node) do
    attrs = node.attributes

    children =
      Enum.filter(
        node.children,
        &(Map.get(&1, :type) == :element and &1.tag not in ~w(title desc))
      )

    geometry = if node.tag == "linearGradient", do: ~w(x1 y1 x2 y2), else: ~w(cx cy r fx fy)

    values =
      Enum.map(geometry, fn property ->
        case Map.get(attrs, property) do
          nil -> {:ok, nil}
          value -> SvgStyleValidator.length(value, property != "r")
        end
      end)

    stops =
      Enum.map(children, fn child ->
        case Map.get(child, :style) do
          nil ->
            {SvgStyleValidator.parse("offset", Map.get(child.attributes, "offset", "0")),
             SvgStyleValidator.parse(
               "stop-color",
               Map.get(child.attributes, "stop-color", "black")
             ),
             SvgStyleValidator.parse(
               "stop-opacity",
               Map.get(child.attributes, "stop-opacity", "1")
             )}

          style ->
            {SvgStyleValidator.parse("offset", Map.get(child.attributes, "offset", "0")),
             {:ok,
              if(style.svg_stop_color == :current_color,
                do: {:color, style.color},
                else: style.svg_stop_color
              )}, {:ok, style.svg_stop_opacity}}
        end
      end)

    valid_stops? =
      Enum.all?(stops, fn stop ->
        case stop do
          {{:ok, _}, {:ok, {:color, {_, _, _}}}, {:ok, _}} -> true
          _ -> false
        end
      end)

    cond do
      Map.has_key?(attrs, "href") or Map.has_key?(attrs, "xlink:href") ->
        unsupported(
          "gradient inheritance references require their complete paint-server cascade",
          node.tag
        )

      Map.get(attrs, "spreadMethod", "pad") != "pad" ->
        unsupported(
          "SVG gradients currently require pad spread; repeating and reflected paint remain pending",
          node.tag
        )

      Map.get(attrs, "gradientUnits", "objectBoundingBox") not in ~w(objectBoundingBox userSpaceOnUse) ->
        invalid("SVG gradientUnits requires userSpaceOnUse or objectBoundingBox", node.tag)

      Enum.any?(geometry, fn property ->
        case Map.get(attrs, property) do
          nil ->
            false

          value ->
            case HtmlValidator.parse_css_number(value) do
              {_, unit} -> unit not in ~w(px pt pc mm cm in em %) and unit != ""
              _ -> false
            end
        end
      end) ->
        unsupported(
          "SVG gradient coordinates currently require native SVG units; rem and quarter-millimeter coordinates remain pending",
          node.tag
        )

      SvgTransformValidator.parse(Map.get(attrs, "gradientTransform", "none")) == :error or
          not Enum.all?(values, &match?({:ok, _}, &1)) ->
        invalid(
          "SVG gradient requires finite bounded coordinates and an affine gradientTransform",
          node.tag
        )

      Map.get(attrs, "fr", "0") != "0" ->
        unsupported("SVG radial focal radii require their complete radial geometry", node.tag)

      length(children) < 2 or Enum.any?(children, &(&1.tag != "stop")) ->
        unsupported("SVG gradients require at least two static stop children", node.tag)

      not valid_stops? ->
        invalid(
          "SVG stops require finite offsets, opaque RGB stop colors and stop-opacity",
          node.tag
        )

      true ->
        offsets = Enum.map(stops, fn {{:ok, value}, _, _} -> value end)
        opacity = Enum.map(stops, fn {_, _, {:ok, value}} -> value end)

        if hd(offsets) == 0.0 and List.last(offsets) == 1.0 and
             Enum.all?(Enum.chunk_every(offsets, 2, 1, :discard), fn [a, b] ->
               # Pinned usvg/PDF gradient offsets use IEEE binary32, a fixed
               # transport invariant rather than a configurable tolerance.
               <<native_a::float-32>> = <<a::float-32>>
               <<native_b::float-32>> = <<b::float-32>>
               native_a < native_b
             end) and
             length(Enum.uniq(opacity)) == 1,
           do: :ok,
           else:
             unsupported(
               "SVG gradient stops currently require strictly increasing native binary32 offsets from 0 to 1 and constant stop alpha",
               node.tag
             )
    end
  end

  defp invalid(message, source),
    do:
      Diagnostics.error(:layout, :invalid_document, message,
        source: source,
        module: __MODULE__,
        operation: :prepare
      )

  defp unsupported(message, source),
    do:
      Diagnostics.error(:layout, :unsupported_html, message,
        source: source,
        module: __MODULE__,
        operation: :prepare
      )
end
