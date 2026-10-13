defmodule NativeElixirPdfUtilities.Validators.SvgFilterValidator do
  @moduledoc false
  alias NativeElixirPdfUtilities.{Diagnostics, Limits}

  alias NativeElixirPdfUtilities.Validators.{
    HtmlValidator,
    SvgSceneValidator,
    SvgStyleValidator,
    SvgForeignObjectValidator,
    SvgImageValidator
  }

  @primitives ~w(feBlend feColorMatrix feComponentTransfer feComposite feConvolveMatrix feDiffuseLighting feDisplacementMap feDropShadow feFlood feGaussianBlur feImage feMerge feMorphology feOffset feSpecularLighting feTile feTurbulence)
  @children ~w(feMergeNode feFuncR feFuncG feFuncB feFuncA feDistantLight fePointLight feSpotLight)
  @inputs ~w(SourceGraphic SourceAlpha)
  @enum %{
    "feBlend" => %{
      "mode" =>
        ~w(normal multiply screen darken lighten overlay color-dodge color-burn hard-light soft-light difference exclusion hue saturation color luminosity)
    },
    "feColorMatrix" => %{"type" => ~w(matrix saturate hueRotate luminanceToAlpha)},
    "feComposite" => %{"operator" => ~w(over in out atop xor arithmetic)},
    "feConvolveMatrix" => %{
      "edgeMode" => ~w(duplicate wrap none),
      "preserveAlpha" => ~w(true false)
    },
    "feMorphology" => %{"operator" => ~w(erode dilate)},
    "feDisplacementMap" => %{"xChannelSelector" => ~w(R G B A), "yChannelSelector" => ~w(R G B A)},
    "feTurbulence" => %{
      "type" => ~w(turbulence fractalNoise),
      "stitchTiles" => ~w(stitch noStitch)
    }
  }
  @numbers ~w(dx dy scale k1 k2 k3 k4 divisor bias targetX targetY seed numOctaves slope intercept amplitude exponent offset surfaceScale diffuseConstant specularConstant specularExponent azimuth elevation pointsAtX pointsAtY pointsAtZ limitingConeAngle)
  @lists ~w(stdDeviation radius baseFrequency order kernelMatrix tableValues values)

  @doc false
  @spec tags() :: [String.t()]
  def tags, do: ["filter" | @primitives ++ @children]

  @doc false
  @spec validate_summary(map()) :: :ok | {:error, {atom(), map()}}
  def validate_summary(summary) do
    if summary.context.mode == :inline,
      do: validate_tree(SvgSceneValidator.summary_tree(summary)),
      else: :ok
  end

  @doc false
  @spec validate_tree(map()) :: :ok | {:error, {atom(), map()}}
  @spec validate_tree(map(), map()) :: :ok | {:error, {atom(), map()}}
  def validate_tree(root, issued \\ %{}) do
    resource_map? =
      is_map(issued) and map_size(issued) > 0 and
        Enum.all?(issued, fn {_id, entry} -> is_map(entry) and Map.has_key?(entry, :data) end)

    if resource_map? do
      with :ok <- SvgImageValidator.validate_descriptors(issued),
           do: validate_issued_tree(root, %{}, issued)
    else
      with {:ok, _ids} <- SvgForeignObjectValidator.validate_slots(issued),
           do: validate_issued_tree(root, issued, %{})
    end
  end

  defp validate_issued_tree(root, html_slots, resources) do
    nodes = SvgSceneValidator.tree_nodes(root)
    ids = Map.new(nodes, fn {node, _} -> {Map.get(node.attributes, "id"), node} end)

    Enum.reduce_while(nodes, :ok, fn {node, ancestors}, :ok ->
      reference =
        case Map.get(node, :style) do
          nil -> SvgStyleValidator.parse("filter", Map.get(node.attributes, "filter", "none"))
          style -> {:ok, Map.get(style, :svg_filter, :none)}
        end

      result =
        with :ok <- validate_parameters(node),
             :ok <- validate_structure(node, ancestors),
             :ok <- if(node.tag == "feImage", do: validate_image_input(node, ids), else: :ok),
             :ok <- if(node.tag == "filter", do: validate_graph(node, ids), else: :ok) do
          case reference do
            {:ok, :none} ->
              :ok

            {:ok, {:fragment, id}} ->
              case Map.get(ids, id) do
                %{tag: "filter"} ->
                  if foreign_surface?(node, ids, MapSet.new(), %{
                       html_slots: html_slots,
                       resources: resources
                     }),
                     do:
                       unsupported(
                         "filtered HTML requires its actual issued surface before effect evaluation",
                         node.tag
                       ),
                     else: :ok

                _ ->
                  invalid(
                    "filter must identify a same-scene filter definition",
                    node.tag <> " filter"
                  )
              end

            _ ->
              invalid("filter requires none or a local filter reference", node.tag <> " filter")
          end
        end

      case result do
        :ok -> {:cont, :ok}
        error -> {:halt, error}
      end
    end)
  end

  # Filter SourceGraphic includes expanded local instances, not just authored
  # descendants. An issued HTML surface cannot be replaced by its empty slot.
  defp foreign_surface?(node, ids, visited, inputs) do
    href = Map.get(node.attributes, "href", Map.get(node.attributes, "xlink:href", ""))

    values =
      case Map.get(node, :style) do
        nil ->
          Enum.flat_map(node.attributes, fn {property, value} ->
            case SvgStyleValidator.parse(property, value) do
              {:ok, parsed} -> [parsed]
              _ -> []
            end
          end)

        style ->
          Map.values(style)
      end

    references =
      for {kind, id} <- values, kind in [:paint_server, :fragment], do: id

    references =
      if node.tag == "use" and String.starts_with?(href, "#"),
        do: [String.trim_leading(href, "#") | references],
        else: references

    if node.tag == "foreignObject" do
      not Enum.any?(inputs.html_slots, fn {_id, slot} -> slot.node == node end)
    else
      (node.tag == "image" and
         map_size(Map.get(Map.get(inputs.resources, href, %{}), :foreign_slots, %{})) > 0) or
        Enum.any?(
          node.children,
          &(Map.get(&1, :type) == :element and foreign_surface?(&1, ids, visited, inputs))
        ) or
        Enum.any?(references, fn reference ->
          not MapSet.member?(visited, reference) and Map.has_key?(ids, reference) and
            foreign_surface?(
              Map.fetch!(ids, reference),
              ids,
              MapSet.put(visited, reference),
              inputs
            )
        end)
    end
  end

  defp validate_image_input(node, ids) do
    href = Map.get(node.attributes, "href", Map.get(node.attributes, "xlink:href", ""))

    case href do
      "#" <> id ->
        case Map.get(ids, id) do
          nil -> invalid("feImage must identify a same-scene painted input", "feImage href")
          target -> validate_image_vector(target, ids, MapSet.new([id]))
        end

      _ ->
        unsupported(
          "feImage external inputs require their authorized graph image resource context",
          "feImage href"
        )
    end
  end

  # Targets retain their original local tree and actual geometry. Source text
  # and HTML require independently issued selected-input plans, never borrowed
  # SourceGraphic replacement metadata.
  defp validate_image_vector(node, ids, visited) do
    if node.tag not in (~w(g rect circle ellipse line polyline polygon path use text tspan title desc linearGradient radialGradient stop clipPath mask pattern marker) ++
                          tags()) do
      unsupported(
        "feImage selected text, HTML and nested image inputs require their genuine independent paint plans",
        "feImage " <> node.tag
      )
    else
      href = Map.get(node.attributes, "href", Map.get(node.attributes, "xlink:href", ""))

      result =
        if node.tag == "use" do
          case href do
            "#" <> id ->
              cond do
                MapSet.member?(visited, id) ->
                  invalid("feImage local input references contain a cycle", "feImage href")

                not Map.has_key?(ids, id) ->
                  invalid("feImage local input reference is missing", "feImage href")

                true ->
                  validate_image_vector(Map.fetch!(ids, id), ids, MapSet.put(visited, id))
              end

            _ ->
              invalid("feImage reused inputs require explicit local references", "feImage href")
          end
        else
          :ok
        end

      values =
        if Map.has_key?(node, :style),
          do: Map.values(node.style),
          else:
            Enum.flat_map(node.attributes, fn {property, value} ->
              case SvgStyleValidator.parse(property, value) do
                {:ok, parsed} -> [parsed]
                _ -> []
              end
            end)

      references = for {kind, id} <- values, kind in [:paint_server, :fragment], do: id

      painted =
        Enum.reduce_while(references, :ok, fn id, :ok ->
          cond do
            MapSet.member?(visited, id) ->
              {:halt, invalid("feImage input paint references contain a cycle", "feImage href")}

            not Map.has_key?(ids, id) ->
              {:halt, invalid("feImage input paint reference is missing", "feImage href")}

            true ->
              case validate_image_vector(Map.fetch!(ids, id), ids, MapSet.put(visited, id)) do
                :ok -> {:cont, :ok}
                error -> {:halt, error}
              end
          end
        end)

      with :ok <- result, :ok <- painted do
        Enum.reduce_while(node.children, :ok, fn child, :ok ->
          if Map.get(child, :type) == :element do
            case validate_image_vector(child, ids, visited) do
              :ok -> {:cont, :ok}
              error -> {:halt, error}
            end
          else
            {:cont, :ok}
          end
        end)
      end
    end
  end

  defp validate_structure(node, ancestors) do
    parent = List.first(ancestors)

    expected =
      cond do
        node.tag in @primitives ->
          parent == "filter"

        node.tag == "feMergeNode" ->
          parent == "feMerge"

        node.tag in ~w(feFuncR feFuncG feFuncB feFuncA) ->
          parent == "feComponentTransfer"

        node.tag in ~w(feDistantLight fePointLight feSpotLight) ->
          parent in ~w(feDiffuseLighting feSpecularLighting)

        true ->
          true
      end

    children =
      Enum.filter(
        node.children,
        &(Map.get(&1, :type) == :element and &1.tag not in ~w(title desc))
      )

    allowed =
      cond do
        node.tag == "filter" ->
          @primitives

        node.tag == "feMerge" ->
          ["feMergeNode"]

        node.tag == "feComponentTransfer" ->
          ~w(feFuncR feFuncG feFuncB feFuncA)

        node.tag in ~w(feDiffuseLighting feSpecularLighting) ->
          ~w(feDistantLight fePointLight feSpotLight)

        node.tag in (@primitives ++ @children) ->
          []

        true ->
          Enum.map(children, & &1.tag)
      end

    cond do
      not expected ->
        invalid("filter operation has an invalid parent", node.tag)

      Enum.any?(children, &(&1.tag not in allowed)) ->
        invalid("filter contains an unsupported operation child", node.tag)

      node.tag in ~w(feDiffuseLighting feSpecularLighting) and length(children) != 1 ->
        invalid("lighting requires exactly one light source", node.tag)

      node.tag == "feComponentTransfer" and
          length(Enum.uniq_by(children, & &1.tag)) != length(children) ->
        invalid("component transfer requires at most one function per channel", node.tag)

      true ->
        :ok
    end
  end

  defp validate_graph(node, ids) do
    href = Map.get(node.attributes, "href", Map.get(node.attributes, "xlink:href"))

    if href &&
         (not String.starts_with?(href, "#") or
            not match?(%{tag: "filter"}, Map.get(ids, String.trim_leading(href, "#")))) do
      invalid("filter inheritance requires a same-scene filter target", "filter href")
    else
      Enum.reduce_while(node.children, {:ok, MapSet.new(@inputs)}, fn child, {:ok, known} ->
        if Map.get(child, :type) == :element and child.tag in @primitives do
          inputs = [child | Enum.filter(child.children, &(Map.get(&1, :tag) == "feMergeNode"))]

          bad =
            Enum.find_value(inputs, fn input ->
              Enum.find(~w(in in2), fn key ->
                value = Map.get(input.attributes, key)
                value != nil and not MapSet.member?(known, value)
              end)
            end)

          result = Map.get(child.attributes, "result")

          cond do
            bad ->
              {:halt,
               invalid(
                 "filter input #{bad} must name SourceGraphic, SourceAlpha or an earlier result",
                 child.tag
               )}

            result != nil and
                (result in @inputs or not Regex.match?(~r/\A[A-Za-z_][A-Za-z0-9_.:-]*\z/, result)) ->
              {:halt,
               invalid("filter result requires a non-reserved local name", child.tag <> " result")}

            true ->
              {:cont, {:ok, if(result, do: MapSet.put(known, result), else: known)}}
          end
        else
          {:cont, {:ok, known}}
        end
      end)
      |> case do
        {:ok, _} -> :ok
        error -> error
      end
    end
  end

  defp validate_parameters(node) do
    if node.tag in tags() do
      attrs = node.attributes

      bad =
        Enum.find(attrs, fn {key, value} ->
          cond do
            key in ~w(x y z) and node.tag in ~w(fePointLight feSpotLight) ->
              not match?({_, ""}, HtmlValidator.parse_css_number(value))

            key in ~w(x y width height) ->
              not match?({:ok, _}, SvgStyleValidator.length(value, key in ~w(x y)))

            key in ~w(filterUnits primitiveUnits) ->
              value not in ~w(userSpaceOnUse objectBoundingBox)

            key in ~w(filterRes kernelUnitLength) ->
              true

            key in ~w(flood-color flood-opacity lighting-color color-interpolation-filters) ->
              SvgStyleValidator.parse(key, value) == :error

            key in @numbers ->
              not match?({_, ""}, HtmlValidator.parse_css_number(value))

            key in @lists ->
              numbers(value) == :error

            Map.has_key?(Map.get(@enum, node.tag, %{}), key) ->
              value not in Map.fetch!(Map.fetch!(@enum, node.tag), key)

            key == "type" and node.tag in ~w(feFuncR feFuncG feFuncB feFuncA) ->
              value not in ~w(identity table discrete linear gamma)

            true ->
              false
          end
        end)

      if bad do
        {key, _} = bad

        invalid(
          "filter #{key} has an invalid or unsupported bounded value",
          node.tag <> " " <> key
        )
      else
        validate_numeric_semantics(node)
      end
    else
      :ok
    end
  end

  defp validate_numeric_semantics(node) do
    attrs = node.attributes

    invalid_list =
      Enum.find(~w(stdDeviation radius baseFrequency), fn key ->
        case Map.get(attrs, key) do
          nil ->
            false

          value ->
            parsed = numbers(value)
            length(parsed) not in [1, 2] or Enum.any?(parsed, &(&1 < 0))
        end
      end)

    number = fn key, default ->
      case HtmlValidator.parse_css_number(Map.get(attrs, key, to_string(default))) do
        {value, ""} -> value
      end
    end

    cond do
      invalid_list ->
        invalid("filter #{invalid_list} requires one or two nonnegative numbers", node.tag)

      node.tag == "feColorMatrix" ->
        type = Map.get(attrs, "type", "matrix")
        values = if Map.has_key?(attrs, "values"), do: numbers(attrs["values"]), else: nil

        valid =
          case {type, values} do
            {_, nil} -> true
            {"matrix", list} -> length(list) == 20
            {"saturate", [value]} -> value >= 0 and value <= 1
            {"hueRotate", [_]} -> true
            {"luminanceToAlpha", _} -> true
            _ -> false
          end

        if valid,
          do: :ok,
          else: invalid("color matrix requires values matching its type", node.tag)

      node.tag == "feConvolveMatrix" ->
        order = numbers(Map.get(attrs, "order", "3"))
        order = if length(order) == 1, do: order ++ order, else: order
        kernel = numbers(Map.get(attrs, "kernelMatrix", ""))
        valid_order = length(order) == 2 and Enum.all?(order, &(&1 > 0 and &1 == trunc(&1)))

        cond do
          not valid_order ->
            invalid("convolution order requires one or two positive integers", node.tag)

          Enum.product(order) > Limits.get(:max_svg_filter_kernel_values) ->
            limit("max_svg_filter_kernel_values", node.tag)

          kernel == :error or length(kernel) != trunc(Enum.product(order)) ->
            invalid("convolution kernelMatrix must match its order", node.tag)

          number.("divisor", 1) == 0 ->
            invalid("convolution divisor cannot be zero", node.tag)

          Enum.any?(Enum.with_index(~w(targetX targetY)), fn {key, index} ->
            value = number.(key, 0)
            value < 0 or value != trunc(value) or value >= Enum.at(order, index)
          end) ->
            invalid("convolution target must lie inside its kernel", node.tag)

          true ->
            :ok
        end

      node.tag == "feTurbulence" ->
        octaves = number.("numOctaves", 1)

        cond do
          octaves < 0 or octaves != trunc(octaves) ->
            invalid("turbulence numOctaves requires a nonnegative integer", node.tag)

          octaves > Limits.get(:max_svg_filter_octaves) ->
            limit("max_svg_filter_octaves", node.tag)

          true ->
            :ok
        end

      node.tag in ~w(feSpecularLighting feSpotLight) and
          (number.("specularExponent", 1) < 1 or number.("specularExponent", 1) > 128) ->
        invalid("specularExponent requires the SVG range 1 through 128", node.tag)

      true ->
        :ok
    end
  end

  # Number-list syntax is checked completely before the backend may normalize it.
  defp numbers(value) do
    parts = String.split(String.trim(value), ~r/[\s,]+/, trim: true)
    parsed = Enum.map(parts, &HtmlValidator.parse_css_number/1)

    if parts != [] and not Regex.match?(~r/\A\s*,|,\s*\z|,\s*,/, value) and
         Enum.all?(parsed, &match?({_, ""}, &1)), do: Enum.map(parsed, &elem(&1, 0)), else: :error
  end

  defp invalid(message, source),
    do:
      Diagnostics.error(:layout, :invalid_document, message,
        module: __MODULE__,
        operation: :prepare,
        source: source
      )

  defp unsupported(message, source),
    do:
      Diagnostics.error(:layout, :unsupported_html, message,
        module: __MODULE__,
        operation: :prepare,
        source: source
      )

  defp limit(name, source),
    do:
      Diagnostics.error(:limits, :resource_limit_exceeded, "SVG filter exceeds " <> name,
        module: __MODULE__,
        operation: :prepare,
        source: source
      )
end
