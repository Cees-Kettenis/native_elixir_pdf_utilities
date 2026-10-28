defmodule NativeElixirPdfUtilities.Validators.SvgFilterWorkValidator do
  @moduledoc false

  alias NativeElixirPdfUtilities.{Diagnostics, Limits}
  alias NativeElixirPdfUtilities.HtmlToPdf.{CssParser, SvgViewport}
  alias NativeElixirPdfUtilities.Validators.{HtmlValidator, SvgSceneValidator, SvgStyleValidator}

  # Resvg 0.47 clips isolated layers to five canvas widths and heights.
  # This is a backend invariant, not a configurable resource ceiling.
  @layer_extent 5
  @neighborhood ~w(feMorphology feConvolveMatrix)
  @preserving ~w(feGaussianBlur feOffset feDropShadow feColorMatrix feComponentTransfer feMorphology)

  @doc "Reserves conservative neighborhood-filter work before any native SVG conversion."
  @spec reserve(map(), keyword(), map()) :: :ok | {:error, {atom(), map()}}
  def reserve(root, dimensions, work) do
    HtmlValidator.with_render_budget(fn ->
      try do
        reserve_document(root, {dimensions[:width], dimensions[:height]}, work, 1, false)
      catch
        {:svg_filter_work_unbounded, context} -> unbounded(context)
        {:render_resource_limit, error} -> error
      end
    end)
    |> case do
      {:error, {reason, diagnostic}} ->
        {:error,
         {reason,
          diagnostic
          |> Map.update!(
            :message,
            fn message ->
              if Map.get(diagnostic, :module) == __MODULE__,
                do: message,
                else:
                  message <>
                    "; reduce SVG size, filter radius/order or reuse, or increase the configured work limit"
            end
          )
          |> Diagnostics.with_context(module: __MODULE__, operation: :prepare, source: "SVG")}}

      :ok ->
        :ok
    end
  end

  defp reserve_document(root, canvas, work, multiplicity, nested?) do
    nodes = Enum.map(SvgSceneValidator.tree_nodes(root), &elem(&1, 0))
    heavy? = Enum.any?(nodes, &(&1.tag in @neighborhood))
    servers? = Enum.any?(nodes, &(&1.tag in ~w(pattern marker mask feImage)))

    with :ok <-
           if(heavy? and (canvas == nil or servers?),
             do: unbounded("pattern, marker, mask or filter-image rendering contexts"),
             else: :ok
           ),
         :ok <-
           if(heavy?,
             do: reserve_filters(root, nodes, canvas, work, multiplicity, nested?),
             else: :ok
           ) do
      Enum.reduce_while(nodes, :ok, fn node, :ok ->
        case Map.get(node, :svg_filter_work) do
          nil ->
            {:cont, :ok}

          %{root: child} = child_work ->
            child_canvas =
              if canvas != nil and not servers? and node.tag != "feImage" do
                {width, height} = canvas
                {@layer_extent * width, @layer_extent * height}
              end

            case reserve_document(
                   child,
                   child_canvas,
                   child_work,
                   multiplicity * work.expansion,
                   true
                 ) do
              :ok -> {:cont, :ok}
              error -> {:halt, error}
            end
        end
      end)
    end
  end

  defp reserve_filters(root, nodes, {width, height}, work, multiplicity, nested?) do
    inherited? =
      Enum.any?(nodes, fn node ->
        node.tag == "filter" and
          Enum.any?(Map.keys(node.attributes), &(&1 in ~w(href xlink:href)))
      end)

    styles =
      Enum.flat_map(nodes, fn node ->
        inline =
          case Map.get(node.attributes, "style") do
            nil -> []
            value -> [{:inline, value}]
          end

        sheet = if node.tag == "style", do: [{:sheet, text(node)}], else: []
        inline ++ sheet
      end)

    css = css_profile(styles)

    implicit_attachments? =
      Enum.any?(nodes, fn node ->
        case SvgStyleValidator.parse("filter", Map.get(node.attributes, "filter", "none")) do
          {:ok, :none} -> false
          {:ok, {:fragment, _}} -> false
          _ -> true
        end
      end)

    repetitions =
      max(
        css.repetitions,
        Enum.reduce(nodes, 1, fn node, maximum ->
          max(maximum, length(:binary.matches(Map.get(node.attributes, "filter", ""), "(")))
        end)
      )

    scale =
      if nested? or inherited? or css.geometry? or
           Enum.any?(nodes, &(&1 != root and &1.tag in ~w(svg symbol))),
         do: :unknown,
         else: world_scale(root, nodes, width, height)

    if css.unknown?, do: throw({:svg_filter_work_unbounded, "CSS filter declarations"})

    plane = {@layer_extent * width, @layer_extent * height}

    nodes
    |> Enum.filter(&(&1.tag == "filter"))
    |> Enum.reduce_while(:ok, fn filter, :ok ->
      region = if inherited?, do: :unknown, else: region_plane(filter, scale)

      input_plane =
        if repetitions == 1 and region != :unknown,
          do: {min(elem(plane, 0), elem(region, 0)), min(elem(plane, 1), elem(region, 1))},
          else: plane

      uses =
        if css.attachments? or implicit_attachments? or repetitions > 1,
          do: work.expansion,
          else: Map.get(work.counts, Map.get(filter.attributes, "id"), 1)

      filter_multiplicity = multiplicity * max(1, uses) * repetitions

      result =
        Enum.reduce_while(filter.children, {:ok, input_plane, %{}}, fn primitive,
                                                                       {:ok, previous, results} ->
          if Map.get(primitive, :type) == :element and String.starts_with?(primitive.tag, "fe") do
            input =
              case Map.get(primitive.attributes, "in") do
                source
                when source in ~w(SourceGraphic SourceAlpha BackgroundImage BackgroundAlpha FillPaint StrokePaint) ->
                  input_plane

                nil ->
                  previous

                name ->
                  # The backend also generates names such as result1 for unnamed
                  # outputs. An unrecognized name may select any earlier output.
                  Map.get(results, name, Map.get(results, :any, input_plane))
              end

            output = if primitive.tag in @preserving, do: input, else: region

            result =
              if primitive.tag in @neighborhood,
                do: reserve_primitive(primitive, input, filter, scale, filter_multiplicity),
                else: :ok

            case result do
              :ok ->
                results = Map.put(results, Map.get(primitive.attributes, "result", ""), output)
                results = Map.update(results, :any, output, &union_plane(&1, output))
                {:cont, {:ok, output, results}}

              error ->
                {:halt, error}
            end
          else
            {:cont, {:ok, previous, results}}
          end
        end)

      case result do
        {:ok, _, _} -> {:cont, :ok}
        error -> {:halt, error}
      end
    end)
  end

  defp union_plane(first, second) do
    case {first, second} do
      {{w1, h1}, {w2, h2}} -> {max(w1, w2), max(h1, h2)}
      _ -> :unknown
    end
  end

  defp reserve_primitive(node, plane, filter, scale, multiplicity) do
    case plane do
      :unknown ->
        unbounded("an intermediate filter region before #{node.tag}")

      {width, height} ->
        case node.tag do
          "feMorphology" ->
            {columns, rows} =
              if scale == :unknown or
                   Map.get(filter.attributes, "primitiveUnits") == "objectBoundingBox" do
                {width, height}
              else
                # The backend replaces zero radii with one. If the shared number
                # parser cannot bound the authored values, use the whole plane.
                radii =
                  case numbers(Map.get(node.attributes, "radius", "1")) do
                    {:ok, [x]} when x >= 0 -> {max(1.0, x), max(1.0, x)}
                    {:ok, [x, y]} when x >= 0 and y >= 0 -> {max(1.0, x), max(1.0, y)}
                    _ -> {width, height}
                  end

                {rx, ry} = radii
                # Round outward, including native f32 conversion at each transform.
                columns =
                  if rx >= width / (2 * scale),
                    do: width,
                    else: min(width, 2 * (ceil(2 * rx * scale) + 1))

                rows =
                  if ry >= height / (2 * scale),
                    do: height,
                    else: min(height, 2 * (ceil(2 * ry * scale) + 1))

                {columns, rows}
              end

            HtmlValidator.reserve_render_resource(
              :max_svg_filter_work,
              width * height * columns * rows * multiplicity,
              :limits
            )

          "feConvolveMatrix" ->
            with {:ok, values} <- numbers(Map.get(node.attributes, "order", "3")),
                 {:ok, {columns, rows}} <- order(values) do
              wrap =
                if String.trim(Map.get(node.attributes, "edgeMode", "")) == "wrap",
                  do: 1 + columns + rows,
                  else: 1

              HtmlValidator.reserve_render_resource(
                :max_svg_filter_work,
                width * height * columns * rows * wrap * multiplicity,
                :limits
              )
            else
              _ -> unbounded("a convolution order that cannot be conservatively parsed")
            end
        end
    end
  end

  defp order(values) do
    pair =
      case values do
        [x] -> {x, x}
        [x, y] -> {x, y}
        _ -> nil
      end

    case pair do
      {x, y} when x > 0 and y > 0 ->
        # f32 exactly represents integers up to 2^24; larger orders round outward.
        upper = fn value ->
          integer = ceil(value)
          max(3, if(value > 16_777_216, do: integer * 2, else: integer))
        end

        {:ok, {upper.(x), upper.(y)}}

      {_, _} ->
        {:ok, {3, 3}}

      nil ->
        :error
    end
  end

  defp region_plane(filter, scale) do
    with true <-
           scale != :unknown and Map.get(filter.attributes, "filterUnits") == "userSpaceOnUse",
         {:ok, width} <- SvgStyleValidator.parse("width", Map.get(filter.attributes, "width")),
         {:ok, height} <- SvgStyleValidator.parse("height", Map.get(filter.attributes, "height")),
         {:user, width} <- width,
         {:user, height} <- height do
      # A rotation or shear can put both authored axes into either device axis.
      extent = 2 * ceil((abs(width) + abs(height)) * scale) + 2
      {extent, extent}
    else
      _ -> :unknown
    end
  rescue
    ArithmeticError -> :unknown
  end

  defp world_scale(root, nodes, width, height) do
    with {:ok, viewport} <-
           SvgViewport.measure(root.attributes, %{font_size: 12.0, _root_font_size: 12.0}, %{
             width: width * 0.75,
             height: height * 0.75
           }),
         true <- viewport.width > 0 and viewport.height > 0 do
      {a, b, c, d, _, _} = viewport.ctm

      initial =
        max(width / viewport.width, height / viewport.height) *
          max(abs(a) + abs(c), abs(b) + abs(d))

      Enum.reduce_while(nodes, max(1.0, initial), fn node, scale ->
        case transform_scale(Map.get(node.attributes, "transform", "")) do
          {:ok, gain} ->
            scaled = scale * gain * gain

            if scaled <= Limits.get(:max_svg_filter_work),
              do: {:cont, scaled},
              else: {:halt, :unknown}

          :error ->
            {:halt, :unknown}
        end
      end)
    else
      _ -> :unknown
    end
  rescue
    ArithmeticError -> :unknown
  end

  defp transform_scale(source, count \\ 0) do
    source = String.trim(source)

    if source in ["", "none"] do
      {:ok, 1.0}
    else
      HtmlValidator.check_render_resource(:max_css_transform_operations, count + 1, :limits)
      pattern = ~r/\A\s*(matrix|translate|scale|rotate|skewX|skewY)\s*\(([^()]*)\)\s*,?\s*/

      case Regex.run(pattern, source) do
        [matched, name, arguments] ->
          with {:ok, values} <- numbers(arguments),
               {:ok, gain} <- transform_gain(name, values),
               {:ok, rest} <-
                 transform_scale(
                   binary_part(
                     source,
                     byte_size(matched),
                     byte_size(source) - byte_size(matched)
                   ),
                   count + 1
                 ) do
            {:ok, max(1.0, gain) * rest}
          else
            _ -> :error
          end

        _ ->
          :error
      end
    end
  rescue
    ArithmeticError -> :error
  end

  defp transform_gain(name, values) do
    case {name, values} do
      {"matrix", [a, b, c, d, _, _]} ->
        {:ok, abs(a) + abs(b) + abs(c) + abs(d)}

      {"translate", [_]} ->
        {:ok, 1.0}

      {"translate", [_, _]} ->
        {:ok, 1.0}

      {"scale", [x]} ->
        {:ok, abs(x)}

      {"scale", [x, y]} ->
        {:ok, max(abs(x), abs(y))}

      {"rotate", [_]} ->
        {:ok, 1.0}

      {"rotate", [_, _, _]} ->
        {:ok, 1.0}

      {name, [angle]} when name in ["skewX", "skewY"] ->
        {:ok, 1.0 + abs(:math.tan(angle * :math.pi() / 180))}

      _ ->
        :error
    end
  end

  defp css_profile(styles) do
    Enum.reduce(
      styles,
      %{unknown?: false, geometry?: false, attachments?: false, repetitions: 1},
      fn {kind, source}, profile ->
        profile = %{
          profile
          | unknown?: profile.unknown? or (kind == :sheet and String.contains?(source, "@"))
        }

        result =
          if kind == :inline do
            CssParser.parse_declarations_detailed(source)
          else
            case CssParser.parse_detailed(source) do
              {:ok, rules} -> {:ok, Enum.flat_map(rules, & &1.declarations)}
              error -> error
            end
          end

        case result do
          {:ok, declarations} ->
            Enum.reduce(declarations, profile, fn declaration, profile ->
              property = elem(declaration, 0)
              value = elem(declaration, 1)

              %{
                profile
                | unknown?:
                    profile.unknown? or String.starts_with?(property, "--") or
                      String.contains?(value, "var("),
                  geometry?: profile.geometry? or property in ~w(transform width height),
                  attachments?: profile.attachments? or property == "filter",
                  repetitions:
                    max(
                      profile.repetitions,
                      if(property == "filter", do: length(:binary.matches(value, "(")), else: 1)
                    )
              }
            end)

          _ ->
            %{profile | unknown?: true}
        end
      end
    )
  end

  defp numbers(source) do
    tokens = String.split(source, ~r/[\s,]+/, trim: true)

    Enum.reduce_while(tokens, {:ok, []}, fn token, {:ok, result} ->
      case HtmlValidator.parse_css_number(token) do
        {number, ""} -> {:cont, {:ok, [number | result]}}
        _ -> {:halt, :error}
      end
    end)
    |> case do
      {:ok, values} -> {:ok, Enum.reverse(values)}
      error -> error
    end
  end

  defp text(node) do
    Enum.map_join(Map.get(node, :children, []), fn child ->
      case child.type do
        :text -> child.text
        :element -> text(child)
      end
    end)
  end

  defp unbounded(context) do
    Diagnostics.error(
      :limits,
      :resource_limit_exceeded,
      "SVG work cannot be bounded within max_svg_filter_work (#{Limits.get(:max_svg_filter_work)}): #{context}; simplify the filter region or reused artwork",
      module: __MODULE__,
      operation: :prepare,
      source: "SVG"
    )
  end
end
