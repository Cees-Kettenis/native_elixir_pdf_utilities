defmodule NativeElixirPdfUtilities.Validators.CssGradientValidator do
  @moduledoc "CSS gradient syntax, stop fixup and shared raster admission."
  alias NativeElixirPdfUtilities.{Diagnostics, Limits}

  alias NativeElixirPdfUtilities.Validators.{
    HtmlValidator,
    CssColorValidator,
    CssLengthValidator,
    CssMathValidator,
    CssValueValidator
  }

  @spaces ~w(srgb srgb-linear display-p3 display-p3-linear a98-rgb prophoto-rgb rec2020 rec2100-linear rec2100-pq rec2100-hlg xyz xyz-d50 xyz-d65 lab lch oklab oklch ictcp jzazbz jzczhz hsl hwb)

  @doc "Prepares a static linear, radial or conic image with its own interpolation and stop list."
  @spec prepare(String.t(), map()) :: {:ok, map()} | :error | {:error, term()}
  def prepare(source, style) do
    HtmlValidator.check_render_resource(:max_input_bytes, byte_size(source), :style)
    HtmlValidator.reserve_render_resource(:max_work, byte_size(source), :style)
    source = source |> HtmlValidator.normalize_css_comments(:value) |> String.trim()

    with {:ok, name, "(" <> rest} <-
           HtmlValidator.parse_css_identifier(String.downcase(source, :ascii)),
         true <- String.ends_with?(rest, ")"),
         name = String.replace_prefix(name, "repeating-", ""),
         true <- name in ~w(linear-gradient radial-gradient conic-gradient) do
      kind =
        case name do
          "linear-gradient" -> :linear
          "radial-gradient" -> :radial
          _ -> :conic
        end

      parts = rest |> binary_part(0, byte_size(rest) - 1) |> HtmlValidator.split_css_comma(true)
      HtmlValidator.reserve_render_resource(:max_entries, length(parts), :style)

      # The shared comma splitter retains an empty component for empty input.
      [first | remaining] = parts

      {header, stops} =
        case stop(first, kind, style) do
          {:ok, [%{color: _} | _]} -> {"", parts}
          _ -> {first, remaining}
        end

      with {:ok, geometry, space, hue} <- header(header, kind),
           {:ok, stops} <- prepare_stops(stops, kind, style),
           do:
             {:ok,
              %{
                kind: :gradient,
                type: kind,
                geometry: geometry,
                space: space,
                hue: hue,
                stops: stops,
                repeating?: String.starts_with?(String.downcase(source, :ascii), "repeating-")
              }}
    else
      _ -> :error
    end
  end

  defp header(source, kind) do
    tokens = HtmlValidator.split_css_tokens(source)
    {before, method} = Enum.split_while(tokens, &(&1 != "in"))

    {geometry, parsed} =
      case method do
        [] ->
          {before, {:ok, "srgb", :shorter}}

        ["in", space | rest] when space in @spaces ->
          {hue, after_method} =
            case rest do
              [method, "hue" | rest] -> {[method, "hue"], rest}
              _ -> {[], rest}
            end

          parsed =
            with {:ok, hue} <- CssColorValidator.interpolation_method(space, hue),
                 do: {:ok, space, hue}

          {before ++ after_method, parsed}

        _ ->
          {[], :error}
      end

    with {:ok, space, hue} <- parsed,
         {:ok, geometry} <- geometry(kind, geometry),
         do: {:ok, geometry, space, hue}
  end

  defp geometry(kind, tokens) do
    case {kind, tokens} do
      {:linear, []} ->
        {:ok, %{direction: {:angle, :math.pi()}}}

      {:linear, [angle]} ->
        with {:ok, angle} <- angle(angle), do: {:ok, %{direction: {:angle, angle}}}

      {:linear, ["to" | sides]} ->
        if length(sides) in [1, 2] and length(Enum.uniq(sides)) == length(sides) and
             Enum.all?(sides, &(&1 in ~w(top bottom left right))) and
             not ("top" in sides and "bottom" in sides) and
             not ("left" in sides and "right" in sides),
           do: {:ok, %{direction: {:sides, sides}}},
           else: :error

      {:conic, tokens} ->
        {from, tokens} =
          case tokens do
            ["from", value | rest] -> {angle(value), rest}
            _ -> {{:ok, 0.0}, tokens}
          end

        with {:ok, from} <- from,
             {:ok, position} <- position(tokens),
             do: {:ok, %{from: from, position: position}}

      {:radial, tokens} ->
        {sizes, pos} = Enum.split_while(tokens, &(&1 != "at"))

        {shape, sizes} =
          case Enum.split_with(sizes, &(&1 in ["circle", "ellipse"])) do
            {[], sizes} ->
              {if(
                 length(sizes) == 1 and
                   hd(sizes) not in ~w(closest-side farthest-side closest-corner farthest-corner),
                 do: :circle,
                 else: :ellipse
               ), sizes}

            {["circle"], sizes} ->
              {:circle, sizes}

            {["ellipse"], sizes} ->
              {:ellipse, sizes}

            _ ->
              {nil, []}
          end

        radius =
          case sizes do
            [] ->
              {:ok, :farthest_corner}

            [keyword]
            when keyword in ~w(closest-side farthest-side closest-corner farthest-corner) ->
              {:ok, keyword |> String.replace("-", "_") |> String.to_existing_atom()}

            [one] when shape == :circle ->
              with {:ok, length} <- CssLengthValidator.prepare(one, :length),
                   do: {:ok, [length]}

            [a, b] when shape == :ellipse ->
              with {:ok, a} <- CssLengthValidator.prepare(a, :padding),
                   {:ok, b} <- CssLengthValidator.prepare(b, :padding),
                   do: {:ok, [a, b]}

            _ ->
              :error
          end

        with true <- shape != nil,
             {:ok, radius} <- radius,
             {:ok, position} <- position(pos),
             do: {:ok, %{shape: shape, radius: radius, position: position}},
             else: (_ -> :error)

      _ ->
        :error
    end
  end

  defp position(tokens) do
    case tokens do
      [] ->
        {:ok, {{:percent, 0.5}, {:percent, 0.5}}}

      ["at" | rest] when rest != [] ->
        CssValueValidator.parse_two_axis_position(Enum.join(rest, " "), nil, true)

      _ ->
        :error
    end
  end

  defp angle(source) do
    case CssMathValidator.evaluate_color_component(source) do
      {:ok, value, %{angle: 1}} when is_number(value) -> {:ok, value}
      {:ok, zero, type} when zero == 0 and type == %{} -> {:ok, 0.0}
      _ -> :error
    end
  end

  defp stop(source, kind, style) do
    case HtmlValidator.split_css_tokens(source) do
      [color | positions] when length(positions) <= 2 ->
        case CssColorValidator.prepare(color, style) do
          {:ok, color} ->
            Enum.reduce_while(positions, {:ok, []}, fn value, {:ok, acc} ->
              case stop_position(value, kind) do
                {:ok, value} -> {:cont, {:ok, acc ++ [value]}}
                error -> {:halt, error}
              end
            end)
            |> case do
              {:ok, []} -> {:ok, [%{color: color, position: nil}]}
              {:ok, positions} -> {:ok, Enum.map(positions, &%{color: color, position: &1})}
              error -> error
            end

          _ when positions == [] ->
            with {:ok, position} <- stop_position(color, kind),
                 do: {:ok, [%{hint: position}]}

          _ ->
            :error
        end

      _ ->
        :error
    end
  end

  defp stop_position(source, kind) do
    if kind == :conic do
      case HtmlValidator.parse_css_number(source) do
        {value, "%"} -> {:ok, {:fraction, value / 100}}
        _ -> with {:ok, angle} <- angle(source), do: {:ok, {:fraction, angle / (2 * :math.pi())}}
      end
    else
      with {:ok, value} when value != :auto <- CssLengthValidator.prepare(source, :margin),
           do: {:ok, value},
           else: (_ -> :error)
    end
  end

  defp prepare_stops(sources, kind, style) do
    Enum.reduce_while(sources, {:ok, []}, fn source, {:ok, stops} ->
      case stop(source, kind, style) do
        {:ok, parsed} -> {:cont, {:ok, Enum.reverse(parsed, stops)}}
        error -> {:halt, error}
      end
    end)
    |> case do
      {:ok, stops} when length(stops) >= 2 ->
        stops = Enum.reverse(stops)

        hints? =
          Enum.chunk_every(stops, 2, 1, :discard)
          |> Enum.any?(fn [a, b] -> Map.has_key?(a, :hint) and Map.has_key?(b, :hint) end)

        if Map.has_key?(hd(stops), :color) and Map.has_key?(List.last(stops), :color) and
             not hints?, do: {:ok, stops}, else: :error

      _ ->
        :error
    end
  end

  @doc "Freezes gradient lengths and positions at their declaring font while keeping used-size percentages."
  @spec compute(map(), map()) :: {:ok, map()} | :error | {:error, term()}
  def compute(gradient, style) do
    geometry = gradient.geometry

    position =
      if Map.has_key?(geometry, :position),
        do:
          NativeElixirPdfUtilities.Validators.CssPositionValidator.compute_position(
            geometry.position,
            style
          ),
        else: {:ok, nil}

    radius =
      case Map.get(geometry, :radius) do
        sizes when is_list(sizes) ->
          Enum.reduce_while(sizes, {:ok, []}, fn size, {:ok, out} ->
            case CssLengthValidator.resolve(size, nil, style) do
              {:ok, n} -> {:cont, {:ok, [n | out]}}
              error -> {:halt, error}
            end
          end)
          |> case do
            {:ok, sizes} -> {:ok, Enum.reverse(sizes)}
            error -> error
          end

        radius ->
          {:ok, radius}
      end

    with {:ok, position} <- position, {:ok, radius} <- radius do
      geometry = if position == nil, do: geometry, else: Map.put(geometry, :position, position)
      geometry = if radius == nil, do: geometry, else: Map.put(geometry, :radius, radius)

      Enum.reduce_while(gradient.stops, {:ok, []}, fn stop, {:ok, out} ->
        key = if Map.has_key?(stop, :hint), do: :hint, else: :position

        case Map.fetch!(stop, key) do
          value when is_number(value) or is_nil(value) ->
            {:cont, {:ok, [stop | out]}}

          {:fraction, _} ->
            {:cont, {:ok, [stop | out]}}

          value ->
            case CssLengthValidator.resolve(value, nil, style) do
              {:ok, value} -> {:cont, {:ok, [Map.put(stop, key, value) | out]}}
              error -> {:halt, error}
            end
        end
      end)
      |> case do
        {:ok, stops} -> {:ok, %{gradient | geometry: geometry, stops: Enum.reverse(stops)}}
        error -> error
      end
    end
  end

  @doc "Checks validator-prepared gradient geometry and color stops supplied to public layout APIs."
  @spec valid_prepared?(term()) :: boolean()
  def valid_prepared?(value) do
    case value do
      %{
        kind: :gradient,
        type: type,
        geometry: geometry,
        space: space,
        hue: hue,
        stops: [_a, _b | _] = stops,
        repeating?: repeating?
      } ->
        HtmlValidator.check_render_resource(:max_entries, length(stops), :layout)

        position? = fn p ->
          case p do
            nil ->
              true

            {:fraction, n} ->
              is_number(n) and match?({:ok, _}, HtmlValidator.normalize_geometry_number(n))

            p ->
              p != :auto and CssLengthValidator.valid_edge?(p, :margin)
          end
        end

        geometry? =
          case {type, geometry} do
            {:linear, %{direction: {:angle, n}}} ->
              is_number(n) and match?({:ok, _}, HtmlValidator.normalize_geometry_number(n))

            {:linear, %{direction: {:sides, sides}}} when is_list(sides) ->
              length(sides) in [1, 2] and length(Enum.uniq(sides)) == length(sides) and
                Enum.all?(sides, &(&1 in ~w(left right top bottom))) and
                not ("left" in sides and "right" in sides) and
                not ("top" in sides and "bottom" in sides)

            {:conic, %{from: from, position: {x, y}}} ->
              is_number(from) and match?({:ok, _}, HtmlValidator.normalize_geometry_number(from)) and
                NativeElixirPdfUtilities.Validators.CssPositionValidator.valid_prepared?(x) and
                NativeElixirPdfUtilities.Validators.CssPositionValidator.valid_prepared?(y)

            {:radial, %{shape: shape, radius: radius, position: {x, y}}}
            when shape in [:circle, :ellipse] ->
              radius? =
                case {shape, radius} do
                  {_, r}
                  when r in [:closest_side, :farthest_side, :closest_corner, :farthest_corner] ->
                    true

                  {:circle, [r]} ->
                    is_number(r) and r >= 0 and
                      match?({:ok, _}, HtmlValidator.normalize_geometry_number(r))

                  {:ellipse, [a, b]} ->
                    CssLengthValidator.valid_edge?(a, :padding) and
                      CssLengthValidator.valid_edge?(b, :padding)

                  _ ->
                    false
                end

              radius? and
                NativeElixirPdfUtilities.Validators.CssPositionValidator.valid_prepared?(x) and
                NativeElixirPdfUtilities.Validators.CssPositionValidator.valid_prepared?(y)

            _ ->
              false
          end

        geometry? and space in @spaces and hue in [:shorter, :longer, :increasing, :decreasing] and
          is_boolean(repeating?) and
          Enum.all?(stops, fn stop ->
            case stop do
              %{color: color, position: p} when map_size(stop) == 2 ->
                CssColorValidator.valid_prepared?(color) and position?.(p)

              %{hint: p} when map_size(stop) == 1 ->
                p != nil and position?.(p)

              _ ->
                false
            end
          end) and Map.has_key?(hd(stops), :color) and Map.has_key?(List.last(stops), :color) and
          not Enum.any?(Enum.chunk_every(stops, 2, 1, :discard), fn [a, b] ->
            Map.has_key?(a, :hint) and Map.has_key?(b, :hint)
          end)

      _ ->
        false
    end
  end

  @doc "Resolves stop positions and fixes decreasing and unspecified positions against the gradient line."
  @spec used_stops(map(), number(), map()) :: {:ok, [map()]} | {:error, term()}
  def used_stops(gradient, basis, style) do
    values =
      Enum.map(gradient.stops, fn stop ->
        key = if Map.has_key?(stop, :hint), do: :hint, else: :position

        resolved =
          case Map.fetch!(stop, key) do
            nil ->
              {:ok, nil}

            {:fraction, value} ->
              {:ok, value}

            value ->
              case CssLengthValidator.resolve(value, basis, style) do
                {:ok, value} when is_number(value) ->
                  {:ok, if(basis == 0, do: 0.0, else: value / basis)}

                error ->
                  error
              end
          end

        case resolved do
          {:ok, value} -> {:ok, Map.put(stop, key, value)}
          error -> error
        end
      end)

    case Enum.find(values, &(not match?({:ok, _}, &1))) do
      nil ->
        values =
          Enum.map(values, &elem(&1, 1))
          |> List.update_at(0, fn stop -> Map.update!(stop, :position, &(&1 || 0.0)) end)
          |> List.update_at(-1, fn stop -> Map.update!(stop, :position, &(&1 || 1.0)) end)

        HtmlValidator.reserve_render_resource(:max_work, 6 * length(values), :layout)

        {values, _} =
          Enum.map_reduce(values, nil, fn stop, previous ->
            key = if Map.has_key?(stop, :hint), do: :hint, else: :position

            case Map.fetch!(stop, key) do
              nil ->
                {stop, previous}

              value ->
                value = if previous == nil, do: value, else: max(previous, value)
                {Map.put(stop, key, value), value}
            end
          end)

        colors = Enum.filter(values, &Map.has_key?(&1, :color))

        {colors, [], _} =
          Enum.reduce(colors, {[], [], nil}, fn stop, {finished, pending, left} ->
            if stop.position == nil do
              {finished, [stop | pending], left}
            else
              intervals = length(pending) + 1

              {finished, _} =
                pending
                |> Enum.reverse()
                |> Enum.reduce({finished, 1}, fn missing, {out, index} ->
                  filled = %{
                    missing
                    | position: left + (stop.position - left) * index / intervals
                  }

                  {[filled | out], index + 1}
                end)

              {[stop | finished], [], stop.position}
            end
          end)

        {values, []} =
          Enum.map_reduce(values, Enum.reverse(colors), fn stop, remaining ->
            case stop do
              %{hint: _} ->
                {stop, remaining}

              _ ->
                [color | rest] = remaining
                {color, rest}
            end
          end)

        {:ok, values}

      error ->
        error
    end
  end

  @doc "Admits a gradient raster through the existing shared raster and decoded-image budgets."
  @spec raster_plan(number(), number(), non_neg_integer()) ::
          {:ok, {pos_integer(), pos_integer()}} | {:error, term()}
  def raster_plan(width, height, stops) do
    scale = Limits.get(:svg_raster_scale) / 0.75
    w = max(ceil(width * scale), 1)
    h = max(ceil(height * scale), 1)

    with :ok <-
           NativeElixirPdfUtilities.Validators.CssImageValueValidator.reserve_surface(
             w,
             h,
             w * h * (stops + 1)
           ),
         do: {:ok, {w, h}},
         else: ({:error, {reason, diagnostic}} -> raster_failure(reason, diagnostic))
  catch
    {:render_resource_limit, {:error, {reason, diagnostic}}} -> raster_failure(reason, diagnostic)
  end

  defp raster_failure(reason, diagnostic) do
    Diagnostics.error(diagnostic.stage, reason, "CSS gradient paint: " <> diagnostic.message,
      module: __MODULE__,
      source: "CSS gradient",
      operation: :rasterize_gradient,
      line: Map.get(diagnostic, :line),
      column: Map.get(diagnostic, :column)
    )
  end
end
