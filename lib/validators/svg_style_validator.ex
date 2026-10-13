defmodule NativeElixirPdfUtilities.Validators.SvgStyleValidator do
  @moduledoc false
  import Kernel, except: [length: 1]
  alias NativeElixirPdfUtilities.Diagnostics
  alias NativeElixirPdfUtilities.Validators.HtmlValidator

  @inherited %{
    svg_text_anchor: "start",
    svg_color_interpolation: "sRGB",
    svg_color_interpolation_filters: "linearRGB",
    svg_fill: {:color, {0, 0, 0}},
    svg_stroke: :none,
    svg_fill_rule: :nonzero,
    svg_clip_rule: :nonzero,
    svg_fill_opacity: 1.0,
    svg_stroke_opacity: 1.0,
    svg_stroke_width: {:user, 1.0},
    svg_visibility: :visible,
    svg_stroke_linecap: "butt",
    svg_stroke_linejoin: "miter",
    svg_stroke_miterlimit: 4.0,
    svg_stroke_dasharray: :none,
    svg_stroke_dashoffset: {:user, 0.0},
    svg_shape_rendering: "auto",
    svg_paint_order: "normal",
    svg_marker_start: :none,
    svg_marker_mid: :none,
    svg_marker_end: :none
  }
  @properties %{
    "text-anchor" => :svg_text_anchor,
    "color-interpolation" => :svg_color_interpolation,
    "stop-color" => :svg_stop_color,
    "stop-opacity" => :svg_stop_opacity,
    "fill" => :svg_fill,
    "stroke" => :svg_stroke,
    "fill-rule" => :svg_fill_rule,
    "clip-rule" => :svg_clip_rule,
    "clip-path" => :svg_clip_path,
    "fill-opacity" => :svg_fill_opacity,
    "stroke-opacity" => :svg_stroke_opacity,
    "stroke-width" => :svg_stroke_width,
    "visibility" => :svg_visibility,
    "opacity" => :svg_opacity,
    "x" => :svg_x,
    "y" => :svg_y,
    "cx" => :svg_cx,
    "cy" => :svg_cy,
    "r" => :svg_r,
    "rx" => :svg_rx,
    "ry" => :svg_ry,
    "width" => :svg_width,
    "height" => :svg_height,
    "transform" => :svg_transform,
    "stroke-linecap" => :svg_stroke_linecap,
    "stroke-linejoin" => :svg_stroke_linejoin,
    "stroke-miterlimit" => :svg_stroke_miterlimit,
    "stroke-dasharray" => :svg_stroke_dasharray,
    "stroke-dashoffset" => :svg_stroke_dashoffset,
    "shape-rendering" => :svg_shape_rendering,
    "paint-order" => :svg_paint_order,
    "marker-start" => :svg_marker_start,
    "marker-mid" => :svg_marker_mid,
    "marker-end" => :svg_marker_end,
    "mask" => :svg_mask,
    "mask-type" => :svg_mask_type,
    "filter" => :svg_filter,
    "color-interpolation-filters" => :svg_color_interpolation_filters,
    "flood-color" => :svg_flood_color,
    "flood-opacity" => :svg_flood_opacity,
    "lighting-color" => :svg_lighting_color
  }
  @geometry ~w(x y cx cy r rx ry width height)
  @geometry_elements %{
    "x" => ~w(svg rect image use symbol foreignObject),
    "y" => ~w(svg rect image use symbol foreignObject),
    "width" => ~w(svg rect image use symbol foreignObject),
    "height" => ~w(svg rect image use symbol foreignObject),
    "cx" => ~w(circle ellipse),
    "cy" => ~w(circle ellipse),
    "r" => ~w(circle),
    "rx" => ~w(rect ellipse),
    "ry" => ~w(rect ellipse)
  }

  @doc false
  @spec inherit(map()) :: map()
  def inherit(style), do: Map.merge(@inherited, Map.take(style, Map.keys(@inherited)))

  @doc false
  @spec defaults(map()) :: map()
  def defaults(inherited) do
    inherit(inherited)
    |> Map.merge(%{
      _svg_specified: %{},
      svg_stop_color: {:color, {0, 0, 0}},
      svg_stop_opacity: 1.0,
      svg_opacity: 1.0,
      svg_clip_path: :none,
      svg_mask: :none,
      svg_filter: :none,
      svg_flood_color: {:color, {0, 0, 0}},
      svg_flood_opacity: 1.0,
      svg_lighting_color: {:color, {1.0, 1.0, 1.0}},
      svg_mask_type: "luminance",
      svg_width: :auto,
      svg_height: :auto,
      svg_x: {:user, 0.0},
      svg_y: {:user, 0.0},
      svg_cx: {:user, 0.0},
      svg_cy: {:user, 0.0},
      svg_r: {:user, 0.0},
      svg_rx: :auto,
      svg_ry: :auto,
      svg_transform: {1.0, 0.0, 0.0, 1.0, 0.0, 0.0}
    })
    |> Map.put(
      :_svg_inherited,
      Map.merge(
        inherit(inherited),
        Map.get(inherited, :_svg_inherited, parent_properties(inherited))
      )
    )
  end

  @doc false
  @spec parent_properties(map()) :: map()
  def parent_properties(style), do: Map.take(style, Map.values(@properties))

  @doc false
  @spec inherited_property?(atom()) :: boolean()
  def inherited_property?(key), do: Map.has_key?(@inherited, key) or key == :color

  @doc false
  @spec presentation_properties() :: %{String.t() => atom()}
  def presentation_properties, do: @properties

  @doc false
  @spec presentation_property?(map(), String.t()) :: boolean()
  def presentation_property?(node, property) do
    ((Map.has_key?(@properties, property) and
        (property not in @geometry or node.tag in Map.fetch!(@geometry_elements, property))) or
       property in ~w(color display overflow font-family font-size font-weight font-style letter-spacing)) and
      not Map.has_key?(Map.get(node, :attribute_namespaces, %{}), property)
  end

  @doc false
  @spec presentation_declarations(map()) :: {:ok, list()} | {:error, {atom(), map()}}
  def presentation_declarations(node) do
    Enum.reduce_while(node.attributes, {:ok, []}, fn {property, value}, {:ok, declarations} ->
      property =
        if (property == "gradientTransform" and node.tag in ~w(linearGradient radialGradient)) or
             (property == "patternTransform" and node.tag == "pattern"),
           do: "transform",
           else: property

      if presentation_property?(node, property) do
        if String.contains?(value, "!") do
          {:halt, invalid(property, "presentation attributes cannot contain !important")}
        else
          # Geometry presentation attributes accept unitless SVG user units.
          # Their canonical representation is handled by the SVG grammar below.
          value =
            if property == "font-size" do
              case HtmlValidator.parse_css_number(value) do
                {_, ""} -> value <> "px"
                _ -> value
              end
            else
              value
            end

          {:cont, {:ok, [{property, value} | declarations]}}
        end
      else
        {:cont, {:ok, declarations}}
      end
    end)
  end

  @doc false
  @spec apply(map(), String.t(), String.t()) ::
          {:ok, map()} | :unhandled | {:error, {atom(), map()}}
  def apply(style, property, value) do
    key = Map.get(@properties, property)

    if key do
      normalized = String.trim(value)
      initial = Map.get(defaults(%{}), key, :auto)

      parsed =
        case normalized do
          "initial" ->
            {:ok, initial}

          "inherit" ->
            {:ok, Map.get(style._svg_inherited, key, initial)}

          "unset" ->
            {:ok,
             if(Map.has_key?(@inherited, key),
               do: Map.get(style._svg_inherited, key, initial),
               else: initial
             )}

          _ ->
            parse(property, normalized)
        end

      case parsed do
        {:ok, parsed} -> {:ok, Map.put(style, key, parsed)}
        :error -> invalid(property, "invalid SVG property value #{inspect(value)}")
      end
    else
      :unhandled
    end
  end

  @doc false
  @spec parse(String.t(), term()) :: {:ok, term()} | :error
  def parse(property, value) do
    case {property, value} do
      {"text-anchor", value} ->
        if value in ~w(start middle end), do: {:ok, value}, else: :error

      {property, value} when property in ~w(color-interpolation color-interpolation-filters) ->
        if value in ~w(sRGB auto linearRGB), do: {:ok, value}, else: :error

      {property, "none"}
      when property in ~w(clip-path mask filter marker-start marker-mid marker-end) ->
        {:ok, :none}

      {property, value}
      when property in ~w(clip-path mask filter marker-start marker-mid marker-end) and
             is_binary(value) ->
        case Regex.run(
               ~r/\Aurl\(\s*(#[A-Za-z_][A-Za-z0-9_.:-]*|"#[A-Za-z_][A-Za-z0-9_.:-]*"|'#[A-Za-z_][A-Za-z0-9_.:-]*')\s*\)\z/,
               value
             ) do
          [_, fragment] ->
            {:ok,
             {:fragment,
              fragment |> String.trim("\"") |> String.trim("'") |> String.trim_leading("#")}}

          _ ->
            :error
        end

      {"mask-type", value} ->
        if value in ~w(luminance alpha), do: {:ok, value}, else: :error

      {"offset", value} ->
        case HtmlValidator.parse_css_number(value) do
          {number, ""} -> {:ok, max(0.0, min(number, 1.0))}
          {number, "%"} -> {:ok, max(0.0, min(number / 100, 1.0))}
          _ -> :error
        end

      {property, "url(" <> _ = value} when property in ~w(fill stroke) ->
        case parse("clip-path", value) do
          {:ok, {:fragment, id}} -> {:ok, {:paint_server, id}}
          _ -> :error
        end

      {"stroke-linecap", value} ->
        if value in ~w(butt round square), do: {:ok, value}, else: :error

      {"stroke-linejoin", value} ->
        if value in ~w(miter round bevel), do: {:ok, value}, else: :error

      {"shape-rendering", value} ->
        if value in ~w(auto optimizeSpeed crispEdges geometricPrecision),
          do: {:ok, value},
          else: :error

      {"paint-order", value} ->
        parts = String.split(value)

        if value == "normal" or
             (parts != [] and Enum.all?(parts, &(&1 in ~w(fill stroke markers))) and
                Kernel.length(Enum.uniq(parts)) == Kernel.length(parts)),
           do: {:ok, value},
           else: :error

      {"stroke-miterlimit", value} ->
        case HtmlValidator.parse_css_number(value) do
          {number, ""} when number >= 1 -> {:ok, number}
          _ -> :error
        end

      {"stroke-dashoffset", value} ->
        length(value, true)

      {"stroke-dasharray", "none"} ->
        {:ok, :none}

      {"stroke-dasharray", value} when is_binary(value) ->
        parts = String.split(value, ~r/[\s,]+/, trim: true)
        parsed = Enum.map(parts, &length/1)

        if parts != [] and not Regex.match?(~r/\A,|,\z|,\s*,/, value) and
             Enum.all?(parsed, &match?({:ok, _}, &1)),
           do: {:ok, Enum.map(parsed, fn {:ok, length} -> length end)},
           else: :error

      {"transform", value} ->
        NativeElixirPdfUtilities.Validators.SvgTransformValidator.parse(value)

      {property, value}
      when property in ~w(fill stroke stop-color flood-color lighting-color) and is_binary(value) ->
        case String.downcase(value, :ascii) do
          "none" ->
            {:ok, :none}

          "currentcolor" ->
            {:ok, :current_color}

          _ ->
            case HtmlValidator.parse_css_color(value, {0, 0, 0}) do
              {:ok, color} -> {:ok, {:color, color}}
              _ -> :error
            end
        end

      {property, value} when property in ~w(fill-rule clip-rule) ->
        case value do
          "nonzero" -> {:ok, :nonzero}
          "evenodd" -> {:ok, :evenodd}
          _ -> :error
        end

      {"visibility", value} ->
        case value do
          "visible" -> {:ok, :visible}
          "hidden" -> {:ok, :hidden}
          "collapse" -> {:ok, :collapse}
          _ -> :error
        end

      {property, value}
      when property in ~w(opacity fill-opacity stroke-opacity stop-opacity flood-opacity) ->
        case HtmlValidator.parse_css_number(value) do
          {number, ""} -> {:ok, max(0.0, min(number, 1.0))}
          {number, "%"} -> {:ok, max(0.0, min(number / 100, 1.0))}
          _ -> :error
        end

      {property, value} when property in @geometry or property == "stroke-width" ->
        if value == "auto" and property in ~w(width height rx ry) do
          {:ok, :auto}
        else
          length(value, property in ~w(x y cx cy))
        end

      _ ->
        :error
    end
  end

  @doc false
  @spec length(term(), boolean()) :: {:ok, term()} | :error
  def length(value, signed? \\ false) do
    case is_binary(value) && HtmlValidator.parse_css_number(String.trim(value)) do
      {number, unit} when signed? or number >= 0 ->
        case unit do
          "" ->
            {:ok, {:user, number}}

          "%" ->
            {:ok, {:percent, number / 100}}

          "em" ->
            {:ok, {:svg_em, number}}

          "rem" ->
            {:ok, {:svg_rem, number}}

          unit when unit in ~w(px pt pc mm cm in q) ->
            # Shared CSS number parsing owns numeric magnitude limits. Absolute
            # CSS lengths are points, while SVG user coordinates remain unitless.
            scale =
              case unit do
                "px" -> 0.75
                "pt" -> 1.0
                "pc" -> 12.0
                "mm" -> 72.0 / 25.4
                "cm" -> 72.0 / 2.54
                "in" -> 72.0
                "q" -> 72.0 / 101.6
              end

            case HtmlValidator.parse_css_number(String.trim(value), scale) do
              {points, ^unit} -> {:ok, {:points, points}}
              _ -> :error
            end

          _ ->
            :error
        end

      _ ->
        :error
    end
  end

  defp invalid(property, message) do
    Diagnostics.error(:css, :invalid_css, message,
      module: __MODULE__,
      operation: :prepare,
      source: property
    )
  end
end
