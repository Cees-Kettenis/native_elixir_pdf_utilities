defmodule NativeElixirPdfUtilities.Validators.CssImageValueValidator do
  @moduledoc "Static CSS image values and deterministic source selection without loading resources."
  alias NativeElixirPdfUtilities.Validators.{
    HtmlValidator,
    CssGradientValidator,
    CssColorValidator,
    CssMathValidator,
    CssSyntaxValidator,
    ResponsiveImageValidator
  }

  @doc "Prepares URL, gradient, image-set and static image fallback values using the existing reference grammar."
  @spec prepare(String.t(), map()) :: {:ok, map() | nil} | :error | {:error, term()}
  def prepare(source, style) do
    prepare_value(String.trim(source), style, 0)
  end

  defp prepare_value(source, style, depth) do
    HtmlValidator.check_render_resource(:max_depth, depth, :style)
    HtmlValidator.reserve_render_resource(:max_work, byte_size(source), :style)

    case HtmlValidator.parse_background_image_reference(source) do
      {:ok, nil} ->
        {:ok, nil}

      {:ok, reference} ->
        {:ok, %{kind: :url, source: reference}}

      {:error, {_, _}} = error ->
        error

      _ ->
        case HtmlValidator.parse_css_identifier(source) do
          {:ok, name, "(" <> rest} ->
            if String.ends_with?(rest, ")") do
              body = binary_part(rest, 0, byte_size(rest) - 1)

              case String.downcase(name, :ascii) do
                name
                when name in ~w(linear-gradient repeating-linear-gradient radial-gradient repeating-radial-gradient conic-gradient repeating-conic-gradient) ->
                  CssGradientValidator.prepare(source, style)

                name when name in ~w(image-set -webkit-image-set) ->
                  image_set(body, style, depth + 1)

                "image" ->
                  fallback_image(body, style, depth + 1)

                "cross-fade" ->
                  cross_fade(body, style, depth + 1)

                "element" ->
                  case HtmlValidator.parse_css_identifier(
                         String.trim_leading(String.trim(body), "#")
                       ) do
                    {:ok, id, ""} ->
                      if String.starts_with?(String.trim(body), "#"),
                        do: {:ok, %{kind: :element, id: id}},
                        else: :error

                    _ ->
                      :error
                  end

                _ ->
                  :error
              end
            else
              :error
            end

          _ ->
            :error
        end
    end
  end

  defp cross_fade(body, style, depth) do
    parts = CssSyntaxValidator.split_trimmed(body, ",")
    HtmlValidator.reserve_render_resource(:max_entries, length(parts), :style)

    Enum.reduce_while(parts, {:ok, []}, fn part, {:ok, acc} ->
      items = HtmlValidator.split_css_tokens(part)

      {operand, weight} =
        case items do
          [operand] ->
            {operand, nil}

          [source, weight] ->
            if String.ends_with?(source, "%"), do: {weight, source}, else: {source, weight}

          _ ->
            {"", "invalid"}
        end

      weight =
        case weight do
          nil ->
            {:ok, nil}

          value ->
            case CssMathValidator.evaluate(value, %{}) do
              {:ok, number, %{percentage: 1}}
              when is_number(number) and number >= 0 and number <= 100 ->
                {:ok, number / 100}

              _ ->
                :error
            end
        end

      selected =
        case CssColorValidator.prepare(operand, style) do
          {:ok, color} -> {:ok, %{kind: :solid, color: color}}
          :error -> prepare_value(operand, style, depth)
        end

      case {selected, weight} do
        {{:ok, image}, {:ok, weight}} when is_map(image) ->
          {:cont, {:ok, [{image, weight} | acc]}}

        {{:error, _} = error, _} ->
          {:halt, error}

        _ ->
          {:halt, :error}
      end
    end)
    |> case do
      {:ok, values} ->
        {sum, omitted} =
          Enum.reduce(values, {0.0, 0}, fn {_, weight}, {sum, missing} ->
            if is_nil(weight), do: {sum, missing + 1}, else: {sum + weight, missing}
          end)

        share = if omitted > 0, do: max(0.0, 1.0 - sum) / omitted, else: 0.0

        values =
          Enum.reverse(values, [])
          |> Enum.map(fn {image, weight} -> {image, (weight || share) / max(1.0, sum)} end)

        {:ok, %{kind: :cross_fade, values: values}}

      error ->
        error
    end
  end

  defp image_set(body, style, depth) do
    candidates = HtmlValidator.split_css_comma(body, true)
    HtmlValidator.reserve_render_resource(:max_entries, length(candidates), :style)

    Enum.reduce_while(candidates, {:ok, []}, fn source, {:ok, prepared} ->
      case HtmlValidator.split_css_tokens(source) do
        [image | descriptors] when length(descriptors) <= 2 ->
          image = url_token(image)

          result =
            Enum.reduce_while(descriptors, {:ok, %{density: nil, type: nil}}, fn token,
                                                                                 {:ok, fields} ->
              case HtmlValidator.parse_css_identifier(token) do
                {:ok, name, "(" <> rest} when is_binary(name) ->
                  string =
                    if String.ends_with?(rest, ")"),
                      do: binary_part(rest, 0, byte_size(rest) - 1),
                      else: ""

                  if String.downcase(name, :ascii) == "type" and fields.type == nil and
                       Regex.match?(~r/\A(?:#{HtmlValidator.css_string_pattern()})\z/us, string),
                     do:
                       {:cont,
                        {:ok,
                         %{
                           fields
                           | type:
                               HtmlValidator.decode_css_string(string) |> String.downcase(:ascii)
                         }}},
                     else: {:halt, :error}

                _ ->
                  case resolution(token) do
                    {:ok, density} when fields.density == nil ->
                      {:cont, {:ok, %{fields | density: density}}}

                    _ ->
                      {:halt, :error}
                  end
              end
            end)

          with {:ok, fields} <- result,
               {:ok, image} when not is_nil(image) <- prepare_value(image, style, depth),
               true <- image.kind != :image_set do
            {:cont,
             {:ok,
              [Map.merge(fields, %{image: image, density: fields.density || 1.0}) | prepared]}}
          else
            {:error, _} = error -> {:halt, error}
            _ -> {:halt, :error}
          end

        _ ->
          {:halt, :error}
      end
    end)
    |> case do
      {:ok, [_ | _] = candidates} ->
        {:ok, %{kind: :image_set, candidates: Enum.reverse(candidates)}}

      {:error, _} = error ->
        error

      _ ->
        :error
    end
  end

  defp fallback_image(body, style, depth) do
    parts = HtmlValidator.split_css_comma(body, true)

    case parts do
      [first | rest] when length(rest) <= 1 ->
        {direction, first} =
          case HtmlValidator.split_css_tokens(first) do
            [direction | image] ->
              case String.downcase(direction, :ascii) do
                normalized when normalized in ["ltr", "rtl"] ->
                  {String.to_existing_atom(normalized), Enum.join(image, " ")}

                _ ->
                  {nil, first}
              end

            _ ->
              {nil, first}
          end

        first = url_token(first)

        fallback =
          case rest do
            [color] -> CssColorValidator.prepare(color, style)
            [] -> CssColorValidator.prepare(first, style)
          end

        case {rest, fallback} do
          {[], {:ok, color}} ->
            {:ok, %{kind: :solid, color: color, direction: direction}}

          {_, {:ok, color}} ->
            with {:ok, image} when not is_nil(image) <-
                   prepare_fallback_source(first, style, depth),
                 do: {:ok, %{kind: :fallback, image: image, color: color, direction: direction}},
                 else: (
                   {:error, _} = error -> error
                   _ -> :error
                 )

          {[], _} ->
            with {:ok, image} when not is_nil(image) <-
                   prepare_fallback_source(first, style, depth),
                 do: {:ok, %{kind: :fallback, image: image, color: nil, direction: direction}},
                 else: (
                   {:error, _} = error -> error
                   _ -> :error
                 )

          _ ->
            :error
        end

      _ ->
        :error
    end
  end

  defp prepare_fallback_source(source, style, depth) do
    case prepare_value(source, style, depth) do
      {:ok, %{kind: :url, source: url} = image} ->
        case String.split(url, "#xywh=", parts: 2) do
          [_, ""] -> :error
          [reference, box] -> spatial_fragment(image, reference, box)
          [_] -> {:ok, image}
        end

      result ->
        result
    end
  end

  defp spatial_fragment(image, reference, box) do
    {unit, values} =
      case String.split(box, ":", parts: 2) do
        ["percent", values] -> {:percent, values}
        ["pixel", values] -> {:pixel, values}
        [values] -> {:pixel, values}
        _ -> {:pixel, ""}
      end

    case Enum.map(String.split(values, ","), &HtmlValidator.parse_css_number/1) do
      [{x, ""}, {y, ""}, {w, ""}, {h, ""}]
      when reference != "" and x >= 0 and y >= 0 and w > 0 and h > 0 ->
        {:ok, %{image | source: reference} |> Map.put(:spatial_fragment, {unit, x, y, w, h})}

      _ ->
        :error
    end
  end

  defp url_token(source) do
    if Regex.match?(~r/\A(?:#{HtmlValidator.css_string_pattern()})\z/us, source),
      do: "url(" <> source <> ")",
      else: source
  end

  @doc "Prepares static image processing policy without reading image resources."
  @spec prepare_policy(String.t(), String.t()) :: {:ok, map()} | :error
  def prepare_policy(property, source) do
    tokens = source |> String.downcase(:ascii) |> HtmlValidator.split_css_tokens()

    case {property, tokens} do
      {"image-rendering", [value]}
      when value in ~w(auto smooth high-quality crisp-edges pixelated) ->
        value =
          case value do
            "high-quality" -> :high_quality
            "crisp-edges" -> :crisp_edges
            "pixelated" -> :pixelated
            "smooth" -> :smooth
            _ -> :auto
          end

        {:ok, %{image_rendering: value}}

      {"image-orientation", ["from-image"]} ->
        {:ok, %{image_orientation: :from_image}}

      {"image-orientation", ["none"]} ->
        {:ok, %{image_orientation: :none}}

      {"image-orientation", tokens} ->
        {flips, angles} = Enum.split_with(tokens, &(&1 == "flip"))

        angle =
          case angles do
            [] when flips == ["flip"] ->
              {:ok, 0.0}

            [angle] ->
              case CssMathValidator.evaluate_color_component(angle) do
                {:ok, n, %{angle: 1}} when is_number(n) -> {:ok, n}
                {:ok, n, type} when n == 0 and type == %{} -> {:ok, 0.0}
                _ -> :error
              end

            _ ->
              :error
          end

        with true <- length(flips) <= 1,
             {:ok, angle} <- angle,
             do:
               {:ok,
                %{
                  image_orientation:
                    {:turns, Integer.mod(floor(angle / (:math.pi() / 2) + 0.5), 4), flips != []}
                }},
             else: (_ -> :error)

      {"image-resolution", tokens} ->
        Enum.reduce_while(
          tokens,
          {:ok, %{density: nil, from_image?: false, snap?: false}},
          fn token, {:ok, value} ->
            case token do
              "from-image" ->
                if value.from_image?,
                  do: {:halt, :error},
                  else: {:cont, {:ok, %{value | from_image?: true}}}

              "snap" ->
                if value.snap?, do: {:halt, :error}, else: {:cont, {:ok, %{value | snap?: true}}}

              token ->
                case resolution(token) do
                  {:ok, n} when value.density == nil -> {:cont, {:ok, %{value | density: n}}}
                  _ -> {:halt, :error}
                end
            end
          end
        )
        |> case do
          {:ok, value} when value.from_image? or value.density != nil ->
            {:ok, %{image_resolution: %{value | density: value.density || 1.0}}}

          _ ->
            :error
        end

      _ ->
        :error
    end
  end

  @doc "Prepares a positive density in CSS pixels per image pixel from the shared resolution-unit grammar."
  @spec resolution(String.t()) :: {:ok, number()} | :error
  def resolution(source) do
    with {:ok, n, %{resolution: 1}} when is_number(n) and n > 0 <-
           CssMathValidator.evaluate(source, %{}),
         {:ok, n} <- HtmlValidator.normalize_geometry_number(n),
         do: {:ok, n},
         else: (_ -> :error)
  end

  @doc "Checks computed image processing controls at the public styled-tree boundary."
  @spec validate_style(map(), String.t()) :: :ok | {:error, term()}
  def validate_style(style, source) do
    case style do
      style
      when not is_map_key(style, :image_orientation) and not is_map_key(style, :image_resolution) and
             not is_map_key(style, :image_rendering) ->
        :ok

      _ ->
        orientation? =
          case Map.get(style, :image_orientation, :from_image) do
            v when v in [:from_image, :none] -> true
            {:turns, n, flip?} -> n in 0..3 and is_boolean(flip?)
            _ -> false
          end

        resolution? =
          case Map.get(style, :image_resolution) do
            nil ->
              true

            %{density: n, from_image?: from?, snap?: snap?} = value when map_size(value) == 3 ->
              is_number(n) and n > 0 and
                match?({:ok, _}, HtmlValidator.normalize_geometry_number(n)) and
                is_boolean(from?) and is_boolean(snap?)

            _ ->
              false
          end

        if orientation? and resolution? and
             Map.get(style, :image_rendering, :auto) in [
               :auto,
               :smooth,
               :high_quality,
               :crisp_edges,
               :pixelated
             ],
           do: :ok,
           else:
             NativeElixirPdfUtilities.Diagnostics.error(
               :layout,
               :invalid_layout,
               "image policies require a supported orientation, positive resolution and static rendering mode",
               operation: :layout,
               module: __MODULE__,
               source: source
             )
    end
  end

  @doc "Applies image orientation and resolution before natural-size layout, preserving encoded source pixels."
  @spec apply_image(map(), map(), number()) :: {:ok, map()} | {:error, term()}
  def apply_image(image, style, device_density) do
    vector? = Map.has_key?(image, :svg_viewport) or image.format == :pdf_form

    orientation =
      if vector? do
        1
      else
        case Map.get(style, :image_orientation, :from_image) do
          :from_image -> Map.get(image, :exif_orientation, 1)
          :none -> 1
          {:turns, n, false} -> Enum.at([1, 6, 3, 8], n)
          {:turns, n, true} -> Enum.at([2, 5, 4, 7], n)
        end
      end

    previous = Map.get(image, :image_orientation, 1)

    base =
      if previous in 5..8, do: %{image | width: image.height, height: image.width}, else: image

    result =
      case if(vector?, do: nil, else: Map.get(style, :image_resolution)) do
        nil ->
          {:ok, base}

        policy ->
          density =
            if policy.from_image?,
              do: Map.get(image, :resolution_densities, {policy.density, policy.density}),
              else: {policy.density, policy.density}

          {x, y} = density

          {x, y} =
            if policy.snap?,
              do:
                {max(round(x / device_density), 1) * device_density,
                 max(round(y / device_density), 1) * device_density},
              else: {x, y}

          ResponsiveImageValidator.correct_natural_dimensions(
            %{base | width: base.width_px * 0.75, height: base.height_px * 0.75},
            %{density: {x, y}, url: "CSS image-resolution"}
          )
      end

    with {:ok, image} <- result do
      image =
        if orientation in 5..8,
          do: %{image | width: image.height, height: image.width},
          else: image

      image =
        if orientation == 1,
          do: Map.delete(image, :image_orientation),
          else: Map.put(image, :image_orientation, orientation)

      {:ok, apply_rendering(image, style)}
    end
  end

  @doc "Applies the PDF sampling hint to decoded and generated CSS image pixels."
  @spec apply_rendering(map(), map()) :: map()
  def apply_rendering(image, style) do
    case Map.get(style, :image_rendering, :auto) do
      :auto -> image
      value when value in [:smooth, :high_quality] -> Map.put(image, :interpolate, true)
      _ -> Map.put(image, :interpolate, false)
    end
  end

  @doc "Applies computed CSS processing to loaded content, marker, shape and background images."
  @spec apply_style(map(), keyword()) :: {:ok, map()} | {:error, term()}
  def apply_style(style, opts) do
    case style do
      style
      when (not is_map_key(style, :image) or :erlang.map_get(:image, style) == nil) and
             (not is_map_key(style, :background_image) or
                :erlang.map_get(:background_image, style) == nil) and
             (not is_map_key(style, :list_style_image) or
                :erlang.map_get(:list_style_image, style) == nil) and
             (not is_map_key(style, :shape_image) or :erlang.map_get(:shape_image, style) == nil) and
             (not is_map_key(style, :border_image_source) or
                :erlang.map_get(:border_image_source, style) == nil) and
             (not is_map_key(style, :background_images) or
                :erlang.map_get(:background_images, style) == nil) ->
        {:ok, style}

      _ ->
        density = Keyword.get(opts, :image_selection_density, 1)

        Enum.reduce_while(
          [
            :image,
            :background_image,
            :list_style_image,
            :shape_image,
            :background_images,
            :border_image_source
          ],
          {:ok, style},
          fn key, {:ok, out} ->
            case Map.get(out, key) do
              nil ->
                {:cont, {:ok, out}}

              images when key == :background_images ->
                Enum.reduce_while(images, {:ok, []}, fn value, {:ok, acc} ->
                  result = apply_loaded(value, style, density)

                  case result do
                    {:ok, value} -> {:cont, {:ok, [value | acc]}}
                    error -> {:halt, error}
                  end
                end)
                |> case do
                  {:ok, images} -> {:cont, {:ok, Map.put(out, key, Enum.reverse(images))}}
                  error -> {:halt, error}
                end

              value when key == :border_image_source ->
                case apply_loaded(value, style, density) do
                  {:ok, value} -> {:cont, {:ok, Map.put(out, key, value)}}
                  error -> {:halt, error}
                end

              image ->
                case apply_image(image, style, density) do
                  {:ok, image} -> {:cont, {:ok, Map.put(out, key, image)}}
                  error -> {:halt, error}
                end
            end
          end
        )
    end
  end

  defp apply_loaded(value, style, density) do
    case value do
      %{kind: :cross_fade, values: values} ->
        map_blend(values, value, &apply_loaded(&1, style, density))

      %{kind: :decoded, image: image} ->
        with {:ok, image} <- apply_image(image, style, density),
             do: {:ok, %{value | image: image}}

      _ ->
        {:ok, value}
    end
  end

  @doc "Applies a preparation step to each cross-fade image while retaining normalized weights."
  @spec map_blend(list(), map(), (map() -> term())) :: {:ok, map()} | :error | {:error, term()}
  def map_blend(values, image, prepare) do
    Enum.reduce_while(values, {:ok, []}, fn {value, weight}, {:ok, acc} ->
      case prepare.(value) do
        {:ok, prepared} -> {:cont, {:ok, [{prepared, weight} | acc]}}
        error -> {:halt, error}
      end
    end)
    |> case do
      {:ok, values} -> {:ok, %{image | values: Enum.reverse(values)}}
      error -> error
    end
  end

  @doc "Computes selected-font CSS units without choosing or loading any image source."
  @spec compute(map() | nil, map()) :: {:ok, map() | nil} | :error | {:error, term()}
  def compute(image, style) do
    case image do
      %{kind: :cross_fade, values: values} ->
        map_blend(values, image, &compute(&1, style))

      %{kind: :gradient} ->
        CssGradientValidator.compute(image, style)

      %{kind: :fallback, image: source} ->
        with {:ok, source} <- compute(source, style), do: {:ok, %{image | image: source}}

      %{kind: :image_set, candidates: candidates} ->
        Enum.reduce_while(candidates, {:ok, []}, fn candidate, {:ok, out} ->
          case compute(candidate.image, style) do
            {:ok, value} -> {:cont, {:ok, [%{candidate | image: value} | out]}}
            error -> {:halt, error}
          end
        end)
        |> case do
          {:ok, candidates} -> {:ok, %{image | candidates: Enum.reverse(candidates)}}
          error -> error
        end

      _ ->
        {:ok, image}
    end
  end

  @doc "Checks a loaded CSS image value before layout consumes its representation."
  @spec valid_loaded?(term()) :: boolean()
  def valid_loaded?(image) do
    case image do
      nil ->
        true

      %{kind: :decoded, image: %{width: width, height: height} = decoded} ->
        NativeElixirPdfUtilities.Validators.WriterValidator.valid_image?(decoded) and
          is_number(width) and width >= 0 and is_number(height) and height >= 0

      %{kind: :cross_fade, values: values} ->
        values != [] and
          Enum.all?(values, fn {image, weight} ->
            is_number(weight) and weight >= 0 and weight <= 1 and valid_loaded?(image)
          end)

      %{kind: :solid, color: color} ->
        CssColorValidator.valid_prepared?(color)

      %{kind: :gradient} ->
        CssGradientValidator.valid_prepared?(image)

      _ ->
        false
    end
  end

  @doc "Reserves a generated CSS surface using the existing raster, image and work budgets."
  @spec reserve_surface(pos_integer(), pos_integer(), non_neg_integer()) :: :ok | {:error, term()}
  def reserve_surface(width, height, work) do
    context = NativeElixirPdfUtilities.HtmlToPdf.ResourceContext.current()
    budget = if context == nil, do: nil, else: context.image_budget

    with :ok <-
           NativeElixirPdfUtilities.Validators.SvgValidator.validate_raster_dimensions(
             width,
             height,
             budget
           ),
         do: HtmlValidator.reserve_render_resource(:max_raster_work, work, :layout)
  end

  @doc "Selects CSS alternatives with the same density policy as responsive HTML images."
  @spec select(map() | nil, number()) :: map() | nil
  def select(image, density) do
    case image do
      %{kind: :image_set, candidates: candidates} ->
        candidates =
          Enum.filter(
            candidates,
            &(&1.type == nil or &1.type in ~w(image/png image/jpeg image/svg+xml))
          )

        case ResponsiveImageValidator.choose_density(candidates, density) do
          nil ->
            nil

          candidate ->
            candidate.image
            |> select(density)
            |> then(fn image ->
              if image == nil, do: nil, else: Map.put(image, :density, candidate.density)
            end)
        end

      image ->
        image
    end
  end
end
