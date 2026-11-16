defmodule NativeElixirPdfUtilities.Validators.CssShapeValidator do
  @moduledoc "Owns CSS flow-shape syntax, computed lengths and image admission."
  alias NativeElixirPdfUtilities.Diagnostics

  alias NativeElixirPdfUtilities.Validators.{
    HtmlValidator,
    CssLengthValidator,
    CssMathValidator,
    CssValueValidator,
    CssSyntaxValidator,
    WriterValidator
  }

  @boxes ~w(margin-box border-box padding-box content-box)

  @doc "Parses flow shapes, shape margins and alpha thresholds without fetching resources."
  @spec prepare(String.t(), String.t()) :: {:ok, map()} | :error | {:error, term()}
  def prepare(property, value) do
    value = String.trim(value)

    case property do
      "shape-margin" ->
        with {:ok, length} <- CssLengthValidator.prepare(value, :padding),
             do: {:ok, %{shape_margin: length}}

      "shape-image-threshold" ->
        case HtmlValidator.parse_css_number(value) do
          {number, ""} -> {:ok, %{shape_image_threshold: min(max(number, 0.0), 1.0)}}
          {number, "%"} -> {:ok, %{shape_image_threshold: min(max(number / 100, 0.0), 1.0)}}
          _ -> :error
        end

      property when property in ["shape-outside", "shape-inside"] ->
        key = if property == "shape-outside", do: :shape_outside, else: :shape_inside

        case String.downcase(value, :ascii) do
          "none" when key == :shape_outside ->
            {:ok, %{key => :none, shape_image_source: nil, shape_image: nil}}

          "auto" when key == :shape_inside ->
            {:ok, %{key => :auto}}

          _ ->
            case HtmlValidator.parse_background_image_reference(value) do
              {:ok, reference} when is_binary(reference) and property == "shape-outside" ->
                {:ok, %{key => :image, shape_image_source: reference}}

              _ ->
                with {:ok, shape} <- shape(value) do
                  explicit_box? =
                    Enum.any?(HtmlValidator.split_css_tokens(value), &(&1 in @boxes))

                  shape =
                    if key == :shape_inside and not explicit_box?,
                      do: %{shape | reference: :content_box},
                      else: shape

                  {:ok, %{key => shape}}
                end
            end
        end

      _ ->
        :error
    end
  end

  defp shape(value) do
    tokens = HtmlValidator.split_css_tokens(value)
    boxes = Enum.filter(tokens, &(String.downcase(&1, :ascii) in @boxes))

    box =
      case boxes do
        [] -> :margin_box
        [box] -> String.to_existing_atom(String.replace(String.downcase(box, :ascii), "-", "_"))
        _ -> nil
      end

    body = Enum.reject(tokens, &(&1 in boxes))

    case {box, body} do
      {box, []} when not is_nil(box) and boxes != [] ->
        {:ok, %{type: :box, reference: box}}

      {box, [body]} when not is_nil(box) ->
        with {:ok, name, "(" <> arguments} <- HtmlValidator.parse_css_identifier(body),
             true <- String.ends_with?(arguments, ")"),
             arguments = binary_part(arguments, 0, byte_size(arguments) - 1),
             {:ok, shape} <- basic(String.downcase(name, :ascii), arguments),
             do: {:ok, Map.put(shape, :reference, box)},
             else: (_ -> :error)

      _ ->
        :error
    end
  end

  defp basic(kind, arguments) do
    case kind do
      kind when kind in ["circle", "ellipse"] ->
        tokens = HtmlValidator.split_css_tokens(arguments)

        {radii, position} =
          case Enum.split_while(tokens, &(&1 != "at")) do
            {radii, []} -> {radii, "center"}
            {radii, ["at" | position]} -> {radii, Enum.join(position, " ")}
          end

        radii =
          if radii == [],
            do: List.duplicate("closest-side", if(kind == "circle", do: 1, else: 2)),
            else: radii

        parsed =
          Enum.map(radii, fn value ->
            case value do
              "closest-side" -> {:ok, :closest_side}
              "farthest-side" -> {:ok, :farthest_side}
              _ -> CssLengthValidator.prepare(value, :padding)
            end
          end)

        with true <- length(radii) == if(kind == "circle", do: 1, else: 2),
             true <- Enum.all?(parsed, &match?({:ok, _}, &1)),
             {:ok, position} <- CssValueValidator.parse_two_axis_position(position, nil, true),
             do:
               {:ok,
                %{
                  type: if(kind == "circle", do: :circle, else: :ellipse),
                  radii: Enum.map(parsed, &elem(&1, 1)),
                  position: position
                }},
             else: (_ -> :error)

      "inset" ->
        tokens = HtmlValidator.split_css_tokens(arguments)

        {offsets, radii} =
          case Enum.split_while(tokens, &(&1 != "round")) do
            {offsets, []} -> {offsets, "0"}
            {offsets, ["round" | radii]} -> {offsets, Enum.join(radii, " ")}
          end

        with {:ok, offsets} <-
               HtmlValidator.parse_css_box_lengths(Enum.join(offsets, " "), {:computed, :margin}),
             false <- Enum.any?(offsets, fn {_, value} -> value == :auto end),
             {:ok, rx, ry} <- corner_radii(radii),
             do: {:ok, %{type: :inset, offsets: offsets, rx: rx, ry: ry}},
             else: (_ -> :error)

      "polygon" ->
        parts = CssSyntaxValidator.split(arguments, ",") |> Enum.map(&String.trim/1)

        {rule, parts} =
          case parts do
            [rule | parts] when rule in ["evenodd", "nonzero"] -> {rule, parts}
            _ -> {"nonzero", parts}
          end

        points =
          Enum.map(parts, fn part ->
            case HtmlValidator.split_css_tokens(part) do
              [x, y] ->
                with {:ok, x} <- CssLengthValidator.prepare(x, :margin),
                     {:ok, y} <- CssLengthValidator.prepare(y, :margin),
                     false <- x == :auto or y == :auto,
                     do: {:ok, {x, y}},
                     else: (_ -> :error)

              _ ->
                :error
            end
          end)

        if length(points) >= 3 and Enum.all?(points, &match?({:ok, _}, &1)),
          do:
            {:ok,
             %{
               type: :polygon,
               points: Enum.map(points, &elem(&1, 1)),
               rule: if(rule == "evenodd", do: :evenodd, else: :nonzero)
             }},
          else: :error

      _ ->
        :error
    end
  end

  defp corner_radii(value) do
    case CssSyntaxValidator.split(value, "/") do
      [x] ->
        with {:ok, radii} <- HtmlValidator.parse_css_box_lengths(x, {:computed, :padding}),
             do: {:ok, radii, radii}

      [x, y] ->
        with {:ok, rx} <- HtmlValidator.parse_css_box_lengths(x, {:computed, :padding}),
             {:ok, ry} <- HtmlValidator.parse_css_box_lengths(y, {:computed, :padding}),
             do: {:ok, rx, ry}

      _ ->
        :error
    end
  end

  @doc "Captures font-dependent units in shape geometry before layout selects reference boxes."
  @spec compute_style(map()) :: {:ok, map()} | {:error, term()} | :error
  def compute_style(style) do
    case style do
      style
      when not is_map_key(style, :shape_outside) and
             not is_map_key(style, :shape_inside) and
             not is_map_key(style, :shape_margin) ->
        {:ok, style}

      _ ->
        Enum.reduce_while([:shape_outside, :shape_inside, :shape_margin], {:ok, style}, fn key,
                                                                                           {:ok,
                                                                                            result} ->
          if Map.has_key?(result, key) do
            case compute(Map.fetch!(result, key), style) do
              {:ok, value} -> {:cont, {:ok, Map.put(result, key, value)}}
              error -> {:halt, error}
            end
          else
            {:cont, {:ok, result}}
          end
        end)
    end
  end

  defp compute(value, style) do
    case value do
      {:css_length_math, _} ->
        CssLengthValidator.resolve(value, nil, style)

      {:css_position_math, source} when is_binary(source) ->
        with {:ok, calculation} <- CssMathValidator.compute_length(source, style),
             do: {:ok, {:css_position_math, calculation}}

      {:rem, ratio} ->
        {:ok, ratio * Map.fetch!(style, :_root_font_size)}

      values when is_map(values) ->
        Enum.reduce_while(values, {:ok, %{}}, fn {key, value}, {:ok, result} ->
          case compute(value, style) do
            {:ok, value} -> {:cont, {:ok, Map.put(result, key, value)}}
            error -> {:halt, error}
          end
        end)

      values when is_list(values) or is_tuple(values) ->
        items = if is_tuple(values), do: Tuple.to_list(values), else: values

        Enum.reduce_while(items, {:ok, []}, fn value, {:ok, result} ->
          case compute(value, style) do
            {:ok, value} -> {:cont, {:ok, [value | result]}}
            error -> {:halt, error}
          end
        end)
        |> case do
          {:ok, result} ->
            {:ok,
             if(is_tuple(values),
               do: result |> Enum.reverse() |> List.to_tuple(),
               else: Enum.reverse(result)
             )}

          error ->
            error
        end

      _ ->
        {:ok, value}
    end
  end

  @doc "Checks computed flow shapes and delegates decoded image validity to the writer's shared validator."
  @spec validate_style(map(), String.t()) :: :ok | {:error, term()}
  def validate_style(style, source) do
    case style do
      style
      when not is_map_key(style, :shape_outside) and
             not is_map_key(style, :shape_inside) and
             not is_map_key(style, :shape_margin) and
             not is_map_key(style, :shape_image_threshold) and
             not is_map_key(style, :shape_image) and
             not is_map_key(style, :shape_image_source) ->
        :ok

      _ ->
        HtmlValidator.with_render_budget(fn ->
          image = Map.get(style, :shape_image)

          valid_image? =
            image == nil or
              (is_map(image) and Map.get(image, :format) in [:png, :jpeg] and
                 WriterValidator.valid_image?(image))

          valid? =
            valid_shape?(Map.get(style, :shape_outside, :none)) and
              valid_shape?(Map.get(style, :shape_inside, :auto)) and
              Map.get(style, :shape_outside, :none) != :auto and
              Map.get(style, :shape_inside, :auto) not in [:none, :image] and
              (Map.get(style, :shape_outside) != :image or image != nil or
                 Map.get(style, :display) == :none) and
              CssLengthValidator.valid_edge?(Map.get(style, :shape_margin, 0.0), :padding) and
              is_number(Map.get(style, :shape_image_threshold, 0.0)) and
              Map.get(style, :shape_image_threshold, 0.0) >= 0.0 and
              Map.get(style, :shape_image_threshold, 0.0) <= 1.0

          if valid? and valid_image?,
            do: :ok,
            else:
              Diagnostics.error(
                :layout,
                :invalid_layout,
                "flow shapes require validated basic geometry, a nonnegative margin and an alpha threshold from zero to one",
                operation: :layout,
                module: __MODULE__,
                source: source
              )
        end)
    end
  end

  defp valid_shape?(value) do
    valid_length? = &CssLengthValidator.valid_edge?(&1, :margin)
    valid_nonnegative? = &CssLengthValidator.valid_edge?(&1, :padding)

    case value do
      value when value in [:none, :auto, :image] ->
        true

      %{reference: box} = shape
      when box in [:margin_box, :border_box, :padding_box, :content_box] ->
        case shape do
          %{type: :box} ->
            true

          %{type: type, radii: radii, position: {x, y}}
          when type in [:circle, :ellipse] and is_list(radii) ->
            length(radii) == if(type == :circle, do: 1, else: 2) and
              Enum.all?(
                radii,
                &(&1 in [:closest_side, :farthest_side] or valid_nonnegative?.(&1))
              ) and
              NativeElixirPdfUtilities.Validators.CssPositionValidator.valid_prepared?(x) and
              NativeElixirPdfUtilities.Validators.CssPositionValidator.valid_prepared?(y)

          %{type: :inset, offsets: offsets, rx: rx, ry: ry} ->
            Enum.all?(
              [offsets, rx, ry],
              &(is_map(&1) and Enum.sort(Map.keys(&1)) == [:bottom, :left, :right, :top])
            ) and
              Enum.all?(offsets, fn {_, value} -> valid_length?.(value) end) and
              Enum.all?([rx, ry], &Enum.all?(&1, fn {_, value} -> valid_nonnegative?.(value) end))

          %{type: :polygon, points: points, rule: rule}
          when is_list(points) and rule in [:evenodd, :nonzero] ->
            HtmlValidator.reserve_render_resource(:max_work, length(points), :layout)
            HtmlValidator.check_render_resource(:max_entries, length(points), :layout)

            length(points) >= 3 and
              Enum.all?(points, fn point ->
                case point do
                  {x, y} -> valid_length?.(x) and valid_length?.(y)
                  _ -> false
                end
              end)

          _ ->
            false
        end

      _ ->
        false
    end
  end
end
