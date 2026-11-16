defmodule NativeElixirPdfUtilities.Validators.CssBackgroundValidator do
  @moduledoc "Layered CSS background grammar and shorthand reset authority."
  alias NativeElixirPdfUtilities.Validators.{
    HtmlValidator,
    CssValueValidator,
    CssImageValueValidator,
    CssColorValidator,
    CssMathValidator
  }

  @plural %{
    "background-image" => :background_images,
    "background-position" => :background_positions,
    "background-size" => :background_sizes,
    "background-repeat" => :background_repeats,
    "background-origin" => :background_origins,
    "background-clip" => :background_clips,
    "background-attachment" => :background_attachments
  }
  @legacy %{
    "background-position" => :background_position,
    "background-size" => :background_size,
    "background-repeat" => :background_repeat
  }
  @defaults %{
    background_image: nil,
    background_color: nil,
    background_position: {{:percent, 0.0}, {:percent, 0.0}},
    background_size: {:auto, :auto},
    background_repeat: :repeat
  }

  @doc "Lists the background declarations owned by this validator."
  @spec properties() :: [String.t()]
  def properties, do: ["background" | Map.keys(@plural)]

  @doc "Prepares layer longhands or the complete shorthand while preserving the single-image representation."
  @spec prepare(map(), String.t(), String.t()) :: {:ok, map()} | :error | {:error, term()}
  def prepare(style, property, value) do
    HtmlValidator.reserve_render_resource(:max_work, byte_size(value), :style)
    parts = HtmlValidator.split_css_comma(value, true)
    HtmlValidator.reserve_render_resource(:max_entries, length(parts), :style)

    cond do
      property == "background" ->
        case HtmlValidator.parse_css_background_color(value, style) do
          {:ok, color} ->
            {:ok,
             style
             |> Map.drop(Map.values(@plural) ++ [:background_image_source])
             |> Map.merge(@defaults)
             |> Map.put(:background_color, if(color == :transparent, do: nil, else: color))}

          _ ->
            shorthand(style, parts)
        end

      property == "background-image" ->
        with {:ok, images} <- prepare_list(parts, &CssImageValueValidator.prepare(&1, style)) do
          style = Map.delete(style, :background_image_source)

          case images do
            [nil] ->
              {:ok, style |> Map.delete(:background_images) |> Map.put(:background_image, nil)}

            [%{kind: :url, source: source}] ->
              {:ok,
               style
               |> Map.delete(:background_images)
               |> Map.put(:background_image_source, source)}

            _ ->
              {:ok,
               style |> Map.put(:background_image, nil) |> Map.put(:background_images, images)}
          end
        end

      is_map_key(@plural, property) ->
        with {:ok, values} <- prepare_list(parts, &layer_value(property, &1, style)) do
          case {Map.get(@legacy, property), values} do
            {:background_repeat, [value]} when is_tuple(value) ->
              {:ok,
               style
               |> Map.put(:background_repeat, value)
               |> Map.put(:background_repeats, [value])}

            {key, [value]} when not is_nil(key) ->
              {:ok, style |> Map.put(key, value) |> Map.delete(Map.fetch!(@plural, property))}

            {key, values} ->
              style = Map.put(style, Map.fetch!(@plural, property), values)
              {:ok, if(key == nil, do: style, else: Map.put(style, key, hd(values)))}
          end
        end

      true ->
        :error
    end
  end

  @doc "Returns the background fields preserved when root or body paint propagates to the page canvas."
  @spec canvas_fields() :: [atom()]
  def canvas_fields,
    do: [
      :background_color,
      :background_image,
      :background_position,
      :background_repeat,
      :background_size | Map.values(@plural)
    ]

  @doc "Freezes font-relative layer values before inheritance and root canvas propagation."
  @spec compute_style(map()) :: {:ok, map()} | :error | {:error, term()}
  def compute_style(style) do
    case style do
      style
      when not is_map_key(style, :background_images) and
             not is_map_key(style, :background_positions) and
             not is_map_key(style, :background_sizes) ->
        {:ok, style}

      _ ->
        Enum.reduce_while(
          [:background_images, :background_positions, :background_sizes],
          {:ok, style},
          fn key, {:ok, out} ->
            case Map.fetch(style, key) do
              :error ->
                {:cont, {:ok, out}}

              {:ok, values} ->
                Enum.reduce_while(values, {:ok, []}, fn value, {:ok, prepared} ->
                  result =
                    case {key, value} do
                      {:background_images, image} ->
                        CssImageValueValidator.compute(image, style)

                      {:background_positions, position} ->
                        NativeElixirPdfUtilities.Validators.CssPositionValidator.compute_position(
                          position,
                          style
                        )

                      {:background_sizes, {a, b}} ->
                        with {:ok, a} <-
                               NativeElixirPdfUtilities.Validators.CssSizingValidator.compute_dimension(
                                 a,
                                 style
                               ),
                             {:ok, b} <-
                               NativeElixirPdfUtilities.Validators.CssSizingValidator.compute_dimension(
                                 b,
                                 style
                               ),
                             do: {:ok, {a, b}}

                      _ ->
                        {:ok, value}
                    end

                  case result do
                    {:ok, value} -> {:cont, {:ok, [value | prepared]}}
                    error -> {:halt, error}
                  end
                end)
                |> case do
                  {:ok, values} -> {:cont, {:ok, Map.put(out, key, Enum.reverse(values))}}
                  error -> {:halt, error}
                end
            end
          end
        )
    end
  end

  @doc "Validates prepared background layer lists at the public styled-tree boundary."
  @spec validate_style(map(), String.t()) :: :ok | {:error, term()}
  def validate_style(style, source) do
    case style do
      style
      when not is_map_key(style, :background_images) and
             not is_map_key(style, :background_positions) and
             not is_map_key(style, :background_sizes) and
             not is_map_key(style, :background_repeats) and
             not is_map_key(style, :background_origins) and
             not is_map_key(style, :background_clips) and
             not is_map_key(style, :background_attachments) ->
        :ok

      _ ->
        valid? =
          Enum.all?(@plural, fn {property, key} ->
            case Map.fetch(style, key) do
              :error ->
                true

              {:ok, [_ | _] = values} ->
                HtmlValidator.check_render_resource(:max_entries, length(values), :layout)

                Enum.all?(values, fn value ->
                  case property do
                    "background-image" ->
                      CssImageValueValidator.valid_loaded?(value)

                    "background-position" ->
                      case value do
                        {a, b} ->
                          NativeElixirPdfUtilities.Validators.CssPositionValidator.valid_prepared?(
                            a
                          ) and
                            NativeElixirPdfUtilities.Validators.CssPositionValidator.valid_prepared?(
                              b
                            )

                        _ ->
                          false
                      end

                    "background-size" ->
                      case value do
                        v when v in [:cover, :contain] ->
                          true

                        {a, b} ->
                          Enum.all?([a, b], fn n ->
                            n == :auto or
                              NativeElixirPdfUtilities.Validators.CssLengthValidator.valid_edge?(
                                n,
                                :padding
                              ) or match?({:percent, r} when is_number(r) and r >= 0, n) or
                              NativeElixirPdfUtilities.Validators.CssSizingValidator.valid_calculation?(
                                n
                              )
                          end)

                        _ ->
                          false
                      end

                    "background-repeat" ->
                      case value do
                        v when v in [:repeat, :no_repeat, :repeat_x, :repeat_y] ->
                          true

                        {a, b} ->
                          a in [:repeat, :no_repeat, :round, :space] and
                            b in [:repeat, :no_repeat, :round, :space]

                        _ ->
                          false
                      end

                    v when v in ["background-origin", "background-clip"] ->
                      value in [:border_box, :padding_box, :content_box]

                    "background-attachment" ->
                      value in [:scroll, :fixed, :local]
                  end
                end)

              _ ->
                false
            end
          end)

        if valid?,
          do: :ok,
          else:
            NativeElixirPdfUtilities.Diagnostics.error(
              :layout,
              :invalid_layout,
              "background layers require valid prepared images, positions, sizes, repeat modes and box selections",
              operation: :layout,
              module: __MODULE__,
              source: source
            )
    end
  end

  defp prepare_list(parts, parser) do
    Enum.reduce_while(parts, {:ok, []}, fn part, {:ok, values} ->
      case parser.(part) do
        {:ok, value} -> {:cont, {:ok, [value | values]}}
        error -> {:halt, error}
      end
    end)
    |> case do
      {:ok, values} -> {:ok, Enum.reverse(values)}
      error -> error
    end
  end

  defp layer_value(property, value, style) do
    tokens = value |> String.downcase(:ascii) |> HtmlValidator.split_css_tokens()

    case property do
      "background-position" ->
        CssValueValidator.parse_two_axis_position(value, Map.get(style, :font_size), true)

      "background-size" ->
        CssValueValidator.parse_background_size(value)

      "background-repeat" ->
        case tokens do
          ["repeat-x"] ->
            {:ok, :repeat_x}

          ["repeat-y"] ->
            {:ok, :repeat_y}

          [repeat] when repeat in ~w(repeat no-repeat round space) ->
            {:ok, {repeat_atom(repeat), repeat_atom(repeat)}}

          [a, b]
          when a in ~w(repeat no-repeat round space) and b in ~w(repeat no-repeat round space) ->
            {:ok, {repeat_atom(a), repeat_atom(b)}}

          _ ->
            :error
        end
        |> case do
          {:ok, {:repeat, :repeat}} -> {:ok, :repeat}
          {:ok, {:no_repeat, :no_repeat}} -> {:ok, :no_repeat}
          {:ok, {:repeat, :no_repeat}} -> {:ok, :repeat_x}
          {:ok, {:no_repeat, :repeat}} -> {:ok, :repeat_y}
          result -> result
        end

      property when property in ["background-origin", "background-clip"] ->
        case tokens do
          [box] when box in ~w(border-box padding-box content-box) ->
            {:ok, box |> String.replace("-", "_") |> String.to_existing_atom()}

          _ ->
            :error
        end

      "background-attachment" ->
        case tokens do
          [attachment] when attachment in ~w(scroll fixed local) ->
            {:ok, String.to_existing_atom(attachment)}

          _ ->
            :error
        end
    end
  end

  defp repeat_atom(value) do
    case value do
      "repeat" -> :repeat
      "no-repeat" -> :no_repeat
      "round" -> :round
      "space" -> :space
    end
  end

  defp shorthand(style, parts) do
    with {:ok, layers} <- prepare_list(parts, &layer(&1, style)),
         true <- Enum.all?(Enum.drop(layers, -1), &(not &1.color_seen?)) do
      base =
        style
        |> Map.drop(Map.values(@plural) ++ [:background_image_source])
        |> Map.merge(@defaults)

      fields = %{
        background_color: List.last(layers).color,
        background_images: Enum.map(layers, & &1.image),
        background_positions: Enum.map(layers, & &1.position),
        background_sizes: Enum.map(layers, & &1.size),
        background_repeats: Enum.map(layers, & &1.repeat),
        background_origins: Enum.map(layers, & &1.origin),
        background_clips: Enum.map(layers, & &1.clip),
        background_attachments: Enum.map(layers, & &1.attachment)
      }

      {:ok, Map.merge(base, fields)}
    else
      {:error, _} = error -> error
      _ -> :error
    end
  end

  defp layer(source, style) do
    tokens = CssMathValidator.component_terms(source, true)

    initial = %{
      image: nil,
      image_seen?: false,
      color: nil,
      color_seen?: false,
      boxes: [],
      repeat: [],
      attachment: nil,
      position: [],
      size: [],
      slash?: false
    }

    Enum.reduce_while(tokens, {:ok, initial}, fn token, {:ok, state} ->
      normalized = String.downcase(token, :ascii)

      cond do
        token == "/" ->
          if state.slash? or state.position == [],
            do: {:halt, :error},
            else: {:cont, {:ok, %{state | slash?: true}}}

        normalized in ~w(border-box padding-box content-box) ->
          if length(state.boxes) == 2,
            do: {:halt, :error},
            else: {:cont, {:ok, %{state | boxes: state.boxes ++ [normalized]}}}

        normalized in ~w(scroll fixed local) ->
          if state.attachment != nil,
            do: {:halt, :error},
            else: {:cont, {:ok, %{state | attachment: normalized}}}

        normalized in ~w(repeat no-repeat repeat-x repeat-y round space) ->
          if length(state.repeat) == 2,
            do: {:halt, :error},
            else: {:cont, {:ok, %{state | repeat: state.repeat ++ [normalized]}}}

        true ->
          case CssImageValueValidator.prepare(token, style) do
            {:ok, image} ->
              if state.image_seen?,
                do: {:halt, :error},
                else: {:cont, {:ok, %{state | image: image, image_seen?: true}}}

            {:error, _} = error ->
              {:halt, error}

            _ ->
              case CssColorValidator.parse_function_color(token, style) do
                {:ok, color} ->
                  if state.color_seen?,
                    do: {:halt, :error},
                    else:
                      {:cont,
                       {:ok,
                        %{
                          state
                          | color: if(color == :transparent, do: nil, else: color),
                            color_seen?: true
                        }}}

                _ ->
                  if state.slash?,
                    do: {:cont, {:ok, %{state | size: state.size ++ [token]}}},
                    else: {:cont, {:ok, %{state | position: state.position ++ [token]}}}
              end
          end
      end
    end)
    |> case do
      {:ok, state} when tokens != [] ->
        {origin, clip} =
          case state.boxes do
            [] -> {"padding-box", "border-box"}
            [box] -> {box, box}
            [a, b] -> {a, b}
          end

        with true <- not state.slash? or state.size != [],
             {:ok, position} <-
               layer_value(
                 "background-position",
                 if(state.position == [], do: "0% 0%", else: Enum.join(state.position, " ")),
                 style
               ),
             {:ok, size} <-
               layer_value(
                 "background-size",
                 if(state.size == [], do: "auto", else: Enum.join(state.size, " ")),
                 style
               ),
             {:ok, repeat} <-
               layer_value(
                 "background-repeat",
                 if(state.repeat == [], do: "repeat", else: Enum.join(state.repeat, " ")),
                 style
               ),
             {:ok, origin} <- layer_value("background-origin", origin, style),
             {:ok, clip} <- layer_value("background-clip", clip, style),
             {:ok, attachment} <-
               layer_value("background-attachment", state.attachment || "scroll", style),
             do:
               {:ok,
                %{
                  image: state.image,
                  color: state.color,
                  color_seen?: state.color_seen?,
                  position: position,
                  size: size,
                  repeat: repeat,
                  origin: origin,
                  clip: clip,
                  attachment: attachment
                }},
             else: (_ -> :error)

      {:error, _} = error ->
        error

      _ ->
        :error
    end
  end
end
