defmodule NativeElixirPdfUtilities.Validators.CssBoxPaintValidator do
  @moduledoc "Static outline, box shadow and text fill/stroke admission."
  alias NativeElixirPdfUtilities.Validators.{
    HtmlValidator,
    CssLengthValidator,
    CssValueValidator,
    WriterValidator
  }

  @properties ~w(outline outline-width outline-style outline-color outline-offset box-shadow -webkit-text-fill-color -webkit-text-stroke-color -webkit-text-stroke-width -webkit-text-stroke)
  @fields [
    :outline_width,
    :outline_style,
    :outline_color,
    :outline_offset,
    :box_shadow,
    :text_fill_color,
    :text_stroke_color,
    :text_stroke_width
  ]

  @doc "Lists supported box paint and compatibility text paint properties."
  @spec properties() :: [String.t()]
  def properties, do: @properties

  @doc "Returns CSS-wide reset values without populating ordinary styles."
  @spec defaults() :: map()
  def defaults,
    do: %{
      outline_width: 2.25,
      outline_style: :none,
      outline_color: :current_color,
      outline_offset: 0.0,
      box_shadow: [],
      text_fill_color: :current_color,
      text_stroke_color: :current_color,
      text_stroke_width: 0.0
    }

  @doc "Prepares outlines, ordered shadows and inherited text paint through shared CSS validators."
  @spec prepare(map(), String.t(), String.t()) :: {:ok, map()} | :error | {:error, term()}
  def prepare(style, property, source) do
    source = source |> String.trim() |> String.downcase(:ascii)

    case property do
      "box-shadow" ->
        shadows(style, source)

      property
      when property in ["outline-color", "-webkit-text-fill-color", "-webkit-text-stroke-color"] ->
        field =
          case property do
            "outline-color" -> :outline_color
            "-webkit-text-fill-color" -> :text_fill_color
            "-webkit-text-stroke-color" -> :text_stroke_color
          end

        with {:ok, color} <- paint(source, style), do: {:ok, Map.put(style, field, color)}

      property
      when property in ["outline-width", "outline-offset", "-webkit-text-stroke-width"] ->
        {field, policy} =
          case property do
            "outline-width" -> {:outline_width, :length}
            "outline-offset" -> {:outline_offset, :signed_length}
            _ -> {:text_stroke_width, :length}
          end

        parsed =
          if property == "outline-width" or property == "-webkit-text-stroke-width",
            do: CssValueValidator.parse_border_width(source),
            else: CssLengthValidator.prepare(source, policy)

        with {:ok, n} <- parsed,
             {:ok, n} <- CssLengthValidator.resolve(n, nil, style),
             do: {:ok, Map.put(style, field, n)}

      "outline-style" ->
        if source in ~w(none auto dotted dashed solid double groove ridge inset outset),
          do: {:ok, Map.put(style, :outline_style, String.to_existing_atom(source))},
          else: :error

      "outline" ->
        tokens = HtmlValidator.split_css_tokens(source)

        if "hidden" in tokens do
          :error
        else
          normalized =
            Enum.map_join(tokens, " ", fn token ->
              if token == "auto", do: "solid", else: token
            end)

          with {:ok, border} <-
                 CssValueValidator.validate_declaration(
                   style,
                   {"border", normalized},
                   HtmlValidator.new_css_budget()
                 ) do
            {:ok,
             Map.merge(style, %{
               outline_width: border.border_widths.top,
               outline_style: if("auto" in tokens, do: :auto, else: border.border_styles.top),
               outline_color: border.border_color || :transparent
             })}
          end
        end

      "-webkit-text-stroke" ->
        tokens = HtmlValidator.split_css_tokens(source)

        if length(tokens) in [1, 2] do
          Enum.reduce_while(tokens, {:ok, %{width: nil, color: nil}}, fn token, {:ok, fields} ->
            case paint(token, style) do
              {:ok, color} when fields.color == nil ->
                {:cont, {:ok, %{fields | color: color}}}

              _ ->
                with nil <- fields.width,
                     {:ok, width} <- CssValueValidator.parse_border_width(token),
                     {:ok, width} <- CssLengthValidator.resolve(width, nil, style) do
                  {:cont, {:ok, %{fields | width: width}}}
                else
                  _ -> {:halt, :error}
                end
            end
          end)
          |> case do
            {:ok, fields} ->
              {:ok,
               Map.merge(style, %{
                 text_stroke_width: fields.width || 0.0,
                 text_stroke_color: fields.color || :current_color
               })}

            error ->
              error
          end
        else
          :error
        end

      _ ->
        :error
    end
  end

  defp paint(source, style) do
    if String.downcase(source, :ascii) == "currentcolor",
      do: {:ok, :current_color},
      else: HtmlValidator.parse_css_color(source, style)
  end

  defp shadows(style, source) do
    if source == "none" do
      {:ok, Map.put(style, :box_shadow, [])}
    else
      entries = HtmlValidator.split_css_comma(source, true)
      HtmlValidator.reserve_render_resource(:max_entries, length(entries), :style)

      Enum.reduce_while(entries, {:ok, []}, fn entry, {:ok, shadows} ->
        tokens = HtmlValidator.split_css_tokens(entry)
        inset = Enum.count(tokens, &(&1 == "inset"))

        result =
          Enum.reduce_while(Enum.reject(tokens, &(&1 == "inset")), {:ok, [], nil}, fn token,
                                                                                      {:ok,
                                                                                       lengths,
                                                                                       color} ->
            case paint(token, style) do
              {:ok, paint} when color == nil ->
                {:cont, {:ok, lengths, paint}}

              _ ->
                policy = if length(lengths) == 2, do: :length, else: :signed_length

                with {:ok, n} <- CssLengthValidator.prepare(token, policy),
                     {:ok, n} <- CssLengthValidator.resolve(n, nil, style) do
                  {:cont, {:ok, lengths ++ [n], color}}
                else
                  _ -> {:halt, :error}
                end
            end
          end)

        case result do
          {:ok, [x, y | rest], color} when length(rest) <= 2 and inset <= 1 ->
            {:cont,
             {:ok,
              [
                %{
                  x: x,
                  y: y,
                  blur: Enum.at(rest, 0, 0.0),
                  spread: Enum.at(rest, 1, 0.0),
                  inset: inset == 1,
                  color: color || :current_color
                }
                | shadows
              ]}}

          _ ->
            {:halt, :error}
        end
      end)
      |> case do
        {:ok, shadows} -> {:ok, Map.put(style, :box_shadow, Enum.reverse(shadows))}
        error -> error
      end
    end
  end

  @doc "Checks prepared box and text paint fields at the styled-tree boundary."
  @spec validate_style(map(), String.t()) :: :ok | {:error, term()}
  def validate_style(style, source) do
    case style do
      style
      when not is_map_key(style, :outline_width) and not is_map_key(style, :outline_style) and
             not is_map_key(style, :outline_color) and not is_map_key(style, :outline_offset) and
             not is_map_key(style, :box_shadow) and not is_map_key(style, :text_fill_color) and
             not is_map_key(style, :text_stroke_color) and
             not is_map_key(style, :text_stroke_width) ->
        :ok

      _ ->
        valid? =
          Enum.all?(@fields, fn key ->
            case Map.fetch(style, key) do
              :error ->
                true

              {:ok, value} ->
                cond do
                  key in [:outline_width, :text_stroke_width] ->
                    is_number(value) and value >= 0 and
                      match?({:ok, _}, HtmlValidator.normalize_geometry_number(value))

                  key == :outline_offset ->
                    match?({:ok, _}, HtmlValidator.normalize_geometry_number(value))

                  key == :outline_style ->
                    value in [
                      :none,
                      :auto,
                      :dotted,
                      :dashed,
                      :solid,
                      :double,
                      :groove,
                      :ridge,
                      :inset,
                      :outset
                    ]

                  key == :box_shadow ->
                    is_list(value) and
                      Enum.all?(value, fn shadow ->
                        case shadow do
                          %{x: x, y: y, blur: blur, spread: spread, inset: inset, color: color} ->
                            Enum.all?(
                              [x, y, blur, spread],
                              &match?({:ok, _}, HtmlValidator.normalize_geometry_number(&1))
                            ) and blur >= 0 and is_boolean(inset) and
                              (color in [:transparent, :current_color] or
                                 WriterValidator.valid_color?(color))

                          _ ->
                            false
                        end
                      end)

                  true ->
                    value in [:transparent, :current_color] or WriterValidator.valid_color?(value)
                end
            end
          end)

        if valid?,
          do: :ok,
          else:
            NativeElixirPdfUtilities.Diagnostics.error(
              :layout,
              :invalid_layout,
              "box paint requires bounded outline lengths, ordered shadows and valid text fill/stroke colors",
              operation: :layout,
              module: __MODULE__,
              source: source
            )
    end
  end

  @doc "Checks optional PDF text stroke metadata after currentColor has been resolved."
  @spec valid_text_box?(map()) :: boolean()
  def valid_text_box?(box) do
    case Map.get(box, :text_stroke_width) do
      nil ->
        not Map.has_key?(box, :text_stroke_color)

      width ->
        is_number(width) and width >= 0 and
          match?({:ok, _}, HtmlValidator.normalize_geometry_number(width)) and
          WriterValidator.valid_color?(Map.get(box, :text_stroke_color))
    end
  end
end
