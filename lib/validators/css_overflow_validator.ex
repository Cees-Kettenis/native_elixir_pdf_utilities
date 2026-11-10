defmodule NativeElixirPdfUtilities.Validators.CssOverflowValidator do
  @moduledoc "Owns independent static overflow axes, clip edges and overflow-marker grammar."
  alias NativeElixirPdfUtilities.Validators.{HtmlValidator, CssMathValidator, CssSizingValidator}

  @string Regex.compile!("\\A" <> HtmlValidator.css_string_pattern() <> "\\z", "u")

  @doc "Parses authored overflow geometry through the shared CSS token and length grammars."
  @spec prepare(String.t(), String.t()) :: {:ok, map()} | :error | {:error, {atom(), map()}}
  def prepare(property, source) do
    tokens = CssMathValidator.component_terms(source, false)

    if tokens != [] do
      case property do
        property when property in ["overflow", "overflow-x", "overflow-y"] ->
          values =
            Enum.map(tokens, fn token ->
              case HtmlValidator.parse_css_identifier(token) do
                {:ok, name, ""} ->
                  Map.get(
                    %{
                      "visible" => :visible,
                      "hidden" => :hidden,
                      "clip" => :clip,
                      "auto" => :auto,
                      "scroll" => :scroll
                    },
                    String.downcase(name, :ascii)
                  )

                _ ->
                  nil
              end
            end)

          case {property, values} do
            {"overflow", [x]} when x != nil ->
              {:ok, %{overflow: x, overflow_x: x, overflow_y: x}}

            {"overflow", [x, y]} when x != nil and y != nil ->
              {:ok, %{overflow_x: x, overflow_y: y}}

            {"overflow-x", [x]} when x != nil ->
              {:ok, %{overflow_x: x}}

            {"overflow-y", [y]} when y != nil ->
              {:ok, %{overflow_y: y}}

            _ ->
              :error
          end

        "overflow-clip-margin" ->
          {boxes, lengths} =
            Enum.split_with(tokens, &(&1 in ["content-box", "padding-box", "border-box"]))

          if length(boxes) <= 1 and length(lengths) <= 1 do
            box =
              Map.fetch!(
                %{
                  "content-box" => :content_box,
                  "padding-box" => :padding_box,
                  "border-box" => :border_box
                },
                List.first(boxes) || "padding-box"
              )

            length = List.first(lengths) || "0"

            with {:ok, size} <- CssSizingValidator.prepare_math(length),
                 {:ok, %{percentage?: false}} <- CssMathValidator.length_dependencies(length) do
              {:ok, %{overflow_clip_margin: {box, size}}}
            else
              {:error, _} = error -> error
              _ -> :error
            end
          else
            :error
          end

        property when property in ["text-overflow", "block-ellipsis"] ->
          markers =
            Enum.map(tokens, fn token ->
              case String.downcase(token, :ascii) do
                "clip" when property == "text-overflow" ->
                  :clip

                "ellipsis" when property == "text-overflow" ->
                  "…"

                "none" when property == "block-ellipsis" ->
                  :clip

                "auto" when property == "block-ellipsis" ->
                  "…"

                _ ->
                  if Regex.match?(@string, token),
                    do: HtmlValidator.decode_css_string(token),
                    else: nil
              end
            end)

          key = if property == "text-overflow", do: :text_overflow, else: :block_ellipsis

          case markers do
            [marker] when marker != nil ->
              {:ok, %{key => marker}}

            [first, last] when first != nil and last != nil and property == "text-overflow" ->
              {:ok, %{key => {first, last}}}

            _ ->
              :error
          end

        "max-lines" ->
          case source do
            "none" ->
              {:ok, %{max_lines: :none}}

            _ ->
              case CssMathValidator.scalar(source, :integer) do
                {:ok, number} when number >= 1 ->
                  HtmlValidator.check_render_resource(:max_css_numeric_magnitude, number, :css)
                  {:ok, %{max_lines: trunc(number)}}

                _ ->
                  :error
              end
          end

        "continue" ->
          case source do
            "auto" -> {:ok, %{continue: :auto}}
            "discard" -> {:ok, %{continue: :discard}}
            _ -> :error
          end

        "line-clamp" ->
          case tokens do
            ["none"] ->
              {:ok, %{max_lines: :none, block_ellipsis: :clip, continue: :auto}}

            [count | rest] ->
              with {:ok, %{max_lines: count}} when is_integer(count) <-
                     prepare("max-lines", count),
                   {:ok, marker} <-
                     prepare(
                       "block-ellipsis",
                       if(rest == [], do: "auto", else: Enum.join(rest, " "))
                     ),
                   do: {:ok, Map.merge(marker, %{max_lines: count, continue: :discard})}
          end

        _ ->
          :error
      end
    else
      :error
    end
  end

  @doc "Computes cross-axis fallbacks once and freezes the clip length at its declaring element."
  @spec compute_style(map()) :: {:ok, map()} | :error | {:error, {atom(), map()}}
  def compute_style(style) do
    style =
      if Map.has_key?(style, :overflow_x) or Map.has_key?(style, :overflow_y) do
        x = Map.get(style, :overflow_x, Map.get(style, :overflow, :visible))
        y = Map.get(style, :overflow_y, Map.get(style, :overflow, :visible))

        compute = fn axis, other ->
          if other not in [:visible, :clip] do
            case axis do
              :visible -> :auto
              :clip -> :hidden
              _ -> axis
            end
          else
            axis
          end
        end

        x_used = compute.(x, y)
        y_used = compute.(y, x)

        Map.merge(style, %{
          overflow_x: x_used,
          overflow_y: y_used,
          overflow: if(x_used == y_used, do: x_used, else: :mixed)
        })
      else
        style
      end

    case Map.get(style, :overflow_clip_margin) do
      {box, {:css_size_math, {source, false}}} ->
        with {:ok, value} <-
               CssSizingValidator.resolve_math({:css_size_math, {source, false}}, nil, style),
             do: {:ok, Map.put(style, :overflow_clip_margin, {box, value})}

      _ ->
        {:ok, style}
    end
  end

  @doc "Validates computed overflow metadata before layout uses axes, clips or shaped-line markers."
  @spec validate_style(map(), String.t()) :: :ok | {:error, {atom(), map()}}
  def validate_style(style, source) do
    case style do
      style
      when not is_map_key(style, :overflow_x) and not is_map_key(style, :overflow_y) and
             not is_map_key(style, :overflow_clip_margin) and
             not is_map_key(style, :text_overflow) and
             not is_map_key(style, :block_ellipsis) and not is_map_key(style, :max_lines) and
             not is_map_key(style, :continue) ->
        :ok

      _ ->
        {x, y} = axes(style)

        marker? = fn marker ->
          marker == :clip or (is_binary(marker) and String.valid?(marker))
        end

        text_marker = Map.get(style, :text_overflow, :clip)

        valid_marker =
          case text_marker do
            {first, last} -> marker?.(first) and marker?.(last)
            marker -> marker?.(marker)
          end

        clip_margin = Map.get(style, :overflow_clip_margin, {:padding_box, 0.0})

        valid_clip =
          case clip_margin do
            {box, length}
            when box in [:content_box, :padding_box, :border_box] and is_number(length) and
                   length >= 0 ->
              HtmlValidator.check_render_resource(:max_css_numeric_magnitude, length, :layout)
              match?({:ok, _}, HtmlValidator.normalize_geometry_number(length))

            _ ->
              false
          end

        maximum = Map.get(style, :max_lines, :none)
        valid_maximum = maximum == :none or (is_integer(maximum) and maximum >= 1)

        if is_integer(maximum),
          do:
            HtmlValidator.check_render_resource(:max_css_numeric_magnitude, abs(maximum), :layout)

        valid_axes =
          x in [:visible, :hidden, :clip, :auto, :scroll] and
            y in [:visible, :hidden, :clip, :auto, :scroll] and
            not (x in [:visible, :clip] and y not in [:visible, :clip]) and
            not (y in [:visible, :clip] and x not in [:visible, :clip])

        if valid_axes and valid_clip and valid_marker and valid_maximum and
             marker?.(Map.get(style, :block_ellipsis, :clip)) and
             Map.get(style, :continue, :auto) in [:auto, :discard] do
          :ok
        else
          NativeElixirPdfUtilities.Diagnostics.error(
            :layout,
            :invalid_layout,
            "overflow requires computed axis values, a nonnegative point clip margin, valid UTF-8 markers and a positive integer max_lines or :none; recompute styles",
            module: __MODULE__,
            operation: :layout,
            source: source
          )
        end
    end
  end

  @doc "Returns the computed axis pair, including styles that predate axis longhands."
  @spec axes(map()) :: {atom(), atom()}
  def axes(style) do
    fallback = Map.get(style, :overflow, :visible)
    {Map.get(style, :overflow_x, fallback), Map.get(style, :overflow_y, fallback)}
  end

  @doc "Identifies scroll-container formatting contexts; clip alone does not create one."
  @spec independent?(map()) :: boolean()
  def independent?(style) do
    {x, y} = axes(style)
    x not in [:visible, :clip] or y not in [:visible, :clip]
  end
end
