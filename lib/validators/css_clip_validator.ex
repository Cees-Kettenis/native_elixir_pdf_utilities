defmodule NativeElixirPdfUtilities.Validators.CssClipValidator do
  @moduledoc "Basic and path clipping through shared CSS shapes and PDF contour geometry."
  alias NativeElixirPdfUtilities.{Diagnostics, Limits}
  alias NativeElixirPdfUtilities.HtmlToPdf.{DrawingGeometry, FlowShape, PathGeometry}

  alias NativeElixirPdfUtilities.Validators.{
    CssShapeValidator,
    CssSyntaxValidator,
    HtmlValidator,
    SvgPathValidator
  }

  @boxes ~w(border-box padding-box content-box margin-box)

  @doc "Prepares CSS clip-path basic shapes or the pinned SVG path grammar without duplicating numeric syntax."
  @spec prepare(map(), binary()) :: {:ok, map()} | :error | {:error, {atom(), map()}}
  def prepare(style, source) do
    source = HtmlValidator.trim_css_whitespace(source)

    source =
      case HtmlValidator.parse_css_identifier(source) do
        {:ok, keyword, ""} -> String.downcase(keyword, :ascii)
        _ -> source
      end

    tokens = HtmlValidator.split_css_tokens(source)

    case tokens do
      ["none"] ->
        {:ok, Map.put(style, :css_clip_path, :none)}

      _ ->
        case CssSyntaxValidator.function(":" <> source, "path") do
          {:ok, arguments, rest} ->
            arguments = CssSyntaxValidator.split(arguments, ",")
            reference = String.trim(rest)

            reference =
              if reference == "", do: "border-box", else: String.downcase(reference, :ascii)

            {rule, data} =
              case arguments do
                [data] ->
                  {:nonzero, data}

                [rule, data] when rule in ["evenodd", "nonzero"] ->
                  {String.to_existing_atom(rule), data}

                _ ->
                  {nil, nil}
              end

            with true <- rule != nil and reference in @boxes,
                 true <-
                   Regex.match?(
                     ~r/\A(?:#{HtmlValidator.css_string_pattern()})\z/us,
                     String.trim(data)
                   ),
                 {:ok, commands} <-
                   SvgPathValidator.prepare(HtmlValidator.decode_css_string(String.trim(data))),
                 do:
                   {:ok,
                    Map.put(style, :css_clip_path, %{
                      type: :path,
                      operations: commands,
                      rule: rule,
                      reference: String.to_existing_atom(String.replace(reference, "-", "_"))
                    })},
                 else: (_ -> :error)

          _ ->
            source =
              if Enum.any?(tokens, &(&1 in @boxes)), do: source, else: source <> " border-box"

            with {:ok, %{shape_outside: shape}} when is_map(shape) <-
                   CssShapeValidator.prepare("shape-outside", source),
                 {:ok, computed} <-
                   CssShapeValidator.compute_style(Map.put(style, :shape_outside, shape)),
                 do: {:ok, Map.put(style, :css_clip_path, computed.shape_outside)},
                 else: (_ -> :error)
        end
    end
  end

  @doc "Identifies a clip that establishes an isolated paint scope."
  @spec active?(map()) :: boolean()
  def active?(style), do: Map.get(style, :css_clip_path, :none) != :none

  @doc "Checks prepared clipping metadata at the styled-tree boundary."
  @spec valid_style?(map()) :: boolean()
  def valid_style?(style) do
    case Map.get(style, :css_clip_path, :none) do
      :none ->
        true

      %{type: :path, operations: operations, rule: rule, reference: reference}
      when is_list(operations) and rule in [:evenodd, :nonzero] and
             reference in [:border_box, :padding_box, :content_box, :margin_box] ->
        length(operations) <= Limits.get(:max_entries) and
          Enum.all?(operations, fn entry ->
            case entry do
              {command, values} when command in ~c"MmZzLlHhVvCcSsQqTtAa" and is_list(values) ->
                # Round-trip through the canonical parser also checks arity, arc flags and initial moves.
                Enum.all?(
                  values,
                  &(is_number(&1) and abs(&1) <= Limits.get(:max_numeric_magnitude))
                )

              _ ->
                false
            end
          end) and valid_operations?(operations)

      shape when is_map(shape) ->
        CssShapeValidator.validate_style(%{shape_outside: shape}, "clip-path") == :ok

      _ ->
        false
    end
  end

  defp valid_operations?(operations) do
    source =
      Enum.map_join(operations, fn {command, values} -> <<command>> <> Enum.join(values, " ") end)

    match?({:ok, _}, SvgPathValidator.prepare(source))
  end

  @doc "Resolves a physical CSS geometry box using the existing border, padding and margin dimensions."
  @spec reference_box(map(), map(), atom()) :: map()
  def reference_box(bounds, style, reference) do
    zero = %{top: 0.0, right: 0.0, bottom: 0.0, left: 0.0}
    border = Map.get(style, :border_widths, zero)

    insets =
      case reference do
        :content_box ->
          Map.new(border, fn {side, n} ->
            {side, n + Map.get(Map.get(style, :padding, zero), side, 0.0)}
          end)

        :padding_box ->
          border

        :margin_box ->
          Map.new(Map.get(style, :margin, zero), fn {side, n} ->
            {side, if(is_number(n), do: -n, else: 0.0)}
          end)

        _ ->
          zero
      end

    %{
      x: bounds.x + insets.left,
      top: bounds.top - insets.top,
      width: max(0.0, bounds.width - insets.left - insets.right),
      height: max(0.0, bounds.height - insets.top - insets.bottom)
    }
  end

  @doc "Resolves admitted clipping geometry into the same canonical commands used by PDF paint and link projection."
  @spec resolve(map(), map()) :: {:ok, map() | nil} | {:error, {atom(), map()}}
  def resolve(style, bounds) do
    if valid_style?(style) do
      case Map.get(style, :css_clip_path, :none) do
        :none ->
          {:ok, nil}

        shape ->
          reference = reference_box(bounds, style, shape.reference)

          {commands, rule} =
            case shape do
              %{type: :path, operations: operations, rule: rule} ->
                {operations
                 |> PathGeometry.commands()
                 |> PathGeometry.transform({0.75, 0, 0, -0.75, reference.x, reference.top}), rule}

              %{type: :box} ->
                {DrawingGeometry.rounded_rect_commands(
                   reference.x,
                   reference.top - reference.height,
                   reference.width,
                   reference.height,
                   Map.get(style, :border_radius, 0.0)
                 ), :nonzero}

              _ ->
                case FlowShape.geometry(shape, reference, style) do
                  {:ellipse, cx, cy, rx, ry} ->
                    radii =
                      Map.new(
                        [:top_left, :top_right, :bottom_left, :bottom_right],
                        &{&1, {rx, ry}}
                      )

                    {DrawingGeometry.elliptical_rect_commands(
                       cx - rx,
                       cy - ry,
                       2 * rx,
                       2 * ry,
                       radii,
                       4
                     ), :nonzero}

                  {:polygon, [{x, y} | points], rule} ->
                    {[{0, [x, y]}] ++ Enum.map(points, fn {x, y} -> {1, [x, y]} end) ++ [{4, []}],
                     rule}
                end
            end

          coordinates = Enum.flat_map(commands, &elem(&1, 1)) |> Enum.chunk_every(2)

          {left, bottom, right, top} =
            case coordinates do
              [] ->
                {reference.x, reference.top, reference.x, reference.top}

              _ ->
                {coordinates |> Enum.map(&hd/1) |> Enum.min(),
                 coordinates |> Enum.map(&List.last/1) |> Enum.min(),
                 coordinates |> Enum.map(&hd/1) |> Enum.max(),
                 coordinates |> Enum.map(&List.last/1) |> Enum.max()}
            end

          clip = %{
            x: left,
            y: bottom,
            width: right - left,
            height: top - bottom,
            path_commands: commands,
            fill_rule: rule,
            css_source_path: true
          }

          case validate_clip(clip) do
            :ok -> {:ok, clip}
            error -> error
          end
      end
    else
      invalid("computed clip-path requires admitted basic/path geometry")
    end
  rescue
    ArithmeticError ->
      invalid(
        "clip-path arithmetic exceeds finite geometry; reduce arc extremes or intermediate coordinates"
      )
  end

  @doc "Admits bounded canonical path clips on native drawing groups."
  @spec validate_clip(term()) :: :ok | {:error, {atom(), map()}}
  def validate_clip(clip) do
    case clip do
      %{path_commands: commands, fill_rule: rule}
      when is_list(commands) and rule in [:evenodd, :nonzero] ->
        valid =
          is_boolean(Map.get(clip, :css_source_path, false)) and
            length(commands) <= Limits.get(:max_entries) and
            Enum.all?(commands, fn command ->
              case command do
                {operation, coordinates}
                when operation in [0, 1, 3, 4] and is_list(coordinates) ->
                  length(coordinates) == Map.fetch!(%{0 => 2, 1 => 2, 3 => 6, 4 => 0}, operation) and
                    Enum.all?(
                      coordinates,
                      &(is_number(&1) and abs(&1) <= Limits.get(:max_numeric_magnitude))
                    )

                _ ->
                  false
              end
            end) and (commands == [] or elem(hd(commands), 0) == 0)

        if valid,
          do: :ok,
          else: invalid("path clips require bounded canonical move/line/cubic/close commands")

      %{path_commands: _} ->
        invalid("path clips require a nonzero or evenodd fill rule")

      _ ->
        :ok
    end
  end

  defp invalid(message),
    do:
      Diagnostics.error(:layout, :invalid_layout, message,
        module: __MODULE__,
        operation: :resolve_css_clip,
        source: "clip-path"
      )
end
