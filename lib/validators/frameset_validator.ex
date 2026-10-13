defmodule NativeElixirPdfUtilities.Validators.FramesetValidator do
  @moduledoc false

  alias NativeElixirPdfUtilities.{Diagnostics, Limits}
  alias NativeElixirPdfUtilities.Validators.{HtmlValidator, ResourceValidator}

  @doc "Prepares bounded legacy frameset tracks, border inheritance and static paint colors."
  @spec prepare(term(), term()) :: {:ok, map()} | {:error, {atom(), map()}}
  @spec prepare(term(), term(), term()) :: {:ok, map()} | {:error, {atom(), map()}}
  def prepare(attributes, inherited \\ nil, style \\ %{}) do
    if is_map(attributes) and is_map(style) do
      with {:ok, enabled, width} <- border(attributes, inherited, :frameset),
           {:ok, hints} <- prepare_color_hints(attributes, inherited),
           {:ok, rows} <- tracks(Map.get(attributes, "rows")),
           {:ok, columns} <- tracks(Map.get(attributes, "cols")),
           :ok <- cells_limit(rows, columns),
           color <-
             (case Map.get(style, :border_colors) do
                %{left: color} -> color
                nil -> Map.get(hints, :border_color) || Map.get(style, :color, {0.0, 0.0, 0.0})
                _ -> :invalid
              end),
           true <- NativeElixirPdfUtilities.Validators.WriterValidator.valid_color?(color) do
        {:ok,
         %{
           rows: rows,
           columns: columns,
           border_enabled: enabled,
           border_width: width,
           has_border_color:
             Map.has_key?(attributes, "bordercolor") or
               (is_map(inherited) and Map.get(inherited, :has_border_color, false)),
           border_color: color
         }}
      else
        false -> invalid("frameset paint requires a validated border color")
        error -> error
      end
    else
      invalid("frameset requires an attribute map")
    end
  catch
    {:render_resource_limit, error} -> error
  end

  @doc "Parses a legacy comma list into absolute, percentage and relative dimensions."
  @spec tracks(term()) :: {:ok, [tuple()]} | {:error, {atom(), map()}}
  def tracks(value) do
    if is_nil(value) or (is_binary(value) and String.valid?(value)) do
      tokens =
        if is_nil(value),
          do: ["*"],
          else: value |> String.replace(~r/,\z/u, "") |> String.split(",", trim: false)

      if length(tokens) > Limits.get(:max_frameset_tracks) do
        limit("frameset tracks exceed max_frameset_tracks")
      else
        Enum.reduce_while(tokens, {:ok, []}, fn token, {:ok, result} ->
          case dimension(token) do
            {:ok, track} -> {:cont, {:ok, [track | result]}}
            error -> {:halt, error}
          end
        end)
        |> case do
          {:ok, result} -> {:ok, Enum.reverse(result)}
          error -> error
        end
      end
    else
      invalid("frameset rows and cols require comma-separated dimension strings")
    end
  end

  # This grammar implements HTML list-of-dimensions recovery, rather than CSS lengths.
  defp dimension(token) do
    if byte_size(token) > Limits.get(:max_html_numeric_token_bytes) do
      limit("frameset dimension token exceeds max_html_numeric_token_bytes")
    else
      token = HtmlValidator.trim_ascii_whitespace(token)

      [_, integer, fraction, unit] =
        Regex.run(~r/\A([0-9]*)(?:\.([0-9 \t\n\f\r]*))?[ \t\n\f\r]*([%*]?)/u, token)

      fraction = String.replace(fraction, ~r/[ \t\n\f\r]/u, "")

      number =
        if(integer == "", do: "0", else: integer) <>
          if(fraction == "", do: "", else: "." <> fraction)

      case HtmlValidator.parse_css_number(number) do
        {value, ""} ->
          kind =
            if(token == "" or unit == "*",
              do: :relative,
              else: if(unit == "%", do: :percentage, else: :absolute)
            )

          {:ok, {kind, if(kind == :relative and value == 0, do: 1.0, else: value)}}

        _ ->
          invalid("frameset dimension exceeds the supported numeric range")
      end
    end
  end

  @doc "Validates legacy frame margins and a local initial source before viewport preparation."
  @spec prepare_frame(term(), term()) :: {:ok, map()} | {:error, {atom(), map()}}
  def prepare_frame(attributes, inherited) do
    if is_map(attributes) do
      with {:ok, enabled, _width} <- border(attributes, inherited, :frame),
           {:ok, horizontal} <- margin(Map.get(attributes, "marginwidth")),
           {:ok, vertical} <- margin(Map.get(attributes, "marginheight")) do
        scrolling = Map.get(attributes, "scrolling", "auto")

        if is_binary(scrolling) do
          {:ok,
           %{
             source: ResourceValidator.iframe_source(Map.delete(attributes, "srcdoc")),
             border_enabled: enabled,
             scrollbars:
               if(String.downcase(scrolling, :ascii) in ["no", "off", "noscroll"],
                 do: :none,
                 else: :auto
               ),
             name: Map.get(attributes, "name"),
             margins: %{horizontal: horizontal, vertical: vertical}
           }}
        else
          invalid("frame scrolling requires a legacy string value")
        end
      end
    else
      invalid("frame requires an attribute map")
    end
  catch
    {:render_resource_limit, error} -> error
  end

  defp margin(value) do
    if is_nil(value) do
      {:ok, nil}
    else
      case legacy_integer(value) do
        {:ok, number} when number < 0 ->
          {:ok, nil}

        {:ok, number} ->
          case HtmlValidator.parse_css_number(Integer.to_string(number), 0.75) do
            {points, ""} -> {:ok, points}
            _ -> invalid("frame margin exceeds the supported numeric range")
          end

        :error ->
          {:ok, 0.0}
      end
    end
  end

  @doc "Validates frame body defaults, which author CSS can subsequently override."
  @spec prepare_body_margins(term()) :: {:ok, nil | map()} | {:error, {atom(), map()}}
  def prepare_body_margins(value) do
    case value do
      nil ->
        {:ok, nil}

      %{horizontal: horizontal, vertical: vertical} = margins when map_size(margins) == 2 ->
        if Enum.all?([horizontal, vertical], fn number ->
             is_nil(number) or
               (match?({:ok, _}, HtmlValidator.normalize_geometry_number(number)) and number >= 0)
           end) do
          {:ok,
           %{
             top: vertical || 6.0,
             bottom: vertical || 6.0,
             left: horizontal || 6.0,
             right: horizontal || 6.0
           }}
        else
          invalid("frame margins require bounded nonnegative points or absent hints")
        end

      _ ->
        invalid("frame margins require horizontal and vertical hints")
    end
  end

  defp border(attributes, inherited, kind) do
    context =
      case inherited do
        nil ->
          {:ok, {true, 4.5}}

        %{border_enabled: enabled, border_width: width} when is_boolean(enabled) ->
          if match?({:ok, _}, HtmlValidator.normalize_geometry_number(width)) and width >= 0,
            do: {:ok, {enabled, width}},
            else: invalid("inherited frameset border requires bounded nonnegative points")

        _ ->
          invalid("inherited frameset border requires an issued enable flag and width")
      end

    with {:ok, {inherited_enabled, inherited_width}} <- context do
      enabled =
        case Map.fetch(attributes, "frameborder") do
          {:ok, value} when is_binary(value) ->
            if kind == :frame do
              case legacy_integer(value) do
                {:ok, number} -> number != 0
                :error -> false
              end
            else
              case String.downcase(value, :ascii) do
                value when value in ["0", "no"] -> false
                value when value in ["1", "yes"] -> true
                _ -> inherited_enabled
              end
            end

          {:ok, _} ->
            :invalid

          :error ->
            inherited_enabled
        end

      if is_boolean(enabled) do
        width =
          cond do
            not enabled ->
              0.0

            kind == :frameset and Map.has_key?(attributes, "border") ->
              case legacy_integer(attributes["border"]) do
                {:ok, 0} ->
                  0.0

                {:ok, number} ->
                  case HtmlValidator.parse_css_number(Integer.to_string(max(number, 1)), 0.75) do
                    {points, ""} -> points
                    _ -> :invalid
                  end

                :error ->
                  0.0
              end

            true ->
              inherited_width
          end

        case HtmlValidator.normalize_geometry_number(width) do
          {:ok, width} -> {:ok, enabled, width}
          _ -> invalid("frameset border exceeds the supported numeric range")
        end
      else
        invalid("frameborder requires a legacy string value")
      end
    end
  end

  # Blink stores these legacy frame attributes as signed 32-bit integers.
  # This is a fixed browser conversion invariant, not a resource ceiling.
  defp legacy_integer(value) do
    case HtmlValidator.parse_html_integer(value) do
      {:ok, integer} when integer >= -2_147_483_648 and integer <= 2_147_483_647 -> {:ok, integer}
      _ -> :error
    end
  end

  @doc "Resolves legacy border-color hints before author CSS takes precedence."
  @spec prepare_color_hints(term(), term()) :: {:ok, map()} | {:error, {atom(), map()}}
  def prepare_color_hints(attributes, inherited) do
    if is_map(attributes) and (is_nil(inherited) or is_map(inherited)) do
      color =
        case HtmlValidator.parse_html_legacy_color(Map.get(attributes, "bordercolor")) do
          {:ok, color} -> color
          :error -> if(is_map(inherited), do: Map.get(inherited, :border_color), else: nil)
        end

      if is_nil(color) or NativeElixirPdfUtilities.Validators.WriterValidator.valid_color?(color) do
        {:ok,
         if(is_nil(color),
           do: %{},
           else: %{
             border_color: color,
             border_colors: NativeElixirPdfUtilities.Validators.CssValueValidator.edges(color)
           }
         )}
      else
        invalid("frameset border-color inheritance requires a validated color")
      end
    else
      invalid("frameset border-color hints require attributes and an inherited plan")
    end
  end

  @doc "Issues shared grid edge flags from already prepared child documents and nested grids."
  @spec prepare_children(term(), term()) :: {:ok, map()} | {:error, {atom(), map()}}
  def prepare_children(plan, children) do
    case {plan, children} do
      {%{rows: rows, columns: columns}, children}
      when is_list(rows) and rows != [] and is_list(columns) and columns != [] and
             is_list(children) ->
        with :ok <- cells_limit(rows, columns),
             :ok <-
               HtmlValidator.check_render_resource(:max_html_nodes, length(children), :layout) do
          row_count = length(rows)
          column_count = length(columns)
          cells = row_count * column_count

          HtmlValidator.reserve_render_resource(
            :max_html_tree_work,
            min(length(children), cells) + row_count + column_count + 2,
            :layout
          )

          children
          |> Enum.take(cells)
          |> Enum.with_index()
          |> Enum.reduce_while({:ok, {MapSet.new(), MapSet.new()}}, fn {child, index},
                                                                       {:ok,
                                                                        {vertical, horizontal}} ->
            edges =
              case child do
                %{
                  tag: "frameset",
                  style: %{_frameset: %{outer_edges: {left, right, top, bottom} = edges}}
                } ->
                  if Enum.all?([left, right, top, bottom], &is_boolean/1),
                    do: {:ok, edges},
                    else: invalid("nested frameset requires issued edge flags")

                %{tag: "frame", style: %{embedded_document: %{frame_border: enabled}}}
                when is_boolean(enabled) ->
                  {:ok, {enabled, enabled, enabled, enabled}}

                _ ->
                  invalid("frameset borders require prepared frame or nested grid edges")
              end

            case edges do
              {:ok, {left, right, top, bottom}} ->
                column = rem(index, column_count)
                row = div(index, column_count)
                vertical = if(left, do: MapSet.put(vertical, column), else: vertical)
                vertical = if(right, do: MapSet.put(vertical, column + 1), else: vertical)
                horizontal = if(top, do: MapSet.put(horizontal, row), else: horizontal)
                horizontal = if(bottom, do: MapSet.put(horizontal, row + 1), else: horizontal)
                {:cont, {:ok, {vertical, horizontal}}}

              error ->
                {:halt, error}
            end
          end)
          |> case do
            {:ok, {vertical, horizontal}} ->
              {:ok,
               Map.merge(plan, %{
                 column_allow_border:
                   List.to_tuple(Enum.map(0..column_count, &MapSet.member?(vertical, &1))),
                 row_allow_border:
                   List.to_tuple(Enum.map(0..row_count, &MapSet.member?(horizontal, &1))),
                 outer_edges:
                   {MapSet.member?(vertical, 0), MapSet.member?(vertical, column_count),
                    MapSet.member?(horizontal, 0), MapSet.member?(horizontal, row_count)}
               })}

            error ->
              error
          end
        end

      _ ->
        invalid("frameset borders require bounded prepared tracks and child cells")
    end
  catch
    {:render_resource_limit, error} -> error
  end

  @doc "Issues the legacy grid style, whose layout is retained despite display:none."
  @spec prepare_initial_style(term()) :: {:ok, map()} | {:error, {atom(), map()}}
  def prepare_initial_style(style) do
    with :ok <- validate_style(style), do: {:ok, Map.put(style, :display, :block)}
  end

  @doc "Rejects author geometry the native legacy grid cannot apply faithfully."
  @spec validate_style(term()) :: :ok | {:error, {atom(), map()}}
  def validate_style(style) do
    if is_map(style) and Map.get(style, :display) in [:block, :none] and
         Enum.all?(
           [:width, :height, :min_width, :max_width, :min_height, :max_height],
           &(Map.get(style, &1) in [nil, :auto])
         ) and
         Map.get(style, :position, :static) == :static and
         Map.get(style, :opacity, 1.0) == 1.0 and
         Map.get(style, :background_color) == nil and Map.get(style, :background_image) == nil and
         Map.get(style, :break_before, :auto) in [:auto, :avoid] and
         Map.get(style, :break_after, :auto) in [:auto, :avoid] and
         Enum.all?([:padding, :margin, :border_widths], fn property ->
           case Map.get(style, property, %{top: 0, right: 0, bottom: 0, left: 0}) do
             %{top: top, right: right, bottom: bottom, left: left} ->
               Enum.all?([top, right, bottom, left], &(&1 == 0))

             _ ->
               false
           end
         end) do
      :ok
    else
      unsupported(
        "frameset/frame author box sizing, positioning and fragmentation require a legacy-grid CSS policy; use native track attributes without those declarations"
      )
    end
  end

  @doc "Keeps noframes inactive without parsing or loading its literal fallback source."
  @spec validate_fallback_style(term()) :: :ok | {:error, {atom(), map()}}
  def validate_fallback_style(style) do
    if is_map(style) and Map.get(style, :display) == :none,
      do: :ok,
      else:
        unsupported(
          "visible noframes fallback requires a separate fallback-document selection contract"
        )
  end

  @doc "Diagnoses unsupported frame-target navigation after the initial document has been authorized."
  @spec validate_document_navigation(term()) :: :ok | {:error, {atom(), map()}}
  def validate_document_navigation(dom) do
    case dom do
      %{type: :document, children: children} when is_list(children) -> navigation_nodes(children)
      _ -> invalid("initial frame navigation requires a parsed document")
    end
  end

  defp navigation_nodes(nodes) do
    Enum.reduce_while(nodes, :ok, fn node, :ok ->
      result =
        case node do
          %{type: :element, tag: "template"} ->
            :ok

          %{type: :element, tag: tag, attributes: attributes, children: children}
          when is_map(attributes) and is_list(children) ->
            target = Map.get(attributes, "target", "")

            if tag in ["a", "base", "form"] and
                 (not is_binary(target) or
                    String.downcase(target, :ascii) not in ["", "_self", "_blank"]),
               do:
                 unsupported(
                   "initial frame content cannot execute named, parent or top target navigation; use self/blank destinations"
                 ),
               else: navigation_nodes(children)

          %{type: :text} ->
            :ok

          %{type: :processing_instruction} ->
            :ok

          _ ->
            invalid("frame navigation requires canonical document nodes")
        end

      if result == :ok, do: {:cont, :ok}, else: {:halt, result}
    end)
  end

  @doc "Validates and sizes prepared tracks against a real clipped viewport in points."
  @spec prepare_layout(term(), term(), term()) :: {:ok, map()} | {:error, {atom(), map()}}
  def prepare_layout(plan, width, height) do
    case plan do
      %{
        rows: rows,
        columns: columns,
        border_enabled: enabled,
        border_width: gap,
        border_color: color,
        has_border_color: colored
      }
      when is_boolean(enabled) and is_boolean(colored) and is_list(rows) and is_list(columns) and
             rows != [] and columns != [] ->
        with {:ok, {width, height}} <- HtmlValidator.prepare_viewport_dimensions(width, height),
             :ok <- cells_limit(rows, columns),
             true <-
               match?({:ok, _}, HtmlValidator.normalize_geometry_number(gap)) and gap >= 0 and
                 (enabled or gap == 0) and
                 NativeElixirPdfUtilities.Validators.WriterValidator.valid_color?(color),
             true <-
               Enum.all?(rows ++ columns, fn
                 {kind, value} when kind in [:absolute, :percentage, :relative] ->
                   match?({:ok, _}, HtmlValidator.normalize_geometry_number(value)) and value >= 0 and
                     (kind != :relative or value > 0)

                 _ ->
                   false
               end) do
          HtmlValidator.reserve_render_resource(
            :max_html_tree_work,
            # Charge the bounded allocation/indexing passes as well as cells.
            length(rows) * length(columns) + 24 * (length(rows) + length(columns)),
            :layout
          )

          {:ok,
           %{
             columns: sizes(columns, max(width - gap * (length(columns) - 1), 0)),
             rows: sizes(rows, max(height - gap * (length(rows) - 1), 0)),
             gap: gap,
             border_color: if(colored, do: color, else: {208 / 255, 208 / 255, 208 / 255})
           }}
        else
          false -> invalid("prepared frameset dimensions require canonical nonnegative tracks")
          error -> error
        end

      _ ->
        invalid("frameset layout requires prepared rows, columns and border metadata")
    end
  catch
    {:render_resource_limit, error} -> error
  end

  @doc "Validates a native frameset scene and every child before execution."
  @spec prepare_scene(term(), term(), term(), term()) :: {:ok, map()} | {:error, {atom(), map()}}
  def prepare_scene(node, width, height, depth) do
    case node do
      %{type: :element, tag: "frameset", style: %{_frameset: plan} = style, children: children}
      when is_list(children) and is_integer(depth) and depth > 0 ->
        cond do
          depth > Limits.get(:max_html_depth) ->
            limit("frameset nesting exceeds max_html_depth")

          length(children) > Limits.get(:max_html_nodes) ->
            limit("frameset children exceed max_html_nodes")

          true ->
            with :ok <- validate_style(style),
                 {:ok, grid} <- prepare_layout(plan, width, height),
                 :ok <-
                   Enum.reduce_while(children, :ok, fn child, :ok ->
                     result =
                       case child do
                         %{
                           type: :element,
                           tag: "frameset",
                           style: %{_frameset: _, display: display},
                           children: grandchildren
                         }
                         when is_list(grandchildren) and display in [:block, :none] ->
                           :ok

                         %{
                           type: :element,
                           tag: "frame",
                           style: %{embedded_document: surface, display: :block}
                         }
                         when is_map(surface) ->
                           case NativeElixirPdfUtilities.Validators.EmbeddedViewportValidator.prepare_layout(
                                  surface,
                                  0,
                                  0
                                ) do
                             {:ok, _} -> :ok
                             error -> error
                           end

                         _ ->
                           invalid(
                             "frameset cells require styled nested framesets or issued frame documents"
                           )
                       end

                     if result == :ok, do: {:cont, :ok}, else: {:halt, result}
                   end) do
              with {:ok, borders} <- border_parts(plan, grid, length(children), width, height),
                   do: {:ok, Map.put(grid, :borders, borders)}
            end
        end

      _ ->
        invalid("frameset rendering requires a prepared canonical grid element")
    end
  catch
    {:render_resource_limit, error} -> error
  end

  defp border_parts(plan, grid, child_count, width, height) do
    rows = List.to_tuple(grid.rows)
    columns = List.to_tuple(grid.columns)
    row_count = tuple_size(rows)
    column_count = tuple_size(columns)
    vertical = Map.get(plan, :column_allow_border)
    horizontal = Map.get(plan, :row_allow_border)

    if is_tuple(vertical) and tuple_size(vertical) == column_count + 1 and
         is_tuple(horizontal) and tuple_size(horizontal) == row_count + 1 and
         Enum.all?(Tuple.to_list(vertical) ++ Tuple.to_list(horizontal), &is_boolean/1) do
      count = min(child_count, row_count * column_count)
      HtmlValidator.reserve_render_resource(:max_html_tree_work, count, :layout)

      if count == 0 or grid.gap == 0 or width == 0 or height == 0 do
        {:ok, []}
      else
        {parts, _, _} =
          Enum.reduce(0..(count - 1), {[], 0.0, 0.0}, fn index, {parts, x, y} ->
            column = rem(index, column_count)
            row = div(index, column_count)
            x = x + elem(columns, column)
            vertical_border = column + 1 < column_count and elem(vertical, column + 1)

            parts =
              if(vertical_border,
                do:
                  bevel_parts(x, height - y, grid.gap, height - y, :vertical, grid.border_color) ++
                    parts,
                else: parts
              )

            x = x + if(vertical_border, do: grid.gap, else: 0)

            if column + 1 == column_count and index + 1 < count do
              y = y + elem(rows, row)
              horizontal_border = row + 1 < row_count and elem(horizontal, row + 1)

              parts =
                if(horizontal_border,
                  do:
                    bevel_parts(0.0, height - y, width, grid.gap, :horizontal, grid.border_color) ++
                      parts,
                  else: parts
                )

              {parts, 0.0, y + if(horizontal_border, do: grid.gap, else: 0)}
            else
              {parts, x, y}
            end
          end)

        {:ok, Enum.reverse(parts)}
      end
    else
      invalid("frameset painting requires issued row and column edge flags")
    end
  end

  # Chromium's native frameset bevel uses fixed edge colors, not CSS relief shading.
  defp bevel_parts(x, top, width, height, orientation, color) do
    thickness = if(orientation == :vertical, do: width, else: height)

    HtmlValidator.reserve_render_resource(
      :max_html_tree_work,
      if(thickness >= 2.25, do: 3, else: 1),
      :layout
    )

    fill = %{x: x, y: top - height, width: width, height: height, fill_color: color}

    if thickness >= 2.25 do
      leading = Map.put(fill, :fill_color, {170 / 255, 170 / 255, 170 / 255})
      trailing = Map.put(fill, :fill_color, {0.0, 0.0, 0.0})

      if orientation == :vertical do
        [
          Map.merge(trailing, %{x: x + width - 0.75, width: 0.75}),
          Map.put(leading, :width, 0.75),
          fill
        ]
      else
        [
          Map.merge(trailing, %{height: 0.75}),
          Map.merge(leading, %{y: top - 0.75, height: 0.75}),
          fill
        ]
      end
    else
      [fill]
    end
  end

  defp cells_limit(rows, columns) do
    if length(rows) > Limits.get(:max_frameset_tracks) or
         length(columns) > Limits.get(:max_frameset_tracks) or
         length(rows) * length(columns) > Limits.get(:max_frameset_cells),
       do: limit("frameset grid exceeds max_frameset_tracks or max_frameset_cells"),
       else: :ok
  end

  # Integer CSS-pixel allocation follows the legacy absolute/percent/star priority.
  defp sizes(tracks, points) do
    dimension = trunc(points / 0.75)

    indices =
      tracks
      |> Enum.with_index()
      |> Enum.group_by(fn {{kind, _}, _} -> kind end, &elem(&1, 1))

    fixed = Map.get(indices, :absolute, [])
    percentages = Map.get(indices, :percentage, [])
    relative = Map.get(indices, :relative, [])

    values =
      Enum.with_index(tracks)
      |> Map.new(fn {{kind, value}, index} ->
        {index,
         case kind do
           :absolute -> trunc(value)
           :percentage -> trunc(value * dimension / 100)
           :relative -> 0
         end}
      end)

    total_fixed = Enum.sum(Enum.map(fixed, &Map.fetch!(values, &1)))
    total_percent = Enum.sum(Enum.map(percentages, &Map.fetch!(values, &1)))

    values =
      if total_fixed > dimension,
        do: distribute_tracks(values, fixed, dimension, total_fixed, false),
        else: values

    remaining = dimension - Enum.sum(Enum.map(fixed, &Map.fetch!(values, &1)))

    values =
      if total_percent > remaining,
        do: distribute_tracks(values, percentages, remaining, total_percent, false),
        else: values

    remaining = dimension - Enum.sum(Map.values(values))

    values =
      if relative != [] do
        weights = for {:relative, weight} <- tracks, do: trunc(max(weight, 1))

        total = Enum.sum(weights)

        values =
          Enum.zip(relative, weights)
          |> Enum.reduce(values, fn {index, weight}, values ->
            Map.put(values, index, div(weight * remaining, total))
          end)

        Map.update!(values, List.last(relative), &(&1 + dimension - Enum.sum(Map.values(values))))
      else
        cond do
          percentages != [] and total_percent > 0 ->
            distribute_tracks(values, percentages, remaining, total_percent, true)

          total_fixed > 0 ->
            distribute_tracks(values, fixed, remaining, total_fixed, true)

          true ->
            values
        end
      end

    # Blink distributes any integer-division residue equally, then adds the
    # indivisible final residue to the final authored track.
    remaining = dimension - Enum.sum(Map.values(values))
    residue_indices = if percentages != [], do: percentages, else: fixed
    residue_count = length(residue_indices)

    values =
      Enum.reduce(residue_indices, values, fn index, values ->
        Map.update!(values, index, &(&1 + div(remaining, residue_count)))
      end)

    values =
      Map.update!(values, map_size(values) - 1, &(&1 + dimension - Enum.sum(Map.values(values))))

    Enum.map(0..(map_size(values) - 1), &(Map.fetch!(values, &1) * 0.75))
  end

  defp distribute_tracks(values, indices, space, total, additive) do
    Enum.reduce(indices, values, fn index, result ->
      value = Map.fetch!(values, index)
      allocation = div(value * space, total)
      Map.put(result, index, allocation + if(additive, do: value, else: 0))
    end)
  end

  defp unsupported(message),
    do:
      Diagnostics.error(:layout, :not_implemented, message,
        module: __MODULE__,
        source: "frameset"
      )

  defp invalid(message),
    do:
      Diagnostics.error(:layout, :invalid_document, message,
        module: __MODULE__,
        source: "frameset"
      )

  defp limit(message),
    do:
      Diagnostics.error(:limits, :resource_limit_exceeded, message,
        module: __MODULE__,
        source: "frameset"
      )
end
