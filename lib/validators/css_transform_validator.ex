defmodule NativeElixirPdfUtilities.Validators.CssTransformValidator do
  @moduledoc "Canonical static CSS affine transforms, measured origins and paginated paint scopes."
  alias NativeElixirPdfUtilities.Diagnostics
  alias NativeElixirPdfUtilities.HtmlToPdf.{AffineGeometry, CssParser}

  alias NativeElixirPdfUtilities.Validators.{
    CssMathValidator,
    CssPositionValidator,
    CssValueValidator,
    HtmlValidator
  }

  @identity {1.0, 0.0, 0.0, 1.0, 0.0, 0.0}

  @doc "Prepares transform declarations while deferring used border-box percentages and selected font units."
  @spec apply(map(), String.t(), String.t()) :: {:ok, map()} | :error | {:error, {atom(), map()}}
  def apply(style, property, value) do
    value = value |> CssParser.strip_comments() |> HtmlValidator.trim_css_whitespace()

    keyword =
      case HtmlValidator.parse_css_identifier(value) do
        {:ok, keyword, ""} -> String.downcase(keyword, :ascii)
        _ -> value
      end

    {key, initial, inherited} =
      if property == "transform",
        do: {:css_transform, :none, :_inherited_css_transform},
        else:
          {:transform_origin, {{:percent, 0.5}, {:percent, 0.5}}, :_inherited_transform_origin}

    metric_key =
      if property == "transform",
        do: :css_transform_metric_style,
        else: :transform_origin_metric_style

    inherited_metric_key =
      if property == "transform",
        do: :_inherited_css_transform_metrics,
        else: :_inherited_transform_origin_metrics

    case keyword do
      "inherit" ->
        {:ok,
         style
         |> Map.put(key, Map.get(style, inherited, initial))
         |> Map.put(
           metric_key,
           Map.get(style, inherited_metric_key) || metric_style(style, property)
         )}

      keyword when keyword in ["initial", "unset", "revert", "revert-layer"] ->
        {:ok, style |> Map.delete(metric_key) |> Map.put(key, initial)}

      _ ->
        result =
          if property == "transform", do: prepare(value), else: prepare_origin(value, style)

        with {:ok, prepared} <- result,
             do: {:ok, style |> Map.delete(metric_key) |> Map.put(key, prepared)}
    end
  end

  @doc "Retains the selected computed font and viewport metrics for explicit CSS inheritance."
  @spec metric_style(map(), String.t()) :: map()
  def metric_style(style, property) do
    key =
      if property == "transform",
        do: :css_transform_metric_style,
        else: :transform_origin_metric_style

    Map.get(style, key) ||
      Map.take(style, [
        :font_face,
        :font_size,
        :line_height,
        :_root_font_face,
        :_root_font_size,
        :_root_line_height,
        :_position_viewport
      ])
  end

  @doc "Retains parent metrics only when an authored transform or origin can need explicit inheritance."
  @spec inherited_metric_style(map(), String.t()) :: map() | nil
  def inherited_metric_style(style, property) do
    authored =
      if property == "transform",
        do: Map.get(style, :css_transform, :none) != :none,
        else: Map.get(style, :transform_origin) not in [nil, {{:percent, 0.5}, {:percent, 0.5}}]

    if authored, do: metric_style(style, property), else: nil
  end

  @doc "Parses a CSS 2D transform function list through shared CSS lexical and numeric grammar."
  @spec prepare(term()) :: {:ok, :none | [tuple()]} | :error | {:error, {atom(), map()}}
  def prepare(value) do
    HtmlValidator.with_render_budget(fn ->
      case value do
        value when is_binary(value) ->
          value = value |> CssParser.strip_comments() |> HtmlValidator.trim_css_whitespace()

          case HtmlValidator.parse_css_identifier(value) do
            {:ok, keyword, ""} ->
              if String.downcase(keyword, :ascii) == "none", do: {:ok, :none}, else: :error

            _ ->
              functions(value, [], 0)
          end

        _ ->
          :error
      end
    end)
  end

  defp functions(source, operations, count) do
    source = HtmlValidator.trim_css_whitespace(source)

    case source do
      "" ->
        if operations == [], do: :error, else: {:ok, Enum.reverse(operations)}

      _ ->
        HtmlValidator.check_render_resource(:max_css_transform_operations, count + 1, :css)

        with {:ok, name, "(" <> rest} <- HtmlValidator.parse_css_identifier(source),
             {:ok, args, remaining} <- function_arguments(rest),
             name = String.downcase(name, :ascii),
             {:ok, prepared} <- prepare_operation(name, args) do
          HtmlValidator.reserve_render_resource(:max_css_work, byte_size(args) + 1, :css)
          functions(remaining, [prepared | operations], count + 1)
        else
          _ -> :error
        end
    end
  end

  defp function_arguments(source) do
    units = HtmlValidator.css_component_units(source)

    result =
      units
      |> Enum.with_index()
      |> Enum.reduce_while({[], 1}, fn {unit, index}, {args, depth} ->
        cond do
          unit == ")" and depth == 1 ->
            {:halt,
             {:ok, args |> Enum.reverse() |> Enum.join(),
              units |> Enum.drop(index + 1) |> Enum.join()}}

          unit == "(" ->
            HtmlValidator.check_render_resource(:max_css_calculation_depth, depth, :css)
            {:cont, {[unit | args], depth + 1}}

          unit == ")" ->
            {:cont, {[unit | args], depth - 1}}

          true ->
            {:cont, {[unit | args], depth}}
        end
      end)

    case result do
      {:ok, _, _} -> result
      _ -> :error
    end
  end

  defp prepare_operation(name, source) do
    arguments = HtmlValidator.split_css_comma(source)

    roles =
      case {name, length(arguments)} do
        {"matrix", 6} -> [:number, :number, :number, :number, :number, :number]
        {"translate", 1} -> [:length_x]
        {"translate", 2} -> [:length_x, :length_y]
        {"translatex", 1} -> [:length_x]
        {"translatey", 1} -> [:length_y]
        {"scale", count} when count in [1, 2] -> List.duplicate(:scale, count)
        {name, 1} when name in ["scalex", "scaley"] -> [:scale]
        {name, 1} when name in ["rotate", "skewx", "skewy"] -> [:angle]
        {"skew", count} when count in [1, 2] -> List.duplicate(:angle, count)
        _ -> nil
      end

    if roles != nil and not Regex.match?(~r/\A\s*,|,\s*\z|,\s*,/, source) do
      prepared = Enum.zip(roles, arguments)

      valid =
        Enum.all?(prepared, fn {role, source} ->
          environment = %{type_check: true, width: 1.0, height: 1.0}

          environment =
            case role do
              role when role in [:length_x, :length_y] ->
                Map.put(environment, :percentage_basis, 1.0)

              :scale ->
                Map.merge(environment, %{percentage_basis: 1.0, percentage_dimension: :number})

              _ ->
                environment
            end

          case CssMathValidator.evaluate(source, environment) do
            {:ok, value, type} ->
              case role do
                role when role in [:length_x, :length_y] ->
                  type == %{length: 1} or (value == 0 and type == %{})

                :angle ->
                  type == %{angle: 1} or (value == 0 and type == %{})

                _ ->
                  type == %{}
              end

            _ ->
              false
          end
        end)

      if valid, do: {:ok, {name, prepared}}, else: :error
    else
      :error
    end
  end

  @doc "Prepares transform-origin through the shared CSS position grammar, with an optional nonpercentage Z length."
  @spec prepare_origin(String.t(), map()) :: {:ok, tuple()} | :error
  def prepare_origin(value, style) do
    case CssMathValidator.component_tokens(value, false) do
      {:ok, parts} when length(parts) in [1, 2] ->
        CssValueValidator.parse_two_axis_position(value, Map.get(style, :font_size, 12), false)

      {:ok, [x, y, z]} ->
        with {:ok, z_value, z_type} <-
               CssMathValidator.evaluate(z, %{type_check: true, width: 1.0, height: 1.0}),
             true <- z_type == %{length: 1} or (z_value == 0 and z_type == %{}),
             {:ok, origin} <-
               CssValueValidator.parse_two_axis_position(
                 x <> " " <> y,
                 Map.get(style, :font_size, 12),
                 false
               ) do
          {:ok, origin}
        else
          _ -> :error
        end

      _ ->
        :error
    end
  end

  @doc "Checks caller-supplied prepared style transforms using the canonical function and position validators."
  @spec valid_style?(term()) :: boolean()
  def valid_style?(style) do
    if is_map(style) do
      operations = Map.get(style, :css_transform, :none)

      valid_operations =
        case operations do
          :none ->
            true

          operations when is_list(operations) and operations != [] ->
            HtmlValidator.check_render_resource(
              :max_css_transform_operations,
              length(operations),
              :layout
            )

            Enum.all?(operations, fn operation ->
              case operation do
                {name, arguments} when is_binary(name) and is_list(arguments) ->
                  if Enum.all?(arguments, &match?({_role, source} when is_binary(source), &1)) do
                    sources = Enum.map(arguments, &elem(&1, 1))

                    Enum.all?(sources, &String.valid?/1) and
                      prepare_operation(name, Enum.join(sources, ",")) == {:ok, operation}
                  else
                    false
                  end

                _ ->
                  false
              end
            end)

          _ ->
            false
        end

      valid_operations and
        case Map.get(style, :transform_origin, {{:percent, 0.5}, {:percent, 0.5}}) do
          {x, y} ->
            CssPositionValidator.valid_prepared?(x) and CssPositionValidator.valid_prepared?(y)

          _ ->
            false
        end
    else
      false
    end
  end

  @doc "Resolves a prepared CSS transform against the actual point border box and selected font/viewport metrics."
  @spec resolve(map(), map()) :: {:ok, tuple() | :none | :empty} | {:error, {atom(), map()}}
  def resolve(style, %{x: x, top: top, width: width, height: height}) do
    HtmlValidator.with_render_budget(fn ->
      case Map.get(style, :css_transform, :none) do
        :none ->
          {:ok, :none}

        operations ->
          with {:ok, environment} <-
                 CssPositionValidator.math_environment(
                   Map.merge(style, metric_style(style, "transform")),
                   width
                 ),
               {:ok, matrix} <-
                 Enum.reduce_while(operations, {:ok, @identity}, fn {name, args}, {:ok, matrix} ->
                   values =
                     Enum.map(args, fn {role, source} ->
                       environment =
                         case role do
                           :length_y ->
                             Map.put(environment, :percentage_basis, height / 0.75)

                           :scale ->
                             Map.merge(environment, %{
                               percentage_basis: 1.0,
                               percentage_dimension: :number
                             })

                           :number ->
                             Map.delete(environment, :percentage_basis)

                           :angle ->
                             Map.delete(environment, :percentage_basis)

                           _ ->
                             environment
                         end

                       CssMathValidator.evaluate(source, environment)
                     end)

                   if Enum.all?(values, &match?({:ok, value, _} when is_number(value), &1)) do
                     values = Enum.map(values, &elem(&1, 1))

                     combined =
                       AffineGeometry.multiply(matrix, operation_matrix(name, values))

                     Enum.each(
                       Tuple.to_list(combined),
                       &HtmlValidator.check_render_resource(
                         :max_pdf_numeric_magnitude,
                         abs(&1),
                         :layout
                       )
                     )

                     {:cont, {:ok, combined}}
                   else
                     {:halt,
                      error(
                        "CSS transform calculation requires finite used values",
                        inspect(args)
                      )}
                   end
                 end),
               {origin_x, origin_y} =
                 Map.get(style, :transform_origin, {{:percent, 0.5}, {:percent, 0.5}}),
               {:ok, origin_x} <-
                 CssPositionValidator.resolve(
                   origin_x,
                   width,
                   Map.merge(style, metric_style(style, "transform-origin"))
                 ),
               {:ok, origin_y} <-
                 CssPositionValidator.resolve(
                   origin_y,
                   height,
                   Map.merge(style, metric_style(style, "transform-origin"))
                 ) do
            {a, b, c, d, e, f} = matrix
            ox = x + origin_x
            oy = top - origin_y

            matrix =
              {a, -b, -c, d, ox - a * ox + c * oy + e * 0.75, oy + b * ox - d * oy - f * 0.75}

            Enum.each(
              Tuple.to_list(matrix),
              &HtmlValidator.check_render_resource(:max_pdf_numeric_magnitude, abs(&1), :layout)
            )

            if a * d - b * c == 0, do: {:ok, :empty}, else: {:ok, matrix}
          end
      end
    end)
  end

  defp operation_matrix(name, values) do
    case {name, values} do
      {"matrix", [a, b, c, d, e, f]} ->
        {a, b, c, d, e, f}

      {"translate", [x]} ->
        {1.0, 0.0, 0.0, 1.0, x, 0.0}

      {"translate", [x, y]} ->
        {1.0, 0.0, 0.0, 1.0, x, y}

      {"translatex", [x]} ->
        {1.0, 0.0, 0.0, 1.0, x, 0.0}

      {"translatey", [y]} ->
        {1.0, 0.0, 0.0, 1.0, 0.0, y}

      {"scale", [x]} ->
        {x, 0.0, 0.0, x, 0.0, 0.0}

      {"scale", [x, y]} ->
        {x, 0.0, 0.0, y, 0.0, 0.0}

      {"scalex", [x]} ->
        {x, 0.0, 0.0, 1.0, 0.0, 0.0}

      {"scaley", [y]} ->
        {1.0, 0.0, 0.0, y, 0.0, 0.0}

      {"rotate", [angle]} ->
        {:math.cos(angle), :math.sin(angle), -:math.sin(angle), :math.cos(angle), 0.0, 0.0}

      {"skew", [x]} ->
        {1.0, 0.0, :math.tan(x), 1.0, 0.0, 0.0}

      {"skew", [x, y]} ->
        {1.0, :math.tan(y), :math.tan(x), 1.0, 0.0, 0.0}

      {"skewx", [x]} ->
        {1.0, 0.0, :math.tan(x), 1.0, 0.0, 0.0}

      {"skewy", [y]} ->
        {1.0, :math.tan(y), 0.0, 1.0, 0.0, 0.0}
    end
  end

  @doc "Records the actual used element border box before ordinary relative placement."
  @spec mark_border_box(map(), [map()], map()) :: [map()]
  def mark_border_box(style, boxes, bounds) do
    if Map.get(style, :css_transform, :none) == :none do
      boxes
    else
      {breaks, boxes} = Enum.split_while(boxes, &(Map.get(&1, :type) == :page_break))

      boxes =
        if boxes == [],
          do: [
            %{
              type: HtmlValidator.reserve_layout_box(:flow_marker),
              x: bounds.x,
              y: bounds.top,
              width: 0.0,
              height: 0.0
            }
          ],
          else: boxes

      [first | rest] = boxes
      breaks ++ [Map.put(first, :css_transform_border_box, bounds) | rest]
    end
  end

  @doc "Attaches an actual element paint scope after used size and positioned child placement."
  @spec attach_scope(map(), [map()]) :: {:ok, [map()]} | {:error, {atom(), map()}}
  def attach_scope(style, boxes) do
    bounds = Enum.find_value(boxes, &Map.get(&1, :css_transform_border_box))

    if bounds == nil do
      {:ok, boxes}
    else
      with {:ok, scope} <- prepare_scope(style, bounds) do
        source = Enum.find(boxes, &(Map.get(&1, :css_transform_border_box) != nil))

        scope =
          if scope == nil,
            do: nil,
            else: Map.put(scope, :paint_fragment_id, Map.get(source, :fieldset_paint_id))

        {:ok,
         Enum.map(boxes, fn box ->
           box = Map.delete(box, :css_transform_border_box)

           if box.type == :page_break or scope == nil do
             box
           else
             scopes = [scope | Map.get(box, :css_transform_scopes, [])]

             HtmlValidator.check_render_resource(
               :max_drawing_group_depth,
               length(scopes),
               :layout
             )

             Map.put(box, :css_transform_scopes, scopes)
           end
         end)}
      end
    end
  end

  @doc "Issues one measured paint scope for an actual source box, including shared table row groups."
  @spec prepare_scope(map(), map()) :: {:ok, map() | nil} | {:error, {atom(), map()}}
  def prepare_scope(style, bounds) do
    with {:ok, matrix} <- resolve(style, bounds) do
      {:ok,
       if(matrix == :none,
         do: nil,
         else: %{
           id: make_ref(),
           matrix: matrix,
           border_box: bounds,
           style:
             Map.take(style, [
               :css_transform,
               :transform_origin,
               :css_transform_metric_style,
               :transform_origin_metric_style,
               :font_face,
               :font_size,
               :line_height,
               :_root_font_face,
               :_root_font_size,
               :_root_line_height,
               :_position_viewport
             ])
         }
       )}
    end
  end

  @doc "Checks retained CSS transform paint scopes through the shared geometry and depth bounds."
  @spec validate_scopes(term()) :: :ok | {:error, {atom(), map()}}
  def validate_scopes(scopes) do
    if is_list(scopes) do
      HtmlValidator.check_render_resource(:max_drawing_group_depth, length(scopes), :layout)

      valid =
        Enum.all?(scopes, fn scope ->
          case scope do
            %{id: id, matrix: matrix, border_box: %{x: x, top: top, width: width, height: height}} ->
              is_reference(id) and is_number(width) and width >= 0 and is_number(height) and
                height >= 0 and
                Enum.all?(
                  [x, top, width, height],
                  &match?({:ok, _}, HtmlValidator.normalize_geometry_number(&1))
                ) and
                (matrix == :empty or
                   (is_tuple(matrix) and tuple_size(matrix) == 6 and
                      Enum.all?(
                        Tuple.to_list(matrix),
                        &match?({:ok, _}, HtmlValidator.normalize_geometry_number(&1))
                      )))

            _ ->
              false
          end
        end)

      if valid,
        do: :ok,
        else:
          error(
            "CSS transform scopes require actual bounded border boxes, identities and six-value matrices",
            "css_transform_scopes"
          )
    else
      error("CSS transform paint scopes require a list", "css_transform_scopes")
    end
  end

  @doc "Materializes actual paginated CSS paint scopes without changing their source flow or point coordinates."
  @spec materialize([map()], keyword()) :: {:ok, [map()]} | {:error, {atom(), map()}}
  @spec materialize([map()], keyword(), term()) :: {:ok, [map()]} | {:error, {atom(), map()}}
  def materialize(
        pages,
        opts,
        context \\ NativeElixirPdfUtilities.HtmlToPdf.ResourceContext.current()
      ) do
    alias NativeElixirPdfUtilities.Validators.{EmbeddedFormValidator, HtmlFormValidator}

    if Enum.all?(pages, fn page ->
         not scoped_boxes?(page.boxes)
       end) do
      {:ok, pages}
    else
      with :ok <-
             Enum.reduce_while(pages, :ok, fn page, :ok ->
               case validate_box_scopes(page.boxes) do
                 :ok -> {:cont, :ok}
                 error -> {:halt, error}
               end
             end) do
        pages = prepare_fragment_scopes(pages, Keyword.get(opts, :css_fragmentation_margins))

        pages =
          Enum.map(pages, fn page ->
            case prepare_group_forms(page.boxes, page.size, opts, context, 0) do
              {:ok, boxes} -> %{page | boxes: boxes}
              {:error, _} = error -> throw({:render_resource_limit, error})
            end
          end)

        controls =
          Enum.flat_map(pages, fn page ->
            Enum.filter(page.boxes, &is_map(Map.get(&1, :form_control)))
          end)

        prepared =
          if controls == [],
            do: {:ok, pages, []},
            else: HtmlFormValidator.prepare_scoped(pages, opts, context)

        with {:ok, pages, fields} <- prepared do
          pages =
            if Keyword.get(opts, :forms, :interactive) == :static do
              Enum.map(pages, fn page ->
                %{page | boxes: Enum.map(page.boxes, &Map.drop(&1, [:form_control, :form_owner]))}
              end)
            else
              pages
            end

          ancestry =
            Map.new(controls, &{&1.form_control.id, Map.get(&1, :css_transform_scopes, [])})

          fields =
            Enum.map(
              fields,
              &Map.put(&1, :css_transform_scopes, Map.get(ancestry, &1.html_control_id, []))
            )

          # The complete field list and live context have already passed scoped
          # preparation. Issuing unchanged subsets preserves that contract.
          pages =
            Enum.reduce(pages, [], fn page, acc ->
              own = Enum.filter(fields, &(&1.page == length(acc) + 1))

              {:ok, boxes, _claimed} =
                scope_boxes(page.boxes, page.size, own, context, MapSet.new())

              unscoped =
                own
                |> Enum.filter(&(Map.get(&1, :css_transform_scopes, []) == []))
                |> Enum.map(&Map.delete(&1, :css_transform_scopes))

              carrier =
                if unscoped == [] do
                  []
                else
                  {:ok, descriptor} = EmbeddedFormValidator.issue(context, unscoped)

                  [
                    %{
                      type: HtmlValidator.reserve_layout_box(:drawing_group),
                      width: elem(page.size, 0),
                      height: elem(page.size, 1),
                      transform: @identity,
                      boxes: [],
                      embedded_widgets: descriptor
                    }
                  ]
                end

              [%{page | boxes: boxes ++ carrier} | acc]
            end)

          {:ok,
           prepare_print_slices(
             Enum.reverse(pages),
             Keyword.get(opts, :css_fragmentation_margins)
           )}
        end
      end
    end
  end

  # Transform fragments first; then slice the continuous print-content strip
  # into its genuine page viewports. Adjacent existing pages receive actual
  # overflow paint, with source text, clips and target identities intact.
  defp prepare_print_slices(pages, margins) do
    alias NativeElixirPdfUtilities.HtmlToPdf.DrawingGeometry

    if is_map(margins) do
      sources =
        pages
        |> Enum.with_index()
        |> Enum.flat_map(fn {page, index} ->
          Enum.filter(
            page.boxes,
            &(Map.get(&1, :role) == :css_transform_plane and
                Map.get(&1, :position_anchor) != :fixed)
          )
          |> Enum.map(fn group ->
            {group, index, print_paint_bounds(group, {1, 1, 0, 0, nil, elem(page.size, 1)})}
          end)
        end)

      last = List.last(pages)
      height = elem(last.size, 1) - margins.top - margins.bottom
      content_top = elem(last.size, 1) - margins.top

      page_count =
        Enum.reduce(sources, length(pages), fn {_group, source, bounds}, count ->
          case bounds do
            nil ->
              count

            {_left, bottom, _right, _top} ->
              max(count, ceil((source * height + content_top - bottom) / height))
          end
        end)

      HtmlValidator.check_render_resource(:max_rendered_pages, page_count, :pagination)
      HtmlValidator.check_render_resource(:max_pdf_pages, page_count, :pagination)
      canvas = Enum.filter(hd(pages).boxes, &(Map.get(&1, :position_anchor) == :canvas))

      HtmlValidator.reserve_render_resource(
        :max_layout_boxes,
        (page_count - length(pages)) * length(canvas),
        :pagination
      )

      pages = pages ++ List.duplicate(%{last | boxes: canvas}, page_count - length(pages))

      work =
        length(pages) * Enum.reduce(pages, 0, fn page, count -> count + length(page.boxes) end)

      HtmlValidator.reserve_render_resource(:max_css_work, work, :pagination)

      Enum.with_index(pages)
      |> Enum.map(fn {page, target} ->
        height = elem(page.size, 1) - margins.top - margins.bottom

        viewport = %{
          x: margins.left,
          y: margins.bottom,
          width: elem(page.size, 0) - margins.left - margins.right,
          height: height
        }

        carried =
          Enum.flat_map(sources, fn {group, source, bounds} ->
            case bounds do
              {_left, bottom, _right, top} when source != target ->
                delta = (target - source) * height

                if top + delta > viewport.y and bottom + delta < viewport.y + viewport.height do
                  [DrawingGeometry.translate_box(group, 0.0, delta) |> issue_page_widget_slices()]
                else
                  []
                end

              _ ->
                []
            end
          end)

        HtmlValidator.reserve_render_resource(:max_layout_boxes, length(carried), :pagination)
        boxes = carried ++ page.boxes

        boxes =
          Enum.map(boxes, fn box ->
            if Map.get(box, :role) == :css_transform_plane do
              %{
                type: HtmlValidator.reserve_layout_box(:drawing_group),
                role: :overflow_clip,
                width: elem(page.size, 0),
                height: elem(page.size, 1),
                x: 0.0,
                y: 0.0,
                transform: @identity,
                clip: viewport,
                boxes: [box]
              }
            else
              box
            end
          end)

        %{page | boxes: boxes}
      end)
    else
      pages
    end
  end

  # Overflow extents include the real descendant paint plane, not just the
  # outer border box. Exact affine clips are applied before taking sheet bounds.
  defp print_paint_bounds(box, composition) do
    alias NativeElixirPdfUtilities.HtmlToPdf.DrawingGeometry
    alias NativeElixirPdfUtilities.Validators.{EmbeddedFormValidator, ClipGeometryValidator}

    HtmlValidator.reserve_render_resource(:max_css_work, 1, :pagination)
    {transform, clip, height} = DrawingGeometry.composition_parts(composition)

    case box do
      %{type: :drawing_group, boxes: children} ->
        own = DrawingGeometry.compose_group(box, composition)
        {matrix, visible, _} = DrawingGeometry.composition_parts(own)

        rectangles =
          case Map.get(box, :css_fragment_bounds) do
            nil ->
              []

            bounds ->
              [
                %{
                  x: bounds.x,
                  y: bounds.top - bounds.height,
                  width: bounds.width,
                  height: bounds.height
                }
              ]
          end

        rectangles =
          case Map.get(box, :embedded_widgets) do
            nil ->
              rectangles

            descriptor ->
              {:ok, fields} = EmbeddedFormValidator.resolve(descriptor)

              rectangles ++
                Enum.map(fields, fn %{rect: [left, bottom, right, top]} ->
                  %{x: left, y: bottom, width: right - left, height: top - bottom}
                end)
          end

        bounds =
          Enum.map(children, &print_paint_bounds(&1, own)) ++
            Enum.map(
              rectangles,
              &print_paint_bounds(&1, %{transform: matrix, clip: visible, height: height})
            )

        join_print_bounds(bounds)

      %{type: :glyph_group, boxes: children} ->
        children |> Enum.map(&print_paint_bounds(&1, composition)) |> join_print_bounds()

      %{type: type} when type in [:flow_marker, :html_target, :page_break] ->
        nil

      %{x: x, y: y, width: width, height: box_height} when width > 0 and box_height > 0 ->
        clip =
          DrawingGeometry.intersect_clip(
            clip,
            DrawingGeometry.transform_box_clip(Map.get(box, :clip), transform)
          )

        points =
          Enum.map(
            [{x, y}, {x + width, y}, {x + width, y + box_height}, {x, y + box_height}],
            fn {px, py} -> DrawingGeometry.transform_point(px, py, transform) end
          )

        xs = Enum.map(points, &elem(&1, 0))
        ys = Enum.map(points, &elem(&1, 1))
        viewport = {Enum.min(xs), Enum.min(ys), Enum.max(xs), Enum.max(ys)}

        with {:ok, quads} <-
               ClipGeometryValidator.project(
                 DrawingGeometry.clip_expression(clip),
                 points,
                 viewport
               ) do
          coordinates = List.flatten(quads)

          if coordinates == [] do
            nil
          else
            points = Enum.chunk_every(coordinates, 2)

            {Enum.min(Enum.map(points, &hd/1)), Enum.min(Enum.map(points, &List.last/1)),
             Enum.max(Enum.map(points, &hd/1)), Enum.max(Enum.map(points, &List.last/1))}
          end
        else
          {:error, _} = failure -> throw({:render_resource_limit, failure})
        end

      _ ->
        nil
    end
  end

  defp join_print_bounds(bounds) do
    case Enum.reject(bounds, &is_nil/1) do
      [] ->
        nil

      bounds ->
        {Enum.min(Enum.map(bounds, &elem(&1, 0))), Enum.min(Enum.map(bounds, &elem(&1, 1))),
         Enum.max(Enum.map(bounds, &elem(&1, 2))), Enum.max(Enum.map(bounds, &elem(&1, 3)))}
    end
  end

  # Each physical widget slice retains one issued source field and real AP
  # graph, while its descriptor token identifies this distinct page projection.
  defp issue_page_widget_slices(box) do
    case NativeElixirPdfUtilities.Validators.EmbeddedFormValidator.issue_drawing_slice(box) do
      {:ok, slice} -> slice
      {:error, _} = failure -> throw({:render_resource_limit, failure})
    end
  end

  # Overflow and row viewports already exist in source flow. Prepare their
  # genuine local control plane before the CSS affine wrappers are emitted.
  defp prepare_group_forms(boxes, size, opts, context, depth) do
    alias NativeElixirPdfUtilities.Validators.{EmbeddedFormValidator, HtmlFormValidator}
    HtmlValidator.check_render_resource(:max_drawing_group_depth, depth, :layout)

    Enum.reduce_while(boxes, {:ok, []}, fn box, {:ok, acc} ->
      prepared =
        case box do
          %{type: :drawing_group, boxes: children} ->
            child_size =
              if Map.get(box, :role) == :overflow_clip, do: size, else: {box.width, box.height}

            with {:ok, children} <-
                   prepare_group_forms(children, child_size, opts, context, depth + 1),
                 {:ok, [page]} <-
                   materialize([%{size: child_size, boxes: children}], opts, context),
                 {:ok, [page], fields} <- HtmlFormValidator.prepare_scoped([page], opts, context),
                 {:ok, descriptor} <-
                   if(fields == [],
                     do: {:ok, nil},
                     else: EmbeddedFormValidator.issue(context, fields)
                   ) do
              children =
                if Keyword.get(opts, :forms, :interactive) == :static,
                  do: Enum.map(page.boxes, &Map.drop(&1, [:form_control, :form_owner])),
                  else: page.boxes

              group = %{box | boxes: children}

              if descriptor == nil do
                {:ok, group}
              else
                # A raw plane cannot already own an issued field appearance.
                if Map.get(group, :embedded_widgets) != nil do
                  error(
                    "A source control plane cannot mix raw controls with an already issued field descriptor",
                    "embedded_widgets"
                  )
                else
                  {:ok, Map.put(group, :embedded_widgets, descriptor)}
                end
              end
            end

          _ ->
            {:ok, box}
        end

      case prepared do
        {:ok, box} -> {:cont, {:ok, [box | acc]}}
        error -> {:halt, error}
      end
    end)
    |> case do
      {:ok, boxes} -> {:ok, Enum.reverse(boxes)}
      error -> error
    end
  end

  # CSS fragmentation defines an origin and percentage reference for each
  # physical border fragment, after layout and before graphical transforms.
  defp prepare_fragment_scopes(pages, margins) do
    membership =
      pages
      |> Enum.with_index()
      |> Enum.reduce(%{}, fn {page, index}, acc ->
        Enum.reduce(page.boxes, acc, fn box, acc ->
          Enum.reduce(Map.get(box, :css_transform_scopes, []), acc, fn scope, acc ->
            Map.update(acc, scope.id, MapSet.new([index]), &MapSet.put(&1, index))
          end)
        end)
      end)

    Enum.map(pages, fn page ->
      scopes =
        page.boxes
        |> Enum.flat_map(&Map.get(&1, :css_transform_scopes, []))
        |> Enum.uniq_by(& &1.id)

      paint_fragments =
        page.boxes
        |> Enum.filter(&(Map.get(&1, :type) == :rect and Map.has_key?(&1, :fieldset_paint_id)))
        |> Map.new(&{&1.fieldset_paint_id, &1})

      resolved =
        Map.new(scopes, fn scope ->
          fragmented = MapSet.size(Map.fetch!(membership, scope.id)) > 1

          paint = Map.get(paint_fragments, Map.get(scope, :paint_fragment_id))

          bounds =
            cond do
              not fragmented ->
                scope.border_box

              paint != nil ->
                %{
                  x: paint.x,
                  top: paint.y + paint.height,
                  width: paint.width,
                  height: paint.height
                }

              is_map(margins) ->
                top = min(scope.border_box.top, elem(page.size, 1) - margins.top)
                bottom = max(scope.border_box.top - scope.border_box.height, margins.bottom)
                %{scope.border_box | top: top, height: max(top - bottom, 0.0)}

              true ->
                scope.border_box
            end

          {:ok, matrix} = resolve(scope.style, bounds)
          {scope.id, %{scope | matrix: matrix, border_box: bounds}}
        end)

      %{
        page
        | boxes:
            Enum.map(page.boxes, fn box ->
              if Map.has_key?(box, :css_transform_scopes),
                do:
                  Map.update!(
                    box,
                    :css_transform_scopes,
                    &Enum.map(&1, fn scope -> Map.fetch!(resolved, scope.id) end)
                  ),
                else: box
            end)
      }
    end)
  end

  # Layout scopes can survive inside genuine overflow and table fragment viewports.
  defp scoped_boxes?(boxes) do
    Enum.any?(boxes, fn box ->
      Map.get(box, :css_transform_scopes, []) != [] or
        (Map.get(box, :type) == :drawing_group and scoped_boxes?(Map.get(box, :boxes, [])))
    end)
  end

  defp validate_box_scopes(boxes) do
    Enum.reduce_while(boxes, :ok, fn box, :ok ->
      result =
        with :ok <- validate_scopes(Map.get(box, :css_transform_scopes, [])) do
          if Map.get(box, :type) == :drawing_group,
            do: validate_box_scopes(Map.get(box, :boxes, [])),
            else: :ok
        end

      if result == :ok, do: {:cont, :ok}, else: {:halt, result}
    end)
  end

  defp scope_boxes(boxes, size, fields, context, claimed) do
    alias NativeElixirPdfUtilities.Validators.EmbeddedFormValidator

    case boxes do
      [] ->
        {:ok, [], claimed}

      [box | rest] ->
        case Map.get(box, :css_transform_scopes, []) do
          [] ->
            prepared =
              case box do
                %{type: :drawing_group, boxes: children} ->
                  child_size =
                    if Map.get(box, :role) == :overflow_clip,
                      do: size,
                      else: {box.width, box.height}

                  with {:ok, children, claimed} <-
                         scope_boxes(children, child_size, fields, context, claimed),
                       do: {:ok, %{box | boxes: children}, claimed}

                _ ->
                  {:ok, box, claimed}
              end

            with {:ok, box, claimed} <- prepared,
                 {:ok, tail, claimed} <- scope_boxes(rest, size, fields, context, claimed),
                 do: {:ok, [box | tail], claimed}

          [scope | _] ->
            {members, remaining} =
              Enum.split_while(boxes, fn box ->
                case Map.get(box, :css_transform_scopes, []) do
                  [%{id: id} | _] -> id == scope.id
                  _ -> false
                end
              end)

            members =
              Enum.map(
                members,
                &Map.update!(&1, :css_transform_scopes, fn [_ | tail] -> tail end)
              )

            own =
              Enum.filter(fields, fn field ->
                case Map.get(field, :css_transform_scopes, []) do
                  [%{id: id}] ->
                    id == scope.id and not MapSet.member?(claimed, field.html_control_id)

                  _ ->
                    false
                end
              end)
              |> Enum.map(&Map.delete(&1, :css_transform_scopes))

            nested_fields =
              Enum.flat_map(fields, fn field ->
                case Map.get(field, :css_transform_scopes, []) do
                  [%{id: id} | tail] when id == scope.id ->
                    [Map.put(field, :css_transform_scopes, tail)]

                  _ ->
                    []
                end
              end)

            with {:ok, children, claimed} <-
                   scope_boxes(members, size, nested_fields, context, claimed),
                 {:ok, descriptor} <-
                   if(own == [], do: {:ok, nil}, else: EmbeddedFormValidator.issue(context, own)),
                 claimed = Enum.reduce(own, claimed, &MapSet.put(&2, &1.html_control_id)),
                 {:ok, tail, claimed} <- scope_boxes(remaining, size, fields, context, claimed) do
              group = %{
                type: HtmlValidator.reserve_layout_box(:drawing_group),
                role: :css_transform_plane,
                css_fragment_bounds: scope.border_box,
                x: 0.0,
                y: 0.0,
                width: elem(size, 0),
                height: elem(size, 1),
                transform: if(scope.matrix == :empty, do: @identity, else: scope.matrix),
                boxes: children
              }

              group =
                if Enum.all?(members, &(Map.get(&1, :position_anchor) == :fixed)),
                  do: Map.put(group, :position_anchor, :fixed),
                  else: group

              group =
                if scope.matrix == :empty,
                  do: Map.put(group, :clip, %{x: 0.0, y: 0.0, width: 0.0, height: 0.0}),
                  else: group

              group =
                if descriptor == nil,
                  do: group,
                  else: Map.put(group, :embedded_widgets, descriptor)

              {:ok, [group | tail], claimed}
            end
        end
    end
  end

  defp error(message, source) do
    Diagnostics.error(:layout, :invalid_layout, message,
      module: __MODULE__,
      operation: :resolve_css_transform,
      source: source
    )
  end
end
