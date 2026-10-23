defmodule NativeElixirPdfUtilities.Validators.ImageMapValidator do
  @moduledoc "Canonical scoped HTML image-map association, area parsing and ordered hit testing."
  alias NativeElixirPdfUtilities.Diagnostics
  alias NativeElixirPdfUtilities.Validators.HtmlValidator

  # Fixed native layout precision, not a configurable resource ceiling.
  # Circle polygons use the same 1/64 CSS-pixel bound for their sagitta.
  @circle_precision 0.75 / 64

  @doc false
  @spec circle_segments(number()) :: pos_integer()
  def circle_segments(radius) do
    point_radius = radius * 0.75

    count =
      if point_radius <= @circle_precision do
        4
      else
        # acos(1 - precision/radius) loses precision for large radii.
        # The half-angle identity keeps that subtraction out of the plan.
        max(
          4,
          ceil(:math.pi() / (2 * :math.asin(:math.sqrt(@circle_precision / point_radius / 2))))
        )
      end

    HtmlValidator.check_render_resource(:max_image_map_geometry_work, count * count, :layout)
    count
  end

  @doc "Indexes the first named map in each authored document/tree scope, excluding inert templates."
  @spec prepare([map()], map()) :: {:ok, map()} | {:error, {atom(), map()}}
  def prepare(children, links) do
    HtmlValidator.with_render_budget(fn ->
      index = collect_maps(children, %{}, links)
      {:ok, index}
    end)
  end

  defp collect_maps(children, index, links) do
    Enum.reduce(children, index, fn node, index ->
      case node do
        %{tag: "template"} ->
          index

        %{type: :element, tag: tag, children: nested, attributes: attrs} ->
          name = Map.get(attrs, "name", "")
          key = {Map.get(node, :document_id), Map.get(node, :tree_scope_id), name}

          index =
            if tag == "map" and Map.get(node, :namespace, :html) == :html and name != "" and
                 not Map.has_key?(index, key) do
              areas = collect_areas(nested, links)

              Map.put(index, key, %{
                name: name,
                identity: Map.get(node, :_selector_id),
                areas: areas
              })
            else
              index
            end

          collect_maps(nested, index, links)

        _ ->
          index
      end
    end)
  end

  defp collect_areas(children, links) do
    Enum.flat_map(children, fn node ->
      case node do
        %{tag: "template"} ->
          []

        %{tag: "area", attributes: attrs} ->
          coords = HtmlValidator.parse_html_float_list(Map.get(attrs, "coords", ""))
          HtmlValidator.reserve_render_resource(:max_image_map_coordinates, length(coords), :html)

          Enum.each(
            coords,
            &HtmlValidator.check_render_resource(:max_css_numeric_magnitude, abs(&1), :html)
          )

          shape =
            case {String.downcase(Map.get(attrs, "shape", "rect"), :ascii), coords} do
              {"default", _} ->
                :default

              {keyword, [x, y, radius | _]} when keyword in ["circle", "circ"] and radius > 0 ->
                {:circle, x, y, radius}

              {keyword, _} when keyword in ["circle", "circ"] ->
                :empty

              {keyword, [_, _, _, _, _, _ | _]} when keyword in ["poly", "polygon"] ->
                {:poly,
                 coords |> Enum.chunk_every(2, 2, :discard) |> Enum.map(fn [x, y] -> {x, y} end)}

              {keyword, _} when keyword in ["poly", "polygon"] ->
                :empty

              {_, [x1, y1, x2, y2 | _]} ->
                {:rect, min(x1, x2), min(y1, y2), max(x1, x2), max(y1, y2)}

              _ ->
                :empty
            end

          destination =
            case Map.get(attrs, "href") do
              nil -> nil
              href -> Map.fetch!(links.links, href)
            end

          [
            %{
              shape: shape,
              destination: destination,
              identity: Map.get(node, :_selector_id),
              alt: Map.get(attrs, "alt", "")
            }
          ]

        %{children: nested} ->
          collect_areas(nested, links)

        _ ->
          []
      end
    end)
  end

  @doc "Associates a usemap hash-name with the first map in the same authored tree."
  @spec associate(map(), map()) :: map() | nil
  def associate(node, index) do
    case Map.get(node.attributes, "usemap") do
      "#" <> name when name != "" ->
        Map.get(index, {Map.get(node, :document_id), Map.get(node, :tree_scope_id), name})

      _ ->
        nil
    end
  end

  @doc false
  @spec valid_model?(term()) :: boolean()
  def valid_model?(model) do
    case model do
      %{identity: identity, name: name, areas: areas}
      when is_reference(identity) and is_binary(name) and is_list(areas) ->
        Enum.all?(areas, fn area ->
          case area do
            %{identity: identity, shape: shape, destination: destination}
            when is_reference(identity) ->
              numbers =
                case shape do
                  :default ->
                    []

                  :empty ->
                    []

                  {:rect, left, top, right, bottom}
                  when is_number(left) and is_number(top) and is_number(right) and
                         is_number(bottom) and left <= right and top <= bottom ->
                    [left, top, right, bottom]

                  {:circle, x, y, radius} when is_number(radius) and radius > 0 ->
                    [x, y, radius]

                  {:poly, points} when is_list(points) and length(points) >= 3 ->
                    Enum.flat_map(points, fn point ->
                      case point do
                        {x, y} -> [x, y]
                        _ -> [:invalid]
                      end
                    end)

                  _ ->
                    [:invalid]
                end

              Enum.all?(numbers, &match?({:ok, _}, HtmlValidator.normalize_geometry_number(&1))) and
                case destination do
                  {:uri, url} ->
                    HtmlValidator.valid_link_url?(url)

                  destination ->
                    NativeElixirPdfUtilities.Validators.HtmlLinkValidator.valid_prepared_destination?(
                      destination
                    )
                end

            _ ->
              false
          end
        end)

      _ ->
        false
    end
  end

  @doc "Returns the first covering area's destination, a deliberate no-link mask, or no area. Coordinates are CSS pixels after image stretching."
  @spec hit_test(map(), term(), term(), term(), term()) ::
          {:ok, term()} | {:blocked, term()} | :none | {:error, {atom(), map()}}
  def hit_test(model, x, y, width, height) do
    if Enum.all?(
         [x, y, width, height],
         &match?({:ok, _}, HtmlValidator.normalize_geometry_number(&1))
       ) and width >= 0 and height >= 0 do
      if x < 0 or y < 0 or x > width or y > height do
        :none
      else
        case Enum.find(model.areas, &covers?(&1.shape, x, y, width, height)) do
          nil -> :none
          %{destination: nil, identity: identity} -> {:blocked, identity}
          %{destination: destination} -> {:ok, destination}
        end
      end
    else
      Diagnostics.error(
        :layout,
        :invalid_layout,
        "image-map lookup requires bounded coordinates and nonnegative image dimensions",
        module: __MODULE__,
        source: "map"
      )
    end
  end

  @doc false
  @spec covers?(term(), number(), number(), number(), number()) :: boolean()
  def covers?(shape, x, y, width, height) do
    case shape do
      :default ->
        x >= 0 and y >= 0 and x <= width and y <= height

      :empty ->
        false

      {:rect, left, top, right, bottom} ->
        left <= x and x <= right and top <= y and y <= bottom

      {:circle, center_x, center_y, radius} ->
        (x - center_x) * (x - center_x) + (y - center_y) * (y - center_y) <= radius * radius

      {:poly, points} ->
        edges = Enum.zip(points, tl(points) ++ [hd(points)])

        boundary? =
          Enum.any?(edges, fn {{ax, ay}, {bx, by}} ->
            (bx - ax) * (y - ay) == (by - ay) * (x - ax) and x >= min(ax, bx) and x <= max(ax, bx) and
              y >= min(ay, by) and y <= max(ay, by)
          end)

        boundary? or
          rem(
            Enum.count(edges, fn {{ax, ay}, {bx, by}} ->
              ay > y != by > y and x < (bx - ax) * (y - ay) / (by - ay) + ax
            end),
            2
          ) == 1
    end
  end

  @doc "Looks up the top painted image-map link at a real PDF-page point using the writer's actual composed/clipped geometry."
  @spec lookup(term(), term(), term(), term()) ::
          {:ok, term()} | :none | {:error, {atom(), map()}}
  def lookup(pages, page_number, x, y) do
    if is_integer(page_number) and page_number > 0 and
         Enum.all?([x, y], &match?({:ok, _}, HtmlValidator.normalize_geometry_number(&1))) do
      with {:ok, context} <-
             NativeElixirPdfUtilities.Validators.WriterValidator.prepare(pages, []) do
        case Enum.at(context.pages, page_number - 1) do
          %{size: {width, height}, boxes: boxes} ->
            if x < 0 or y < 0 or x > width or y > height do
              :none
            else
              selected =
                Enum.find(Enum.reverse(boxes), fn box ->
                  if Map.get(box, :type) == :link_region and
                       (Map.get(box, :link_url) != nil or Map.get(box, :link_destination) != nil) do
                    HtmlValidator.check_render_resource(
                      :max_image_map_geometry_work,
                      length(box.link_quads) * 4,
                      :layout
                    )

                    clip =
                      if Map.has_key?(box, :drawing_annotation_rect),
                        do: nil,
                        else:
                          NativeElixirPdfUtilities.HtmlToPdf.DrawingGeometry.box_clip_bounds(
                            Map.get(box, :clip)
                          )

                    inside_clip =
                      case clip do
                        nil ->
                          true

                        {left, bottom, right, top} ->
                          x >= left and x <= right and y >= bottom and y <= top
                      end

                    inside_clip and
                      Enum.any?(box.link_quads, &covers?({:poly, &1}, x, y, width, height))
                  else
                    false
                  end
                end)

              case selected do
                nil -> :none
                box -> {:ok, Map.get(box, :link_destination) || {:uri, box.link_url}}
              end
            end

          _ ->
            Diagnostics.error(
              :layout,
              :invalid_layout,
              "image-map lookup page is outside the actual document",
              module: __MODULE__,
              source: "map"
            )
        end
      end
    else
      Diagnostics.error(
        :layout,
        :invalid_layout,
        "image-map lookup requires a positive page number and bounded PDF coordinates",
        module: __MODULE__,
        source: "map"
      )
    end
  end

  @doc false
  @spec valid_link_region?(term()) :: boolean()
  def valid_link_region?(box) do
    case box do
      %{type: :link_region, x: x, y: y, width: width, height: height, link_quads: quads} ->
        is_number(width) and is_number(height) and width > 0 and height > 0 and
          Enum.all?(
            [x, y, width, height],
            &match?({:ok, _}, HtmlValidator.normalize_geometry_number(&1))
          ) and
          is_list(quads) and quads != [] and
          Enum.all?(quads, fn quad ->
            is_list(quad) and length(quad) == 4 and
              Enum.all?(quad, fn point ->
                case point do
                  {px, py} ->
                    match?({:ok, _}, HtmlValidator.normalize_geometry_number(px)) and
                      match?({:ok, _}, HtmlValidator.normalize_geometry_number(py)) and
                      px >= x - 0.0001 and px <= x + width + 0.0001 and
                      py >= y - 0.0001 and py <= y + height + 0.0001

                  _ ->
                    false
                end
              end) and
              Enum.all?(Enum.chunk_every(quad ++ Enum.take(quad, 2), 3, 1, :discard), fn [
                                                                                           {ax,
                                                                                            ay},
                                                                                           {bx,
                                                                                            by},
                                                                                           {cx,
                                                                                            cy}
                                                                                         ] ->
                (bx - ax) * (cy - by) - (by - ay) * (cx - bx) >= -0.0000001
              end)
          end)

      _ ->
        false
    end
  end
end
