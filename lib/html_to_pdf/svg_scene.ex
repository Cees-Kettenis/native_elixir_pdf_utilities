defmodule NativeElixirPdfUtilities.HtmlToPdf.SvgScene do
  @moduledoc false
  alias NativeElixirPdfUtilities.HtmlToPdf.{
    SvgBackend,
    SvgViewport,
    PngDecoder,
    RenderCache,
    ResourceContext
  }

  alias NativeElixirPdfUtilities.Validators.{
    HtmlValidator,
    SvgFormValidator,
    SvgClipValidator,
    SvgReferenceValidator,
    SvgPaintValidator,
    SvgEffectsValidator,
    SvgFilterValidator,
    SvgLinkValidator,
    SvgForeignObjectValidator,
    SvgTextValidator,
    SvgTextPositionValidator,
    SvgSceneValidator,
    SvgImageValidator,
    SvgStyleValidator
  }

  alias NativeElixirPdfUtilities.Limits

  @doc false
  @spec authored_source(map()) :: binary()
  def authored_source(node) do
    namespace =
      if Map.get(node, :namespace, :html) == :html,
        do: "http://www.w3.org/1999/xhtml",
        else: "http://www.w3.org/2000/svg"

    attrs =
      node.attributes
      |> Map.put("xmlns", namespace)
      |> Map.put("xmlns:xlink", "http://www.w3.org/1999/xlink")

    attrs =
      Enum.map_join(Enum.sort(attrs), " ", fn {key, value} ->
        key <> "=\"" <> escape(value) <> "\""
      end)

    children =
      Enum.map_join(node.children, "", fn child ->
        case child do
          %{type: :element} -> authored_source(child)
          %{type: :text, text: text} -> escape(text)
          _ -> "<?unsupported?>"
        end
      end)

    "<#{node.tag} #{attrs}>#{children}</#{node.tag}>"
  end

  @doc false
  @spec prepare(map(), HtmlValidator.image_budget()) :: {:ok, map()} | {:error, {atom(), map()}}
  @spec prepare(map(), HtmlValidator.image_budget(), keyword()) ::
          {:ok, map()} | {:error, {atom(), map()}}
  def prepare(node, budget, options \\ []) do
    with :ok <- SvgSceneValidator.validate_paint_style(node.style),
         :ok <- SvgClipValidator.validate_tree(node),
         :ok <- SvgReferenceValidator.validate_tree(node),
         :ok <- SvgPaintValidator.validate_tree(node),
         :ok <- SvgEffectsValidator.validate_tree(node),
         {:ok, text} <- SvgTextValidator.prepare_tree(node),
         {:ok, link_plan} <- SvgLinkValidator.prepare_tree(node, options),
         {:ok, projected, slots} <- SvgForeignObjectValidator.prepare_tree(node, options),
         :ok <- SvgFilterValidator.validate_tree(node, slots),
         {:ok, projected, resources, fonts} <-
           SvgImageValidator.prepare_tree(projected, budget, options, text.fonts),
         :ok <- SvgFilterValidator.validate_tree(projected, resources),
         # The SVG replaced-document default viewport is fixed at 300 x 150 CSS pixels.
         {:ok, document_viewport} <-
           SvgViewport.measure(node.attributes, node.style, %{width: 225.0, height: 112.5}) do
      projected = SvgLinkValidator.project_tree(projected, link_plan)

      document_node = %{
        projected
        | style:
            Map.merge(projected.style, %{
              svg_width: {:points, document_viewport.width},
              svg_height: {:points, document_viewport.height}
            })
      }

      slots =
        Enum.reduce(resources, slots, fn {_key, descriptor}, slots ->
          Map.merge(slots, Map.get(descriptor, :foreign_slots, %{}))
        end)

      source = serialize(projected, true, nil, false, text.aliases, text.positions)

      effect_source =
        if map_size(slots) > 0,
          do:
            serialize(
              html_effect_tree(projected, slots),
              true,
              nil,
              false,
              text.aliases,
              text.positions
            ),
          else: nil

      html_effects? =
        map_size(slots) > 0 and
          Enum.any?(SvgSceneValidator.tree_nodes(node), fn {child, _} ->
            Map.get(child.style, :svg_filter, :none) != :none
          end)

      {:ok,
       %{
         source: source,
         document_source:
           serialize(document_node, false, nil, false, text.aliases, text.positions),
         original_tree: node,
         foreign_slots: slots,
         html_effect_source: effect_source,
         html_effects?: html_effects?,
         attributes: node.attributes,
         style: node.style,
         budget: budget,
         logical_text: text.logical_text,
         links: [],
         link_plan: link_plan,
         selected_fonts: fonts,
         resources: resources,
         resource_context: ResourceContext.current(),
         node_id: Map.get(node, :node_id),
         document_id: Map.get(node, :document_id),
         tree_scope_id: Map.get(node, :tree_scope_id)
       }}
    end
  end

  # The image validator has already resolved authored image identifiers and
  # byte-font scopes. Replace only issued HTML slots in that execution tree.
  defp html_effect_tree(node, slots) do
    id = Map.get(node.attributes, "id")

    if node.tag == "rect" and Map.has_key?(slots, id) do
      node
      |> Map.put(:tag, "image")
      |> Map.update!(:attributes, fn attributes ->
        attributes
        |> Map.put("href", "NativeHtmlSurface:" <> id)
        |> Map.put("preserveAspectRatio", "none")
      end)
    else
      Map.update!(node, :children, fn children ->
        Enum.map(children, fn child ->
          if Map.get(child, :type) == :element, do: html_effect_tree(child, slots), else: child
        end)
      end)
    end
  end

  @doc false
  @spec measure(map(), number(), number() | nil) :: {:ok, map()} | {:error, {atom(), map()}}
  def measure(scene, width, height \\ nil),
    do: SvgViewport.measure(scene.attributes, scene.style, %{width: width, height: height})

  @doc false
  @spec paint(map(), number(), number() | nil) :: {:ok, map()} | {:error, {atom(), map()}}
  def paint(scene, width, height \\ nil) do
    context = ResourceContext.current()

    with :ok <- SvgSceneValidator.validate_paint_scope(scene, context) do
      if is_nil(context) or Map.get(scene, :link_plan, %{urls: []}).urls != [] do
        paint_scene(scene, width, height)
      else
        key =
          {:svg_scene_paint, context.document_id, context.tree_scope_id, context.resolution_base,
           context.authorization_root, scene.source, Map.get(scene, :resources, %{}),
           Map.get(scene, :foreign_slots, %{}), scene.selected_fonts, scene.attributes,
           scene.style, width, height}

        RenderCache.fetch(
          context.scope,
          key,
          fn -> paint_scene(scene, width, height) end,
          fn result -> SvgSceneValidator.reserve_paint_hit(scene, width, height, result) end
        )
      end
    end
  end

  defp paint_scene(scene, width, height) do
    with {:ok, viewport} <- measure(scene, width, height) do
      if viewport.paint? and Map.get(scene.style, :display) != :none do
        scale = Limits.get(:svg_raster_scale)
        raster_width = max(ceil(viewport.width / 0.75 * scale), 1)
        raster_height = max(ceil(viewport.height / 0.75 * scale), 1)
        # The root dimensions describe the actual host viewport. Intrinsic
        # dimensions and viewBox remain separate measurement facts.
        dimensions = " width=\"#{viewport.width / 0.75}\" height=\"#{viewport.height / 0.75}\""
        source = String.replace(scene.source, "<svg ", "<svg#{dimensions} ", global: false)

        if map_size(Map.get(scene, :foreign_slots, %{})) > 0 do
          assembled =
            if Map.get(scene, :html_effects?, false) do
              effect_source =
                String.replace(scene.html_effect_source, "<svg ", "<svg#{dimensions} ",
                  global: false
                )

              NativeElixirPdfUtilities.HtmlToPdf.SvgHtmlSurface.paint(
                source,
                effect_source,
                scene.foreign_slots,
                [width: raster_width, height: raster_height],
                %{
                  mode: :inline,
                  fonts: scene.selected_fonts,
                  resources: Map.get(scene, :resources, %{})
                },
                scene.budget
              )
            else
              with {:ok, skeleton} <-
                     SvgBackend.vectorize_slots(
                       source,
                       [width: raster_width, height: raster_height],
                       %{
                         mode: :inline,
                         fonts: scene.selected_fonts,
                         resources: Map.get(scene, :resources, %{})
                       },
                       scene.foreign_slots,
                       scene.budget
                     ) do
                NativeElixirPdfUtilities.HtmlToPdf.SvgFormComposition.compose(
                  skeleton,
                  scene.foreign_slots,
                  &SvgForeignObjectValidator.layout_slot/1
                )
              end
            end

          with {:ok, pdf, placements} <- assembled,
               {:ok, form} <- SvgFormValidator.prepare_source(pdf),
               {:ok, svg_links} <- SvgLinkValidator.project(form, scene.link_plan),
               {:ok, html_links} <-
                 NativeElixirPdfUtilities.Validators.SvgFormSlotValidator.prepare_links(
                   placements,
                   viewport.width,
                   viewport.height
                 ) do
            {:ok,
             %{
               viewport: viewport,
               image: %{
                 format: :pdf_form,
                 data: pdf,
                 width_px: raster_width,
                 height_px: raster_height
               },
               ink_extent: viewport.layout_extent,
               layout_extent: viewport.layout_extent,
               baseline: viewport.baseline,
               logical_text: scene.logical_text,
               links: scene.links ++ html_links ++ svg_links,
               selected_fonts: scene.selected_fonts,
               html_placements: placements
             }}
          end
        else
          with {:ok, png} <-
                 SvgBackend.rasterize(
                   source,
                   [width: raster_width, height: raster_height],
                   %{
                     mode: :inline,
                     fonts: scene.selected_fonts,
                     resources: Map.get(scene, :resources, %{})
                   },
                   scene.budget
                 ),
               {:ok, image} <- PngDecoder.decode(png, scene.budget, false) do
            ink_extent = ink_extent(image, viewport)

            vector_image =
              if is_nil(ink_extent) do
                {:ok, nil}
              else
                with {:ok, pdf} <-
                       SvgBackend.vectorize(
                         source,
                         [width: raster_width, height: raster_height],
                         %{
                           mode: :inline,
                           fonts: scene.selected_fonts,
                           resources: Map.get(scene, :resources, %{})
                         },
                         scene.budget
                       ),
                     {:ok, form} <- SvgFormValidator.prepare_source(pdf),
                     {:ok, links} <- SvgLinkValidator.project(form, scene.link_plan) do
                  {:ok,
                   %{
                     format: :pdf_form,
                     svg_links: links,
                     data: pdf,
                     width_px: image.width_px,
                     height_px: image.height_px
                   }}
                end
              end

            with {:ok, vector_image} <- vector_image do
              {:ok,
               %{
                 viewport: viewport,
                 image: vector_image,
                 ink_extent: ink_extent,
                 layout_extent: viewport.layout_extent,
                 baseline: viewport.baseline,
                 logical_text: scene.logical_text,
                 links:
                   scene.links ++
                     if(vector_image, do: Map.get(vector_image, :svg_links, []), else: []),
                 selected_fonts: scene.selected_fonts
               }}
            end
          end
        end
      else
        {:ok,
         %{
           viewport: viewport,
           image: nil,
           ink_extent: nil,
           layout_extent: viewport.layout_extent,
           baseline: viewport.baseline,
           logical_text: scene.logical_text,
           links: scene.links,
           selected_fonts: scene.selected_fonts
         }}
      end
    end
  end

  defp serialize(node, root?, references, reference?, fonts, positions) do
    references = references || SvgReferenceValidator.referenced_ids(node)
    reference? = reference? or MapSet.member?(references, Map.get(node.attributes, "id"))
    specified = Map.get(node.style, :_svg_specified, %{})

    properties =
      Map.new(SvgStyleValidator.presentation_properties(), fn {name, key} -> {key, name} end)

    attributes =
      node.attributes
      |> Map.drop(
        ["style", "xmlns", "xmlns:xlink"] ++
          Enum.flat_map(properties, fn {_, property} ->
            if SvgStyleValidator.presentation_property?(node, property), do: [property], else: []
          end)
      )

    attributes =
      if root?, do: Map.drop(attributes, ["width", "height", "x", "y"]), else: attributes

    attributes = Map.put(attributes, "xmlns", "http://www.w3.org/2000/svg")

    attributes =
      if root?,
        do: Map.put(attributes, "xmlns:xlink", "http://www.w3.org/1999/xlink"),
        else: attributes

    attributes =
      cond do
        reference? and Map.get(specified, "color") in ~w(inherit unset) ->
          Map.put(attributes, "color", "inherit")

        not reference? or Map.has_key?(specified, "color") ->
          Map.put(attributes, "color", property_value({:color, node.style.color}, node.style))

        true ->
          Map.delete(attributes, "color")
      end

    attributes =
      cond do
        reference? and Map.get(specified, "font-size") in ~w(inherit unset) ->
          Map.put(attributes, "font-size", "inherit")

        reference? and is_binary(Map.get(specified, "font-size")) and
          String.ends_with?(specified["font-size"], "em") and
            not String.ends_with?(specified["font-size"], "rem") ->
          Map.put(attributes, "font-size", specified["font-size"])

        not reference? or Map.has_key?(specified, "font-size") ->
          Map.put(attributes, "font-size", "#{node.style.font_size / 0.75}px")

        true ->
          Map.delete(attributes, "font-size")
      end

    attributes =
      if node.tag in ~w(svg symbol marker),
        do: Map.put(attributes, "overflow", to_string(node.style.overflow)),
        else: attributes

    attributes =
      Enum.reduce(properties, attributes, fn {key, property}, attrs ->
        eligible? =
          SvgStyleValidator.presentation_property?(node, property)

        root_geometry? = root? and property in ~w(width height x y)
        value = Map.get(node.style, key)
        inherited? = SvgStyleValidator.inherited_property?(key)

        output_property =
          cond do
            property == "transform" and node.tag in ~w(linearGradient radialGradient) ->
              "gradientTransform"

            property == "transform" and node.tag == "pattern" ->
              "patternTransform"

            true ->
              property
          end

        cond do
          not eligible? or root_geometry? or is_nil(value) or value == :auto ->
            attrs

          reference? and inherited? and not Map.has_key?(specified, property) ->
            attrs

          reference? and inherited? and Map.get(specified, property) in ~w(inherit unset) ->
            Map.put(attrs, output_property, "inherit")

          reference? and match?({:svg_em, _}, value) ->
            {:svg_em, number} = value
            Map.put(attrs, output_property, "#{number}em")

          reference? and value == :current_color ->
            Map.put(attrs, output_property, "currentColor")

          property == "opacity" and node.tag == "svg" ->
            attrs

          true ->
            Map.put(attrs, output_property, property_value(value, node.style))
        end
      end)

    attributes =
      if node.tag in ~w(text tspan textPath use) and
           Map.has_key?(fonts, Map.get(node.style.font_face, :data)) do
        alias = Map.fetch!(fonts, node.style.font_face.data)

        attributes
        |> Map.put("font-family", alias)
        |> Map.put("font-weight", to_string(node.style.font_weight))
        |> Map.put("font-style", to_string(node.style.font_style))
        |> Map.put("letter-spacing", "#{node.style.letter_spacing / 0.75}px")
      else
        attributes
      end

    attributes =
      if node.tag in ~w(text tspan textPath) do
        plan = Map.fetch!(positions, SvgTextPositionValidator.plan_key(node))

        Enum.reduce(plan.lengths, attributes, fn {property, lengths}, attrs ->
          Map.put(attrs, property, property_value(lengths, node.style, reference?))
        end)
      else
        attributes
      end

    attributes =
      attributes
      |> Map.put("display", if(node.style.display == :none, do: "none", else: "inline"))

    children =
      Enum.map_join(node.children, "", fn child ->
        case child do
          %{type: :element, tag: tag} when tag in ~w(style title desc) ->
            ""

          %{type: :element, namespace: :svg} ->
            serialize(child, false, references, reference?, fonts, positions)

          %{type: :text, text: text} when node.tag in ~w(text tspan textPath) ->
            escape(text)

          _ ->
            ""
        end
      end)

    children =
      if node.tag == "svg",
        do: "<g opacity=\"#{node.style.svg_opacity}\">#{children}</g>",
        else: children

    attrs =
      Enum.map_join(Enum.sort(attributes), " ", fn {key, value} ->
        key <> "=\"" <> escape(value) <> "\""
      end)

    "<#{node.tag} #{attrs}>#{children}</#{node.tag}>"
  end

  defp property_value(value, style, reference? \\ false) do
    case value do
      {:paint_server, id} ->
        "url(##{id})"

      {:fragment, id} ->
        "url(##{id})"

      :current_color ->
        property_value({:color, style.color}, style)

      {:color, :transparent} ->
        "transparent"

      {:color, {r, g, b}} ->
        "rgb(#{r * 255},#{g * 255},#{b * 255})"

      {:color, {r, g, b, a}} ->
        "rgba(#{r * 255},#{g * 255},#{b * 255},#{a})"

      {:user, n} ->
        to_string(n)

      {:points, n} ->
        "#{n / 0.75}px"

      {:percent, n} ->
        "#{n * 100}%"

      {:svg_em, n} ->
        if reference?, do: "#{n}em", else: "#{n * style.font_size / 0.75}"

      {:svg_rem, n} ->
        "#{n * style._root_font_size / 0.75}"

      value when is_tuple(value) and tuple_size(value) == 6 ->
        "matrix(" <> Enum.map_join(Tuple.to_list(value), " ", &to_string/1) <> ")"

      value when is_list(value) ->
        Enum.map_join(value, " ", &property_value(&1, style, reference?))

      value ->
        to_string(value)
    end
  end

  defp escape(value) do
    value
    |> String.replace("&", "&amp;")
    |> String.replace("\"", "&quot;")
    |> String.replace("<", "&lt;")
    |> String.replace(">", "&gt;")
  end

  defp ink_extent(image, viewport) do
    case Map.get(image, :alpha_data) do
      nil ->
        viewport.layout_extent

      alpha ->
        {bounds, _} =
          for <<byte <- alpha>>, reduce: {nil, 0} do
            {bounds, index} ->
              x = rem(index, image.width_px)
              y = div(index, image.width_px)

              bounds =
                if byte == 0 do
                  bounds
                else
                  case bounds do
                    nil ->
                      {x, y, x, y}

                    {left, top, right, bottom} ->
                      {min(left, x), min(top, y), max(right, x), max(bottom, y)}
                  end
                end

              {bounds, index + 1}
          end

        case bounds do
          nil ->
            nil

          {left, top, right, bottom} ->
            sx = viewport.width / image.width_px
            sy = viewport.height / image.height_px
            {left * sx, top * sy, (right - left + 1) * sx, (bottom - top + 1) * sy}
        end
    end
  end
end
