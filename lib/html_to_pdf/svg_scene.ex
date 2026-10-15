defmodule NativeElixirPdfUtilities.HtmlToPdf.SvgScene do
  @moduledoc false
  alias NativeElixirPdfUtilities.HtmlToPdf.{
    SvgRasterizer,
    SvgViewport,
    PngDecoder,
    ResourceContext
  }

  alias NativeElixirPdfUtilities.Validators.{
    HtmlValidator,
    SvgSceneValidator,
    SvgImageValidator,
    SvgStyleValidator,
    SvgReferenceValidator,
    SvgConverterValidator
  }

  alias NativeElixirPdfUtilities.Limits

  @doc "Extracts a complete selected SVG document in its current authorized resource scope."
  @spec extract(binary(), HtmlValidator.image_budget(), keyword()) ::
          {:ok, map()} | {:error, {atom(), map()}}
  def extract(source, budget, options) do
    with {:ok, document, _summary} <- SvgConverterValidator.prepare_extraction_details(source) do
      extract_prepared_source(document, budget, options)
    end
  end

  @doc "Extracts a document image after actual source admission and independent font policy preparation."
  @spec extract(binary(), HtmlValidator.image_budget(), keyword(), keyword()) ::
          {:ok, map(), [map()]} | {:error, {atom(), map()}}
  def extract(source, budget, options, image_font_options) do
    with {:ok, document, summary} <- SvgConverterValidator.prepare_extraction_details(source),
         {:ok, options} <- NativeElixirPdfUtilities.HtmlToPdf.Font.normalize_options(options),
         configured_fonts = Keyword.get(options, :fonts, []),
         {:ok, fonts} <-
           NativeElixirPdfUtilities.Validators.FontValidator.prepare_svg_image_fonts(
             Keyword.get(options, :fonts, []),
             image_font_options,
             summary
           ) do
      options = Keyword.put(options, :fonts, fonts)

      options =
        if configured_fonts == [] and fonts != [],
          do: Keyword.put(options, :default_font, hd(fonts).family),
          else: options

      with {:ok, scene} <- extract_prepared_source(document, budget, options),
           do: {:ok, scene, fonts}
    end
  end

  defp extract_prepared_source(document, budget, options) do
    with {:ok, styled} <-
           NativeElixirPdfUtilities.HtmlToPdf.Style.prepare_svg_detailed(document, options),
         {:ok, root} <- SvgSceneValidator.extraction_root(styled) do
      prepare(root, budget, options)
    end
  end

  @doc false
  @spec authored_source(map()) :: binary()
  def authored_source(node) do
    serialize_authored(node, nil)
  end

  # Namespace declarations belong at namespace boundaries. Preserve authored
  # declarations, and let HTML descendants inherit their integration namespace.
  defp serialize_authored(node, inherited_namespace) do
    namespace =
      if Map.get(node, :namespace, :html) == :html,
        do: "http://www.w3.org/1999/xhtml",
        else: "http://www.w3.org/2000/svg"

    attrs =
      if namespace == inherited_namespace,
        do: node.attributes,
        else: Map.put_new(node.attributes, "xmlns", namespace)

    attrs =
      if is_nil(inherited_namespace) and namespace == "http://www.w3.org/2000/svg",
        do: Map.put_new(attrs, "xmlns:xlink", "http://www.w3.org/1999/xlink"),
        else: attrs

    attrs =
      Enum.map_join(Enum.sort(attrs), " ", fn {key, value} ->
        key <> "=\"" <> escape(value) <> "\""
      end)

    children =
      Enum.map_join(node.children, "", fn child ->
        case child do
          %{type: :element} ->
            serialize_authored(child, namespace)

          %{type: :text, text: text} ->
            escape(text)

          %{type: :processing_instruction, target: target, data: data} ->
            "<?#{target}#{if data == "", do: "", else: " " <> data}?>"

          _ ->
            "<?unsupported?>"
        end
      end)

    if Map.get(node, :namespace, :html) == :html and HtmlValidator.html_void_tag?(node.tag),
      do: "<#{node.tag} #{attrs}/>",
      else: "<#{node.tag} #{attrs}>#{children}</#{node.tag}>"
  end

  @doc "Extracts the selected host cascade and authorized image bytes for the existing SVG converter."
  @spec prepare(map(), HtmlValidator.image_budget(), keyword()) ::
          {:ok, map()} | {:error, {atom(), map()}}
  @spec prepare(map(), HtmlValidator.image_budget()) :: {:ok, map()} | {:error, {atom(), map()}}
  def prepare(node, budget, options \\ []) do
    with :ok <- SvgSceneValidator.validate_paint_style(node.style),
         {:ok, fonts, aliases} <- SvgConverterValidator.prepare_tree_fonts(node),
         {:ok, projected, resources, fonts} <-
           SvgImageValidator.prepare_tree(node, budget, options, fonts),
         {:ok, viewport} <-
           SvgViewport.measure(node.attributes, node.style, %{width: 225.0, height: 112.5}) do
      document_node = %{
        projected
        | style:
            Map.merge(
              projected.style,
              %{svg_width: {:points, viewport.width}, svg_height: {:points, viewport.height}}
            )
      }

      {:ok,
       %{
         source: serialize(projected, true, nil, false, aliases),
         document_source: serialize(document_node, false, nil, false, aliases),
         original_tree: node,
         attributes: node.attributes,
         style: node.style,
         budget: budget,
         selected_fonts: fonts,
         resources: resources,
         resource_context: ResourceContext.current(),
         node_id: Map.get(node, :node_id),
         document_id: Map.get(node, :document_id),
         tree_scope_id: Map.get(node, :tree_scope_id)
       }}
    end
  end

  @doc "Measures the SVG replaced viewport in its actual HTML containing block."
  @spec measure(map(), number(), number() | nil) :: {:ok, map()} | {:error, {atom(), map()}}
  def measure(scene, width, height \\ nil),
    do: SvgViewport.measure(scene.attributes, scene.style, %{width: width, height: height})

  @doc "Embeds the existing converter's PNG output at the measured host viewport."
  @spec paint(map(), number(), number() | nil) :: {:ok, map()} | {:error, {atom(), map()}}
  def paint(scene, width, height \\ nil) do
    with :ok <- SvgSceneValidator.validate_paint_scope(scene, ResourceContext.current()),
         {:ok, viewport} <- measure(scene, width, height) do
      if viewport.paint? and Map.get(scene.style, :display) != :none do
        scale = Limits.get(:svg_raster_scale)

        dimensions = [
          width: max(ceil(viewport.width / 0.75 * scale), 1),
          height: max(ceil(viewport.height / 0.75 * scale), 1)
        ]

        with {:ok, source} <-
               SvgConverterValidator.with_root_viewport(
                 scene.source,
                 {viewport.width / 0.75, viewport.height / 0.75},
                 %{mode: :inline, resources: scene.resources}
               ),
             {:ok, png} <-
               SvgRasterizer.rasterize(source, dimensions, scene.budget, scene.selected_fonts, %{
                 mode: :inline,
                 resources: scene.resources
               }),
             {:ok, image} <- PngDecoder.decode(png, scene.budget, false) do
          {:ok,
           %{
             viewport: viewport,
             image: image,
             ink_extent: ink_extent(image, viewport),
             layout_extent: viewport.layout_extent,
             baseline: viewport.baseline,
             links: [],
             selected_fonts: scene.selected_fonts
           }}
        end
      else
        {:ok,
         %{
           viewport: viewport,
           image: nil,
           ink_extent: nil,
           layout_extent: viewport.layout_extent,
           baseline: viewport.baseline,
           links: [],
           selected_fonts: scene.selected_fonts
         }}
      end
    end
  end

  defp serialize(node, root?, references, reference?, fonts) do
    references = references || SvgReferenceValidator.referenced_ids(node)
    reference? = reference? or MapSet.member?(references, Map.get(node.attributes, "id"))
    specified = Map.get(node.style, :_svg_specified, %{})

    properties =
      Map.new(SvgStyleValidator.presentation_properties(), fn {name, key} -> {key, name} end)

    attributes =
      node.attributes
      |> Map.drop(
        ["style", "marker", "xmlns", "xmlns:xlink"] ++
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

          property == "transform" and value == "none" ->
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
      attributes
      |> Map.put("display", if(node.style.display == :none, do: "none", else: "inline"))

    # Host CSS has already selected the final presentation values. XML names
    # with different case are inert to SVG painting; do not emit a second
    # spelling beside a projected property and turn it into an HTML duplicate.
    projected_names = Map.values(properties) ++ ~w(width height x y)

    attributes =
      Map.reject(attributes, fn {name, _} ->
        canonical = String.downcase(name, :ascii)
        name != canonical and canonical in projected_names and Map.has_key?(attributes, canonical)
      end)

    children =
      Enum.map_join(node.children, "", fn child ->
        case child do
          %{type: :element, tag: "style"} ->
            ""

          %{type: :element, namespace: :svg} ->
            serialize(child, false, references, reference?, fonts)

          %{type: :element} ->
            authored_source(child)

          %{type: :text, text: text} ->
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
