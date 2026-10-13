defmodule NativeElixirPdfUtilities.Validators.SvgValidator do
  @moduledoc false
  alias NativeElixirPdfUtilities.Diagnostics
  alias NativeElixirPdfUtilities.Limits

  alias NativeElixirPdfUtilities.Validators.{
    HtmlValidator,
    ForeignContentValidator,
    SvgTextPositionValidator
  }

  @typedoc "Internal bounded XML summary issued by validate_processing/2, including original document geometry metadata."
  @type document_summary :: %{
          required(:source_root) => %{String.t() => String.t()},
          required(:source_nodes) => [
            %{
              attributes: %{String.t() => String.t()},
              tag: String.t(),
              namespace: :svg | :html,
              index: pos_integer(),
              parent: non_neg_integer() | nil,
              children: []
            }
          ],
          required(:expanded_work) => non_neg_integer(),
          optional(atom()) => term()
        }

  @doc false
  @spec validate(term(), term(), HtmlValidator.image_budget() | nil) ::
          {:ok, keyword()} | {:error, {atom(), map()}}
  def validate(svg, raster_options, image_budget) do
    with {:ok, summary} <- validate_processing(svg, %{mode: :image}) do
      validate_dimensions(summary.root, raster_options, image_budget)
    end
  end

  @doc false
  @spec validate_dimensions(map(), term(), HtmlValidator.image_budget() | nil) ::
          {:ok, keyword()} | {:error, {atom(), map()}}
  def validate_dimensions(attributes, options, budget) do
    if is_list(options) and Keyword.keyword?(options) and
         Enum.all?(Keyword.keys(options), &(&1 in [:width, :height])) do
      with {:ok, {intrinsic_width, intrinsic_height}} <- svg_intrinsic_dimensions(attributes),
           {:ok, {width, height}} <-
             svg_raster_dimensions(options, intrinsic_width, intrinsic_height),
           :ok <- validate_raster_dimensions(width, height, budget) do
        {:ok, [width: width, height: height]}
      end
    else
      Diagnostics.error(:style, :invalid_document, "SVG options accept only width and height",
        source: "SVG"
      )
    end
  rescue
    ArithmeticError ->
      Diagnostics.error(
        :style,
        :invalid_document,
        "SVG dimensions exceed the supported numeric range; reduce dimensions or aspect ratio",
        source: "SVG"
      )
  catch
    {:render_resource_limit, {:error, {reason, diagnostic}}} ->
      {:error,
       {reason,
        Diagnostics.with_context(diagnostic,
          source: "SVG",
          module: __MODULE__,
          operation: :rasterize
        )}}
  end

  @doc "Validates and charges the actual SVG raster canvas before native allocation."
  @spec validate_raster_dimensions(term(), term(), HtmlValidator.image_budget() | nil) ::
          :ok | {:error, {atom(), map()}}
  def validate_raster_dimensions(width, height, budget) do
    if is_integer(width) and width > 0 and is_integer(height) and height > 0 do
      with :ok <- if(is_nil(budget), do: :ok, else: HtmlValidator.validate_image_budget(budget)),
           :ok <- validate_svg_raster_budget(width, height),
           :ok <-
             HtmlValidator.reserve_render_resource(
               :max_aggregate_svg_raster_pixels,
               width * height,
               :style
             ) do
        if budget == nil,
          do: :ok,
          else: HtmlValidator.reserve_decoded_image(budget, width, height, 4)
      end
    else
      Diagnostics.error(
        :style,
        :invalid_document,
        "SVG raster dimensions must be positive integers",
        source: "SVG"
      )
    end
  catch
    {:render_resource_limit, {:error, {reason, diagnostic}}} ->
      {:error,
       {reason,
        Diagnostics.with_context(diagnostic,
          source: "SVG",
          module: __MODULE__,
          operation: :rasterize
        )}}
  end

  @doc false
  @spec conversion_result(term()) :: {:ok, binary()} | {:error, {atom(), map()}}
  def conversion_result(result) do
    case result do
      {:ok, png} when is_binary(png) ->
        if byte_size(png) <= Limits.get(:max_svg_output_bytes) do
          {:ok, png}
        else
          Diagnostics.error(
            :limits,
            :resource_limit_exceeded,
            "SVG conversion output exceeds max_svg_output_bytes",
            source: "SVG"
          )
        end

      {:error, message} when is_binary(message) ->
        Diagnostics.error(:style, :invalid_document, "SVG conversion failed: #{message}",
          source: "SVG"
        )

      _ ->
        Diagnostics.error(
          :style,
          :invalid_document,
          "SVG converter returned an unexpected result",
          source: "SVG"
        )
    end
  end

  @doc false
  @spec validate_processing(term(), term()) ::
          {:ok, document_summary()} | {:error, {atom(), map()}}
  def validate_processing(svg, context) do
    with {:ok, context} <- validate_context(context),
         :ok <- validate_source(svg) do
      HtmlValidator.reserve_render_resource(
        :max_aggregate_svg_source_bytes,
        byte_size(svg),
        :style
      )

      validate_structure(svg, context)
    end
  catch
    {:render_resource_limit, {:error, {reason, diagnostic}}} ->
      {:error,
       {reason,
        Diagnostics.with_context(diagnostic,
          source: "SVG",
          module: __MODULE__,
          operation: :prepare
        )}}
  end

  @doc false
  @spec validate_context(term()) :: {:ok, map()} | {:error, {atom(), map()}}
  def validate_context(context) do
    case context do
      %{mode: mode} when mode in [:inline, :image, :reference] ->
        parent = Map.get(context, :parent_mode, :inline)
        resources = Map.get(context, :resources, %{})
        fonts = Map.get(context, :fonts, [])

        if parent in [:inline, :image, :reference] and is_map(resources) and is_list(fonts) and
             Enum.all?(
               Map.keys(context),
               &(&1 in [:mode, :parent_mode, :resources, :fonts])
             ) do
          {:ok,
           %{
             mode: mode,
             parent_mode: parent,
             resources: resources,
             fonts: fonts,
             external_resources?: mode != :image and parent != :image
           }}
        else
          Diagnostics.error(
            :options,
            :invalid_options,
            "SVG context accepts processing modes and byte-backed resources/fonts only",
            source: "SVG",
            module: __MODULE__,
            operation: :prepare
          )
        end

      _ ->
        Diagnostics.error(
          :options,
          :invalid_options,
          "SVG processing mode must be :inline, :image or :reference",
          source: "SVG",
          module: __MODULE__,
          operation: :prepare
        )
    end
  end

  defp validate_source(svg) do
    max_svg_bytes = Limits.get(:max_svg_bytes)

    cond do
      not is_binary(svg) ->
        Diagnostics.error(:style, :invalid_document, "SVG source must be a UTF-8 binary",
          source: "SVG"
        )

      byte_size(svg) > max_svg_bytes ->
        Diagnostics.error(
          :limits,
          :resource_limit_exceeded,
          "SVG source exceeds the #{max_svg_bytes}-byte limit",
          source: "SVG"
        )

      not String.valid?(svg) ->
        Diagnostics.error(:style, :invalid_document, "SVG source must be valid UTF-8",
          source: "SVG"
        )

      true ->
        :ok
    end
  end

  defp validate_structure(svg, context) do
    # Reject both DTD events because a bare DOCTYPE emits only endDTD in xmerl.
    state = %{
      depth: 0,
      nodes: 0,
      paths: 0,
      filters: 0,
      references: 0,
      root: nil,
      source_root: nil,
      styles: [],
      stack: [],
      context: context,
      elements: MapSet.new(),
      images: 0,
      ids: %{},
      graph: %{},
      path_costs: %{},
      text_costs: %{},
      position_costs: %{},
      text_by_node: %{},
      hrefs: [],
      text: [],
      geometry: [],
      source_nodes: [],
      opacity_layers: 0,
      nested_viewports: 0,
      clip_references: 0,
      surface_costs: %{}
    }

    result =
      :xmerl_sax_parser.stream(svg, [
        :skip_external_dtd,
        :disallow_entities,
        {:external_entities, :none},
        {:event_fun, &validate_event/3},
        {:event_state, state}
      ])

    case result do
      {:ok, %{root: attributes} = summary, rest} when is_map(attributes) ->
        if Regex.match?(~r/\A(?:[ \t\r\n]|<!--(?:[^-]|-(?!-))*-->)*\z/u, rest) do
          validate_expansion(summary)
        else
          Diagnostics.error(
            :style,
            :invalid_document,
            "SVG has trailing content after its root",
            source: "SVG"
          )
        end

      {:svg_validation, _location, error, _tags, _state} ->
        error

      {:render_resource_limit, _location, {:error, {reason, diagnostic}}, _tags, _state} ->
        {:error,
         {reason,
          Diagnostics.with_context(diagnostic,
            source: "SVG",
            module: __MODULE__,
            operation: :prepare
          )}}

      {:fatal_error, {_file, _entity, line}, message, _tags, _state} ->
        Diagnostics.error(
          :style,
          :invalid_document,
          "SVG XML is malformed: #{to_string(message)}",
          source: "SVG",
          line: line
        )
    end
  end

  defp validate_event(event, {_file, _entity, line}, state) do
    case event do
      {:startDTD, _name, _public, _system} ->
        reject("SVG resource references are not authorized; remove DTD/entity declarations", line)

      :endDTD ->
        reject("SVG resource references are not authorized; remove DTD/entity declarations", line)

      {:startElement, uri, local, _name, attributes} ->
        name = to_string(local)
        attribute_names = Enum.map(attributes, fn {uri, _prefix, key, _value} -> {uri, key} end)

        if length(Enum.uniq(attribute_names)) != length(attribute_names),
          do: reject("SVG attributes must have unique expanded namespace/name pairs", line)

        attrs =
          attributes
          |> Enum.filter(fn {uri, _prefix, _key, _value} -> uri == [] end)
          |> Map.new(fn {_uri, _prefix, key, value} ->
            {to_string(key), to_string(value)}
          end)

        attrs =
          Enum.reduce(attributes, attrs, fn {attribute_uri, _prefix, key, value}, attrs ->
            if to_string(attribute_uri) == "http://www.w3.org/XML/1998/namespace" and
                 to_string(key) == "space",
               do: Map.put(attrs, "xml:space", to_string(value)),
               else: attrs
          end)

        namespace =
          case to_string(uri) do
            "http://www.w3.org/1999/xhtml" -> :html
            value when value in ["", "http://www.w3.org/2000/svg"] -> :svg
            _ -> reject("SVG elements must use the SVG or XHTML namespace", line)
          end

        parent = List.first(state.stack)

        if namespace == :html do
          case parent do
            %{namespace: :svg, name: integration}
            when integration in ["foreignObject", "desc", "title"] ->
              html_context = if integration == "foreignObject", do: "div", else: "span"

              if not HtmlValidator.valid_html_child?(html_context, name),
                do:
                  reject(
                    "SVG #{integration} requires #{if html_context == "div", do: "flow", else: "phrasing"} HTML content",
                    line
                  )

            %{namespace: :html, name: html_parent} ->
              if not HtmlValidator.valid_html_child?(html_parent, name),
                do: reject("invalid HTML child #{name} in SVG #{html_parent}", line)

            _ ->
              reject("XHTML content requires an SVG integration point", line)
          end
        else
          if match?(%{namespace: :html}, parent) and name != "svg",
            do: reject("HTML content may re-enter SVG only through an svg element", line)
        end

        policy_result =
          with :ok <- ForeignContentValidator.validate_static_input(namespace, name, attributes) do
            if namespace == :html,
              do: HtmlValidator.validate_html_element_attributes(name, attrs),
              else: :ok
          end

        case policy_result do
          :ok ->
            :ok

          {:error, {reason, diagnostic}} ->
            throw(
              {:svg_validation,
               Diagnostics.error(:style, reason, diagnostic.message,
                 source: "SVG",
                 line: line,
                 module: __MODULE__,
                 operation: :rasterize
               )}
            )
        end

        if state.root == nil and
             (name != "svg" or to_string(uri) not in ["", "http://www.w3.org/2000/svg"]) do
          reject("SVG document must have an svg root", line)
        end

        hrefs =
          for {_uri, _prefix, key, value} <- attributes,
              to_string(key) == "href",
              do: String.trim(to_string(value))

        resource_hrefs =
          if namespace == :html and name == "a" and state.context.mode == :inline and
               state.context.external_resources?,
             do: Enum.reject(hrefs, &HtmlValidator.valid_link_url?/1),
             else: hrefs

        if (name in ["image", "feImage"] and not state.context.external_resources?) or
             Enum.any?(resource_hrefs, fn value ->
               not String.starts_with?(value, "#") and
                 (not state.context.external_resources? or
                    not Map.has_key?(state.context.resources, value))
             end) do
          reject(
            "SVG resource references are not authorized; use self-contained shapes and internal #id references",
            line
          )
        end

        Enum.each(attributes, fn {_uri, _prefix, key, value} ->
          if to_string(key) in [
               "style",
               "fill",
               "stroke",
               "filter",
               "clip-path",
               "mask",
               "marker",
               "marker-start",
               "marker-mid",
               "marker-end",
               "cursor"
             ] do
            validate_css_references(to_string(value), line)
          end
        end)

        if Enum.any?(
             resource_hrefs,
             &Regex.match?(~r/^(?:javascript|vbscript|data|file|https?):|^\/\//i, &1)
           ),
           do:
             reject(
               "SVG active URLs and backend file/network references are not authorized",
               line
             )

        paint_hrefs =
          if state.context.mode == :inline do
            Enum.flat_map(~w(clip-path fill stroke), fn property ->
              case NativeElixirPdfUtilities.Validators.SvgStyleValidator.parse(
                     property,
                     Map.get(attrs, property, "none")
                   ) do
                {:ok, {kind, id}} when kind in [:fragment, :paint_server] ->
                  [{"#" <> id, property}]

                _ ->
                  []
              end
            end)
          else
            []
          end

        position_work =
          if namespace == :svg and name in ~w(text tspan textPath) do
            case SvgTextPositionValidator.reserve_work(%{tag: name, attributes: attrs}, :style) do
              {:ok, work} -> work
              error -> throw({:svg_validation, error})
            end
          else
            0
          end

        paths = byte_size(Map.get(attrs, "d", "")) + byte_size(Map.get(attrs, "points", ""))

        index = state.nodes + 1
        id = Map.get(attrs, "id")
        if id != nil and Map.has_key?(state.ids, id), do: reject("SVG IDs must be unique", line)
        graph = Map.put(state.graph, index, [])
        graph = if parent == nil, do: graph, else: Map.update!(graph, parent.index, &[index | &1])
        HtmlValidator.reserve_render_resource(:max_aggregate_svg_nodes, 1, :style)
        HtmlValidator.reserve_render_resource(:max_aggregate_svg_path_bytes, paths, :style)

        HtmlValidator.reserve_render_resource(
          :max_aggregate_svg_filter_primitives,
          if(String.starts_with?(name, "fe"), do: 1, else: 0),
          :style
        )

        HtmlValidator.reserve_render_resource(
          :max_aggregate_svg_references,
          length(hrefs) + length(paint_hrefs),
          :style
        )

        state = %{
          state
          | stack: [%{namespace: namespace, name: name, index: index} | state.stack],
            source_root:
              if(state.root == nil,
                do:
                  Map.new(
                    for {uri, _prefix, key, value} <- attributes,
                        to_string(uri) == "",
                        do: {to_string(key), to_string(value)}
                  ),
                else: state.source_root
              ),
            source_nodes: [
              %{
                tag: name,
                namespace: namespace,
                attributes: attrs,
                children: [],
                index: index,
                parent: if(parent, do: parent.index, else: nil)
              }
              | state.source_nodes
            ],
            elements: MapSet.put(state.elements, {namespace, name}),
            geometry:
              if(state.context.mode == :inline and namespace == :svg,
                do: [
                  %{
                    type: :element,
                    namespace: :svg,
                    tag: name,
                    attributes:
                      if(
                        name in ~w(use textPath) and not Map.has_key?(attrs, "href") and
                          hrefs != [],
                        do: Map.put(attrs, "href", hd(hrefs)),
                        else: attrs
                      ),
                    children: [],
                    svg_index: index
                  }
                  | state.geometry
                ],
                else: state.geometry
              ),
            surface_costs:
              Map.put(state.surface_costs, index, %{
                opacity:
                  if(
                    namespace == :svg and Map.get(attrs, "clip-path", "none") == "none" and
                      case HtmlValidator.parse_css_number(Map.get(attrs, "opacity", "1")) do
                        {v, ""} -> v > 0 and v < 1
                        _ -> false
                      end,
                    do: 1,
                    else: 0
                  ),
                clip:
                  if(namespace == :svg and Map.get(attrs, "clip-path", "none") != "none",
                    do: 1,
                    else: 0
                  ),
                viewport:
                  if(
                    namespace == :svg and
                      (name == "symbol" or (name == "svg" and state.root != nil)),
                    do: 1,
                    else: 0
                  )
              }),
            opacity_layers:
              state.opacity_layers +
                if(
                  namespace == :svg and Map.get(attrs, "clip-path", "none") == "none" and
                    case HtmlValidator.parse_css_number(Map.get(attrs, "opacity", "1")) do
                      {value, ""} -> value > 0 and value < 1
                      _ -> false
                    end,
                  do: 1,
                  else: 0
                ),
            clip_references:
              state.clip_references +
                if(namespace == :svg and Map.get(attrs, "clip-path", "none") != "none",
                  do: 1,
                  else: 0
                ),
            nested_viewports:
              state.nested_viewports +
                if(namespace == :svg and name == "svg" and state.root != nil, do: 1, else: 0),
            images: state.images + if(name == "image", do: 1, else: 0),
            ids: if(id == nil, do: state.ids, else: Map.put(state.ids, id, index)),
            graph: graph,
            path_costs: Map.put(state.path_costs, index, paths),
            text_costs: Map.put(state.text_costs, index, 0),
            position_costs: Map.put(state.position_costs, index, position_work),
            hrefs:
              Enum.map(hrefs, &{index, &1, line, name}) ++
                Enum.map(paint_hrefs, fn {href, property} -> {index, href, line, property} end) ++
                state.hrefs,
            depth: state.depth + 1,
            nodes: state.nodes + 1,
            styles: if(name == "style", do: [[] | state.styles], else: state.styles),
            paths: state.paths + paths,
            filters: state.filters + if(String.starts_with?(name, "fe"), do: 1, else: 0),
            references: state.references + length(hrefs) + length(paint_hrefs),
            root:
              state.root || Map.new(attrs, fn {key, value} -> {String.downcase(key), value} end)
        }

        for {key, count} <- [
              {:max_svg_nodes, state.nodes},
              {:max_svg_depth, state.depth},
              {:max_svg_path_bytes, state.paths},
              {:max_svg_filter_primitives, state.filters},
              {:max_svg_references, state.references}
            ] do
          if count > Limits.get(key) do
            throw(
              {:svg_validation,
               Diagnostics.error(:limits, :resource_limit_exceeded, "SVG exceeds #{key}",
                 source: "SVG",
                 line: line
               )}
            )
          end
        end

        state

      {:endElement, _uri, local, _name} ->
        styles =
          if to_string(local) == "style" do
            [chunks | rest] = state.styles
            validate_css_references(chunks |> Enum.reverse() |> IO.iodata_to_binary(), line)
            rest
          else
            state.styles
          end

        %{state | depth: state.depth - 1, styles: styles, stack: tl(state.stack)}

      {:processingInstruction, target, _data} ->
        reject("SVG processing instructions are not supported: #{to_string(target)}", line)

      {:characters, characters} ->
        state =
          if Enum.any?(state.stack, &(&1.namespace == :svg and &1.name == "text")) do
            [%{index: index} | _] = state.stack
            text = to_string(characters)

            %{
              state
              | text: [text | state.text],
                text_by_node: Map.update(state.text_by_node, index, text, &(&1 <> text)),
                text_costs:
                  Map.update!(state.text_costs, index, &(&1 + length(String.to_charlist(text))))
            }
          else
            state
          end

        case state.styles do
          [] -> state
          [chunks | rest] -> %{state | styles: [[to_string(characters) | chunks] | rest]}
        end

      _ ->
        state
    end
  end

  defp validate_expansion(summary) do
    graph =
      Enum.reduce(summary.hrefs, summary.graph, fn {index, href, line, name}, graph ->
        case href do
          "#" <> id when name != "a" ->
            case Map.fetch(summary.ids, id) do
              {:ok, target} -> Map.update!(graph, index, &[target | &1])
              :error -> reject("SVG reference ##{id} has no target", line)
            end

          _ ->
            graph
        end
      end)

    # Every authored node and every reused subtree charges the same outer scope.
    work =
      visit_expansion(
        [{1, MapSet.new(), 1}],
        graph,
        summary.path_costs,
        summary.text_costs,
        summary.position_costs,
        %{
          nodes: 0,
          positions: 0,
          paths: 0,
          counts: %{}
        }
      )

    costs =
      Enum.reduce(work.counts, %{opacity: 0, clip: 0, viewport: 0}, fn {index, count}, total ->
        cost = Map.fetch!(summary.surface_costs, index)
        Map.new(total, fn {key, amount} -> {key, amount + Map.fetch!(cost, key) * count} end)
      end)

    {:ok,
     Map.merge(summary, %{
       expanded_work: work.nodes,
       expanded_position_work: work.positions,
       expansion_counts: work.counts,
       opacity_layers: costs.opacity,
       clip_references: costs.clip,
       nested_viewports: costs.viewport
     })}
  catch
    {:svg_validation, error} -> error
  end

  defp visit_expansion(pending, graph, path_costs, text_costs, position_costs, work) do
    case pending do
      [] ->
        work

      [{index, ancestors, depth} | rest] ->
        paths = Map.fetch!(path_costs, index)
        node_work = 1 + Map.fetch!(text_costs, index)
        positions = Map.fetch!(position_costs, index)

        cond do
          MapSet.member?(ancestors, index) ->
            reject("SVG reference cycle is not supported", 1)

          depth > Limits.get(:max_svg_depth) ->
            reject("SVG reference expansion exceeds max_svg_depth", 1)

          work.nodes + node_work + work.positions + positions >
              Limits.get(:max_svg_expansion_work) ->
            throw(
              {:svg_validation,
               Diagnostics.error(
                 :limits,
                 :resource_limit_exceeded,
                 "SVG exceeds max_svg_expansion_work",
                 source: "SVG"
               )}
            )

          work.paths + paths > Limits.get(:max_svg_expanded_path_bytes) ->
            throw(
              {:svg_validation,
               Diagnostics.error(
                 :limits,
                 :resource_limit_exceeded,
                 "SVG exceeds max_svg_expanded_path_bytes",
                 source: "SVG"
               )}
            )

          true ->
            HtmlValidator.reserve_render_resource(
              :max_svg_expansion_work,
              node_work + if(Map.has_key?(work.counts, index), do: positions, else: 0),
              :style
            )

            HtmlValidator.reserve_render_resource(:max_svg_expanded_path_bytes, paths, :style)
            ancestors = MapSet.put(ancestors, index)
            children = Enum.map(Map.fetch!(graph, index), &{&1, ancestors, depth + 1})

            visit_expansion(children ++ rest, graph, path_costs, text_costs, position_costs, %{
              nodes: work.nodes + node_work,
              positions: work.positions + positions,
              paths: work.paths + paths,
              counts: Map.update(work.counts, index, 1, &(&1 + 1))
            })
        end
    end
  end

  defp validate_css_references(value, line) do
    if String.contains?(value, "\\") or
         Regex.match?(~r/@import\b|url\(\s*["']?(?!\s*["']?#)[^)]/iu, value) do
      reject(
        "SVG external CSS resources and escaped resource references are not authorized",
        line
      )
    end
  end

  defp reject(message, line) do
    throw(
      {:svg_validation,
       Diagnostics.error(:style, :invalid_document, message, source: "SVG", line: line)}
    )
  end

  defp svg_intrinsic_dimensions(values) do
    with {:ok, view_box} <- svg_view_box(Map.get(values, "viewbox")),
         {:ok, width} <- svg_intrinsic_length(Map.get(values, "width"), view_box, 0),
         {:ok, height} <- svg_intrinsic_length(Map.get(values, "height"), view_box, 1) do
      {:ok, {width, height}}
    else
      _ ->
        Diagnostics.error(
          :style,
          :invalid_document,
          "SVG source must contain valid intrinsic dimensions or a viewBox within the supported numeric range",
          source: "SVG"
        )
    end
  end

  defp svg_view_box(value) do
    case NativeElixirPdfUtilities.Validators.SvgViewportValidator.view_box(value) do
      {:ok, nil} -> {:ok, nil}
      {:ok, {_x, _y, width, height}} when width > 0 and height > 0 -> {:ok, {width, height}}
      _ -> :error
    end
  end

  defp svg_intrinsic_length(value, view_box, index) do
    case value do
      nil ->
        svg_view_box_length(view_box, index)

      value when is_binary(value) ->
        normalized = String.trim(value)

        case Regex.run(
               ~r/^([+]?(?:\d+(?:\.\d*)?|\.\d+)(?:e[+-]?\d+)?)(px|pt|pc|mm|cm|in|q)?$/iu,
               normalized
             ) do
          [_, number] ->
            case parse_svg_number(number) do
              {:ok, number} when number > 0 -> {:ok, number}
              _ -> :error
            end

          [_, number, unit] ->
            case parse_svg_number(number) do
              {:ok, number} when number > 0 ->
                {:ok, number * svg_pixels_per_unit(String.downcase(unit))}

              _ ->
                :error
            end

          _ ->
            case Regex.run(~r/^([+]?(?:\d+(?:\.\d*)?|\.\d+))%$/u, normalized) do
              [_, percentage] ->
                with {:ok, percentage} when percentage > 0 <- parse_svg_number(percentage),
                     {:ok, base} <- svg_view_box_length(view_box, index) do
                  {:ok, base * percentage / 100.0}
                else
                  _ -> :error
                end

              _ ->
                :error
            end
        end
    end
  end

  defp parse_svg_number(value) do
    case Float.parse(value) do
      {number, ""} -> {:ok, number}
      _ -> :error
    end
  end

  defp svg_view_box_length(view_box, index) do
    case {view_box, index} do
      {{width, _height}, 0} -> {:ok, width}
      {{_width, height}, 1} -> {:ok, height}
      {nil, _index} -> :error
    end
  end

  defp svg_pixels_per_unit(unit) do
    case unit do
      "px" -> 1.0
      "pt" -> 96.0 / 72.0
      "pc" -> 16.0
      "mm" -> 96.0 / 25.4
      "cm" -> 96.0 / 2.54
      "in" -> 96.0
      "q" -> 96.0 / 101.6
    end
  end

  defp svg_raster_dimensions(raster_options, intrinsic_width, intrinsic_height) do
    width = Keyword.get(raster_options, :width)
    height = Keyword.get(raster_options, :height)

    case {width, height} do
      {width, height}
      when is_integer(width) and width > 0 and is_integer(height) and height > 0 ->
        {:ok, {width, height}}

      {width, nil} when is_integer(width) and width > 0 ->
        {:ok, {width, max(round(width * intrinsic_height / intrinsic_width), 1)}}

      {nil, height} when is_integer(height) and height > 0 ->
        {:ok, {max(round(height * intrinsic_width / intrinsic_height), 1), height}}

      {nil, nil} ->
        {:ok, {max(round(intrinsic_width), 1), max(round(intrinsic_height), 1)}}

      _ ->
        Diagnostics.error(
          :style,
          :invalid_document,
          "SVG raster dimensions must be positive integers"
        )
    end
  end

  defp validate_svg_raster_budget(width, height) do
    max_dimension = Limits.get(:max_svg_raster_dimension)
    max_pixels = Limits.get(:max_svg_raster_pixels)

    cond do
      width > max_dimension or height > max_dimension ->
        Diagnostics.error(
          :limits,
          :resource_limit_exceeded,
          "SVG raster dimensions #{width}x#{height} exceed the #{max_dimension}-pixel per-axis limit"
        )

      width * height > max_pixels ->
        Diagnostics.error(
          :limits,
          :resource_limit_exceeded,
          "SVG raster dimensions #{width}x#{height} exceed the #{max_pixels}-pixel limit"
        )

      width > 0x7FFFFFFF or height > 0x7FFFFFFF ->
        # PNG dimensions and the renderer's signed geometry are fixed format bounds.
        Diagnostics.error(
          :style,
          :invalid_document,
          "SVG raster dimensions exceed the PNG signed 31-bit dimension range",
          source: "SVG"
        )

      true ->
        :ok
    end
  end
end
