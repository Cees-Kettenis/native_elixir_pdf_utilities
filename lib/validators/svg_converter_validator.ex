defmodule NativeElixirPdfUtilities.Validators.SvgConverterValidator do
  @moduledoc false
  alias NativeElixirPdfUtilities.Diagnostics
  alias NativeElixirPdfUtilities.HtmlToPdf.{Font, HtmlParser, SvgScene, PngDecoder}

  alias NativeElixirPdfUtilities.Validators.{
    FontValidator,
    HtmlValidator,
    SvgValidator,
    SvgImageValidator,
    SvgSceneValidator,
    SvgViewportValidator
  }

  @doc "Validates the complete authored SVG document before host styling or asset resolution."
  @spec prepare_extraction(term()) :: {:ok, map()} | {:error, {atom(), map()}}
  def prepare_extraction(source) do
    with {:ok, document, _summary} <- prepare_extraction_details(source), do: {:ok, document}
  end

  @doc "Returns the parsed extraction document and its actual canonical XML summary."
  @spec prepare_extraction_details(term()) :: {:ok, map(), map()} | {:error, {atom(), map()}}
  def prepare_extraction_details(source) do
    parsed =
      case HtmlParser.parse_detailed(source) do
        {:ok, _} = result ->
          result

        error ->
          # An XML document failure must keep the canonical XML diagnostic,
          # even when the HTML-oriented tokenizer fails first.
          with {:ok, _} <- SvgValidator.validate_processing(source, %{mode: :inline}), do: error
      end

    with {:ok, document} <- parsed,
         {:ok, root} <- SvgSceneValidator.extraction_root(document),
         {:ok, summary} <-
           SvgValidator.validate_processing(source, %{
             mode: :inline,
             resources: SvgImageValidator.pending_references(root)
           }),
         :ok <- validate_capabilities(summary),
         {:ok, document} <- SvgSceneValidator.align_xml_source(document, summary),
         {:ok, root} <- SvgSceneValidator.extraction_root(document),
         :ok <- SvgSceneValidator.validate_source_tree(root) do
      {:ok, document, summary}
    end
  end

  @doc "Prepares bounded, self-contained converter input from authorized byte descriptors."
  @spec prepare(term(), term(), term(), term(), term()) ::
          {:ok, map()} | {:error, {atom(), map()}}
  def prepare(source, options, budget, fonts, context) do
    prepare(source, options, budget, fonts, context, nil)
  end

  @doc "Prepares converter input with independent caller font options after actual source admission."
  @spec prepare(term(), term(), term(), term(), term(), term()) ::
          {:ok, map()} | {:error, {atom(), map()}}
  def prepare(source, options, budget, fonts, context, image_font_options) do
    budget = if is_nil(budget), do: HtmlValidator.new_image_budget(), else: budget

    HtmlValidator.with_render_budget(fn ->
      context =
        case context do
          context when is_map(context) -> Map.put(context, :fonts, fonts)
          context -> context
        end

      with {:ok, context} <- SvgValidator.validate_context(context),
           {:ok, source, summary, fonts} <-
             prepare_source(source, context, budget, 0, image_font_options),
           {:ok, dimensions} <- SvgValidator.validate_dimensions(summary.root, options, budget),
           {:ok, source} <- stretch_raster_source(source, summary.root, dimensions, context),
           :ok <-
             HtmlValidator.reserve_render_resource(
               :max_aggregate_svg_surface_pixels,
               dimensions[:width] * dimensions[:height] *
                 (1 + 25 * summary.opacity_layers +
                    75 * (summary.nested_viewports + summary.clip_references)),
               :style
             ),
           {:ok, fonts} <- prepare_fonts(fonts, summary),
           {:ok, family} <-
             (case fonts do
                [] -> {:ok, nil}
                [first | _] -> FontValidator.prepare_font_family_name(first)
              end) do
        options =
          if is_nil(family) do
            dimensions
          else
            dimensions ++
              Enum.map(
                ~w(font_family serif_family sans_serif_family cursive_family fantasy_family monospace_family)a,
                &{&1, family}
              )
          end

        {:ok, %{source: source, options: options, fonts: fonts}}
      end
    end)
    |> case do
      {:error, {reason, diagnostic}} ->
        {:error, {reason, Diagnostics.with_context(diagnostic, source: "SVG")}}

      result ->
        result
    end
  catch
    {:render_resource_limit, {:error, {reason, diagnostic}}} ->
      {:error, {reason, Diagnostics.with_context(diagnostic, source: "SVG")}}
  end

  @doc "Retains the genuine selected host text faces without converter-specific aliases."
  @spec prepare_tree_fonts(map()) :: {:ok, [map()], map()} | {:error, {atom(), map()}}
  def prepare_tree_fonts(root) do
    FontValidator.with_budget(fn ->
      nodes = SvgSceneValidator.tree_nodes(root)
      text? = Enum.any?(nodes, fn {node, _} -> node.tag in ~w(text tspan textPath) end)

      Enum.reduce_while(nodes, {:ok, [], %{}}, fn {node, _}, {:ok, fonts, names} ->
        if (node.tag in ~w(text tspan textPath) or
              (text? and node.tag == "use" and
                 get_in(node, [:style, :font_face, :type]) == :embedded)) and
             Map.get(node.style, :display) != :none do
          case Map.get(node.style, :font_face) do
            %{type: :embedded, data: data} = face ->
              with {:ok, name} <- FontValidator.prepare_font_family_name(data),
                   :ok <-
                     HtmlValidator.validate_font_embedding(
                       face.family,
                       face.embedding_flags,
                       face.variable_font?
                     ) do
                fonts = if Map.has_key?(names, data), do: fonts, else: fonts ++ [face]
                {:cont, {:ok, fonts, Map.put(names, data, name)}}
              else
                error -> {:halt, error}
              end

            _ ->
              {:halt,
               unsupported(
                 "SVG text requires an authorized byte-backed font; configure its font bytes",
                 node.tag
               )}
          end
        else
          {:cont, {:ok, fonts, names}}
        end
      end)
    end)
  end

  @doc "Serializes the actual selected root size without changing descendant SVG geometry."
  @spec with_root_viewport(binary(), tuple(), map()) ::
          {:ok, binary()} | {:error, {atom(), map()}}
  @spec with_root_viewport(binary(), tuple()) :: {:ok, binary()} | {:error, {atom(), map()}}
  def with_root_viewport(source, size, context \\ %{mode: :image}) do
    with {:ok, root} <- sized_root(source, size, context),
         do: {:ok, SvgScene.authored_source(root)}
  end

  @doc "Places a document root in its actual child canvas without stretching its authored viewport."
  @spec with_document_viewport(binary(), tuple(), tuple(), map()) ::
          {:ok, binary()} | {:error, {atom(), map()}}
  def with_document_viewport(source, root_size, canvas_size, context) do
    with {:ok, root} <- sized_root(source, root_size, context),
         {:ok, {width, height}} <- viewport_size(canvas_size) do
      # Resvg applies a nested SVG's transform twice. The equivalent parent
      # group applies it once, in the same parent coordinate system as Chrome.
      # Extraction has already resolved CSS into this root's attributes.
      root =
        case Map.pop(root.attributes, "transform") do
          {nil, _attributes} ->
            root

          {transform, attributes} ->
            %{
              type: :element,
              namespace: :svg,
              tag: "g",
              attributes: %{"transform" => transform},
              children: [%{root | attributes: attributes}]
            }
        end

      {:ok,
       "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"#{width}\" height=\"#{height}\" viewBox=\"0 0 #{width} #{height}\">#{SvgScene.authored_source(root)}</svg>"}
    end
  end

  defp sized_root(source, size, context) do
    with {:ok, summary} <- SvgValidator.validate_processing(source, context),
         {:ok, {width, height}} <- viewport_size(size),
         {:ok, document} <- HtmlParser.parse_detailed(source),
         {:ok, document} <- SvgSceneValidator.align_xml_source(document, summary),
         {:ok, root} <- SvgSceneValidator.extraction_root(document) do
      root = %{
        root
        | attributes:
            root.attributes
            |> Map.put("width", to_string(width))
            |> Map.put("height", to_string(height))
      }

      {:ok, root}
    end
  end

  @doc "Returns an actionable operational diagnostic for approved snapshot staging failures."
  @spec staging_error(term()) :: {:error, {atom(), map()}}
  def staging_error(reason),
    do: invalid("could not stage approved SVG font bytes: #{inspect(reason)}")

  # An explicit two-dimensional raster target stretches the authored canvas per
  # CSS default object-fit fill; resvg would otherwise fit the smaller
  # dimension and letterbox the artwork inside the requested canvas.
  defp stretch_raster_source(source, root_attributes, dimensions, context) do
    with {:ok, overrides} <-
           SvgValidator.stretch_viewport_attributes(
             root_attributes,
             Keyword.get(dimensions, :width),
             Keyword.get(dimensions, :height)
           ) do
      case overrides do
        nil ->
          {:ok, source}

        overrides ->
          with {:ok, summary} <-
                 SvgValidator.validate_processing(
                   source,
                   Map.take(context, [:mode, :parent_mode, :fonts, :resources])
                 ),
               {:ok, document} <- HtmlParser.parse_detailed(source),
               {:ok, document} <- SvgSceneValidator.align_xml_source(document, summary),
               {:ok, root} <- SvgSceneValidator.extraction_root(document) do
            {:ok,
             SvgScene.authored_source(%{root | attributes: Map.merge(root.attributes, overrides)})}
          end
      end
    end
  end

  defp viewport_size(size) do
    case size do
      {width, height} when is_number(width) and is_number(height) and width > 0 and height > 0 ->
        with {:ok, width} <- HtmlValidator.normalize_geometry_number(width),
             {:ok, height} <- HtmlValidator.normalize_geometry_number(height),
             do: {:ok, {width, height}}

      _ ->
        invalid("SVG viewport requires positive bounded dimensions")
    end
  end

  defp prepare_source(source, context, budget, depth, image_font_options) do
    with :ok <- HtmlValidator.check_render_resource(:max_embedded_document_depth, depth, :style),
         {:ok, summary} <-
           SvgValidator.validate_processing(
             source,
             Map.take(context, [:mode, :parent_mode, :fonts, :resources])
           ),
         :ok <- SvgImageValidator.validate_descriptors(context.resources),
         :ok <- validate_capabilities(summary),
         {:ok, dom} <- HtmlParser.parse_detailed(source),
         {:ok, dom} <- SvgSceneValidator.align_xml_source(dom, summary),
         {:ok, root} <- SvgSceneValidator.extraction_root(dom),
         :ok <- SvgSceneValidator.validate_source_tree(root),
         occurrences =
           Map.new(Enum.with_index(SvgSceneValidator.tree_nodes(root), 1), fn {{node, _}, index} ->
             {node.node_id, Map.get(summary.expansion_counts, index, 1)}
           end),
         {:ok, fonts} <-
           if(is_nil(image_font_options),
             do: {:ok, context.fonts},
             else:
               FontValidator.prepare_svg_image_fonts(context.fonts, image_font_options, summary)
           ),
         {:ok, root, fonts} <-
           embed_resources(root, context, budget, depth, occurrences, image_font_options, fonts),
         normalized = normalize_converter_source(root),
         approved = SvgScene.authored_source(normalized),
         :ok <- HtmlValidator.check_render_resource(:max_svg_bytes, byte_size(approved), :style) do
      {:ok, approved, summary, fonts}
    end
  end

  # Zero-area marker viewBoxes must paint nothing. Large glyph turns are
  # reduced before the converter's single-precision angle arithmetic.
  defp normalize_converter_source(node) do
    children =
      Enum.map(node.children, fn child ->
        if Map.get(child, :type) == :element,
          do: normalize_converter_source(child),
          else: child
      end)

    node = %{node | children: children}

    case Map.get(node, :tag) do
      "marker" -> normalize_converter_marker(node)
      tag when tag in ~w(text tspan textPath) -> normalize_converter_rotation(node)
      _ -> node
    end
  end

  defp normalize_converter_marker(marker) do
    case SvgViewportValidator.view_box(Map.get(marker.attributes, "viewBox")) do
      {:ok, {_min_x, _min_y, view_width, view_height}} when view_width == 0 or view_height == 0 ->
        %{marker | children: []}

      _ ->
        marker
    end
  end

  defp normalize_converter_rotation(node) do
    case Map.get(node.attributes, "rotate") do
      nil ->
        node

      value ->
        tokens = String.split(value, ~r/[\s,]+/, trim: true)

        # Longer lists may interleave optional cx/cy centers, whose grouping is
        # ambiguous, so only one or two plain glyph turns are reduced here.
        reduced = if length(tokens) in 1..2, do: reduce_converter_rotation(tokens), else: :error

        case reduced do
          {:ok, tokens} ->
            %{node | attributes: Map.put(node.attributes, "rotate", Enum.join(tokens, " "))}

          :error ->
            node
        end
    end
  end

  defp reduce_converter_rotation(tokens) do
    Enum.reduce_while(tokens, {:ok, []}, fn token, {:ok, reduced} ->
      case HtmlValidator.parse_css_number(token) do
        {angle, ""} ->
          # The IEEE remainder of a division by 360 is computed exactly, so a
          # reduced turn keeps the authored geometry bit-for-bit while avoiding
          # the converter's single-precision angle loss.
          reduced_angle = :math.fmod(angle, 360.0)

          if reduced_angle == angle do
            {:cont, {:ok, reduced ++ [token]}}
          else
            case converter_number(reduced_angle) do
              {:ok, text} -> {:cont, {:ok, reduced ++ [text]}}
              :error -> {:halt, :error}
            end
          end

        _ ->
          {:halt, :error}
      end
    end)
  end

  defp converter_number(value) do
    # The only caller supplies a remainder modulo 360, including zero.
    if value == trunc(value) do
      {:ok, Integer.to_string(trunc(value))}
    else
      text = :erlang.float_to_binary(value, [:short])
      if String.contains?(text, ["e", "E"]), do: :error, else: {:ok, text}
    end
  end

  @doc "Checks whether the existing converter can paint the complete validated SVG input."
  @spec validate_capabilities(map()) :: :ok | {:error, {atom(), map()}}
  def validate_capabilities(summary) do
    cond do
      MapSet.member?(summary.elements, {:svg, "foreignObject"}) ->
        unsupported(
          "the existing SVG converter does not render foreignObject; its HTML content cannot be omitted",
          "foreignObject"
        )

      MapSet.member?(summary.converter_features, :non_scaling_stroke) ->
        unsupported(
          "the existing SVG converter does not render non-scaling strokes; vector-effect cannot be omitted",
          "vector-effect"
        )

      Enum.any?(
        summary.source_nodes,
        &(&1.tag == "textPath" and Map.has_key?(&1.attributes, "path"))
      ) ->
        unsupported(
          "the existing SVG converter does not render SVG2 textPath path data; use a referenced path with href instead",
          "textPath path"
        )

      true ->
        :ok
    end
  end

  defp embed_resources(node, context, budget, depth, occurrences, image_font_options, fonts) do
    href =
      if node.tag in ~w(image feImage),
        do: Map.get(node.attributes, "href", Map.get(node.attributes, "xlink:href")),
        else: nil

    result =
      case Map.fetch(context.resources, href) do
        {:ok, %{data: data, media_type: mime}} ->
          with :ok <-
                 HtmlValidator.reserve_render_resource(
                   :max_aggregate_svg_resource_bytes,
                   byte_size(data),
                   :style
                 ),
               :ok <- HtmlValidator.reserve_image_source(budget, byte_size(data)),
               {:ok, data, resource_fonts} <-
                 resource_bytes(
                   data,
                   mime,
                   Map.update!(context, :resources, &Map.delete(&1, href)),
                   budget,
                   depth + 1,
                   Map.fetch!(occurrences, node.node_id),
                   image_font_options
                 ) do
            attrs =
              node.attributes
              |> Map.delete("xlink:href")
              |> Map.put("href", "data:#{mime};base64,#{Base.encode64(data)}")

            {:ok, %{node | attributes: attrs}, Enum.uniq_by(fonts ++ resource_fonts, & &1.data)}
          end

        :error ->
          {:ok, node, fonts}
      end

    with {:ok, node, fonts} <- result do
      Enum.reduce_while(node.children, {:ok, [], fonts}, fn child, {:ok, children, fonts} ->
        result =
          if Map.get(child, :type) == :element,
            do:
              embed_resources(
                child,
                context,
                budget,
                depth,
                occurrences,
                image_font_options,
                fonts
              ),
            else: {:ok, child, fonts}

        case result do
          {:ok, child, fonts} -> {:cont, {:ok, [child | children], fonts}}
          error -> {:halt, error}
        end
      end)
      |> case do
        {:ok, children, fonts} -> {:ok, %{node | children: Enum.reverse(children)}, fonts}
        error -> error
      end
    end
  end

  defp resource_bytes(data, mime, context, budget, depth, count, image_font_options) do
    case mime do
      "image/svg+xml" ->
        with {:ok, source, summary, fonts} <-
               prepare_source(data, context, budget, depth, image_font_options),
             {:ok, _} <- SvgValidator.validate_dimensions(summary.root, [], budget),
             {:ok, _} <- prepare_fonts(fonts, summary),
             do: {:ok, source, fonts}

      "image/png" ->
        with {:ok, image} <- PngDecoder.decode(data, budget, true),
             :ok <-
               HtmlValidator.reserve_render_resource(
                 :max_aggregate_svg_raster_pixels,
                 image.width_px * image.height_px * count,
                 :style
               ),
             :ok <-
               (if count > 1 do
                  Enum.reduce_while(2..count, :ok, fn _, :ok ->
                    case HtmlValidator.reserve_decoded_image(
                           budget,
                           image.width_px,
                           image.height_px,
                           image.decoded_stride
                         ) do
                      :ok -> {:cont, :ok}
                      error -> {:halt, error}
                    end
                  end)
                else
                  :ok
                end),
             do: {:ok, data, []}

      "image/jpeg" ->
        case HtmlValidator.jpeg_metadata(data) do
          {:ok, image} ->
            with :ok <-
                   HtmlValidator.reserve_render_resource(
                     :max_aggregate_svg_raster_pixels,
                     image.width_px * image.height_px * count,
                     :style
                   ),
                 :ok <-
                   Enum.reduce_while(1..count, :ok, fn _, :ok ->
                     # The converter decodes JPEG resources into four-channel image planes.
                     case HtmlValidator.reserve_decoded_image(
                            budget,
                            image.width_px,
                            image.height_px,
                            4
                          ) do
                       :ok -> {:cont, :ok}
                       error -> {:halt, error}
                     end
                   end),
                 do: {:ok, data, []}

          :error ->
            invalid("SVG JPEG resource bytes are invalid")
        end
    end
  end

  defp prepare_fonts(fonts, summary) do
    FontValidator.with_budget(fn ->
      with :ok <- HtmlValidator.check_render_resource(:max_font_count, length(fonts), :font) do
        Enum.reduce_while(fonts, {:ok, []}, fn font, {:ok, bytes} ->
          result =
            case font do
              %{data: data} when is_binary(data) ->
                FontValidator.reserve_source(data)

                HtmlValidator.reserve_render_resource(
                  :max_svg_font_work_bytes,
                  byte_size(data),
                  :font
                )

                case FontValidator.memo({:validated_ttf, data}, fn ->
                       FontValidator.prepare_ttf(data)
                     end) do
                  {:ok, parsed} ->
                    with :ok <-
                           HtmlValidator.validate_font_embedding(
                             Map.get(font, :family, "SVG font"),
                             parsed.embedding_flags,
                             parsed.variable_font?
                           ),
                         do: {:ok, data, parsed}

                  {:error, _} = error ->
                    error

                  :error ->
                    invalid("SVG fonts require supported approved TrueType bytes")
                end

              _ ->
                invalid("SVG fonts accept byte descriptors, never filesystem paths")
            end

          case result do
            {:ok, data, parsed} -> {:cont, {:ok, [{data, parsed} | bytes]}}
            error -> {:halt, error}
          end
        end)
        |> case do
          {:ok, faces} ->
            if Enum.all?(summary.text, fn text ->
                 Enum.all?(String.to_charlist(text), fn scalar ->
                   scalar in [9, 10, 13] or
                     Enum.any?(faces, fn {_, parsed} ->
                       Font.supports_text?(Map.put(parsed, :type, :embedded), <<scalar::utf8>>)
                     end)
                 end)
               end),
               do: {:ok, Enum.reverse(faces) |> Enum.map(&elem(&1, 0))},
               else:
                 unsupported(
                   "approved SVG font bytes do not cover the original text; converter fallback is restricted to approved faces",
                   "SVG text"
                 )

          error ->
            error
        end
      end
    end)
  end

  defp invalid(message),
    do:
      Diagnostics.error(:style, :invalid_document, message,
        source: "SVG",
        module: __MODULE__,
        operation: :convert
      )

  defp unsupported(message, source),
    do:
      Diagnostics.error(:layout, :unsupported_html, message,
        source: source,
        module: __MODULE__,
        operation: :convert
      )
end
