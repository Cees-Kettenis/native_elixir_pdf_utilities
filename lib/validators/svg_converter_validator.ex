defmodule NativeElixirPdfUtilities.Validators.SvgConverterValidator do
  @moduledoc false
  alias NativeElixirPdfUtilities.Diagnostics
  alias NativeElixirPdfUtilities.HtmlToPdf.{Font, HtmlParser, SvgScene, PngDecoder}

  alias NativeElixirPdfUtilities.Validators.{
    FontValidator,
    HtmlValidator,
    SvgValidator,
    SvgImageValidator,
    SvgSceneValidator
  }

  @doc "Validates the complete authored SVG document before host styling or asset resolution."
  @spec prepare_extraction(term()) :: {:ok, map()} | {:error, {atom(), map()}}
  def prepare_extraction(source) do
    with {:ok, document, _summary} <- prepare_extraction_details(source), do: {:ok, document}
  end

  @doc "Returns the parsed extraction document and its actual canonical XML summary."
  @spec prepare_extraction_details(term()) :: {:ok, map(), map()} | {:error, {atom(), map()}}
  def prepare_extraction_details(source) do
    with {:ok, document} <- HtmlParser.parse_detailed(source),
         {:ok, root} <- SvgSceneValidator.extraction_root(document),
         {:ok, summary} <-
           SvgValidator.validate_processing(
             source,
             %{mode: :inline, resources: SvgImageValidator.pending_references(root)}
           ),
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
           {:ok, source, summary} <- prepare_source(source, context, budget, 0),
           {:ok, fonts} <-
             if(is_nil(image_font_options),
               do: {:ok, fonts},
               else: FontValidator.prepare_svg_image_fonts(fonts, image_font_options, summary)
             ),
           {:ok, dimensions} <- SvgValidator.validate_dimensions(summary.root, options, budget),
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
      Enum.reduce_while(SvgSceneValidator.tree_nodes(root), {:ok, [], %{}}, fn {node, _},
                                                                               {:ok, fonts, names} ->
        if (node.tag in ~w(text tspan textPath) or
              (node.tag == "use" and get_in(node, [:style, :font_face, :type]) == :embedded)) and
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
    with {:ok, _} <- SvgValidator.validate_processing(source, context),
         {:ok, {width, height}} <- viewport_size(size),
         {:ok, document} <- HtmlParser.parse_detailed(source),
         {:ok, root} <- SvgSceneValidator.extraction_root(document) do
      root = %{
        root
        | attributes:
            root.attributes
            |> Map.put("width", to_string(width))
            |> Map.put("height", to_string(height))
      }

      {:ok, SvgScene.authored_source(root)}
    else
      {:error, _} = error -> error
      _ -> invalid("SVG extraction requires exactly one SVG root")
    end
  end

  @doc "Places a document root in its actual child canvas without stretching its authored viewport."
  @spec with_document_viewport(binary(), tuple(), tuple(), map()) ::
          {:ok, binary()} | {:error, {atom(), map()}}
  def with_document_viewport(source, root_size, canvas_size, context) do
    with {:ok, source} <- with_root_viewport(source, root_size, context),
         {:ok, {width, height}} <- viewport_size(canvas_size) do
      {:ok,
       "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"#{width}\" height=\"#{height}\" viewBox=\"0 0 #{width} #{height}\">#{source}</svg>"}
    end
  end

  @doc "Returns an actionable operational diagnostic for approved snapshot staging failures."
  @spec staging_error(term()) :: {:error, {atom(), map()}}
  def staging_error(reason),
    do: invalid("could not stage approved SVG font bytes: #{inspect(reason)}")

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

  defp prepare_source(source, context, budget, depth) do
    with :ok <- HtmlValidator.check_render_resource(:max_embedded_document_depth, depth, :style),
         {:ok, summary} <-
           SvgValidator.validate_processing(
             source,
             Map.take(context, [:mode, :parent_mode, :fonts, :resources])
           ),
         :ok <- SvgImageValidator.validate_descriptors(context.resources),
         :ok <- validate_capabilities(summary),
         {:ok, dom} <- HtmlParser.parse_detailed(source),
         {:ok, root} <- SvgSceneValidator.extraction_root(dom),
         :ok <- SvgSceneValidator.validate_source_tree(root),
         occurrences =
           Map.new(Enum.with_index(SvgSceneValidator.tree_nodes(root), 1), fn {{node, _}, index} ->
             {node.node_id, Map.get(summary.expansion_counts, index, 1)}
           end),
         {:ok, root} <- embed_resources(root, context, budget, depth, occurrences),
         approved = SvgScene.authored_source(root),
         :ok <- HtmlValidator.check_render_resource(:max_svg_bytes, byte_size(approved), :style) do
      {:ok, approved, summary}
    else
      {:error, _} = error -> error
      _ -> invalid("SVG converter input requires exactly one complete root")
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

      true ->
        :ok
    end
  end

  defp embed_resources(node, context, budget, depth, occurrences) do
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
               :ok <-
                 if(is_nil(budget),
                   do: :ok,
                   else: HtmlValidator.reserve_image_source(budget, byte_size(data))
                 ),
               {:ok, data} <-
                 resource_bytes(
                   data,
                   mime,
                   Map.update!(context, :resources, &Map.delete(&1, href)),
                   budget,
                   depth + 1,
                   Map.fetch!(occurrences, node.node_id)
                 ) do
            attrs =
              node.attributes
              |> Map.delete("xlink:href")
              |> Map.put("href", "data:#{mime};base64,#{Base.encode64(data)}")

            {:ok, %{node | attributes: attrs}}
          end

        :error ->
          {:ok, node}
      end

    with {:ok, node} <- result do
      Enum.reduce_while(node.children, {:ok, []}, fn child, {:ok, children} ->
        result =
          if Map.get(child, :type) == :element,
            do: embed_resources(child, context, budget, depth, occurrences),
            else: {:ok, child}

        case result do
          {:ok, child} -> {:cont, {:ok, [child | children]}}
          error -> {:halt, error}
        end
      end)
      |> case do
        {:ok, children} -> {:ok, %{node | children: Enum.reverse(children)}}
        error -> error
      end
    end
  end

  defp resource_bytes(data, mime, context, budget, depth, count) do
    case mime do
      "image/svg+xml" ->
        with {:ok, source, summary} <- prepare_source(data, context, budget, depth),
             {:ok, _} <- SvgValidator.validate_dimensions(summary.root, [], budget),
             {:ok, _} <- prepare_fonts(context.fonts, summary),
             do: {:ok, source}

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
             do: {:ok, data}

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
                 do: {:ok, data}

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

                case FontValidator.prepare_ttf(data) do
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
