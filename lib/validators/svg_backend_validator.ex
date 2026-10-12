defmodule NativeElixirPdfUtilities.Validators.SvgBackendValidator do
  @moduledoc false
  alias NativeElixirPdfUtilities.{Diagnostics, Limits}
  alias NativeElixirPdfUtilities.HtmlToPdf.{Font, PngDecoder}
  alias NativeElixirPdfUtilities.Validators.{FontValidator, HtmlValidator, SvgValidator}

  @doc false
  @spec prepare(term(), term(), term(), HtmlValidator.image_budget() | nil) ::
          {:ok, map()} | {:error, {atom(), map()}}
  def prepare(source, options, context, image_budget) do
    image_budget = image_budget || HtmlValidator.new_image_budget()

    with {:ok, summary} <- SvgValidator.validate_processing(source, context),
         context = summary.context,
         :ok <- validate_capabilities(summary, context),
         {:ok, fonts} <- validate_fonts(context.fonts, summary.text),
         {:ok, resources} <- validate_resources(context.resources, image_budget, summary),
         {:ok, dimensions} <-
           SvgValidator.validate_dimensions(summary.root, options, image_budget),
         # Pinned resvg's actual max_bbox is five canvas widths/heights (25× area).
         # Allow three temporary surfaces per expanded node/text character. Author
         # clipPath and stylesheet scenes remain gated until exact scene accounting.
         :ok <-
           HtmlValidator.reserve_render_resource(
             :max_aggregate_svg_surface_pixels,
             75 * dimensions[:width] * dimensions[:height] * summary.expanded_work,
             :layout
           ) do
      {:ok,
       %{
         source: source,
         width: dimensions[:width],
         height: dimensions[:height],
         mode: context.mode,
         fonts: fonts,
         resources: resources
       }}
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
  @spec native_load_result(term()) :: :ok | {:error, {atom(), map()}}
  def native_load_result(result) do
    case result do
      {:module, NativeElixirPdfUtilities.HtmlToPdf.SvgBackend.NativeBytes} ->
        :ok

      result ->
        Diagnostics.error(
          :layout,
          :svg_backend_unavailable,
          "SVG native artifacts could not load: #{inspect(result)}; rebuild native/svg_backend with the pinned Cargo toolchain",
          source: "SVG",
          module: __MODULE__,
          operation: :rasterize
        )
    end
  end

  @doc false
  @spec validate_capabilities(map(), map()) :: :ok | {:error, {atom(), map()}}
  def validate_capabilities(summary, context) do
    supported =
      ~w(svg g defs symbol use path rect circle ellipse line polyline polygon linearGradient radialGradient stop text tspan textPath image title desc a)

    cond do
      context.mode == :inline ->
        unsupported(
          "inline SVG requires host cascade, viewport layout and scene composition; this adapter does not enable inline rendering"
        )

      MapSet.member?(summary.elements, {:svg, "foreignObject"}) ->
        unsupported(
          "usvg 0.47.0 has no foreignObject conversion branch; native HTML scene insertion is required"
        )

      Enum.any?(summary.elements, fn {namespace, tag} ->
        namespace == :html or tag in ~w(filter mask pattern marker)
      end) ->
        unsupported(
          "SVG effects and HTML require bounded intermediate scene composition before this adapter can render them"
        )

      Enum.any?(summary.elements, fn {_namespace, tag} -> tag not in supported end) ->
        unsupported(
          "SVG backend element is outside this bounded adapter profile; complete scene support remains pending"
        )

      summary.images >
          summary.hrefs
          |> Enum.filter(fn {_index, _href, _line, name} -> name == "image" end)
          |> Enum.map(&elem(&1, 0))
          |> Enum.uniq()
          |> length() ->
        unsupported("SVG image requires an explicit validated resource reference")

      Enum.any?(summary.hrefs, fn {_index, href, _line, name} ->
        (name == "image" and String.starts_with?(href, "#")) or
            (name not in ["image", "a"] and not String.starts_with?(href, "#"))
      end) ->
        unsupported(
          "SVG images require supplied PNG bytes; external SVG references require later scene support"
        )

      MapSet.member?(summary.elements, {:svg, "text"}) and context.fonts == [] ->
        unsupported(
          "SVG text requires selected validated font bytes; an empty backend font database would omit artwork"
        )

      true ->
        :ok
    end
  end

  defp validate_fonts(fonts, text) do
    FontValidator.with_budget(fn ->
      if length(fonts) > Limits.get(:max_font_count) do
        Diagnostics.error(:limits, :resource_limit_exceeded, "SVG fonts exceed max_font_count",
          source: "SVG"
        )
      else
        with {:ok, validated} <-
               Enum.reduce_while(fonts, {:ok, []}, fn font, {:ok, validated} ->
                 # Use the same parser, source fingerprints and limits as the host font registry.
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
                           {:ok, data, parsed}

                         {:error, _} = error ->
                           error

                         :error ->
                           Diagnostics.error(
                             :font,
                             :invalid_document,
                             "SVG font bytes must contain a supported TrueType font",
                             source: "SVG"
                           )
                       end

                     _ ->
                       Diagnostics.error(
                         :font,
                         :invalid_document,
                         "SVG fonts accept byte descriptors %{data: binary}, never paths",
                         source: "SVG"
                       )
                   end

                 case result do
                   {:ok, data, parsed} -> {:cont, {:ok, [{data, parsed} | validated]}}
                   error -> {:halt, error}
                 end
               end) do
          characters =
            text |> Enum.join() |> String.graphemes() |> Enum.reject(&(&1 in ["\t", "\r", "\n"]))

          if Enum.all?(characters, fn character ->
               Enum.any?(validated, fn {_data, parsed} ->
                 Font.supports_text?(Map.put(parsed, :type, :embedded), character)
               end)
             end) do
            {:ok, validated |> Enum.reverse() |> Enum.map(&elem(&1, 0))}
          else
            unsupported(
              "selected SVG font bytes do not cover all text; glyph fallback integration is required"
            )
          end
        end
      end
    end)
  end

  defp validate_resources(resources, image_budget, summary) do
    budget = image_budget

    Enum.reduce_while(resources, {:ok, %{}}, fn {name, descriptor}, {:ok, validated} ->
      occurrences =
        Enum.reduce(summary.hrefs, 0, fn {index, href, _line, tag}, count ->
          if tag == "image" and href == name,
            do: count + Map.fetch!(summary.expansion_counts, index),
            else: count
        end)
        |> max(1)

      result =
        case {name, descriptor} do
          {name, %{data: data, media_type: "image/png"}}
          when is_binary(name) and is_binary(data) ->
            cond do
              not Regex.match?(~r/\Asvg-resource:[A-Za-z0-9_.-]+\z/, name) ->
                Diagnostics.error(
                  :style,
                  :invalid_document,
                  "SVG resource keys must be opaque svg-resource: identifiers",
                  source: "SVG"
                )

              byte_size(data) > Limits.get(:max_svg_resource_bytes) ->
                Diagnostics.error(
                  :limits,
                  :resource_limit_exceeded,
                  "SVG resource exceeds max_svg_resource_bytes",
                  source: name
                )

              true ->
                HtmlValidator.reserve_render_resource(
                  :max_aggregate_svg_resource_bytes,
                  byte_size(data),
                  :style
                )

                with :ok <- HtmlValidator.reserve_image_source(budget, byte_size(data)),
                     {:ok, image} <- PngDecoder.decode(data, budget, true),
                     :ok <-
                       HtmlValidator.reserve_render_resource(
                         :max_aggregate_svg_raster_pixels,
                         image.width_px * image.height_px * occurrences,
                         :style
                       ),
                     :ok <-
                       Enum.reduce_while(1..occurrences, :ok, fn _occurrence, :ok ->
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
            end

          _ ->
            Diagnostics.error(
              :style,
              :invalid_document,
              "SVG resources require PNG byte descriptors; paths and nested SVG are not supported by this adapter",
              source: "SVG"
            )
        end

      case result do
        {:ok, data} -> {:cont, {:ok, Map.put(validated, name, data)}}
        error -> {:halt, error}
      end
    end)
  end

  defp unsupported(message) do
    Diagnostics.error(:layout, :unsupported_html, message,
      source: "SVG",
      operation: :rasterize,
      module: __MODULE__
    )
  end
end
