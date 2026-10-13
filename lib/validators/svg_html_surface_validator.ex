defmodule NativeElixirPdfUtilities.Validators.SvgHtmlSurfaceValidator do
  @moduledoc false
  alias NativeElixirPdfUtilities.{Diagnostics, Limits, Text}
  alias NativeElixirPdfUtilities.HtmlToPdf.{PdfWriter, ResourceContext, SvgBackend}
  alias NativeElixirPdfUtilities.Pdf.Reader

  alias NativeElixirPdfUtilities.Validators.{
    HtmlValidator,
    SvgBackendValidator,
    SvgForeignObjectValidator,
    SvgValidator
  }

  @doc "Prepares an effect input from the actual issued HTML slot and its original native drawing group."
  @spec prepare(map()) :: {:ok, map()} | {:error, {atom(), map()}}
  def prepare(slot) do
    original = slot.original

    ResourceContext.with_document(original.context, fn ->
      with {:ok, group} <- SvgForeignObjectValidator.layout_slot(slot),
           {:ok, source} <- PdfWriter.render([%{size: {slot.width, slot.height}, boxes: [group]}]),
           {:ok, context} <- Reader.read_validated(source),
           :ok <- validate_native_graph(context.document),
           {:ok, dimensions, input_pixels} <- validate_input(context, slot),
           :ok <- SvgBackendValidator.native_load_result(SvgBackend.load_native()),
           {:ok, {pixels, work, glyphs, selected}} <-
             NativeElixirPdfUtilities.HtmlToPdf.SvgBackend.NativeBytes.prepare_html_surface(
               source,
               dimensions.width,
               dimensions.height,
               {Limits.get(:max_aggregate_svg_surface_pixels), Limits.get(:max_svg_filter_work)},
               input_pixels
             )
             |> prepare_result(),
           :ok <-
             HtmlValidator.reserve_render_resource(
               :max_aggregate_svg_surface_pixels,
               pixels,
               :layout
             ),
           :ok <- HtmlValidator.reserve_render_resource(:max_svg_filter_work, work, :layout),
           {:ok, spans} <- selected_spans(source),
           {:ok, projected} <- visible_spans(spans, selected),
           logical = Text.painted_projection(projected),
           {:ok, clusters} <- logical_clusters(logical, projected) do
        {:ok,
         %{
           slot: slot,
           group: group,
           source: source,
           dimensions: dimensions,
           spans: spans,
           logical: logical,
           logical_clusters: clusters,
           visible_spans: projected,
           glyphs: glyphs,
           selected: selected,
           surface_pixels: pixels,
           surface_work: work
         }}
      end
    end)
  end

  @doc false
  @spec render(map()) :: {:ok, binary()} | {:error, {atom(), map()}}
  def render(prepared) do
    NativeElixirPdfUtilities.HtmlToPdf.SvgBackend.NativeBytes.render_html_surface(
      prepared.source,
      prepared.dimensions.width,
      prepared.dimensions.height
    )
    |> SvgValidator.conversion_result()
  end

  defp validate_input(context, slot) do
    {a, b, c, d, _, _} = slot.transform

    width =
      max(
        ceil(
          slot.width / 0.75 *
            Limits.get(:svg_raster_scale) *
            :math.sqrt(a * a + b * b)
        ),
        1
      )

    height =
      max(
        ceil(
          slot.height / 0.75 *
            Limits.get(:svg_raster_scale) *
            :math.sqrt(c * c + d * d)
        ),
        1
      )

    images =
      Enum.reduce_while(context.document.objects, {:ok, 0}, fn {_ref, object}, {:ok, pixels} ->
        case object.value do
          %{"Subtype" => {:name, "Image"}, "Width" => w, "Height" => h}
          when is_integer(w) and w > 0 and is_integer(h) and h > 0 ->
            {:cont, {:ok, pixels + w * h}}

          %{"Subtype" => {:name, "Image"}} ->
            {:halt, invalid("issued HTML painting image has invalid dimensions")}

          _ ->
            {:cont, {:ok, pixels}}
        end
      end)

    cond do
      width > Limits.get(:max_svg_raster_dimension) or
          height > Limits.get(:max_svg_raster_dimension) ->
        limit("max_svg_raster_dimension")

      width * height > Limits.get(:max_svg_raster_pixels) ->
        limit("max_svg_raster_pixels")

      # Pinned vello_cpu represents physical raster dimensions using u16.
      width > 65_535 or height > 65_535 ->
        invalid("issued HTML surface exceeds the pinned native canvas representation")

      true ->
        case images do
          {:ok, pixels} -> {:ok, %{width: width, height: height}, pixels}
          error -> error
        end
    end
  end

  # The pinned interpreter has an intrinsic 50-level interpretation stack.
  # Check every own-writer resource, including masks which Text does not visit,
  # before native execution can silently stop at that representation bound.
  defp validate_native_graph(document) do
    Enum.reduce_while(document.objects, :ok, fn {ref, object}, :ok ->
      if is_map(object.value) and object.value["Subtype"] == {:name, "Form"} do
        case resource_depth(ref, document.objects, 0, MapSet.new()) do
          :ok -> {:cont, :ok}
          error -> {:halt, error}
        end
      else
        {:cont, :ok}
      end
    end)
  end

  defp resource_depth(ref, objects, depth, path) do
    if MapSet.member?(path, ref) do
      invalid("issued HTML painting resources contain a reference cycle")
    else
      case Map.get(objects, ref) do
        %{value: value} ->
          depth =
            depth + if(is_map(value) and value["Subtype"] == {:name, "Form"}, do: 1, else: 0)

          cond do
            depth > Limits.get(:max_pdf_form_depth) ->
              limit("max_pdf_form_depth")

            depth >= 50 ->
              Diagnostics.error(
                :layout,
                :unsupported_html,
                "issued HTML resources exceed the pinned native interpreter stack",
                module: __MODULE__,
                operation: :prepare,
                source: "foreignObject HTML effect input"
              )

            true ->
              value =
                if is_map(value), do: Map.drop(value, ~w(Parent P Pages Annots)), else: value

              Enum.reduce_while(resource_references(value), :ok, fn child, :ok ->
                case resource_depth(child, objects, depth, MapSet.put(path, ref)) do
                  :ok -> {:cont, :ok}
                  error -> {:halt, error}
                end
              end)
          end

        _ ->
          invalid("issued HTML painting resource is missing")
      end
    end
  end

  defp resource_references(value) do
    case value do
      {:ref, ref} -> [ref]
      value when is_map(value) -> Enum.flat_map(Map.values(value), &resource_references/1)
      value when is_list(value) -> Enum.flat_map(value, &resource_references/1)
      _ -> []
    end
  end

  defp selected_spans(source) do
    case Text.extract_spans(source) do
      {:ok, %{pages: [page]}} -> {:ok, Enum.filter(page.spans, & &1.paints_text?)}
      {:error, _} = error -> error
    end
  end

  # Native glyphs come from the same writer-produced embedded fonts/CIDs.
  # Bind each original decoded operand to those glyphs before selecting visible
  # text. Keep its original matrix, advance and source index for projection.
  defp visible_spans(spans, selected) do
    result =
      Enum.reduce_while(spans, {:ok, [], selected}, fn span, {:ok, projected, remaining} ->
        {glyphs, remaining, decoded} =
          Enum.reduce_while(remaining, {[], remaining, ""}, fn glyph,
                                                               {glyphs, remaining, decoded} ->
            {unicode, _gid, _matrix, _bounds, _visible} = glyph

            if byte_size(decoded) >= byte_size(span.text),
              do: {:halt, {glyphs, remaining, decoded}},
              else: {:cont, {[glyph | glyphs], tl(remaining), decoded <> unicode}}
          end)

        if decoded == span.text do
          text =
            glyphs
            |> Enum.reverse()
            |> Enum.filter(fn {unicode, _, _, _, visible} ->
              visible or String.trim(unicode) == ""
            end)
            |> Enum.map_join(fn {unicode, _, _, _, _} -> unicode end)

          projected =
            if String.trim(text) == "",
              do: projected,
              else: [
                Map.merge(span, %{text: text, selected_html_glyphs: Enum.reverse(glyphs)})
                | projected
              ]

          {:cont, {:ok, projected, remaining}}
        else
          {:halt,
           Diagnostics.error(
             :layout,
             :unsupported_html,
             "issued HTML effect input requires complete original cluster mapping for its selected glyphs",
             module: __MODULE__,
             operation: :prepare,
             source: "foreignObject selected HTML clusters"
           )}
        end
      end)

    case result do
      {:ok, projected, []} ->
        {:ok, Enum.reverse(projected)}

      {:ok, _projected, _remaining} ->
        invalid("issued HTML glyph painting and original selected operands disagree")

      error ->
        error
    end
  end

  # Inferred spaces retain Text's standard projection policy and belong to
  # the following genuine selected glyph, rather than an unrelated effect box.
  defp logical_clusters(logical, spans) do
    glyphs =
      Enum.flat_map(spans, & &1.selected_html_glyphs)
      |> Enum.filter(fn {unicode, _, _, _, visible} -> visible or String.trim(unicode) == "" end)

    result =
      Enum.reduce_while(glyphs, {:ok, [], "", logical}, fn {unicode, _, _, bounds, _},
                                                           {:ok, clusters, pending, remaining} ->
        prefix =
          if String.starts_with?(remaining, unicode),
            do: "",
            else: Regex.run(~r/^\s*/u, remaining) |> hd()

        text = prefix <> unicode

        if String.starts_with?(remaining, text) do
          tail = binary_part(remaining, byte_size(text), byte_size(remaining) - byte_size(text))

          if String.trim(text) == "" do
            {:cont, {:ok, clusters, pending <> text, tail}}
          else
            {:cont, {:ok, [{pending <> text, bounds} | clusters], "", tail}}
          end
        else
          {:halt,
           invalid("original selected HTML glyphs cannot bind to their logical projection")}
        end
      end)

    case result do
      {:ok, clusters, _pending, remaining} ->
        if String.trim(remaining) == "",
          do: {:ok, Enum.reverse(clusters)},
          else: invalid("original selected HTML logical projection is incomplete")

      error ->
        error
    end
  end

  defp prepare_result(result) do
    case result do
      {:ok, _} ->
        result

      {:error, limit} when limit in ["max_aggregate_svg_surface_pixels", "max_svg_filter_work"] ->
        limit(limit)

      {:error, message} ->
        invalid("issued HTML surface cannot paint faithfully: " <> message)
    end
  end

  defp invalid(message),
    do:
      Diagnostics.error(:layout, :invalid_document, message,
        module: __MODULE__,
        operation: :prepare,
        source: "foreignObject HTML effect input"
      )

  defp limit(name),
    do:
      Diagnostics.error(:limits, :resource_limit_exceeded, "issued HTML surface exceeds " <> name,
        module: __MODULE__,
        operation: :prepare,
        source: "foreignObject HTML effect input"
      )
end
